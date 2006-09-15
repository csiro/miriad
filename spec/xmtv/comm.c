/*
 *	<comm.c> - communications routines.
 *	History:
 *	  jm  03jun92 Original code (some borrowed from XAS).
 */

#include "xmtv.h"

/* Header info needed for socket routines (including i/o).  */
#include <sys/types.h>
#include <sys/time.h>

#ifdef AIX
#include <sys/select.h>
#endif

#include <sys/socket.h>
#include <sys/un.h>
#include <netdb.h>
#include <netinet/in.h>

#define UNIX_DOMAIN 0
#define INET_DOMAIN 1

/* Private variables. */

static int domain_type;
static struct sockaddr_in server_in;
static struct sockaddr_un server_un;

/* Source code. */

/************************************************************************/
int MakeLink(inet, buffered, service, portnumber)
Boolean inet;
Boolean buffered;
String service;
unsigned int portnumber;
/*
    Attempts to bind a socket for communications.  If `inet' is True
    and `service' points to a proper I/O service line, that port number
    is used for the connection.  If `service' is NULL or does not return
    a proper port designation, the input `portnumber' is used.  If
    `buffered' is True, then the output is buffered for some operations.
    If `inet' is False, then a UNIX connection is tried.  For UNIX
    connections `buffered' will always be set to False and the service
    name is taken to be the name of a Unix domain socket.

    Returns the opened socket if successful; -1 otherwise.
------------------------------------------------------------------------*/
{
    int i, listenSocket;
    struct servent *sp_in;

    if (inet == True) {                               /* INET domain.  */
      domain_type = INET_DOMAIN;

      (void)memset((void *)&server_in, (int)0, sizeof(struct sockaddr_in));
      server_in.sin_family = AF_INET;
      server_in.sin_port = htons(portnumber);
      server_in.sin_addr.s_addr = htonl(INADDR_ANY);

      if ((service != (String)NULL) && (strlen(service) > 0) &&
          ((sp_in = getservbyname((char *)service, "tcp")) != NULL)) {
        if (sp_in->s_port != portnumber)
          (void)fprintf(stderr, "XMTV: Service %s %s %d to %d\n",
            service, "will modify the I/O Port number from a value of",
            portnumber, sp_in->s_port);
        server_in.sin_port = htons(sp_in->s_port);
      }

      if ((listenSocket = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        perror("XMTV:MakeLink - socket (INET)");
        return(-1);
      }

      if (bind(listenSocket, (struct sockaddr *)&server_in,
          sizeof(server_in)) < 0) {
        perror ("XMTV:MakeLink - bind error (INET)");
        return(-1);
      }
    } else {                                          /* Unix domain.  */
      domain_type = UNIX_DOMAIN;
      buffered = False;

      (void)memset((void *)&server_un, (int)0, sizeof(struct sockaddr_un));
      server_un.sun_family = AF_UNIX;
      (void)strcpy(server_un.sun_path, service);

      if ((listenSocket = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
        perror("XMTV:MakeLink - socket (UNIX)");
        return(-1);
      }

      (void)unlink((char *)service);   /* First, unlink if it exists.  */

      if (bind(listenSocket, (struct sockaddr *)&server_un,
               strlen(server_un.sun_path) + 2) < 0) {
        perror("XMTV:MakeLink - bind error (UNIX)");
        return(-1);
      }
    }

    (void)listen(listenSocket, 5);         /* Queue up to 5 requests.  */

       /* Set up the opcodes we want to buffer (basically all writes). */
    for (i = 0; i <= NUMOP; i++)
      bufferop[i] = False;

    if (buffered == True) {
      bufferop[CLEAR]=True;
      bufferop[IMWRT]=True;
      bufferop[WLUT]=True;
      bufferop[WOFM]=True;
      bufferop[WCURS]=True;
      bufferop[GRAPH]=True;
      bufferop[SPLIT]=True;
      bufferop[WGRFX]=True;
      bufferop[WZOOM]=True;
      bufferop[WSCROL]=True;
      bufferop[WZSCR]=True;
    }

    return(listenSocket);
}

/************************************************************************/
int CheckLink(listenSocket)
int listenSocket;
/*
    Checks the listen socket to see if any data are present.  If there
    is some, it tries to set up a file descriptor to do the I/O.
    Returns the I/O socket if successful; 0 if no data present; -1 otherwise.
------------------------------------------------------------------------*/
{
    int len;
    int ioSocket = 0;
    fd_set read_mask;
    struct timeval time_0;
    struct sockaddr *addr;
    struct sockaddr_in in_addr;
    struct sockaddr_un un_addr;

    time_0.tv_sec = 0;
    time_0.tv_usec = 0;

    FD_ZERO(&read_mask);
    FD_SET(listenSocket, &read_mask);
    if (select(listenSocket+1, &read_mask, (fd_set *)NULL,
        (fd_set *)NULL, &time_0) > 0) {
      if (domain_type == INET_DOMAIN) {
        addr = (struct sockaddr *)&in_addr;
        len = sizeof(in_addr);
      } else {
        addr = (struct sockaddr *)&un_addr;
        len = sizeof(un_addr);
      }
      if ((ioSocket = accept(listenSocket, addr, &len)) < 0) {
        perror("CheckLink: accept");
        return(-1);
      }
    }

    return(ioSocket);
}

/************************************************************************/
int ReadLink(link, in)
int link;
XMTVinput *in;
{
    register int i;
    int bytes_togo, bytes_trans;
    short int lbuf[6];
    char *abuf;

    /*  Read header. */
    abuf = (char *)lbuf;
    bytes_togo = 6 * sizeof(short int);
    while (bytes_togo > 0) {
      bytes_trans = read(link, abuf, bytes_togo);
      if (bytes_trans <= 0) {
        (void)fprintf(stderr, "ReadLink read header error - shutdown\n");
        return(-1);
      }
      bytes_togo -= bytes_trans;
      abuf       += bytes_trans;
    }

    in->opcode = ntohs(lbuf[0]);
    for (i = 0; i < NPARMS; i++ )
      in->parms[i] = ntohs(lbuf[i+1]);
    in->data_length = ntohs(lbuf[5]);

   /* Note: in->data[] is unsigned char and isn't byte swapped. */
   /* "Variable length" data is read in bytes, not words;       */
   /* therefore, always read in an even number of bytes.        */

    abuf = (char *)in->data;
    bytes_togo = in->data_length + (in->data_length % 2);
    while (bytes_togo > 0) {
      bytes_trans = read(link, abuf, bytes_togo);
      if (bytes_trans <= 0) {
        (void)fprintf(stderr, "ReadLink read data error - shutdown\n");
        return (-1);
      }
      bytes_togo -= bytes_trans;
      abuf       += bytes_trans;
    }
    return(0);
}

/************************************************************************/
int WriteLink(link, in, out)
int link;
XMTVoutput *out;
XMTVinput *in;
{
    register int i, j;
    int buflen;
    static short int packet[4096+2];

    if (!bufferop[in->opcode]) {
      buflen = out->return_data_length;
      packet[0] = htons(out->status);
      packet[1] = htons(out->return_data_length);
      for(i = 2, j = 0; j < buflen; i++, j++)
        packet[i] = htons(out->data[j]);
      buflen = (buflen + 2) * sizeof(short int);
      if (write(link, (char *)packet, buflen) < buflen) {
        perror("XMTV:WriteLink - write");
        out->status = -1;
        return(-1);
      }
    } else {
      if (out->status != 0)
        (void)fprintf(stderr, "Buffered op=%d, status=%d\n", in->opcode,
          out->status);
    }

    return(0);
}

/************************************************************************/
void closeLink()
{
/*
    Shuts down the link; but only if the domain type is UNIX.
------------------------------------------------------------------------*/
    if (domain_type == UNIX_DOMAIN)
      if (unlink(server_un.sun_path) < 0)
        perror("XMTV: closeLink - unlink");
}
