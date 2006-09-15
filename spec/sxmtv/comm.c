/*--------------------------------------------------------------------*/
/* 06oct92 jm  Modified bind calls to cast second argument correctly. */
/*--------------------------------------------------------------------*/

#include "sxmtv.h"
#include <stdlib.h>
#include <strings.h>

short int swapbytes( a )
short int a;
{
    union {
        short int shortint;
        struct {
            unsigned char lo, hi;
        }byte;
    }x;
    x.shortint = a;
    return x.byte.lo << 8 | x.byte.hi;
}

#if BSD

MakeLink()
/*--------------------------------------------------------------------*/
/* Get a socket and bind it appropriately, if possible.  The          */
/* environmental variable TVDEV is expanded to find the TV            */
/* assignment which is in turn expanded to a device name. If the      */
/* device name contains a colon then the portion of the name before   */
/* the colon is taken to be a service name and an INET socket is      */
/* created. If the character before the colon is a 'B' then it is     */
/* omitted from the service name and certain opcodes may be buffered. */
/* If no colon is present the device name is taken to be the name of  */
/* a Unix domain socket.                                              */
/*--------------------------------------------------------------------*/
{
   char device[80];                     /* socket device name         */
   char service[16];                    /* INET service name          */
   char *tptr1, *tptr2;
   int buffered;                       /* True if socket is buffered. */
   register int i;
/*--------------------------------------------------------------------*/
                                        /* Check that TVDEV is        */
                                        /* defined and that it        */
                                        /* contains the name of an    */
                                        /* environmental variable.    */
   if (((tptr1 = getenv("TVDEV")) == NULL) || ((tptr1 = getenv(tptr1)) == NULL))
      tptr1 = "XASINB:";
   (void)strcpy(device, tptr1);

                                        /* INET domain                */
   if ((strchr(device, ':') != NULL) ||
        (strncmp(&device[0], "XASIN", 5) == 0)) {
      domain_type = INET_DOMAIN;
      tptr1 = device;
      tptr2 = service;
      while (*tptr1 != ':')
         *(tptr2++) = *(tptr1++);
      *tptr2 = '\0';
      if (*(--tptr2) == 'B') {
         buffered = True;
         *tptr2 = '\0';
         }
      else
         buffered = False;
      if ((sp_in = getservbyname (service, "tcp")) == NULL) {
         fprintf(stderr, "Failed to find port # -- Using port # 5000\n");
         server_in.sin_port = 5000;
      } else {
         server_in.sin_port = sp_in->s_port;
      }
      if ((MirSocket = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
         perror ("MakeLink: socket (INET)");
         return (-1);
         }
      if (bind(MirSocket, (struct sockaddr *)&server_in,
        sizeof(server_in)) < 0){
         perror ("MakeLink: bind error (INET)");
         return (-1);
         }
      }
                                                       /* UNIX domain */
   else {
      domain_type = UNIX_DOMAIN;
      buffered = False;
      server_un.sun_family = AF_UNIX;
      if ((MirSocket = socket (AF_UNIX, SOCK_STREAM, 0)) < 0) {
         perror ("MakeLink: socket (UNIX)");
         return (-1);
         }
                                         /* Otherwise, open socket    */
      unlink (device);                   /* first unlink if it exists */
      strcpy (server_un.sun_path, device);
      if (bind (MirSocket, (struct sockaddr *)&server_un,
        strlen(server_un.sun_path) + 2) < 0) {
         perror ("MakeLink: bind error (UNIX)");
         return (-1);
         }
      }
    listen (MirSocket, 5);             /* Queue up to 5 requests     */

                                        /* Set up the opcodes we want */
                                        /* to buffer. Basically all   */
                                        /* the writes.                */
   for (i = 0; i < (NUMOP+1); i++)
      bufferop[i] = False;
   if (buffered) {
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

   return (0);
} /* end MakeLink */
#endif

#if VMS
MakeLink()
{
    (mirname, MIR_SOCKET);
    (sxmtvname, SXMTV_SOCKET);
    int status,chan;

    /* create mailbox with logical name  "MirSocket" */

    status = sys (0, &chan, maxmsg, bufquo, 0, 0, &mirname);
    if (!(status & SS)) lib(status);

    MirLink = chan;

    /* create mailbox with logical name  "SxmtvSocket" */

    status = sys (0, &chan, maxmsg, bufquo, 0, 0, &sxmtvname);
    if (!(status & SS)) lib(status);

    SxmtvLink = chan;


} /* end MakeLink */
#endif

#if BSD

ReadLink (link, in, out)
int link;
Sxmtvinput *in;
Sxmtvoutput *out;
{
   register int i;
   int bytes_togo, bytes_trans;
   short int lbuf[6];
   char *abuf;
   static int size_i2 = sizeof(short int);

                                        /* read header                */
   abuf = (char *)lbuf;
   bytes_togo = 6 * size_i2;
   while (bytes_togo > 0) {
      bytes_trans = read(link, abuf, bytes_togo);
      if (bytes_trans < 0) {
        out->status = -1;               /* error -> shutdown          */
        return (-1);
      }
      bytes_togo -= bytes_trans;
      abuf       += bytes_trans;
   }

   out->status = OK;
   in->opcode = ntohs (lbuf[0]);
   in->data_length = ntohs (lbuf[5]);
   for (i = 0; i < NPARMS; i++ )
      in->parms[i] =  ntohs (lbuf[i+1]);

   /* Note: in->data[] is unsigned char and isn't swapped */
                                        /* "Variable length" data is  */
                                        /* in bytes, not words.       */
                                        /* Always read an even number */
                                        /* of bytes.                  */
   abuf = (char *)in->data;
   bytes_togo = in->data_length + (in->data_length)%2;
   while (bytes_togo > 0) {
      bytes_trans = read (link, abuf, bytes_togo);
      if (bytes_trans <= 0) {
         fprintf (stderr, "ReadLink read data error - shutdown?\n");
         out->status = -1;
         return (-1);
         }
      bytes_togo -= bytes_trans;
      abuf       += bytes_trans;
      }

   return (0);
} /* end ReadLink */


WriteLink (link, in, out)
int link;
Sxmtvoutput *out;
Sxmtvinput *in;
{
   register int i, j;
   int buflen;
   static int size_i2 = sizeof(short int);
   static short int packet[4096+2];

   buflen = out->return_data_length;
   if (!bufferop[in->opcode]) {
      packet[0] = htons (out->status);
      packet[1] = htons (out->return_data_length);
      i = 2; j = 0;
      while (j < buflen)
         packet[i++] = htons (out->data[j++]);
      buflen = (buflen + 2) * size_i2;
      if (write (link, packet, buflen) < buflen) {
         perror ("sxmtv:WriteLink - ");
         out->status = -1;
         return (-1);
         }
      }
   else {
      if (out->status != 0)
         fprintf (stderr, "Buffered op %d, istat=%d\n", in->opcode,
            out ->status);
      }
   out->status = OK;

   return (0);
} /* end WriteLink */
#endif

#if VMS

SxmtvMirReadAST()
{
   if (!(read_status.ioresult & SS))
      fprintf (stderr, "sxmtv:ReadLink - ");

   if (ybuf.status == OK)  ProcessMirRequest();

   read_in_progress = 0;

   WriteLink();

}

ReadLink()
{
   int s;

   if (read_in_progress) return (0);

   read_in_progress = 1;

/*********************!!!!!!!!!!!!!!!!!!!!******************/
/* This next line is a problem since xbuf.buf is no longer */
/* defined.  Need to upgrade this for the new structure!   */
/*********************!!!!!!!!!!!!!!!!!!!!******************/
   s = sys (0, SxmtvLink, IO, &read_status, &SxmtvMirReadAST,
      0, xbuf.buf, sizeof(xbuf.buf), 0, 0, 0, 0);
   if (!(s & SS)) {
      fprintf (stderr, "sxmtv:ReadLink - ");
      ybuf.status = -1;
      return (-1);
      }
   ybuf.status = OK;
   if (ByteSwapped) {
      xbuf.opcode = swapbytes (xbuf.opcode);
      xbuf.data_length = swapbytes (xbuf.data_length);
      for (i = 0; i < NPARMS; i++)
         xbuf.parms[i] = swapbytes (xbuf.parms[i]);
     /* Note: xbuf.data[] is unsigned char and isn't swapped */
      }
} /* end ReadLink */


/* This could be real strange if more than one process tries to   */
/* send commands to this process. This assumes that the "master"  */
/* process is doing IO synchronusly(since it must wait for status)*/
/* Question: should all error checking be done by the process     */
/*           thus eliminating the need for synchronous IO???      */

SxmtvMirWriteAST()
{
   if(!(write_status.ioresult & SS))
        fprintf(stderr,"sxmtv:WriteLink - ");


   write_in_progress = 0;

   /* Ready to do the next read */
   ReadLink();

   }

WriteLink()
{
    int s, i, buflen;

    if (write_in_progress)
       return (0);

   if (!bufferop[xbuf.opcode]) {
      write_in_progress = 1;

      buflen = ybuf.return_data_length;
      if (ByteSwapped) {
         ybuf.status = swapbytes (ybuf.status);
         ybuf.return_data_length = swapbytes (ybuf.return_data_length);

         /* the returned data is of type short int */
         for (i = 0; i < buflen; i++)
            ybuf.data[i] = swapbytes (ybuf.data[i]);
         }

      s = sys (0, MirLink, IO,
            &write_status, &SxmtvMirWriteAST, 0, ybuf.status,
            4+2*buflen, 0, 0, 0, 0);
      if (!(s & SS)) fprintf (stderr, "sxmtv:WriteLink - ");
      }

} /* end WriteLink */
#endif


closedown()
{
   int status, i;

   XFreeGC (display, ImageGC);
   for (i = 0; i < NGREY; i++ )
       XDestroyImage (plane[i]);
   XDestroyImage (graph);
   XDestroyImage (line);
   XCloseDisplay (display);

#if BSD
    if (domain_type == UNIX_DOMAIN)
        if( unlink(server_un.sun_path) < 0) {
            perror ("closedown: unlink");
        }
#endif

#if VMS
   status = sys(MirLink);
   if (!(status & SS)) lib(status);
   status = sys(SxmtvLink);
   if (!(status & SS)) lib(status);
#endif
   exit(1);
}

printbufin()
{
    int i, j, limit;

    fprintf (stderr,"parms[]= %4d %4d %4d %4d\n", xbuf.parms[0],
        xbuf.parms[1], xbuf.parms[2], xbuf.parms[3]);
    fprintf (stderr, "data_length= %d\n", xbuf.data_length);
    limit = 16;                                  /* xbuf.data_length; */
    for (i = 0; i < limit; i += 16) {
       for (j = i; (j < i+16) && (j < limit); j++)
          fprintf (stderr, " %4d", xbuf.data[j]);
       fprintf (stderr, "\n");
       }
} /* end printfbuf */

printbufout()
{
    int i, j, limit;

    fprintf (stderr, "status =  %4d\n", ybuf.status);
    fprintf (stderr, "parms[]= %4d %4d %4d %4d\n", xbuf.parms[0],
        xbuf.parms[1], xbuf.parms[2], xbuf.parms[3]);
    fprintf (stderr, "return_data_length= %d\n", ybuf.return_data_length);
    limit = ybuf.return_data_length;
    for (i = 0; i < limit; i += 16) {
       for (j = i; (j < i+16) && (j < limit); j++)
          fprintf (stderr," %4d", ybuf.data[j]);
       fprintf (stderr,"\n");
       }
} /* end printfbuf */

/*--------------------------------------------------------------------*/
