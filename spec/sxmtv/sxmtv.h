/*--------------------------------------------------------------------*/
/* 06oct92 jm   Modified int2pix/pix2int/gph_mask from (int *) to     */
/*              (unsigned long *).  Also changed *_data pointers      */
/*              from (unsigned char *) to (char *).                   */
/*--------------------------------------------------------------------*/
#define BSD 1                           /* Select operating system    */
/* #define VMS 0  */                    /* set desired system to 1    */
#ifdef _AIX                             /* IBM AIX needs BSD = 1 also */
#define AIX 1                           /* use AIX specific code      */
#undef VMS                              /* Do not us VMS specific code*/
#endif

#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

#if BSD

#include <X11/bitmaps/xlogo64>

/* Header info needed for socket routines (including i/o) */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netdb.h>
#include <netinet/in.h>

struct sockaddr_un server_un;
struct sockaddr_in server_in;
struct servent *sp_in;

int MirSocket, MirLink;
int domain_type;
#define UNIX_DOMAIN 0
#define INET_DOMAIN 1

#endif /* BSD */


#if VMS

/* ##include "xlogo64." */
#include <ssdef.h>
#include <iodef.h>
#include <descrip.h>

#define SXMTV_SOCKET "SxmtvSocket"
#define MIR_SOCKET "MirSocket"

#define emask ExposureMask|KeyPressMask|StructureNotifyMask|PointerMotionMask

#define maxmsg 2064
#define bufquo 2064

typedef struct {
   short int ioresult;
   short int iolength;
   int unused;
} IOSB;

int SxmtvLink, MirLink;
IOSB read_status, write_status;
int read_in_progress, write_in_progress;
static  (dtime, "0 00:00:00.25");
int delta[2];

#endif /* VMS */

/************* for old versions, make them define X11R3 manually.
#ifndef X11R4
#define X11R3
#endif
*************/
                                        /* Screen parameters          */
#define NGREY    2
#define NGRAPH   4
                                        /* total number of planes      */
                                        /* (grey-scale + graphics)     */
#define NGRTOT   (NGREY+NGRAPH)
#define MAXZOOM  16
                                        /* Border allocations         */
/* #define TEXT_SPACE      0 */         /* empty space @ bottom screen*/
#define TEXT_SPACE      69              /* preferred at NRAO?         */
                                        /* I can find no way to ask   */
                                        /* the window manager the size*/
                                        /* of its top banner.         */
                                        /* They need to be visible to */
                                        /* allow window resize, move..*/
#ifdef AIX                              /* Numbers for IBM Motif      */
#define SCREEN_LEFT    11
#define SCREEN_RIGHT   11
#define SCREEN_TOP     34
#define SCREEN_BOTTOM  (11 + TEXT_SPACE)
#else                                   /* Numbers for SUN OpenLook   */
#define SCREEN_LEFT     5
#define SCREEN_RIGHT    5
#define SCREEN_TOP     26
#define SCREEN_BOTTOM   (5 + TEXT_SPACE)
#endif
                                        /* total screen size          */
                                        /* used in positioning        */
int twidth, theight, bwid;
                                        /* size of logical screen      */
                                        /* must be EVEN numbers !      */
int Screen_Height, Screen_Width;
int Cur_Xsize, Cur_Ysize, Cur_Xzero, Cur_Yzero;
                                        /* number grey levels in:      */
                                        /* total levels: NColour+1-15  */
int NColour, NValue;                    /* for graphics, and cursor    */
                                        /* number of grey-scale (OFM)  */
                                        /* intensities                 */
#define NINTENS       256
#define COLORSHIFT      8               /* 255 max from ofm then shift */
                                        /* 8 bits left for col table   */

                                        /* cursor [0], graphics [1-4]  */
int rgrfx[5], ggrfx[5], bgrfx[5], rgcol[16], ggcol[16], bgcol[16];

#define OK 0

#define NPARMS 4
typedef struct {
   short int
      opcode,
      parms[NPARMS],
      data_length;
   unsigned char data[2048];
} Sxmtvinput;
typedef struct {
   short int
      return_data_length,
      status;
      short int data[1024];
} Sxmtvoutput;
/* unsigned char buf[2064]; */

                                        /* Global variables           */
Sxmtvinput xbuf;                          /* I/O buffer                 */
Sxmtvoutput ybuf;                         /* I/O buffer                 */
Bool SXMTVDebug;                        /* Toggle with F8 */
Bool XDebug;                            /* Toggle with F9 */
Bool ByteSwapped;                       /* Flag for byte swapped machine */
Display *display;
int screen_num;
Cursor cursor;
Window win;
GC ImageGC;                             /* X11 graphics contexts for   */
                                        /* drawing images & graphics   */

                                        /* Image data structures:       */
XImage *plane[NGREY];                   /* grey-scale planes            */
XImage *line;                           /* buffer for zoomed image line */
XImage *gline;                          /* buffer for zoomed graphics   */
                                        /* line                         */
/* All graphs are kept in one plane via a binary trick                  */
/* a pixel value of 1 means only graph 1 on, 2 means only graph 2 on    */
/* a pixel value of 4 means only graph 3 on, 8 means only graph 4 on    */
/* a pixel value of 3 means both graph 1 and 2 on, 15 means all on, etc */
XImage *graph;                          /* graphics overlay             */
char *plane_data[NGREY];                /* data storage                 */
char *line_data;
char *gline_data;
char *graph_data;

int depth;
unsigned long int rwgraph;
unsigned long int gph_mask;

int rofm[NINTENS], gofm[NINTENS],       /* red, green and blue OFM     */
    bofm[NINTENS];                      /* registers                   */
                                        /* red, green and blue LUT     */
int *rlut[NGREY], *glut[NGREY], *blut[NGREY];
unsigned long int *int2pix, *pix2int;   /* input int <-> assigned pixv */

int TvStatus[NGREY+NGRAPH];             /* TV Image + Graphics status  */

int cur_chan;

Colormap TV_colour;
XColor *colour_table;
XColor fg_curs, bg_curs;
unsigned char image_offset;
unsigned char graphics_offset;

int upleft_x[NGREY], upleft_y[NGREY], upleft_mag;
int sc_centre_x, sc_centre_y, sc_zoom_mag;
int sc_width, sc_height, sc_width2, sc_height2;

                                        /* Useful macros              */
#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif

#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif

#define intswap(a,b) { int tmp; tmp=a; a=b; b=tmp; }

#define Memory_x(Coord_x)  ((Coord_x) - 1)
#define Memory_y(Coord_y)  (Screen_Height - (Coord_y))
#define coord_x(memory_x)  ((memory_x) + 1)
#define coord_y(memory_y)  (Screen_Height - (memory_y))
#define chg_s(var,val,msk) ((var) = ((var)&(~(msk)))|((val) ? (msk) : 0))
#define chg_g(var,msk) (((var)&(msk)) ? 1 : 0)

/* Defined opcodes */
#define NUMOP   84                      /* Largest opcode             */

int bufferop[NUMOP+1];                  /* bufferop[OP] is True if    */
                                        /* there is no status return  */
                                        /* in buffered mode           */
#define OPEN    11     /* Opens the SXMTV connection                  */
#define CLOSE   12     /* Close the SXMTV, allows new connections     */
#define INTGT   13     /* Interrogate: get SXMTV major parameters     */
#define WINDO   14     /* Read, write the X window size               */
#define CLEAR   15     /* Clear some or all channels                  */
#define IMWRT   21     /* Write image line to some channel            */
#define IMRD    22     /* Read image line from some channel,          */
#define SCALE   29     /* Set image scale for image,                  */
#define WLUT    41     /* Write LUT to a channel.                     */
#define RLUT    42     /* Read LUT to a channel.                      */
#define WOFM    43     /* Write OFM.                                  */
#define ROFM    44     /* Read OFM.                                   */
#define GRAPH   45     /* On/off graphics channel(s)                  */
#define SPLIT   46     /* On/off image channels(s)                    */
#define WGRFX   51     /* Write graphics/cursor colours               */
#define RGRFX   52     /* Read  graphics/cursor colours               */
#define RCURS   61     /* Read the cursor position.                   */
#define RBUTT   62     /* Read the status of the buttons              */
#define WCURS   63     /* Write the cursor position.                  */
#define RCURB   64     /* Read the cursor position and buttons        */
#define SLCTP   71     /* Point selection.                            */
#define WZOOM   81     /* Write zoom info to the SXMTV                */
#define WSCROL  82     /* Write scroll registers                      */
#define WZSCR   83     /* Write zoom/scroll to SXMTV using ULC        */
#define RZSCR   84     /* Read zoom/scroll registers.                 */

static char *opcodes[NUMOP+1] = {

"CODE0 ","CODE1 ","CODE2 ","CODE3 ","CODE4 ",
"CODE5 ","CODE6 ","CODE7 ","CORD8 ","CODE9 ",
"CODE10","OPEN  ","CLOSE ","INTGT ","WINDO ",
"CLEAR ","CODE16","CODE17","CODE18","CODE19",
"CODE20","IMWRT ","IMRD  ","CODE23","CODE24",
"CODE25","CODE26","CODE27","CODE28","SCALE ",
"CODE30","CODE31","CODE32","CODE33","CODE34",
"CODE35","CODE36","CODE37","CODE38","CODE39",
"CODE40","WLUT  ","RLUT  ","WOFM  ","ROFM  ",
"GRAPH ","SPLIT ","CODE47","CODE48","CODE49",
"CODE50","WGRFX ","RGRFX ","CODE53","CODE54",
"CODE55","CODE56","CODE57","CODE58","CODE59",
"CODE60","RCURS ","RBUTT ","WCURS ","RCURB ",
"CODE65","CODE66","CODE67","CODE68","CODE69",
"CODE70","SLCTP ","CODE72","CODE73","CODE74",
"CODE75","CODE76","CODE77","CODE78","CODE79",
"CODE80","WZOOM ","WSCROL","WZSCR ","RZSCR "

   };

static char  *event_names[] = {
        "",
        "",
        "KeyPress",
        "KeyRelease",
        "ButtonPress",
        "ButtonRelease",
        "MotionNotify",
        "EnterNotify",
        "LeaveNotify",
        "FocusIn",
        "FocusOut",
        "KeymapNotify",
        "Expose",
        "GraphicsExpose",
        "NoExpose",
        "VisibilityNotify",
        "CreateNotify",
        "DestroyNotify",
        "UnmapNotify",
        "MapNotify",
        "MapRequest",
        "ReparentNotify",
        "ConfigureNotify",
        "ConfigureRequest",
        "GravityNotify",
        "ResizeRequest",
        "CirculateNotify",
        "CirculateRequest",
        "PropertyNotify",
        "SelectionClear",
        "SelectionRequest",
        "SelectionNotify",
        "ColormapNotify",
        "ClientMessage",
        "MappingNotify"
   };
