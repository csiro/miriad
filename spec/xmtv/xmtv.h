/*---------------------------------------------------------------------*/
/*  XMTV header file                                                   */
/*---------------------------------------------------------------------*/

#define BSD 1                            /* Select operating system;   */
/* #define VMS 0  */                     /* set desired system to 1.   */
#ifdef _AIX                             /* IBM AIX needs BSD = 1 also. */
#define AIX 1                                /* Use AIX specific code. */
#undef VMS                            /* Do not use VMS specific code. */
#endif

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/cursorfont.h>
#include <X11/Xaw/Simple.h>

#include "declare.h"                      /* Extern code declarations. */

                                         /* Screen parameters          */
#define NGREY    2
#define NGRAPH   4
                                        /* total number of planes      */
                                        /* (grey-scale + graphics)     */
#define NGRTOT   (NGREY+NGRAPH)
#define MAXZOOM  16
                                        /* size of logical screen      */
                                        /* must be EVEN numbers !      */
int Screen_Height, Screen_Width;
Position PortX, PortY;
Dimension PortW, PortH;
                                        /* number grey levels in:      */
                                        /* total levels: NColour+1-15  */
int NColour, NValue;                    /* for graphics, and cursor    */
                                        /* number of grey-scale (OFM)  */
                                        /* intensities                 */
#define NINTENS       256
#define COLORSHIFT      8               /* 255 max from ofm then shift */
                                        /* 8 bits left for col table   */

#define NPARMS 4

typedef struct {
   short int opcode;
   short int parms[NPARMS];
   short int data_length;
   unsigned char data[2048];
} XMTVinput;

typedef struct {
   short int return_data_length;
   short int status;
   short int data[1024];
} XMTVoutput;

XMTVinput xbuf;                          /* I/O buffer for XMTV        */
XMTVoutput ybuf;                         /* I/O buffer for XMTV        */

Boolean AppDebug;                                    /* Toggle with F8 */
Boolean XDebug;                                      /* Toggle with F9 */

Widget canvas;         /* Widget where all image and graphics happens. */

                                       /* Image data structures:       */
XImage *plane[NGREY];                  /* grey-scale planes            */
XImage *line;                          /* buffer for zoomed image line */

/* All graphs are kept in one plane via a binary trick.                */
/* A pixel value of 1 means only graph 1 on; 2 means only graph 2 on;  */
/* a pixel value of 4 means only graph 3 on; 8 means only graph 4 on;  */
/* a pixel value of 3 means both graph 1 and 2 on; 15, all on; etc.    */

XImage *graph;                         /* graphics overlay             */
unsigned char *plane_data[NGREY];      /* data storage                 */
unsigned char *line_data;
unsigned char *gline_data;
unsigned char *graph_data;

unsigned long int *int2pix;             /* input int <-> assigned pixv */
int *pix2int;

int cur_chan;
int rwgraph, depth;

int upleft_mag;
int upleft_x[NGREY], upleft_y[NGREY];  /* These are in "zoomed" units. */

int sc_centre_x, sc_centre_y;

#ifndef min
#define min(a,b) ((a) < (b) ? (a) : (b))
#endif

#ifndef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#endif

#define Memory_x(user_x)  ((user_x) - 1)
#define Memory_y(user_y)  (Screen_Height - (user_y))
#define User_x(memory_x)  ((memory_x) + 1)
#define User_y(memory_y)  (Screen_Height - (memory_y))
#define chg_s(var,val,msk) ((var) = ((var)&(~(msk)))|((val) ? (msk) : 0))
#define chg_g(var,msk) (((var)&(msk)) ? 1 : 0)

/* Defined opcodes */
#define NUMOP   84                      /* Largest opcode             */

int bufferop[NUMOP+1]; /* True if no status is returned in buffered mode. */

#define OPEN    11     /* Opens the XMTV connection                   */
#define CLOSE   12     /* Close the XMTV, allows new connections      */
#define INTGT   13     /* Interrogate: get XMTV major parameters      */
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
#define WZOOM   81     /* Write zoom info to the XMTV                 */
#define WSCROL  82     /* Write scroll registers                      */
#define WZSCR   83     /* Write zoom/scroll to XMTV using ULC         */
#define RZSCR   84     /* Read zoom/scroll registers.                 */

static char *opcodes[NUMOP+1] = {
    "CODE0 ", "CODE1 ", "CODE2 ", "CODE3 ", "CODE4 ",
    "CODE5 ", "CODE6 ", "CODE7 ", "CORD8 ", "CODE9 ",
    "CODE10", "OPEN  ", "CLOSE ", "INTGT ", "WINDO ",
    "CLEAR ", "CODE16", "CODE17", "CODE18", "CODE19",
    "CODE20", "IMWRT ", "IMRD  ", "CODE23", "CODE24",
    "CODE25", "CODE26", "CODE27", "CODE28", "SCALE ",
    "CODE30", "CODE31", "CODE32", "CODE33", "CODE34",
    "CODE35", "CODE36", "CODE37", "CODE38", "CODE39",
    "CODE40", "WLUT  ", "RLUT  ", "WOFM  ", "ROFM  ",
    "GRAPH ", "SPLIT ", "CODE47", "CODE48", "CODE49",
    "CODE50", "WGRFX ", "RGRFX ", "CODE53", "CODE54",
    "CODE55", "CODE56", "CODE57", "CODE58", "CODE59",
    "CODE60", "RCURS ", "RBUTT ", "WCURS ", "RCURB ",
    "CODE65", "CODE66", "CODE67", "CODE68", "CODE69",
    "CODE70", "SLCTP ", "CODE72", "CODE73", "CODE74",
    "CODE75", "CODE76", "CODE77", "CODE78", "CODE79",
    "CODE80", "WZOOM ", "WSCROL", "WZSCR ", "RZSCR "
};

static char *event_names[] = {
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
