/*= XMTV - X-window TV screen display server. */
/*& jm */
/*: visual display */
/*+      XMTV is an X-window Screen server based on the Athena (Xaw)
         widget set.  It is only available when running X-windows and
         it should be invoked separately before being used.  To invoke,
         it is best to start the process in the background.  Remember
         that the environment variable DISPLAY may be set to the name of
         your local machine prior to running XMTV to display it remotely.

         There are several resources that may be set to customize your
         version of XMTV.  In addition to the standard widget options,
         the following resources may be set (either in an .Xdefaults
         file or via command line overrides).

         Resource file options:
           Instance Name    Class Name      Default Value
         -------------------------------------------------------------
           iconGeometry     IconGeometry    NULL  (Random placement)
           XDebug           Debug           False (Turned off)
           AppDebug         Debug           False (Turned off)
           useInet          UseInet         True  (Use Internet)
           buffered         Buffered        True  (Buffer some commands)
           service          Service         NULL  (No service name)
           cursor           Cursor          NULL  (No cursor defined)
           cursorName       Cursor          crosshair
           cursorColor      CursorColor     magenta
           graphics1Color   GraphicsColor   yellow
           graphics2Color   GraphicsColor   chartreuse
           graphics3Color   GraphicsColor   plum1
           graphics4Color   GraphicsColor   black
           portNumber       PortNumber      5000
         -------------------------------------------------------------

         Command line options:
           Option Name      Default Value
         -------------------------------------------------------------
           -icongeometry",  NULL  (Random placement)
           -iconGeometry",  NULL  (Random placement)
           -ig",            NULL  (Random placement)
           -debug",         True  (XDebug and AppDebug turned on)
           -xdebug",        True  (XDebug turned on)
           -appdebug",      True  (AppDebug turned on)
           -inet",          True  (Internet communication turned on)
           -unix",          False (Unix socket communication turned on)
           -buffer",        True  (Buffering of some commands turned on)
           -buffered",      True  (Buffering of some commands turned on)
           -nobuffer",      False (Buffering of some commands turned off)
           -service",       NULL  (No service name specified)
           -port",          NULL  (Use default Port number: 5000)
         -------------------------------------------------------------

         Widget tree layout (indentation represents parentage):
           Widget Class Name : Widget Instance Name
         -------------------------------------------------------------
           XMtv : xmtv
               Paned : paned
                   Grip : grip
                   Porthole : porthole
                       Simple : canvas
                   Form : form
                       Label : cursorMotion
                       Command : A
                       Command : B
                       Command : C
                       Command : D
                       Command : +
                       Command : -
                       Command : Reset
                       Command : Resize
                       Command : Quit
                       Command : Panner
                           TransientShell : panshell
                               Form : panform
                                   Toggle : pan
                                   Toggle : fiddle
                                   Command : close
                                   Panner : panner
                       Command : Luts
                           TransientShell : lutmenu
                               Box : menuBox
                                   Toggle : B&W
                                   Toggle : Colour
                                   Toggle : IRAF
                                   Toggle : -B&W
                                   Toggle : -Colour
                                   Toggle : -IRAF
                       Command : Channels
                           TransientShell : channelmenu
                               Box : menuBox
                                   Toggle : 1
                                   Toggle : 2
         -------------------------------------------------------------
 */
/*--*/
/************************************************************************/
/*                                                                      */
/*  XMTV is a TCP/IP and Unix Socket server based on X-windows and the  */
/*  Athena widget set.  Hence, it is necessary to link these routines   */
/*  with the following libraries (in the order listed):                 */
/*     -lXaw -lXmu -lXt [-lXext] -lX11                                  */
/*  Note: the Xext library may be optionally included or omitted.       */
/*  The Xext library supplies code to generate rounded Command buttons. */
/*                                                                      */
/*  History:                                                            */
/*    01jul92 jm  Original code.                                        */
/*    11nov92 jm  Fixed a memory allocation bug in initImage().         */
/*                Also initialized and provided checks for file         */
/*                static (private) variables in widgets.c.              */
/*    14oct93 rjs Close socket when done.				*/
/************************************************************************/
#include "xmtv.h"
#include "xmtv.icon"
#include "version.h"

#define DEFAULTPORT 5000
#define CHECKTIME 100   /* Number of milli-s to wait before */
                        /* looking for new connections. */

/* Private variables. */

static Boolean connected = False;

static XtActionsRec actions[] = {
  {"cursorMotionEvent", (XtActionProc)cursorMotionEvent},
  {"keyboardPressed",   (XtActionProc)keyboardPressed},
  {"buttonEvent",       (XtActionProc)buttonEvent},
  {"quit",              (XtActionProc)CloseDown},
  {"canvasExpose",      (XtActionProc)canvasExpose},
  {NULL, NULL}
};

typedef struct {
  String   icon_geometry;
  Boolean  XDebug;
  Boolean  AppDebug;
  Boolean  use_inet;
  Boolean  buffered;
  String   service;
  Cursor   cursor;
  String   cursor_name;
  Pixel    cursor_color;
  Pixel    graphics1_color;
  Pixel    graphics2_color;
  Pixel    graphics3_color;
  Pixel    graphics4_color;
  int      port_number;
} ApplicationData;

#define Offset(field) XtOffsetOf(ApplicationData, field)
static XtResource resources[] = {
  {"iconGeometry",    "IconGeometry",   XtRString,       sizeof(String),
     Offset(icon_geometry),     XtRString,      (XtPointer)NULL},
  {"XDebug",          "Debug",          XtRBoolean,      sizeof(Boolean),
     Offset(XDebug),            XtRImmediate,   (XtPointer)False},
  {"AppDebug",        "Debug",          XtRBoolean,      sizeof(Boolean),
     Offset(AppDebug),          XtRImmediate,   (XtPointer)False},
  {"useInet",         "UseInet",        XtRBoolean,      sizeof(Boolean),
     Offset(use_inet),          XtRImmediate,   (XtPointer)True},
  {"buffered",        "Buffered",       XtRBoolean,      sizeof(Boolean),
     Offset(buffered),          XtRImmediate,   (XtPointer)True},
  {"service",         "Service",        XtRString,       sizeof(String),
     Offset(service),           XtRString,      (XtPointer)NULL},
  {XtNcursor,         XtCCursor,        XtRCursor,       sizeof(Cursor),
     Offset(cursor),            XtRImmediate,   (XtPointer)NULL},
  {XtNcursorName,     XtCCursor,        XtRString,       sizeof(String),
     Offset(cursor_name),       XtRString,      (XtPointer)"crosshair"},
  {"cursorColor",     "CursorColor",    XtRPixel,        sizeof(Pixel),
     Offset(cursor_color),      XtRString,      (XtPointer)"magenta"},
  {"graphics1Color",  "GraphicsColor",  XtRPixel,        sizeof(Pixel),
     Offset(graphics1_color),   XtRString,      (XtPointer)"yellow"},
  {"graphics2Color",  "GraphicsColor",  XtRPixel,        sizeof(Pixel),
     Offset(graphics2_color),   XtRString,      (XtPointer)"chartreuse"},
  {"graphics3Color",  "GraphicsColor",  XtRPixel,        sizeof(Pixel),
     Offset(graphics3_color),   XtRString,      (XtPointer)"plum1"},
  {"graphics4Color",  "GraphicsColor",  XtRPixel,        sizeof(Pixel),
     Offset(graphics4_color),   XtRString,      (XtPointer)"black"},
  {"portNumber",      "PortNumber",     XtRInt,          sizeof(int),
     Offset(port_number),       XtRImmediate,   (XtPointer)DEFAULTPORT},
};
#undef Offset

static XrmOptionDescRec options[] = {
  {"-icongeometry",  "iconGeometry",   XrmoptionSepArg, (XtPointer)NULL},
  {"-iconGeometry",  "iconGeometry",   XrmoptionSepArg, (XtPointer)NULL},
  {"-ig",            "iconGeometry",   XrmoptionSepArg, (XtPointer)NULL},
  {"-debug",         "Debug",          XrmoptionNoArg,  (XtPointer)"True"},
  {"-xdebug",        "XDebug",         XrmoptionNoArg,  (XtPointer)"True"},
  {"-appdebug",      "AppDebug",       XrmoptionNoArg,  (XtPointer)"True"},
  {"-inet",          "useInet",        XrmoptionNoArg,  (XtPointer)"True"},
  {"-unix",          "useInet",        XrmoptionNoArg,  (XtPointer)"False"},
  {"-buffer",        "buffered",       XrmoptionNoArg,  (XtPointer)"True"},
  {"-buffered",      "buffered",       XrmoptionNoArg,  (XtPointer)"True"},
  {"-nobuffer",      "buffered",       XrmoptionNoArg,  (XtPointer)"False"},
  {"-service",       "service",        XrmoptionSepArg, (XtPointer)NULL},
  {"-port",          "portNumber",     XrmoptionSepArg, (XtPointer)NULL},
};

static char *fallback_resources[] = {
  "*Box.hSpace:                 1",
  "*Box.vSpace:                 1",
  "*Luts.leftBitmap:            menu10",
  "*Channels.leftBitmap:        menu10",
  "*paned.form*top:             ChainTop",
  "*paned.form*bottom:          ChainTop",
  "*paned.form*left:            ChainLeft",
  "*paned.form*right:           ChainLeft",
  "*panform*top:                ChainTop",
  "*panform*bottom:             ChainTop",
  "*panform*left:               ChainLeft",
  "*panform*right:              ChainLeft",
  "*porthole*showGrip:          False",
  "*panner*cursorName:          hand2",
  "*panner.resize:              False",
  "*panner.width:               200",
  "*panner.height:              200",
  "*Toggle.baseTranslations:   #override        \\n\
      <EnterWindow>:        highlight(Always)   \\n\
      <LeaveWindow>:        unhighlight()       \\n\
      <Btn1Down>,<Btn1Up>:  set() notify()      \\n\
      <Btn3Up>:             set() notify()",
  "*paned.form.A.accelerators: #override        \\n\
      <KeyPress>a:          set()               \\n\
      <KeyRelease>(1+)a:    notify() unset()    \\n\
      <KeyPress>F3:         set()               \\n\
      <KeyRelease>(1+)F3:   notify() unset()    \\n\
      <KeyPress>KP_F1:      set()               \\n\
      <KeyRelease>(1+)KP_F1: notify() unset()",
  "*paned.form.B.accelerators: #override        \\n\
      <KeyPress>b:          set()               \\n\
      <KeyRelease>(1+)b:    notify() unset()    \\n\
      <KeyPress>F4:         set()               \\n\
      <KeyRelease>(1+)F4:   notify() unset()    \\n\
      <KeyPress>KP_F2:      set()               \\n\
      <KeyRelease>(1+)KP_F2: notify() unset()",
  "*paned.form.C.accelerators: #override        \\n\
      <KeyPress>c:          set()               \\n\
      <KeyRelease>(1+)c:    notify() unset()    \\n\
      <KeyPress>F5:         set()               \\n\
      <KeyRelease>(1+)F5:   notify() unset()    \\n\
      <KeyPress>KP_F3:      set()               \\n\
      <KeyRelease>(1+)KP_F3: notify() unset()",
  "*paned.form.D.accelerators: #override        \\n\
      <KeyPress>d:          set()               \\n\
      <KeyRelease>(1+)d:    notify() unset()    \\n\
      <KeyPress>F6:         set()               \\n\
      <KeyRelease>(1+)F6:   notify() unset()    \\n\
      <KeyPress>KP_F4:      set()               \\n\
      <KeyRelease>(1+)KP_F4: notify() unset()",
  "*paned.form.Resize.accelerators: #override   \\n\
      <KeyPress>F2:         set()               \\n\
      <KeyRelease>(1+)F2:   notify() unset()    \\n\
      <KeyPress>F7:         set()               \\n\
      <KeyRelease>(1+)F7:   notify() unset()    \\n\
      <KeyPress>KP_Add:     set()               \\n\
      <KeyRelease>(1+)KP_Add: notify() unset()",
  "*canvas.baseTranslations:   #override        \\n\
      <Key>F8:              buttonEvent(F8)     \\n\
      <Key>F9:              buttonEvent(F9)     \\n\
      <Btn1Down>:           buttonEvent()       \\n\
      <Btn1Motion>:         buttonEvent()       \\n\
      <Key>F20:             quit()              \\n\
      <Key>Escape:          quit()              \\n\
      <EnterNotify>:        keyboardPressed()   \\n\
      <LeaveNotify>:        keyboardPressed()   \\n\
      <Motion>:             cursorMotionEvent() \\n\
      <Expose>:             canvasExpose()",
  NULL,
};

/* Global variables. */

XtAppContext context;
static XtTimerCallbackProc checkSocket();

/* Source code. */

/************************************************************************/
static void bug(string)
char *string;
{
    perror(string);
    exit(-1);
}

/************************************************************************/
static void privateXtAppMainLoop(context)
XtAppContext context;
{
    XEvent event;

    for (;;) {
      XtAppNextEvent(context, &event);
      if (XDebug) (void)fprintf(stderr, "%s\n", event_names[event.type]);
      XtDispatchEvent(&event);
    }
}

/************************************************************************/
static void startTimer(socketID)
int socketID;
{
    unsigned long interval = CHECKTIME; /* in milli-seconds */

    (void)XtAppAddTimeOut(context, interval, checkSocket, (XtPointer)socketID);
}

/************************************************************************/
static void initializeSocket(portNumber, useInet, buffered, service)
int portNumber;
Boolean useInet;
Boolean buffered;
String service;
{
    int listenSocket;
    unsigned int Port;

/* Get a socket to the outside world, and bind to it. */

    Port = (unsigned int)portNumber;
    if ((useInet == True) && (Port != DEFAULTPORT))
      (void)fprintf(stderr, "%s from the default value of %d to %d\n",
          "XMTV: Re-assigning the I/O Port number", DEFAULTPORT, Port);

    listenSocket = MakeLink(useInet, buffered, service, Port);
    if (listenSocket < 0)
      bug("initializeSocket - MakeLink error");

    connected = False;
    startTimer(listenSocket);
}

/************************************************************************/
/* ARGSUSED */
static XtInputCallbackProc readSocket(client_data, iosocket, inputID)
XtPointer client_data;  /* Unused */
int *iosocket;
XtInputId *inputID;
{
    int fd = (int)(*iosocket);

    if (ReadLink(fd, &xbuf) < 0)
      xbuf.opcode = CLOSE;

    if (ProcessThisRequest() < 0) {
      XtRemoveInput(*inputID);
      connected = False;
      (void)close(fd);
    }

    if (connected == True)
      WriteLink(fd, &xbuf, &ybuf);
}

/************************************************************************/
/* ARGSUSED */
static XtTimerCallbackProc checkSocket(client_data, ptrTimer)
XtPointer client_data;
XtIntervalId *ptrTimer;
{
    int listenSocket, ioSocketId;
    XtInputId FdId;
    XtInputMask condition = XtInputReadMask;

    listenSocket = (int)client_data;
    startTimer(listenSocket);

    if (connected == False) {
      if ((ioSocketId = CheckLink(listenSocket)) < 0)
        bug("checkSocket:accept");
      if (ioSocketId > 0) {
        FdId = XtAppAddInput(context, ioSocketId, (XtPointer)condition,
          readSocket, (XtPointer)NULL);
        connected = True;
      }
    }
    return;
}

/************************************************************************/
static void ParseIconGeometry(w, str, x, y, iconw, iconh)
Widget w;
String str;
int *x, *y;
int iconw, iconh;
{
    int bitmask, RootWidth, RootHeight;
    unsigned int width, height;

    RootWidth = XDisplayWidth(XtDisplay(w), DefaultScreen(XtDisplay(w))) - 1;
    RootHeight = XDisplayHeight(XtDisplay(w), DefaultScreen(XtDisplay(w))) - 1;

    *x = 0;
    *y = 0;
    bitmask = XParseGeometry(str, x, y, &width, &height);
    if ((bitmask & (XNegative|XValue)) == (XNegative|XValue))
      *x += (RootWidth - iconw);
    if ((bitmask & (YNegative|YValue)) == (YNegative|YValue))
      *y += (RootHeight - iconh);
}

/************************************************************************/
static int initScreen(topwidget)
Widget topwidget;
{
    int displayDepth, screen_num;

    screen_num = DefaultScreen(XtDisplay(topwidget));

                                     /* Get root (full) screen size.  */
    Screen_Width = DisplayWidth(XtDisplay(topwidget), screen_num);
    Screen_Width = (Screen_Width / 2) * 2;
    Screen_Height = DisplayHeight(XtDisplay(topwidget), screen_num);
    Screen_Height = (Screen_Height / 2) * 2;
    sc_centre_x = (Screen_Width / 2) - 1;
    sc_centre_y = (Screen_Height / 2) - 1;

        /* Check that the server has sufficient depth for our needs.  */
    if ((displayDepth = DisplayPlanes(XtDisplay(topwidget), screen_num)) < 6) {
      (void)fprintf(stderr, "XMTV: X server color table too small\n");
      (void)fprintf(stderr, "XMTV: Image display not possible\n");
      (void)fprintf(stderr, "XMTV: Exiting...\n");
      exit(-1);
    }

    if (displayDepth > 8) {
      fprintf (stderr, "*******************************************\n");
      fprintf (stderr, "**  depth = %d > 8!", displayDepth);
      fprintf (stderr, "  Sorry, it must be limited to 8\n");
      fprintf (stderr, "** due to the 1-character I/O limit.\n");  
      fprintf (stderr, "*******************************************\n");
      displayDepth = 8;
    }

    NValue = 1 << displayDepth;
    NValue = NValue - 40;                /* Be considerate, leave some. */
    NValue = min(NValue, 256);           /* Forced to use 8-bit chars.  */
    NColour = NValue - (1 << NGRAPH);

    (void)fprintf(stderr, "Screen width and height: %dX%d,",
      Screen_Width, Screen_Height);
    (void)fprintf(stderr, "  the maximum grey level is %d.\n", NColour - 1);

    return(displayDepth);
}

/************************************************************************/
static XVisualInfo *getVisualList(topwidget, screenDepth)
Widget topwidget;
int screenDepth;
{
    int nvis;                        /* Number of visuals in the list.  */
    XVisualInfo template;                 /* type of visual XMTV uses.  */
    XVisualInfo *vislist;    /* list of visuals matching the template.  */

    template.screen = DefaultScreen(XtDisplay(topwidget));
    template.depth = screenDepth;
    template.class = PseudoColor;

    vislist = XGetVisualInfo(XtDisplay(topwidget),
      (VisualScreenMask|VisualDepthMask|VisualClassMask), &template, &nvis);
 
    if (nvis == 0) {
      perror("No suitable visual");
      return(NULL);
    }

    return(vislist);
}

/************************************************************************/
/* ARGSUSED */
XtActionProc CloseDown(w, event, params, nparams)
Widget w;          /* Unused */
XEvent *event;     /* Unused */
String *params;    /* Unused */
Cardinal *nparams; /* Unused */
{
/*
    Shut down the link; close down all windows; and exit.
------------------------------------------------------------------------*/

    freeCanvas();
    freeColors();
    freeImage();

    XtDestroyApplicationContext(context);
    closeLink();
    exit(1);
}

/************************************************************************/
int main(argc, argv)
int argc;
char *argv[];
{
    char title[256];
    int x, y;
    int screenDepth;
    ApplicationData AppData;
    Arg args[10];
    Cardinal i;
    Pixmap iconPixmap;
    Widget toplevel;
    XVisualInfo *visualList; /* List of visuals used to get a colormap. */

    /* Create the base level frame. */

    (void)sprintf(title,"XMTV: %s", VERSION);
    (void)fprintf(stderr,"%s\n", title);

    toplevel = XtAppInitialize(&context, "XMtv", options, XtNumber(options),
       (Cardinal *)&argc, argv, fallback_resources, NULL, (Cardinal)0);

    XtGetApplicationResources(toplevel, (XtPointer)&AppData,
       resources, XtNumber(resources), (ArgList)NULL, (Cardinal)0);

    XtAppAddActions(context, actions, XtNumber(actions));

    screenDepth = initScreen(toplevel);
    visualList = getVisualList(toplevel, screenDepth);
    if (visualList == (XVisualInfo *)NULL)
      bug("Trouble getting a visual list -- exiting.");

    /* Create all of the images.  */
    if (initImage(toplevel, visualList, screenDepth) != 0)
      bug("Trouble allocating storage for XImages.");

    /* Build all the subwindows.... */
    canvas = BuildWindow(toplevel, NGREY, screenDepth,
      AppData.cursor, AppData.cursor_name,
      (Dimension)Screen_Width, (Dimension)Screen_Height);

    /* Geometry should really apply to the canvas!! */
    /* canvas is a global variable. */

    XtSetMappedWhenManaged(toplevel, False);
    XtRealizeWidget(toplevel);

    if (initColors(toplevel, visualList, screenDepth) != 0)
      bug("Trouble allocating storage for the color table.");

    iconPixmap = XCreateBitmapFromData(XtDisplay(toplevel),
       XtWindow(toplevel), xmtv_bits, xmtv_width, xmtv_height);

    i = 0;
    XtSetArg(args[i], XtNminWidth,          (XtArgVal)100); i++;
    XtSetArg(args[i], XtNminHeight,         (XtArgVal)100); i++;
    XtSetArg(args[i], XtNinput,            (XtArgVal)True); i++;
    XtSetArg(args[i], XtNiconPixmap, (XtArgVal)iconPixmap); i++;
    XtSetArg(args[i], XtNtitle,           (XtArgVal)title); i++;
    XtSetValues(toplevel, args, i);

    if (AppData.icon_geometry) {
      ParseIconGeometry(toplevel, AppData.icon_geometry, &x, &y,
        xmtv_width, xmtv_height);
      i = 0;
      XtSetArg(args[i], XtNiconX,             (XtArgVal)x); i++;
      XtSetArg(args[i], XtNiconY,             (XtArgVal)y); i++;
      XtSetValues(toplevel, args, i);
    }

                                    /* Set some global variable values. */
    depth = screenDepth;
    XDebug = AppData.XDebug;
    AppDebug = AppData.AppDebug;

    initGraphics(XtDisplay(canvas), AppData.cursor_color,
      AppData.graphics1_color, AppData.graphics2_color,
      AppData.graphics3_color, AppData.graphics4_color);

    initCanvas(canvas);

    (void)localReset(NULL, NULL, NULL);
    (void)clearChannel(0);

    initializeSocket(AppData.port_number, AppData.use_inet, AppData.buffered,
      AppData.service);

    XtSetMappedWhenManaged(toplevel, True);
    XtMapWidget(toplevel);

    /* Because of XDebug; call my own MainLoop routine. */
    privateXtAppMainLoop(context);
}
