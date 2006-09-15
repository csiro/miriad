#define VERSION_ID  "21-sep-91"
/*= XPANEL - X-window control panel program
/*& jm
/*: tools
/*+
XPanel is a TCP/IP server based on X-windows, which listens for
connections from the Miriad "ctrl" routines, constructs a
control panel according to commands from the Ctrl routines,
and allows the user and programmer to communicate through the
control panel.

See the Miriad Programmers manual, for documentation of the Ctrl
routines.

For X users familiar with application resources, the user may
set up characteristics for individual panel items.  The Widget
tree layout used by Xpanel is shown below:

	Level 1:
	Top level shell:	"Xpanel"

	Level 2:
	Child of "Xpanel":	"subframe"

	Level 3:
	Child of "subframe":	"panel"      (formWidget)

	Level 4:
	Children of "panel":	"button"     (commandWidget)
				"list"       (commandWidget)
				"status"     (labelWidget)
				"slider"     (formWidget)
				"cursor"     (coreWidget)

	Level 5:
	Child of "list":	"ButtonList" (transientWidget)

	Children of "slider":	"Slab"       (labelWidget)
				"Sval"       (labelWidget)
				"Smin"       (labelWidget)
				"Scroll"     (scrollbarWidget)
				"Smax"       (labelWidget)

	Level 6:
	Child of "ButtonList":	"MenuBox"    (boxWidget)

	Level 7:
	Child of "MenuBox":     list entry  (commandWidget)

In addition to the above widget items, there are application
resources that the user may set in their application file or
on the command line.  In addition to the usual X-window resources,
Xpanel provides the following resources, along with their defaults:

   *cursorForeground:	"XtDefaultForeground" (Fore/Background color of)
   *cursorBackground:	"XtDefaultBackground" (... cursor in coreWidget)
   *noIconic:		False            (Start application non-iconic)
   *Font:		"9x15"                (Font used for strings)
   *portNumber:		5001	   (Port number used for communications)
/*--

  History:

     jm  16sep91 Original based on SunView panel routine.
     mjs 21sep93 #define bzero -> memset on sun; make in-code docs
                 compatible with miriad docs; make icon "unsigned
                 char".
/************************************************************************/
#ifdef sun
#define bzero(a,b) memset((a),0,(b))
#endif
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/cursorfont.h>
#include <X11/Xmu/Misc.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Scrollbar.h>
#include <netdb.h>
#include <netinet/in.h>
#define xpanel_width 64
#define xpanel_height 64
static unsigned char xpanel_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x18, 0x18, 0x8f, 0x3f, 0x3c, 0xfe, 0xf1, 0x07, 0x38, 0x1c, 0x8f, 0x7f,
   0x3c, 0xfe, 0xf1, 0x0f, 0x38, 0x1c, 0x86, 0x61, 0x18, 0x86, 0x31, 0x0c,
   0x78, 0x1e, 0x86, 0x61, 0x18, 0x86, 0x31, 0x0c, 0x78, 0x1e, 0x86, 0x61,
   0x18, 0x86, 0x31, 0x0c, 0xd8, 0x1b, 0x86, 0x7f, 0x18, 0xfe, 0x31, 0x0c,
   0x98, 0x19, 0x86, 0x3f, 0x18, 0xfe, 0x31, 0x0c, 0x98, 0x19, 0x86, 0x19,
   0x18, 0x86, 0x31, 0x0c, 0x18, 0x18, 0x86, 0x39, 0x18, 0x86, 0x31, 0x0c,
   0x18, 0x18, 0x86, 0x31, 0x18, 0x86, 0x31, 0x0c, 0x18, 0x18, 0x86, 0x31,
   0x18, 0x86, 0x31, 0x0c, 0x18, 0x18, 0x8f, 0x61, 0x3c, 0x86, 0xf1, 0x0f,
   0x18, 0x18, 0x8f, 0x61, 0x3c, 0x86, 0xf1, 0x07, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x00, 0x00, 0x78, 0x00, 0x00,
   0x00, 0x00, 0xfc, 0x01, 0x00, 0x3c, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x03,
   0x00, 0x1e, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x07, 0x00, 0x0f, 0x00, 0x00,
   0x00, 0x00, 0xe0, 0x0f, 0x80, 0x07, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x1f,
   0xc0, 0x03, 0x00, 0x00, 0x00, 0x00, 0x80, 0x3f, 0xe0, 0x01, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x7f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfe,
   0x78, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfc, 0x3c, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x78, 0x1e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x30,
   0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x07, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x8c, 0x0f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x8e,
   0x1f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x8f, 0x3f, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x80, 0x07, 0x7f, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x03,
   0xfe, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x01, 0xfc, 0x01, 0x00, 0x00,
   0x00, 0x00, 0xf0, 0x00, 0xf8, 0x03, 0x00, 0x00, 0x00, 0x00, 0x78, 0x00,
   0xf0, 0x07, 0x00, 0x00, 0x00, 0x00, 0x3c, 0x00, 0xe0, 0x0f, 0x00, 0x00,
   0x00, 0x00, 0x1e, 0x00, 0xc0, 0x1f, 0x00, 0x00, 0x00, 0x00, 0x0e, 0x00,
   0x80, 0x3f, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x7f, 0x00, 0x00,
   0x00, 0x00, 0x02, 0x00, 0x00, 0x7e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x83, 0x7f, 0x18,
   0x18, 0xfe, 0x61, 0x00, 0xf8, 0x87, 0x7f, 0x38, 0x18, 0xfe, 0x61, 0x00,
   0x18, 0x86, 0x61, 0x38, 0x18, 0x06, 0x60, 0x00, 0x18, 0x86, 0x61, 0x78,
   0x18, 0x06, 0x60, 0x00, 0x18, 0x86, 0x61, 0xf8, 0x18, 0x06, 0x60, 0x00,
   0x18, 0x86, 0x7f, 0xd8, 0x19, 0x7e, 0x60, 0x00, 0xf8, 0x83, 0x7f, 0x98,
   0x19, 0x7e, 0x60, 0x00, 0xf8, 0x83, 0x61, 0x98, 0x1b, 0x06, 0x60, 0x00,
   0x18, 0x80, 0x61, 0x18, 0x1b, 0x06, 0x60, 0x00, 0x18, 0x80, 0x61, 0x18,
   0x1e, 0x06, 0x60, 0x00, 0x18, 0x80, 0x61, 0x18, 0x1c, 0x06, 0x60, 0x00,
   0x18, 0x80, 0x61, 0x18, 0x1c, 0xfe, 0xe1, 0x1f, 0x18, 0x80, 0x61, 0x18,
   0x18, 0xfe, 0xe1, 0x1f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

/* These are now done by <X11/Intrinsic.h> */
/* #define True  1 */
/* #define False 0 */
/* #define NULL  0 */

#define CTRL_DEFINE	1
#define CTRL_DISPLAY	2
#define CTRL_CLEAR	3
#define CTRL_CHECK	4
#define CTRL_WAIT	5
#define CTRL_SET	6

#define PORT 5001
#define PANEL_WIDTH 13
#define MAXBUF 1024
#define CHECKTIME 100   /* Number of milli-s to wait before */
                        /* looking for new connections. */

static int Port = PORT; /* Used to identify the communications socket number. */

int listen_socket,io_socket;
int connected,visible,waiting,changes,cursorx,cursory,cursor_changes,ncursors;

Widget top_level;
Widget subframe;
GC gc;         /* Should this be global??? */

XtAppContext context;
XtIntervalId Timer;

typedef struct items {	char *values;
			int type,nvalues,changes,itno;
			int sval;        /* used by scrollbars and lists.*/
			int smin,smax,slen; /* used only with scrollbars.*/
			Widget main;
                        String *buttonlist; /* List of item values in a popup.*/
			struct items *fwd; } ITEMS;
ITEMS *items_head = NULL;

#define ITEM_BUTTONS 1
#define ITEM_SLIDERS 2
#define ITEM_CURSORS 3
#define ITEM_STATUSES 4

static void bug();
static void start_timer(), init_socket();
XtTimerCallbackProc check_socket();
XtInputCallbackProc read_socket();
static void clear_control_panel(), check_control_panel();
static void wait_control_panel(), define_control_panel();
static void set_control_panel(), destroy_control_panel();
static void create_control_panel();
XtActionProc CursorPaint(), CursorEvents();
static void SetUpListMenu(), SetUpSlider();
static void LabelScrollBarValue(), LabelScrollBarMinMax();
XtCallbackProc PlaceMenu(), ListButtonPushed();
XtCallbackProc button_next(), button_changed();
XtCallbackProc SliderScrollProc(), SliderJumpProc();
XtActionProc UpdateScroll(), QuitPanel();

typedef struct {
  Pixel   cursor_foreground;
  Pixel   cursor_background;
  Boolean no_iconic;
  int     port_number;
  XFontStruct *font;
} ApplicationData, *ApplicationDataPtr;
ApplicationData App_Data;

#define Offset(field) XtOffset(ApplicationDataPtr, field)
static XtResource resources[] = {
  {"cursorForeground", "CursorForeground", XtRPixel,      sizeof(Pixel),
      Offset(cursor_foreground), XtRString,    "XtDefaultForeground"},
  {"cursorBackground", "CursorBackground", XtRPixel,      sizeof(Pixel),
      Offset(cursor_background), XtRString,    "XtDefaultBackground"},
  {"noIconic",         "NoIconic",         XtRBoolean,    sizeof(Boolean),
      Offset(no_iconic),         XtRImmediate, (caddr_t) False      },
  {"portNumber",       "PortNumber",       XtRInt,        sizeof(int),
      Offset(port_number),       XtRImmediate, (caddr_t) PORT       },
  {XtNfont,             XtCFont,           XtRFontStruct, sizeof(XFontStruct *),
      Offset(font),              XtRString,    "9x15"               },
};
#undef Offset

static XtActionsRec actionTable[] = {
  {"quitpanel",    (XtActionProc) QuitPanel},
  {"cursorpaint",  (XtActionProc) CursorPaint},
  {"cursorevents", (XtActionProc) CursorEvents},
  {"updatescroll", (XtActionProc) UpdateScroll},
  {NULL, NULL}
};

static XrmOptionDescRec options[] = {
/* The last argument is not used when XrmoptionSepArg is specified. */
  {"-cursorforeground", "cursor_foreground", XrmoptionSepArg,  NULL },
  {"-cfg",              "cursor_foreground", XrmoptionSepArg,  NULL },
  {"-cursorbackground", "cursor_background", XrmoptionSepArg,  NULL },
  {"-cbg",              "cursor_background", XrmoptionSepArg,  NULL },
  {"-port",             "port_number",       XrmoptionSepArg,  NULL },
  {"-noiconic",         "no_iconic",         XrmoptionNoArg,  "TRUE"},
};

static String fallback_resources[] = {
  NULL,
};

/************************************************************************/
static void bug(string)
char *string;
{
    perror(string);
    exit(-1);
}
/************************************************************************/
main(argc,argv)
int argc;
char *argv[];
{
  int i;
  Arg args[10];
  Pixmap iconPixmap;
  XGCValues gcv;
  static String QuitTrans = "#override \n\
                        !<Key>Escape: quitpanel()";

/*
 * Get the input socket. For some reason, X will allocate the socket
 * I want if I call init_socket() after calling XtAppInitialize().
 */
  init_socket();

/* Create the base level frame. */

  (void)fprintf(stderr,"PANEL: %s\n",VERSION_ID);

  top_level = XtAppInitialize( &context, "Xpanel", options, XtNumber(options),
       (Cardinal *) &argc, argv, fallback_resources, NULL, (Cardinal) 0 );

  XtGetApplicationResources(top_level, (caddr_t) &App_Data,
       resources, XtNumber(resources), NULL, (Cardinal) 0 );

  XtAppAddActions( context, actionTable, XtNumber(actionTable) );

  i = 0;
  App_Data.no_iconic = 1;
  if (!App_Data.no_iconic){ XtSetArg(args[i], XtNiconic, (XtArgVal) True); i++;}
  XtSetArg(args[i], XtNheight, (XtArgVal) 64); i++;
  XtSetArg(args[i], XtNwidth, (XtArgVal) 200); i++;
  XtSetArg(args[i], XtNtitle, (XtArgVal) "Miriad Control Panel"); i++;
  XtSetValues(top_level, args, i);

  XtOverrideTranslations(top_level, XtParseTranslationTable(QuitTrans));

  XtRealizeWidget(top_level);

  iconPixmap = XCreateBitmapFromData(XtDisplay(top_level),
       XtWindow(top_level), xpanel_bits, xpanel_width, xpanel_height);

  i = 0;
  XtSetArg(args[i], XtNiconPixmap, iconPixmap); i++;
  XtSetValues(top_level, args, i);

  gcv.foreground = App_Data.cursor_foreground;
  gcv.background = App_Data.cursor_background;
  gc = XCreateGC(XtDisplay(top_level), XtWindow(top_level),
         (unsigned long) (GCForeground | GCBackground), &gcv);

  waiting = False;
  changes = 0;
  connected = False;
  visible = False;

/* Set up the timer for accept connections */
  start_timer();

/* Wait for connections, and go into the main loop. */

  XtAppMainLoop(context);
  exit(0);
}
/************************************************************************/
static void start_timer()
{
  unsigned long interval = CHECKTIME; /* in milli-seconds */

  Timer = XtAppAddTimeOut(context, interval, check_socket, NULL);
}
/************************************************************************/
static void init_socket()
{
  struct sockaddr_in sin;

/* Get a socket to the outside world, and bind to it. */

  bzero((char *)&sin, sizeof(sin));
  sin.sin_family = AF_INET;
  sin.sin_port = htons((u_short)Port);
  sin.sin_addr.s_addr = INADDR_ANY;

  if ((listen_socket = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    bug("open_socket:socket");
  if (bind(listen_socket, (caddr_t)&sin, sizeof(sin)) < 0)
    bug("open_socket:bind");
  listen(listen_socket, 5);
}
/************************************************************************/
/* ARGSUSED */
XtTimerCallbackProc check_socket(dummy, ptrTimer)
XtPointer dummy;  /* Unused */
XtIntervalId *ptrTimer;
{
  int len,read_mask;
  struct timeval time_0;
  struct sockaddr_in addr;
  XtInputId FdId;

  time_0.tv_sec = 0;
  time_0.tv_usec = 0;

  start_timer();

  read_mask = 1 << listen_socket;
  if ((connected == False) && (select(listen_socket+1, &read_mask, NULL,
					NULL, &time_0) > 0)) {
    len = sizeof(addr);
    if ((io_socket = accept(listen_socket, &addr, &len)) < 0)
      bug("get_client:accept");
    FdId = XtAppAddInput(context, io_socket, XtInputReadMask, read_socket, NULL);
    connected = True;
  }
  return;
}
/************************************************************************/
/* ARGSUSED */
XtInputCallbackProc read_socket(dummy, iosocket, fdid)
XtPointer dummy;  /* Unused */
int *iosocket;
XtInputId *fdid;
{
  int fd,nread,size;
  short int buffer[MAXBUF];

  fd = *iosocket;
  nread = read(fd,(char *)buffer,4);
  if(nread != 4){
    destroy_control_panel();
    XtRemoveInput(*fdid);
    close(fd);
    connected = False;
    visible = False;
  }else if(buffer[0] == CTRL_DEFINE){
    size = 2*buffer[1] + 4;
    nread = read(fd,(char *)&buffer[2],size);
    if(nread < size) bug("Unexpected End-of-Data");
    define_control_panel(buffer);
  }else if(buffer[0] == CTRL_DISPLAY){
    create_control_panel();
  }else if(buffer[0] == CTRL_CLEAR){
    clear_control_panel();
  }else if(buffer[0] == CTRL_CHECK){
    check_control_panel(buffer[1]);
  }else if(buffer[0] == CTRL_WAIT){
    wait_control_panel();
  }else if(buffer[0] == CTRL_SET){
    size = 2*buffer[1] + 2;
    nread = read(fd,(char *)&buffer[2],size);
    if(nread < size) bug("Unexpected End-of-Data");
    set_control_panel(buffer);
  }else bug("read_socket:I should never get here");
}
/************************************************************************/
static void clear_control_panel()
/*
  Clear all the flags that say something has changed.
------------------------------------------------------------------------*/
{
  ITEMS *ip;

  changes = 0;
  for(ip = items_head; ip != NULL; ip = ip->fwd) ip->changes = 0;
  cursor_changes = 0;
}
/************************************************************************/
static void check_control_panel(itno)
int itno;
/*
  Check the current state of a particular item.
------------------------------------------------------------------------*/
{
  ITEMS *ip;
  short int buf[4];

  for(ip = items_head; ip != NULL && ip->itno != itno; ip = ip->fwd);
  if(ip == NULL) bug("check_control_panel: Did not find the item");

  buf[2] = 0;
  buf[3] = 0;
  if(ip->type == ITEM_SLIDERS){
    buf[2] = ip->smin + ((ip->smax - ip->smin) * ip->sval / ip->slen);
  }else if(ip->type == ITEM_BUTTONS && ip->nvalues > 1){
    buf[2] = ip->sval;
  }else if(ip->type == ITEM_CURSORS){
    buf[2] = cursorx;
    buf[3] = 99 - cursory;
    ip->changes = cursor_changes;
    cursor_changes = 0;
  }
  buf[0] = ip->itno;
  buf[1] = ip->changes;
  changes -= ip->changes;
  ip->changes = 0;
  write(io_socket,(char *)buf,8);
}
/************************************************************************/
static void wait_control_panel()
/*
  Wait for something to happen. When something has happened, send a message
  to the client to say so.
------------------------------------------------------------------------*/
{
  ITEMS *ip;

  if(changes != 0){
    for(ip = items_head; 
        ip->changes == 0 && (ip->type != ITEM_CURSORS || cursor_changes == 0);
        ip = ip->fwd);
    check_control_panel(ip->itno);
    waiting = False;
  }else{
    waiting = True;
  }
}
/************************************************************************/
static void define_control_panel(message)
short int message[];
/*
  This client has requested that an additional item be added to our list
  of items. Add in the description.

  Inputs:
    message	An array containing:
		CTRL_DEFINE,size,itno,item_type,values
------------------------------------------------------------------------*/
{
  ITEMS *ip;
  int l,size;
  short int *in;
  char *out;

  ip = items_head;
  if(ip == NULL){
    items_head = (ITEMS *)malloc(sizeof(ITEMS));
    ip = items_head;
  }else{
    while(ip->fwd != NULL) ip = ip->fwd;
    ip->fwd = (ITEMS *)malloc(sizeof(ITEMS));
    ip = ip->fwd;
  }

  ip->itno = message[2];
  ip->type = message[3];
  ip->fwd = NULL;
  size = message[1];
  ip->values = malloc(size);
  ip->changes = 0;
  ip->nvalues = 0;
  ip->buttonlist = NULL;

  in = &message[4];
  out = ip->values;
  for(l=0; l < size; l++){
    if(*in == 0) ip->nvalues++;
    *out++ = *in++;
  }
}
/************************************************************************/
static void set_control_panel(message)
short int message[];
/*
  This client has requested that a change be made to one of our items.
  Currently, only either one or two values are permitted.

  Note that this routine is ONLY useful AFTER the panel is visible!

  Inputs:
    message	An array containing:
		CTRL_SET,size,itno,values
------------------------------------------------------------------------*/
{
  int j,k,itno,size,val[2];
  short int *in;
  char *cval,*out;
  float top;
  Widget w;
  Arg args[10];
  ITEMS *ip;

  if(!visible) return;

  size = message[1];
  itno = message[2];
  for(ip = items_head; ip != NULL && ip->itno != itno; ip = ip->fwd);
  if(ip == NULL) bug("set_control_panel: Did not find the item.");
  cval = malloc(size);
  in = &message[3];
  out = cval;
  for(j=0,k=0; j < size; j++){
    if(!(*out++ = *in++)){
      if(k > 1) break; /* No more than two parameters for now. */
      if(ip->type != ITEM_STATUSES)val[k] = atoi(cval);
      k++;
      out = cval;
    }
  }
  if(k == 0) bug("set_control_panel: Did not find any values.");

  if((ip->type == ITEM_BUTTONS) && (ip->nvalues > 1)){
    if((val[0] >= 0) && (val[0] < ip->nvalues) && (val[0] != ip->sval)){
      ip->sval = val[0] - 1;
      button_next(ip->main, (caddr_t)ip, (caddr_t)NULL); 
    }
  }else if(ip->type == ITEM_BUTTONS){
    ; /* NULL command for one valued buttons */
  }else if(ip->type == ITEM_STATUSES){
    XtSetArg(args[0], XtNlabel, cval);
    XtSetValues(ip->main, args, 1);
  }else if(ip->type == ITEM_SLIDERS){
    if(k == 1){
      top = (float)(val[0] - ip->smin)/(float)(ip->smax - ip->smin);
      ip->sval = top * ip->slen;
      w = XtNameToWidget(ip->main, "Scroll");
      XawScrollbarSetThumb(w, top, (float)(-1.0)); /* -1 means unchanged. */
      LabelScrollBarValue(ip->main, val[0]);
    }else if(k == 2){
      ip->smin = val[0];
      ip->smax = val[1];
      LabelScrollBarMinMax(ip->main, ip->smin, ip->smax);
    }else {
      bug("set_control_panel: Too many slider values!");
    }
  }else if(ip->type == ITEM_CURSORS){
    if(k == 1){
      cursorx = val[0];
      cursory = 99 - cursorx;
    }else if(k == 2){
      cursorx = val[0];
      cursory = 99 - val[1];
    }else {
      bug("set_control_panel: Too many cursor values!");
    }
    CursorPaint(ip->main, (XEvent *)NULL, (String *)NULL, (Cardinal *)NULL);
  }else{
    bug("set_control_panel: I cannot get here!");
  }
  (void)free(cval);
}
/************************************************************************/
static void destroy_control_panel()
/*
  Delete the structures created to handle this control panel.
------------------------------------------------------------------------*/
{
  int i;
  ITEMS *ip,*next;

  next = items_head;
  while(next != NULL){
    ip = next;
    next = ip->fwd;

    if (ip->buttonlist) {
      for (i = 0; ip->buttonlist[i] != NULL; i++)
        XtFree(ip->buttonlist[i]);
      XtFree(ip->buttonlist[i]);
    }

    (void)free(ip->values);
    (void)free((char *)ip);

  }
  if(visible) XtDestroyWidget(subframe);
  items_head = NULL;
  visible = False;
  connected = False;
}
/************************************************************************/
static void create_control_panel()
/*
  It is now time to create the control panel that the user wants.
------------------------------------------------------------------------*/
{
  ITEMS *ip;
  int i,panel_width,char_width,char_height;
  int max_width,max_height,item_width;
  int Cursor_Shape;
  Cursor cursor, nocursor;
  XFontStruct *font;
  Arg args[20];
  Widget panel;  /* Just about all items sit in this widget. */
  Widget canvas; /* Used for the cursor. */
  Widget Lasthoriz, Lastvert, SaveFirst;

  static String Trans = "#override \n\
                        <Btn3Down>: XtMenuPopup(ButtonList)";
  static String CoreTrans = "#override \n\
                             <Expose>: cursorpaint() \n\
                             <Btn1Down>: cursorevents() \n\
                             <Btn2Down>: cursorevents() \n\
                             <Btn1Motion>: cursorevents() \n\
                             <Btn2Motion>: cursorevents()";

  font = App_Data.font;
  char_width  = font->max_bounds.width;
  char_height = font->max_bounds.descent + font->max_bounds.ascent;
  panel_width = PANEL_WIDTH * char_width + 30;
  clear_control_panel();
  if(visible) XtDestroyWidget(subframe);
  Lasthoriz = NULL;
  Lastvert = NULL;
  SaveFirst = NULL;
  max_width = 0;
  max_height = 0;

/* Create a cursor for button items and a null cursor for all other items. */

  Cursor_Shape = XC_X_cursor;
  nocursor = XCreateFontCursor(XtDisplay(top_level), Cursor_Shape);
  Cursor_Shape = XC_arrow;
  cursor = XCreateFontCursor(XtDisplay(top_level), Cursor_Shape);

/* Create the subframe, and its panel. */

  i = 0;
  subframe = XtCreatePopupShell("Miriad Control Panel", transientShellWidgetClass,
                top_level, args, i);
  XtAddCallback(subframe, XtNpopupCallback, PlaceMenu, (XtPointer)NULL);

  i = 0;
  panel = XtCreateManagedWidget("panel", formWidgetClass, subframe, args, i);

  ncursors = 0;
  for(ip = items_head; ip != NULL; ip = ip->fwd){
    if(ip->type == ITEM_CURSORS){
      ncursors++;
    }else if(ip->type == ITEM_BUTTONS){
      if(Lasthoriz == NULL) max_height++;
      item_width = panel_width / 2;
      if(ip->nvalues == 1){
        i = 0;
        XtSetArg(args[i], XtNfromHoriz, (XtArgVal) Lasthoriz); i++;
        XtSetArg(args[i], XtNfromVert,   (XtArgVal) Lastvert); i++;
        XtSetArg(args[i], XtNlabel,    (XtArgVal) ip->values); i++;
        XtSetArg(args[i], XtNresize,        (XtArgVal) False); i++;
        XtSetArg(args[i], XtNwidth,    (XtArgVal) item_width); i++;
        XtSetArg(args[i], XtNcursor,       (XtArgVal) cursor); i++;
        ip->main = XtCreateManagedWidget("button", commandWidgetClass,
                     panel, args, i);
        XtAddCallback(ip->main, XtNcallback, button_changed, (XtPointer)ip);
        if (Lasthoriz) {
          Lastvert = Lasthoriz;
          Lasthoriz = NULL;
        } else {
          Lasthoriz = ip->main;
        }
        if (Lastvert && (item_width > max_width) && (max_height < 8)){
          max_width = item_width;
          SaveFirst = ip->main;
        }
      }else{
        i = 0;
        XtSetArg(args[i], XtNfromHoriz, (XtArgVal) Lasthoriz); i++;
        XtSetArg(args[i], XtNfromVert,   (XtArgVal) Lastvert); i++;
        XtSetArg(args[i], XtNlabel,    (XtArgVal) ip->values); i++;
        XtSetArg(args[i], XtNresize,        (XtArgVal) False); i++;
        XtSetArg(args[i], XtNwidth,    (XtArgVal) item_width); i++;
        XtSetArg(args[i], XtNcursor,       (XtArgVal) cursor); i++;
        ip->main = XtCreateManagedWidget("list", commandWidgetClass,
                     panel, args, i);
        XtAddCallback(ip->main, XtNcallback, button_next, (XtPointer)ip);
        XtOverrideTranslations(ip->main, XtParseTranslationTable(Trans));
        SetUpListMenu(ip, item_width);
        if (Lasthoriz) {
          Lastvert = Lasthoriz;
          Lasthoriz = NULL;
        } else {
          Lasthoriz = ip->main;
        }
        if (Lastvert && (item_width > max_width) && (max_height < 8)){
          max_width = item_width;
          SaveFirst = ip->main;
        }
      }
    }else if(ip->type == ITEM_STATUSES){
      max_height++;
      if (Lasthoriz) { /* Give this its own line. */
        Lastvert = Lasthoriz;
        Lasthoriz = NULL;
      }
      item_width = strlen(ip->values);
      if (panel_width > item_width) item_width = (2 * panel_width);
      i = 0;
      XtSetArg(args[i], XtNfromHoriz, (XtArgVal) Lasthoriz); i++;
      XtSetArg(args[i], XtNfromVert,   (XtArgVal) Lastvert); i++;
      XtSetArg(args[i], XtNlabel,    (XtArgVal) ip->values); i++;
      XtSetArg(args[i], XtNwidth,    (XtArgVal) item_width); i++;
      XtSetArg(args[i], XtNcursor,     (XtArgVal) nocursor); i++;
      ip->main = XtCreateManagedWidget("status", labelWidgetClass,
                   panel, args, i);
      Lastvert = ip->main;
      if ((item_width > max_width) && (max_height < 8)){
        max_width = item_width;
        SaveFirst = ip->main;
      }
    }else if(ip->type == ITEM_SLIDERS){
      max_height++;
      if (Lasthoriz) { /* Give this its own line. */
        Lastvert = Lasthoriz;
        Lasthoriz = NULL;
      }
      item_width = (2 * panel_width);
      i = 0;
      XtSetArg(args[i], XtNfromHoriz, (XtArgVal) Lasthoriz); i++;
      XtSetArg(args[i], XtNfromVert,   (XtArgVal) Lastvert); i++;
      XtSetArg(args[i], XtNwidth,    (XtArgVal) item_width); i++;
      XtSetArg(args[i], XtNcursor,     (XtArgVal) nocursor); i++;
      ip->main = XtCreateManagedWidget("slider", formWidgetClass,
                   panel, args, i);
      SetUpSlider(ip, nocursor, panel_width/2, panel_width*2/3, char_height);
      Lastvert = ip->main;
      if ((item_width > max_width) && (max_height < 8)){
        max_width = item_width;
        SaveFirst = ip->main;
      }
    }else bug("create_control_panel: I cannot get here!");
  }

/* Check if the user requested cursors. This implementation only allows
   one cursor. This maps as follows:
    wait_control_panel:  It maps to the first cursor defined.
    check_control_panel: It maps to the requested cursor. */

  if(ncursors != 0){
    cursorx = 50;
    cursory = 50;
    i = 0;
    XtSetArg(args[i], XtNtop,      (XtArgVal)XtChainTop); i++;
    XtSetArg(args[i], XtNright,  (XtArgVal)XtChainRight); i++;
    XtSetArg(args[i], XtNresize,        (XtArgVal)False); i++;
    XtSetArg(args[i], XtNwidth,   (XtArgVal)panel_width); i++;
    XtSetArg(args[i], XtNheight,  (XtArgVal)panel_width); i++;
    XtSetArg(args[i], XtNfromVert,       (XtArgVal)NULL); i++;
    XtSetArg(args[i], XtNfromHoriz, (XtArgVal)SaveFirst); i++;
    canvas = XtCreateManagedWidget("cursor", coreWidgetClass,
                   panel, args, i);
    XtOverrideTranslations(canvas, XtParseTranslationTable(CoreTrans));
    for(ip = items_head; ip != NULL; ip = ip->fwd)
      if(ip->type == ITEM_CURSORS)
        ip->main = canvas;
  }

/*  Push the subframe onto the screen. */

  XtPopup(subframe, XtGrabNone);
  visible = True;

/*  Only after it has been realized can one assign a cursor to the canvas. */

  if(ncursors != 0) {
    Cursor_Shape = XC_hand2;
    cursor = XCreateFontCursor(XtDisplay(top_level), Cursor_Shape);
    XDefineCursor(XtDisplay(canvas), XtWindow(canvas), cursor);
  }
}
/************************************************************************/
/* ARGSUSED */
XtActionProc CursorPaint(w, event, params, nparams)
Widget w;
XEvent *event;     /* Unused */
String *params;    /* Unused */
Cardinal *nparams; /* Unused */
/*
  Draw a cross at the cursor position.
------------------------------------------------------------------------*/
{
  int width,height,left,right,top,bottom;
  XWindowAttributes wattr;

  if(XGetWindowAttributes(XtDisplay(w), XtWindow(w), &wattr)){
    width = (int)wattr.width;
    height = (int)wattr.height;
    left = width*cursorx/100 - 10;
    right = left + 21;
    top = height*cursory/100 - 10;
    bottom = top + 21;
    XClearWindow(XtDisplay(w), XtWindow(w));
    XDrawLine(XtDisplay(w), XtWindow(w), gc, left, top, right, bottom);
    XDrawLine(XtDisplay(w), XtWindow(w), gc, left, bottom, right, top);
  }
}
/************************************************************************/
/* ARGSUSED */
XtActionProc CursorEvents(w, event, params, nparams)
Widget w;
XEvent *event;
String *params;    /* Unused */
Cardinal *nparams; /* Unused */
/*
  This receives events when the cursor is moved or pushed.
------------------------------------------------------------------------*/
{
  int x, y;
  int width, height;
  XWindowAttributes wattr;

  switch (event->type) {
    case ButtonPress:  x = event->xbutton.x; y = event->xbutton.y; break;
    case MotionNotify: x = event->xmotion.x; y = event->xmotion.y; break;
    default: /* ?? */  x = event->xbutton.x; y = event->xbutton.y; break;
  }
  if(XGetWindowAttributes(XtDisplay(w), XtWindow(w), &wattr)){
    width = (int)wattr.width;
    height = (int)wattr.height;
    if((x >= 0) && (x < width) && (y >= 0) && (y < height)){
      cursor_changes++;
      changes++;
      cursorx = 100 * x / width;
      cursory = 100 * y / height;
      CursorPaint(w, event, params, nparams);
      if(waiting) wait_control_panel();
      waiting = False;
    }
  }
}
/************************************************************************/
static void SetUpListMenu(item, width)
ITEMS *item;
int width;
{
    char *s;
    int i;
    ITEMS *ip;
    Arg args[10];
    Widget popupshell, popbox, wlist;
    static String PopTrans = "#override \n\
                              <Btn3Up>: XtMenuPopdown(ButtonList)";
    static String LisTrans = "#override \n\
                              <EnterWindow>: highlight() \n\
                              <LeaveWindow>: reset() \n\
                              <Btn3Up>: set() notify() unset() ";

    ip = item;
    if (ip == NULL) bug("SetUpListMenu: Did not find the item");
    if (ip->type != ITEM_BUTTONS) bug("SetUpListMenu: item not a button");

    i = 0;
    popupshell = XtCreatePopupShell("ButtonList", transientShellWidgetClass,
                                      ip->main, args, i);
    XtAddCallback(popupshell, XtNpopupCallback, PlaceMenu, (XtPointer)NULL);
    XtOverrideTranslations(popupshell, XtParseTranslationTable(PopTrans));

    i = 0;
    XtSetArg(args[i], XtNhSpace, (XtArgVal) 1); i++;
    XtSetArg(args[i], XtNvSpace, (XtArgVal) 1); i++;
    popbox = XtCreateManagedWidget("MenuBox", boxWidgetClass,
                                      popupshell, args, i);

    XtSetArg(args[0], XtNwidth, (XtArgVal) width);
    ip->buttonlist = (String *)XtMalloc((ip->nvalues + 1) * sizeof(String));
    s = ip->values;
    for (i = 0; i < ip->nvalues; i++) {
      ip->buttonlist[i] = (String) s;
      wlist = XtCreateManagedWidget(ip->buttonlist[i], commandWidgetClass,
                                     popbox, args, 1);
      XtAddCallback(wlist, XtNcallback, ListButtonPushed, (XtPointer)ip);
      XtOverrideTranslations(wlist, XtParseTranslationTable(LisTrans));
      s += strlen(s) + 1;
    }
    ip->buttonlist[i] = NULL;
    ip->sval = 0; /* SVAL points to the current hightlighted entry. */
}
/************************************************************************/
static void SetUpSlider(item, nocursor, labelwidth, width, height)
ITEMS *item;
Cursor nocursor;
int labelwidth, width, height;
{
    int i;
    float top, shown;
    ITEMS *ip;
    Arg args[20];
    Widget wlab, wval, wmin, wmax;
    Widget wscroll;
    char ScrollTrans[300];
    static String SprintfTrans = "#override \n\
      <Btn2Up>: NotifyScroll(Proportional) EndScroll() updatescroll(%d) \n\
      <BtnUp>:  NotifyScroll(Proportional) EndScroll() ";

    ip = item;
    if (ip == NULL) bug("SetUpSlider: Did not find the item");
    if(ip->type != ITEM_SLIDERS) bug("SetUpSlider: item is not a Slider");

    ip->smin = 0;
    ip->smax = 100;
    ip->slen = width;
    ip->sval = width / 2;
    top = (float)ip->sval;
    shown = (float)ip->sval / (float)ip->slen;

    i = 0;
    XtSetArg(args[i], XtNlabel,            ip->values); i++;
    XtSetArg(args[i], XtNwidth, (XtArgVal) labelwidth); i++;
    XtSetArg(args[i], XtNborderWidth,    (XtArgVal) 0); i++;
    XtSetArg(args[i], XtNfromVert,    (XtArgVal) NULL); i++;
    XtSetArg(args[i], XtNfromHoriz,   (XtArgVal) NULL); i++;
    XtSetArg(args[i], XtNcursor,  (XtArgVal) nocursor); i++;
    wlab = XtCreateManagedWidget("Slab", labelWidgetClass, ip->main, args, i);

    i = 0;
    XtSetArg(args[i], XtNlabel,              "[100]"); i++;
    XtSetArg(args[i], XtNborderWidth,   (XtArgVal) 0); i++;
    XtSetArg(args[i], XtNfromHoriz,  (XtArgVal) wlab); i++;
    XtSetArg(args[i], XtNhorizDistance, (XtArgVal) 5); i++;
    XtSetArg(args[i], XtNcursor, (XtArgVal) nocursor); i++;
    wval = XtCreateManagedWidget("Sval", labelWidgetClass, ip->main, args, i);

    i = 0;
    XtSetArg(args[i], XtNlabel,                "100"); i++;
    XtSetArg(args[i], XtNborderWidth,   (XtArgVal) 0); i++;
    XtSetArg(args[i], XtNfromHoriz,  (XtArgVal) wval); i++;
    XtSetArg(args[i], XtNhorizDistance, (XtArgVal) 5); i++;
    XtSetArg(args[i], XtNcursor, (XtArgVal) nocursor); i++;
    wmin = XtCreateManagedWidget("Smin", labelWidgetClass, ip->main, args, i);

    i = 0;
    XtSetArg(args[i], XtNfromHoriz,      (XtArgVal) wmin); i++;
    XtSetArg(args[i], XtNhorizDistance,     (XtArgVal) 0); i++;
    XtSetArg(args[i], XtNorientation, XtorientHorizontal); i++;
    XtSetArg(args[i], XtNlength,     (XtArgVal) ip->slen); i++;
    XtSetArg(args[i], XtNthickness,    (XtArgVal) height); i++;
    XtSetArg(args[i], XtNtop,             (XtArgVal) top); i++;
    XtSetArg(args[i], XtNshown,         (XtArgVal) shown); i++;
    wscroll = XtCreateManagedWidget("Scroll", scrollbarWidgetClass,
                 ip->main, args, i);
    XtAddCallback(wscroll, XtNscrollProc, SliderScrollProc, (XtPointer)ip);
    XtAddCallback(wscroll, XtNjumpProc, SliderJumpProc, (XtPointer)ip);
    (void)sprintf(ScrollTrans, SprintfTrans, ip->itno);
    XtOverrideTranslations(wscroll, XtParseTranslationTable(ScrollTrans));

    i = 0;
    XtSetArg(args[i], XtNlabel,                  "100"); i++;
    XtSetArg(args[i], XtNborderWidth,     (XtArgVal) 0); i++;
    XtSetArg(args[i], XtNfromHoriz, (XtArgVal) wscroll); i++;
    XtSetArg(args[i], XtNhorizDistance,   (XtArgVal) 0); i++;
    XtSetArg(args[i], XtNcursor,   (XtArgVal) nocursor); i++;
    wmax = XtCreateManagedWidget("Smax", labelWidgetClass, ip->main, args, i);

    LabelScrollBarMinMax(ip->main, ip->smin, ip->smax);
    LabelScrollBarValue(ip->main, (int)50);
    XawScrollbarSetThumb(wscroll, (float)(0.5), (float)(-1.0));
        /* -1 for the last two arguments means unchanged. */
}
/************************************************************************/
static void LabelScrollBarValue(parent, value)
Widget parent;
int value;
{
    Arg args[2];
    char string[20];
    Dimension width;
    Widget widget;

    widget = XtNameToWidget(parent, "Sval");
    sprintf(string, "[%d]", value);
    XtSetArg(args[0], XtNwidth, &width);
    XtGetValues(widget, args, 1);
    XtSetArg(args[0], XtNwidth, width);
    XtSetArg(args[1], XtNlabel, string);
    XtSetValues(widget, args, 2);
}
/************************************************************************/
static void LabelScrollBarMinMax(parent, min, max)
Widget parent;
int min, max;
{
    char string[20];
    Arg args[1];
    Widget widget;

    widget = XtNameToWidget(parent, "Smin");
    sprintf(string, "%d", min);
    XtSetArg(args[0], XtNlabel, string);
    XtSetValues(widget, args, 1);

    widget = XtNameToWidget(parent, "Smax");
    sprintf(string, "%d", max);
    XtSetArg(args[0], XtNlabel, string);
    XtSetValues(widget, args, 1);
}
/************************************************************************/
/* ARGSUSED */
XtCallbackProc PlaceMenu(w, client_data, call_data)
Widget w;
XtPointer client_data; /* Unused */
XtPointer call_data;   /* Unused */
{
    int i;
    Arg args[10];
    Widget button;    /* Widget of parent of popup widget. */
    Position x, y;    /* Coords of the parent widget on the root window. */
    Dimension height; /* Height of parent;  menu is placed below parent. */
/*
 *  Translate the position of the popup window to the coordinates of
 *  the button window origin.
 */
    button = XtParent(w);
    XtTranslateCoords(button, (Position) 0, (Position) 0, &x, &y);
    XtSetArg(args[0], XtNheight, &height);
    XtGetValues(button, args, 1);

/*  Move the popup shell height pixels below and right of this position. */
/*  (The popup widget is not visible yet.) */
    i = 0;
    XtSetArg(args[i], XtNx, x + height); i++;
    XtSetArg(args[i], XtNy, y + height/2); i++;
    XtSetValues(w, args, i);
}
/************************************************************************/
/* ARGSUSED */
XtCallbackProc ListButtonPushed(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;   /* Unused */
{
    int listindex;
    Arg args[1];
    ITEMS *ip;
    Widget button;
    String listitem;

    ip = (ITEMS *)client_data;
    if(ip == NULL) bug("ListButtonPushed: Did not find the item");
    if (ip->type != ITEM_BUTTONS) bug("ListButtonPushed: item not a button");

    XtSetArg(args[0], XtNlabel, &listitem);
    XtGetValues(w, args, 1);

    button = XtParent(XtParent(w)); /* ButtonList<-MenuBox<-buttonlist[i] */
    XtPopdown(button);

    button = XtParent(button);  /* list<-ButtonList<-MenuBox<-buttonlist[i] */

    if((int)strlen(listitem) > 0) {
      for (listindex = 0; listindex < ip->nvalues; listindex++)
        if(strcmp(listitem, ip->buttonlist[listindex]) == 0) break;

      if((listindex != ip->sval) && (listindex < ip->nvalues)) {
        XtSetArg(args[0], XtNlabel, ip->buttonlist[listindex]);
        XtSetValues(button, args, 1);
        ip->sval = listindex;

        button_changed(w, (caddr_t) ip, (caddr_t) NULL);
      }
    }
}
/************************************************************************/
/* ARGSUSED */
XtCallbackProc button_next(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;   /* Unused */
{
    int listindex;
    Arg args[1];
    ITEMS *ip;

    ip = (ITEMS *)client_data;
    if(ip == NULL) bug("button_next: Did not find the item");
    if (ip->type != ITEM_BUTTONS) bug("button_next: item not a button");

    listindex = (ip->sval + 1) % ip->nvalues;

    XtSetArg(args[0], XtNlabel, ip->buttonlist[listindex]);
    XtSetValues(w, args, 1);

    ip->sval = listindex;

    button_changed(w, (caddr_t) ip, (caddr_t) NULL);
}
/************************************************************************/
/* ARGSUSED */
XtCallbackProc button_changed(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;   /* Unused */
{
    ITEMS *ip;

    ip = (ITEMS *)client_data;
    if(ip == NULL) bug("button_changed: Did not find the item");

    ip->changes++;
    changes++;
    if(waiting) check_control_panel(ip->itno);
    waiting = False;
}
/************************************************************************/
XtCallbackProc SliderScrollProc(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
    int value;
    float fraction;
    ITEMS *ip;

    ip = (ITEMS *)client_data;
    if(ip == NULL) bug("SliderScrollProc: Did not find the item");
    if(ip->type != ITEM_SLIDERS) bug("SliderScrollProc: item is not a Slider");

    ip->sval -= (int)call_data / 10;
    if (ip->sval < 0) ip->sval = 0;
    if (ip->sval > ip->slen) ip->sval = ip->slen;
    fraction = (float)ip->sval / (float)ip->slen;
    value = ip->smin + (fraction * (ip->smax - ip->smin));
    XawScrollbarSetThumb(w, fraction, (float)(-1.0)); /* -1 means unchanged. */
    LabelScrollBarValue(ip->main, value);
    button_changed(w, (XtPointer) ip, (XtPointer) NULL);
}
/************************************************************************/
/* ARGSUSED */
XtCallbackProc SliderJumpProc(w, client_data, call_data)
Widget w;            /* Unused */
XtPointer client_data;
XtPointer call_data;
{
    int value;
    float fraction;
    ITEMS *ip;

    ip = (ITEMS *)client_data;
    if(ip == NULL) bug("SliderJumpProc: Did not find the item");
    if(ip->type != ITEM_SLIDERS) bug("SliderJumpProc: item is not a Slider");

    fraction = *(float *)call_data;
    ip->sval = fraction * ip->slen;
    value = ip->smin + (fraction * (ip->smax - ip->smin));
    LabelScrollBarValue(ip->main, value);
/*  Only notify when the scrolling is finished....   */
/*  button_changed(w, (XtPointer) ip, (XtPointer) NULL); */
}
/************************************************************************/
/* ARGSUSED */
XtActionProc UpdateScroll(w, event, params, nparams)
Widget w;
XEvent *event;     /* Unused */
String *params;    /* ITEMS* ip->itno */
Cardinal *nparams; /* Only 1 */
{
    int itno;
    ITEMS *ip;

    if (*nparams != 1) return;
    itno = atoi(*params);

    for(ip = items_head; ip != NULL && ip->itno != itno; ip = ip->fwd) ;
    if(ip == NULL) bug("UpdateScroll: Did not find the item");

    button_changed(w, (XtPointer) ip, (XtPointer) NULL);
}
/************************************************************************/
/* ARGSUSED */
XtActionProc QuitPanel(w, event, params, nparams)
Widget w;          /* Unused */
XEvent *event;     /* Unused */
String *params;    /* Unused */
Cardinal *nparams; /* Unused */
{
    destroy_control_panel();
    XtCloseDisplay(XtDisplay(top_level));
    exit(0);
}
