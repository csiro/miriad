#define VERSION_ID  "31-oct-94"
/*= XPANEL - X-window control panel program
/*& jm
/*: tools
/*+
XPanel is a TCP/IP server based on X-windows, which listens for
connections from the Miriad "ctrl" routines, constructs a control
panel according to commands from the Ctrl routines, and allows the
user and programmer to communicate through the control panel.

See the Miriad Programmers manual for documentation of the Ctrl
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
	Child of "MenuBox":	list entry  (commandWidget)

In addition to the above widget items, there are application
resources that the user may set in their application file or
on the command line.  In addition to the usual X-window resources,
Xpanel provides the following resources, along with their defaults:

   *cursorForeground:	"XtDefaultForeground" (Fore/Background color of)
   *cursorBackground:	"XtDefaultBackground" (... cursor in coreWidget)
   *noIconic:		False             (Start application non-iconic)
   *Font:		"9x15"                   (Font used for strings)
   *portNumber:		5001	   (Port number used for communications)
/*--

  History:
    jm  16sep91 Original based on SunView panel routine.
    mjs 21sep93 #define bzero -> memset on sun; make in-code docs
                compatible with miriad docs; make icon "unsigned char".
    jm  31oct94 Fixed problem that would not let port number be
                reassigned (also added a message).  Also, cleaned
                socket code section and removed some global
                variables they depended on.
/************************************************************************/
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
#include "xpanel.icon"

#ifdef sun
#define bzero(a,b) memset((a),0,(b))
#endif

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

static int ioSocket;
static int connected, visible, waiting, changes;
static int cursorx, cursory, cursor_changes, ncursors;

static Widget top_level;
static Widget subframe;
static GC gc;

static XtAppContext context;

typedef struct items {	char *values;
			int type,nvalues,changes,itno;
			int sval;        /* used by scrollbars and lists.*/
			int smin,smax,slen; /* used only with scrollbars.*/
			Widget main;
                        String *buttonlist; /* List of item values in a popup.*/
			struct items *fwd; } ITEMS;
static ITEMS *items_head = (ITEMS *)NULL;

#define ITEM_BUTTONS 1
#define ITEM_SLIDERS 2
#define ITEM_CURSORS 3
#define ITEM_STATUSES 4

static void bug();
static void startTimer(), initializeSocket();
static XtTimerCallbackProc checkSocket();
static XtInputCallbackProc readSocket();
static void clear_control_panel(), check_control_panel();
static void wait_control_panel(), define_control_panel();
static void set_control_panel(), destroy_control_panel();
static void create_control_panel();
static XtActionProc CursorPaint(), CursorEvents();
static void SetUpListMenu(), SetUpSlider();
static void LabelScrollBarValue(), LabelScrollBarMinMax();
static XtCallbackProc PlaceMenu(), ListButtonPushed();
static XtCallbackProc button_next(), button_changed();
static XtCallbackProc SliderScrollProc(), SliderJumpProc();
static XtActionProc UpdateScroll(), QuitPanel();

typedef struct {
  Pixel       cursor_foreground;
  Pixel       cursor_background;
  Boolean     no_iconic;
  int         port_number;
  XFontStruct *font;
} ApplicationData;

static ApplicationData App_Data;

#define Offset(field) XtOffsetOf(ApplicationData, field)
static XtResource resources[] = {
  {"cursorForeground", "CursorForeground", XtRPixel,      sizeof(Pixel),
      Offset(cursor_foreground), XtRString,    (XtPointer)XtDefaultForeground},
  {"cursorBackground", "CursorBackground", XtRPixel,      sizeof(Pixel),
      Offset(cursor_background), XtRString,    (XtPointer)XtDefaultBackground},
  {"noIconic",         "NoIconic",         XtRBoolean,    sizeof(Boolean),
      Offset(no_iconic),         XtRImmediate, (XtPointer)False},
  {"portNumber",       "PortNumber",       XtRInt,        sizeof(int),
      Offset(port_number),       XtRImmediate, (XtPointer)PORT},
  {XtNfont,             XtCFont,           XtRFontStruct, sizeof(XFontStruct *),
      Offset(font),              XtRString,    (XtPointer)"9x15"},
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
  unsigned int Port; /* Used to identify the communications socket number. */
  Arg args[10];
  Pixmap iconPixmap;
  XGCValues gcv;
  static String QuitTrans = "#override \n\
                        !<Key>Escape: quitpanel()";

/* Create the base level frame. */

  (void)fprintf(stderr,"PANEL: %s\n",VERSION_ID);

  top_level = XtAppInitialize( &context, "Xpanel", options, XtNumber(options),
       &argc, argv, fallback_resources, NULL, (Cardinal) 0 );

  XtGetApplicationResources(top_level, (caddr_t) &App_Data,
       resources, XtNumber(resources), NULL, (Cardinal) 0 );

  XtAppAddActions( context, actionTable, XtNumber(actionTable) );

  i = 0;
  if (!App_Data.no_iconic) XtSetArg(args[i], XtNiconic, (XtArgVal) True); i++;
  XtSetArg(args[i], XtNheight, (XtArgVal) 64); i++;
  XtSetArg(args[i], XtNwidth, (XtArgVal) 200); i++;
  XtSetArg(args[i], XtNtitle, (XtArgVal) "Miriad Control Panel"); i++;
  XtSetValues(top_level, args, i);

  XtOverrideTranslations(top_level, XtParseTranslationTable(QuitTrans));

  XtRealizeWidget(top_level);

  iconPixmap = XCreateBitmapFromData(XtDisplay(top_level),
       XtWindow(top_level), (char *)xpanel_bits, xpanel_width, xpanel_height);

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

/* Get a socket for accepting connections and then start the timer. */
  Port = App_Data.port_number;
  if (Port != PORT)
    (void)fprintf(stderr,
      "PANEL: Re-assigning the I/O Port number from %d to %d\n", PORT, Port);
  initializeSocket(Port);

/* Wait for connections, and go into the main loop. */

  XtAppMainLoop(context);
  exit(0);
}
/************************************************************************/
static void startTimer(socketID)
int socketID;
{
  unsigned long interval = CHECKTIME; /* in milli-seconds */

  (void)XtAppAddTimeOut(context, interval,
    (XtTimerCallbackProc)checkSocket, (XtPointer)socketID);
}
/************************************************************************/
static void initializeSocket(portnumber)
unsigned int portnumber;
{
  int listenSocket;
  struct sockaddr_in sin;

/* Get a socket to the outside world, and bind to it. */

  bzero((char *)&sin, sizeof(sin));
  sin.sin_family = AF_INET;
  sin.sin_port = htons(portnumber);
  sin.sin_addr.s_addr = htonl(INADDR_ANY);

  if ((listenSocket = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    bug("initializeSocket:socket");
  if (bind(listenSocket, (struct sockaddr *)&sin, sizeof(sin)) < 0)
    bug("initializeSocket:bind");
  listen(listenSocket, 5);
  startTimer(listenSocket);
}
/************************************************************************/
/* ARGSUSED */
static XtTimerCallbackProc checkSocket(client_data, ptrTimer)
XtPointer client_data;
XtIntervalId *ptrTimer;
{
  int len;
  int listenSocket;
  fd_set read_mask;
  struct timeval time_0;
  struct sockaddr_in addr;
  XtInputMask condition = XtInputReadMask;

  time_0.tv_sec = 0;
  time_0.tv_usec = 0;

  listenSocket = (int)client_data;
  startTimer(listenSocket);

  if (connected == False) {
    FD_ZERO(&read_mask);
    FD_SET(listenSocket, &read_mask);
    if (select(listenSocket+1, &read_mask, (fd_set *)NULL,
      (fd_set *)NULL, &time_0) > 0) {
      len = sizeof(addr);
      if ((ioSocket = accept(listenSocket, (struct sockaddr *)&addr, &len)) < 0)
        bug("checkSocket:accept");
      (void)XtAppAddInput(context, ioSocket, (XtPointer)condition,
        (XtInputCallbackProc)readSocket, (XtPointer)NULL);
      connected = True;
    }
  }
  return NULL;
}
/************************************************************************/
/* ARGSUSED */
static XtInputCallbackProc readSocket(dummy, iosocket, fdid)
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
  }else bug("readSocket:I should never get here");

  return NULL;
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
  write(ioSocket,(char *)buf,8);
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
      (void)button_next(ip->main, (caddr_t)ip, (caddr_t)NULL); 
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
    (void)CursorPaint(ip->main, (XEvent *)NULL, (String *)NULL, (Cardinal *)NULL);
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
  XtAddCallback(subframe, XtNpopupCallback, (XtCallbackProc)PlaceMenu,
    (XtPointer)NULL);

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
        XtAddCallback(ip->main, XtNcallback,
          (XtCallbackProc)button_changed, (XtPointer)ip);
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
        XtAddCallback(ip->main, XtNcallback,
          (XtCallbackProc)button_next, (XtPointer)ip);
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
static XtActionProc CursorPaint(w, event, params, nparams)
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
  return NULL;
}
/************************************************************************/
/* ARGSUSED */
static XtActionProc CursorEvents(w, event, params, nparams)
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
      (void)CursorPaint(w, event, params, nparams);
      if(waiting) wait_control_panel();
      waiting = False;
    }
  }
  return NULL;
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
    XtAddCallback(popupshell, XtNpopupCallback,
      (XtCallbackProc)PlaceMenu, (XtPointer)NULL);
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
      XtAddCallback(wlist, XtNcallback,
        (XtCallbackProc)ListButtonPushed, (XtPointer)ip);
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
    Widget wlab, wval, wmin;
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
    XtAddCallback(wscroll, XtNscrollProc,
      (XtCallbackProc)SliderScrollProc, (XtPointer)ip);
    XtAddCallback(wscroll, XtNjumpProc,
      (XtCallbackProc)SliderJumpProc, (XtPointer)ip);
    (void)sprintf(ScrollTrans, SprintfTrans, ip->itno);
    XtOverrideTranslations(wscroll, XtParseTranslationTable(ScrollTrans));

    i = 0;
    XtSetArg(args[i], XtNlabel,                  "100"); i++;
    XtSetArg(args[i], XtNborderWidth,     (XtArgVal) 0); i++;
    XtSetArg(args[i], XtNfromHoriz, (XtArgVal) wscroll); i++;
    XtSetArg(args[i], XtNhorizDistance,   (XtArgVal) 0); i++;
    XtSetArg(args[i], XtNcursor,   (XtArgVal) nocursor); i++;
    (void)XtCreateManagedWidget("Smax", labelWidgetClass, ip->main, args, i);

    LabelScrollBarMinMax(ip->main, ip->smin, ip->smax);
    LabelScrollBarValue(ip->main, (int)50);
    XawScrollbarSetThumb(wscroll, (float)(0.5), (float)(-1.0));
        /* -1 for the last two arguments means unchanged. */

    return;
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
    (void)sprintf(string, "[%d]", value);
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
    (void)sprintf(string, "%d", min);
    XtSetArg(args[0], XtNlabel, string);
    XtSetValues(widget, args, 1);

    widget = XtNameToWidget(parent, "Smax");
    (void)sprintf(string, "%d", max);
    XtSetArg(args[0], XtNlabel, string);
    XtSetValues(widget, args, 1);
    return;
}
/************************************************************************/
/* ARGSUSED */
static XtCallbackProc PlaceMenu(w, client_data, call_data)
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
    return NULL;
}
/************************************************************************/
/* ARGSUSED */
static XtCallbackProc ListButtonPushed(w, client_data, call_data)
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

    if(strlen(listitem) > (size_t)0) {
      for (listindex = 0; listindex < ip->nvalues; listindex++)
        if(strcmp(listitem, ip->buttonlist[listindex]) == 0) break;

      if((listindex != ip->sval) && (listindex < ip->nvalues)) {
        XtSetArg(args[0], XtNlabel, ip->buttonlist[listindex]);
        XtSetValues(button, args, 1);
        ip->sval = listindex;

        (void)button_changed(w, (caddr_t) ip, (caddr_t) NULL);
      }
    }
    return NULL;
}
/************************************************************************/
/* ARGSUSED */
static XtCallbackProc button_next(w, client_data, call_data)
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

    (void)button_changed(w, (caddr_t) ip, (caddr_t) NULL);
    return NULL;
}
/************************************************************************/
/* ARGSUSED */
static XtCallbackProc button_changed(w, client_data, call_data)
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
    return NULL;
}
/************************************************************************/
static XtCallbackProc SliderScrollProc(w, client_data, call_data)
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
    (void)button_changed(w, (XtPointer) ip, (XtPointer) NULL);
    return NULL;
}
/************************************************************************/
/* ARGSUSED */
static XtCallbackProc SliderJumpProc(w, client_data, call_data)
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
/*  (void)button_changed(w, (XtPointer) ip, (XtPointer) NULL); */
    return NULL;
}
/************************************************************************/
/* ARGSUSED */
static XtActionProc UpdateScroll(w, event, params, nparams)
Widget w;
XEvent *event;     /* Unused */
String *params;    /* ITEMS* ip->itno */
Cardinal *nparams; /* Only 1 */
{
    int itno;
    ITEMS *ip;

    if (*nparams != 1) return NULL;
    itno = atoi(*params);

    for(ip = items_head; ip != NULL && ip->itno != itno; ip = ip->fwd) ;
    if(ip == NULL) bug("UpdateScroll: Did not find the item");

    (void)button_changed(w, (XtPointer) ip, (XtPointer) NULL);
    return NULL;
}
/************************************************************************/
/* ARGSUSED */
static XtActionProc QuitPanel(w, event, params, nparams)
Widget w;          /* Unused */
XEvent *event;     /* Unused */
String *params;    /* Unused */
Cardinal *nparams; /* Unused */
{
    destroy_control_panel();
    XtCloseDisplay(XtDisplay(top_level));
    exit(0);
}
