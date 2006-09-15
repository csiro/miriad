/*
	<widgets.c> - code for control panel widgets.

	13may92 jm  Original code.
	14nov95 jm  Corrected callback declaration syntax.
*/
#include "xmtv.h"

#include <X11/Xaw/Form.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Panner.h>
#include <X11/Xaw/Porthole.h>
#include <X11/Xaw/Toggle.h>

/* Private variables. */

static Boolean doingPan = True;
static Dimension canvasWidth = 0;
static Dimension canvasHeight = 0;
static Widget cursorPosition = (Widget)NULL;
static Widget staticPanner = (Widget)NULL;
static Widget zoomdown = (Widget)NULL;
static Widget zoomup = (Widget)NULL;

/* Source code. */

/************************************************************************/
/* ARGSUSED */
static void portholeCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;   /* Unused. */
XtPointer call_data;
{
    XawPannerReport *report = (XawPannerReport *)call_data;

    if (AppDebug)
      (void)fprintf(stderr, "portholeCB: x, y = %d %d",
        report->slider_x, report->slider_y);

    if (report->changed & ~(XawPRSliderX | XawPRSliderY)) {
      if (AppDebug) {
        (void)fprintf(stderr, " sw,sh,cw,ch = %d %d %d %d",
          report->slider_width, report->slider_height,
          report->canvas_width, report->canvas_height);
        (void)fprintf(stderr, " flag = 0X%0X\n", report->changed);
      }
      resizePorthole(w, report->slider_width, report->slider_height);
      setSizeOfPanner(report->canvas_width, report->canvas_height);
    } else {
      if (AppDebug)
        (void)fprintf(stderr, "\n");
    }

    return;
}

/************************************************************************/
/* ARGSUSED */
static void pannerCallback(w, client_data, call_data)
Widget w;                /* Unused. */
XtPointer client_data;   /* Unused. */
XtPointer call_data;
{
    int sx, sy;
    int width, height;
    XawPannerReport *report = (XawPannerReport *)call_data;

    sx = (int)report->slider_x;
    sy = (int)report->slider_y;
    width = report->canvas_width - report->slider_width;
    height = report->canvas_height - report->slider_height;

    if (AppDebug)
      (void)fprintf(stderr, "pannerCB: x, y = %d %d w, h = %d %d\n",
        sx, sy, width, height);

    localPanEvent((doingPan == True), sx, sy, width, height);

    return;
}

/************************************************************************/
/* ARGSUSED */
static void toggleCallback(w, client_data, call_data)
Widget w;                /* Unused. */
XtPointer client_data;
XtPointer call_data;     /* Unused. */
{
    Arg args[10];
    Boolean toggle;
    Cardinal i;
    Dimension tmpCW, tmpCH, tmpSW, tmpSH;
    static Dimension savedCW, savedCH;
    static Dimension savedSW, savedSH;
    Position tmpSX, tmpSY;
    static Position savedSX = 1000;
    static Position savedSY = 1000;

    toggle = (Boolean)client_data;
    if (toggle != doingPan) {
      doingPan = toggle;

      i = 0;
      XtSetArg(args[i], XtNsliderX,      &tmpSX); i++;
      XtSetArg(args[i], XtNsliderY,      &tmpSY); i++;
      XtSetArg(args[i], XtNsliderWidth,  &tmpSW); i++;
      XtSetArg(args[i], XtNsliderHeight, &tmpSH); i++;
      XtSetArg(args[i], XtNcanvasWidth,  &tmpCW); i++;
      XtSetArg(args[i], XtNcanvasHeight, &tmpCH); i++;
      XtGetValues(staticPanner, args, i);

      if (doingPan == True) {
        i = 0;
        XtSetArg(args[i], XtNsliderX,      savedSX); i++;
        XtSetArg(args[i], XtNsliderY,      savedSY); i++;
        XtSetArg(args[i], XtNsliderWidth,  savedSW); i++;
        XtSetArg(args[i], XtNsliderHeight, savedSH); i++;
        XtSetArg(args[i], XtNcanvasWidth,  savedCW); i++;
        XtSetArg(args[i], XtNcanvasHeight, savedCH); i++;
        XtSetValues(staticPanner, args, i);
        zoomEnable(True, True);
      } else {
        i = 0;
        XtSetArg(args[i], XtNsliderX,      savedSX); i++;
        XtSetArg(args[i], XtNsliderY,      savedSY); i++;
        XtSetArg(args[i], XtNsliderWidth,  200);     i++;
        XtSetArg(args[i], XtNsliderHeight, 200);     i++;
        XtSetArg(args[i], XtNcanvasWidth,  2200);    i++;
        XtSetArg(args[i], XtNcanvasHeight, 2200);    i++;
        XtSetValues(staticPanner, args, i);

        savedSW = tmpSW;
        savedSH = tmpSH;
        savedCW = tmpCW;
        savedCH = tmpCH;
        zoomEnable(False, False);
      }

      savedSX = tmpSX;
      savedSY = tmpSY;

      if (AppDebug)
        (void)fprintf(stderr, "toggleCB: doingPan = %s\n",
          (doingPan == True ? "True" : "False"));
    }

    return;
}

/************************************************************************/
/* ARGSUSED */
static void atodCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;     /* Unused. */
{
    String button = (String)client_data;

    if (button == (String)NULL)
      button = XtName(w);

    buttonPressed(button[0]);

    return;
}

/************************************************************************/
/* ARGSUSED */
static void quitCallback(w, client_data, call_data)
Widget w;                /* Unused. */
XtPointer client_data;   /* Unused. */
XtPointer call_data;     /* Unused. */
{
    (void)CloseDown(NULL, NULL, NULL, NULL);

    return;
}

/************************************************************************/
static Widget CreatePanner(parent)
Widget parent;
{
    Arg args[10];
    Cardinal i;
    Widget shell, form, pan, fiddle, close, panner;
    static XtPopdownIDRec xpid;

    i = 0;
    shell = XtCreatePopupShell("panshell", transientShellWidgetClass,
      parent, args, i);
    XtAddCallback(parent, XtNcallback, XtCallbackNone, (XtPointer)shell);

    i = 0;
    form = XtCreateManagedWidget("panform", formWidgetClass, shell, args, i);

    i = 0;
    XtSetArg(args[i], XtNstate, (XtArgVal)True); i++;
    pan = XtCreateManagedWidget("pan", toggleWidgetClass, form, args, i);
    XtAddCallback(pan, XtNcallback, toggleCallback, (XtPointer)True);

    i = 0;
    XtSetArg(args[i], XtNfromHoriz,  (XtArgVal)pan); i++;
    XtSetArg(args[i], XtNradioGroup, (XtArgVal)pan); i++;
    fiddle = XtCreateManagedWidget("fiddle", toggleWidgetClass, form, args, i);
    XtAddCallback(fiddle, XtNcallback, toggleCallback, (XtPointer)False);

    xpid.shell_widget = shell;
    xpid.enable_widget = parent;
    i = 0;
    XtSetArg(args[i], XtNfromHoriz, (XtArgVal)fiddle); i++;
    close = XtCreateManagedWidget("close", commandWidgetClass, form, args, i);
    XtAddCallback(close, XtNcallback, XtCallbackPopdown, (XtPointer)&xpid);

    i = 0;
    XtSetArg(args[i], XtNfromVert, (XtArgVal)pan); i++;
    panner = XtCreateManagedWidget("panner", pannerWidgetClass, form, args, i);
    XtAddCallback(panner, XtNreportCallback, pannerCallback, (XtPointer)NULL);

    return(panner);
}

/************************************************************************/
static Widget BuildControlPanel(parent, maxgrey)
Widget parent;
int maxgrey;
{
    Arg args[10];
    Cardinal i;
    Widget panner, form, button, menu;

    i = 0;
    form = XtCreateManagedWidget("form", formWidgetClass, parent, args, i);

    /********** First row **********/

    i = 0;
    button = XtCreateManagedWidget("A", commandWidgetClass, form, args, i);
    XtAddCallback(button, XtNcallback, atodCallback, (XtPointer)"a");
    i = 0;
    XtSetArg(args[i],  XtNfromHoriz, (XtArgVal)button); i++;
    button = XtCreateManagedWidget("B", commandWidgetClass, form, args, i);
    XtAddCallback(button, XtNcallback, atodCallback, (XtPointer)"b");
    i = 0;
    XtSetArg(args[i],  XtNfromHoriz, (XtArgVal)button); i++;
    button = XtCreateManagedWidget("C", commandWidgetClass, form, args, i);
    XtAddCallback(button, XtNcallback, atodCallback, (XtPointer)"c");
    i = 0;
    XtSetArg(args[i],  XtNfromHoriz, (XtArgVal)button); i++;
    button = XtCreateManagedWidget("D", commandWidgetClass, form, args, i);
    XtAddCallback(button, XtNcallback, atodCallback, (XtPointer)"d");

    i = 0;
    XtSetArg(args[i],  XtNfromHoriz, (XtArgVal)button); i++;
    button = XtCreateManagedWidget("Panner", commandWidgetClass, form, args, i);
    panner = CreatePanner(button);

    i = 0;
    XtSetArg(args[i],  XtNfromHoriz, (XtArgVal)button); i++;
    menu = CreateLUTMenu(form);
    XtSetValues(menu, args, i);
    menu = button; /* Save for next menu location. */

    /********** Second row **********/

    i = 0;
    XtSetArg(args[i],  XtNfromVert, (XtArgVal)menu); i++;
    button = XtCreateManagedWidget("+", commandWidgetClass, form, args, i);
    XtAddCallback(button, XtNcallback, localZoomUp, (XtPointer)NULL);
    zoomup = button;
    i = 0;
    XtSetArg(args[i],  XtNfromVert, (XtArgVal)menu); i++;
    XtSetArg(args[i],  XtNfromHoriz, (XtArgVal)button); i++;
    button = XtCreateManagedWidget("-", commandWidgetClass, form, args, i);
    XtAddCallback(button, XtNcallback, localZoomDown, (XtPointer)NULL);
    zoomdown = button;
    i = 0;
    XtSetArg(args[i],  XtNfromVert, (XtArgVal)menu); i++;
    XtSetArg(args[i],  XtNfromHoriz, (XtArgVal)button); i++;
    button = XtCreateManagedWidget("Reset", commandWidgetClass, form, args, i);
    XtAddCallback(button, XtNcallback, localReset, (XtPointer)form);
    i = 0;
    XtSetArg(args[i],  XtNfromVert, (XtArgVal)menu); i++;
    XtSetArg(args[i],  XtNfromHoriz, (XtArgVal)button); i++;
    button = XtCreateManagedWidget("Resize", commandWidgetClass, form, args, i);
    XtAddCallback(button, XtNcallback, resizeCallback,
      (XtPointer)XtParent(parent)); /* toplevel */

    i = 0;
    XtSetArg(args[i],  XtNfromVert, (XtArgVal)menu); i++;
    XtSetArg(args[i],  XtNfromHoriz, (XtArgVal)button); i++;
    button = XtCreateManagedWidget("Quit", commandWidgetClass, form, args, i);
    XtAddCallback(button, XtNcallback, quitCallback, (XtPointer)NULL);

    i = 0;
    XtSetArg(args[i],  XtNfromVert, (XtArgVal)menu); i++;
    XtSetArg(args[i],  XtNfromHoriz, (XtArgVal)button); i++;
    menu = CreateChannelMenu(form, maxgrey);
    XtSetValues(menu, args, i);

    i = 0;
    XtSetArg(args[i],  XtNfromVert, (XtArgVal)menu); i++;
    XtSetArg(args[i],  XtNjustify, (XtArgVal)XtJustifyLeft); i++;
    cursorPosition = XtCreateManagedWidget("cursorMotion", labelWidgetClass,
      form, args, i);

    return(panner);
}

/************************************************************************/
static Widget BuildCanvas(parent, depth, cursor, cursorName, w, h)
Widget parent;
int depth;
Cursor cursor;
String cursorName;
Dimension w, h;
{
    Arg args[10];
    Cardinal i;
    Widget ph, canvas;

    i = 0;                                    /* For faster updates.... */
    XtSetArg(args[i],  XtNbackgroundPixmap, (XtArgVal)None); i++;
    ph = XtCreateManagedWidget("porthole", portholeWidgetClass, parent,
      args, i);
    XtAddCallback(ph, XtNreportCallback, portholeCallback, (XtPointer)NULL);

    i = 0;
    XtSetArg(args[i],  XtNwidth,               (XtArgVal)w); i++;
    XtSetArg(args[i],  XtNheight,              (XtArgVal)h); i++;
    XtSetArg(args[i],  XtNdepth,           (XtArgVal)depth); i++;
    XtSetArg(args[i],  XtNcursor,         (XtArgVal)cursor); i++;
    XtSetArg(args[i],  XtNcursorName, (XtArgVal)cursorName); i++;
    canvas = XtCreateManagedWidget("canvas", simpleWidgetClass, ph, args, i);

    return(canvas);
}

/************************************************************************/
Widget BuildWindow(parent, maxgrey, depth, cursor, cursorName, w, h)
Widget parent;
int maxgrey;
int depth;
Cursor cursor;
String cursorName;
Dimension w, h;
{
    Arg args[10];
    Cardinal i;
    Widget paned, canvas;

    i = 0;
    paned = XtCreateManagedWidget("paned", panedWidgetClass, parent, args, i);

    staticPanner = BuildControlPanel(paned, maxgrey);
    canvas = BuildCanvas(paned, depth, cursor, cursorName, w, h);

    XtInstallAccelerators(canvas, XtNameToWidget(paned, "*.form.A"));
    XtInstallAccelerators(canvas, XtNameToWidget(paned, "*.form.B"));
    XtInstallAccelerators(canvas, XtNameToWidget(paned, "*.form.C"));
    XtInstallAccelerators(canvas, XtNameToWidget(paned, "*.form.D"));
    XtInstallAccelerators(canvas, XtNameToWidget(paned, "*.form.Resize"));

    return(canvas);
}

/************************************************************************/
/* ARGSUSED */
void keyboardPressed(w, event, params, nparams)
Widget w;
XEvent *event;     /* Unused */
String *params;
Cardinal *nparams;
/*
  Keyboard button pressed while over canvas.
------------------------------------------------------------------------*/
{
    if (*nparams == 1)
      buttonPressed(*params[0]);

    return;
}

/************************************************************************/
/* ARGSUSED */
void canvasExpose(w, event, params, nparams)
Widget w;          /* Unused */
XEvent *event;     /* Unused */
String *params;    /* Unused */
Cardinal *nparams; /* Unused */
/*
  Take care of repainting the canvas.
------------------------------------------------------------------------*/
{
    imageRefresh();

    return;
}

/************************************************************************/
/* ARGSUSED */
void cursorMotionEvent(w, event, params, nparams)
Widget w;                /* Unused. */
XEvent *event;
String *params;          /* Unused. */
Cardinal *nparams;       /* Unused. */
/*
  Mouse motion while over canvas.
------------------------------------------------------------------------*/
{
    char label[100];
    int cx, cy;
    float value;
    Arg args[10];
    Cardinal i;

    cx = event->xmotion.x;
    cy = event->xmotion.y;
    zoomCursor(&cx, &cy);
    value = pixelValue(cx, cy);

    if (XDebug)
      (void)sprintf(label,
        "Event Position %3d %3d Cursor Position: %3d %3d Value: %G",
        event->xmotion.x, event->xmotion.y, cx, cy, value);
    else
      (void)sprintf(label, "Cursor Position: %3d %3d Value: %G", cx, cy, value);

    if (cursorPosition != (Widget)NULL) {
      i = 0;
      XtSetArg(args[i], XtNlabel, (XtArgVal)label); i++;
      XtSetValues(cursorPosition, args, i);
    } else {
      (void)fprintf(stderr, "%s\n", label);
    }

    return;
}

/************************************************************************/
/* ARGSUSED */
void buttonEvent(w, event, params, nparams)
Widget w;          /* Unused */
XEvent *event;
String *params;
Cardinal *nparams;
/*
  Either button 1 down, motion, or debug toggle selected.
------------------------------------------------------------------------*/
{
    if (*nparams == 1) {
      if ((params[0][0] == 'F') && (params[0][1] == '8')) {
        AppDebug = !AppDebug;
        (void)fprintf(stderr,
          "XMTV: Application debugging Toggled %s with button F8.\n",
          ((AppDebug == True) ? "on" : "off"));
      }
      if ((params[0][0] == 'F') && (params[0][1] == '9')) {
        XDebug = !XDebug;
        (void)fprintf(stderr, "XMTV: X-Debugging Toggled %s with button F9.\n",
          ((XDebug == True) ? "on" : "off"));
      }
    } else {
      switch (event->type) {
        case MotionNotify:
          RecordCursor(event->xmotion.x, event->xmotion.y);
          break;
        case ButtonPress:
          RecordCursor(event->xbutton.x, event->xbutton.y);
          break;
        default: /* Do nothing. */
          break;
      }
    }

    return;
}

/************************************************************************/
void setSizeOfPanner(cw, ch)
Dimension cw, ch;
{
    Arg args[10];
    Cardinal i;

    if (doingPan == True) {
      if ((cw != canvasWidth) || (ch != canvasHeight)) {
        canvasWidth = cw;
        canvasHeight = ch;

        if (AppDebug)
          (void)fprintf(stderr, "setSizeOfPanner: cw,ch = %d %d\n", cw, ch);

        i = 0;
        XtSetArg(args[i], XtNsliderWidth,  (XtArgVal)cw);  i++;
        XtSetArg(args[i], XtNsliderHeight, (XtArgVal)ch);  i++;
        XtSetArg(args[i], XtNcanvasWidth,  (XtArgVal)cw);  i++;
        XtSetArg(args[i], XtNcanvasHeight, (XtArgVal)ch);  i++;
        XtSetValues(staticPanner, args, i);
      }
    }
    return;
}

/************************************************************************/
void movePanner(mag, x, y)
int mag;
int x, y;
{
    Arg args[10];
    Cardinal i;
    Dimension w, h;

    if (doingPan == True) {
      w = canvasWidth * mag;
      h = canvasHeight * mag;

      if (AppDebug)
        (void)fprintf(stderr,
          "movePanner: x,y,zw,zh,cw,ch = %d %d %d %d %d %d\n",
          x, y, w, h, canvasWidth, canvasHeight);

      i = 0;
      XtSetArg(args[i], XtNsliderX,      (XtArgVal)x);  i++;
      XtSetArg(args[i], XtNsliderY,      (XtArgVal)y);  i++;
      XtSetArg(args[i], XtNcanvasWidth,  (XtArgVal)w);  i++;
      XtSetArg(args[i], XtNcanvasHeight, (XtArgVal)h);  i++;
      XtSetValues(staticPanner, args, i);
    }
    return;
}

/************************************************************************/
void zoomEnable(up, down)
Boolean up, down;
{
    static Boolean lastUp, lastDown;
    static Boolean lastUpSaved = False;
    static Boolean lastDownSaved = False;

    if ((zoomup == (Widget)NULL) || (zoomdown == (Widget)NULL))
      return;

    if (doingPan == True) {
      XtSetSensitive(zoomup, ((lastUpSaved == True) ? lastUp : up));
      lastUpSaved = False;
    } else {
      lastUp = XtIsSensitive(zoomup);
      lastUpSaved = True;
      XtSetSensitive(zoomup, up);
    }

    if (doingPan == True) {
      XtSetSensitive(zoomdown, ((lastDownSaved == True) ? lastDown : down));
      lastDownSaved = False;
    } else {
      lastDown = XtIsSensitive(zoomdown);
      lastDownSaved = True;
      XtSetSensitive(zoomdown, down);
    }
}

/************************************************************************/
void toggleReset(form)
Widget form;
{
    Widget pan = XtNameToWidget(form, "*pan");

    if (pan != (Widget)NULL)
      XawToggleSetCurrent(pan, (XtPointer)XtName(pan));
}
