/*
 *	<rubberband.c>
 */

#include "xmtv.h"

/*
 *  If the user moves the mouse out of the window, the line is erased
 *  and the routine resets so that the initial point must be chosen
 *  again.  This is the method a user would use if it were decided
 *  that the initial point was incorrect.
 */

/*
 *  Input format for SLCT:
 *
 *    OPCODE      SLCT
 *
 *    PARMS[0]    Channel -- Ignored for now.
 *                (X,Y positions are relative to this memory channel.)
 *    PARMS[1]    Select point type:
 *                0 - Point and click to choose 1 point.
 *                Rubber band types:
 *                1 - line from point 1 to point 2.
 *                2 - rectangle with center at point 1.
 *                3 - rectangle with corner at point 1.
 *                4 - 'V' Draws a line from start point to mouse and from
 *                  mouse to end point. BUF holds start and end points.
 *                  Returns start point and middle point.
 *    PARMS[2]    Click mode.
 *    PARMS[3]    Unused.
 *
 *    DATA_LENGTH 0 or 8 (characters).
 *
 *    DATA        If type is 'VBAND', 4 short integers are read to supply
 *                start and end points.
 */

/*
 *  Input format for SLCT:
 *
 *    STATUS      <0 if routine could not enable rubber band drawing;
 *                0 for a normal function exit.
 *
 *    RETURN_DATA_LENGTH = 5 (short ints).
 *
 *    DATA        5 short integers are returned:
 *                x0 -- the x position of the first point selected;
 *                y0 -- the y position of the first point selected;
 *                x1 -- the x position of the final point selected;
 *                y1 -- the y position of the final point selected;
 *                ch -- the character typed or (A,D,X for Mouse 1,2,3).
 *                For single point select mode, x1 = x0 and y1 = y0.
 */

/*  Internal Declarations. */

#define SelectTypeSingle 0         /* Choose a single point. */
#define SelectTypeRband  1         /* Simple rubber band line. */
#define SelectTypeCrect  2         /* Rectangle centered on initial point. */
#define SelectTypeRect   3         /* Rectangle drawn from initial point. */
#define SelectTypeVband  4         /* 'V' form of rubber band. */
#define NumberofTypes    5         /* Maximum number of types. */

#define SelectPushDrag   0         /* Push-Drag-Release mode. */
#define SelectPushPush   1         /* Push-Release-Move-Push mode. */
#define SelectPushPush2  2         /* Push-Drag-Release-Move-Push mode. */
#define NumberofPushes   3         /* Maximum number of Push modes. */

typedef struct rubberband {
    int channel;                   /* Unused at this point. */
    unsigned int buttonPushed;     /* Number of Button pushed. */
    short int currentMode;
    short int currentType;
    short int pushMode;
    int firstx, firsty;            /* First point selected. */
    int currentx, currenty;        /* Current position of the pointer. */
    int vbandx, vbandy;            /* Last point selected for "VBand". */
    GC SelectGC;                   /* GC used for drawing. */
    Cursor savedCursor;            /* Cursor widget had when started. */
    Widget widget;                 /* Widget drawing will appear. */
} RubberBand;

#define NumberofCursors  2         /* Maximum number of cursors defined. */
#define RESETCURSOR     -1

static String cursorNames[] = {
    "ll_angle",
    "ur_angle",
};

/* Routines. */

/*
 *  Draw rubber band line or rectangle from initial point to current
 *  point.  (and end point for 'Vband'). Uses XOR drawing so calling
 *  this routine twice will result in the line being erased.
 */
static void drawLine(r)
RubberBand *r;
{
    int x0, y0, x1, y1, xv, yv;
    unsigned int width, height;
    Display *dpy;
    GC xorgc;
    Window window;

    if (r == (RubberBand *)NULL) return;

    x0 = r->firstx;                                    /* Starting point. */
    y0 = r->firsty;
    x1 = r->currentx;                                    /* Ending point. */
    y1 = r->currenty;
    dpy = XtDisplay(r->widget);
    xorgc = r->SelectGC;
    window = XtWindow(r->widget);

    switch (r->currentType){
      case SelectTypeSingle:                             /* Single point. */
        XDrawPoint(dpy, window, xorgc, x1, y1);
        break;
      case SelectTypeRband:                          /* Rubber band line. */
        XDrawLine(dpy, window, xorgc, x0, y0, x1, y1);
        break;
      case SelectTypeCrect:               /* Rectangle centered at x0/y0. */
        x0 -= (x1 - x0);
        y0 -= (y1 - y0);        /* Fall through to TypeRect code to draw. */
      case SelectTypeRect:                       /* Rectangle from x0/y0. */
        width = max(x0, x1) - min(x0, x1);
        height = max(y0, y1) - min(y0, y1);
        x0 = min(x0, x1);
        y0 = min(y0, y1);
        XDrawRectangle(dpy, window, xorgc, x0, y0, width, height);
        break;
      case SelectTypeVband:   /* 'V' - Start to variable midpoint to end. */
        xv = r->vbandx;
        yv = r->vbandy;
        XDrawLine(dpy, window, xorgc, x0, y0, x1, y1);
        XDrawLine(dpy, window, xorgc, x1, y1, xv, yv);
        break;
      default:	/* Ignore unknown since there isn't much to be done. */
        break;
    }
}

/*  This routine simply updates the current point position. */
static void selectDone(r, cx, cy)
RubberBand *r;
int cx, cy;
{
    drawLine(r);    /* Erase old line before the current x/y are updated. */
    r->currentx = cx;               /* Update the current mouse position. */
    r->currenty = cy;
/*
 *  If SelectType is single, force the first position to match the
 *  current position.
 */
    if (r->currentType == SelectTypeSingle) {
      r->firstx = r->currentx;
      r->firsty = r->currenty;
    }
}

/* Set up the appropriate cursor. */
static void changeCursor(r, newentry)
RubberBand *r;
short int newentry;
{
    Arg args[10];
    Cardinal i;

    if ((newentry >= 0) && (newentry < NumberofCursors)) {
      i = 0;
      XtSetArg(args[i], XtNcursorName, (XtArgVal)cursorNames[newentry]); i++;
      XtSetValues(r->widget, args, i);
    } else if (newentry == RESETCURSOR) {
      i = 0;
      XtSetArg(args[i], XtNcursor, (XtArgVal)r->savedCursor); i++;
      XtSetValues(r->widget, args, i);
    }
}

/*
 *  When the first point is selected, this is called to remember it.
 *  It is also called when a reset to the current point is desired.
 *  This sets the current and initial points to the current point.
 */
static void setPosition(r, cx, cy)
RubberBand *r;
int cx, cy;
{
    r->currentx = cx;	/* Set the current position. */
    r->currenty = cy;

/*  If not drawing a V, remember the current position as the start position. */
    if (r->currentType != SelectTypeVband) {
      r->firstx = cx;
      r->firsty = cy;
    }
}

/* Convert the cursor position to screen coordinates (from cursor.c). */
static void scaleCursor(xin, yin, xout, yout)
int xin, yin, *xout, *yout;
{
    if (movecursor(xin, yin) == 0) {
      currentCursor(xout, yout);
    } else {
      *xout = 0;
      *yout = 0;
    }
}

/* Returns 1 if finished selecting; 0 otherwise... meaning call this again. */
static int EventCycle(r)
RubberBand *r;
{
    Boolean domore;
    Cardinal nparams = 0;
    Display *dpy;
    String params = NULL;
    Window window;
    Widget w;
    XEvent event;
    XtAppContext context;

    dpy = XtDisplay(r->widget);
    window = XtWindow(r->widget);
    context = XtDisplayToApplicationContext(dpy);

    changeCursor(r, 0);
    domore = True;

    while (domore == True) {                   /* Select the first point. */
      XtAppNextEvent(context, &event);
      if (XDebug)
        (void)fprintf(stderr, "  %s\n", event_names[event.type]);

      if (event.xany.window != window) {
        XtDispatchEvent(&event);
        continue;              /* Return to the start of the domore loop. */
      }

      r->buttonPushed = 0;
      switch (event.type) {
        case ButtonPress:
          if (r->pushMode == SelectPushDrag) {
            r->buttonPushed = event.xbutton.button;
            setPosition(r, event.xbutton.x, event.xbutton.y);
            drawLine(r);               /* Set and draw the initial point. */
            domore = False;
          }
          break;
        case ButtonRelease:
          if (r->pushMode != SelectPushDrag) {
            r->buttonPushed = event.xbutton.button;
            setPosition(r, event.xbutton.x, event.xbutton.y);
            drawLine(r);               /* Set and draw the initial point. */
            domore = False;
          }
          break;
        case Expose:
          w = XtWindowToWidget(event.xany.display, event.xany.window);
          (void)canvasExpose(w, &event, &params, &nparams);
          break;
        case MotionNotify:
          (void)cursorMotionEvent(w, &event, &params, &nparams);
          /* Ignore all Motion events until a button is pressed/released. */
          break;
        case EnterNotify:
          XSetInputFocus(dpy, window, RevertToPointerRoot, CurrentTime);
          break;
        case LeaveNotify:
          XSetInputFocus(dpy, PointerRoot, RevertToPointerRoot, CurrentTime);
          break;
        default:
          XtDispatchEvent(&event);
          break;
      } /* end switch */
    } /* end while(domore) */

    if (r->currentType == SelectTypeSingle) { /* ...then we are finished. */
      selectDone(r, event.xbutton.x, event.xbutton.y);
    } else {
      changeCursor(r, 1);
    }
                              /* Set up loop only if we need more events; */
                    /* otherwise, fall through to the end of the routine. */
    domore = (r->currentType != SelectTypeSingle) ? True : False;

    while (domore == True) {                            /* Drawing stage. */
      XtAppNextEvent(context, &event);
      if (XDebug)
        (void)fprintf(stderr, "  %s", event_names[event.type]);

      if (event.xany.window != window) {
        XtDispatchEvent(&event);
        if (XDebug) (void)fprintf(stderr, "\n");
        continue;              /* Return to the start of the domore loop. */
      }

      switch (event.type) {
        case ButtonPress:
          if (XDebug)
            (void)fprintf(stderr, "  button=%d x,y=%d,%d",
              event.xbutton.button, event.xbutton.x, event.xbutton.y);
          if (event.xbutton.button == r->buttonPushed) {
            if (r->pushMode != SelectPushDrag) {
              selectDone(r, event.xbutton.x, event.xbutton.y);
              domore = False;
            }
          }
          break;
        case ButtonRelease:
          if (XDebug)
            (void)fprintf(stderr, "  button=%d x,y=%d,%d",
              event.xbutton.button, event.xbutton.x, event.xbutton.y);
          if (event.xbutton.button == r->buttonPushed) {
            if (r->pushMode == SelectPushDrag) {
              selectDone(r, event.xbutton.x, event.xbutton.y);
              domore = False;
            }
          }
          break;
        case Expose:
          w = XtWindowToWidget(event.xany.display, event.xany.window);
          (void)canvasExpose(w, &event, &params, &nparams);
          drawLine(r);                               /* Erase old figure. */
          break;
        case MotionNotify:
          w = XtWindowToWidget(event.xany.display, event.xany.window);
          (void)cursorMotionEvent(w, &event, &params, &nparams);
          if (XDebug)
            (void)fprintf(stderr, "  state=%d x,y=%d,%d",
              event.xmotion.state, event.xmotion.x, event.xmotion.y);
          drawLine(r);                               /* Erase old figure. */
          r->currentx = event.xmotion.x;      /* Update current position. */
          r->currenty = event.xmotion.y;
          drawLine(r);                                /* Draw new figure. */
          break;
        case EnterNotify:
          XSetInputFocus(dpy, window, RevertToPointerRoot, CurrentTime);
          break;
        case LeaveNotify:
          drawLine(r);                               /* Erase old figure. */
          XSetInputFocus(dpy, PointerRoot, RevertToPointerRoot, CurrentTime);
          if (XDebug) (void)fprintf(stderr, "\n");
          return(0); /* Force a return back to the start of this routine. */
        default:                            /* Dispatch any other events. */
          XtDispatchEvent(&event);
          break;
      } /* end switch */
      if (XDebug) (void)fprintf(stderr, "\n");
    } /* end while(domore) */

    /* Finished selecting. */
    changeCursor(r, RESETCURSOR);
    return(1);
}

static void getPoints(r)
RubberBand *r;
{
    Arg args[10];
    Cardinal i;
    Display *dpy = XtDisplay(r->widget);
    XEvent event;

    /* Create a graphics context for the Select drawing routines. */
    r->SelectGC = XCreateGC(dpy, XtWindow(r->widget), 0, (XGCValues *)NULL);
    XSetForeground(dpy, r->SelectGC, (((unsigned long)1) << depth) - 1);
    XSetFunction(dpy, r->SelectGC, GXxor);
    XSetPlaneMask(dpy, r->SelectGC, AllPlanes);
                                 /* Use r->channel to fix the PlaneMask?? */
    XSetSubwindowMode(dpy, r->SelectGC, IncludeInferiors);

    i = 0;
    XtSetArg(args[i], XtNcursor, (XtArgVal)&r->savedCursor); i++;
    XtGetValues(r->widget, args, i);

    /* Discard any ButtonPress or ButtonRelease events encountered until now. */
    while (XCheckTypedEvent(dpy, ButtonPress, &event))
      /* NULL */ ;
    while (XCheckTypedEvent(dpy, ButtonRelease, &event))
      /* NULL */ ;

    while (EventCycle(r) == 0)/* Event loop; where everything gets done. */
      /* NULL */ ;

    XFreeGC(dpy, r->SelectGC);                    /* Release this GC. */
    return;
}

/*------------------------------------------------------------------------
 * This routine responds to opcode 71.
 *------------------------------------------------------------------------*/

int selectPoints(w, nwords)
Widget w;
short int *nwords;
{
    short int *ip;
    int x, y;
    RubberBand *r;
    RubberBand rdata;
    static Boolean inUse = False;

    r = &rdata;

/*  Will only happen if more than one connection at a time is supported. */
    if (inUse == True) {
      (void)fprintf(stderr, "Can't have more than one select at once!\n");
      return(-1);
    }

    r->channel = xbuf.parms[0];             /* Memory channel to draw on. */
    if ((r->channel < 1) || (r->channel > NGRTOT)) {
/* Ignore any bad channels for now...
      (void)fprintf(stderr, "Bad select channel = %d\n", r->channel);
      return(-2);
   ... */
    }

    r->currentType = (short int)xbuf.parms[1];             /* Point type. */
    if ((r->currentType < 0) || (r->currentType >= NumberofTypes)) {
      (void)fprintf(stderr, "Bad point type = %d\n", r->currentType);
      return(-3);
    }

    r->pushMode = (short int)xbuf.parms[2];                 /* Push mode. */
    if ((r->pushMode < 0) || (r->pushMode >= NumberofPushes)) {
      (void)fprintf(stderr, "Bad choice of push mode = %d\n", r->pushMode);
      return(-4);
    }

    if (r->currentType == SelectTypeVband) { /* Get start and end points. */
      if (xbuf.data_length != (4 * sizeof(short int))) {/* Need 4 shorts. */
        (void)fprintf(stderr,
          "Bad number of input positions: input %d (bytes) != required %d\n",
          xbuf.data_length, 4 * sizeof(short int));
        return(-5);
      }
      ip = (short *)xbuf.data;
      x = (int)*ip++;
      y = (int)*ip++;
      scaleCursor(x, y, &r->firstx, &r->firsty);
      x = (int)*ip++;
      y = (int)*ip++;
      scaleCursor(x, y, &r->vbandx, &r->vbandy);
    }
    r->widget = w;

    inUse = True;
    getPoints(r);

/* Convert to external coordinate system and then return the data. */
    RecordCursor(r->firstx, r->firsty);
    GetCursor(&ybuf.data[0], &ybuf.data[1]);
    RecordCursor(r->currentx, r->currenty);
    GetCursor(&ybuf.data[2], &ybuf.data[3]);
    ybuf.data[4] = (short int)r->buttonPushed;
    *nwords = 5;
    inUse = False;

    return(0);
}
