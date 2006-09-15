#include "sxmtv.h"
#include <X11/cursorfont.h>

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
 *    RETURN_DATA_LENGTH 4 (short ints).
 *
 *    DATA        4 short integers are returned:
 *                x0 -- the x position of the first point selected;
 *                y0 -- the y position of the first point selected;
 *                x1 -- the x position of the last point selected;
 *                y1 -- the y position of the last point selected.
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

#define NumberofCursors  2         /* Maximum number of cursors defined. */

typedef struct rubberband {
    int channel;                   /* Unused at this point. */
    unsigned int buttonPushed;     /* Number of Button pushed. */
    short int currentMode;
    short int currentType;
    short int pushMode;
    int firstx, firsty;            /* First point selected. */
    int lastx, lasty;              /* Last point selected for "VBand". */
    int currentx, currenty;        /* Current position of the pointer. */
    GC SelectGC;                   /* GC used for drawing. */
    Cursor cursors[NumberofCursors]; /* A different cursor for each mode. */
} RubberBand;

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
    GC sgc;

    if (r == (RubberBand *)NULL) return;

    x0 = r->firstx;                                    /* Starting point. */
    y0 = r->firsty;
    x1 = r->currentx;                                    /* Ending point. */
    y1 = r->currenty;

    sgc = r->SelectGC;
    switch (r->currentType){
      case SelectTypeSingle:                             /* Single point. */
        XDrawPoint(display, win, sgc, r->currentx, r->currenty);
        break;
      case SelectTypeRband:                          /* Rubber band line. */
        XDrawLine(display, win, sgc, x0, y0, x1, y1);
        break;
      case SelectTypeCrect:               /* Rectangle centered at x0/y0. */
        x0 -= (x1 - x0);
        y0 -= (y1 - y0);        /* Fall through to TypeRect code to draw. */
      case SelectTypeRect:                       /* Rectangle from x0/y0. */
        width = max(x0, x1) - min(x0, x1);
        height = max(y0, y1) - min(y0, y1);
        x0 = min(x0, x1);
        y0 = min(y0, y1);
        XDrawRectangle(display, win, sgc, x0, y0, width, height);
        break;
      case SelectTypeVband:   /* 'V' - Start to variable midpoint to end. */
        xv = r->lastx;
        yv = r->lasty;
        XDrawLine(display, win, sgc, x0, y0, x1, y1);
        XDrawLine(display, win, sgc, x1, y1, xv, yv);
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
    Cursor newcursor;

    if ((newentry < 0) || (newentry >= NumberofCursors)) return;

    if ((newcursor = r->cursors[newentry]) != (Cursor)NULL)
      XDefineCursor(display, win, newcursor);
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
    int cx, cy;

    cx = Memory_x(xin);
    cy = Memory_y(yin);
					/* -> wrt to window           */
    *xout = cx - sc_centre_x + sc_width2 - 1;
    *yout = cy - sc_centre_y + sc_height2 - 1;
}

/* select uses its own cursors.  initializeCursor() creates them. */
static void initializeCursor(r)
RubberBand *r;
{
    register int i;

    static int cursorShape[] = {
      XC_ll_angle,
      XC_ur_angle
    };

    for (i = 0; i < NumberofCursors; i++) {
      r->cursors[i] = (Cursor)NULL; /* Null in case some are missing. */
      if (cursorShape[i] > 0)
        r->cursors[i] = XCreateFontCursor(display, cursorShape[i]);
    }
}

static void freeCursors(r)
RubberBand *r;
{
    register int i;

    for (i = 0; i < NumberofCursors; i++) {
      if (r->cursors[i] != (Cursor)NULL)
        XFreeCursor(display, r->cursors[i]);
    }
}

/* Returns 1 if finished selecting; 0 otherwise... meaning call this again. */
static int SelectCycle(r)
RubberBand *r;
{
    short int domore;
    int endEventType;
    unsigned long totalmask, motionmask, endmask;
    XEvent event;

    changeCursor(r, 0);
    domore = True;
    while (domore == True) {                   /* Select the first point. */
      XNextEvent(display, &event);
      if (XDebug)
        (void)fprintf(stderr, "  %s\n", event_names[event.type]);
      r->buttonPushed = 0;
      switch (event.type) {
        case Expose:
          while (XCheckTypedEvent(event.xexpose.display, Expose, &event))
            /* NULL */ ;
          if (SXMTVDebug)
            (void)fprintf(stderr, "expose event w,h,x,y %d %d %d %d\n",
                          Cur_Xsize, Cur_Ysize, Cur_Xzero, Cur_Yzero);
                                                  /* Repaint the picture. */
          scrwrt(0, 0, Screen_Width - 1, Screen_Height - 1);
          break;
        case ButtonPress:
          if (event.xbutton.button == Button1) {
            if (r->pushMode == SelectPushDrag) {
              r->buttonPushed = event.xbutton.button;
              setPosition(r, event.xbutton.x, event.xbutton.y);
              drawLine(r);             /* Set and draw the initial point. */
              domore = False;
            }
          }
          break;
        case ButtonRelease:
          if (event.xbutton.button == Button1) {
            if (r->pushMode != SelectPushDrag) {
              r->buttonPushed = event.xbutton.button;
              setPosition(r, event.xbutton.x, event.xbutton.y);
              drawLine(r);             /* Set and draw the initial point. */
              domore = False;
            }
          }
          break;
        case MotionNotify:
          /* Ignore all Motion events until a button is pressed/released. */
          break;
        case EnterNotify:
          XSetInputFocus(display, win, RevertToPointerRoot, CurrentTime);
          break;
        case LeaveNotify:
          XSetInputFocus(display, PointerRoot, RevertToPointerRoot,
            CurrentTime);
          break;
        default:                              /* Ignore any other events. */
          break;
      } /* end switch */
    } /* end while(domore) */

    domore = True;   /* Reset the flag; SelectTypeSingle may turn it off. */

    if (r->currentType == SelectTypeSingle) { /* ...then we are finished. */
      selectDone(r, event.xbutton.x, event.xbutton.y);
      domore = False; /* Reset to fall through to the end of the routine. */
    } else {            /* Set up some masks for use in the next section. */
      if (r->pushMode == SelectPushDrag) {
        motionmask = ButtonMotionMask;
        endmask = ButtonReleaseMask;
        endEventType = ButtonRelease;
      } else {
        motionmask = PointerMotionMask;
        endmask = ButtonPressMask;
        endEventType = ButtonPress;
      }
      totalmask = EnterWindowMask | LeaveWindowMask | ExposureMask;
      totalmask |= motionmask | endmask;
      changeCursor(r, 1);
    }

    while (domore == True) {                            /* Drawing stage. */
                            /* Block until there is an interesting event. */
      XMaskEvent(display, totalmask, &event);
      if (XDebug)
        (void)fprintf(stderr, "  %s", event_names[event.type]);
      switch (event.type) {
        case Expose:
          while (XCheckTypedEvent(display, Expose, &event))
            /* NULL */ ;
          if (SXMTVDebug)
            (void)fprintf(stderr, "expose event w,h,x,y %d %d %d %d\n",
                          Cur_Xsize, Cur_Ysize, Cur_Xzero, Cur_Yzero);
                                                  /* Repaint the picture. */
          scrwrt(0, 0, Screen_Width - 1, Screen_Height - 1);
          drawLine(r);                               /* Erase old figure. */
          break;
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
        case MotionNotify:
          /* Save only the most recent motion event; motion compression.  */
          while (XCheckMaskEvent(display, motionmask | endmask, &event)) {
            if (event.type == endEventType)
              break;
          }
          if (XDebug)
            (void)fprintf(stderr, "  state=%d x,y=%d,%d",
              event.xmotion.state, event.xmotion.x, event.xmotion.y);
          drawLine(r);                               /* Erase old figure. */
          if (event.type != endEventType) {
            r->currentx = event.xmotion.x;    /* Update current position. */
            r->currenty = event.xmotion.y;
          } else {
            r->currentx = event.xbutton.x;    /* Update current position. */
            r->currenty = event.xbutton.y;
            /* Push this event back on the stack so the next pass through */
            /* the event loop will terminate properly. */
            XPutBackEvent(display, &event);
          }
          drawLine(r);                                /* Draw new figure. */
          break;
        case EnterNotify:
          XSetInputFocus(display, win, RevertToPointerRoot, CurrentTime);
          break;
        case LeaveNotify:
          XSetInputFocus(display, PointerRoot, RevertToPointerRoot,
            CurrentTime);
          drawLine(r);                               /* Erase old figure. */
          if (XDebug) (void)fprintf(stderr, "\n");
          return(0); /* Force a return back to the start of this routine. */
          break;
        default:                              /* Ignore any other events. */
          break;
      } /* end switch */
      if (XDebug) (void)fprintf(stderr, "\n");
    } /* end while(domore) */

    /* Finished selecting. */
    return(1);
}

static short int getPoints(r)
RubberBand *r;
{
    XEvent event;

    /* Create a graphics context for the Select drawing routines. */
    r->SelectGC = XCreateGC(display, win, 0, (XGCValues *)NULL);
    XSetForeground(display, r->SelectGC,
      (((unsigned long)1) << depth) - 1);
/*    BlackPixel(display, DefaultScreen(display))); */
    XSetFunction(display, r->SelectGC, GXxor);
    XSetPlaneMask(display, r->SelectGC, AllPlanes);
                                 /* Use r->channel to fix the PlaneMask?? */
    XSetSubwindowMode(display, r->SelectGC, IncludeInferiors);

    /* Discard any ButtonPress or ButtonRelease events encountered until now. */
    while (XCheckTypedEvent(display, ButtonPress, &event))
      /* NULL */ ;
    while (XCheckTypedEvent(display, ButtonRelease, &event))
      /* NULL */ ;

    while (SelectCycle(r) == 0)/* Event loop; where everything gets done. */
      /* NULL */ ;

    XFreeGC(display, r->SelectGC);                    /* Release this GC. */

    XDefineCursor(display, win, cursor);  /* cursor is a global variable. */

/* Convert to external coordinate system and then return the data. */
    RecordCursor((int)r->firstx, (int)r->firsty);
    GetCursor(&ybuf.data[0], &ybuf.data[1]);
    RecordCursor((int)r->currentx, (int)r->currenty);
    GetCursor(&ybuf.data[2], &ybuf.data[3]);
    ybuf.data[4] = (short int)r->buttonPushed;
    return(5);
}

/*------------------------------------------------------------------------
 * This routine responds to opcode 71.
 *------------------------------------------------------------------------*/

int selectPoints(nwords)
short int *nwords;
{
    short int *ip;
    static short int inUse = 0;
    int x, y;
    RubberBand *r;
    RubberBand rdata;

    r = &rdata;

/*  Will only happen if more than one connection at a time is supported. */
    if (inUse != 0) {
      (void)fprintf(stderr, "Can't have more than one select at once!\n");
      return(-1);
    }

    r->channel = xbuf.parms[0];             /* Memory channel to draw on. */
    if ((r->channel < 1) || (r->channel > NGRTOT)) {
      (void)fprintf(stderr, "Bad select channel = %d\n", r->channel);
      return(-2);
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
      if ((xbuf.data_length/sizeof(short int)) != 4) {  /* Need 4 shorts. */
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
      scaleCursor(x, y, &r->lastx, &r->lasty);
    }

    inUse = 1;

    initializeCursor(r);
    *nwords = (short int)getPoints(r);
    freeCursors(r);                              /* Free up cursor array. */

    inUse = 0;
    return(0);
}
