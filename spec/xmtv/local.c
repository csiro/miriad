/*
	<local.c> - A module of local routines of interest for XMTV.
	12may92  jm  Original code (converted from MSSS local routines).
	13nov92  jm  Modified local fiddle code to permit full range
		     of contrast and bias values.
*/

#include "xmtv.h"

/************************************************************************/
void localResize(w)
Widget w;
/*
    Toggle the size of the display between full screen and initial size.
------------------------------------------------------------------------*/
{
    Arg args[10];
    Cardinal i;
    Dimension width, height;
    Position x, y;

    static Boolean fullScreen = True;
    static Dimension savedWidth, savedHeight;
    static Position savedX, savedY;

    if (fullScreen == True) { /* Set the canvas to the maximum size. */
      fullScreen = False;
      i = 0;
      XtSetArg(args[i], XtNx,      (XtArgVal)&savedX);      i++;
      XtSetArg(args[i], XtNy,      (XtArgVal)&savedY);      i++;
      XtSetArg(args[i], XtNwidth,  (XtArgVal)&savedWidth);  i++;
      XtSetArg(args[i], XtNheight, (XtArgVal)&savedHeight); i++;
      XtGetValues(w, args, i);
      x = y = 0;
      width = Screen_Width;
      height = Screen_Height;
    } else { /* Reset the canvas back to the previous size. */
      fullScreen = True;
      x = savedX;
      y = savedY;
      width = savedWidth;
      height = savedHeight;
    }

    i = 0;
    XtSetArg(args[i], XtNx,      (XtArgVal)x);      i++;
    XtSetArg(args[i], XtNy,      (XtArgVal)y);      i++;
    XtSetArg(args[i], XtNwidth,  (XtArgVal)width);  i++;
    XtSetArg(args[i], XtNheight, (XtArgVal)height); i++;
    XtSetValues(w, args, i);

    if (AppDebug)
      (void)fprintf(stderr,
        "resize toggled: W H  X Y %d %d  %d %d\n", width, height, x, y);
}

/************************************************************************/
/* ARGSUSED */
XtCallbackProc resizeCallback(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;     /* Unused */
/*
    Toggle the size of the display between full screen and initial size.
------------------------------------------------------------------------*/
{
    Widget top = (Widget)client_data;

    localResize(top);
}

/************************************************************************/
static void privateZoom(newmag)
int newmag;
/*
    Private routine to adjust the zoom.
------------------------------------------------------------------------*/
{
    register int i;
    Boolean upState, downState;

    if (newmag < 1)            newmag = 1;
    else if (newmag > MAXZOOM) newmag = MAXZOOM;

    upState = (newmag < MAXZOOM) ? True : False;
    downState = (newmag > 1) ? True : False;
    zoomEnable(upState, downState);

    if (newmag == upleft_mag)  return;

    if (AppDebug) (void)fprintf(stderr, "privateZoom: Z X Y %d %d %d",
      upleft_mag, upleft_x[0], upleft_y[0]);

    for (i = 0; i < NGREY; i++){
      upleft_x[i] /= upleft_mag;
      upleft_y[i] /= upleft_mag;
      upleft_x[i] += (sc_centre_x / upleft_mag);
      upleft_y[i] += (sc_centre_y / upleft_mag);
      if (upleft_x[i] >= Screen_Width) upleft_x[i] -= Screen_Width;
      if (upleft_y[i] >= Screen_Height) upleft_y[i] -= Screen_Height;
      upleft_x[i] -= (sc_centre_x / newmag);
      upleft_y[i] -= (sc_centre_y / newmag);
      if (upleft_x[i] < 0) upleft_x[i] += Screen_Width;
      if (upleft_y[i] < 0) upleft_y[i] += Screen_Height;
      upleft_x[i] *= newmag;
      upleft_y[i] *= newmag;
    }
    upleft_mag = newmag;

    if (AppDebug) (void)fprintf(stderr, " => %d %d %d\n",
      upleft_mag, upleft_x[0], upleft_y[0]);

    movePanner(upleft_mag, upleft_x[0], upleft_y[0]);
    imageRefresh();
}

/************************************************************************/
/* ARGSUSED */
XtCallbackProc localZoomDown(w, client_data, call_data)
Widget w;                /* Unused */
XtPointer client_data;   /* Unused */
XtPointer call_data;     /* Unused */
/*
    Zoom down the display one step.
------------------------------------------------------------------------*/
{
    privateZoom(upleft_mag/2);
}

/************************************************************************/
/* ARGSUSED */
XtCallbackProc localZoomUp(w, client_data, call_data)
Widget w;                /* Unused */
XtPointer client_data;   /* Unused */
XtPointer call_data;     /* Unused */
/*
    Zoom up the display one step.
------------------------------------------------------------------------*/
{
    privateZoom(upleft_mag*2);
}

/************************************************************************/
/* ARGSUSED */
XtCallbackProc localReset(w, client_data, call_data)
Widget w;                /* Unused */
XtPointer client_data;
XtPointer call_data;     /* Unused */
/*
    Reset everything possible.
------------------------------------------------------------------------*/
{
    register int i;
    Widget form = (Widget)client_data;

    cur_chan = 1;                                     /* Channel 1 set. */
    rwgraph = 0;                                /* Graphics turned off. */

    if (form != (Widget)NULL)       /* Set the Panner toggles to Pan... */
      toggleReset(form);          /* ...mode before resetting the zoom. */

    upleft_mag = 1;            /* Reset the zoom and scroll parameters. */
    for (i = 0; i < NGREY; i++)
      upleft_x[i] = upleft_y[i] = 0;
    privateZoom(upleft_mag);

    changeOFM("B&W");
    (void)lutRamp(1, NColour);

    if (form != (Widget)NULL) {
      movePanner(upleft_mag, upleft_x[0], upleft_y[0]);
      resetMenus(1, 1); /* ("B&W", cur_chan) */
      imageRefresh();
    }
}

/************************************************************************/
void localPanEvent(panMode, x, y, width, height)
int panMode;
int x, y, width, height;
/*
    This routine performs an interactive fiddle of the LUT on the
    current channel or pans the image.
    Set panMode to 1 for panning; 0 for LUT fiddles.
------------------------------------------------------------------------*/
{
    register int i;
    int xc, yc;
    int beg, fin;

    if (panMode) {                                /* Do the pan option. */
      for (i = 0; i < NGREY; i++) {       /* Set the scroll parameters. */
        upleft_x[i] = x;
        upleft_y[i] = y;
      }
      imageRefresh();
    } else {                               /* Do the LUT fiddle option. */
      y = height - y;                   /* Reverse direction of y-axis. */
      xc = ((NColour - 1) * x) / width;                    /* 2 * Bias. */
      yc = ((NColour - 1) * y) / height;                   /* Contrast. */
      beg = xc - yc;                    /* -1 <= Threshold value <= +1. */
      fin = xc + yc;                     /* 0 <= Saturation value <= 2. */

/** Old method...
#define BIGINT 65535
      xc = (BIGINT * x) / width;
      yc = (BIGINT * y) / height;
      beg = (NColour * min(max((xc-yc)/2,0),BIGINT)) / BIGINT;
      fin = (NColour * min(max((xc+yc)/2,0),BIGINT)) / BIGINT;
#undef BIGINT
** Old method...**/

      (void)lutRamp(beg, fin);
    }
}
