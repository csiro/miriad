/*
 *	<screen.c> - screen re-write routines.
 *	19dec95 jm Force x/ymin positions in scrwrt() to be in
 *		   the range of 0 - Screen_WH.
 */

#include "xmtv.h"

/* Private variables. */

static GC ImageGC;  /* Graphics context for drawing images & graphics.  */

/* Source code. */

/************************************************************************/
void initCanvas(w)
Widget w;
/*
    Sets the Gravity bit and creates a GC.
------------------------------------------------------------------------*/
{
    unsigned long valueMask;
    Display *dpy = XtDisplay(w);
    Window window = XtWindow(w);
    XSetWindowAttributes WinAtt;

    valueMask = CWBitGravity;       /* Change the canvas' bit gravity.  */
    WinAtt.bit_gravity = CenterGravity;
    XChangeWindowAttributes(dpy, window, valueMask, &WinAtt);

                                         /* Create a graphics context.  */
    ImageGC = XCreateGC(dpy, window, 0, (XGCValues *)NULL);
           /* Specify the foreground or else it may be white on white.  */
    XSetForeground(dpy, ImageGC, BlackPixel(dpy, DefaultScreen(dpy)));
    XSetFunction(dpy, ImageGC, GXcopy);
    XSetPlaneMask(dpy, ImageGC, AllPlanes);

    return;
}

/************************************************************************/
void freeCanvas()
/*
    Releases the GC.
------------------------------------------------------------------------*/
{
    XFreeGC(XtDisplay(canvas), ImageGC);
}

/************************************************************************/
static void scrdoit(xs, ys, xe, ye, xmin, ymin, xmax, ymax, xoff, yoff)
int xs, ys, xe, ye, xmin, ymin, xmax, ymax, xoff, yoff;
/*
    Draws from memory image to the screen taking account of zoom,
    scroll and window offsets etc.  Updates that rectangle enclosed
    by xs, ys, xe, ye.  Dimensions are screen units BEFORE zoom for xs,
    ys, xe, ye and AFTER zoom for the others.  This means that the left
    column and upper row and right column and lower row may not be
    replicated upleft_mag times in zoom.
------------------------------------------------------------------------*/
{
    register unsigned char *pi, *pl, *gi;
    register int j, jj;
    int i, x, y, offset, choff;
    int gphv, imv, yy, nx, ny;
    int mem_full[4096];
    unsigned long pv;
    Display *dpy = XtDisplay(canvas);
    Window win = XtWindow(canvas);

    upleft_mag = max(1, upleft_mag);
    if (AppDebug) {
      (void)fprintf(stderr, "xs,ys,xe,ye,mag %d %d %d %d %d\n",
         xs, ys, xe, ye, upleft_mag);
      (void)fprintf(stderr, "xmin,xmax,ymin,ymax %d %d %d %d\n",
         xmin, xmax, ymin, ymax);
      (void)fprintf(stderr, "xoff, yoff %d %d\n", xoff, yoff);
    }

    xs = max(xs, xmin/upleft_mag);
    ys = max(ys, ymin/upleft_mag);
    xe = min(xe, xmax/upleft_mag);
    ye = min(ye, ymax/upleft_mag);
    choff = max(0, cur_chan - 1);

    if ((xe >= xs) && (ye >= ys)) {    /* Find the number of x-points.  */
      nx = upleft_mag * (xe - xs + 1);
      offset = (upleft_mag * xs) - xmin + xoff;
      if (offset < 0) {
        nx += offset;
        offset = 0;
      }
      nx = min(nx, xmax + xoff - offset + 1);

      if (rwgraph <= 0) {               /* No graphics will be faster.  */
        if (upleft_mag <= 1) {                   /* No zoom is easier.  */
          XPutImage(dpy, win, ImageGC, plane[choff], xs, ys,
               xs - xmin + xoff, ys - ymin + yoff, nx, ye - ys + 1);
        } else {                             /* Zoomed by replication.  */
          for (i = ys; i <= ye; i++) {
            if (depth == 8) {           /* Fast when 8-bit characters.  */
              for (x=0; x < upleft_mag; x++) {          /* Zoom a row.  */
                offset = (upleft_mag * xs) - xmin + x + xoff;
                pl = line_data + offset;
                pi = plane_data[choff] + (Screen_Width * i) + xs;
                j = xe - xs + 1;
                if (offset < 0) {
                  pl += upleft_mag;
                  pi += 1;
                  j--;
                }
                while (j--) {
                  *pl = *pi++;
                  pl += upleft_mag;
                }
              }
                                                 /* Replicate the row.  */
              offset = max((upleft_mag * xs) - xmin + xoff, 0);
              yy = (upleft_mag * i) - ymin + yoff;
              ny = upleft_mag;
              if (yy < 0) {
                ny += yy;
                yy = 0;
              }
              ny = min(ymax + yoff + 1 - yy, ny);
              for (y = 1; y < ny; y++) {
                (void)memcpy((char *)(line_data + offset + (Screen_Width * y)),
                             (char *)(line_data + offset), (size_t)nx);
              }
                                               /* Move to the display.  */
              XPutImage(dpy, win, ImageGC, line, offset, 0, offset,
                yy, nx, ny);
            } else {                                     /* depth != 8. */
      /* Displays not 8-bits deep are handled with generic Xlib calls.  */
              offset = (upleft_mag * xs) - xmin + xoff; /* Zoom a row.  */
              for (j = xs; j <= xe; j++) {
                pv = XGetPixel(plane[choff], j, i);
                for (jj = 0; jj < upleft_mag; jj++) {
                  if (offset > 0) XPutPixel(line, offset, 0, pv);
                  offset++;
                }
              }
                                                 /* Replicate the row.  */
              offset = (upleft_mag * xs) - xmin + xoff;
              yy = (upleft_mag * i) - ymin + yoff;
              ny = upleft_mag;
              if (yy < 0) {
                ny += yy;
                yy = 0;
              }
              ny = min(ymax + yoff + 1 - yy, ny);
              for (y = 1; y < ny; y++) {
                XPutImage(dpy, win, ImageGC, line, offset, 0, offset,
                  yy + y, nx, 1);
              }
            }
          }
        }
      } else {                          /* Otherwise, graphics are on.  */
        for (i = ys; i <= ye; i++) {
          if (depth == 8) {             /* Fast when 8-bit characters.  */
                                   /* Get the graphics and image line.  */
            pi = plane_data[choff] + (Screen_Width * i) + xs;
            gi = graph_data + (Screen_Width * i) + xs;
            jj = NColour - 1;
            for (j = xs; j <= xe; j++) {
              gphv = rwgraph & *gi++;
              imv = *pi++;
              mem_full[j] = (gphv > 0) ? int2pix[jj + gphv] : imv;
            }

            for (x = 0; x < upleft_mag; x++) {         /* Zoom a row.  */
              offset = (upleft_mag * xs) - xmin + x + xoff;
              pl = line_data + offset;
              yy = xs;
              if (offset < 0) {
                pl += upleft_mag;
                yy++;
              }
              for (j = yy; j <= xe; j++) {
                *pl = mem_full[j];
                pl += upleft_mag;
              }
            }
                                                 /* Replicate the row.  */
            offset = max((upleft_mag * xs) - xmin + xoff, 0);
            yy = (upleft_mag * i) - ymin + yoff;
            ny = upleft_mag;
            if (yy < 0) {
              ny += yy;
              yy = 0;
            }
            ny = min(ymax + yoff + 1 - yy, ny);
            for (y = 1; y < ny; y++) {
              (void)memcpy((char *)(line_data + offset + (Screen_Width * y)),
                           (char *)(line_data + offset), (size_t)nx);
            }
                                          /* move to the display        */
            XPutImage(dpy, win, ImageGC, line, offset, 0, offset,
              yy, nx, ny);
          } else {                                       /* depth != 8. */
      /* Displays not 8-bits deep are handled with generic Xlib calls.  */
                                   /* Get the graphics and image line.  */
            offset = (upleft_mag * xs) - xmin + xoff;
            for (j = xs; j <= xe; j++) {
              gphv = rwgraph & XGetPixel(graph, j, i);
              pv = (gphv > 0) ? int2pix[jj + gphv] :
                                XGetPixel(plane[choff], j, i);
              for (jj = 0; jj < upleft_mag; jj++) { /* Zoom the pixel.  */
                if (offset > 0) XPutPixel(line, offset, 0, pv);
                offset++;
              }
            }
                                                 /* Replicate the row.  */
            offset = (upleft_mag * xs) - xmin + xoff;
            yy = (upleft_mag * i) - ymin + yoff;
            ny = upleft_mag;
            if (yy < 0) {
              ny += yy;
              yy = 0;
            }
            ny = min(ymax + yoff + 1 - yy, ny);
            for (y = 1; y < ny; y++) {
              XPutImage(dpy, win, ImageGC, line, offset, 0, offset,
                yy + y, nx, 1);
            }
          }
        }
      }
    }
}

/************************************************************************/
void scrwrt(xs, ys, xe, ye)
int xs, ys, xe, ye;
/*
    Draws from memory image to the screen taking account of zoom,
    scroll and window offsets etc. Updates that rectangle enclosed
    by xs, ys, xe, ye.  Dimensions are in screen units.
------------------------------------------------------------------------*/
{
    int xmin, xmax, ymin, ymax, xext, yext, amin, amax, xoff, yoff;
    int choff;

    choff = max(0, cur_chan - 1);

    if (xs < 0) xs = 0;
    if (ys < 0) ys = 0;
    if (xe > Screen_Width - 1) xe = Screen_Width - 1;
    if (ye > Screen_Height - 1) ye = Screen_Height - 1;

                                              /*  Upper left quadrant.  */
    xoff = Screen_Width * upleft_mag;
    xmin = upleft_x[choff] + PortX;
    while (xmin < 0) xmin += xoff;
    while (xmin >= xoff) xmin -= xoff;
    xmax = xmin + PortW - 1;

    xext = xmax - ((Screen_Width - 1) * upleft_mag);
    if (xext > 0) xmax = (Screen_Width - 1) * upleft_mag;

    yoff = Screen_Height * upleft_mag;
    ymin = upleft_y[choff] + PortY;
    while (ymin < 0) ymin += yoff;
    while (ymin >= yoff) ymin -= yoff;
    ymax = ymin + PortH - 1;

    yext = ymax - ((Screen_Height - 1) * upleft_mag);
    if (yext > 0) ymax = (Screen_Height - 1) * upleft_mag;

    xoff = yoff = 0;
    scrdoit(xs, ys, xe, ye, xmin, ymin, xmax, ymax, xoff, yoff);

                                            /*  Upper right quadrant.  */
    if (xext > 0) {
      amin = 0;
      amax = xext - upleft_mag;
      xoff = xmax + upleft_mag - xmin;
      yoff = 0;
      scrdoit(xs, ys, xe, ye, amin, ymin, amax, ymax, xoff, yoff);
    }

                                             /*  Lower left quadrant.  */
    if (yext > 0) {
      amin = 0;
      amax = yext - upleft_mag;
      xoff = 0;
      yoff = ymax + upleft_mag - ymin;
      scrdoit(xs, ys, xe, ye, xmin, amin, xmax, amax, xoff, yoff);
    }

                                            /*  Lower right quadrant.  */
    if ((xext > 0) && (yext > 0)) {
      xoff = xmax + upleft_mag - xmin;
      yoff = ymax + upleft_mag - ymin;
      amin = 0;
      amax = yext - upleft_mag;
      xmin = 0;
      xmax = xext - upleft_mag;
      scrdoit(xs, ys, xe, ye, xmin, amin, xmax, amax, xoff, yoff);
    }
}

/************************************************************************/
void imageRefresh()
/*
    Refresh the entire image display.
------------------------------------------------------------------------*/
{
    if (AppDebug)
      (void)fprintf(stderr, "imageRefresh() called\n");

    scrwrt(0, 0, Screen_Width - 1, Screen_Height - 1);
}
