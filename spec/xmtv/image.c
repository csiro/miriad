/*
 *	<image.c> - Image handling routines.
 *
 */

#include "xmtv.h"

#include <math.h>

/* Private variables. */

static int TvStatus[NGREY+NGRAPH];      /* TV Image + Graphics status.  */
static float imzero = 0.0;
static float imscale = 1.0;

/* Source code. */

/************************************************************************/
int setScale(izero1, izero2, iscale1, iscale2)
int izero1, izero2, iscale1, iscale2;
{
    imzero = (float)izero2 * pow(10.0, (double)izero1);
    imscale = (float)iscale2 * pow(10.0, (double)iscale1);
    return(0);
}

/************************************************************************/
float pixelValue(cx, cy)
int cx, cy;
/*
    Return the pixel value at a specified position.
    Note that input positions are in User units!
------------------------------------------------------------------------*/
{
    float pixval = 0.0;

    if (TvStatus[cur_chan-1] != 0) {
      cx = Memory_x(cx);
      cy = Memory_y(cy);

      if ((cx >= 0) && (cx < Screen_Width) &&
          (cy >= 0) && (cy < Screen_Height)) {
        pixval = (float)pix2int[XGetPixel(plane[cur_chan-1], cx, cy)];
        if (imscale != 0.0) pixval = imzero + (pixval / imscale);
      }
    }
    return(pixval);
}

/************************************************************************/
int initImage(w, visualList, screenDepth)
Widget w;
XVisualInfo visualList[];
int screenDepth;
/*
    Allocate some image handling arrays.
------------------------------------------------------------------------*/
{
    register int i;
    register int j;
    int size, offset;
    static Boolean initialized = False;

    if (initialized == True) return(0);
    initialized = True;
    offset = 0;

    size = Screen_Width * Screen_Height * ((screenDepth + 1) / 8);
    for (i = 0; i < NGREY; i++) {
      plane_data[i] = (unsigned char *)XtMalloc(sizeof(unsigned char) * size);
      if (plane_data[i] == (unsigned char *)NULL) {
        (void)fprintf(stderr, "Trouble allocating storage for plane %d\n", i+1);
        return(-1);
      }

      plane[i] = XCreateImage(XtDisplay(w), visualList[0].visual,
        screenDepth, ZPixmap, offset, (char *)plane_data[i],
        Screen_Width, Screen_Height, 8, 0);
      if (plane[i] == (XImage *)NULL) {
        (void)fprintf(stderr, "Trouble creating XImage for plane %d\n", i+1);
        return(-1);
      }
    }

    graph_data = (unsigned char *)XtMalloc(sizeof(unsigned char) * size);
    if (graph_data == (unsigned char *)NULL) {
      (void)fprintf(stderr, "Trouble allocating the graphics plane storage.\n");
      return(-1);
    }

    graph = XCreateImage(XtDisplay(w), visualList[0].visual,
      screenDepth, ZPixmap, offset, (char *)graph_data,
      Screen_Width, Screen_Height, 8, 0);
    if (graph == (XImage *)NULL) {
      (void)fprintf(stderr, "Trouble creating XImage for graphics\n");
      return(-1);
    }

    size = Screen_Width * MAXZOOM * ((screenDepth + 1) / 8);
    line_data = (unsigned char *)XtMalloc(sizeof(unsigned char) * size);
    if (line_data == (unsigned char *)NULL) {
      (void)fprintf(stderr, "Trouble allocating temporary image storage.\n");
      return(-1);
    }

    line = XCreateImage(XtDisplay(w), visualList[0].visual,
      screenDepth, ZPixmap, offset, (char *)line_data,
      Screen_Width, MAXZOOM, 8, 0);
    if (line == (XImage *)NULL) {
      (void)fprintf(stderr, "Trouble creating temporary XImage storage.\n");
      return(-1);
    }

    return(0);
}

/************************************************************************/
void freeImage()
/*
    Release allocated image handling arrays.
------------------------------------------------------------------------*/
{
    register int i;

    for (i = 0; i < NGREY; i++) {
      if (plane_data[i] != (unsigned char *)NULL)
        XtFree((char *)plane_data[i]);
      plane_data[i] = (unsigned char *)NULL;
      XDestroyImage(plane[i]);
    }

    if (graph_data != (unsigned char *)NULL)
      XtFree((char *)graph_data);
    graph_data = (unsigned char *)NULL;
    XDestroyImage(graph);

    if (line_data != (unsigned char *)NULL)
      XtFree((char *)line_data);
    line_data = (unsigned char *)NULL;
    XDestroyImage(line);
}

/************************************************************************/
int imwrt()
/*
    Writes an image plane or a graphics plane to the internal arrays and
    then displays the array onto the screen.  TvStatus is modified to
    record when data is displayed.
------------------------------------------------------------------------*/
{
    unsigned char gph_mask;
    register int i, npix;
    int xs, ys, iangl, channel, j, vv;

    channel = xbuf.parms[2];
    if ((channel < 1) || (channel > NGRTOT)) {
      (void)fprintf(stderr, "Bad image write channel = %d\n", channel);
      return(-1);
    } else {                                   /* Channel number okay.  */
      TvStatus[channel-1] = 1; /* Flag that data written to this channel.  */
    }

    npix = xbuf.data_length;
    iangl = xbuf.parms[3];
    xs =  Memory_x(xbuf.parms[0]);
    ys =  Memory_y(xbuf.parms[1]);

    gph_mask = 0;
    if (channel == NGREY+1) gph_mask = 1;
    if (channel == NGREY+2) gph_mask = 2;
    if (channel == NGREY+3) gph_mask = 4;
    if (channel == NGREY+4) gph_mask = 8;
    j = channel - 1;

    switch (iangl % 4) {
      case 0:
        if (channel <= NGREY) {                      /* Writing Image.  */
          for (i = 0; i < npix; i++) {
            XPutPixel(plane[j], xs+i, ys, int2pix[xbuf.data[i]]);
          }
        } else {                 /* Otherwise, writing to graph plane.  */
          for (i = 0; i < npix; i++) {    /* Set all pixels in a line.  */
            vv = XGetPixel(graph, xs+i, ys);
            chg_s(vv, xbuf.data[i], gph_mask);
            XPutPixel(graph, xs+i, ys, vv);
          }
        }
        if ((channel == cur_chan) || (rwgraph & gph_mask))
          scrwrt(xs, ys, xs + npix, ys);
        break;
      case 1:
        if (channel <= NGREY) {
          for (i = 0; i < npix; i++) {
            XPutPixel(plane[j], xs, ys-i, int2pix[xbuf.data[i]]);
          }
        } else {
          for (i = 0; i < npix; i++) {
            vv = XGetPixel(graph, xs, ys-i);
            chg_s(vv, xbuf.data[i], gph_mask);
            XPutPixel(graph, xs, ys-i, vv);
          }
        }
        if ((channel == cur_chan) || (rwgraph & gph_mask))
          scrwrt(xs, ys - npix, xs, ys);
        break;
      case 2:
        if (channel <= NGREY) {
          for (i = 0; i < npix; i++) {
            XPutPixel(plane[j], xs-i, ys, int2pix[xbuf.data[i]]);
          }
        } else {
          for (i = 0; i < npix; i++) {
            vv = XGetPixel(graph, xs-i, ys);
            chg_s(vv, xbuf.data[i], gph_mask);
            XPutPixel(graph, xs-i, ys, vv);
          }
        }
        if ((channel == cur_chan) || (rwgraph & gph_mask))
          scrwrt(xs - npix, ys, xs, ys);
        break;
      case 3:
        if (channel <= NGREY) {
          for (i = 0; i < npix; i++) {
            XPutPixel(plane[j], xs, ys+i, int2pix[xbuf.data[i]]);
          }
        } else {
          for (i = 0; i < npix; i++) {
            vv = XGetPixel(graph, xs, ys+i);
            chg_s(vv, xbuf.data[i], gph_mask);
            XPutPixel(graph, xs, ys+i, vv);
          }
        }
        if ((channel == cur_chan) || (rwgraph & gph_mask))
          scrwrt(xs, ys, xs, ys + npix);
        break;
      default:
        break;
    }

   return(0);
}

/************************************************************************/
int imrd(nwpix)
short int *nwpix;
/*
    This subroutine returns an image line to the client.
------------------------------------------------------------------------*/
{
    unsigned char gph_mask;
    int xs, ys, iangl, channel, j;
    unsigned short int jj;
    register int i, npix;

    xs = Memory_x(xbuf.parms[0]);
    ys = Memory_y(xbuf.parms[1]);
    channel = xbuf.parms[2];
    iangl = xbuf.parms[3];
    jj = *((unsigned short int *)xbuf.data); /* Special I*2 word in buffer. */
    npix = ntohs(jj);
    *nwpix = npix;

    if ((channel < 1) || (channel > NGRTOT)) {
      (void)fprintf(stderr, "Bad imrd channel = %d\n", channel);
      return(-1);
    }

    gph_mask = 0;
    if (channel == NGREY+1) gph_mask = 1;
    if (channel == NGREY+2) gph_mask = 2;
    if (channel == NGREY+3) gph_mask = 4;
    if (channel == NGREY+4) gph_mask = 8;
    j = channel - 1;

    switch (iangl % 4) {
      case 0:
        if (channel <= NGREY) {
          for (i = 0; i < npix; i++) {
            ybuf.data[i] = pix2int[XGetPixel(plane[j], xs+i, ys)];
          }
        } else {
          for (i = 0; i < npix; i++) {
            ybuf.data[i] = chg_g(XGetPixel(graph, xs+i, ys), gph_mask);
          }
        }
        break;
      case 1:
        if (channel <= NGREY) {
          for (i = 0; i < npix; i++) {
            ybuf.data[i] = pix2int[XGetPixel(plane[j], xs, ys-i)];
          }
        } else {
          for (i = 0; i < npix; i++) {
            ybuf.data[i] = chg_g(XGetPixel(graph, xs, ys-i), gph_mask);
          }
        }
        break;
      case 2:
        if (channel <= NGREY) {
          for (i = 0; i < npix; i++) {
            ybuf.data[i] = pix2int[XGetPixel(plane[j], xs-i, ys)];
          }
        } else {
          for (i = 0; i < npix; i++) {
            ybuf.data[i] = chg_g(XGetPixel(graph, xs-i, ys), gph_mask);
          }
        }
        break;
      case 3:
        if (channel <= NGREY) {
          for (i = 0; i < npix; i++) {
            ybuf.data[i] = pix2int[XGetPixel(plane[j], xs, ys+i)];
          }
        } else {
          for (i = 0; i < npix; i++) {
            ybuf.data[i] = chg_g(XGetPixel(graph, xs, ys+i), gph_mask);
          }
        }
        break;
      default:
        break;
    }

    return(0);
}

/************************************************************************/
void resizePorthole(w, width, height)
Widget w;
Dimension width, height;
/*
    Checks the Porthole size and position.
------------------------------------------------------------------------*/
{
    Arg args[10];
    Cardinal i;
    Dimension pw2, ph2;

                                        /*  Set widths.                 */
    PortW = min(width, (Dimension)Screen_Width);
    PortH = min(height, (Dimension)Screen_Height);
    pw2 = PortW / 2;
    ph2 = PortH / 2;
                                        /*  Set center coords.          */
    sc_centre_x = max(sc_centre_x, (int)(pw2 - 1));
    sc_centre_y = max(sc_centre_y, (int)(ph2 - 1));
    sc_centre_x = min(sc_centre_x, (int)(Screen_Width - pw2 - 1));
    sc_centre_y = min(sc_centre_y, (int)(Screen_Height - ph2 - 1));

    PortX = sc_centre_x - pw2 + 1;
    PortY = sc_centre_y - ph2 + 1;
    if (AppDebug) (void)fprintf(stderr,
      "Porthole resize: W H X Y %d %d %d %d\n", PortW, PortH, PortX, PortY);

                                        /*  Resize and bring into view. */
    if ((width != PortW) || (height != PortH)) {
      i = 0;
      XtSetArg(args[i], XtNwidth,  (XtArgVal)PortW); i++;
      XtSetArg(args[i], XtNheight, (XtArgVal)PortH); i++;
      XtSetValues(w, args, i);
      if (AppDebug) (void)fprintf(stderr,
        "Forced porthole resize: W H %d %d\n", PortW, PortH);
    }
}

/************************************************************************/
int windo_status(ph)
Widget ph;
/*
    Returns current window corners.
------------------------------------------------------------------------*/
{
    int itx, ity, icx, icy;
    Dimension width, height;

                                        /*  Force screen size.          */
    if ((xbuf.parms[0] > 0) && (xbuf.parms[1] > 0) &&
        (xbuf.parms[2] > 0) && (xbuf.parms[3] > 0)) {
      xbuf.parms[2] = min(xbuf.parms[2], Screen_Width);
      xbuf.parms[3] = min(xbuf.parms[3], Screen_Height);
      PortX = icx = Memory_x(xbuf.parms[0]);
      itx = Memory_x(xbuf.parms[2]);
      PortY = icy = Memory_y(xbuf.parms[3]);
      ity = Memory_y(xbuf.parms[1]);
      width = itx - icx + 1;
      height = ity - icy + 1;
      sc_centre_x = (icx + itx - 1) / 2;
      sc_centre_y = (icy + ity - 1) / 2;
      resizePorthole(ph, width, height);
    }

    ybuf.data[0] = User_x(PortX);
    ybuf.data[3] = User_y(PortY);
    ybuf.data[2] = User_x(PortX + PortW - 1);
    ybuf.data[1] = User_y(PortY + PortH - 1);

    return(0);
}

/************************************************************************/
int Interogate(nparms)
short int *nparms;
/*
    This returns the critical parameters of the TV to the client.
------------------------------------------------------------------------*/
{
    *nparms = 29;
    ybuf.data[0]  =  NGREY;
    ybuf.data[1]  =  NGRAPH;
    ybuf.data[2]  = -1;
    ybuf.data[3]  =  Screen_Width;
    ybuf.data[4]  =  Screen_Height;
    ybuf.data[5]  =  NColour - 1;
    ybuf.data[6]  = (NINTENS) - 1;
    ybuf.data[7]  = (NINTENS) - 1;
    ybuf.data[8]  = (NINTENS) - 1;
    ybuf.data[9]  =  1;
    ybuf.data[10] =  1;
    ybuf.data[11] =  1 - (MAXZOOM);
    ybuf.data[12] = -1;
    ybuf.data[13] = -1;
    ybuf.data[14] =  0;
    ybuf.data[15] =  0;
    ybuf.data[16] =  3;
    ybuf.data[17] =  3;
    ybuf.data[18] = -1;
    ybuf.data[19] = -1;
    ybuf.data[20] = -1;
    ybuf.data[21] = -1;
    ybuf.data[22] = -1;
    ybuf.data[23] = -1;
    ybuf.data[24] = -1;
    ybuf.data[25] = -1;
    ybuf.data[26] = -1;
    ybuf.data[27] = -1;
    ybuf.data[28] = -1;

    return(0);
}

/************************************************************************/
int clearChannel(channel)
int channel;
/*
    clearChannel clears both graphics and image planes, if requested.
    Modified to clear more rapidly if only one graph plane is on.
    Inputs:
      channel   I    If Channel is 0 then all images and graphics cleared
                     If Channel <= NGREY then one images is cleared
                     If Channel >  NGREY then one graph is cleared
                     If Channel =  NGRTOT+1 then clear all graphs only
------------------------------------------------------------------------*/
{
    unsigned char gph_mask;
    unsigned char *t1, *t2;
    int imax;
    unsigned long int izero;
    register int i, j, k;
    Boolean OnlyWritten;

    if ((channel < 0) || (channel > NGRTOT+1))    /* If invalid, exit.  */
      return(-1);

    /* If only one graph and already cleared, then return.  However, if */
    /* clearing all (channel=0), then clear for safty sake (no return). */
    if ((channel > 0) && (channel <= NGRTOT) && (TvStatus[channel-1] == 0))
      return(0);                        /* already done, return success */

    if (channel == 0) {                       /* Clearing all channels. */
      OnlyWritten = True;
      for (j=0; j < NGRTOT; j++)
        TvStatus[j] = 0;                     /* Set them as un-written. */
    } else {                    /* Clearing only one or some channels.  */
      if (channel == NGRTOT+1) {         /* Clearing all graph planes.  */
        OnlyWritten = True;
        for (j = NGREY; j < NGRTOT; j++) 
          TvStatus[j] = 0;               /* Set graph planes as clear.  */
      } else {                       /* Clearing only one graph plane.  */
        TvStatus[channel-1] = 0;                /* Set plane as clear.  */
        OnlyWritten = True;           /* Assume only this one written.  */ 
        for (j = NGREY; j < NGRAPH+NGREY; j++) {
                            /* Check for data in other graphic planes.  */
          if ((j != channel-1) && (TvStatus[j] != 0)) OnlyWritten = False;
        }
      } /* End if all graph planes off.  */
    } /* End if all channels off.  */

    imax = Screen_Width * Screen_Height * ((depth+1)/8);
    izero = int2pix[0];

    for (j = 0; j < NGREY; j++) {                   /* Clear an Image.  */
      if ((channel == 0) || (channel == j+1)) {
        if (depth == 8) {         /* If special depth, do quick clear.  */
          t1 = plane_data[j];
          for (i = 0; i < imax; i++)
            *t1++ = izero;                       /* Set pixel to zero.  */
        } else {                        /* Otherwise, do a slow clear.  */
          for (i = 0; i < Screen_Width; i++) { 
            for (k = 0; k < Screen_Height; k++)
              XPutPixel(plane[j], i, k, izero);
          }
        }
      }
    }

    if ((channel == 0) || (OnlyWritten == True)) { /* Clear all graphs. */
      t2 = graph_data;                          /* Clear graphics data. */
      for (i = 0; i < imax; i++)
        *t2++ = 0;
    } else {                       /* Otherwise, clear only one graph.  */
      if (channel > NGREY) {               
        gph_mask = (2 << NGRAPH) - 1;     /* Set mask to all channels.  */
	/* Now mask out desired channel.  */
        if (channel == NGREY + 1) gph_mask ^= 1;
        if (channel == NGREY + 2) gph_mask ^= 2;
        if (channel == NGREY + 3) gph_mask ^= 4;
        if (channel == NGREY + 4) gph_mask ^= 8;
        t2 = graph_data;     /* Point to beginning of the graph plane.  */
        for (i = 0; i < imax; i++) {               /* Zero this graph.  */
          *t2 &= gph_mask;
          t2++;
        }
      } /* End if clear only one graph.  */
    } /* End if clearing all.  */

   imageRefresh();
   return(0);
}

/************************************************************************/
int zoom(chan, mag, scrlx, scrly)
int chan, mag, scrlx, scrly;
/*
    Sets the zoom registers.
------------------------------------------------------------------------*/
{
    register int i;
    Boolean upState, downState;

    if (mag < 1)            mag = 1;
    else if (mag > MAXZOOM) mag = MAXZOOM;

    upState = (mag < MAXZOOM) ? True : False;
    downState = (mag > 1) ? True : False;

    if ((chan < 0) || (chan > NGRTOT)) {
      (void)fprintf(stderr, "Illegal zoom channel %d\n", chan);
      return(-1);
    } else if (chan > NGREY) {
      /* NULL */ ;
    } else if (chan == 0) {
      upleft_mag = mag;
      for (i = 0; i < NGREY; i++) {
        upleft_x[i] = scrlx * upleft_mag;
        upleft_y[i] = scrly * upleft_mag;
      }
      movePanner(upleft_mag, upleft_x[0], upleft_y[0]);
      imageRefresh();
      zoomEnable(upState, downState);
    } else {
      upleft_mag = mag;
      upleft_x[chan-1] = scrlx * upleft_mag;
      upleft_y[chan-1] = scrly * upleft_mag;
      if (chan == cur_chan) {
        movePanner(upleft_mag, upleft_x[chan-1], upleft_y[chan-1]);
        imageRefresh();
      }
      zoomEnable(upState, downState);
    }
    return(0);
}

/************************************************************************/
int read_zoom(nparms)
short int *nparms;
/*
    Reads the zoom/scroll registers.
------------------------------------------------------------------------*/
{
    *nparms = 4;
    upleft_mag = max(1, upleft_mag);
    ybuf.data[0] = cur_chan;
    ybuf.data[1] = upleft_mag;
    ybuf.data[2] = upleft_x[cur_chan-1] / upleft_mag;
    ybuf.data[3] = upleft_y[cur_chan-1] / upleft_mag;

    return(0);
}
