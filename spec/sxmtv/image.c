/*--------------------------------------------------------------------*/
/*  06oct92 jm  Modified izero from int to unsigned long int.         */
/*--------------------------------------------------------------------*/

#include "sxmtv.h"

int imwrt() 
/* IMWRT Writes an image plane or a graphics plane to the internal sxmtv*/
/* arrays, then displays the array on the screen.                     */
{
   register int i, npix;
   int xs, ys, iangl, channel, j, k;
   unsigned long int vv;
   unsigned char goffset,ioffset;
/*--------------------------------------------------------------------*/
   channel = xbuf.parms[2];
   if ((channel < 1) || (channel > NGRTOT)) {
      fprintf (stderr, "Bad imwrt channel = %d\n", channel);
      return (-1);
      }
   else {                               /* Else channel OK            */
      TvStatus[channel-1] = 1;          /* record Data put on TV      */
      }                                 /* End if channel number bad  */

   npix = xbuf.data_length;
   iangl = xbuf.parms[3];
   xs =  Memory_x (xbuf.parms[0]);
   ys =  Memory_y (xbuf.parms[1]);

   goffset = graphics_offset;
   ioffset = image_offset;

   gph_mask = 0;
   if (channel == NGREY+1) gph_mask = 1;
   if (channel == NGREY+2) gph_mask = 2;
   if (channel == NGREY+3) gph_mask = 4;
   if (channel == NGREY+4) gph_mask = 8;
   j = channel - 1;

   if (iangl == 0) {
      if (channel <= NGREY)             /* If writing Image            */
         for (i = 0; i < npix; i++) {
            XPutPixel (plane[j], xs+i, ys, int2pix[xbuf.data[i]]);
            }
      else {                            /* Else writing graph          */
         for (i = 0; i < npix; i++) {   /* For all pixels in line      */
            vv = XGetPixel(graph, xs+i, ys);
            chg_s (vv, xbuf.data[i], gph_mask);
            XPutPixel (graph, xs+i, ys, vv);
   	    }                           /* End for all graph pixels    */
         }                              /* End if writing image plane  */
      if ((channel == cur_chan) || (rwgraph & gph_mask)) /* If chan on */
         scrwrt (xs, ys, xs + npix, ys); /* refresh the display        */
      }
   else if (iangl == 1) {
      if (channel <= NGREY)
         for (i = 0; i < npix; i++) {
            XPutPixel (plane[j], xs, ys-i, int2pix[xbuf.data[i]]);
            }
      else {
         for (i = 0; i < npix; i++) {
            vv = XGetPixel(graph, xs, ys-i);
            chg_s (vv, xbuf.data[i], gph_mask);
            XPutPixel (graph, xs, ys-i, vv);
            }
         }
      if ((channel == cur_chan) || (rwgraph & gph_mask))
         scrwrt (xs, ys - npix, xs, ys);
      }
   else if (iangl == 2) {
      if (channel <= NGREY)
         for (i = 0; i < npix; i++) {
            XPutPixel (plane[j], xs-i, ys, int2pix[xbuf.data[i]]);
            }
      else {
         for (i = 0; i < npix; i++) {
            vv = XGetPixel(graph, xs-i, ys);
            chg_s (vv, xbuf.data[i], gph_mask);
            XPutPixel (graph, xs-i, ys, vv);
            }
         }
      if ((channel == cur_chan) || (rwgraph & gph_mask))
        scrwrt (xs - npix, ys, xs, ys);
      }
   else if (iangl == 3) {
      if (channel <= NGREY)
         for (i = 0; i < npix; i++) {
            XPutPixel (plane[j], xs, ys+i, int2pix[xbuf.data[i]]);
            }
      else {
         for (i = 0; i < npix; i++) {
            vv = XGetPixel(graph, xs, ys+i);
            chg_s (vv, xbuf.data[i], gph_mask);
            XPutPixel (graph, xs, ys+i, vv);
            }
         }
      if ((channel == cur_chan) || (rwgraph & gph_mask))
         scrwrt (xs, ys, xs, ys + npix);
      }

    return (0);
} /* end imwrt */

int imrd (nwpix)
/*--------------------------------------------------------------------*/
/* This subroutine returns an image line to the client.               */
/*--------------------------------------------------------------------*/
short int *nwpix;
/*--------------------------------------------------------------------*/
{
   int xs, ys, iangl, channel, j, vv;
   short int jj;
   register int i, npix;
/*--------------------------------------------------------------------*/
   xs = Memory_x (xbuf.parms[0]);
   ys = Memory_y (xbuf.parms[1]);
   channel = xbuf.parms[2];
   iangl = xbuf.parms[3];
                                        /* special I*2 word in buffer */
   jj = *((short int *)xbuf.data);
   npix =  ntohs (jj);
   *nwpix = npix;

   if ((channel < 1) || (channel > NGRTOT)) {
      fprintf (stderr, "Bad imrd channel = %d\n", channel);
      return (-1);
      }

   gph_mask = 0;
   if (channel == NGREY+1) gph_mask = 1;
   if (channel == NGREY+2) gph_mask = 2;
   if (channel == NGREY+3) gph_mask = 4;
   if (channel == NGREY+4) gph_mask = 8;
   j = channel - 1;

   if (iangl == 0) {
      if (channel <= NGREY)
         for (i = 0; i < npix; i++) {
            ybuf.data[i] = pix2int[XGetPixel (plane[j], xs+i, ys)];
            }
      else
         for (i = 0; i < npix; i++) {
            ybuf.data[i] = chg_g (XGetPixel (graph, xs+i, ys), gph_mask);
            }
      }
   else if (iangl == 1) {
      if (channel <= NGREY)
         for (i = 0; i < npix; i++) {
            ybuf.data[i] = pix2int[XGetPixel (plane[j], xs, ys-i)];
            }
      else
         for (i = 0; i < npix; i++) {
            ybuf.data[i] = chg_g (XGetPixel (graph, xs, ys-i), gph_mask);
            }
      }
   if (iangl == 2) {
      if (channel <= NGREY)
         for (i = 0; i < npix; i++) {
            ybuf.data[i] = pix2int[XGetPixel (plane[j], xs-i, ys)];
            }
      else
         for (i = 0; i < npix; i++) {
            ybuf.data[i] = chg_g (XGetPixel (graph, xs-i, ys), gph_mask);
            }
      }
   else if (iangl == 3) {
      if (channel <= NGREY)
         for (i = 0; i < npix; i++) {
            ybuf.data[i] = pix2int[XGetPixel (plane[j], xs, ys+i)];
            }
      else
         for (i = 0; i < npix; i++) {
            ybuf.data[i] = chg_g (XGetPixel (graph, xs, ys+i), gph_mask);
            }
      }

   return (0);
}

void resize_canvas (width, height, ix0, iy0)
/*--------------------------------------------------------------------*/
/* checks canvas size and position                                    */
/*--------------------------------------------------------------------*/
   int width, height, ix0, iy0;
/*--------------------------------------------------------------------*/
{
   int icx, icy, itx, ity;

   icx = ix0;
   icy = iy0;
   itx = width;
   ity = height;
                                        /* set widths                 */
   sc_width = min (itx, Screen_Width);
   sc_height = min (ity, Screen_Height);
   sc_width2 = sc_width / 2;
   sc_height2 = sc_height / 2;
                                        /* set center coords          */
   sc_centre_x = max (sc_centre_x, sc_width2 - 1);
   sc_centre_y = max (sc_centre_y, sc_height2 - 1);
   sc_centre_x = min (sc_centre_x, Screen_Width - sc_width2 - 1);
   sc_centre_y = min (sc_centre_y, Screen_Height - sc_height2 - 1);
                                        /* check frame coords         */
   itx = sc_width;
   ity = sc_height;
   icx += bwid;
   icy += bwid;
   icx = max (icx, SCREEN_LEFT);
   icy = max (icy, SCREEN_TOP);
   icx = min (icx, twidth - SCREEN_RIGHT - itx) - bwid;
   icy = min (icy, theight - SCREEN_BOTTOM - ity) - bwid;
                                        /* resize, bring into view    */
   if (/*(ix0 != icx) || (iy0 != icy) ||*/ (width != itx) ||
      (height != ity)) {
      Cur_Xsize = itx;
      Cur_Ysize = ity;
      XResizeWindow (display, win, itx, ity);
      if (SXMTVDebug) fprintf (stderr, 
         "force resize_canvas: W H %d %d\n", itx, ity);
/*    Cur_Xzero = icx;
      Cur_Yzero = icy; */
      }

} /* end resize_canvas */

void resize_pressed()
/*--------------------------------------------------------------------*/
/* Toggle between full screen and smaller window set by the window    */
/* manager.                                                           */
/*--------------------------------------------------------------------*/
{
   int itx, ity, icx, icy;
   static int big_screen = 0;  /* 1 if the screen is at the max size. */
   static int cur_xsize, cur_ysize;  /* Saved size of smaller window. */
   static int cur_xcorn, cur_ycorn; /* Lower-left position of window. */

   big_screen = !big_screen;
   if (big_screen) {
      cur_xcorn = Cur_Xzero + bwid;
      cur_ycorn = Cur_Yzero + bwid;
      cur_xsize = Cur_Xsize;
      cur_ysize = Cur_Ysize;
      itx = Screen_Width;
      ity = Screen_Height;
      icx = 0;
      icy = 0;
      }
   else {
      itx = cur_xsize;
      ity = cur_ysize;
      icx = cur_xcorn;
      icy = cur_ycorn;
      }
   icx = max (icx, SCREEN_LEFT);
   icy = max (icy, SCREEN_TOP);
   icx = min (icx, twidth - SCREEN_RIGHT - itx) - bwid;
   icy = min (icy, theight - SCREEN_BOTTOM - ity) - bwid;

   Cur_Xzero = icx;
   Cur_Yzero = icy;
   Cur_Xsize = itx;
   Cur_Ysize = ity;

/*   XResizeWindow (display, win, itx, ity);
   if (SXMTVDebug) fprintf (stderr, 
     "resize_pressed: W H, %d %d\n", itx, ity); */
   XMoveResizeWindow (display, win, icx, icy, itx, ity);
   if (SXMTVDebug) fprintf (stderr, 
     "resize_pressed: W H, X Y %d %d, %d %d\n", itx, ity, icx, icy);
}

windo_status()
/*--------------------------------------------------------------------*/
/*  Returns current window corners                                    */
/*--------------------------------------------------------------------*/
{
   int itx, ity, icx, icy;
/*--------------------------------------------------------------------*/
                                        /* force screen size          */
   if (xbuf.parms[0] > 0 && xbuf.parms[1] > 0
         && xbuf.parms[2] > 0 && xbuf.parms[3] > 0) {
      xbuf.parms[2] = min (xbuf.parms[2], Screen_Width);
      xbuf.parms[3] = min (xbuf.parms[3], Screen_Height);
      icx = Memory_x (xbuf.parms[0]);
      itx = Memory_x (xbuf.parms[2]);
      icy = Memory_y (xbuf.parms[3]);
      ity = Memory_y (xbuf.parms[1]);
      sc_width = itx - icx + 1;
      sc_height = ity - icy + 1;
      sc_centre_x = (icx + itx - 1) / 2;
      sc_centre_y = (icy + ity - 1) / 2;
      sc_width2 = sc_width / 2;
      sc_height2 = sc_height / 2;
      itx = sc_width;
      ity = sc_height;
      icx = Cur_Xzero + bwid;
      icy = Cur_Yzero + bwid;
      icx = max (icx, SCREEN_LEFT);
      icy = max (icy, SCREEN_TOP);
      icx = min (icx, twidth - SCREEN_RIGHT - itx) - bwid;
      icy = min (icy, theight - SCREEN_BOTTOM - ity) - bwid;

      Cur_Xzero = icx;
      Cur_Yzero = icy;
      Cur_Xsize = itx;
      Cur_Ysize = ity;
/*      XResizeWindow (display, win, itx, ity);
      if (SXMTVDebug) fprintf (stderr, 
         "windo_status: W H %d %d\n", itx, ity); */
      XMoveResizeWindow (display, win, icx, icy, itx, ity);
      if (SXMTVDebug) fprintf (stderr, 
         "windo_status: W H, X Y %d %d, %d %d\n",
         itx, ity, icx, icy);
      }
   ybuf.data[0] = coord_x (sc_centre_x - sc_width2 + 1);
   ybuf.data[3] = coord_y (sc_centre_y - sc_height2 + 1);
   ybuf.data[2] = coord_x (sc_centre_x + sc_width2);
   ybuf.data[1] = coord_y (sc_centre_y + sc_height2);

   return (0);
}

Interogate (nparms)
/*--------------------------------------------------------------------*/
/* This subroutine returns the critical parameters of the TV to the   */
/* client in the order of DTVC.INC                                    */
/*--------------------------------------------------------------------*/
short int *nparms;
/*--------------------------------------------------------------------*/
{

   *nparms = 29;
   ybuf.data[0] = NGREY;
   ybuf.data[1] = NGRAPH;
   ybuf.data[2] = -1;
   ybuf.data[3] = Screen_Width;
   ybuf.data[4] = Screen_Height;
   ybuf.data[5] = NColour - 1;
   ybuf.data[6] = (NINTENS) - 1;
   ybuf.data[7] = (NINTENS) - 1;
   ybuf.data[8] = (NINTENS) - 1;
   ybuf.data[9] = 1;
   ybuf.data[10] = 1;
   ybuf.data[11] = 1 - (MAXZOOM);
   ybuf.data[12] = -1;
   ybuf.data[13] = -1;
   ybuf.data[14] = 0;
   ybuf.data[15] = 0;
   ybuf.data[16] = 3;
   ybuf.data[17] = 3;
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

   return (0);
}

ClearChan()
/*----------------------------------------------------------------------*/
/* ClearChan clears both graphics and image planes, if requested        */
/* Inputs                                                               */
/*   channel   I    If Channel is 0 then all images and graphics cleared*/
/*                  If Channel <= NGREY then one images is cleared      */
/*                  If Channel >  NGREY then one graph is cleared       */
/*                  If Channel =  NGRTOT+1 then clear all graphs only   */
/* Modified to clear more rapidly if only one graph plane is on         */
{  Bool OnlyWritten;
   int channel, imax;
   unsigned long int izero;
   register int i, j, k;
   char *t1, *t2;

   channel = xbuf.parms[0];
                                        /* If channel invalid, exit     */
   if ((channel < 0) || (channel > NGRTOT+1))
      return (-1);

   /* If only one graph and already cleared */
   /* Note. if clearing all, channel=0, clear for safty sake (no return)*/
   if (channel > 0 && channel <= NGRTOT && TvStatus[channel-1] == 0) {
      return (0);                       /* already done, return success */
      }

   if (channel == 0) {                  /* if clearing all channels     */
      OnlyWritten = True;
      for (j=0; j < NGRTOT; j++)
         TvStatus[j] = 0;               /* tell all are un-written      */
      }
   else {                               /* else clearing only some      */
      if (channel == NGRTOT+1) {        /* if clearing all graphs       */
         OnlyWritten = True;
         for (j=NGREY; j < NGRTOT; j++) 
            TvStatus[j] = 0;            /* tell graphs is clear         */
      	 }
      else {                            /* clearing one graph           */
         TvStatus[channel-1] = 0;       /* record Data put on TV        */
         OnlyWritten = True;            /* Assume only this one written */ 
         for (j=NGREY; j < NGRAPH+NGREY; j++) {
                                        /* If data in other graphics    */
            if (j != channel-1 && TvStatus[j] != 0 ) OnlyWritten = False;
            }
         }                              /* End if all graphs off        */
      }                                 /* End if all channels off      */

   imax = Screen_Width * Screen_Height * ((depth+1)/8);
   izero = int2pix[0];

   for (j = 0; j < NGREY; j++) {        /* Clear Image                  */
      if ((channel == 0) || (channel == j+1)) {
         if (depth == 8) {              /* If special depth, quick clear*/
            t1 = plane_data[j];
            for (i = 0; i < imax; i++)
                *t1++ = izero;          /* set pixel to zero            */
            }
         else {                         /* Else slow clear              */
            for (i = 0; i < Screen_Width; i++) { 
               for (k = 0; k < Screen_Height; k++)
                  XPutPixel (plane[j], i, k, izero);
               }
            }
         }
      }
   if (channel == 0 || OnlyWritten) {   /* If clearing all graphs       */
      t2 = graph_data;                  /* Clear graphics data          */
      for (i = 0; i < imax; i++)
         *t2++ = 0;
      }
   else {                               /* Else clearing only one graph */
      if (channel > NGREY) {               
         gph_mask = 15;
                                        /* set mask to all but channel  */
         if (channel == NGREY + 1) gph_mask = 14;
         if (channel == NGREY + 2) gph_mask = 13;
         if (channel == NGREY + 3) gph_mask = 11;
         if (channel == NGREY + 4) gph_mask = 7;
         t2 = graph_data;               /* point to begin of graph plane*/
         for (i = 0; i < imax; i++)     /* for all pixels in graph plane*/
            *t2++ = *t2 & gph_mask;     /* zero this graph              */
         }                              /* End if clear only one graph  */
      }                                 /* End if clering all           */

   scrwrt (0, 0, Screen_Width - 1, Screen_Height - 1);

   return (0);
}

zoom()
/*--------------------------------------------------------------------*/
/* Sets the zoom registers                                            */
/*--------------------------------------------------------------------*/
{
      int i;
/*--------------------------------------------------------------------*/
   if ((xbuf.parms[0] < 0) || (xbuf.parms[0] > NGRTOT)) {
      fprintf (stderr, "Illegal zoom channel %d\n", xbuf.parms[0]);
      return (-1);
      }
   else if (xbuf.parms[0] > NGREY) {
      }
   else if (xbuf.parms[0] == 0) {
      upleft_mag = xbuf.parms[1];
      for (i = 0; i < NGREY; i++) {
         upleft_x[i] = xbuf.parms[2];
         upleft_y[i] = xbuf.parms[3];
         }
      scrwrt (0, 0, Screen_Width - 1, Screen_Height - 1);
      }
   else {
      upleft_mag = xbuf.parms[1];
      upleft_x[xbuf.parms[0]-1] = xbuf.parms[2];
      upleft_y[xbuf.parms[0]-1] = xbuf.parms[3];
      if (xbuf.parms[0] == cur_chan)
         scrwrt (0, 0, Screen_Width - 1, Screen_Height - 1);
      }
   return (0);
}

read_zoom(nparms)
/*--------------------------------------------------------------------*/
/* Read the zoom/scroll registers.                                    */
/*--------------------------------------------------------------------*/
short int *nparms;
/*--------------------------------------------------------------------*/
{
   int i;
/*--------------------------------------------------------------------*/
   i = cur_chan - 1;

   *nparms = 4;
   ybuf.data[0] = cur_chan;
   ybuf.data[1] = upleft_mag;
   ybuf.data[2] = upleft_x[i];
   ybuf.data[3] = upleft_y[i];

   return (0);
}
