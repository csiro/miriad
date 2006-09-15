/*--------------------------------------------------------------------*/

#include "sxmtv.h"

scrwrt (xs, ys, xe, ye)
/*--------------------------------------------------------------------*/
/* Draws from memory image to the screen taking account of zoom,      */
/* scroll and window offsets etc. Updates that rectangle enclosed by  */
/* xs, ys, xe, ye.  Dimensions are screen units.                      */
/*--------------------------------------------------------------------*/
   int xs, ys, xe, ye;
/*--------------------------------------------------------------------*/
{
   int xmin, xmax, ymin, ymax, xext, yext, amin, amax, xoff, yoff;
   int choff;
   void scrdoit();
/*--------------------------------------------------------------------*/
   choff = max (0, cur_chan - 1);
/*                                      Upper left quadrant           */
   xmin = upleft_x[choff] * upleft_mag + sc_centre_x - sc_width2 + 1;
   xmax = upleft_x[choff] * upleft_mag + sc_centre_x + sc_width2;
   if (xmin >= Screen_Width * upleft_mag) {
      xmin -= Screen_Width * upleft_mag;
      xmax -= Screen_Width * upleft_mag;
      }
   xext = xmax - (Screen_Width - 1) * upleft_mag;
   if (xext > 0) xmax = (Screen_Width - 1) * upleft_mag;

   ymin = upleft_y[choff] * upleft_mag + sc_centre_y - sc_height2 + 1;
   ymax = upleft_y[choff] * upleft_mag + sc_centre_y + sc_height2;
   if (ymin >= Screen_Height * upleft_mag) {
      ymin -= Screen_Height * upleft_mag;
      ymax -= Screen_Height * upleft_mag;
      }
   yext = ymax - (Screen_Height - 1) * upleft_mag;
   if (yext > 0) ymax = (Screen_Height - 1) * upleft_mag;
   xoff = yoff = 0;
   scrdoit (xs, ys, xe, ye, xmin, ymin, xmax, ymax, xoff, yoff);
/*                                         Upper right quadrant       */
   if (xext > 0) {
      amin = 0;
      amax = xext - upleft_mag;
      xoff = (xmax + upleft_mag - xmin);
      yoff = 0;
      scrdoit (xs, ys, xe, ye, amin, ymin, amax, ymax, xoff, yoff);
      }
/*                                          Lower left quadrant       */
   if (yext > 0) {
      amin = 0;
      amax = yext - upleft_mag;
      xoff = 0;
      yoff = (ymax + upleft_mag - ymin);
      scrdoit (xs, ys, xe, ye, xmin, amin, xmax, amax, xoff, yoff);
      }
/*                                          Lower right quadrant      */
   if ((xext > 0) && (yext > 0)) {
      xoff = (xmax + upleft_mag - xmin);
      yoff = (ymax + upleft_mag - ymin);
      amin = 0;
      amax = yext - upleft_mag;
      xmin = 0;
      xmax = xext - upleft_mag;
      scrdoit (xs, ys, xe, ye, xmin, amin, xmax, amax, xoff, yoff);
      }

}

void scrdoit (xs, ys, xe, ye, xmin, ymin, xmax, ymax, xoff, yoff)
/*--------------------------------------------------------------------*/
/* Draws from memory image to the screen taking account of zoom,      */
/* scroll and window offsets etc. Updates that rectangle enclosed     */
/* by xs, ys, xe, ye.  Dimensions are screen units before zoom for xs,*/
/* ys, xe, ye and AFTER zoom for the others.  This means that the left*/
/* column and upper row and right column and lower row may not be     */
/* replicated upleft_mag times in zoom.                               */
/*--------------------------------------------------------------------*/
   int xs, ys, xe, ye, xmin, ymin, xmax, ymax, xoff, yoff;
/*--------------------------------------------------------------------*/
{
   int i, x, y, offset, k, choff;
   register char *pi, *pl, *gi;
   int gphv, imv, mem_full[4096], xx, yy, nx, ny;
   unsigned long int pv;
   register int j, jj;
/*--------------------------------------------------------------------*/
   upleft_mag = max (1, upleft_mag);
   if (SXMTVDebug) {
      fprintf (stderr, "xs,ys,xe,ye,mag %d %d %d %d %d\n", xs, ys, xe,
         ye, upleft_mag);
      fprintf (stderr, "xmin,xmax,ymin,ymax %d %d %d %d\n", xmin, xmax,
         ymin, ymax);
      fprintf (stderr, "xoff, yoff %d %d\n", xoff, yoff);
      }
   xs = max (xs, xmin/upleft_mag);
   ys = max (ys, ymin/upleft_mag);
   xe = min (xe, xmax/upleft_mag);
   ye = min (ye, ymax/upleft_mag);
   choff = max (cur_chan - 1, 0);

   if ((xe >= xs) && (ye >= ys)) {
                                       /* find number x points        */
      nx = upleft_mag * (xe - xs + 1);
      offset = upleft_mag * xs - xmin + xoff;
      if (offset < 0) {
         nx += offset;
         offset = 0;
         }
      nx = min (nx, xmax + xoff - offset + 1);
                                       /* no graphics is faster       */
      if (rwgraph <= 0) {
                                       /* no zoom is easy             */
         if (upleft_mag <= 1)
            XPutImage (display, win, ImageGC, plane[choff], xs, ys,
               xs - xmin + xoff, ys - ymin + yoff, nx, ye - ys + 1);
                                        /* zoomed by replication      */
         else {
            for (i = ys; i <= ye; i++) {
                                        /* fast when 8-bit chars      */
               if (depth == 8) {
                                        /* zoom a row                 */
                  for (x=0; x < upleft_mag; x++) {
                     offset = upleft_mag * xs - xmin + x + xoff;
                     pl = line_data + offset;
                     pi = plane_data[choff] + Screen_Width * i + xs;
                     j = (xe - xs) + 1;
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
                                        /* replicate the row          */
                  offset = max (upleft_mag * xs - xmin + xoff, 0);
                  yy = upleft_mag * i - ymin + yoff;
                  ny = upleft_mag;
                  if (yy < 0) {
                     ny += yy;
                     yy = 0;
                     }
                  ny = min (ymax + yoff + 1 - yy, ny);
                  for (y = 1; y < ny; y++)
                     bcopy (line_data + (offset),
                        line_data + (Screen_Width * y + offset), nx);
                                        /* move to the display        */
                  XPutImage (display, win, ImageGC, line, offset, 0,
                     offset, yy, nx, ny);
                  } /* depth == 8 */
                                        /* Displays that are not      */
                                        /* 8-bit deep are handled     */
                                        /* with generic Xlib calls.   */
               else {
                                        /* zoom a row                 */
                  offset = upleft_mag * xs - xmin + xoff;
                  for (j = xs; j <= xe; j++) {
                     pv = XGetPixel (plane[choff], j, i);
                     for (jj = 0; jj < upleft_mag; jj++) {
                        if (offset > 0) XPutPixel (line, offset, 0, pv);
                        offset++;
                        } /* jj */
                     } /* j = xs-xe */
                                        /* replicate the row          */
                  offset = upleft_mag * xs - xmin + xoff;
                  yy = upleft_mag * i - ymin + yoff;
                  ny = upleft_mag;
                  if (yy < 0) {
                     ny += yy;
                     yy = 0;
                     }
                  ny = min (ymax + yoff + 1 - yy, ny);
                  for (y = 1; y < ny; y++)
                     XPutImage (display, win, ImageGC, line, offset, 0,
                        offset, yy + y, nx, 1);
                  }  /* depth != 8 */
               }     /* for i = ys:ye       */
            }        /* else upleft_mag > 1 */
         }           /* rwgraph <= 0        */
                                        /* graphics are on            */
      else {
         for (i = ys; i <= ye; i++) {
                                        /* fast when 8-bit chars      */
            if (depth == 8) {
                                        /* get graphics + image line  */
               pi = plane_data[choff] + Screen_Width * i + xs;
               gi = graph_data + Screen_Width * i + xs;
               jj = NColour - 1;
               for (j = xs; j <= xe; j++) {
                  gphv = rwgraph & *gi++;
                  imv = *pi++;
                  if (gphv > 0)
                     mem_full[j] = int2pix[jj + gphv];
                  else
                     mem_full[j] = imv;
                  }
                                        /* zoom a row                 */
               for (x = 0; x < upleft_mag; x++) {
                  offset = upleft_mag * xs - xmin + x + xoff;
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
                                        /* replicate the row         */
               offset = max (upleft_mag * xs - xmin + xoff, 0);
               yy = upleft_mag * i - ymin + yoff;
               ny = upleft_mag;
               if (yy < 0) {
                  ny += yy;
                  yy = 0;
                  }
               ny = min (ymax + yoff + 1 - yy, ny);
               for (y = 1; y < ny; y++)
                  bcopy (line_data + (offset),
                     line_data + (Screen_Width * y + offset), nx);
                                        /* move to the display        */
               XPutImage (display, win, ImageGC, line, offset, 0,
                  offset, yy, nx, ny);
               } /* depth = 8 */
                                        /* Displays that are not      */
                                        /* 8-bit deep are handled     */
                                        /* with generic Xlib calls.   */
            else {
                                        /* get graphics + image line  */
               offset = upleft_mag * xs - xmin + xoff;
               for (j = xs; j <= xe; j++) {
                  gphv = rwgraph & XGetPixel (graph, j, i);
                  if (gphv > 0)
                     pv = int2pix[jj + gphv];
                  else
                     pv = XGetPixel (plane[choff], j, i);
                                        /* zoom the pixel             */
                  for (jj = 0; jj < upleft_mag; jj++) {
                     if (offset > 0) XPutPixel (line, offset, 0, pv);
                     offset++;
                     } /* jj */
                  } /* j = xs-xe */
                                        /* replicate the row          */
               offset = upleft_mag * xs - xmin + xoff;
               yy = upleft_mag * i - ymin + yoff;
               ny = upleft_mag;
               if (yy < 0) {
                  ny += yy;
                  yy = 0;
                  }
               ny = min (ymax + yoff + 1 - yy, ny);
               for (y = 1; y < ny; y++)
                  XPutImage (display, win, ImageGC, line, offset, 0,
                     offset, yy + y, nx, 1);
               }  /* depth != 8 */
            }     /* for i = ys:ye       */
         }        /* rwgraph > 0         */
      }           /* xe>=xs, ye>=ys      */

}
