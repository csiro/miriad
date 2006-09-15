#include "sxmtv.h"

cmap_wlut()
/*--------------------------------------------------------------------*/
/*   Write the NColour LookUpTable into memory and to colormap        */
/*--------------------------------------------------------------------*/
{
   register int i, j;
/*--------------------------------------------------------------------*/
   j = xbuf.parms[3] - 1;
   if ((xbuf.parms[3] < 1) || (xbuf.parms[3] > NGREY)) {
      fprintf (stderr, "Illegal grey channel %d\n", xbuf.parms[3]);
      return (-1);
      }
   else {
      if (xbuf.parms[0] != 0)
         for (i = 0; i < NColour; ++i)
            rlut[j][i] = xbuf.data[i];
      if (xbuf.parms[1] != 0)
         for (i = 0; i < NColour; ++i)
            glut[j][i] = xbuf.data[i];
      if (xbuf.parms[2] != 0)
         for (i = 0; i < NColour; ++i)
            blut[j][i] =  xbuf.data[i];
      return (xbuf.parms[3] == cur_chan ? cmap_change() : 0);
      }
}

cmap_rlut()
/*--------------------------------------------------------------------*/
/*   Read the NColour LookUpTable from  memory                        */
/*--------------------------------------------------------------------*/
{
   register int i, j;
/*--------------------------------------------------------------------*/

   j = xbuf.parms[3] - 1;
   if ((j < 0) || (j > NGREY-1)) {
      fprintf (stderr, "Illegal grey channel %d\n", j+1);
      return (-1);
      }
   else {
      if (xbuf.parms[0] != 0)
         for (i = 0; i < NColour; ++i)
            ybuf.data[i] = rlut[j][i];
      if (xbuf.parms[1] != 0)
         for (i = 0; i < NColour; ++i)
            ybuf.data[i] = glut[j][i];
      if (xbuf.parms[2] != 0)
         for (i = 0; i < NColour; ++i)
            ybuf.data[i] = blut[j][i];
      return (0);
      }
}

cmap_wofm()
/*--------------------------------------------------------------------*/
/*   Write the NINTENS OutputFunction into memory and to colormap     */
/*--------------------------------------------------------------------*/
{
   register int i;
/*--------------------------------------------------------------------*/
   if (xbuf.parms[0] != 0)
      for (i = 0; i < NINTENS; ++i)
         rofm[i] = xbuf.data[i];
   if (xbuf.parms[1] != 0)
      for (i = 0; i < NINTENS; ++i)
         gofm[i] =  xbuf.data[i];
   if (xbuf.parms[2] != 0)
      for (i = 0; i < NINTENS; ++i)
         bofm[i] =  xbuf.data[i];

   return (cmap_change());
}

cmap_rofm()
/*--------------------------------------------------------------------*/
/*   Read the NINTENS OutputFunction from memory                      */
/*--------------------------------------------------------------------*/
{
   register int i;
/*--------------------------------------------------------------------*/
   if (xbuf.parms[0] != 0)
      for (i = 0; i < NINTENS; ++i)
         ybuf.data[i] = rofm[i];
   if (xbuf.parms[1] != 0)
      for (i = 0; i < NINTENS; ++i)
         ybuf.data[i] = gofm[i];
   if (xbuf.parms[2] != 0)
      for (i = 0; i < NINTENS; ++i)
         ybuf.data[i] = bofm[i];

   return (0);
}

cmap_change()
/*--------------------------------------------------------------------*/
/* Changes the colormap based on the stored LUTs and OFMs in core.    */
/* Adapted from a routine used in  SSS.                               */
/*--------------------------------------------------------------------*/
{
   register int i, ns, j;
/*--------------------------------------------------------------------*/
   j = cur_chan - 1;
                                       /* do the NColour of image      */
                                       /* first                        */
   if (cur_chan > 0) {
      for (i = 0; i < NColour; ++i) {
         colour_table[i].red = rofm[rlut[j][i]] << COLORSHIFT;
         colour_table[i].green = gofm[glut[j][i]] << COLORSHIFT;
         colour_table[i].blue = bofm[blut[j][i]] << COLORSHIFT;
         }
      colour_table[0].red = colour_table[0].blue
        = colour_table[0].green = 0;
      }
                                       /* image is off                */
   else {
      for (i = 0; i < NColour; ++i) {
         colour_table[i].red = colour_table[i].blue
           = colour_table[i].green = 0;
         }
      }
/*                                      add in graphics               */
   ns = NColour;
   for (i = NColour; i < NValue; ++i) {
       colour_table[i].red = rgcol[i-ns] << COLORSHIFT;
       colour_table[i].green = ggcol[i-ns] << COLORSHIFT;
       colour_table[i].blue = bgcol[i-ns] << COLORSHIFT ;
       }
   XStoreColors (display, TV_colour, colour_table, NValue);

   return (0);
}

cmap_graph()
/*--------------------------------------------------------------------*/
/*   Switch graphics plane(s) on or off.                              */
/*--------------------------------------------------------------------*/
{
   unsigned long int prevgraph;
/*--------------------------------------------------------------------*/
   if ((xbuf.parms[0] < 1) || (xbuf.parms[0] > NGRAPH)) {
      fprintf (stderr, "Illegal graphics channel %d\n", xbuf.parms[0]);
      return (-1);
      }
   else {
      prevgraph = rwgraph;
      gph_mask = 1;
      if (xbuf.parms[0] == 2) gph_mask = 2;
      if (xbuf.parms[0] == 3) gph_mask = 4;
      if (xbuf.parms[0] == 4) gph_mask = 8;
      if (xbuf.parms[1] > 0)
         rwgraph = rwgraph | gph_mask;
      else
         rwgraph = rwgraph & (~ gph_mask);
      if (prevgraph != rwgraph) {
         scrwrt (0, 0, Screen_Width - 1, Screen_Height - 1);
         return (cmap_change());
         }
      else
         return (0);
      }
}

cmap_split()
/*--------------------------------------------------------------------*/
/*   Switch grey channel on or off.                                   */
/*--------------------------------------------------------------------*/
{
      int prevchan;
/*--------------------------------------------------------------------*/
      if ((xbuf.parms[0] < 0) || (xbuf.parms[0] > NGREY)) {
         fprintf (stderr, "Illegal grey channel %d\n", xbuf.parms[0]);
         return (-1);
         }
      else if ((xbuf.parms[1] != xbuf.parms[0]) ||
         (xbuf.parms[2] != xbuf.parms[0]) ||
         (xbuf.parms[3] != xbuf.parms[0])) {
         fprintf (stderr, "Split not implemented %d %d %d %d\n",
            xbuf.parms[0], xbuf.parms[1], xbuf.parms[2], xbuf.parms[3]);
         return (-1);
         }
      else {
         prevchan = cur_chan;
         cur_chan = xbuf.parms[0];
         if (prevchan != cur_chan) {
            scrwrt (0, 0, Screen_Width - 1, Screen_Height - 1);
            return (cmap_change());
            }
         else
            return (0);
         }
}

cmap_wgrfx()
/*--------------------------------------------------------------------*/
/* Writes the cursor and graphics colour assignment.                  */
/*                                                                    */
/* MRC 90/Feb/15:                                                     */
/*--------------------------------------------------------------------*/
{
   void crscol();
   int i;

   if ((xbuf.parms[0] < 0) || (xbuf.parms[0] > NGRAPH)) {
      fprintf (stderr, "Illegal grafix channel %d\n", xbuf.parms[0]);
      return (-1);
      }

   rgrfx[xbuf.parms[0]] = xbuf.parms[1];
   ggrfx[xbuf.parms[0]] = xbuf.parms[2];
   bgrfx[xbuf.parms[0]] = xbuf.parms[3];
                                        /* make cross colors          */
   crscol (rgrfx, rgcol);
   crscol (ggrfx, ggcol);
   crscol (bgrfx, bgcol);
                                        /* make cursor colors         */
   if (xbuf.parms[0] == 0) {
      if (fg_curs.pixel >= 0) {
         bg_curs.red = 0;
         bg_curs.green = 0;
         bg_curs.blue = 0;
         fg_curs.red = rgcol[15] << COLORSHIFT;
         fg_curs.green = ggcol[15] << COLORSHIFT;
         fg_curs.blue = bgcol[15] << COLORSHIFT;
         XRecolorCursor (display, cursor, &fg_curs, &bg_curs);
         }
      return (0);
      }
                                        /* change the screen colors   */
   else
      return (cmap_change());

}

void crscol (grfx, gcol)
/*--------------------------------------------------------------------*/
/* converts grfx[5] to full set of colors gcol[16] where              */
/* Input:  grfx i[5]   cursor, 4 graphics values for a color          */
/* Output: gcol i[16]  graphics values 1-15, cursor for the color     */
/*--------------------------------------------------------------------*/
   int *grfx, *gcol;
/*--------------------------------------------------------------------*/
{
   *(gcol+0) = *(grfx+1);
   *(gcol+1) = *(grfx+2);
   *(gcol+2) = *(grfx+1) ^ *(grfx+2);
   *(gcol+3) = *(grfx+3);
   *(gcol+4) = *(grfx+1) ^ *(grfx+3);
   *(gcol+5) = *(grfx+2) ^ *(grfx+3);
   *(gcol+6) = *(grfx+1) ^ *(gcol+5);
   *(gcol+7) = *(grfx+4);
   *(gcol+8) = *(grfx+4) ^ *(gcol+0);
   *(gcol+9) = *(grfx+4) ^ *(gcol+1);
   *(gcol+10) = *(grfx+4) ^ *(gcol+2);
   *(gcol+11) = *(grfx+4) ^ *(gcol+3);
   *(gcol+12) = *(grfx+4) ^ *(gcol+4);
   *(gcol+13) = *(grfx+4) ^ *(gcol+5);
   *(gcol+14) = *(grfx+4) ^ *(gcol+6);

   *(gcol+15) = *(grfx+0);
}

cmap_rgrfx()
/*--------------------------------------------------------------------*/
/* Reads the cursor and graphics colour assignment.                   */
/*                                                                    */
/* MRC 90/Feb/15:                                                     */
/*--------------------------------------------------------------------*/
{
   if ((xbuf.parms[0] < 0) || (xbuf.parms[0] > NGRAPH)) {
      fprintf (stderr, "Illegal grafix channel %d\n", xbuf.parms[0]);
      return (-1);
      }

   ybuf.data[0] = rgrfx[xbuf.parms[0]];
   ybuf.data[1] = ggrfx[xbuf.parms[0]];
   ybuf.data[2] = bgrfx[xbuf.parms[0]];

   return (0);
}
