/*
	<colors.c>
	13nov92 jm  Modified lutRamp code to properly handle input
		    ranges that lie beyond the color table index limits.
 */

#include "xmtv.h"
#include "ofms.h"

#include <math.h>

/* Private variables. */

static XColor *colourTable;
static Colormap TV_colour;

static int *rlut[NGREY];
static int *glut[NGREY];
static int *blut[NGREY];

static unsigned short int rofm[NINTENS];
static unsigned short int gofm[NINTENS];
static unsigned short int bofm[NINTENS];

static unsigned short int rgrfx[5];       /* cursor [0]; graphics [1-4] */
static unsigned short int ggrfx[5];
static unsigned short int bgrfx[5];
static unsigned short int rgcol[16];
static unsigned short int ggcol[16];
static unsigned short int bgcol[16];

/* Source code. */

/************************************************************************/
static void colorCursor()
/*
   Sets the cursor foreground and background colors.

   This requires that int2pix be allocated and initialized and that
   the (global variable) Widget canvas has been realized.
------------------------------------------------------------------------*/
{
    Arg args[10];
    Cardinal i;
    static Boolean firstime = True;
    static Cursor canvasCursor;
    static XColor fg_curs;
    static XColor bg_curs;

                 /* Set up cursor foreground and background parameters. */
    if (firstime == True) {
      firstime = False;
      fg_curs.pixel = int2pix[NColour + 15];
      bg_curs.pixel = int2pix[0];
      fg_curs.flags = bg_curs.flags = DoRed | DoGreen | DoBlue;
              /* Retrieve the cursor associated with the Widget canvas. */
      i = 0;
      XtSetArg(args[i],  XtNcursor, (XtArgVal)&canvasCursor); i++;
      XtGetValues(canvas, args, i);
    }

    bg_curs.red = 0;
    bg_curs.green = 0;
    bg_curs.blue = 0;
    fg_curs.red = rgcol[15] << COLORSHIFT;
    fg_curs.green = ggcol[15] << COLORSHIFT;
    fg_curs.blue = bgcol[15] << COLORSHIFT;
    XRecolorCursor(XtDisplay(canvas), canvasCursor, &fg_curs, &bg_curs);
}

/************************************************************************/
static void crscol(grfx, gcol)
unsigned short int *grfx, *gcol;
/*
   Converts grfx[5] to full set of colors gcol[16] where
     Input:  grfx int[5]   cursor, 4 graphics values for a color
     Output: gcol int[16]  graphics values 1-15, cursor for the color
------------------------------------------------------------------------*/
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

/************************************************************************/
static void iraf_ofm(npts, red, green, blue)
int npts;
unsigned short int red[], green[], blue[];
/*
  Generate Doug's Tody IRAF colour table.
------------------------------------------------------------------------*/
{
    int vsat, step, i, v;
    int knot[7];

    vsat = npts - 1;
    step = npts / 6;
    for (i = 0;  i < 7;  i++) knot[i] = i * step;
    knot[6] = vsat;

    for (i = 0; i < npts; i++) red[i] = green[i] = blue[i] = 0;

    for (i = knot[0]; i <= knot[1]; i++) blue[i] = vsat * (i - knot[0]) / step;
    for (i = knot[1]; i <= knot[2]; i++) blue[i] = vsat;
    for (i = knot[2]; i <= knot[3]; i++) blue[i] = vsat * (knot[3] - i) / step;

    for (i = knot[1]; i <= knot[2]; i++) green[i] = vsat * (i - knot[1]) / step;
    for (i = knot[2]; i <= knot[4]; i++) green[i] = vsat;
    for (i = knot[4]; i <= knot[5]; i++) green[i] = vsat * (knot[5] - i) / step;

    for (i = knot[3]; i <= knot[4]; i++) red[i] = vsat * (i - knot[3]) / step;
    for (i = knot[4]; i <= knot[6]; i++) red[i] = vsat;

    for (i = knot[5]; i <= knot[6]; i++) {
      v = vsat * (i - knot[5]) / step;
      green[i] = blue[i] = (v > vsat ? vsat : v );
    }
}

/************************************************************************/
void initGraphics(dpy, cColor, g1, g2, g3, g4)
Display *dpy;
Pixel cColor, g1, g2, g3, g4;
/*
    Load the cursor and graphic plane colors.
------------------------------------------------------------------------*/
{
    Colormap colormap;
    XColor c;

    colormap = DefaultColormap(dpy, DefaultScreen(dpy));

    c.pixel = cColor;
    XQueryColor(dpy, colormap, &c);
    rgrfx[0] = c.red;  ggrfx[0] = c.green;  bgrfx[0] = c.blue;

    c.pixel = g1;
    XQueryColor(dpy, colormap, &c);
    rgrfx[1] = c.red;  ggrfx[1] = c.green;  bgrfx[1] = c.blue;

    c.pixel = g2;
    XQueryColor(dpy, colormap, &c);
    rgrfx[2] = c.red;  ggrfx[2] = c.green;  bgrfx[2] = c.blue;

    c.pixel = g3;
    XQueryColor(dpy, colormap, &c);
    rgrfx[3] = c.red;  ggrfx[3] = c.green;  bgrfx[3] = c.blue;

    c.pixel = g4;
    XQueryColor(dpy, colormap, &c);
    rgrfx[4] = c.red;  ggrfx[4] = c.green;  bgrfx[4] = c.blue;
                                     /* Make the cross pattern colors.  */
    crscol(rgrfx, rgcol);
    crscol(ggrfx, ggcol);
    crscol(bgrfx, bgcol);

    colorCursor();

    return;
}

/************************************************************************/
int initColors(w, visualList, screenDepth)
Widget w;
XVisualInfo visualList[];
int screenDepth;
/*
    Allocate some arrays, fill some static values, and initialize rest.
------------------------------------------------------------------------*/
{
    static short int initialized = 0;
    register int i, j;
    register unsigned long int k;
    int size;
    unsigned long planeMask;
    unsigned long border, background;
    double zz;
    Arg args[10];
    Cardinal count;
    Colormap defCmap;
    Display *dpy;

    if (initialized) return(0);
    initialized = 1;

    int2pix = (unsigned long int *)XtMalloc(sizeof(unsigned long int) * NValue);
    pix2int = (int *)XtMalloc(sizeof(int) * (1 << screenDepth));
    if ((int2pix == (unsigned long int *)NULL) || (pix2int == (int *)NULL)) {
      (void)fprintf(stderr, "Trouble allocating storage arrays.\n");
      return(-1);
    }

    size = visualList[0].colormap_size;
    if ((colourTable = (XColor *)XtMalloc(sizeof(XColor) * size)) == NULL) {
      (void)fprintf(stderr, "Can not allocate space for the colour table!");
      return(-1);
    }

              /* Attempt to allocate colors from the default colormap.  */
    dpy = XtDisplay(w);
    TV_colour = defCmap = DefaultColormap(dpy, DefaultScreen(dpy));

    if (XAllocColorCells(dpy, TV_colour, True, &planeMask, 0,
      int2pix, NValue) == 0) {                       /* Attempt failed! */

      (void)fprintf(stderr, "XMTV: WARNING -- Creating a virtual colormap.\n");

      TV_colour = XCreateColormap(dpy, XtWindow(w), visualList[0].visual,
        AllocAll);

              /* Copy colors from default colormap to virtual colormap. */
   /* Attempts to retain as much of the rest of the screen as possible. */
      for (i = 0; i < size; i++) {
        colourTable[i].pixel = i;
        colourTable[i].flags = DoRed | DoGreen | DoBlue;
      }

      XQueryColors(dpy, defCmap, colourTable, size);
      XStoreColors(dpy, TV_colour, colourTable, size);

      border = WhitePixel(dpy, DefaultScreen(dpy));
      background = BlackPixel(dpy, DefaultScreen(dpy));

      k = (border < (size - NValue)) ? (size - NValue) : 0;
      for (i = 0; i < NValue; i++) {        /* Avoid basic colors.  */
        if ((k == border) || (k == background)) k++;
        if ((k == border) || (k == background)) k++;
        int2pix[i] = k++;
      }

      count = 0;
      XtSetArg(args[count], XtNcolormap, (XtArgVal)TV_colour); count++;
      XtSetValues(w, args, count);
      XtSetValues(XtNameToWidget(w, "*panshell"), args, count);
    }

    for (i = 0; i < NGREY; i++) { /* Allocate space for lookup tables.  */
      rlut[i] = (int *)XtMalloc(NColour * (sizeof(int)));
      glut[i] = (int *)XtMalloc(NColour * (sizeof(int)));
      blut[i] = (int *)XtMalloc(NColour * (sizeof(int)));
    }

    zz = (NINTENS - 1) / (NColour - 1);
                             /* Start with a linear transfer function.  */
    for (i = 0; i < NValue; i++) {
      colourTable[i].pixel = int2pix[i];
      colourTable[i].flags = DoRed | DoGreen | DoBlue;
      if (i < NColour) {
        rlut[0][i] = glut[0][i] = blut[0][i] = i * zz;
        for (j = 0; j < NGREY; j++)
          rlut[j][i] = glut[j][i] = blut[j][i] = rlut[0][i];
      }
    }

    for (i = 0; i < NValue; i++)    /* Set up the reverse translation.  */
      pix2int[int2pix[i]] = i;

    return(0);
}

/************************************************************************/
void freeColors()
/*
    Release allocated color arrays.
------------------------------------------------------------------------*/
{
    register int i;

    if (colourTable != (XColor *)NULL) XtFree((char *)colourTable);
    colourTable = (XColor *)NULL;

    for (i = 0; i < NGREY; i++) {
      if (rlut[i] != NULL) {XtFree((char *)rlut[i]); rlut[i] = (int *)NULL;}
      if (glut[i] != NULL) {XtFree((char *)glut[i]); glut[i] = (int *)NULL;}
      if (blut[i] != NULL) {XtFree((char *)blut[i]); blut[i] = (int *)NULL;}
    }

    if (int2pix != (unsigned long int *)NULL) XtFree((char *)int2pix);
    int2pix = (unsigned long int *)NULL;
    if (pix2int != (int *)NULL) XtFree((char *)pix2int);
    pix2int = (int *)NULL;
}

/************************************************************************/
static int cmap_change()
/*
   Changes the colormap based on the stored LUTs and OFMs in memory.
------------------------------------------------------------------------*/
{
    register int i, j;

    j = cur_chan - 1;
    if (cur_chan > 0) {             /* Do the NColour of image first.  */
      for (i = 0; i < NColour; i++) {
        colourTable[i].red = rofm[rlut[j][i]] << COLORSHIFT;
        colourTable[i].green = gofm[glut[j][i]] << COLORSHIFT;
        colourTable[i].blue = bofm[blut[j][i]] << COLORSHIFT;
      }
      colourTable[0].red = colourTable[0].blue = colourTable[0].green = 0;
    } else {             /* Otherwise, the image plane is turned off.  */
      for (i = 0; i < NColour; i++)
        colourTable[i].red = colourTable[i].blue = colourTable[i].green = 0;
    }

    for (j = 0, i = NColour; i < NValue; j++, i++) { /* Add in the graphics. */
      colourTable[i].red = rgcol[j] << COLORSHIFT;
      colourTable[i].green = ggcol[j] << COLORSHIFT;
      colourTable[i].blue = bgcol[j] << COLORSHIFT ;
    }

    XStoreColors(XtDisplay(canvas), TV_colour, colourTable, NValue);

    return(0);
}

/************************************************************************/
int newChannel(new, resetMenu)
int new;
Boolean resetMenu;
/*
    Switch the current channel.
------------------------------------------------------------------------*/
{
    if ((cur_chan != new) && (resetMenu == True)) {
      resetMenus(0, new);
    } else if (cur_chan != new) {
      cur_chan = new;
      imageRefresh();
      return(cmap_change());
    }
    return(0);
}

/************************************************************************/
int lutRamp(beg, fin)
int beg, fin;
/*
   Load a ramp into the LUT arrays.
------------------------------------------------------------------------*/
{
    register int i, j;
    register int thresh, saturate;
    double slope;

    slope = (double)(NINTENS - 1);
    if (beg != fin) slope /= (double)(fin - beg);

    thresh = max(beg, 0);
    thresh = min(thresh, NColour - 1);

    saturate = max(fin, 0);
    saturate = min(saturate, NColour - 1);

    for (j = 0; j < NGREY; j++) {
      for (i = 0; i < thresh; i++)
        rlut[j][i] = glut[j][i] = blut[j][i] = 0;

      for (i = thresh; i < saturate; i++)
        rlut[j][i] = glut[j][i] = blut[j][i] = (i - beg) * slope;

      for (i = saturate; i < NColour; i++)
        rlut[j][i] = glut[j][i] = blut[j][i] = NINTENS - 1;
    }

    return(cmap_change());
}

#define BLACK_WHITE 0
#define COLOUR 1
#define LOUSY 2
#define NOFM 3

/************************************************************************/
int changeOFM(newOFMName)
String newOFMName;
/*
  Load either the colour or black and white OFMs into the lookup tables.
------------------------------------------------------------------------*/
{
    char *ptr;
    int value;
    unsigned short int t;
    register int i, j;
    static int lastValue = -1;
    double xx, yy, zz;

    value = 0;
    ptr = (char *)newOFMName;
    if (*ptr == '-') {
      value += NOFM;
      ptr++;
    }
    switch (*ptr) {
      case 'i': case 'I': value += LOUSY;       break;
      case 'c': case 'C': value += COLOUR;      break;
      case 'b': case 'B': value += BLACK_WHITE; break;
      default:            value  = BLACK_WHITE; break;
    }
    if (value == lastValue) return(0);
    lastValue = value;

    if ((value % NOFM) == BLACK_WHITE) {   /* set B&W OFM, Gamma = 2.2. */
      yy = NINTENS - 1;
      zz = 1.0 / 2.2;
      for (i = 0; i < NINTENS; i++) {
        xx = i / yy;
        rofm[i] = gofm[i] = bofm[i] = yy * pow(xx, zz);
/* Old way...  rofm[i] = gofm[i] = bofm[i] = i; */
      }
    } else if ((value % NOFM) == COLOUR) {
      j = sizeof(blue_ofm_def) / sizeof(blue_ofm_def[0]);
      if (j > NINTENS) j = NINTENS;
      for (i = 0; i < j; i++) {                   /* Defined in ofms.h. */
        rofm[i] = red_ofm_def[i];
        gofm[i] = green_ofm_def[i];
        bofm[i] = blue_ofm_def[i];
      }
    } else {
      iraf_ofm(NINTENS, rofm, gofm, bofm);
    }

    if (value >= NOFM) {                          /* Reverse direction. */
      for (i = 0, j = NINTENS - 1; i < NINTENS/2; i++, j--) {
        t = rofm[i];
        rofm[i] = rofm[j];
        rofm[j] = t;

        t = gofm[i];
        gofm[i] = gofm[j];
        gofm[j] = t;

        t = bofm[i];
        bofm[i] = bofm[j];
        bofm[j] = t;
      }
    }

    return(cmap_change());
}

/************************************************************************/
int cmap_wlut()
/*
   Write the NColour LookUpTable into memory and to the colormap.
------------------------------------------------------------------------*/
{
    register int i, j;

    if ((xbuf.parms[3] < 1) || (xbuf.parms[3] > NGREY)) {
      (void)fprintf(stderr, "Illegal grey channel %d\n", xbuf.parms[3]);
      return(-1);
    }

    j = xbuf.parms[3] - 1;

    if (xbuf.parms[0] != 0)
      for (i = 0; i < NColour; i++)
        rlut[j][i] = xbuf.data[i];

    if (xbuf.parms[1] != 0)
      for (i = 0; i < NColour; i++)
        glut[j][i] = xbuf.data[i];

    if (xbuf.parms[2] != 0)
      for (i = 0; i < NColour; i++)
        blut[j][i] =  xbuf.data[i];

    return((xbuf.parms[3] == cur_chan) ? cmap_change() : 0);
}

/************************************************************************/
int cmap_rlut()
/*
   Read the NColour LookUpTable from memory.
------------------------------------------------------------------------*/
{
    register int i, j;

    if ((xbuf.parms[3] < 1) || (xbuf.parms[3] > NGREY)) {
      (void)fprintf(stderr, "Illegal grey channel %d\n", xbuf.parms[3]);
      return(-1);
    }

    j = xbuf.parms[3] - 1;

    if (xbuf.parms[0] != 0)
      for (i = 0; i < NColour; i++)
        ybuf.data[i] = rlut[j][i];

    if (xbuf.parms[1] != 0)
      for (i = 0; i < NColour; i++)
        ybuf.data[i] = glut[j][i];

    if (xbuf.parms[2] != 0)
      for (i = 0; i < NColour; i++)
        ybuf.data[i] = blut[j][i];

    return(0);
}

/************************************************************************/
int cmap_wofm()
/*
   Write the NINTENS OutputFunction into memory and to the colormap.
------------------------------------------------------------------------*/
{
    register int i;

    if (xbuf.parms[0] != 0)
      for (i = 0; i < NINTENS; i++)
        rofm[i] = xbuf.data[i];

    if (xbuf.parms[1] != 0)
      for (i = 0; i < NINTENS; i++)
        gofm[i] =  xbuf.data[i];

    if (xbuf.parms[2] != 0)
      for (i = 0; i < NINTENS; i++)
        bofm[i] =  xbuf.data[i];

    return(cmap_change());
}

/************************************************************************/
int cmap_rofm()
/*
   Read the NINTENS OutputFunction from memory.
------------------------------------------------------------------------*/
{
    register int i;

    if (xbuf.parms[0] != 0)
      for (i = 0; i < NINTENS; ++i)
        ybuf.data[i] = rofm[i];

    if (xbuf.parms[1] != 0)
      for (i = 0; i < NINTENS; ++i)
        ybuf.data[i] = gofm[i];

    if (xbuf.parms[2] != 0)
      for (i = 0; i < NINTENS; ++i)
        ybuf.data[i] = bofm[i];

    return(0);
}

/************************************************************************/
int cmap_graph()
/*
   Switch graphics plane(s) on or off.
------------------------------------------------------------------------*/
{
    unsigned char gph_mask;
    int prevgraph;

    if ((xbuf.parms[0] < 1) || (xbuf.parms[0] > NGRAPH)) {
      (void)fprintf(stderr, "Illegal graphics channel %d\n", xbuf.parms[0]);
      return(-1);
    }

    prevgraph = rwgraph;
    gph_mask = 1;
    if (xbuf.parms[0] == 2) gph_mask = 2;
    if (xbuf.parms[0] == 3) gph_mask = 4;
    if (xbuf.parms[0] == 4) gph_mask = 8;

    if (xbuf.parms[1] > 0)
      rwgraph |= gph_mask;
    else
      rwgraph &= (~gph_mask);

    if (prevgraph != rwgraph) {
      imageRefresh();
      return(cmap_change());
    } else {
      return(0);
    }
}

/************************************************************************/
int cmap_split()
/*
   Switch grey channel on or off.
------------------------------------------------------------------------*/
{
    if ((xbuf.parms[0] < 0) || (xbuf.parms[0] > NGREY)) {
      (void)fprintf(stderr, "Illegal grey channel %d\n", xbuf.parms[0]);
      return(-1);
    } else if ((xbuf.parms[1] != xbuf.parms[0]) ||
               (xbuf.parms[2] != xbuf.parms[0]) ||
               (xbuf.parms[3] != xbuf.parms[0])) {
      (void)fprintf(stderr, "Split not implemented %d %d %d %d\n",
            xbuf.parms[0], xbuf.parms[1], xbuf.parms[2], xbuf.parms[3]);
      return(-1);
    } else {
      return(newChannel((int)xbuf.parms[0], True));
    }
}

/************************************************************************/
int cmap_wgrfx()
/*
   Writes the cursor and graphics colour assignment.
   [From code by MRC 15Feb90]
------------------------------------------------------------------------*/
{
    if ((xbuf.parms[0] < 0) || (xbuf.parms[0] > NGRAPH)) {
      (void)fprintf(stderr, "Illegal grafix channel %d\n", xbuf.parms[0]);
      return(-1);
    }

    rgrfx[xbuf.parms[0]] = xbuf.parms[1];
    ggrfx[xbuf.parms[0]] = xbuf.parms[2];
    bgrfx[xbuf.parms[0]] = xbuf.parms[3];
                                        /* make cross colors          */
    crscol(rgrfx, rgcol);
    crscol(ggrfx, ggcol);
    crscol(bgrfx, bgcol);
                                        /* make cursor colors         */
    if (xbuf.parms[0] == 0) {
      colorCursor();
      return(0);
    } else {                           /* change the screen colors   */
      return(cmap_change());
    }
}

/************************************************************************/
int cmap_rgrfx()
/*
   Reads the cursor and graphics colour assignment.
   [From code by MRC 15Feb90]
------------------------------------------------------------------------*/
{
    if ((xbuf.parms[0] < 0) || (xbuf.parms[0] > NGRAPH)) {
      (void)fprintf(stderr, "Illegal grafix channel %d\n", xbuf.parms[0]);
      return(-1);
    }

    ybuf.data[0] = rgrfx[xbuf.parms[0]];
    ybuf.data[1] = ggrfx[xbuf.parms[0]];
    ybuf.data[2] = bgrfx[xbuf.parms[0]];

    return(0);
}
