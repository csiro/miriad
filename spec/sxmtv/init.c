/*--------------------------------------------------------------------*/
/*  06oct92 jm  Modified ww and hh from int to unsigned int.          */
/*--------------------------------------------------------------------*/

#include "sxmtv.h"
#include <math.h>

static int cur_xcorn = -99999;        /* Current location of top-left */
static int cur_ycorn = -99999;           /* corner of smaller window. */
static int ic_xcorn  = -99999;  /* Used to set the icon x/y position. */
static int ic_ycorn  = -99999;  /* Used to set the icon x/y position. */
static int ic_width, ic_height;     /* Used to set icon width/height. */
static int cursor_shape;             /* Standard cursor shape number. */

/*--------------------------------------------------------------------*/

init()
{
    int i;
    union {
        short int shortint;
        struct {
            unsigned char lo, hi;
        }byte;
    }x;

    SXMTVDebug = False;
    XDebug  = False;
                                        /* check for byte swapped     */
    x.shortint = 17; /* any small number will do */
    if (x.shortint == x.byte.lo)
       ByteSwapped = True;
    else
       ByteSwapped = False;

   cur_chan = 1;                        /* channel 1 on, graphics off */
   rwgraph = 0;

   for (i = 0; i < NGREY; i++)
      upleft_x[i] = upleft_y[i] = 0;

   for (i = 0; i < NGREY; i++)                 /* TV channels are off */
      TvStatus[i] = 0;

   for (i = 0; i < NGRAPH; i++)          /* Graphics channels are off */
      TvStatus[NGREY+i] = 0;

   upleft_mag = 1;
                                              /* Default, buffer none */
   for (i = 0; i < (NUMOP+1); i++)
      bufferop[i] = False;
}

SetupWindow (argc, argv)
int argc;
char *argv[];
{
   int offset = 0;
   int x = 0, y = 0;              /* window position */
   unsigned int border_width = 2; /* border 2 pixels wide */
   unsigned long GCmask = 0;      /* ignore XGCValues, use defaults */
   char *window_name = "Simple XMTV Screen-Server";
   char *icon_name = "Simple TV";
   char *display_name = NULL;
   char *ProgName;
   unsigned long event_mask, value_mask;

   Pixmap icon_pixmap;
#include "xmtv.icon"

   XWMHints wm_hints;
   XSizeHints size_hints;
   XClassHint class_hints;
   XGCValues values;
   XSetWindowAttributes WinAtt;
   double xx, yy, zz;

   XVisualInfo template;                /* type of visual XVSS uses   */
   XVisualInfo *vislist;                /* list of visuals matching   */
                                        /* the template               */
   int nvis;                            /* number of visuals in list  */

   XGCValues gc_values;                 /* X11 graphics context       */
                                        /* template                   */

   unsigned long plane_masks[15];       /* plane masks from XAlloc-   */
                                        /* Colorcells.                */
   register int i, j, k;
   int icx, icy, imax;
   unsigned long int izero;             /* Matches size of int2pix.   */
   char *t1, *t2;
   unsigned long curs_pixel[2];
/*--------------------------------------------------------------------*/
                                        /* connect to X server        */
   ProgName = argv[0];

   if ((display = XOpenDisplay (display_name)) == NULL) {
      fprintf (stderr, "SXMTV: cannot connect to X server %s\n",
         XDisplayName(display_name));
      exit (-1);
      }
                                        /* get root screen number     */
   screen_num = DefaultScreen (display);
                                        /* get root (full) screen size*/
   theight = DisplayHeight (display, screen_num);
   twidth = DisplayWidth (display, screen_num);

    /* Check that the server has sufficient depth for our needs */
   if ((depth = DisplayPlanes (display, screen_num)) < 6) {
      (void) fprintf (stderr, "SXMTV: X server color table too small\n");
      (void) fprintf (stderr, "SXMTV: Image display not possible\n");
      (void) fprintf (stderr, "SXMTV: Exiting...\n");
      exit (-1);
      }

   if (depth > 8) {
      fprintf (stderr, "*******************************************\n");
      fprintf (stderr, "**  depth = %d > 8!", depth);
      fprintf (stderr, "  Sorry, must limit to 8\n");
      fprintf (stderr, "** due to 1-character limit of ZSSSX2.\n");  
      fprintf (stderr, "*******************************************\n");
      depth = 8;
      }

   template.screen = DefaultScreen(display);
   template.depth = depth;
   template.class = PseudoColor;

   vislist = XGetVisualInfo (display, VisualScreenMask|VisualDepthMask|
      VisualClassMask, &template, &nvis);
 
   if (nvis == 0) {
      perror ("No suitable visual");
      exit (1);
      }
                                        /* screen size details        */
   Screen_Width = twidth - SCREEN_LEFT - SCREEN_RIGHT;
   Screen_Height = theight - SCREEN_TOP - SCREEN_BOTTOM;
   Screen_Width = (Screen_Width / 2) * 2;
   Screen_Height = (Screen_Height / 2) * 2;
   NValue = 1 << depth;
   NValue = NValue - 40;                /* be considerate, leave some */
   NValue = min (NValue, 256);          /* ZSSSX2 uses 8-bit chars    */
   NColour = NValue - (1 << NGRAPH);
   i = NColour - 1;
   fprintf (stderr,"Using screen width height %d %d,",
      Screen_Width, Screen_Height);
   fprintf (stderr,"  max grey level %d,\n", i);
                                        /* Get cursor graphics colours*/
   ic_height = max (xmtv_width,xmtv_height);
   ic_width = ic_height;
   user_options (argc, argv);

                                        /* create opaque window       */
   value_mask = CWBorderPixel | CWBackPixel | CWBitGravity;
   WinAtt.border_pixel = curs_pixel[0] = 
      WhitePixel (display, screen_num);
   WinAtt.background_pixel = curs_pixel[1] =
      BlackPixel (display, screen_num);
   WinAtt.bit_gravity = CenterGravity;
   win = XCreateWindow (display, RootWindow(display,screen_num),
      x, y, Screen_Width, Screen_Height, border_width, depth,
      InputOutput, vislist->visual, value_mask, &WinAtt);
#ifdef AIX
   bwid = 0;
#else
   bwid = border_width;
#endif
                                        /* create pixmap of depth 1   */
                                        /* (bitmap) for icon          */
   icon_pixmap = XCreateBitmapFromData (display, win, xmtv_bits,
      (long)xmtv_width, (long)xmtv_height);
/*   icon_pixmap = XCreateBitmapFromData (display, win, xlogo64_bits,
       xlogo64_width, xlogo64_height); */
    

                                        /* set size, class, other     */
                                        /* hints for Window Manager   */
   size_hints.x = cur_xcorn + SCREEN_LEFT - bwid;
   size_hints.y = cur_ycorn + SCREEN_TOP - bwid;
   size_hints.width = sc_width;
   size_hints.height = sc_height;

   size_hints.min_width = 16;
   size_hints.min_height = 16;
   size_hints.max_width = Screen_Width;
   size_hints.max_height = Screen_Height;
   size_hints.flags = PPosition | PSize | PMinSize | PMaxSize;
                                        /* Assure keyboard input      */
   wm_hints.input = True;
   wm_hints.initial_state = IconicState ;
   wm_hints.flags = InputHint | StateHint | IconPixmapHint |
      IconPositionHint;
   wm_hints.icon_pixmap = icon_pixmap;
   wm_hints.icon_x = ic_xcorn;
   wm_hints.icon_y = ic_ycorn;
   class_hints.res_name = ProgName;
   class_hints.res_class = "SXMTVServer" ;

#ifdef X11R3                            /* X11 Release 3 or earlier   */

                                        /* set properties for window  */
                                        /* manager (before mapping)   */
   XSetStandardProperties (display, win, window_name, icon_name,
      icon_pixmap, argv, argc, &size_hints);

   XSetWMHints (display, win, &wm_hints);

   XSetClassHint (display, win, &class_hints);

#else                                   /* X11 Release 4 or later     */
    {
    XTextProperty windowName, iconName;

    if (XStringListToTextProperty (&window_name, 1, &windowName) == 0) {
       fprintf (stderr, "structure allocation for windowName fails\n");
       exit (-1);
       }
    if (XStringListToTextProperty (&icon_name, 1, &iconName) == 0) {
       fprintf (stderr, "structure allocation for iconName fails\n");
       exit (-1);
       }

   XSetWMProperties (display, win, &windowName, &iconName, argv, argc,
      &size_hints, &wm_hints, &class_hints);
   XFree(windowName.value);
   XFree(iconName.value);
   }
#endif

                                        /* select event types wanted  */
   event_mask = ExposureMask | KeyPressMask | StructureNotifyMask |
                ButtonPressMask | ButtonMotionMask |
/* added this line 4/28 jm. */ ButtonReleaseMask |
                EnterWindowMask | LeaveWindowMask;
   XSelectInput (display, win, event_mask);

                                        /* create all the images      */
    for (i = 0; i < NGREY; i++) {
       plane_data[i] = (char*) malloc 
          (Screen_Width * Screen_Height * ((depth+1) / 8));
       plane[i] = XCreateImage (display, vislist->visual, depth, ZPixmap,
          offset, plane_data[i], Screen_Width, Screen_Height, 8, 0);
       }

   graph_data = (char*) malloc
      (Screen_Width * Screen_Height * ((depth+1)/8));
   graph = XCreateImage (display, vislist->visual, depth, ZPixmap, offset,
      graph_data, Screen_Width, Screen_Height, 8, 0);
   line_data = (char*) malloc
      (Screen_Width * MAXZOOM * ((depth+1)/8));
   line = XCreateImage (display, vislist->visual, depth, ZPixmap, offset,
      line_data, Screen_Width, MAXZOOM, 8, 0);

                                       /* Allocate space for lookups   */
   for (i = 0; i < NGREY; i++) {
      rlut[i] = (int *) malloc (NColour * (sizeof (int)));
      glut[i] = (int *) malloc (NColour * (sizeof (int)));
      blut[i] = (int *) malloc (NColour * (sizeof (int)));
      }
   int2pix = (unsigned long int *)malloc(NValue * (sizeof(unsigned long int)));
   pix2int = (unsigned long int *)malloc((1 << depth) * (sizeof(unsigned long int)));

                                       /* Allocate space for the       */
                                       /* colour table:                */
   if ((colour_table = (XColor *) malloc
      (vislist->colormap_size * sizeof(XColor))) == NULL) {
      perror ("Cannot allocate space for colour table");
      exit (-1);
      }
                                       /* Attempt to allocate colours  */
                                       /* from default colourmap       */
   TV_colour = DefaultColormap (display, DefaultScreen(display));
   if (XAllocColorCells (display, TV_colour, True, plane_masks, 0,
      int2pix, NValue) == 0) {
                                       /* Attempt failed --- create a  */
                                       /* new (virtual) colour map and */
                                       /* install it as the colour map */
                                       /* for the TV:                  */
      fprintf (stderr, "SXMTV: WARNING -- creating virtual colormap\n");
      TV_colour = XCreateColormap (display, win, vislist->visual,
         AllocAll);
                                       /* Copy colours from default   */
                                       /* colourmap to virtual        */
                                       /* colourmap (retains as much  */
                                       /* of the rest of the screen   */
                                       /* as possible):               */
      for (i = 0; i < vislist->colormap_size; i++) {
         colour_table[i].pixel = i;
         colour_table[i].flags = DoRed | DoGreen | DoBlue;
         }
      XQueryColors (display,
         DefaultColormap(display, DefaultScreen(display)),
         colour_table, vislist->colormap_size);
      XStoreColors (display, TV_colour, colour_table,
         vislist->colormap_size);

                                       /* Avoid the basic colors     */
      j = 0;
      for (i = 0; i < NValue; i++) {
         if ((j == curs_pixel[0]) || (j == curs_pixel[1])) j++;
         if ((j == curs_pixel[0]) || (j == curs_pixel[1])) j++;
         int2pix[i] = j++;
         }
                                       /* Install the colourmap and  */
                                       /* let the window manager     */
                                       /* know that we want our own  */
                                       /* colour map on the canvas   */
                                       /* and the default colour map */
                                       /* on the button panel:       */
      XSetWindowColormap (display, win, TV_colour);
      }
                                       /* reverse translation        */
   for (i = 0; i < NValue; i++)
      pix2int[int2pix[i]] = i;
                                       /* Set graphics contexts:     */
   gc_values.function = GXcopy;
   gc_values.plane_mask = AllPlanes;
   ImageGC = XCreateGC (display, win, GCFunction | GCPlaneMask,
      &gc_values);
                                        /* create cursor for window   */
   cursor = XCreateFontCursor (display, cursor_shape);
   XDefineCursor (display, win, cursor);
                                        /* color the cursor           */
   fg_curs.red = rgcol[15] << COLORSHIFT;
   fg_curs.green = ggcol[15] << COLORSHIFT;
   fg_curs.blue = bgcol[15] << COLORSHIFT;
   bg_curs.red = 0;
   bg_curs.green = 0;
   bg_curs.blue = 0;
   fg_curs.flags = DoRed | DoGreen | DoBlue;
   bg_curs.flags = DoRed | DoGreen | DoBlue;
   fg_curs.pixel = int2pix[NColour + 15];
   bg_curs.pixel = int2pix[0];
   XRecolorCursor (display, cursor, &fg_curs, &bg_curs);
                                        /* specify foreground since   */
                                        /* else may be white on white */
   XSetForeground (display, ImageGC, BlackPixel(display, screen_num));
                                        /* set initial sizes          */
   Cur_Xzero = cur_xcorn - bwid;
   Cur_Yzero = cur_ycorn - bwid;
   Cur_Xsize = sc_width;
   Cur_Ysize = sc_height;
   sc_width2   = sc_width/2;
   sc_height2  = sc_height/2;
   sc_centre_x = Screen_Width/2 - 1;
   sc_centre_y = Screen_Height/2 - 1;
   XMoveResizeWindow (display, win, Cur_Xzero, Cur_Yzero, Cur_Xsize,
      Cur_Ysize);
                                        /* set OFM, Gamma = 2.2       */
   yy = NINTENS - 1;
   zz = 1.0 / 2.2;
   for (i = 0; i < NINTENS; i++) {
      xx = i / yy;
      rofm[i] = gofm[i] = bofm[i] = yy * pow (xx, zz);
      }
                                        /* "zero" fill memories       */
   imax = Screen_Width * Screen_Height * ((depth+1)/8);
   izero = int2pix[0];
   for (j = 0; j < NGREY; j++) {
      if (depth == 8) {
         t1 = plane_data[j];
         for (i = 0; i < imax; i++)
            *t1++ = izero;
         }
      else {
         for (i = 0; i < Screen_Width; i++) {
            for (k = 0; k < Screen_Height; k++)
                XPutPixel (plane[j], i, k, izero);
            }
         }
      }
   t2 = graph_data;
   for (i = 0; i < imax; i++)
      *t2++ = 0;
                                        /* Start up the window with a */
                                        /* linear transfer function:  */
   zz = NColour - 1;
   zz = yy / zz;
   for (i = 0; i < NValue; i++) {
      colour_table[i].pixel = int2pix[i];
      colour_table[i].flags = DoRed | DoGreen | DoBlue;
      if (i < NColour) {
         rlut[0][i] = glut[0][i] = blut[0][i] =  i * zz;
         if (NGREY > 1) {
            for (j = 1; j < NGREY; j++) 
               rlut[j][i] = glut[j][i] = blut[j][i] = rlut[0][i];
            }
         colour_table[i].red = colour_table[i].green =
           colour_table[i].blue = rofm[rlut[0][i]] << COLORSHIFT;
         }
      else {
         colour_table[i].red = rgcol[i-NColour] << COLORSHIFT;
         colour_table[i].blue = bgcol[i-NColour] << COLORSHIFT;
         colour_table[i].green = ggcol[i-NColour] << COLORSHIFT;
         }
      }
   XStoreColors (display, TV_colour, colour_table, NValue);
   XRecolorCursor (display, cursor, &fg_curs, &bg_curs);

} /* end SetupWindow */

user_options (argc, argv)
int argc;
char *argv[];
/*--------------------------------------------------------------------*/
/*   Set cursor, graphics colors reading user's .Xdefaults file       */
/*   Also get the icon and window geometries from the command line or */
/*   from the .Xdefaults file.					      */
/*--------------------------------------------------------------------*/
{
   char *option, *arg;
   char *IG, *ig, *WG, *wg, *uwg, *uig, g;
   char *BasicTV;
   int i, j, xx, yy, wasi, wasw;
   unsigned int ww, hh;
   long flags;
/*--------------------------------------------------------------------*/

   sc_width = sc_height = 0;
   wasi = wasw = 0;
   BasicTV = argv[0];

                                        /* Geometries                 */
                                        /* from Command line          */
   for (i = 1; i < argc; i++) {
      arg = argv[i];
      if (arg[0] == '-') {
         switch (arg[1]) {
            case 'I':                   /* Ig, IconGeometry           */
               if ((++i < argc) &&
                  ((arg[2] == 'g') || (arg[2] == 'c'))) {
                  IG = argv[i];
                  wasi = wasi + 1;
                   }
               continue;
            case 'i':                   /* ig, iconGeometry           */
               if ((++i < argc) &&
                  ((arg[2] == 'g') || (arg[2] == 'c'))) {
                  ig = argv[i];
                  wasi = wasi + 4;
                  }
               continue;
            case 'G':                   /* G, Geometry                */
               if (++i < argc) {
                  WG = argv[i];
                  wasw = wasw + 1;
                  }
               continue;
            case 'g':                   /* g, geometry                */
               if (++i < argc) {
                  wg = argv[i];
                  wasw = wasw + 4;
                  }
               continue;
            default:
               continue;
            }
         }
      }
                                        /* and/or from .Xdefaults     */
   option = XGetDefault (display, BasicTV, "geometry");
   if (option) {
      wasw = wasw + 2;
      uwg = option;
      }
   option = XGetDefault (display, BasicTV, "iconGeometry");
   if (option) {
      wasi = wasi + 2;
      uig = option;
      }
                                        /* parse the window geometry  */
   if (wasw & 4) {
      flags = XParseGeometry (wg, &xx, &yy, &ww, &hh);
      if ((XValue & flags) && (cur_xcorn < 0)) {
         if (XNegative & flags) xx = -xx;
         if ((xx >= 0) && (xx < Screen_Width)) {
            if (XNegative & flags)
               cur_xcorn = Screen_Width + xx;
            else
               cur_xcorn = xx;
            }
         }
      if ((YValue & flags) && (cur_ycorn < 0)) {
         if (YNegative & flags) yy = -yy;
         if ((yy >= 0) && (yy < Screen_Height)) {
            if (YNegative & flags)
               cur_ycorn = Screen_Height + yy;
            else
               cur_ycorn = yy;
            }
         }
      if ((WidthValue & flags) && (sc_width <= 0)) {
         if ((ww > 0) && (ww <= Screen_Width)) sc_width = ww;
         }
      if ((HeightValue & flags) && (sc_height <= 0)) {
         if ((hh > 0) && (hh <= Screen_Height)) sc_height = hh;
         }
      }
   if (wasw & 2) {
      flags = XParseGeometry (uwg, &xx, &yy, &ww, &hh);
      if ((XValue & flags) && (cur_xcorn < 0)) {
         if (XNegative & flags) xx = -xx;
         if ((xx >= 0) && (xx < Screen_Width)) {
            if (XNegative & flags)
               cur_xcorn = Screen_Width + xx;
            else
               cur_xcorn = xx;
            }
         }
      if ((YValue & flags) && (cur_ycorn < 0)) {
         if (YNegative & flags) yy = -yy;
         if ((yy >= 0) && (yy < Screen_Height)) {
            if (YNegative & flags)
               cur_ycorn = Screen_Height + yy;
            else
               cur_ycorn = yy;
            }
         }
      if ((WidthValue & flags) && (sc_width <= 0)) {
         if ((ww > 0) && (ww <= Screen_Width)) sc_width = ww;
         }
      if ((HeightValue & flags) && (sc_height <= 0)) {
         if ((hh > 0) && (hh <= Screen_Height)) sc_height = hh;
         }
      }
   if (wasw & 1) {
      flags = XParseGeometry (WG, &xx, &yy, &ww, &hh);
      if ((XValue & flags) && (cur_xcorn < 0)) {
         if (XNegative & flags) xx = -xx;
         if ((xx >= 0) && (xx < Screen_Width)) {
            if (XNegative & flags)
               cur_xcorn = Screen_Width + xx;
            else
               cur_xcorn = xx;
            }
         }
      if ((YValue & flags) && (cur_ycorn < 0)) {
         if (YNegative & flags) yy = -yy;
         if ((yy >= 0) && (yy < Screen_Height)) {
            if (YNegative & flags)
               cur_ycorn = Screen_Height + yy;
            else
               cur_ycorn = yy;
            }
         }
      if ((WidthValue & flags) && (sc_width <= 0)) {
         if ((ww > 0) && (ww <= Screen_Width)) sc_width = ww;
         }
      if ((HeightValue & flags) && (sc_height <= 0)) {
         if ((hh > 0) && (hh <= Screen_Height)) sc_height = hh;
         }
      }

   if (sc_width <= 0) sc_width = 518;
   if (sc_height <= 0) sc_height = 518;
   cur_xcorn = max (0, cur_xcorn);
   cur_ycorn = max (0, cur_ycorn);
   if (cur_xcorn >= Screen_Width) cur_xcorn = twidth - SCREEN_RIGHT
      + Screen_Width - cur_xcorn - sc_width;
   if (cur_ycorn >= Screen_Height) cur_ycorn = theight - SCREEN_BOTTOM
      + Screen_Height - cur_ycorn - sc_height;
   cur_xcorn = cur_xcorn + SCREEN_LEFT;
   cur_ycorn = cur_ycorn + SCREEN_TOP;
   cur_xcorn = min (cur_xcorn, twidth - SCREEN_RIGHT - sc_width);
   cur_ycorn = min (cur_ycorn, theight - SCREEN_BOTTOM - sc_height);
   if (SXMTVDebug) fprintf (stderr,"Initial window w,h,x,y %d %d %d %d\n",
      sc_width, sc_height, cur_xcorn, cur_ycorn);
                                        /* parse the icon geometry    */
   if (wasi & 4) {
      flags = XParseGeometry (ig, &xx, &yy, &ww, &hh);
      if ((XValue & flags) && (ic_xcorn < 0)) {
         if (XNegative & flags) xx = -xx;
         if ((xx >= 0) && (xx < twidth)) {
            if (XNegative & flags)
               ic_xcorn = twidth + xx;
            else
               ic_xcorn = xx;
            }
         }
      if ((YValue & flags) && (ic_ycorn < 0)) {
         if (YNegative & flags) yy = -yy;
         if ((yy >= 0) && (yy < theight)) {
            if (YNegative & flags)
               ic_ycorn = theight + yy;
            else
               ic_ycorn = yy;
            }
         }
      }
   if (wasi & 2) {
      flags = XParseGeometry (uig, &xx, &yy, &ww, &hh);
      if ((XValue & flags) && (ic_xcorn < 0)) {
         if (XNegative & flags) xx = -xx;
         if ((xx >= 0) && (xx < twidth)) {
            if (XNegative & flags)
               ic_xcorn = twidth + xx;
            else
               ic_xcorn = xx;
            }
         }
      if ((YValue & flags) && (ic_ycorn < 0)) {
         if (YNegative & flags) yy = -yy;
         if ((yy >= 0) && (yy < theight)) {
            if (YNegative & flags)
               ic_ycorn = theight + yy;
            else
               ic_ycorn = yy;
            }
         }
      }
   if (wasi & 1) {
      flags = XParseGeometry (IG, &xx, &yy, &ww, &hh);
      if ((XValue & flags) && (ic_xcorn < 0)) {
         if (XNegative & flags) xx = -xx;
         if ((xx >= 0) && (xx < twidth)) {
            if (XNegative & flags)
               ic_xcorn = twidth + xx;
            else
               ic_xcorn = xx;
            }
         }
      if ((YValue & flags) && (ic_ycorn < 0)) {
         if (YNegative & flags) yy = -yy;
         if ((yy >= 0) && (yy < theight)) {
            if (YNegative & flags)
               ic_ycorn = theight + yy;
            else
               ic_ycorn = yy;
            }
         }
      }

   if (ic_xcorn < 0) ic_xcorn = twidth;
   ic_ycorn = max (0, ic_ycorn);
   if (ic_xcorn >= twidth)
      ic_xcorn = 2 * twidth - ic_xcorn - ic_width;
   if (ic_ycorn >= theight)
      ic_ycorn = 2 * theight - ic_ycorn - ic_height;
   ic_xcorn = min (ic_xcorn, twidth - ic_width);
   ic_ycorn = min (ic_ycorn, theight - ic_height);
   if (SXMTVDebug) fprintf (stderr, "Initial icon w,h,x,y %d %d %d %d\n",
      ic_width, ic_height, ic_xcorn, ic_ycorn);
                                        /* ONLY from .Xdefaults       */
                                        /* cursor shape               */
                                        /* 34 => XC_crosshair         */
   option = XGetDefault (display, BasicTV, "cursorShape");
   i = option ? atoi (option) : -1;
   cursor_shape = ((i >= 0) && (i <= 154)) ? i : 34;
   cursor_shape = (cursor_shape / 2) * 2;
                                        /* cursor colours             */
   option = XGetDefault (display, BasicTV, "cursorR");
   i = option ? atoi (option) : -1;
   rgrfx[0] = ((i >= 0) && (i <= 255)) ? i : 255;
   option = XGetDefault (display, BasicTV, "cursorG");
   i = option ? atoi (option) : -1;
   ggrfx[0] = ((i >= 0) && (i <= 255)) ? i : 0;
   option = XGetDefault (display, BasicTV, "cursorB");
   i = option ? atoi (option) : -1;
   bgrfx[0] = ((i >= 0) && (i <= 255)) ? i : 255;
                                        /* graphics colours           */
   option = XGetDefault (display, BasicTV, "graphics1R");
   i = option ? atoi (option) : -1;
   rgrfx[1] = ((i >= 0) && (i <= 255)) ? i : 255;
   option = XGetDefault (display, BasicTV, "graphics1G");
   i = option ? atoi (option) : -1;
   ggrfx[1] = ((i >= 0) && (i <= 255)) ? i : 255;
   option = XGetDefault (display, BasicTV, "graphics1B");
   i = option ? atoi (option) : -1;
   bgrfx[1] = ((i >= 0) && (i <= 255)) ? i : 0;
   option = XGetDefault (display, BasicTV, "graphics2R");
   i = option ? atoi (option) : -1;
   rgrfx[2] = ((i >= 0) && (i <= 255)) ? i : 16;
   option = XGetDefault (display, BasicTV, "graphics2G");
   i = option ? atoi (option) : -1;
   ggrfx[2] = ((i >= 0) && (i <= 255)) ? i : 255;
   option = XGetDefault (display, BasicTV, "graphics2B");
   i = option ? atoi (option) : -1;
   bgrfx[2] = ((i >= 0) && (i <= 255)) ? i : 0;
   option = XGetDefault (display, BasicTV, "graphics3R");
   i = option ? atoi (option) : -1;
   rgrfx[3] = ((i >= 0) && (i <= 255)) ? i : 255;
   option = XGetDefault (display, BasicTV, "graphics3G");
   i = option ? atoi (option) : -1;
   ggrfx[3] = ((i >= 0) && (i <= 255)) ? i : 171;
   option = XGetDefault (display, BasicTV, "graphics3B");
   i = option ? atoi (option) : -1;
   bgrfx[3] = ((i >= 0) && (i <= 255)) ? i : 255;
   option = XGetDefault (display, BasicTV, "graphics4R");
   i = option ? atoi (option) : -1;
   rgrfx[4] = ((i >= 0) && (i <= 63)) ? i : 0;
   option = XGetDefault (display, BasicTV, "graphics4G");
   i = option ? atoi (option) : -1;
   ggrfx[4] = ((i >= 0) && (i <= 63)) ? i : 0;
   option = XGetDefault (display, BasicTV, "graphics4B");
   i = option ? atoi (option) : -1;
   bgrfx[4] = ((i >= 0) && (i <= 63)) ? i : 0;
                                        /* make cross colors          */
   crscol (rgrfx, rgcol);
   crscol (ggrfx, ggcol);
   crscol (bgrfx, bgcol);

} /* end user_options */
