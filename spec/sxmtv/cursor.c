#include "sxmtv.h"

static int button_a = 0;                     /* Number of times each  */
static int button_b = 0;                     /* button was pressed    */
static int button_c = 0;                     /* since the last RBUTT  */
static int button_d = 0;                     /* operation.            */
static int cursor_x = 0;                     /* Last cursor x and y   */
static int cursor_y = 0;                     /* position.             */

/*--------------------------------------------------------------------*/

RecordCursor (x, y)
int x, y;
{
                                        /* -> wrt to window           */
   cursor_x = x + sc_centre_x - sc_width2 + 1;
   cursor_y = y + sc_centre_y - sc_height2 + 1;

              /* guard against border crossings with a button pressed */
   if (cursor_x < 0) cursor_x = 0;
   if (cursor_y < 0) cursor_y = 0;
   if (cursor_x > Screen_Width - 1) cursor_x = Screen_Width - 1;
   if (cursor_y > Screen_Height - 1) cursor_y = Screen_Height - 1;
}

int CheckKey(key)
KeySym key;
{
   int retval = 0;

   /* The keys F3, F4, F5 and F6 are the buttons A, B, C, D */
   switch (key) {
       case XK_F8:                     /* Toggle SXMTV debug flag */
           SXMTVDebug = !SXMTVDebug;
           fprintf (stderr,"SXMTV   : Debugging Toggled with button F8\n");
           break;
       case XK_F9:                     /* Toggle Xwin debug flag */
           XDebug = !XDebug;
           break;
       case XK_KP_F1:                  /* A button */
       case XK_F3:
       case XK_A:
       case XK_a:
           button_a++;
           break;
       case XK_KP_F2:                  /* B button */
       case XK_F4:
       case XK_B:
       case XK_b:
           button_b++;
           break;
       case XK_KP_F3:                  /* C button */
       case XK_F5:
       case XK_C:
       case XK_c:
           button_c++;
           break;
       case XK_KP_F4:                  /* D button */
       case XK_F6:
       case XK_D:
       case XK_d:
           button_d++;
           break;
       case XK_KP_Add:
       case XK_F7:
       case XK_F2:
           (void) resize_pressed();
           fprintf (stderr,"SXMTV   : Screen Resize Toggled with button F2\n");
           break;
       case XK_F20:
       case XK_Escape:
           retval = -1;
           break;
       default:
           break;
      }
      return(retval);
}

GetCursor(cx, cy)
short int *cx;
short int *cy;
{
    *cx = coord_x(cursor_x);
    *cy = coord_y(cursor_y);

    return(0);
}

movecursor()
{
   int xc, yc;

   cursor_x =  Memory_x (xbuf.parms[0]);
   cursor_y =  Memory_y (xbuf.parms[1]);
                                        /* -> wrt to window           */
   xc = cursor_x - sc_centre_x + sc_width2 - 1;
   yc = cursor_y - sc_centre_y + sc_height2 - 1;

                         /* move cursor only if it would be in window */
   if ((xc < sc_width) && (yc < sc_height) && (xc >= 0) && (yc >= 0))
      XWarpPointer (display, win, win, 0, 0, sc_width, sc_height,
         xc, yc);

    return (0);
}

readbuttons()
{
    ybuf.data[0] =  button_a;
    ybuf.data[1] =  button_b;
    ybuf.data[2] =  button_c;
    ybuf.data[3] =  button_d;

    button_a = button_b = button_c = button_d = 0;
    return (0);
}

int cursor_button()
{
    ybuf.data[0] = coord_x (cursor_x);
    ybuf.data[1] = coord_y (cursor_y);

    ybuf.data[2] =  button_a;
    ybuf.data[3] =  button_b;
    ybuf.data[4] =  button_c;
    ybuf.data[5] =  button_d;

    button_a = button_b = button_c = button_d = 0;
    return (0);
}

