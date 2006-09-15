/*
 *	<cursor.c>
 */

#include "xmtv.h"

/* Private variables. */

static int button_a = 0;  /* Record the number of times each  */
static int button_b = 0;  /* button is pressed since the last */
static int button_c = 0;  /* button read operation.           */
static int button_d = 0;

static int cursor_x = 0;  /* Record the last cursor x and y position. */
static int cursor_y = 0;

/* Source code. */

/************************************************************************/
void RecordCursor(x, y)
int x, y;
{
                                        /* -> wrt to window           */
   cursor_x = x + PortX;
   cursor_y = y + PortY;

              /* guard against border crossings with a button pressed */
   if (cursor_x < 0) cursor_x = 0;
   if (cursor_y < 0) cursor_y = 0;
   if (cursor_x > Screen_Width - 1) cursor_x = Screen_Width - 1;
   if (cursor_y > Screen_Height - 1) cursor_y = Screen_Height - 1;
}

/************************************************************************/
void currentCursor(x, y)
int *x, *y;
{
    *x = cursor_x;
    *y = cursor_y;
}

/************************************************************************/
int GetCursor(cx, cy)
short int *cx, *cy;
{
    *cx = User_x(cursor_x);
    *cy = User_y(cursor_y);

    return(0);
}

/************************************************************************/
int movecursor(cx, cy)
int cx, cy;
{
    int xc, yc;

    cursor_x = Memory_x(cx);
    cursor_y = Memory_y(cy);
                                        /* -> wrt to window           */
    xc = cursor_x - PortX;
    yc = cursor_y - PortY;

                         /* move cursor only if it would be in window */
    if ((xc < (int)PortW) && (yc < (int)PortH) && (xc >= 0) && (yc >= 0))
      XWarpPointer(XtDisplay(canvas), XtWindow(canvas), XtWindow(canvas),
        0, 0, PortW, PortH, xc, yc);

    return(0);
}

/************************************************************************/
void zoomCursor(x, y)
int *x, *y;
{
    int cx, cy;

    RecordCursor(*x, *y);
    currentCursor(&cx, &cy);
    cx += upleft_x[cur_chan-1];
    cy += upleft_y[cur_chan-1];
    cx /= upleft_mag;
    cy /= upleft_mag;
    *x = User_x(cx);
    *y = User_y(cy);
}

/************************************************************************/
void buttonPressed(button)
char button;
{
    switch (button) {
      case 'a': case 'A':  button_a++; break;
      case 'b': case 'B':  button_b++; break;
      case 'c': case 'C':  button_c++; break;
      case 'd': case 'D':  button_d++; break;
      default: /* do nothing. */  break;
    }
}

/************************************************************************/
int readButtons(array)
short int array[];
{
    array[0] =  button_a;
    array[1] =  button_b;
    array[2] =  button_c;
    array[3] =  button_d;

    button_a = button_b = button_c = button_d = 0;
    return(0);
}

/************************************************************************/
int cursorButton(array)
short int array[];
{
    int retval;

    if ((retval = GetCursor(&array[0], &array[1])) == 0)
      retval = readButtons(&array[2]);

    return(retval);
}
