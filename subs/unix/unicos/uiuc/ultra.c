/************************************************************************/
/*									*/
/*		Routines to drive the Ultra Frame buffer.		*/
/*									*/
/* History:								*/
/* rjs   nov88 Original version.					*/
/* rjs 16jul89 Tidied error handling. Fiddled buffer switching.		*/
/* rjs 27jul89 More fiddles for an unreliable device.			*/
/* rjs 29jul89 Set zoom to 1 in ureset. Debugging statements		*/
/************************************************************************/
#include <errno.h>
#include <ultra/ugraf.h>
#ifdef FILE
#include <fcntl.h>
#endif
#define BUFSIZE 131072
#define NULL 0
#define TRUE 1
#define FALSE 0

struct UG_PARAM params;
struct UG_TBLK tblock;
int *buffer;
int lut_lo[256],lut_hi[256],sign,output,status;

/* Externals. */

char *malloc();
static void uflush(),ubug();

#define min(a,b) ((a) < (b) ? (a) : (b))

/************************************************************************/
void UOPEN()
{
  buffer = (int *)malloc(sizeof(int)*BUFSIZE);

  tblock.tx = 0;
  tblock.ty = 0;
  tblock.npixel = 0;
  tblock.nline = 0;
  tblock.stride = 0;
  tblock.packed = 0;
  tblock.link = NULL;
  tblock.addr = buffer;
  output = FALSE;
  sign = -1;

  params.buffer = 0;
  params.sig = 0;
  params.host = NULL;
  params.term_type = UG_SONY;
  params.dx = 0 /*384*/;
  params.dy = 0;
  params.zx = params.zy = 1;
  params.blank = 0;
  params.pan_set = 0 /*1*/;
  params.zoom_set = 0 /*1*/;
  params.buf_ctl = 0;
  params.link = NULL;

#ifdef DEBUG
  printf("uopen ...");
#endif
  if(status = ugraf(UG_OPEN,&params)) ubug();
#ifdef DEBUG
  printf("Done\n");
#endif
  params.buf_ctl = 0;
  params.zoom_set = 0;
  params.pan_set = 0;
}
/************************************************************************/
void UFLUSH()
{
  if(output) params.buf_ctl = UG_SW;
  output = FALSE;
  uflush();
}
/************************************************************************/
static void uflush()
/*
  Flush anything that may need to be flushed to the Ultra frame buffer.
------------------------------------------------------------------------*/
{
#ifdef FILE
  int fd;
#endif
  if(params.zoom_set || params.pan_set || params.buf_ctl || tblock.nline){
    if(params.buf_ctl){
      params.buf_ctl = UG_SW/*|UG_SW_H*/;
    }
    if(tblock.nline){
      tblock.addr = buffer;
      if(sign < 0){
	tblock.addr += BUFSIZE - tblock.nline*tblock.npixel/2;
	tblock.ty -= tblock.nline - 1;
      }
      tblock.stride = tblock.npixel;
      params.link = &tblock;
#ifdef FILE
      fd = open("ultra.dmp",O_CREAT|O_TRUNC|O_WRONLY,0666);
      write(fd,tblock.addr,tblock.nline*tblock.npixel*4);
      close(fd);
#endif
    }
#ifdef DEBUG
    printf("uflush: ");
    if(params.zoom_set)printf("zoom %d,%d: ",params.zx,params.zy);
    if(params.pan_set) printf("pan %d,%d: ",params.dx,params.dy);
    if(params.buf_ctl) printf("buf_ctl %d: ",params.buf_ctl);
    if(tblock.nline)   printf("lines x,y,nx,ny %d,%d,%d,%d",
	tblock.tx,tblock.ty,tblock.npixel,tblock.nline);
#endif
    if(status = ugraf(UG_WRITE,&params)) ubug();
    if(status = ugraf(UG_WAIT,&params))  ubug();
#ifdef DEBUG
    printf("... Done\n");
#endif
    params.link = NULL;
    params.buf_ctl = tblock.nline = params.zoom_set = params.pan_set = 0;
  }
}
/************************************************************************/
void UCLOSE()
/*
  Close the device. Before doing this, check if we need to switch the
  buffer being displayed, and flush anything remaining.
------------------------------------------------------------------------*/
{

  if(output) params.buf_ctl = UG_SW;
  output = FALSE;
  uflush();
  free(buffer);
#ifdef DEBUG
  printf("uclose... ");
#endif
  if(status = ugraf(UG_CLOSE,&params)) ubug();
#ifdef DEBUG
  printf("Done\n");
#endif
}
/************************************************************************/
void UCHAR(xmax,ymax,channels,levels)
int *xmax,*ymax,*channels,*levels;
/*
  Return characteristics about the Ultra Frame Buffer.
------------------------------------------------------------------------*/
{
  *xmax = 1280/*UG_MAXP*/;
  *ymax = UG_MAXL;
  *channels = 1;
  *levels = 256;
}
/************************************************************************/
void UOFM(red,green,blue)
int *red,*green,*blue;
/*
  This loads the internal LUTS.
------------------------------------------------------------------------*/
{
  int i;
  for(i=0; i<256; i++){
    lut_lo[i] = *red++ | (*green++ << 8) | (*blue++ << 16);
    lut_hi[i] = lut_lo[i] << 32;
  }
}
/************************************************************************/
void URESET()
/*
  This clears the Ultra frame buffer screen and sets the zoom and pan
  registers to 0.
------------------------------------------------------------------------*/
{
  int incr,i,j;
  int *s;

  incr = 2*BUFSIZE / UG_MAXP;
  s = buffer;
  for(i=0; i < BUFSIZE; i++) *s++ = 0;

  output = FALSE;

  tblock.tx = 0;
  tblock.npixel = UG_MAXP;
  for(j=0; j < 2; j++){
    for(i=0; i < UG_MAXL; i += incr){
      sign = 1;
      tblock.ty = i;
      tblock.nline = min(incr,UG_MAXL - i);
      if(i + tblock.nline == UG_MAXL) params.buf_ctl = UG_SW;
      uflush();
    }
  }
  params.dx = params.dy = 0;
  params.zx = params.zy = 1;
  params.zoom_set = params.pan_set = 1;
  uflush();
}
/************************************************************************/
void UVIEW(xmin,ymin,xmax,ymax)
int *xmin,*ymin,*xmax,*ymax;
/*
  Set the zoom and pan registers of the ultra frame buffer.
------------------------------------------------------------------------*/
{
  int x0,y0,x1,y1;
  int ugetzoom();

  if(output) params.buf_ctl = UG_SW;
  output = FALSE;

  x0 = *xmin & ~3;
  y0 = UG_MAXL - 1 - *ymax;
  x1 = *xmax;
  y1 = UG_MAXL - 1- *ymin;

  params.zx = ugetzoom(x0,x1,1280,UG_MAXP);
  params.zy = ugetzoom(y0,y1,1024,UG_MAXL);
  params.dx = min(x0,UG_MAXP - 1280/params.zx);
  params.dy = min(y0,UG_MAXL - 1024/params.zy);
  params.pan_set = params.zoom_set = 1;
}
/************************************************************************/
int ugetzoom(xy0,xy1,xyview,xymax)
int xy0,xy1,xyview,xymax;
/*
  Determine the zoom factor. Because we probably cannot find an exact fit,
  choose that zoom factor which minimises an error measure.
------------------------------------------------------------------------*/
{
  int err,zoom,t,i;

  err = xymax;
  zoom = 1;
  for(i=1; i <= 8; i *= 2){
    t = abs(xy1 - xy0 - xyview/i + 1);
    if(t < err){
      zoom = i;
      err = t;
    }
  }
  return(zoom);
}
/************************************************************************/
void ULINE(x0,y0,data,n0)
int *x0,*y0,*data,*n0;
/*
  Write a line to the screen.
  Input:
    x0		X coordinate where to write.
    y0		Y coordinate where to write.
    data	Data to write.
    n0		Number of pixels to write.
------------------------------------------------------------------------*/
{
  int *s;
  int i,x,y,n;

/* The start x coordinate and the number of pixels must be multiples of
   4. If they are not, skip some pixels so that they are. Also flip the
   y coordinate. */

  x = (*x0 + 3) & ~3;
  y = UG_MAXL - 1 - *y0;
  n = (*n0 - (x - *x0)) & ~3;
  data += (x - *x0);
  if(n <= 0) return;

/* By default, we assume that we are painting the screen, starting at the
   base and moving up. If this is not true, then reset things so that what
   we are doing is still done efficiently. */

  if(tblock.nline == 1 &&
     tblock.tx    == x &&
     tblock.ty+1  == y &&
     tblock.npixel== n){
    memwcpy(buffer,buffer+BUFSIZE-n/2,n/2);
    sign = 1;
  }

/* Check if we need to flush the buffer. */

  if(	   tblock.nline == 0
	|| x != tblock.tx
	|| y != tblock.ty + sign*tblock.nline
	|| n != tblock.npixel
	|| (tblock.nline+1) * tblock.npixel > 2*BUFSIZE){
    if(tblock.nline)uflush();
    tblock.tx = x;
    tblock.ty = y;
    tblock.npixel = n;
    tblock.nline = 0;
    sign = -1;
  }

/* Copy the stuff to the buffer. */

  output = TRUE;
  if(sign < 0)	s = buffer + BUFSIZE - (tblock.nline+1) * n / 2;
  else 		s = buffer + tblock.nline * n / 2;
  for(i = 0; i < n; i += 2) *s++ = lut_lo[*(data+i+1)] | lut_hi[*(data+i)];
  tblock.nline++;
}
/************************************************************************/
static void ubug()
/*
  Something has gone wrong with the ultra frame buffer (not again!).
  Give an error message, and abort.
------------------------------------------------------------------------*/
{
static char *messages[] = {
	"Device busy on open",
	"ug_param.term_type illegal",
	"ug_param.dx value illegal",
	"ug_param.dy value illegal",
	"ug_tblk.tx value illegal",
	"ug_tblk.ty value illegal",
	"ug_param.zx value illegal",
	"ug_param.zy value illegal",
	"no message Number 9, Number 9 ...",
	"operation attempted before open",
	"illegal function code",
	"close without recall on all requests",
	"open call when already open",
	"system io error detected",
	"wait error...p.intern invalid",
	"can't allocate copy buffers",
	"busy response to STATUS call",
	"program compiled with obsolete ugraf.h"};

  printf("### Ultra error: Errno = %d\n",errno);
  if(status >= 0 || status < -18)
    printf("### Fatal: Unrecognised Ultra error number %d\n",status);
  else
    printf("### Fatal Ultra error: %s\n",messages[-status-1]);
  printf("### Program exiting with return code = 1 ###\n");
  exit (1);
}
