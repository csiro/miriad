/************************************************************************/
/*									*/
/*	Routines to pretend to be a hard copy display device.		*/
/*	The display for the TV device is buffered in memory.		*/
/*	When the TV is nominally flushed or closed, an image		*/
/*	is written to disk.						*/
/*									*/
/*  History:								*/
/*    rjs  20sep89  Original version.					*/
/************************************************************************/

#include <fcntl.h>

#define min(a,b) ((a) < (b) ? (a) : (b))
#define max(a,b) ((a) < (b) ? (b) : (a))

typedef int STRING;
#ifdef cray2
#define addr(string)     ((char *)(string & 0xE0000007FFFFFFFF))
#define len(first,string,last,n) ((string & 0x1FFFFFF800000000) >> 35 )
#else
#define addr(string)     ((char *)(string & 0xE0000000FFFFFFFF))
#define len(first,string,last,n) ((string & 0x1FFFFFFF00000000) >> 32 )
#endif

#define MAXNAME 128
#define NPIXELS 1024
#define NULL 0

char basename[MAXNAME];
char *buffer;
int xmin,xmax,ymin,ymax,fixed,sequence,palseq;

char *malloc();
/************************************************************************/
TVFINIT(name,dofixed)
STRING name;
int *dofixed;
{
  int length,i;
  char *s,*t;

/* Copy the base name. */

  length = min(len(name,name,dofixed,1),MAXNAME-10);
  s = addr(name) + length;
  while(length && *--s <= ' ')length--;
  s = addr(name);
  t = basename;
  for(i=0; i<length; i++)*t++ = *s++;
  *t = 0;

/* Set other parameters. */

  palseq = sequence = 0;
  fixed = *dofixed;
  buffer = malloc(NPIXELS*NPIXELS);
  if(buffer == NULL){perror("TVFinit"); exit(1);}

/* Clear the image plane. */

  TVFRESET();
}
/************************************************************************/
TVFRESET()
/*
  Nominally clear the TV screen.
------------------------------------------------------------------------*/
{
  memset(buffer,0,NPIXELS*NPIXELS);
  xmin = ymin = NPIXELS-1;
  xmax = ymax = 0;
}
/************************************************************************/
TVFOFM(red,blue,green)
int *red,*blue,*green;
/*
  Flush out a lookup table.
------------------------------------------------------------------------*/
{
  char table[768],name[MAXNAME];
  int i,fd;
  char *t;
  int *s;

/* Open the output file. */

    sprintf(name,"%s.pal.%d",basename,++palseq);
    fd = open(name,O_CREAT|O_TRUNC|O_RDWR,0644);
    if(fd < 0){perror("TvLut"); exit(1);}

/* Copy the luts to the output buffer. */

  t = table;
  s = red;
  for(i=0; i<256; i++) *t++ = *s++;
  s = blue;
  for(i=0; i<256; i++) *t++ = *s++;
  s = green;
  for(i=0; i<256; i++) *t++ = *s++;

  write(fd,table,768);
  close(fd);
}
/************************************************************************/
TVFFLUSH()
{
/*
  Nominally flush the TV buffers. In reality, this writes the current
  version of the screen buffer to a file.
------------------------------------------------------------------------*/
  int fd,i,nx,ny;
  char name[MAXNAME],*s;

  if(xmin <=xmax && ymin <= ymax){
    if(fixed){
      nx = ny = NPIXELS;
      s = buffer;
    } else {
      nx = xmax - xmin + 1; ny = ymax - ymin + 1;
      s = buffer + ymin*NPIXELS + xmin;
    }
    sprintf(name,"%s.%d",basename,++sequence);

/* Open the output file. */

    fd = open(name,O_CREAT|O_TRUNC|O_RDWR,0644);
    if(fd < 0){perror("TvFlush"); exit(1);}

/* Write out the pixel data. */

    if(nx == NPIXELS)write(fd,s,nx*ny);
    else for(i=0; i<ny; i++){
      write(fd,s,nx);
      s += NPIXELS;
    }

/* Close it up. */

    close(fd);

  }
}
/************************************************************************/
TVFCLOSE()
{
  TVFLUSH();
  free(buffer);
}
/************************************************************************/
TVFLINE(x,y,data,n)
int *x,*y,*data,*n;
/*
  TVFLINE copies data to a buffer, and keeps track of the region of the
  buffer that has been written to.
------------------------------------------------------------------------*/
{
  int x0,y0,n0,i;
  int *s;
  char *t;

  if(*y < 0 || *y >= NPIXELS)return;
  y0 = NPIXELS - 1 - *y;
  x0 = max(0,*x); n0 = min(*x + *n,NPIXELS) - x0;
  if(n0 <= 0) return;
  s = data + x0 - *x;
  t = buffer + y0*NPIXELS + x0;

  for(i=0; i<n0; i++) *t++ = *s++;

/* Update the region where we think we have data. */

  xmin = min(xmin,x0);
  xmax = max(xmax,x0+n0-1);
  ymin = min(ymin,y0);
  ymax = max(ymax,y0);
}
