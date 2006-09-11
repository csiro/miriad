/************************************************************************/
/*									*/
/*  The pack routines -- these convert between the host format and	*/
/*  the disk format. Disk format is IEEE 32 and 64 bit reals, and 2's	*/
/*  complement integers. Byte order is the FITS byte order (most	*/
/*  significant bytes first).						*/
/*									*/
/*  This version is for a machine which uses IEEE internally, but which	*/
/*  uses least significant bytes first (little endian), e.g. PCs and	*/
/*  Alphas.								*/
/*									*/
/*  History:								*/
/*    rjs  21nov94 Original version.					*/
/*    rjs  02jan05 Added pack64 and unpack64.				*/
/************************************************************************/

#include "miriad.h"
#include "sysdep.h"

/************************************************************************/
void pack16_c(in,out,n)
char *out;
int *in,n;
/*
  Pack an integer array into 16 bit integers.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)in;
  for(i=0; i < n; i++){
    *out++ = *(s+1);
    *out++ = *s;
    s += sizeof(int);
  }
}
/************************************************************************/
void unpack16_c(in,out,n)
int *out,n;
char *in;
/*
  Unpack an array of 16 bit integers into integers.
------------------------------------------------------------------------*/
{
  int i;
  unsigned char *s;

  s = (char *)out;
  for(i=0; i < n; i++){
    *s++ = *(in+1);
    *s++ = *in;
    if(0x80 & *in){
      *s++ = 0xFF;
      *s++ = 0xFF;
    } else {
      *s++ = 0;
      *s++ = 0;
    }
    in += 2;
  }
}
/************************************************************************/
void pack32_c(in,out,n)
int *in,n;
char *out;
/*
  Pack an array of integers into 32 bit integers.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)in;
  for(i = 0; i < n; i++){
    *out++ = *(s+3);
    *out++ = *(s+2);
    *out++ = *(s+1);
    *out++ = *s;
    s += 4;
  }
}
/************************************************************************/
void unpack32_c(in,out,n)
int *out,n;
char *in;
/*
  Unpack an array of 32 bit integers into integers.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)out;
  for(i = 0; i < n; i++){
    *s++ = *(in+3);
    *s++ = *(in+2);
    *s++ = *(in+1);
    *s++ = *in;
    in += 4;
  }
}
/************************************************************************/
void packr_c(in,out,n)
int n;
float *in;
char *out;
/*
  Pack an array of reals into IEEE reals -- just do byte reversal.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)in;
  for(i = 0; i < n; i++){
    *out++ = *(s+3);
    *out++ = *(s+2);
    *out++ = *(s+1);
    *out++ = *s;
    s += 4;
  }
}
/************************************************************************/
void unpackr_c(in,out,n)
char *in;
float *out;
int n;
/*
  Unpack an array of IEEE reals into reals -- just do byte reversal.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)out;
  for(i = 0; i < n; i++){
    *s++ = *(in+3);
    *s++ = *(in+2);
    *s++ = *(in+1);
    *s++ = *in;
    in += 4;
  }
}
/************************************************************************/
void packd_c(in,out,n)
double *in;
char *out;
int n;
/*
  Pack an array of doubles -- this involves simply performing byte
  reversal.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)in;
  for(i = 0; i < n; i++){
    *out++ = *(s+7);
    *out++ = *(s+6);
    *out++ = *(s+5);
    *out++ = *(s+4);
    *out++ = *(s+3);
    *out++ = *(s+2);
    *out++ = *(s+1);
    *out++ = *s;
    s += 8;
  }
}
/************************************************************************/
void unpackd_c(in,out,n)
char *in;
double *out;
int n;
/*
  Unpack an array of doubles -- this involves simply performing byte
  reversal.
------------------------------------------------------------------------*/
{
  int i;
  char *s;

  s = (char *)out;
  for(i = 0; i < n; i++){
    *s++ = *(in+7);
    *s++ = *(in+6);
    *s++ = *(in+5);
    *s++ = *(in+4);
    *s++ = *(in+3);
    *s++ = *(in+2);
    *s++ = *(in+1);
    *s++ = *in;
    in += 8;
  }
}

/************************************************************************/
void pack64_c(from,to,n)
char *to;
int8 *from;
int n;
/*
  Pack int8's into 64 bit integers.

  Input:
    from	Array of int8 to pack.
    n		Number to pack.
  Output:
    to		Output array of 64-bit integers.
------------------------------------------------------------------------*/
{
  char *s;
  int i;
  if(sizeof(int8) == 8){
    s = (char *)from;
    for(i=0; i < n; i++){
      *to++ = *(s+7);
      *to++ = *(s+6);
      *to++ = *(s+5);
      *to++ = *(s+4);
      *to++ = *(s+3);
      *to++ = *(s+2);
      *to++ = *(s+1);
      *to++ = *s;
      s += 8;
    }
  }else if(sizeof(int8) == 4){
    s = (char *)from;
    for(i=0; i < n; i++){
      *to++ = 0;
      *to++ = 0;
      *to++ = 0;
      *to++ = 0;
      *to++ = *(s+3);
      *to++ = *(s+2);
      *to++ = *(s+1);
      *to++ = *s;
      s += 4;
    }
  }else{
    bug_c('f',"Unsupported size of int8 variables in pack64_c");
  }
}
/************************************************************************/
void unpack64_c(from,to,n)
char *from;
int8 *to ;
int n;
/*
  Unpack an array of 64 bit integers into the host int8 format.

  Input:
    from	Array of 64 bit integers.
    n		Number of integers to convert.
  Output:
    to		Array of host int8s.
------------------------------------------------------------------------*/
{
  char *s;
  int i;
  if(sizeof(int8) == 8){
    s = (char *)to;
    for(i=0; i < n; i++){
      *s++ = *(from+7);
      *s++ = *(from+6);
      *s++ = *(from+5);
      *s++ = *(from+4);
      *s++ = *(from+3);
      *s++ = *(from+2);
      *s++ = *(from+1);
      *s++ = *from;
      from += 8;
    }
  }else if(sizeof(int8) == 4){
    s = (char *)to;
    for(i=0; i < n; i++){
      if(*(from+7) != 0 || *(from+6) != 0 || *(from+5) != 0 || *(from+4) != 0)
	bug_c('f',"Overflow in unpack64_c when converting from 64 to 32 bit integer");
      *s++ = *(from+3);
      *s++ = *(from+2);
      *s++ = *(from+1);
      *s++ = *from;
      from += 8;
    }
  }else{
    bug_c('f',"Unsupported size of int8 variables in unpack64_c");
  }
}
