/************************************************************************/
/*									*/
/*	Convert between FITS/IEEE and Cray format numbers.		*/
/*									*/
/*  The "FITS" numbers are the FITS orderings of 16 and 32 bit		*/
/*  integers. The IEEE formats are the formats for 32 and 64 bit	*/
/*  floating point numbers. Special case IEEE formats (NaN,		*/
/*  unnormalised, etc), are not handled.				*/
/*									*/
/*  Though the convention is to pass the array containing the foreign	*/
/*  formats are a pointer to char, the routines insist that the		*/
/*  foreign formats are partially aligned (i.e. 16 bit objects are on	*/
/*  even byte boundaries, 32 bit objects on 4 byte boundaries, and	*/
/*  64 bit objects must align with Cray words).				*/
/*									*/
/************************************************************************/

#define TWO15  0x8000
#define TWO16  0x10000
#define TWO31  0x80000000
#define TWO32  0x100000000
#define HILONG 0xFFFFFFFF00000000
#define LOLONG 0x00000000FFFFFFFF
#define WORD0  0x000000000000FFFF
#define WORD1  0x00000000FFFF0000
#define WORD2  0x0000FFFF00000000
#define WORD3  0xFFFF000000000000

/* Masks for IEEE floating format (both hi and lo types). */

#define IEEE_HISIGN	0x8000000000000000
#define IEEE_HIEXPO	0x7F80000000000000
#define IEEE_HIMANT	0x007FFFFF00000000
#define IEEE_LOSIGN	0x0000000080000000
#define IEEE_LOEXPO	0x000000007F800000
#define IEEE_LOMANT	0x00000000007FFFFF
#define IEEE_DMANT	0x000FFFFFFFFFFFF0
#define IEEE_DEXPO	0x7FF0000000000000

/* Masks for Cray floating format. */

#define CRAY_MANT	0x0000FFFFFF000000	/* Including unhidden bit. */
#define CRAY_MANT1	0x00007FFFFF000000	/* No unhidden bit. */
#define CRAY_DMANT	0x0000FFFFFFFFFFFF
#define CRAY_DMANT1	0x00007FFFFFFFFFFF
#define CRAY_EXPO	0x7FFF000000000000
#define SIGN		0x8000000000000000

/* Mask of a pointer to char giving the character offset in a Cray word. */

#define CHAR_OFFSET 0xE000000000000000

/************************************************************************/
void pack16_c(in,out,n)
char *out;
int *in,n;
/*
  Pack an integer array into 16 bit integers.
------------------------------------------------------------------------*/
{
  int temp,offset,*outd,in1,in2,in3,in4,i;

  if(n <= 0)return;				/* Return if nothing to do. */
  temp = (int)out;
  offset = ( temp & CHAR_OFFSET ) >> 62;	/* Get offset of first word. */
  outd = (int *)(temp & ~CHAR_OFFSET);	/* Get address of words. */

/* Handle the first few which are not aligned on a Cray word. */

  switch(offset){
    case 1: *outd = (*outd & ~WORD2) | ((*in++ << 32) & WORD2);
	    if(--n == 0)break;
    case 2: *outd = (*outd & ~WORD1) | ((*in++ << 16) & WORD1);
	    if(--n == 0)break;
    case 3: *outd = (*outd & ~WORD0) | ((*in++    ) & WORD0);
	    outd++;
  }

/* Handle the ones which are aligned on a Cray word. */

  for(i=0; i < n-3; i=i+4){
    in1 = *in++ << 48;
    in2 = *in++ << 32;
    in3 = *in++ << 16;
    in4 = *in++;
    *outd++ = (in1 & WORD3) | (in2 & WORD2) | (in3 & WORD1) | (in4 & WORD0);
  }
  n -= i;

/* Handle the last ones which are not aligned on a Cray word. */

  if(n-- > 0){
    *outd = (*outd & ~WORD3) | ((*in++ << 48) & WORD3);
    if(n-- > 0){
      *outd = (*outd & ~WORD2) | ((*in++ << 32) & WORD2);
      if(n-- > 0){
	*outd = (*outd & ~WORD1) | ((*in++ << 16) & WORD1);
      }
    }
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
  int temp,offset,*ind,i;

  if(n <= 0)return;				/* Return if nothing to do. */
  temp = (int)in;
  offset = ( temp & CHAR_OFFSET ) >> 62;	/* Get offset of first word. */
  ind = (int *)(temp & ~CHAR_OFFSET);	/* Get address of words. */

/* Handle the first few which are not word aligned. */

  switch(offset){
    case 1:  temp = (*ind >> 32) & WORD0;
	     *out++ = (temp < TWO15 ? temp : temp - TWO16);
	     if(--n == 0) break;
    case 2:  temp = (*ind >> 16) & WORD0;
	     *out++ = (temp < TWO15 ? temp : temp - TWO16);
	     if(--n == 0) break;
    case 3:  temp = (*ind++    ) & WORD0;
	     *out++ = (temp < TWO15 ? temp : temp - TWO16);
	     if(--n == 0) break;
  }

/* Handle those that are Cray-word-aligned. */

  for(i=0; i < n-3; i=i+4){
    temp = (*ind >> 48) & WORD0;
    *out++ = (temp < TWO15 ? temp : temp - TWO16);
    temp = (*ind >> 32) & WORD0;
    *out++ = (temp < TWO15 ? temp : temp - TWO16);
    temp = (*ind >> 16) & WORD0;
    *out++ = (temp < TWO15 ? temp : temp - TWO16);
    temp = (*ind++    ) & WORD0;
    *out++ = (temp < TWO15 ? temp : temp - TWO16);
  }
  n -= i;

/* Handle the last few which are not Cray-word-aligned. */

  if(n-- > 0){
    temp = (*ind >> 48) & WORD0;
    *out++ = (temp < TWO15 ? temp : temp - TWO16);
    if(n-- > 0){
      temp = (*ind >> 32) & WORD0;
      *out++ = (temp < TWO15 ? temp : temp - TWO16);
      if(n-- > 0){
	temp = (*ind >> 16) & WORD0;
	*out++ = (temp < TWO15 ? temp : temp - TWO16);
      }
    }
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
  int temp,offset,*outd,i,in1,in2;

  if(n <= 0)return;				/* Return if nothing to do. */
  temp = (int)out;
  offset = ( temp & CHAR_OFFSET ) >> 63;	/* Get offset of first long. */
  outd = (int *)(temp & ~CHAR_OFFSET);	/* Get address of words. */

/* Do the first one, if it is not aligned on a Cray word. */

  if(offset==1){
    *outd = (*outd & ~LOLONG) | (*in++ & LOLONG);
    outd++;
  }
  n -= offset;

/* Do those which are Cray word aligned. */

  for(i=0; i < n-1; i=i+2){
    in1 = *in++ << 32;
    in2 = *in++;
    *outd++ = (in1 & HILONG) | (in2 & LOLONG);
  }
  n -= i;

/* Handle the last one, if there is one. */

  if(n==1)*outd =  (*outd & ~HILONG) | ((*in++ << 32) & HILONG);
}
/************************************************************************/
void unpack32_c(in,out,n)
int *out,n;
char *in;
/*
  Unpack an array of 32 bit integers into integers.
------------------------------------------------------------------------*/
{
  int temp,offset,*ind,i;

  if(n <= 0)return;				/* Return if nothing to do. */
  temp = (int)in;
  offset = ( temp & CHAR_OFFSET ) >> 63;	/* Get offset of first word. */
  ind = (int *)(temp & ~CHAR_OFFSET);	/* Get address of words. */

/* Handle one which is not Cray word aligned. */

  if(offset==1){
    temp = (*ind++ & LOLONG);
    *out++ = (temp < TWO31 ? temp : temp - TWO32);
  }
  n -= offset;

/* Handle those which are Cray word aligned. */

  for(i=0; i < n-1; i=i+2){
    temp = (*ind >> 32) & LOLONG;
    *out++ = (temp < TWO31 ? temp : temp - TWO32);
    temp = (*ind++    ) & LOLONG;
    *out++ = (temp < TWO31 ? temp : temp - TWO32);
  }
  n -= i;

/* Possibly handle a last one which is not Cray word aligned. */

  if(n==1){
    temp = (*ind >> 32) & LOLONG;
    *out++ = (temp < TWO31 ? temp : temp - TWO32);
  }
}
/************************************************************************/
void packr_c(in,out,n)
int n;
float *in;
char *out;
/*
  Pack an array of Cray reals into IEEE reals.
------------------------------------------------------------------------*/
{
  int temp,offset,*outd,bias,*ind,tin,tout,i;

  if(n <= 0)return;				/* Return if nothing to do. */
  temp = (int)out;
  offset = ( temp & CHAR_OFFSET ) >> 63;	/* Get offset of first long. */
  outd = (int *)(temp & ~CHAR_OFFSET);	/* Get address of words. */
  bias = (16384 - 126) << 48;
  ind = (int *)in;

/* Do the first one, if it is not aligned on a Cray word. */

  if(offset==1){
    tin     = *ind++;
    *outd    =   (*outd & ~LOLONG) |
		 (tin & CRAY_MANT ? (((tin & CRAY_EXPO)-bias) >> 25) |
      		((tin & CRAY_MANT1) >> 24) | ((tin & SIGN) >> 32) : 0);
    outd++;
  }
  n -= offset;

/* Do those which are Cray word aligned. */

  for(i=0; i < n-1; i=i+2){
    tin = *ind++;
    tout =	(tin & CRAY_MANT ? (((tin & CRAY_EXPO)-bias) << 7) |
      		((tin & CRAY_MANT1) << 8) |   (tin & SIGN)        : 0);
    tin = *ind++;
    *outd++ =   tout | 
		(tin & CRAY_MANT ? (((tin & CRAY_EXPO)-bias) >> 25) |
      		((tin & CRAY_MANT1) >> 24) | ((tin & SIGN) >> 32) : 0);
  }
  n -= i;

/* Handle the last one, if there is one. */

  if(n==1){
    tin = *ind;
    *outd = (*outd & ~HILONG) | 
	    (tin & CRAY_MANT ? (((tin & CRAY_EXPO)-bias) << 7) |
      	    ((tin & CRAY_MANT1) << 8) |   (tin & SIGN)        : 0);
  }
}
/************************************************************************/
void unpackr_c(in,out,n)
char *in;
float *out;
int n;
/*
  Unpack an array of IEEE reals into Cray reals.
------------------------------------------------------------------------*/
{
  int temp,tin,*ind,*outd,offset,i,bias;

  if(n <= 0)return;				/* Return if nothing to do. */
  temp = (int)in;
  offset = ( temp & CHAR_OFFSET ) >> 63;	/* Get offset of first word. */
  ind = (int *)(temp & ~CHAR_OFFSET);	/* Get address of words. */
  outd = (int *)out;
  bias = ((16384-126) <<48) + (1 << 47);

/* Handle the first one if it is not aligned on a Cray word. */

  if(offset==1){
    tin = *ind++;
    *outd++ = (tin & IEEE_LOEXPO ? (((tin & IEEE_LOEXPO) << 25)+bias) |
	((tin & IEEE_LOMANT) << 24) | ((tin & IEEE_LOSIGN) << 32) : 0);
  }
  n -= offset;

/* Handle the bulk of them that are aligned on Cray words. */

  for(i=0; i < n-1; i=i+2){
    tin = *ind++;
    *outd++ = (tin & IEEE_HIEXPO ? (((tin & IEEE_HIEXPO) >> 7)+bias) |
	((tin & IEEE_HIMANT) >> 8 ) |  (tin & IEEE_HISIGN)        : 0);
    *outd++ = (tin & IEEE_LOEXPO ? (((tin & IEEE_LOEXPO) << 25)+bias) |
	((tin & IEEE_LOMANT) << 24) | ((tin & IEEE_LOSIGN) << 32) : 0);
  }
  n -= i;

/* Handle the last one, if needed, which is not aligned on a Cray word. */

  if(n==1){
    tin = *ind;
    *outd++ = (tin & IEEE_HIEXPO ? (((tin & IEEE_HIEXPO) >> 7)+bias) |
	((tin & IEEE_HIMANT) >> 8 ) |  (tin & IEEE_HISIGN)        : 0);
  }
}
/************************************************************************/
void packd_c(in,out,n)
double *in;
char *out;
int n;
/*
  Pack an array of Cray reals into IEEE double precision. This assumes
  that a "double" and a "float" are identical.
------------------------------------------------------------------------*/
{
  int *ind,*outd,bias,i,tin;

  ind = (int *)in;
  outd = (int *)out;
  bias = (16384 - 1022) << 48;

  for(i=0; i < n; i++){
    tin = *ind++;
    *outd++ = (tin & CRAY_DMANT ? (tin & SIGN) |
      (((tin & CRAY_EXPO)-bias) << 4) | ((tin & CRAY_DMANT1) << 5) : 0 );
  }
}
/************************************************************************/
void unpackd_c(in,out,n)
char *in;
double *out;
int n;
/*
  Unpack an array of IEEE double precision numbers into Cray reals. This
  assumes that a "double" and a "float" are identical.
------------------------------------------------------------------------*/
{
  int *ind,*outd,bias,i,tin;

  ind = (int *)in;
  outd = (int *)out;
  bias = ((16384 - 1022) << 48) | (1 << 47);

  for(i=0; i < n; i++){
    tin = *ind++;
    *outd++ = (tin & IEEE_DEXPO ? (tin & SIGN) |
      (((tin & IEEE_DEXPO) >> 4) + bias) | ((tin & IEEE_DMANT) >> 5) : 0 );
  }
}
