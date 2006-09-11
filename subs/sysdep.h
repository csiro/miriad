/*
 *  History:
 *    pjt 31oct89 _trace_ added as defined() option, BUFALIGN 8.
 *    rjs 21feb90 Added alternate way of defining FORT_TRUE and FALSE
 *		  to improve XMP and Cray-2 compatibility. This change
 *		  care of Brian Glendenning.
 *    mjs  ??     Increased BUFSIZE to avoid apparent OS problem on "ral".
 *    rjs  8feb91 Convex definitions.
 *    mjs 18mar91 More Convex definitions.
 *    rjs 16apr91 Removed macros redefining memcpy as bcopy -- no longer
 *		  needed, and bcopy is slower on the Suns anyway.
 *    rjs 24jun91 Added memcmp define for the convex.
 *    rjs 18dec92 Added hpux. Various tidying.
 *    mjs 19feb93 Added mips.
 *     jm 07nov94 Added definition of Null and typedef of Void.  The
 *                Void typedef permits proper casting in both ANSI
 *                and non-ANSI archs.  Also added definition to permit
 *                the use of const in non-ANSI declarations.
 *     jm 17nov94 Changed the conditional definition around the typedef
 *                of Void because Sun defines __STDC__ even when it is
 *                zero!  Defined PROTOTYPE as 1 if __STDC__ is set to 1;
 *                otherwise it is undefined.  Also added ARGS definition
 *                to aide forward declartion prototyping.
 *    rjs 20nov94 Added "alpha" ifdef.
 *    rjs 19mar97 Add FORTRAN_LOGICAL define and check that miriad.h declarations
 *		  have not been done before doing them again.
 *    pjt 14jun01 use WORDS_BIGENDIAN to figure out the pack routines
 *                removed 'trace' clutter from the old multiflow
 *    pjt 24jun01 PPC/powerpc is a BIGENDIAN (linux) machine
 *    pjt 21jun02 MIR4
 *    rjs 01jan05 Tidy up
 */

#if !defined(MIR_SYSDEP_H)
#define MIR_SYSDEP_H

/************************************************************************/
/*                                                                      */
/*                      VMS definitions.                                */
/*                                                                      */
/************************************************************************/

#ifdef vms
#define FORT_TRUE -1
#define FORT_FALSE 0
#define FORT_LOGICAL(a) (0x01 & (a))
#define BUFDBUFF 1
#define BUFALIGN 512
#define BUFSIZE 16384
#define defined_params
#endif

/************************************************************************/
/*									*/
/*			UNICOS definitions				*/
/*									*/
/************************************************************************/

#ifdef unicos
#include <fortran.h>
#define FORT_TRUE  _btol(1)
#define FORT_FALSE _btol(0)
#define FORT_LOGICAL(a) (_ltob((&(a))))
#define BUFDBUFF 0
#define BUFALIGN 8
#define BUFSIZE 16384
#define defined_params
#endif

/************************************************************************/
/*									*/
/*			UNIX definitions.				*/
/*									*/
/************************************************************************/

#ifndef defined_params
#if defined(convex) || defined(alpha) || defined(__alpha)
#  define FORT_TRUE  -1
#else
#  define FORT_TRUE 1
#endif

#define FORT_FALSE 0
#define FORT_LOGICAL(a) ((a) != FORT_FALSE)

#define BUFDBUFF 0
#define BUFALIGN 2
#define BUFSIZE 16384

/* Some machines have the "strerror" routine. Linux whinges significantly
   if you use the "old" way of doing effectively what strerror does. */

#if defined(linux)
#  define HAS_STRERROR
#endif

/*  Short cut routines when no conversion is necessary. These are
    used for any IEEE floating point machine with big endian (same as FITS)
    ordered bytes.	

*/

void pack16_c(int *in, char *out, int n);
void unpack16_c(char *in, int *out, int n);
void pack64_c(int8 *in, char *out, int n);
void unpack64_c(char *in, int8 *out, int n);

# if defined (sun) || defined(alliant) || defined(trace) || defined (convex) || defined (mips) || defined(sgi) || defined(hpux) || defined(powerpc)

#  define packr_c(a,b,c)    memcpy((b),(char *)(a),sizeof(float)*(c))
#  define unpackr_c(a,b,c)  memcpy((char *)(b),(a),sizeof(float)*(c))
#  define packd_c(a,b,c)    memcpy((b),(char *)(a),sizeof(double)*(c))
#  define unpackd_c(a,b,c)  memcpy((char *)(b),(a),sizeof(double)*(c))
#  define pack32_c(a,b,c)   memcpy((b),(char *)(a),sizeof(int)*(c))
#  define unpack32_c(a,b,c) memcpy((char *)(b),(a),sizeof(int)*(c))

#else

void pack32_c(int *in, char *out, int n);
void unpack32_c(char *in, int *out, int n);
void packr_c(float *in, char *out, int n);
void unpackr_c(char *in, float *out, int n);
void packd_c(double *in, char *out, int n);
void unpackd_c(char *in, double *out, int n);

#endif
#endif

#endif /* MIR_SYSDEP_H */
