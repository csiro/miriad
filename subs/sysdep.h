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
 *    rjs 20nov94 Added alphas.
 */

#ifndef Null
#define Null '\0'
#endif

#ifdef __STDC__
typedef void Void;
#else
typedef char Void;
#define const /* NULL */
#endif /*__STDC__*/

typedef int int2;

/************************************************************************/
/*									*/
/*			VMS definitions.				*/
/*									*/
/************************************************************************/

#ifdef vaxc
#define FORT_TRUE -1
#define FORT_FALSE 0
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
#if defined(convex) || defined(alpha)
#  define FORT_TRUE  -1
#else
#  define FORT_TRUE 1
#endif
#define FORT_FALSE 0

#define BUFDBUFF 0

#if defined(trace)
#  define BUFALIGN 8
#else
#  define BUFALIGN 2
#endif

#define BUFSIZE 16384

/* The Multiflow machine does not have the memcpy routine. Use bcopy
   instead.								*/

#if defined(trace)
#  define memcpy(a,b,c) bcopy((b),(a),(c))
#  define memcmp(a,b,c) bcmp((a),(b),(c))
#endif

/*  Short cut routines when no conversion is necessary. These are
    used for any IEEE floating point machine with FITS ordered bytes.	*/

#if defined(sun) || defined(alliant) || defined(trace) || defined(convex) || defined(hpux) || defined(mips)
#  define packr_c(a,b,c)    memcpy((b),(char *)(a),sizeof(float)*(c))
#  define unpackr_c(a,b,c)  memcpy((char *)(b),(a),sizeof(float)*(c))
#  define packd_c(a,b,c)    memcpy((b),(char *)(a),sizeof(double)*(c))
#  define unpackd_c(a,b,c)  memcpy((char *)(b),(a),sizeof(double)*(c))
#  define pack32_c(a,b,c)   memcpy((b),(char *)(a),sizeof(int)*(c))
#  define unpack32_c(a,b,c) memcpy((char *)(b),(a),sizeof(int)*(c))
#endif
#endif

