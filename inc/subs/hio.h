#if !defined(MIR_HIO_H)
#define MIR_HIO_H

#include "sysdep.h"

/* 
 * MAXTYPES is pretty arbitrary, just make sure it's at least the last H_<type>+1
 *
 */

#define MAXTYPES	10
#define MAXPATH		256
#define MAXOPEN		26

#define H_BYTE_SIZE     1
#define H_INT_SIZE      4
#define H_INT2_SIZE     2
#define H_INT8_SIZE     8
#define H_REAL_SIZE     4
#define H_DBLE_SIZE     8
#define H_TXT_SIZE      1
#define H_CMPLX_SIZE    8

/* prototypes are now in miriad.h (mostly) and sysdep.h (pack routines)  */

/* Other handy definitions. */

#define max(a,b) 	((a)>(b)?(a):(b))
#define min(a,b) 	((a)<(b)?(a):(b))
#define mroundup(a,b)	((b)*(((a)+(b)-1)/(b)))

#endif /* MIR_HIO_H */
