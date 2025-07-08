/*============================================================================
*
* This handles errors and can abort your program.
*
*  History:
*    rjs,mjs ????    Very mixed history. Created, destroyed, rewritten.
*    rjs     26aug93 Call habort_c.
*    rjs     14jul98 Add a caste operation in errmsg_c, to attempt
*                    to appease some compilers.
*    rjs      9aug02 Support for "darwin" OS.
*    rjs      3jul04 Use strerror routine.
*    pjt      1jan05 bugv_c: finally, a real stdargs version!!!
*                    though cannot be exported to Fortran
*    pkgw    14dec11 Make errmsg_c public for use in uvio.c
*
*  $Id: bug.c,v 1.7 2022/01/31 06:36:29 wie017 Exp $
*===========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "miriad.h"
#include "sysdep.h"

char *errmsg_c();

char *Name = NULL;
int reentrant=0;

#define MAXMSG 256
static char msg[MAXMSG];      /* formatted message for bugv_c()         */

/************************************************************************/

void buglabel_c(Const char *name)
/** buglabel -- Give the "program name" to be used as a label in messages. */
/*& mjs									*/
/*: error-handling							*/
/*+ FORTRAN call sequence:
      subroutine buglabel(name)
      character name*(*)

  Give the name that is to be used as a label in error messages.

  Input:
    name	The name to be given as a label in error messages.	*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  if(Name != NULL)free(Name);
  Name = malloc(strlen(name)+1);
  strcpy(Name,name);
}

/************************************************************************/

void bug_c(char s,Const char *m)
/** bug -- Issue an error message, given by the caller.			*/
/*& mjs									*/
/*: error-handling							*/
/*+ FORTRAN call sequence:
      subroutine bug(severity,message)
      character severity*1
      character message*(*)

  Output the error message given by the caller, and abort if needed.

  Input:
    severity	Error severity. Can be one of 'i', 'w', 'e' or 'f'
		for "informational", "warning", "error", or "fatal"
    message	The error message text.					*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  char *p;
  int doabort;

  doabort = 0;
  if (s == 'i' || s == 'I') {
    p = "Informational";
  } else if (s == 'w' || s == 'W') {
    p = "Warning";
  } else if (s == 'e' || s == 'E') {
    p = "Error";
  } else {
    p = "Fatal Error";
    doabort = 1;
  }

  fprintf(stderr,"### %s:  %s\n",p,m);
  if (doabort) {
    reentrant = !reentrant;
    if (reentrant) habort_c();
    exit(1);
  }
}
/************************************************************************/
void bugv_c(char s,Const char *m, ...)
/** bugv_c -- Issue a dynamic error message, given by the caller.	*/
/*& pjt									*/
/*: error-handling							*/
/*+ C call sequence:
	bugv_c(severity,message,....)

  Output the error message given by the caller, and abort if needed.
  Note this routine has no counterpart in FORTRAN.

  Input:
    severity	Error severity character.
                Can be one of 'i', 'w', 'e' or 'f'
		for "informational", "warning", "error", or "fatal"
    message	The error message string, can contain %-printf style
                directives, as used by the following arguments.
     ...         Optional argument, in the printf() style               */
/*--									*/
/*----------------------------------------------------------------------*/
{
  va_list ap;

  va_start(ap,m);
  vsnprintf(msg,MAXMSG,m,ap);
  msg[MAXMSG-1] = '\0'; /* backstop */
  va_end(ap);

  bug_c(s, msg);
}

/************************************************************************/

void bugno_c(char s,int n)
/** bugno -- Issue an error message, given a system error number.	*/
/*& mjs									*/
/*: error-handling							*/
/*+ FORTRAN call sequence:
      subroutine bugno(severity,errno)
      character severity*1
      integer errno

  Output the error message associated with a particular error number.

  Input:
    severity	Error severity. Can be one of 'i', 'w', 'e' or 'f'
		for "informational", "warning", "error", or "fatal"
    errno	host error number.					*/
/*--									*/
/*----------------------------------------------------------------------*/
{
  if (n == -1) {
    bug_c(s,"End of file detected");
  } else {
    bug_c(s,errmsg_c(n));
  }
}
/************************************************************************/
char *errmsg_c(int n)
/*
  Return the error message associated with some error number.
------------------------------------------------------------------------*/
{
#ifdef HAS_STRERROR
  return(strerror(n));
#else
  static char string[128];
/*
# if !defined(linux) && !defined(linux64) && !defined(darwin_ppc) && !defined(darwin_x86) && !defined(darwin_x86_64) && !defined(arm64)
  extern int sys_nerr;
  extern char *sys_errlist[];
# endif
*/
  if (n > 0 && n <= sys_nerr) {
    return (char *)sys_errlist[n];
  } else {
    sprintf(string,"Unknown error with number %d detected.",n);
    return string;
  }
#endif
}
