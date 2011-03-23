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
*
*  $Id: bug.c,v 1.4 2011/03/23 01:48:39 cal103 Exp $
*===========================================================================*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "miriad.h"
#include "sysdep.h"

private char *errmsg_c();

char *Name = NULL;
int reentrant=0;

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

private char *errmsg_c(n)
int n;
/*
  Return the error message associated with some error number.
------------------------------------------------------------------------*/
{
#ifdef HAS_STRERROR
  return(strerror(n));
#else
  static char string[128];
# if !defined(linux) && !defined(linux64) && !defined(darwin_ppc) && !defined(darwin_x86) && !defined(darwin_x86_64)
  extern int sys_nerr;
  extern char *sys_errlist[];
# endif
  if (n > 0 && n <= sys_nerr) {
    return (char *)sys_errlist[n];
  } else {
    sprintf(string,"Unknown error with number %d detected.",n);
    return string;
  }
#endif
}
