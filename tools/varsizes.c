/*============================================================================
*  Simple program to return variable sizes used on your platform.
*  To build: cc -o varsizes varsizes.c
*
*  History 
*   31mar05 Chris Phillips Original version.
*   02apr09 rjs		   Added several additional data types.
*
*  $Id: varsizes.c,v 1.4 2009/04/14 03:59:45 cal103 Exp $
*===========================================================================*/

#include <stddef.h>
#include <stdio.h>
#include <sys/types.h>

int main (void)
{
  int i;

  printf("System variable sizes (in bytes)\n\n");
  printf(" Sizeof char = %lu\n", sizeof(char));  
  printf(" Sizeof short = %lu\n", sizeof(short));  
  printf(" Sizeof int = %lu\n", sizeof(int));  
  printf(" Sizeof long = %lu\n", sizeof(long));
  printf(" Sizeof long long = %lu\n", sizeof(long long));
  printf(" Sizeof *void = %lu\n", sizeof((void*) &i));
  printf(" Sizeof off_t = %lu\n", sizeof(off_t) );
  printf(" Sizeof size_t = %lu\n", sizeof(size_t) );
  printf(" Sizeof ptrdiff_t = %lu\n", sizeof(ptrdiff_t) );
  printf("\n");
  printf(" Sizeof float = %lu\n", sizeof(float));
  printf(" Sizeof double = %lu\n", sizeof(double));
  printf(" Sizeof long double = %lu\n", sizeof(long double));
  printf("\n");

  return 0;
}
