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
  printf(" Sizeof char = %d\n", sizeof(char));  
  printf(" Sizeof short = %d\n", sizeof(short));  
  printf(" Sizeof int = %d\n", sizeof(int));  
  printf(" Sizeof long = %d\n", sizeof(long));
  printf(" Sizeof long long = %d\n", sizeof(long long));
  printf(" Sizeof *void = %d\n", sizeof((void*) &i));
  printf(" Sizeof off_t = %d\n", sizeof(off_t) );
  printf(" Sizeof size_t = %d\n", sizeof(size_t) );
  printf(" Sizeof ptrdiff_t = %d\n", sizeof(ptrdiff_t) );
  printf("\n");
  printf(" Sizeof float = %d\n", sizeof(float));
  printf(" Sizeof double = %d\n", sizeof(double));
  printf(" Sizeof long double = %d\n", sizeof(long double));
  printf("\n");

  return 0;
}
