/* Simple program to return variable sizes used on your platform.
   To build:  cc -o varsizes varsizes.c
   Author: Chris.Phillips@csiro.au
   Date: 2005-03-31
*/
int main (void) {

  int i;

  printf("System variable sizes (in bytes)\n\n");
  printf(" Sizeof char = %d\n", sizeof(char));  
  printf(" Sizeof short = %d\n", sizeof(short));  
  printf(" Sizeof int = %d\n", sizeof(int));  
  printf(" Sizeof long = %d\n", sizeof(long));
  printf(" Sizeof long long = %d\n", sizeof(long long));
  printf(" Sizeof *void = %d\n", sizeof((void*) &i));
  printf("\n");
  printf(" Sizeof double = %d\n", sizeof(double));
  printf(" Sizeof long double = %d\n", sizeof(long double));
  printf("\n");

  return(0);
}
