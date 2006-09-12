#include <stdio.h>

/************************************************************************/
main(argc,argv)
int argc;
char *argv[];
{
  int indent,c,i;
  char *infile,*outfile,*s;
  FILE *in,*out;

  outfile = infile = NULL; 
  for(i=1;i<argc;i++){
    s = argv[i];
    if(infile == NULL) infile = s;
    else if(outfile == NULL ) outfile = s;
    else fprintf(stderr,"### Argument %s ignored\n",s);
  }

/*
 * Open the input and output files. If not file is present, assume
 * standard input and output.
 */

  if(infile == NULL) in = stdin;
  else in = fopen(infile,"r");
  if(in==NULL){
    fprintf(stderr,"### Failed to open input file\n");
    exit(2);
  }
  if(outfile == NULL) out = stdout;
  else out = fopen(outfile,"w");
  if(out==NULL){
    fprintf(stderr,"### Failed to open output file\n");
    exit(2);
  }

/*
 * Perform the copy operation.
 */

  indent = 0;
  while((c=getc(in)) != EOF){
    if(c == '\n'){
      (void)putc(c,out);
      indent = 0;
    }else if(c == '\t'){
      do (void)putc(' ',out);
      while((++indent)%8);
    }else{
      (void)putc(c,out);
      indent++;
    }
  }

/*
 * Close up shop.
 */

  (void)fclose(in);
  (void)fclose(out);
}
