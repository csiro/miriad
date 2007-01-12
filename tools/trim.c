/************************************************************************/
/*									*/
/*	This compacts a text file, by discarding unnecessary characters	*/
/*									*/
/*  History:								*/
/*    rjs  18sep89  Original version.					*/
/*    rjs  17feb04  Gosh is it really 15 years old! Return a status.    */
/*    rjs  30dec06  Simple handling of escape sequences.		*/
/*									*/
/* Operations performed include:					*/
/*	Trim off trailing  blanks.					*/
/*	Replace multiple spaces with tabs where possible.		*/
/*	Remove FORTRAN sequence numbers (-f flag).			*/
/*	Handle backspace and delete characters, escape sequences.	*/
/*	Delete blank lines (-b flag).					*/
/*									*/
/************************************************************************/

#define private static
#include <stdlib.h>
#include <stdio.h>
#define MAXLINE 512

#define TRUE 1
#define FALSE 0

private void process();

/************************************************************************/
int main(argc,argv)
int argc;
char *argv[];
{
  char *outfile,*s;
  int fort,blank,i,nfiles;
  FILE *outfd,*fin;

  nfiles = 0;
  outfile = NULL;
  fort = FALSE;
  blank = FALSE;
  for(i=1; i < argc; i++){
    s = argv[i];
    if(*s == '-'){
      while(*++s)switch(*s){
        case 'f': fort = TRUE; break;
        case 'b': blank   = TRUE; break;
        default: fprintf(stderr,"Unrecognised flag %c\n",*s);
      }
      argv[i] = 0;
    } else if(*s == '>') {
      argv[i++] = NULL;
      if(i < argc){
	outfile = argv[i];
	argv[i] = NULL;
      }
    } else nfiles++;
  }

/* Open the output file, if required. */

  if(outfile == NULL) outfd = stdout;
  else outfd = fopen(outfile,"w");
  if( outfd == NULL) { perror("open out"); exit(0); }

/* Process each of the input files. */

  if(nfiles == 0) process(outfd,stdin,fort,blank);
  else for(i=1; i<argc; i++){
    if(argv[i] != NULL){
      fin = fopen(argv[i],"r");
      if(fin == NULL) perror(argv[i]);
      else{
        process(outfd,fin,fort,blank);
        fclose(fin);
      }
    }
  }
 return(0);
}
/************************************************************************/
private void process(fout,fin,fort,blank)
FILE *fout,*fin;
int fort,blank;
/*
  Process an input file, and write it to the output.
  Inputs:
    fout	Stdio output file.
    fin		FILE* of the input.
------------------------------------------------------------------------*/
{
  char line[MAXLINE];
  char *s,*t;
  int dofort,dowrite,linelen0,linelen,mode;

#define mESC  1
#define mESCB 2

/* Process the input file. */

  while(fgets(line,MAXLINE,fin) != NULL){

/* Eliminate backspace and delete characters from the line, as well as
   any other rubbish. */

    mode = 0;
    t = line;
    for(s=line; *s; s++){
      if(mode == mESC && *s == '['){ mode = mESCB;}
      else if(mode == mESCB){
        if(!isdigit(*s))mode=0;
      }else{
	mode = 0;
        if(*s == '\b' || *s == '\177' ){ if(t > line) t--; }
        else if(*s == '\e'){mode = mESC;}
        else if((*s < ' ' && *s != '\t') || (*s > '\177'));
        else *t++ = *s;
      }
    }
    *t = 0;

/*  Determine if we want to perform a FORTRAN trimming of this line. */

    dofort = fort && line[0] <= ' ';

/* Go through, shortening the line. */

    linelen = linelen0 = 0;
    t = line;
    for(s=line; *s; s++){
      if(*s == '\t') linelen = 8*(linelen/8 + 1);
      else if(*s == ' ') linelen++;
      else if(dofort && linelen >= 72) break;
      else {
	if(linelen > linelen0){
	  if(linelen0 % 8 == 7){*t++ = ' '; linelen0++;}
	  while(8*(linelen0/8 + 1) <= linelen){
	    *t++ = '\t';
	    linelen0 = 8*(linelen0/8 + 1);
	  }
	  while(linelen0+1 <= linelen){
	    *t++ = ' ';
	    linelen0++;
	  }
	}
	*t++ = *s;
	linelen0 = ++linelen;
      }
    }

/* Trim back the line to the first non-blank character. */

    t--;
    while(t >= line && *t == ' ')t--;

/* Add a line feed and a zero terminating byte. */

    *(++t) = '\n'; *(++t) = 0;

/* Write out the line, if desired. */

    dowrite = line[0] != '\n' || !blank;
    if(dowrite) fputs(line,fout);
  }
}
