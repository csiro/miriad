/************************************************************************/
/*= docfmt - Format Miriad .doc files.					*/
/*& rjs									*/
/*: tools								*/
/*+
  docfmt formats a Miriad .doc file into a more readable form.
									*/
/*--									*/
/*
  History:
    25jun91 rjs	 Original version.
    16jul91 rjs  Changed output format to appease nebk, and added -k
		 switch.
     9aug93 rjs  Exit (rather than return) with 0 to appease VAXs.
************************************************************************/

#define VERSION "Docfmt: version 1.0 16-Jul-91"
#define private static
#include <stdio.h>
#define MAXLINE 128

#define TRUE 1
#define FALSE 0

private void process(),usage();
private char *skip();

/************************************************************************/
main(argc,argv)
int argc;
char *argv[];
{
  char *outfile,*s,*keyword;
  int i,nin;
  FILE *infd,*outfd;

/* Process the command line. */

  keyword = outfile = NULL;
  nin = 0;
  for(i=1; i < argc; i++){
    s = argv[i];

/* Flags and switches. */

    if(*s == '-'){
      argv[i] = NULL;
      while(*++s)switch(*s){
	case '?': usage(); exit(0);
	case 'k': if(i < argc-1){
		    keyword = argv[++i];
		    argv[i] = NULL;
		   }
		   break;
        default: fprintf(stderr,"Unrecognised flag %c\n",*s);
      }

/* An output file */

    } else if(*s == '>') {
      argv[i++] = NULL;
      if(i < argc){
	outfile = argv[i];
	argv[i] = NULL;
      }

/* An input file. */

    } else {
      nin++;
    }
  }

/* Open the output file, if required. */

  if(outfile == NULL) outfd = stdout;
  else outfd = fopen(outfile,"w");
  if( outfd == NULL) { perror("open out"); exit(1); }

/* Process each of the input files. */

  if(nin > 0){
    for(i=1; i<argc; i++) if(argv[i] != NULL){
      infd = fopen(argv[i],"r");
      if(infd == NULL) { perror("open in"); exit(1); }
      process(infd,outfd,keyword);
      fclose(infd);
    }
  } else {
    process(stdin,outfd,keyword);
  }
  exit(0);
}
/**********************************************************************/
private void usage()
{
  fprintf(stderr,"%s\n",VERSION);
  fprintf(stderr,"This formats a Miriad .doc file into a readable form\n\n");
  fprintf(stderr,"Usage:\n");
  fprintf(stderr,"docfmt [-?] [-k keyword] infile > outfile\n");
}
/**********************************************************************/
private void process(fin,fout,keyword)
FILE *fin,*fout;
char *keyword;
/*
  This takes a Miriad .doc file (FILE *fin), and writes to the output
  (FILE *fout) a format version of it.

  Inputs:
    fin		File descriptor of the input.
    fout	File descriptor of the output.
----------------------------------------------------------------------*/
{
  char line[MAXLINE],*s;
  int echo,length;

/* Process the input file. */

  echo = keyword == NULL;
  if(!echo)length = strlen(keyword);

  while(fgets(line,MAXLINE,fin) != NULL){

/* If the line does not start with a percent, just write it out. Otherwise
   do some processing. */

    if(!echo && strncmp(line,"%A",2));
    else if(*line != '%')fputs(line,fout);
    else switch(line[1]){
      case 'N': fprintf(fout,"\nTask: %s",skip(line));		break;
      case 'D': fprintf(fout,"Purpose: %s",skip(line));		break;
      case ':': fprintf(fout,"Categories: %s",skip(line));	break;
      case 'B': fprintf(fout,"\n");				break;
      case 'A':
	s = skip(line);
	echo = keyword == NULL;
	if(!echo)echo = !strncmp(s,keyword,length);
	if(echo)fprintf(fout,"\nKey: %s",skip(line));		break;
      case 'P':							break;
      default: fputs(line,fout);
    }
  }
}
/**********************************************************************/
private char *skip(line)
char *line;
/*
  This skips over the leading %x and blanks.
  Input:
    line	The input string.
  Output:
    skip	Points to the first non-blank character after the %x.
----------------------------------------------------------------------*/
{
  char *s;
  s = line + 2;
  while(*s == ' ' || *s == '\t')s++;
  return(s);
}
