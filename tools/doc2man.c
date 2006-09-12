/************************************************************************/
/*= doc2man - Format Miriad *doc files as a man page			*/
/*& pjt 								*/
/*: tools								*/
/*+
  doc2man is a tool that formats a MIRIAD .doc file into an even less 
  readable form: the unix man format. However, with X windows utilities 
  like xman or hman, a much more superior online help system is created.
  Within miriad a script 'mir.man' or alias 'mirman' should be present
  to aid in starting up a windows based man browser.
  Optionally a section number can be passed to the output man file.

  Usage:
		doc2man [-section] file.doc > file.man
									*/
/*--									*/
/*
  History:
     6mar92 pjt  1.0 Original version (cloned off docfmt, after an idea by
                 Bobs) - Hey, it's Michelangelo's birthday today, and I'm
                 in Charlottesville, VA!
    23mar92 pjt  More formally installed in UMD: $MIR/local
    17nov92 pjt  merge PURPOSE into the NAME section, for whatis database
    11jan93 pjt  1.2 handle new %H hypertext directive (not implemented in
		 other doc systems
    21jul93 pjt  improved doc for release
    26jul93 pjt  merge PURPOSE and NAME for the whatis database - I've
		 seen to have done this before. Also added -section option
    
************************************************************************/

#define VERSION "Doc2man: version 1.3 26-jul-93"

#include <stdio.h>
#include <string.h>

#define private static
#define MAXLINE 128

#define TRUE 1
#define FALSE 0

private void process(), filling(), usage();
private char *skip(),*gline();


/************************************************************************/
main(argc,argv)
int argc;
char *argv[];
{
  char *outfile,*s,*keyword,*section="1";
  int i,nin;
  FILE *infd,*outfd;

  keyword = outfile = NULL;
  nin = 0;
  for(i=1; i < argc; i++){            /* Process the command line. */
    s = argv[i];
    if(*s == '-'){		            /* Flags and switches. */
      argv[i] = NULL;
      while(*++s)switch(*s){
	case '?': usage(); exit(0);
	case '0': 
	case '1': 
	case '2': 
	case '3': 
	case '4': 
	case '5': 
	case '6':
	case '7':
	case '8':
	case '9': section = s; break;
        default: fprintf(stderr,"Unrecognised flag %c\n",*s); usage(); exit(1);
      }
    } else if(*s == '>') {                      /* An output file */
      argv[i++] = NULL;
      if(i < argc){
	outfile = argv[i];
	argv[i] = NULL;
      }
    } else {                                    /* An input file. */
      nin++;
    }
  }

  if(outfile == NULL) outfd = stdout;/* Open output file, if req. */
  else outfd = fopen(outfile,"w");
  if( outfd == NULL) { perror("open out"); exit(1); }

  if(nin > 0){                /* Process each of the input files. */
    for(i=1; i<argc; i++) if(argv[i] != NULL){
      infd = fopen(argv[i],"r");
      if(infd == NULL) { perror("open in"); exit(1); }
      process(infd,outfd,keyword,section);
      fclose(infd);
    }
  } else {
    process(stdin,outfd,keyword);
  }
  return(0);
}
/**********************************************************************/
private void usage()
{
  fprintf(stderr,"%s\n",VERSION);
  fprintf(stderr,"This formats a Miriad .doc file into a man page\n\n");
  fprintf(stderr,"Usage:\n");
  fprintf(stderr,"doc2man [-?] [-section] infile > outfile\n");
  fprintf(stderr,"     (man) section must start with a digit. [Default: 1]\n");
}
/**********************************************************************/
private void process(fin,fout,keyword,section)
FILE *fin,*fout;
char *keyword;
char *section;
/*
  This takes a Miriad .doc file (FILE *fin), and writes to the output
  (FILE *fout) a man format version of it.

  Inputs:
    fin		File descriptor of the input.
    fout	File descriptor of the output.
    keyword     Ignored...... (see docfmt.c)
----------------------------------------------------------------------*/
{
  char line[MAXLINE],*s, *has_file;
  int inipar=0;
  int seen_name=0, seen_purpose=0;
  char name[MAXLINE], purpose[MAXLINE], files[MAXLINE];


/*
 * Process the input file.
 */

  files[0] = 0;
  while(gline(line,MAXLINE,fin) != NULL) {

/* 
 * If the line does not start with a percent, just write it out. Otherwise
 * do some processing. 
 */

    if(*line != '%') {
      filling(line,fout);
      if(strlen(line) > 0)
        fputs(line,fout);
      else
        fputs(".sp",fout);
      fputs("\n",fout);
    } else switch(line[1]){
      case 'N': 
	has_file = strchr(&line[1],'%');
	if (has_file) {
	    *has_file++ = 0;        /* patch the string locally */
            if (*has_file != 'F') { /* but check next character */
                fprintf(stderr,"### Warning: %%%c on %%N line?\n",*has_file);
                files[0] = 0;
            } else {
                has_file++;
                strcpy(files,has_file);
            }
	}
        fprintf(fout,".TH %s %s\n",skip(line),section);
        seen_name = 1;
        strcpy(name,skip(line));
        break;
      case 'D': 
        seen_purpose = 1;
        if (seen_name)
          fprintf(fout,".SH NAME\n%s - %s\n",name,skip(line));	
        else
          fprintf(stderr,"### doc2man: %%D before %%N seen\n");
        break;
      case ':': 
	if (!seen_purpose) seen_purpose++;
        fprintf(fout,".SH CATEGORIES\n%s\n",skip(line));break;
      case 'H':
	if (!seen_purpose) seen_purpose++;
	fprintf(fout,".SH SEE ALSO\n%s\n",skip(line));  break;
      case 'B': 
	if (!seen_purpose) seen_purpose++;
        fprintf(fout,".SH DESCRIPTION\n");		break;
      case 'P':
	if (!seen_purpose) seen_purpose++;
	fprintf(fout,".SH PERSON RESPONSIBLE\n%s\n",skip(line)); break;
      case 'F':
	if (!seen_purpose) seen_purpose++;
	fprintf(fout,".SH FILE\n%s\n",skip(line));      break;
      case 'A':
	if (!seen_purpose) seen_purpose++;
        if(inipar==0) {
            fprintf(fout,".SH PARAMETERS\n");
            inipar = 1;
        }
	s = skip(line);
	fprintf(fout,".TP\n\\fI%s\\fP\n",skip(line));  	break;
      default: 
	if (!seen_purpose) seen_purpose++;
        filling(line,fout);
        fputs(line,fout);
        fputs("\n",fout);
    }
  }
  if (seen_purpose > 1) fprintf(stderr,"### Warning: no purpose line\n");
  if (files[0]) fprintf(fout,".SH FILES\n%s\n",files);
}
/**********************************************************************/
private void filling(line,fout)
char *line;
FILE *fout;
{
    static int filling=0;

    if(line[0] == ' ' && filling==0) {
        fprintf(fout,".nf\n");
        filling=1;
    } else if (line[0] != ' ' && filling==1) {
        fprintf(fout,".fi\n");
        filling=0;
    }
}
/**********************************************************************/
private char *skip(line)
char *line;
/*
  This skips over the leading %, X and following blanks.
  Note it returns pointer to the private space given to the routine

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
/************************************************************************/
private char *gline(line,length,fd)
char *line;
int length;
FILE *fd;
{
  static int first=1;
  static int skip=0;
  char sline[MAXLINE],*s;
  int i;

  s =fgets(sline,length,fd);
  if(s == NULL)return(s);
  sline[strlen(sline)-1] = 0;    /* patch the newline, we will newline */
  if(sline[0] == ' ' && first){
    for(s = sline, skip = 0; *s == ' '; s++, skip++);
    first = 0;
  }
  for(s = sline, i=0; *s == ' ' && i < skip; s++, i++);
  strcpy(line,s);
  return(line);
}
