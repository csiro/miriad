/************************************************************************/
/*= doc -- Strip preamble comments out of miriad source files		*/
/*& rjs									*/
/*: tools								*/
/*+
  doc strips preamble ("doc") comments out of Miriad source files,
  producing so-called ".doc" documentation files.

    Usage:
      doc [-O outdir] [-f] files ... [ > outfile ]

    where

      -O    Specify an output directory for the files.
      -f    Pipe the output to standard output, rather than creating
            output files.
      -x    Generate "extra" help files as well.
      -?    Give a brief usage message.
									*/
/*-									*/
/*
  History:
    Long ago rjs  Original version.
    02mar90  bpw  Full rewrite with extended functionality.
    19nov98  rjs  Full rewrite.
    21jul99  rjs  Improved indenting algorithm somewhat. Added X command.
     6feb00  rjs  Support for Perl.
************************************************************************/

#define VERSION "Doc: version 1.0 6-feb-00"
#define private static
#include <stdio.h>
#include <string.h>

#define MAXLINE 128

#define TRUE 1
#define FALSE 0

private void process(),usage(),getmodes(),getindent();
private int getline();
private char *skip();

/************************************************************************/
main(argc,argv)
int argc;
char *argv[];
{
  char *infile,*c1,*c2,*outfile,*s,*outdir;
  int i,nin,redir,doex;
  FILE *infd,*outfd;

/* Process the command line. */

  doex = 1;
  redir = 0;
  outdir  = NULL;
  outfile = NULL;
  nin = 0;
  for(i=1; i < argc; i++){
    s = argv[i];

/* Flags and switches. */

    if(*s == '-'){
      argv[i] = NULL;
      while(*++s)switch(*s){
	case '?': usage(); exit(0);
	case 'p': break;
	case 'x': doex = 0;  break;
	case 'f': redir = 1; break;
	case 'O':
	case 'o': if(++i < argc){outdir  = argv[i]; argv[i] = NULL;} break;
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

  if(redir){
    if(outfile == NULL) outfd = stdout;
    else outfd = fopen(outfile,"w");
    if( outfd == NULL) {fprintf(stderr,"### Error redirecting output to %s\n",outfile);exit(1); }
  } else outfd = NULL;

/* Process each of the input files. */

  if(nin > 0){
    for(i=1; i<argc; i++) if(argv[i] != NULL){
      getmodes(argv[i],&c1,&c2,&infile);
      infd = fopen(argv[i],"r");
      if(infd == NULL) { fprintf(stderr,"### Error opening %s\n",argv[i]); exit(1); }
      process(infd,outfd,c1,c2,infile,outdir,doex);
      fclose(infd);
    }
  }
  exit(0);
}
/**********************************************************************/
private void usage()
{
  printf("%s\n",VERSION);
  printf("This strips preamble comments from Miriad source files.\n\n");
  printf("Usage:\n");
  printf("doc [-?] [-f] [-O outdir] infile > outfile\n");
}
/**********************************************************************/
private void getmodes(name,s1,s2,infile)
char *name,**s1,**s2,**infile;
/*
  Determine the language type of the input file from its extension.
  Possible extensions are:
  Extension	Language	Comment Char
  ---------	--------	------------
    .c or .cc	 C or C++	Normal C comments.
    .f or .for	 FORTRAN	C  or c
    .csh,.sh,.pl Shells		#
    .com	 VMS DCL	$!
----------------------------------------------------------------------*/
{
#define NXT 9
  static char *exts[] = {"c","cc","C","f","for","csh","sh","pl","com"};
  static char *c1[]   = {"/*","/*","/*","C","C","#","#","#","$!"};
  static char *c2[]   = {"//","//","//","c","c","#","#","#","$!"};

  char *s;
  int l,mode,i;

  s = name + strlen(name);
  while( s > name && *(s-1) != ']' && *(s-1) != '/') s--;
  *infile = s;
  if(*s == 0){fprintf(stderr,"### Invalid file name: %s\n",name);exit(1);}
  while(*s != 0 && *s != '.')s++;
  if(*s != '.'){fprintf(stderr,"### Cannot determine file type: %s\n",name);exit(1);}
  s++;
  l = -1;

  for( i=0; i < NXT;i++)if(strcmp(s,exts[i]) == 0) l = i;

  if(l < 0){fprintf(stderr,"### Unrecognised file extension for: %s\n",name);exit(1);}

  *s1 = c1[l];
  *s2 = c2[l];

}
/**********************************************************************/
private void process(fin,fout,s1,s2,infile,outdir,doex)
char *s1,*s2,*infile,*outdir;
int doex;
FILE *fin,*fout;
/*
  This takes a Miriad .doc file (FILE *fin), and writes to the output
  (FILE *fout) a format version of it.

  Inputs:
    fin		File descriptor of the input.
    fout	File descriptor of the output.
    s1,s2	Character strings introducing comments.
    doex        Obey the "exclude" command.
----------------------------------------------------------------------*/
{

#define OUTSIDE  0
#define PREAMBLE 1
#define MAIN     2
#define EXCLUDE  3

  char line[MAXLINE],task[MAXLINE],one[MAXLINE],author[MAXLINE],cat[MAXLINE];
  char indent[MAXLINE],outname[MAXLINE];
  char *t,*u;
  char c;
  FILE *foutd;
  int dosub,doslash,s,mode;

  *indent = 0;

/* Process the input file. */

  doslash = FALSE;
  if(outdir){
    s = *(outdir + strlen(outdir) - 1);
    doslash = isalnum(s);
  }
  *task = *one = *cat = *author = *indent = 0;
  foutd = NULL;
  mode = OUTSIDE;

  while(s = getline(fin,line,s1,s2)){
    t = skip(line);
    switch(s){
      case '=':
      case '*':
	if(mode == EXCLUDE ){
	  mode = OUTSIDE;
	}else if(mode == OUTSIDE){
	  dosub = s == '*';
	  u = task;
	  while(*t != ' ' && *t != '-' && *t != 0 ){
	    c = *t++;
	    if (c >= 'A' && c <= 'Z') c = c - 'A' + 'a';
	    *u++ = c;
	  }
	  *u = 0;
	  if(*task) mode = PREAMBLE;
	  while(*t == ' ' || *t == '-')t++;
	  strcpy(one,t);
	}
	break;
      case '&':
	if(mode == PREAMBLE)strcpy(author,t);
	break;
      case ':':
	if(mode == PREAMBLE)strcpy(cat,t);
	break;
      case '+':
	if(mode == PREAMBLE){
	  if(fout != NULL ) foutd = fout;
	  else {
	    if(outdir){
	      strcpy(outname,outdir);
	      if(doslash)strcat(outname,"/");
	      strcat(outname,task);
	    } else strcpy(outname,task);
	    strcat(outname,".doc");
	    foutd = fopen(outname,"w");
	    if(foutd == NULL){fprintf(stderr,"### Error opening output: %s\n",outname);exit(1);}
	  }
	  if(dosub)  fprintf(foutd,"%%N %s (file: %s)\n",task,infile);
	  else       fprintf(foutd,"%%N %s\n",task);
	  if(*one)   fprintf(foutd,"%%D %s\n",one);
	  if(*author)fprintf(foutd,"%%P %s\n",author);
	  if(*cat)   fprintf(foutd,"%%: %s\n",cat);
	  fprintf(foutd,"%%B\n");
	  if(*t)fprintf(foutd,"%s\n",line);
	  mode = MAIN;
	}
	break;
      case '@':
      case '<':
	if(mode == MAIN){
	  fprintf(foutd,"%%A %s\n",t);
	  if(s == '<')
	    fprintf(foutd,"%sStandard keyword %s. See the help on \"%s\" for more information.\n",indent,t,t);
	}
	break;
      case '-':
      case 'X':
	if(foutd != NULL && fout == NULL)fclose(foutd);
	*task = *one = *cat = *author = 0;
	foutd = NULL;
	mode = OUTSIDE;
	if(s == 'X' && doex)mode = EXCLUDE;
	break;
      default:
	if(mode == MAIN){
	  getindent(indent,line);
	  fprintf(foutd,"%s\n",line);
        }
	break;
    }
  }
  if(foutd != NULL && fout == NULL)fclose(fout);
}
/**********************************************************************/
private int getline(fin,line,c1,c2)
FILE *fin;
char *line,*c1,*c2;
{
  char buf[MAXLINE],*s,*t,*u;
  int i,indent;
  char tcom[3];
  strcpy(tcom,"*");
  strcat(tcom,"/");

  s = fgets(buf,MAXLINE,fin);
  if(s == NULL)return 0;

 /* Detab the line. */

  t = line;
  indent = 1;
  while(*s){
    if(*s == '\t'){do *t++ = ' '; while((indent++)%8); s++;}
    else	  {*t++ = *s++; indent++; }
  }
  *t = 0;

/* If it ends in , strip this off. Get rid of trailing blanks. */

  t--;
  while(t >= line && isspace(*t)) t--;
  *++t = 0;
  if(t-2 >= line)if(strncmp(t-2,tcom,2) == 0){ t -= 2; *t = 0;}
  t--;
  while(t >= line && isspace(*t)) t--;
  *++t = 0;

/* Is it a comment character in the language? */

  s = c1;
  t = line;
  u = buf;
  while( *s++ && *t)*u++ = *t++;
  *u = 0;
  if(strcmp(buf,c1) == 0 || strcmp(buf,c2) == 0){
    s = c1;
    t = line;
    while(*s++)*t++ = ' ';
    if(*t && *(t+1) && ! isspace(*(t+1)) && ! isalpha(*(t+1)) && 
	     *(t+1) != '-') i = ' ';
    else if(*t) i = *t;
    else        i = ' ';
    if(*t && i != ' ') *t = ' ';
  } else {
    i = ' ';
  }
  return(i);  
}
/**********************************************************************/
private void getindent(indent,line)
char *indent,*line;
{
  int i;
  i = 0;
  while(isspace(*line)){line++; i++;}
  if(*line){
    while(i--)*indent++ = ' ';
    *indent++ = 0;
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
  s = line;
  while(isspace(*s))s++;
  return(s);
}
