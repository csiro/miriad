/*= ratty FORTRAN preprocessor, so that same code works on more machines */
/*& bpw */
/*: tools */
/*+
Ratty is a FORTRAN preprocessor for MIRIAD source code, intended to
make the same FORTRAN code compatible on VMS, SUN, Cray, Alliant,
and Convex. The output is suitable for use with the specified compilers
for these machines.

Usage:

 ratty [-s system] [-I incdir] [-D symbol] [-bglu [-n start inc] [in] [out]

    system:  One of "f77" (generic unix compiler), "unicos" (Cray FORTRAN
             compiler), "vms" (VMS FORTRAN), "alliant" (alliant unix
             compiler), "convex" (convex unix compiler) or "sun"
             (sun unix compiler), "f2c" (NETLIBs f2c compiler),
             "hpux", "alpha" or "sgi", "linux"

    incdir:  Directory to search for include files.  The -I option may
             be used repeatedly, but must list only one directory for
             each -I parameter (eg, list two directories as
             "-I dir1 -I dir2").

    symbol:  Define this symbol to the preprocessor. Multiple
             definitions are allowed, defining one symbol per -D entry
             (eg, define two symbols as "-D sym1 -D sym2").

    -b:      If specified, backslashes inside quoted textstrings are
             doubled. This allows for compilers which treat the
             backslash as an escape character.

    -g:      If specified, it will direct the code to include line number
             directives for your native compiler, if it understands them.
             This will enable you to run dbx, and look directly at your
             [in] file, instead of your [out] file. Note that standard
             input cannot be used with this option.

    -n:      This gives the start and increment for line numbers generated
             by ratty. The default is 90000 1.

    -l:      Convert all variables, etc, to lower case.

    -u:      Convert all variables, etc, to upper case.
             (some of the system generated if/then/else/endif/continue
              are not converted to upper case)

    in:      Input file name. If omitted, standard input is assumed
             and output must be the standard output.

    out:     Output file name. If omitted, standard output is assumed.


Ratty recognizes the standard C preprocessor directives #ifdef,
#ifndef, #else and #endif.

The VAX do/dowhile/enddo extension for do loops is converted to
ANSI-FORTRAN if the host machine does not support this extension.

'IMPLICIT NONE' and 'IMPLICIT UNDEFINED(A-Z)' are converted to
whatever the specified compiler supports.

Certain directives are recognized if the target machine has
vector processing capacities (compilers "unicos", "alliant" and "convex"):

    #maxloop, followed by a number, is converted to "cdir$ shortloop"
    on "unicos" and to "cvd$  shortloop" on "alliant".

    #ivdep is converted to "cdir$ ivdep" on "unicos", to "cvd$  nodepchk"
    on "alliant", and to "c$dir no_recurrence" on "convex".

    #nooptimize is converted to "cdir$ nextscalar" on "unicos" and to
    "cvd$ noconcur" followed by "cvd$ novector" on "alliant".
*/
/*-- */
/************************************************************************/
/*									*/
/*  History:								*/
/*    rjs    oct86 Original FORTRAN version.				*/
/*    rjs    nov88 Converted to C.					*/
/*    rjs  15sep89 Handle `implicit undefined' statement.		*/
/*    rjs   9feb90 -D flag, -I flag. Got stderr to work on VMS.		*/
/*    pjt  15may90 added -? flag for people with poor memory            */
/*    bpw  ??????? messing with string continuation, inline doc         */
/*    pjt  17dec90 return proper exit status to shell when OK           */
/*    mjs/bpw 17jan91 Rewrote documentation                             */
/*    rjs  23jan91 Corrected documentation. Fixed errors introduced in	*/
/*                 usage()                                              */
/*    bpw  26mar91 Corrected handling of uppercase defined symbols:     */
/*                 now such symbols are not required to be lowercase    */
/*                 (which was undocumented).                            */
/*    rjs  17sep91 Label start and increment. Sun is a recognised system*/
/*    pjt  12dec91 Added tentative -g option for #line dbx comments	*/
/*		   for non-local include files the path is not added yet*/
/*    rjs  13dec91 Merged back in some innovate rjs options (-l|-u)     */
/*		   also fixed old bug: -i switch didn't work (-I did)   */
/*    rjs  14jul92 Check for consistent use of the -b switch.           */
/*    rjs  07jan93 Get #define to work.					*/
/*    rjs  20nov94 Added alpha.						*/
/*    pjt   3jan95 Added f2c (used on linux)                            */
/*    rjs  15aug95 Added sgi		                                */
/*    rjs  22may06 Change to appease cygwin.				*/
/*    mrc  14jul06 Get it to compile with 'gcc -Wall' without warnings. */
/*									*/
/************************************************************************/
/* ToDos/Shortcomings:                                                  */
/*  The -u flag doesn't convert self-generated if/then/continue etc.    */
/*  This would mean occurences like                                     */
/*      textout("continue\n");                                          */
/*  to be changed to:                                                   */
/*      (uflag?textout("continue\n"):textout("CONTINUE\n"));            */
/************************************************************************/
#define VERSION_ID   "15-Aug-95"

#define max(a,b) ((a) > (b) ? (a) : (b) )
#define min(a,b) ((a) < (b) ? (a) : (b) )

#define private static
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TRUE 1
#define FALSE 0

/* A few things to stop lint complaining. */
#define Strcpy (void)strcpy
#define Strcat (void)strcat
#define Fclose (void)fclose
#define Fputc  (void)fputc
#define Malloc(a) malloc((unsigned int)(a))

/*
 * Define capacities of the various compilers. These are:
 *  doloop	is true if the compiler handles the VAX do/dowhile/enddo
 *		extension.
 *  prog	is true if the compiler expects the CTSS/NOS form of the
 *		program statement.
 *  vector	is true if the target machine has vector processing
 *		capacities.
 */

struct system {char *name;int doloop,prog,vector,bslash;} systems[] = {
	{ "unknown", FALSE, FALSE, FALSE, 0 },
	{ "f77",     FALSE, FALSE, FALSE, 0 },
	{ "f2c",     TRUE,  FALSE, FALSE, 1 },
	{ "unicos",  FALSE, TRUE,  TRUE, -1 },
	{ "vms",     TRUE,  FALSE, FALSE,-1 },
	{ "alliant", TRUE,  FALSE, TRUE,  0 },
	{ "convex",  TRUE,  FALSE, TRUE, -1 },
        { "trace",   TRUE,  FALSE, FALSE, 0 },
	{ "sun",     TRUE,  FALSE, FALSE, 1 },
	{ "sgi",     TRUE,  FALSE, FALSE, 0 },
	{ "linux",   TRUE,  FALSE, FALSE, 1 },
	{ "hpux",    FALSE, FALSE, FALSE, 0 },
	{ "alpha",   TRUE,  FALSE, FALSE, 1 }};
#define SYS_F77    1
#define SYS_F2C    2
#define SYS_CFT    3
#define SYS_VMS    4
#define SYS_FX     5
#define SYS_CONVEX 6
#define SYS_TRACE  7
#define SYS_SUN    8
#define SYS_SGI    9
#define SYS_LINUX 10
#define SYS_HPUX  11
#define SYS_ALPHA 12
#define NSYS (sizeof(systems)/sizeof(struct system))

#define LOWER 90000	/* Ratty uses statement labels above this number. */
#define MAXDEPTH 32	/* Max nesting of do-loops than Ratty can handle. */
#define MAXLINE 256	/* Max length of a line. */

static FILE *out;
static int dbslash,offlevel,level,sys,label,uselabel,depth,lines,routines,chars;
static int comment,in_routine,gflag,lflag,uflag;
static int loops[MAXDEPTH],dowhile[MAXDEPTH];
struct link_list {char *name; struct link_list *fwd;} *defines,*incdir;

private void process(),message(),textout(),labelout(),numout(),blankout(),lowercase(),
	cppline(),get_labelnos(),usage();
private struct link_list *add_list();
private int get_line(),reformat(),isdefine();
private char *getparm(),*progtok(),*skipexp();
private FILE *incopen();
private int continuation,quoted=FALSE;
private int lower,increment;
/************************************************************************/
int main(argc,argv)
int argc;
char *argv[];
{
  char *s,*infile,*outfile,*sysname;
  int i;
  FILE *in;

/* Initalize everything in sight. */

  incdir = defines = NULL;
  in_routine = dbslash = comment = gflag = lflag = uflag = FALSE;
  offlevel = level = uselabel = depth = lines = routines = chars= 0;
  lower = LOWER;
  increment = 1;

/* Handle the command line. */

  sysname = "unknown";
  infile = NULL;
  outfile = NULL;

  for(i=1;i<argc;i++){
    s = argv[i];
    if(*s == '-'){				/* Handle flags. */
      s++;
      while(*s)switch(*s++){
        case 'b': dbslash = TRUE;				break;
	case 's': if(++i < argc) sysname = argv[i];		break;
	case 'd':
	case 'D': if(++i < argc) defines = add_list(defines,argv[i]);	break;
	case 'n': if(i+2 < argc){ get_labelnos(argv[i+1],argv[i+2]); i += 2; }; break;
	case 'i':
	case 'I': if(++i < argc) incdir  = add_list(incdir,argv[i]);	break;
	case 'g': gflag=TRUE; 					break;
        case 'l': lflag = TRUE;                                 break;
	case 'u': uflag = TRUE;                                 break;
	case '?': usage();		/* will also exit */
	default:  fprintf(stderr,"### Ignored unrecognized flag %c\n",*(s-1));
						break;
      }
    } else if(infile == NULL) infile = s;
    else if(outfile == NULL ) outfile = s;
    else fprintf(stderr,"### Argument %s ignored\n",s);
  }

  label = lower - increment;

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
  if(gflag && infile==NULL) gflag=FALSE;  /* handle idiots */

/* Determine the target compiler type, and define a few parameters. */

  sys = 0;
  while(sys < NSYS && strcmp(systems[sys].name,sysname) != 0) sys++;
  if(sys == NSYS){
    message("Unrecognised system type, basic system assumed");
    sys = 0;
  }
  defines = add_list(defines,sysname);
  if(systems[sys].vector) defines = add_list(defines,"vector");

/* Check for sensible use of the -b flag. */

  if((systems[sys].bslash > 0 && !dbslash) ||
     (systems[sys].bslash < 0 &&  dbslash))
    fprintf(stderr,"### Warning: Should the -b switch be used?\n");

/* Process the input file, creating the output. Then close down. */

  process(in,infile);
  Fclose(in);
  Fclose(out);

/* Give a final message. */

  printf("Number of lines = %d; number of routines = %d\n",lines,routines);
  exit(0);
}
/************************************************************************/
private void get_labelnos(slower,sinc)
char *slower,*sinc;
/*
  Get lowest label to use, and the increment to apply.
------------------------------------------------------------------------*/
{
  char *s;
  lower = 0;

  s = slower; lower = 0;
  while(*s >= '0' && *s <= '9') lower = 10*lower + *s++ - '0';
  if(*s != 0){
    fprintf(stderr,"### Bad label start: %s\n",slower);
    exit(2);
  }

  s = sinc; increment = 0;
  while(*s >= '0' && *s <= '9') increment = 10*increment + *s++ - '0';
  if(*s != 0){
    fprintf(stderr,"### Bad label increment: %s\n",sinc);
    exit(2);
  }
}
/************************************************************************/
private void process(in,infile)
FILE *in;
char *infile;
/*
  Process a newly opened input file, writing code to the output.
  Note: process() is called recursively.
------------------------------------------------------------------------*/
{
  int type,lineno,lineno1,indent,bracketting, glines=0, oldglines;
  char *s,*s0,line[MAXLINE],token[MAXLINE],msg[MAXLINE];
  char gfile[MAXLINE];
  FILE *in2;

  if(gflag)strcpy(gfile,infile);            /* init -g filename */

  while((type = get_line(in,line))){
    lines++;
    if(gflag){
        glines++;
        sprintf(msg,"# %d \"%s\"\n",glines,gfile);
        textout(msg);
    }
    if(*line == 0);			/* Do nothing if a null line. */
    else if(type == '#')cppline(line);	/* Handle preprocessor line. */
    else if(offlevel != 0);		/* Do nothing if output is off. */
    else if(type == 'c'){		/* Handle a comment. */
      textout(line); textout("\n");
    } else {				/* Handle FORTRAN. */
      in_routine = TRUE;
      s = progtok(line,token,&indent,&lineno,&bracketting);
      if(lineno >= lower && lineno <= uselabel)
	message("RATTY may have used this label number");
      label = max(label,lineno);

/* Program statement. */
      if(!strcmp(token,"program") && systems[sys].prog){
	blankout(indent); textout("program "); textout(s);
	textout("(tty,input=tty,output=tty)\n");

/* DO and DOWHILE loops. */
      } else if(!systems[sys].doloop &&
	       (!strcmp(token,"do") || !strcmp(token,"dowhile"))){
	if(depth == MAXDEPTH){
	  message("DO/DOWHILE loops nested too deep");
	} else {
	  if(lineno == 0 && !strcmp(token,"dowhile")){
	    label += increment;
	    lineno1 = lineno = uselabel = label;
	  }else if(!strcmp(token,"do")){
	    label += increment;
	    lineno1 = uselabel = label;
	  }else lineno1 = lineno;
	  loops[ ++depth ] = lineno1;
	  if(!strcmp(token,"do")){
	    dowhile[depth] = FALSE;
	    if(lineno != 0) {labelout(lineno); blankout(indent-5);}
	    else blankout(indent);
	    textout("do " ); numout(lineno1); textout(" "); textout(s);
	    textout("\n");
	  } else {
	    dowhile[depth] = TRUE;
	    labelout(lineno1); blankout(indent-5);
	    textout("if"); textout(s);
	    while(bracketting){
	      textout("\n");
	      type = get_line(in,line);
	      if(type != '*'){
		message("Bad DOWHILE statement");
		bracketting = 0;
	      }else{
		s = skipexp(line+6,&bracketting);
		if(*s) message("Bad DOWHILE statement");
	      }
	      textout(line);
	    }
	    textout("then\n");
	  }
	}

/* ENDDO statement. */
      } else if(!strcmp(token,"enddo") && !systems[sys].doloop){
	if(depth == 0) message("Found ENDDO without corresponding DO");
	else {
	  if(lineno != 0){
	    labelout(lineno); blankout(indent-5); textout("continue\n");

	  }
	  if(dowhile[depth]){
	    blankout(indent); textout("goto "); numout(loops[depth]);
	    textout("\n");
	    blankout(indent); textout("endif\n");
	  } else {
	    labelout(loops[depth]); blankout(indent-5); textout("continue\n");
	  }
	  depth --;
	}

/* Process an INCLUDE file. */
      } else if(!strcmp(token,"include")){
	s++;
	s0 = token;
	while(*s && *s != '\'')*s0++ = *s++;
	*s0 = 0;
	in2 = incopen(token);
	if(in2 == NULL){
	  sprintf(msg,"Error opening include file %s",token);
	  message(msg);
	  textout(line); textout("\n");
	} else {
	  if(lineno != 0){
	    labelout(lineno); blankout(indent-5); textout("continue\n");
	  }
          oldglines = glines;
	  sprintf(msg,"c >>> %s\n",token);
	  textout(msg);
	  process(in2,token);
	  sprintf(msg,"c <<< %s\n",token);
	  textout(msg);
	  Fclose(in2);
          glines = oldglines;
	}

/* Procedure END */
      } else if(!strcmp(token,"end")){
	if(depth != 0)message("Ended a procedure with DO/DOWHILE unclosed");
	depth = 0;
	label = LOWER - 1;
	uselabel = 0;
	textout(line); textout("\n");
	routines++;
	in_routine = FALSE;

/* IMPLICIT NONE statement. */
      } else if(!strcmp(token,"implicitnone") ||
		!strcmp(token,"implicitundefined")){
	if(sys == SYS_VMS || sys == SYS_FX || sys == SYS_CONVEX || 
           sys == SYS_SUN || sys == SYS_F2C) {
	  blankout(indent); textout("implicit none\n");
	} else if(sys == SYS_F77){
	  blankout(indent); textout("implicit undefined (a-z)\n");
	}

/* Some other line. */
      } else {
	textout(line); textout("\n");
      }
    }
  }
}
/************************************************************************/
private void message(text)
char *text;
{
  fprintf(stderr,"### %s\n",text);
  fprintf(out,"c### %s\n",text);
}
/************************************************************************/
private void textout(text)
char *text;
{
  int l;
  if(!chars ) comment = (*text == 'c');
  l = strlen(text); chars += l;
  fprintf(out,"%s",text);
  if(*(text+l-1) == '\n'){
    chars --;
    l = (chars > 72 && !comment);
    chars = 0;
    comment = FALSE;
    if(l) message("Line longer than 72 chars");
  }
}
/************************************************************************/
private void numout(label)
int label;
{
  char num[10];
  sprintf(num,"%d",label);
  fprintf(out,"%s",num);
  chars += strlen(num);
  comment = FALSE;
}
/************************************************************************/
private void labelout(label)
int label;
{
  fprintf(out,"%5d",label);
  chars += 5;
  comment = FALSE;
}
/************************************************************************/
private void blankout(blanks)
int blanks;
{
  chars += blanks;
  while(blanks-- > 0)Fputc(' ',out);
  comment = FALSE;
}
/************************************************************************/
private void lowercase(string)
char *string;
/*
  Convert a string to lower case.
------------------------------------------------------------------------*/
{
  while(*string){
    if(*string >= 'A' && *string <= 'Z') *string = *string - 'A' + 'a';
    string++;
  }
}
/************************************************************************/
private char *getparm(line,token)
char *line,*token;
{
  while(*line == ' ')line++;				/* Skip white. */
  while(*line != ' ' && *line != 0)*token++ = *line++;	/* Copy token. */
  *token = 0;						/* Terminate token. */
  return(line);
}
/************************************************************************/
private void cppline(line)
char *line;
{
  int ok,loop;
  char *s,token[MAXLINE],parm[MAXLINE];

/* Skip leading blanks, c's and hashes. */

  while(*line == ' ' || *line == '#' || *line == 'c' || *line == 'C' ||
        *line == '*' )line++;
  line = getparm(line,token);
  lowercase(token);

/* Determine what the directive is. */

/* #ifdef or #ifndef derective. */

  if(!strcmp(token,"ifdef") || !strcmp(token,"ifndef")){
    line = getparm(line,parm);
    level++;
    if(!offlevel){
      if(!*parm)message("Bad #ifdef statement taken as satisfied.");
      else {
	ok = isdefine(parm);
	if((ok && !strcmp(token,"ifndef")) || (!ok && !strcmp(token,"ifdef")))
	  offlevel = level;
      }
    }

/* #else directive. */

  } else if(!strcmp(token,"else")){
    if(!level) message("Unexpected #else ignored");
    else if(!offlevel) offlevel = level;
    else if(offlevel == level) offlevel = 0;

/* #endif directive. */

  } else if(!strcmp(token,"endif")){
    if(!level) message("Unexpected #endif ignored");
    else if(offlevel == level--) offlevel = 0;

/* #define directive. */

  } else if(!strcmp(token,"define")){
    if(!offlevel){
      line = getparm(line,parm);
      if(!*parm)message("Bad #define statement ignored.");
      else defines = add_list(defines,parm);
    }
    
/* #maxloop directive. Issue a "short loop" directive if appropriate. */

  } else if(!strcmp(token,"maxloop")){
    line = getparm(line,parm);
    loop = 0;
    s = parm;
    while(*s) loop = 10*loop + *s++ - '0';
    if(!loop) message("Bad #maxloop directive");
    else if(offlevel);
    else if(sys == SYS_CFT && loop <= 64) textout("cdir$ shortloop\n");
    else if(sys == SYS_FX  && loop <= 32) textout("cvd$  shortloop\n");

/* #ivdep directive. Issue a "no dependency" directive. */

  } else if(!strcmp(token,"ivdep")){
    if(offlevel);
    else if(sys == SYS_CFT) textout("cdir$ ivdep\n");
    else if(sys == SYS_FX ) textout("cvd$  nodepchk\n");
    else if(sys == SYS_CONVEX) textout("c$dir no_recurrence\n");

/* #nooptimize directive. Do not optimize the following loop. */

  } else if(!strcmp(token,"nooptimize")){
    if(offlevel);
    else if(sys == SYS_CFT)
      textout("cdir$ nextscalar\n");
    else if(sys == SYS_FX ){
      textout("cvd$  noconcur\n");
      textout("cvd$  novector\n");
    }

/* Unrecognised directive. */

  } else {
    message("Unrecognised directive ignored");
    textout("c"); textout(token); textout("\n");
  }
}
/************************************************************************/
private char *progtok(line,token,indent,lineno,bracketting)
char *line,*token;
int *indent,*lineno,*bracketting;
/*
  This gets the first token out of a line. Tokens can be:
    program
    do
    dowhile
    enddo
    end
    include
    implicit none
    DEC continuation format

  Some checks are also made for "standard" format of comments and
  parameter statements.

------------------------------------------------------------------------*/
{
  char *s,*t,*u;

/* Determine the statement label and the indentation. */

  s = line;
  *bracketting = 0;
  *lineno = 0;
  while(*s == ' ')s++;
  while(*s >= '0' && *s <= '9' && s - line < 5) *lineno = 10 * *lineno + *s++ - '0';
  while(*s == ' ')s++;
  *indent = s - line;

  t = token;
  u = s;

/* A standard continuation line. Just ignore it. */

  if(*indent == 5);

/* DEC format for fortran continuation. */

  else if((*s < 'a' || *s > 'z') && (*s < 'A' || *s > 'Z')){
    *(line+5) = '*';
    *s = ' ';
    *indent = 5;
    message("VMS format continuation converted");

/* Some form of statement. Handle it correctly. Copy, ignoring blanks, to
   the first  non-alphanumeric code. Convert to lower case on the way. */

  } else while(*s == ' ' || (*s >= 'a' && *s <= 'z') || (*s >= 'A' && *s <= 'Z')
			|| *s == '$' || *s == '_' || (*s >= '0' && *s <= '9')){
    if(*s == ' ')s++;
    else if(*s >= 'A' && *s <= 'Z') *t++ = *s++ + 'a' - 'A';
    else 			    *t++ = *s++;

  }
  *t = 0;			/* Zero terminate. */

/* Make sure the token is really what it looks like. */

  if(!strcmp(token,"implicitnone") || !strcmp(token,"enddo")
				   || !strcmp(token,"end")){
    if(*s) *token = 0;
  } else if(!strcmp(token,"implicitundefined")){
    if(*s != '(') *token = 0;
  } else if(!strcmp(token,"include")){
    if(*s != '\'') *token = 0;
  } else if(!strncmp(token,"program",7)){
    if(*s)*token = 0;
    else  token[7] = 0;

/* Check for a DO or DOWHILE statement. */

  } else if(!strcmp(token,"dowhile")){
    if(*skipexp(s,bracketting)) *token = 0;
  } else if(!strncmp(token,"do",2)){
    if((token[2] >= '0' && token[2] <= '9') || *s != '=') *token = 0;
    else {
      if(*skipexp(s+1,bracketting) != ',') *token = 0;
      else token[2] = 0;
    }

/* Make a few checks to see if its one of the other ANSI statements. */    

  } else {
    *token = 0;
  }

/* Skip over the part of the line which corresponds to the token. Remember
   that FORTRAN allows imbedded blanks. */

  s = u;
  t = token;
  while(*t){
    if(*s != ' ')t++;
    s++;
  }
  while(*s == ' ')s++;

  return(s);
}
/************************************************************************/
private char *skipexp(s,bracketting)
char *s;
int *bracketting;
/*
  This skips over an expression. It returns a pointer to the first non-blank
  character after the expression.
------------------------------------------------------------------------*/
{
  int inchar,inexp,depth;

  depth = *bracketting;
  inchar = FALSE;
  inexp = TRUE;
  while(*s && inexp){
    if(*s == '\'')inchar = !inchar;
    else if(inchar);
    else if(*s == '(') depth++;
    else if(*s == ')') depth--;
    else if(!depth)inexp = *s != ',' && *s != '=';
    if(inexp)s++;
  }
  *bracketting = depth;
  return(s);
}
/************************************************************************/
private int get_line(in,line)
FILE *in;
char *line;
/*
  Get a line from the input file, and make a quick determination about
  what it is. This returns: 'c' if its a comment, ' ' for a normal
  line, '#' for a preprocessor line, and 0 for EOF.
------------------------------------------------------------------------*/
{
  char *s;
  int type;

  if(fgets(line,MAXLINE,in) == NULL)return(0);

/* Trim trailing blanks. */

  s = line + strlen(line);
  while(s-line >= 0 && *s <= ' ')*s-- = 0;

/* Handle a blank line. */

  if(!*line){
    type = 'c';
    if(in_routine) Strcpy(line,"c");

/* Handle a RATTY directive. */

  } else if(!strncmp(line,"#",1) || !strncmp(line,"C#",2) || !strncmp(line,"c#",2)){
    type = '#';

/* A comment. */

  } else if(*line == 'c' || *line == 'C' || *line == '*' || *line == 'd' ||
	   *line == 'D' ){
    *line = 'c';
    type = 'c';

/* Must be a normal line. Reformat it. */

  } else {
    type = reformat(line);
  }
  return(type);
}
/************************************************************************/
private int reformat(s)
char *s;
/*
  Reformat a normal line. Get rid of tabs. Look for special characters.
  Check for unbalanced quotes. Strip off trailing ! comments.
------------------------------------------------------------------------*/
{
  char *s0,*t,line[MAXLINE],c;
  int pad,first,type;

/* Make a copy of the input, so we can overwrite it. */

  Strcpy(line,s);
  s0 = s;

  t = line;
  continuation=FALSE;
  while(*t == ' ')t++;
  while(*t >= '0' && *t <= '9' && t-s < 5) t++;
  while(*t == ' ')t++;
/* A standard continuation line. */
  if(t-line == 5) continuation=TRUE;
/* DEC format for fortran continuation. */
  else if((*t < 'a' || *t > 'z') && (*t < 'A' || *t > 'Z')) continuation=TRUE;
  if ( !continuation && quoted ) message("Quotes are not balanced");
  if ( !continuation ) quoted = FALSE;

/* Scan through the line. */
  type = ' ';
  first = TRUE;
  t = line;
  while((c = *t++)){
    if(c == ' ') *s++ = ' ';
    else if(c == '\t'){
      pad = 8 * ( (s - s0)/8 + 1 ) - (s - s0);
      while(pad-- > 0)*s++ = ' ';
    } else{
      if( (s-s0) == 5 ||
	(first && (s-s0) > 5 && (c <'a' || c > 'z') && (c < 'A' || c > 'Z')) ){
	*s++ = ' ';
	*(s0+5) = '*';
	first = FALSE;
	type = '*';
      } else if(c == '\''){
	quoted = !quoted;
	*s++ = c;
      } else if(quoted){
	if(c == '\\' && dbslash)*s++ = '\\';
	*s++ = c;
      } else if(c == '!'){
	while(*t)t++;
        while(*(s-1) == ' ')s--;
      } else if(lflag && isupper(c)){
        *s++ = tolower(c);
      } else if(uflag && islower(c)){
        *s++ = toupper(c);
      } else *s++ = c;
      first = FALSE;
    }
  }
  *s = 0;
  return(type);
}
/************************************************************************/
private struct link_list *add_list(list,name)
struct link_list *list;
char *name;
/*
  Indicate that a thing is defined.
------------------------------------------------------------------------*/
{
  struct link_list *s,*t;

  t = (struct link_list *)Malloc(sizeof(struct link_list));
  t->fwd = NULL;
  t->name = (char *)Malloc(strlen(name)+1);
  Strcpy(t->name,name);

/* Link it in. */

  if(list == NULL) return(t);

  for(s=list; s->fwd != NULL; s = s->fwd);
  s->fwd = t;
  return(list);
}
/************************************************************************/
private FILE *incopen(name)
char *name;
/*
  Attempt to open an include file.
------------------------------------------------------------------------*/
{
  FILE *fd;
  char *s,c,line[MAXLINE];
  struct link_list *t;

/* Try the plain, unadulterated name. */

  if((fd = fopen(name,"r")) != NULL) return(fd);

/* Otherwise try appending it to the list of include file directories. */

  for(t = incdir; t != NULL; t = t->fwd){
    s = t->name;
    Strcpy(line,s);
    c = *(s + strlen(s) - 1);
    if(isalnum(c))Strcat(line,"/");
    strcat(line,name);
    if((fd = fopen(line,"r")) != NULL) break;
  }
  return(fd);
}
/************************************************************************/
private int isdefine(name)
char *name;
/*
  Check whether a thingo is defined.
------------------------------------------------------------------------*/
{
  struct link_list *t;
  t = defines;
  while(t != NULL && strcmp(t->name,name))t = t->fwd;
  if(t != NULL)return(TRUE);
  return(FALSE);
}
/************************************************************************/
private void usage()
{
   int i;

   fprintf(stderr,"RATTY: Version %s\n",VERSION_ID);
   fprintf(stderr,"Usage: \n");
   fprintf(stderr,"ratty [-s system] [-I incdir] [-D symbol] [-bglu] [-n start inc] [in] [out]\n");
   fprintf(stderr,"-s system    compile for system (");
   for (i=1; i<NSYS; i++) fprintf(stderr," %s",systems[i].name);
   fprintf(stderr," )\n");
   fprintf(stderr,"-I incdir    add dir to include directory\n");
   fprintf(stderr,"-D symbol    set a symbol to be defined for #ifdef\n");
   fprintf(stderr,"-n start inc set generated label start and inc\n");
   fprintf(stderr,"-b           double slashes in quoted textstrings\n");
   fprintf(stderr,"-g           include # references for dbx\n");
   fprintf(stderr,"-l           convert program text to lower case\n");
   fprintf(stderr,"-u           convert program text to upper case\n");
   fprintf(stderr,"-?           help (this list)\n");
   exit(0);
}

