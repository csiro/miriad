/******************************************************************************

  Flint is a FORTRAN checker. It takes FORTRAN code as its input, and
  generates warnings about dubious FORTRAN. These messages are written
  both to the terminal, and a listing file.

  History:
    rjs  ??????? Original version.
    rjs   4oct89 Fixed implied-do-loop bug on write statements.
		 Recognised VMS continuation (but complains). Fixed
		 a bug for COMMON blocks containing array declarations, and
		 mixing numerics and characters.
    rjs   1nov89 Fixed it so that it notices two consecutive commas in a 
		 routine call or argument list.
    rjs   3feb90 Fixed spurious complaint about subroutine or function
		 calls which did not contain any arguments. Better checking
		 for argument type consistency of generic functions.
    rjs   8feb90 Fixed a bug I introduced on 3feb90.
    rjs  12feb90 Fixed another bug I introduced on 3feb90.
    pjt  30apr90 Add I flag for search-include files
    pjt  13may90 renamed the 'i' flag to a 'j' flag for VMS (I for include)
    pjt  21jun90 warn if intrinsic functions are used as variables
    rjs  17oct90 Made -i synomous with -I for the sake of VMS.
    rjs  13nov90 Some support for "double complex" statement.
    rjs   7dec90 Added a missing "end comment" character.
    rjs  21dec90 Check for doubly declared variables. Better redundant arg
		 messages. Fixed bug checking for functions with no args.
		 Tidied pjt's check for intrinsics.
   rjs   10jan91 Better cross reference listing.
   pjt   20jan91 changed some doc's (more alpha)
   rjs   22jan91 Merged pjt and rjs versions.
   rjs   30jan91 Fixed bug so that complex values can contain + and - signs.
   rjs   17jul91 Better handles statements starting with "do" (e.g.
		 do-loops and double). Check nesting of do/enddo, if/endif, etc.
   rjs   22jul91 Better treatment at END statement and missing END statements.
   rjs   24jul91 Better treatment of bad if/elsseif statements.
   rjs    9oct91 Mods to get flint to recognise some forms of character
		 assignment and declaration that it was failing to before,
		 as found by bpw.
   rjs   10mar92 Crude support for EQUIVALENCE statements.
   rjs   16mar92 -k switch to suppress common block warnings.
		 Statement label checking.
   rjs   26mar92 Suppress some warnings about things starting with % (i.e.
		 VMS functions).
   rjs   21may92 Handle err=label.
   rjs   16jun92 Fixed bug dealing with mixing tabs and spaces at start of line.
   rjs    2sep92 You can use multiple -I switches.
   rjs   24dec92 Fixed incrementing bug if checking for invalid command lines.
   rjs    9aug93 Exit (rather than return) with 0, to appease VAXs.
   rjs   25aug93 Don't complain about lack of initialisation for variables
		 appearing in equivalences.
   rjs   20sep94 Eliminate bug dealing with blank lines.
   rjs   30sep94 Check ends of ENDDO, ENDIF, ELSE statements.
   rjs   25nov95 Fix EQUIVALENCE handling, better treatment of exclamations,
		 do-loop variables. Flag VMS record structures.
   rjs   22may06 Change to appease cygwin.
******************************************************************************/

#define VERSION_ID "22-May-06"

/*= flint - fortran source code verifier */
/*& rjs pjt */
/*: tools */
/*+
  Flint is a FORTRAN checker, producing warnings about non-standard,
  possibly incorrect or poor FORTRAN code.

  Flint takes a large number of flags, to attempt to keep the error 
  messages that it generates down to manageable proportions. The 
  command format is:

   flint [-acdfhjkrsux2?] [-I dir] [-o file] [-l] file ...

    a    make crude list of all variables used
    c    Allow comments and continuation lines to be interwoven. 
         Normally flint flags this as an error.
    d    Do not insist that variables are always explicitly declared.
    f    Disable "line checks".
    h    Crude treatment of hollerith.
    k    Do not warn about COMMON block alignment problems.
    j    Do not check if a variable has been initialised.
    r    Do not warn about seemingly redundant variables.
    s    Load the definitions of specific functions and FORTRAN-IV
         standard function.
    u    Do not worry about unused variables.
    x    Allow names longer than 8 characters.
    2    Flint performs two passes.
    ?    Print a message describing the flags.

    o    Generate output file giving subroutine definitions only. The 
         next command line argument gives the output file name.
    l    The following file is to be processed in "library mode". This
         means that the file is not echoed to flint.log, and that errors
         detected are to be ignored.
    I    add a directory to search for include files.
    i    Equivalent to "I".

  Flint is NOT intended to replace compiler checks.  Flint is blind
  to much bad code that any compiler will pick up. However you might
  find flint's log file useful when doing initial source code corrections
  after writing a piece of code.					*/
/*-- */
/*

  Checks Flint performs include:
    * undeclared variables.
    * variable declared but never used.
    * variables used before being assigned.
    * variables assigned to but not otherwise used.
    * names longer than 8 characters, or containing $ or _ characters.
    * lines longer than 72 characters, or containing an odd number of
      quote characters.
    * subroutine argument consistency: number, type and intent.
    * possible common block alignment problems.

  Areas for Improvement:
    * Improved error checking and handling.
    * Improve "initialisation checking" algorithm.
    * Consistency in common block definition. Checks could include 
      common block size, variable type matching, and even variable 
      name matching.
    * Better checks for illegal mixed expressions (i.e. mixing integers 
      and logicals).
    * Passing character expressions containing char*(*) variables.
    * Parsing of DATA statements is very crude.
    * SAVE statement usage.
    * Treatment of EQUIVALENCE statement is very crude.

  Things Flint does not understand:
    * ASSIGN
    * Assigned GOTO
    * PAUSE
    * ENTRY
    * alternate returns
    * many archaic i/o statements
    * much non-standard FORTRAN

  Flint ignores:
    * IMPLICIT
    * FORMAT

  Recognised FORTRAN extensions:
    * INTENT statement
    * DO/ENDDO, DOWHILE/ENDDO
    * # to start comments
    * tabs and full ascii character set.

  There are some places where better use could have been made of stdio.
  However Flint is intended to work on a number of systems, and so
  it has been written to avoid some bugs in some non-UNIX stdio packages.
  Systems on which Flint is believed to work are Berkeley UNIX (Sun, 
  Alliant, Convex), UNICOS, VMS, Turbo-C (MS-DOS) and CTSS (hcc 
  compiler).

  Basic Workings
  ==============
  Flint maintains two (hash) tables which contain all Flint info about
  variables and subroutines. Almost always, these hash tables are 
  accessed through the set_variable and set_routine subroutines. The 
  caller passes in whatever it knows about the variable or routine, 
  and the set_? routine passes back the accumulated knowledge about 
  the variable or routine. set_variable and set_routine are responsible 
  for generating many warnings. 

  Determining Intent
  ==================
  Flint attempts to uncover the intent of subroutine arguments. It 
  gleans information from both calls to the routine and the source of 
  the routine itself (if available).

  The following rules are used when analysing a call to a routine:
  * Arguments which are passed in as constants, expressions or parameters
    are input! Arguments which are input dummy arguments to the current
    routine are also deemed to be inputs. These rules are reliable.
  * Arguments which are variables which have not been initialised are
    assumed to be output. Also arguments which are output dummy arguments
    to the current routine, and have not yet been initialised, are also
    deemed to be outputs. These rules depend on the accuracy of the
    initialisation checking algorithm, which can be inaccurate. These
    rules are disabled if initialisation checking is disabled.

  The following rules are used when analysing the source of the routine.
  * Dummy arguments which are used as input only are clearly input. This
    rule is reliable.
  * Dummy arguments which are assigned to before they are used, are 
    deemed as output. This rule depends on the initialisation checking 
    algorithm, and is turned off, if initialisation checking is turned 
    off.
  * Dummy arguments which are used and then assigned to are deemed to
    be input/output. 

  What goes on when dummy arguments are passed to subroutines produces 
  much recursive thought. There are some instances where Flint has to 
  discard some hard won information, because there are too few flags 
  to describe all the contortions that can occur.

  Flags and Hash Tables
  =====================
  Each entry in the "vhash" hash table consists of a name and set of 
  flags. These are mostly just variables, but they also contain entries 
  for each EXTERNAL and each function or subroutine called (except the
  FORTRAN intrinsics?).

  Each entry in the "rhash" hash table is a subroutine or function 
  definition. Each definition consists of the name, flags for the 
  routine, the number of subroutine arguments, and an array of flags, 
  one for each argument. Entries for FORTRAN intrinsics appear in this 
  table.

  The meaning of most flags (I hope) is fairly clear, but
  F_IN,F_OUT,F_PIN,F_POUT require more attention. For a variable,
  F_IN and F_OUT is set if the variable has been used as source or
  destination of an operation, respectively. F_PIN is set for subroutine
  arguments, indicating that this variable may have been passed in by the
  caller. If Flint determines that this is not so, then this flag is
  turned off. F_POUT indicates that this variable may have been passed
  out by a subroutine. 

  For a subroutine or function, these flags are used only in the
  vhash table. F_IN and F_OUT indicate that the routine
  has been called. F_PIN is always set if it was a dummy argument.
  F_POUT is set if it was passed to a subroutine.

  For a routine argument (in the "rhash" table), F_IN and F_OUT means
  passed in and passed out. An absence of these indicates that Flint 
  does not know.                                                       */

#define TRUE		1
#define FALSE		0

#define max(a,b) ((a) > (b) ? (a) : (b) )
#define min(a,b) ((a) < (b) ? (a) : (b) )

#define private static
#include <ctype.h>
#include <stdio.h>

/* Define all the flags. */

#define F_DATA		0x0001		/* Variable in DATA statement */
#define F_ROUTINE	0x0002		/* Name appears in a PROGRAM,
					SUBROUTINE, FUNCTION statement, or is
					a global in a CALL or function call. */
#define F_EQUIV	     0x4000000		/* Variable in EQUIVALENCE statement */
#define F_SAVE		0x0004		/* Variable in SAVE statement */
#define F_COMMON	0x0008		/* Variable in COMMON statement */
#define F_OUT		0x0010
#define F_IN		0x0020
#define F_ARG		0x0040		/* Variable or EXTERNAL is a dummy
					   argument */
#define F_SUBARG     0x1000000          /* Set if it appears as an actual
					   subroutine argument. */
#define F_CALLED     0x2000000		/* Set if this sub/func has been
					   called. */
#define F_INTRINSIC	0x0080		/* Function is a Fortran intrinsic */
#define F_PIN		0x0100
#define F_POUT		0x0200
#define F_PARAMETER     0x0400		/* Variable is in PARAMETER statement */
#define F_ARRAY		0x0800		/* Variable is an array */
#define F_SPECIAL	0x1000		/* An intrinsic function requiring some
					   special handling */
#define F_ACTUAL	0x2000		/* In a rhash flags, this indicates
					   that the source for the routine was
					   actually parsed. */
#define F_INTEGER	0x4000		/* Integer variable or function */
#define F_REAL		0x8000		/* Real */
#define F_DOUBLE       0x10000		/* Double precision */
#define F_COMPLEX      0x20000		/* Complex */
#define F_CHAR	       0x40000		/* Character */
#define F_LOGICAL      0x80000		/* Logical */
#define F_VOID	      0x100000		/* SUBROUTINE */
#define F_GENERIC     0x200000		/* Fortran generic function */
#define F_STATEMENT   0x400000		/* Statement function. */
#define F_EXTERNAL    0x800000		/* Appears in an EXTERNAL statement. */

#define IO_MASK    (F_IN|F_OUT)
#define TYPE_MASK  (F_LOGICAL|F_INTEGER|F_REAL|F_DOUBLE|F_COMPLEX|F_CHAR|\
					F_GENERIC|F_VOID)
#define TYPES_MASK (F_LOGICAL|F_INTEGER|F_REAL|F_DOUBLE|F_COMPLEX|F_CHAR)

#define ROUTE_MASK (F_ROUTINE|F_STATEMENT|F_EXTERNAL)

/* These are or'ed together to form an argument to a routine which 
   handles array and substring indices. */

#define INDICE_1ONLY	1
#define INDICE_WILD	2
#define INDICE_COLON	4
#define INDICE_NULL	8

/* These values are returned by the routine which gets a line from an
   input file, and determines whether its a comment, continuation, etc. */

#define LINE_NORMAL	1
#define LINE_COMMENT	2
#define LINE_CONTINUE	3
#define LINE_VCONTINUE	4	/* VMS continuation format. */
#define LINE_END	5
#define LINE_BAD	6

/* These values are used to keep track of block nesting. */

#define BLOCK_DO	1
#define BLOCK_ENDDO	2
#define BLOCK_IF	3
#define BLOCK_ELSEIF	4
#define BLOCK_ELSE	5
#define BLOCK_ENDIF	6

/* These values are used to for the statement labels. */

#define LABEL_DEF	0x01
#define LABEL_GOTO	0x02
#define LABEL_DO	0x04
#define LABEL_IO	0x08

/* These are returned by routines associated with parsing expressions. */

#define TOKEN_LEFT	 0x0001
#define TOKEN_RIGHT	 0x0002
#define TOKEN_LOGICAL	 0x0004
#define TOKEN_RELATIONAL 0x0008
#define TOKEN_ARITH	 0x0010
#define TOKEN_PLUS_MINUS 0x0020
#define TOKEN_NOT	 0x0040

#define BINARY_MASK \
	(TOKEN_LOGICAL|TOKEN_RELATIONAL|TOKEN_ARITH|TOKEN_PLUS_MINUS)
#define UNARY_MASK (TOKEN_PLUS_MINUS|TOKEN_NOT)

#define STATE_PROG_SUB_FUNC	1
#define STATE_DECL		2
#define STATE_EXEC		3
#define STATE_DATA		4
#define MAXVARLEN	8
#define MAXVAR		128
#define MAXBLOCKS	64
#define MAXLINE		132
#define MAXTLINE	(80*25)

#define HASHSIZE	701
#define SHASHSIZE	51

typedef struct statement { char *name;
			   int flags,state,length;
			   struct statement *fwd;
			   void (*parser)();} STATEMENT;

typedef struct labels { int labno,flags;
			struct labels *fwd;} LABELS;

typedef struct symbol {	char *name;
		int flags,narg,*args;
		struct symbol *fwd;} SYMBOL;

/* Declare all the routines that I use. */

private void bug(),error(),clear_hash_table(),the_end();
private void define_intrinsics(),define_specifics(),define_statements();
private void generate_output(),generate_function(),generate_line();

private void call_statement(),declaration_statement(),close_statement();
private void simple_statement(),data_statement(),do_statement(),dowhile_statement();
private void if_elseif_statement(),ignore_statement(),end_statement();
private void prog_sub_func_statement(),goto_statement(),include_statement();
private void equivalence_statement(),vms_record_statement();
private void inquire_statement(),open_statement(),parameter_statement();
private void read_write_statement(),rewind_statement(),common_statement();
private void blockdata_statement(),block_statement();
private int assignment_statement();
private void parse_line(),parse_file();

private void add_inc();
private FILE *open_inc();
private char *handle_implied_do_loop(),*handle_dio_list();
private char *handle_variable(),*handle_expression();
private char *handle_indices(),*handle_length(),*handle_label();
private char *handle_sub_func_call(),*handle_keywords(),*handle_value();
private char *handle_operator(),*handle_statement_func();
private char *get_line(),*get_name();
private char *skip_integer(),*skip_expression(),*skip_enclosed(),*skip_token();
private char *skip_numeric(),*skip_logical(),*skip_string();
private char *usage(),*crosssum();
private SYMBOL *get_keyword();
private SYMBOL *create_symbol(),*add_symbol(),*find_symbol();
private int set_variable(),inquire_variable(),set_label();
private SYMBOL *set_routine(),*inquire_routine();
private int isfunction(),issubstring(),get_arg_intent();
private void banish_hollerith(),set_block(),end_label(),end_block();
void *malloc();

#define issymbol(s) (isalnum(s) || (s) == '_' || (s) == '$' || (s) == '%')

/* Global variables. */

static int blocks[MAXBLOCKS],nblocks;
static LABELS *lhash[HASHSIZE];
static SYMBOL *vhash[HASHSIZE], *rhash[HASHSIZE];
static SYMBOL  routine;
static STATEMENT *shash[SHASHSIZE];
static int state,new_state,given,library_mode;
static int unused,initialisation,extended,redundant,declare,interweave;
static int lcheck,hollerith,twopass,cross,common;
static char *unknown = "(Unknown)";
static FILE *log,*msg;
static char errmsg[MAXLINE];
typedef struct inc_dir { struct inc_dir *fwd;
		 char *dir; } INC_DIR;
INC_DIR *dirhead;

#define ERROR(a) sprintf a;error(errmsg)
/************************************************************************/
int main(argc,argv)
int argc;
char *argv[];
{
  char *output_file,*s,*library_flag;
  int i,i0;

/*
  Initialise the hash tables.
*/
  for(i=0;i<HASHSIZE;i++){
    vhash[i] = NULL;
    rhash[i] = NULL;
    lhash[i] = NULL;
  }
  given = FALSE;
  routine.name = unknown;
  routine.narg = 0;
  routine.flags = F_VOID|F_ROUTINE;
  routine.fwd = NULL;
  state = 0;
  nblocks = 0;

/* Load the appropriate hash tables with the definitions of intrinsic and
   hash functions. */

  define_intrinsics();
  define_statements();

/* Open the log and message file. */

  log = fopen("flint.log","w");
  msg = NULL;

/* Handle any flags on the command line. */

  interweave = TRUE;		/* Warn on interwoven comments and
							continuations.	*/
  lcheck = TRUE;		/* Perform simple line checks. 		*/
  unused = TRUE;		/* Warn of unused variables. 		*/
  declare = TRUE;		/* Warn on undeclared variables.	*/
  redundant = TRUE;		/* Warn of redundant variables.		*/
  output_file = NULL;		/* No output file.			*/
  initialisation = TRUE;	/* Check variable initialisation.	*/
  extended = FALSE;		/* Use "strict" name rules.		*/
  twopass = FALSE;		/* Do not do two passes.		*/
  hollerith = FALSE;		/* Do not handle hollerith at all.	*/
  common = TRUE;		/* Check COMMON blocks.			*/
  library_flag = "-l";
  dirhead = NULL;		/* no extra include directory?		*/
  cross = FALSE;		/* generate cross reference map ?       */

  for(i=1;i<argc;i++){
    s = argv[i];
    if(*s == '-'){				/* Handle flags. */
      s++;
      i0 = i;
      argv[i] = NULL;
      while(*s)switch(*s++){
        case 'a': cross = TRUE;                 break;
	case 'c': interweave = FALSE;		break;
	case 'd': declare = FALSE;		break;
	case 'f': lcheck = FALSE;		break;
	case 'h': hollerith = TRUE;		break;
	case 'i':   /* VMS cannot distringuish 'i' from 'I' */
        case 'I': if (++i < argc) {add_inc(argv[i]); argv[i] = NULL;}
                                                break;
        case 'j': initialisation = FALSE;	break;
        case 'k': common = FALSE;		break;
	case 'l': argv[i0] = library_flag;	break;
		/* the -l switch does not process following filename ??? */
        case 'o': if(++i < argc){output_file = argv[i]; argv[i] = NULL;}
						break;
	case 'r': redundant = FALSE;		break;
        case 's': define_specifics();		break;
        case 'u': unused = FALSE;		break;
        case 'x': extended  = TRUE;		break;
	case '2': twopass = TRUE;		break;
	case '?':
	  printf("%s Version: %s\n",argv[0],VERSION_ID);
          printf("Usage: flint [-flags] files ... [-I incdir] [-l files] [-o outfile]\n");
	  printf("Flags:\n");
	  printf("  a  Make a crude list of all used variables\n");
	  printf("  c  Allow interwoven comments and continuations\n");
	  printf("  d  Do not insist on variables being explicitly declared\n");
	  printf("  f  Do not perform line checks\n");
	  printf("  h  Recognise hollerith (crude treatment)\n");
	  printf("  j  Do not check if a variable has been initialised\n");
	  printf("  k  Suppress COMMON block warning messages\n");
	  printf("  r  Do not warn about seemingly redundant variables\n");
	  printf("  s  Recognise specific and FORTRAN-IV library routines\n");
	  printf("  u  Do not warn about unused variables\n");
	  printf("  x  Allow long (extended) variable and routine names\n");
	  printf("  2  Perform two passes, to help determine intents better\n");
	  printf("  ?  Print this message\n");
	  printf("Other flags which need an argument:\n");
          printf("  I  Extra search directory for include files\n");
	  printf("  i  Equivalent to I\n");
	  printf("  l  Treat following file in library mode\n");
	  printf("  o  Generate output library file\n");
	  break;
	default: fprintf(stderr,"Unrecognised flag %c\n",*(s-1));
						break;
      } /* switch(*s++) */

/* Allow output redirection, if the invoking shell does not provide it. */

    }else if(*s == '>'){
      argv[i] = NULL;
      if(++i < argc){msg = fopen(argv[i],"w"); argv[i] = NULL;}
    }
  } /* i */

/* If doing two passes, do the first pass. */

  if(twopass){
    library_mode = TRUE;
    for(i=1;i<argc;i++)
      if(argv[i] != library_flag && argv[i] != NULL)parse_file(argv[i]);
  }

/* Do the final pass. Do not process library files a second time. */

  library_mode = FALSE;
  for(i=1;i<argc;i++){
    if(argv[i] == library_flag){
      library_mode = TRUE;
    }else if(argv[i] != NULL && (!library_mode || !twopass) ){
      parse_file(argv[i]);
      if(state != 0)the_end();
      library_mode = FALSE;
    }
  }

/* Close the log file. */

  fclose(log);

/* If the user wanted a interface file, generate it. */

  if(output_file != NULL)generate_output(output_file);
  exit(0);
}
/************************************************************************/
private void add_inc(dir)
char *dir;
/*
  Add a directory to the list of include directories to search.
------------------------------------------------------------------------*/
{
  INC_DIR *p,*q;

/* Create and initialise a new directory structure. */

  q = (INC_DIR *)malloc(sizeof(INC_DIR));
  q->fwd = NULL;
  q->dir = malloc(strlen(dir)+1);
  strcpy(q->dir,dir);

  p = dirhead;
  if(p == NULL) dirhead = q;
  else{
    while(p->fwd != NULL) p = p->fwd;
    p->fwd = q;
  }
}
/************************************************************************/
private FILE *open_inc(name)
char *name;
/*
  Open an include file (or any other file for that matter).
------------------------------------------------------------------------*/
{
  INC_DIR *p;
  FILE *stream;
  char newname[MAXVAR],c;

  stream = fopen(name,"r");

/* If we failed to open it, try in the include directory. */

  p = dirhead;
  while(stream == NULL && p != NULL){
    strcpy(newname,p->dir);
    c = *(newname + strlen(newname) - 1);
    if(isalnum(c))strcat(newname,"/");
    strcat(newname,name);
    stream = fopen(newname,"r");
    p = p->fwd;
  }
  return(stream);
}
/************************************************************************/
private void generate_output(output_file)
char *output_file;
/*
  Generate an output file.
------------------------------------------------------------------------*/
{
  int i;
  FILE *out;
  SYMBOL *p;

  out = fopen(output_file,"w");
  if(out == NULL)bug("Failed to open output file %s",output_file);
  for(i=0;i<HASHSIZE;i++){
    p = rhash[i];
    for(p = rhash[i]; p != NULL; p = p->fwd){
      if(p->flags & F_ACTUAL)generate_function(out,p);
/*      generate_function(out,p); */
    }
  }
  fclose(out);
}
/************************************************************************/
private void generate_function(fh,p)
FILE *fh;
SYMBOL *p;
/*
  Generate a function into the output file.
------------------------------------------------------------------------*/
{
  int i;
  char line[MAXVAR],temp[32];
  static struct {char *leader,*trailer; int mask,target;}
	states[] =     {{"",	     ")",  0,0},
			{"integer ",  "",   F_INTEGER,F_INTEGER},
			{"real ",     "",   F_REAL,F_REAL},
			{"double precision ","",F_DOUBLE,F_DOUBLE},
			{"complex ",  "",   F_COMPLEX,F_COMPLEX},
			{"character ","",   F_CHAR,F_CHAR},
			{"logical ",  "",   F_LOGICAL,F_LOGICAL},
			{"external ", "",   F_EXTERNAL,F_EXTERNAL},
			{"intent(out) ","", F_OUT,F_OUT},
			{"intent(in) ","",  F_IN,F_IN},
		        {"intent(unknown) ","",F_IN|F_OUT,0}};

#define NSTATES 11

  fprintf(fh,"c***************************************************************\n");
  sprintf(line,"%s %s(",usage(p->flags,temp),p->name);
  states[0].leader = line;

  if(p->narg > 0){
    for(i=0; i < NSTATES; i++)generate_line(fh, states[i].leader,
						states[i].trailer,
						states[i].mask,
						states[i].target,
						p->args,p->narg);
  }else{
    fprintf(fh,"\t%s )\n",line);
  }
  if(!(p->flags & F_VOID))fprintf(fh,"\tintent(out)%s\n",p->name);
  fprintf(fh,"\tend\n");
}
/************************************************************************/
private void generate_line(fh,leader,trailer,mask,target,flags,n)
FILE *fh;
char *leader,*trailer;
int mask,target,flags[],n;
/*
  Output a line to the summary file, in a neat FORTRAN syntax.
------------------------------------------------------------------------*/
{
  char line[MAXLINE];
  int i,nout,used;

  nout = strlen(leader);
  sprintf(line,"\t%s",leader);
  leader = line;
  used = FALSE;

  for(i=0; i < n; i++){
    if( (mask & flags[i]) == target){
      fprintf(fh,"%sa%d",leader,i+1);
      nout += ( i < 9 ? 3 : 4);
      used = TRUE;
      if(nout < 56){
	leader = ",";
      }else{
	nout = 0;
	leader = ",\n     *\t";
      }
    }
  }
  if(used)fprintf(fh,"%s\n",trailer);
}
/************************************************************************/
private void bug(a,b)
char *a,*b;
{
  fprintf(stderr,a,b); fprintf(stderr,"\n");
  perror("Flint");
  exit(0);
}
/************************************************************************/
private void error(text)
char *text;
{
  if(library_mode)return;
  if(log != NULL){
    fprintf(log,"### Warning: %s\n",text);
  }
  if(!given){
    if(msg == NULL)printf("Routine %s.\n",routine.name);
    else	  fprintf(msg,"Routine %s.\n",routine.name);
  }
  if(msg == NULL)printf(" %s\n",text);
  else          fprintf(msg," %s\n",text);
  given = TRUE;
}
/************************************************************************/
private void define_intrinsics()
/*
  Declare all the instrinsic functions.
------------------------------------------------------------------------*/
{
  int n,i;

  static int flag1[1] = {F_REAL|F_DOUBLE|F_IN};
  static int flag2[1] = {F_INTEGER|F_REAL|F_DOUBLE|F_COMPLEX|F_IN};
  static int flag3[1] = {F_REAL|F_DOUBLE|F_COMPLEX|F_IN};
  static int flag4[1] = {F_COMPLEX|F_IN};
  static int flag5[1] = {F_INTEGER|F_IN};
  static int flag6[1] = {F_CHAR|F_IN};
  static int flag7[2] = {F_INTEGER|F_REAL|F_DOUBLE|F_IN,
			 F_INTEGER|F_REAL|F_DOUBLE|F_IN};
  static int flag8[2] = {F_REAL|F_DOUBLE|F_IN,
			 F_REAL|F_DOUBLE|F_IN};
  static int flag9[2] = {F_CHAR|F_IN,
			 F_CHAR|F_IN};
  static int flag_len[1] = {F_CHAR};
#define MAXARG 8
  static int flag_minmax[MAXARG] = {F_INTEGER|F_REAL|F_DOUBLE|F_IN,
				    F_INTEGER|F_REAL|F_DOUBLE|F_IN,
				    F_INTEGER|F_REAL|F_DOUBLE|F_IN,
				    F_INTEGER|F_REAL|F_DOUBLE|F_IN,
				    F_INTEGER|F_REAL|F_DOUBLE|F_IN,
				    F_INTEGER|F_REAL|F_DOUBLE|F_IN,
				    F_INTEGER|F_REAL|F_DOUBLE|F_IN,
				    F_INTEGER|F_REAL|F_DOUBLE|F_IN};

#define def_intrinsic(name,narg,flags,args) {name,\
	flags|F_INTRINSIC|F_ROUTINE,narg,args,NULL}

  static SYMBOL intrinsics[] = {
  def_intrinsic( "aint",  1, F_GENERIC, flag1),
  def_intrinsic( "anint", 1, F_GENERIC, flag1),
  def_intrinsic( "nint",  1, F_INTEGER, flag1),
  def_intrinsic( "tan",   1, F_GENERIC, flag1),
  def_intrinsic( "log10", 1, F_GENERIC, flag1),
  def_intrinsic( "asin",  1, F_GENERIC, flag1),
  def_intrinsic( "acos",  1, F_GENERIC, flag1),
  def_intrinsic( "atan",  1, F_GENERIC, flag1),
  def_intrinsic( "sinh",  1, F_GENERIC, flag1),
  def_intrinsic( "cosh",  1, F_GENERIC, flag1),
  def_intrinsic( "tanh",  1, F_GENERIC, flag1),
  def_intrinsic( "abs",   1, F_SPECIAL|F_GENERIC, flag2),
  def_intrinsic( "real",  1, F_REAL,	flag2),
  def_intrinsic( "dble",  1, F_DOUBLE,	flag2),
  def_intrinsic( "int",   1, F_INTEGER,	flag2),
  def_intrinsic( "cos",   1, F_GENERIC,	flag3),
  def_intrinsic( "sin",   1, F_GENERIC,	flag3),
  def_intrinsic( "exp",   1, F_GENERIC,	flag3),
  def_intrinsic( "log",   1, F_GENERIC,	flag3),
  def_intrinsic( "sqrt",  1, F_GENERIC,	flag3),
  def_intrinsic( "aimag", 1, F_REAL,	flag4),
  def_intrinsic( "conjg", 1, F_COMPLEX,	flag4),
  def_intrinsic( "char",  1, F_CHAR,	flag5),
  def_intrinsic( "ichar", 1, F_INTEGER,	flag6),
  def_intrinsic( "len",   1, F_INTEGER,	flag_len),
  def_intrinsic( "mod",   2, F_GENERIC,	flag7),
  def_intrinsic( "dim",   2, F_GENERIC,	flag7),
  def_intrinsic( "sign",  2, F_GENERIC,	flag7),
  def_intrinsic( "min",   2, F_SPECIAL|F_GENERIC,	flag_minmax),
  def_intrinsic( "max",   2, F_SPECIAL|F_GENERIC,	flag_minmax),
  def_intrinsic( "cmplx", 2, F_SPECIAL|F_COMPLEX,	flag7),
  def_intrinsic( "atan2", 2, F_GENERIC,	flag8),
  def_intrinsic( "index", 2, F_INTEGER,	flag9),
  def_intrinsic( "lle",   2, F_LOGICAL, flag9),
  def_intrinsic( "llt",   2, F_LOGICAL, flag9),
  def_intrinsic( "lge",   2, F_LOGICAL, flag9),
  def_intrinsic( "lgt",   2, F_LOGICAL, flag9)};

/* Add all these intrinsic functions to the standard functions list. */

  n = sizeof(intrinsics)/sizeof(SYMBOL);
  for(i=0; i < n; i++)(void)add_symbol(&intrinsics[i],rhash);
}
/************************************************************************/
private void define_specifics()
/*
  Declare the specific names for the intrinsic functions.
------------------------------------------------------------------------*/
{
  int n,i;

  static int flag_i1[1] =      {F_INTEGER|F_IN};
  static int flag_r1[1] =      {F_REAL|F_IN};
  static int flag_d1[1] =      {F_DOUBLE|F_IN};
  static int flag_c1[1] =      {F_COMPLEX|F_IN};
  static int flag_i2[2] =      {F_INTEGER|F_IN,  F_INTEGER|F_IN};
  static int flag_r2[2] =      {F_REAL   |F_IN,  F_REAL   |F_IN};
  static int flag_d2[2] =      {F_DOUBLE |F_IN,  F_DOUBLE |F_IN};
  static int flag_ix[MAXARG] = {F_INTEGER|F_IN,  F_INTEGER|F_IN,
				F_INTEGER|F_IN,  F_INTEGER|F_IN,
				F_INTEGER|F_IN,  F_INTEGER|F_IN,
				F_INTEGER|F_IN,  F_INTEGER|F_IN};
  static int flag_rx[MAXARG] = {F_REAL   |F_IN,  F_REAL   |F_IN,
				F_REAL   |F_IN,  F_REAL   |F_IN,
				F_REAL   |F_IN,  F_REAL   |F_IN,
				F_REAL   |F_IN,  F_REAL   |F_IN};
  static int flag_dx[MAXARG] = {F_DOUBLE |F_IN,  F_DOUBLE |F_IN,
				F_DOUBLE |F_IN,  F_DOUBLE |F_IN,
				F_DOUBLE |F_IN,  F_DOUBLE |F_IN,
				F_DOUBLE |F_IN,  F_DOUBLE |F_IN};

  static SYMBOL intrinsics[] = {
  def_intrinsic( "dsqrt",   1,	F_DOUBLE,  flag_d1),
  def_intrinsic( "csqrt",   1,	F_COMPLEX, flag_c1),
  def_intrinsic( "alog",    1,	F_REAL,    flag_r1),
  def_intrinsic( "dlog",    1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "clog",    1,  F_COMPLEX, flag_c1),
  def_intrinsic( "alog10",  1,  F_REAL,    flag_r1),
  def_intrinsic( "dlog10",  1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "dexp",    1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "cexp",    1,  F_COMPLEX, flag_c1),
  def_intrinsic( "dsin",    1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "csin",    1,  F_COMPLEX, flag_c1),
  def_intrinsic( "dcos",    1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "ccos",    1,  F_COMPLEX, flag_c1),
  def_intrinsic( "dtan",    1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "dasin",   1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "dacos",   1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "datan",   1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "datan2",  2,  F_DOUBLE,  flag_d2),
  def_intrinsic( "dsinh",   1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "dcosh",   1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "dtanh",   1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "dabs",    1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "cabs",    1,  F_REAL,    flag_c1),
  def_intrinsic( "iabs",    1,  F_INTEGER, flag_i1),
  def_intrinsic( "idint",   1,  F_INTEGER, flag_d1),
  def_intrinsic( "dint",    1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "idnint",  1,  F_INTEGER, flag_d1),
  def_intrinsic( "dnint",   1,  F_DOUBLE,  flag_d1),
  def_intrinsic( "sngl",    1,  F_REAL,    flag_d1),
  def_intrinsic( "ifix",    1,  F_INTEGER, flag_r1),
  def_intrinsic( "float",   1,  F_REAL,    flag_i1),
  def_intrinsic( "amax1",   1,  F_REAL|F_SPECIAL,    flag_rx),
  def_intrinsic( "dmax1",   1,  F_DOUBLE|F_SPECIAL,  flag_dx),
  def_intrinsic( "max0",    1,  F_INTEGER|F_SPECIAL, flag_ix),
  def_intrinsic( "max1",    1,  F_INTEGER|F_SPECIAL, flag_rx),
  def_intrinsic( "amax0",   1,  F_REAL|F_SPECIAL,    flag_ix),
  def_intrinsic( "amin1",   1,  F_REAL|F_SPECIAL,    flag_rx),
  def_intrinsic( "dmin1",   1,  F_DOUBLE|F_SPECIAL,  flag_dx),
  def_intrinsic( "min0",    1,  F_INTEGER|F_SPECIAL, flag_ix),
  def_intrinsic( "min1",    1,  F_INTEGER|F_SPECIAL, flag_rx),
  def_intrinsic( "amin0",   1,  F_REAL|F_SPECIAL,    flag_ix),
  def_intrinsic( "ddim",    2,  F_DOUBLE,  flag_d2),
  def_intrinsic( "idim",    2,  F_INTEGER, flag_i2),
  def_intrinsic( "amod",    2,  F_REAL,    flag_r2),
  def_intrinsic( "dmod",    2,  F_DOUBLE,  flag_d2),
  def_intrinsic( "dsign",   2,  F_DOUBLE,  flag_d2),
  def_intrinsic( "isign",   2,  F_INTEGER, flag_i2)};


/* Add all these specific functions to the standard functions list. */

  n = sizeof(intrinsics)/sizeof(SYMBOL);
  for(i=0; i < n; i++)(void)add_symbol(&intrinsics[i],rhash);
}
/************************************************************************/
private void define_statements()
/*
  Fill in the hash table used to store the valid FORTRAN statements.
------------------------------------------------------------------------*/
{
#define def_statement(name,routine,flag,state) \
	{name,flag,state,sizeof(name)-1,NULL,routine}

  char *s;
  int i,hashval;
  static STATEMENT fortstat[]={
    def_statement("backspace(",rewind_statement,0,STATE_EXEC),
    def_statement("blockdata",blockdata_statement,0,STATE_PROG_SUB_FUNC),
    def_statement("byte",declaration_statement,F_SPECIAL|F_INTEGER,STATE_DECL),
    def_statement("call",call_statement,0,STATE_EXEC),
    def_statement("character",declaration_statement,F_CHAR,STATE_DECL),
    def_statement("close(",close_statement,0,STATE_EXEC),
    def_statement("common",common_statement,F_COMMON,STATE_DECL),
    def_statement("complex",declaration_statement,F_COMPLEX,STATE_DECL),
    def_statement("continue",simple_statement,0,STATE_EXEC),
    def_statement("data",data_statement,0,STATE_DATA),
    def_statement("do",do_statement,0,STATE_EXEC),
    def_statement("double",declaration_statement,F_SPECIAL|F_DOUBLE,STATE_DECL),
    def_statement("doublecomplex",declaration_statement,F_COMPLEX,STATE_DECL),
    def_statement("doubleprecision",declaration_statement,F_DOUBLE,STATE_DECL),
    def_statement("dowhile(",dowhile_statement,0,STATE_EXEC),
    def_statement("dimension",declaration_statement,F_ARRAY,STATE_DECL),
    def_statement("else",block_statement,BLOCK_ELSE,STATE_EXEC),
    def_statement("elseif(",if_elseif_statement,0,STATE_EXEC),
    def_statement("end",end_statement,0,0),
    def_statement("enddo",block_statement,BLOCK_ENDDO,STATE_EXEC),
    def_statement("endif",block_statement,BLOCK_ENDIF,STATE_EXEC),
    def_statement("endfile(",rewind_statement,0,STATE_EXEC),
    def_statement("endmap",vms_record_statement,0,STATE_DECL),
    def_statement("endstructure",vms_record_statement,0,STATE_DECL),
    def_statement("endunion",vms_record_statement,0,STATE_DECL),
    def_statement("equivalence(",equivalence_statement,0,STATE_DECL),
    def_statement("external",declaration_statement,F_EXTERNAL,STATE_DECL),
    def_statement("format(",ignore_statement,0,STATE_EXEC),
    def_statement("function",prog_sub_func_statement,0,STATE_PROG_SUB_FUNC),
    def_statement("goto",goto_statement,0,STATE_EXEC),
    def_statement("if(",if_elseif_statement,0,STATE_EXEC),
    def_statement("implicit",ignore_statement,0,STATE_DECL),
    def_statement("include\'",include_statement,0,0),
    def_statement("inquire(",inquire_statement,0,STATE_EXEC),
    def_statement("integer",declaration_statement,F_INTEGER,STATE_DECL),
    def_statement("intent(in)",declaration_statement,F_IN,STATE_DECL),
    def_statement("intent(out)",declaration_statement,F_OUT,STATE_DECL),
    def_statement("intent(unknown)",declaration_statement,F_POUT,STATE_DECL),
    def_statement("intrinsic",declaration_statement,F_EXTERNAL,STATE_DECL),
    def_statement("logical",declaration_statement,F_LOGICAL,STATE_DECL),
    def_statement("map",vms_record_statement,0,STATE_DECL),
    def_statement("open(",open_statement,0,STATE_EXEC),
    def_statement("parameter",parameter_statement,0,STATE_DECL),
    def_statement("program",prog_sub_func_statement,F_VOID,STATE_PROG_SUB_FUNC),
    def_statement("read(",read_write_statement,F_OUT,STATE_EXEC),
    def_statement("real",declaration_statement,F_REAL,STATE_DECL),
    def_statement("record/",vms_record_statement,0,STATE_DECL),
    def_statement("return",simple_statement,0,STATE_EXEC),
    def_statement("rewind(",rewind_statement,0,STATE_EXEC),
    def_statement("save",declaration_statement,F_SAVE,STATE_DECL),
    def_statement("structure/",vms_record_statement,0,STATE_DECL),
    def_statement("subroutine",prog_sub_func_statement,F_VOID,STATE_PROG_SUB_FUNC),
    def_statement("stop",simple_statement,0,STATE_EXEC),
    def_statement("union",vms_record_statement,0,STATE_DECL),
    def_statement("write(",read_write_statement,F_IN,STATE_EXEC)};

#define N_STATEMENTS (sizeof(fortstat)/sizeof(STATEMENT))

  for(i=0; i < SHASHSIZE; i++)shash[i] = NULL;

  for(i=0; i < N_STATEMENTS; i++){
    s = fortstat[i].name;
    hashval = (*s - 'a') + (*(s+1) - 'a');
    fortstat[i].fwd = shash[hashval];
    shash[hashval] = &fortstat[i];
  }
}
/************************************************************************/
private void parse_file(name)
char *name;
/*
  Process this particular file. Open the file, then loop through it,
  reading full lines of FORTRAN, then call the routine to parse the
  line.
------------------------------------------------------------------------*/
{
  char line[MAXLINE],tline[MAXTLINE],*in,*out;
  FILE *stream;
  int lines,flag,echo,quotes,column;

  echo = (log != NULL && !library_mode);
  stream = open_inc(name);
  if (stream==NULL) {
     ERROR((errmsg,"Error opening %s.",name));
     return;
  }

  in = get_line(line,MAXLINE,stream,&flag,&column);
  while(flag != LINE_END){

/* Loop around getting an input line, append it to the total line, converting
   to lower case, and stripping out blanks as we go. */

    if(flag == LINE_NORMAL){
      lines = 0;
      out = tline;
      do{
	if(echo)fputs(line,log);
	if(flag == LINE_VCONTINUE) error("VMS format continuation");
        if(hollerith)banish_hollerith(in);
	if(lines++ < 20){
	  quotes = 0;
	  while(*in){
	    if(*in == '\t')column = 8*(column/8) + 7;
	    else if(isspace(*in));
	    else if(isupper(*in))*out++ = *in - 'A' + 'a';
	    else if(*in == '\'') {*out++ = *in; quotes++;}
	    else if(*in == '!' && !(quotes % 2)){
	      if(lcheck)error("Exclamation comment on line");
	      *(in+1) = 0;
	    } else                 *out++ = *in;
	    in++;
	    column++;
	  }
	}
	column--;		/* Ignore the \n at the end of the line. */
	if(quotes % 2 && lcheck)error("Odd number of quotes on line");
	if(column > 72 && lcheck)error("Line longer than 72 characters");
	in = get_line(line,MAXLINE,stream,&flag,&column);
	while(!interweave && flag == LINE_COMMENT)
		in = get_line(line,MAXLINE,stream,&flag,&column);
      }while(flag == LINE_CONTINUE || flag == LINE_VCONTINUE);
      *out = '\0';
      if(lines > 20)error("Too many continuations -- line ignored");
      else parse_line(tline);

/* Comments and spurious continuation lines. */

    }else if(flag != LINE_END){
      if(flag == LINE_CONTINUE || flag == LINE_VCONTINUE || flag == LINE_BAD)
        error("Badly formed line ignored");
      if(echo)fputs(line,log);
      in = get_line(line,MAXLINE,stream,&flag,&column);
    }
  }
  fclose(stream);
}
/************************************************************************/
private char *get_line(line,maxline,stream,flag,colnum)
char *line;
FILE *stream;
int maxline,*flag,*colnum;
/*
  Get a line of FORTRAN, skip over leading blanks or statement labels,
  and determine whether its a continuation line of not. Ignore comments.
------------------------------------------------------------------------*/
{
  char *s,*p;
  int column,labno;

/* Get a line, and return immediately if its an eof. */

  *flag = LINE_END;
  s = fgets(line,maxline,stream);
  if(s == NULL)return(s);

  if(*s == '\t' && isdigit(*(s+1))){
    *flag = LINE_VCONTINUE;
    s += 2;
    column = 10;
  } else {
    column = 1;
    while(isspace(*s)){
      if(*s == '\t')column = 8*((column-1)/8+1) + 1;
      else column++;
      s++;
    }
  }

  if(*flag == LINE_VCONTINUE){;
  }else if(column == 6 && *s){
    *flag = LINE_CONTINUE;
    s++;
  }else if((column == 1 && (*s=='C' || *s=='c' || *s=='*' || *s=='#'))){
    *flag = LINE_COMMENT;
  }else{
    labno = 0;
    while(column < 6 && isdigit(*s)){
      column++;
      labno = 10*labno + *s - '0';
      s++;
    }
    while(isspace(*s)){
      if(*s == '\t')column = 8*((column-1)/8+1) + 1;
      else column++;
      s++;
    }
    if(!*s || *s == '!')	       *flag = LINE_COMMENT;
    else if(column >= 7 && isalpha(*s))*flag = LINE_NORMAL;
    else 			       *flag = LINE_BAD;
  }

/* If its a normal or continuation line, remove trailing white chars.
   There is at least one non-white char in the line. */

  if(*flag == LINE_NORMAL || *flag == LINE_CONTINUE || *flag == LINE_VCONTINUE){
    p = s + strlen(s);
    while(isspace(*(p-1)))p--;
    *p++ = '\n'; *p++ = '\0';
  }
  *colnum = column - 1;
  if(*flag == LINE_NORMAL && labno > 0) (void)set_label(labno,LABEL_DEF);
  return(s);
}
/************************************************************************/
private void parse_line(s)
char *s;
/*
  Determine what sort of FORTRAN statement this line represents,
  then call the appropriate routine to proccess it.
------------------------------------------------------------------------*/
{
  int isexp,hashval;
  char *sd;
  STATEMENT *p;

/* Check to see if this is an assignment statement. */

  isexp = FALSE;
  sd = s;
  if(isalpha(*sd)){
    while(issymbol(*sd))sd++;
    if(*sd == '(')sd = skip_expression(sd);
    if(*sd == '(')sd = skip_expression(sd);
    if(*sd == '='){
      sd = skip_expression(sd+1);
      isexp = !(*sd);
    }
  }

/* If its an assignment statement, process it. */

  if(isexp){
    new_state = assignment_statement(s);

/* Otherwise determine the statements hash code, look it up in the
   hash table, and then process it. */

  }else{
    hashval = max(min((*s - 'a') + (*(s+1) - 'a'), SHASHSIZE-1),0);
    for(p=shash[hashval]; p != NULL; p = p->fwd)
      if(!strncmp(s,p->name,p->length))break;
    if(p == NULL){
      error("Unrecognised statement");
      new_state = 0;
    } else {
      new_state = p->state;
      (*(p->parser))(s,p);
    }
  }
/*
  Check if the type of statement we just analysed is out of order.
*/
  if((state != 0 && new_state == STATE_PROG_SUB_FUNC) ||
     (state == STATE_DATA && new_state == STATE_DECL) ||
     (state == STATE_EXEC && new_state == STATE_DECL) ||
     (state == STATE_EXEC && new_state == STATE_DATA))
    error("Statement is out of standard order");
  if(new_state != 0) state = new_state;
}
/************************************************************************/
private void common_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a COMMON statement
------------------------------------------------------------------------*/
{
  int flags,flag0,align_given,mixed_given,decl_given;
  char variable[MAXVAR];

  s += f->length;
  if( *s == '/'){
    s = get_name(s+1,variable);
    if( *s == '/')s++;
    else error("Bad COMMON statement");
  }
  align_given = mixed_given = decl_given = !common;
  flags = 0;
  while(*s){
    s = get_name(s,variable);
    if(*variable){
      flag0 = set_variable(variable,F_COMMON);
      flags |= flag0;
      if(*s == '('){
	if(!decl_given){
	  error("Array declaration in common block.");
	  decl_given = TRUE;
	}
	(void)set_variable(variable,F_ARRAY);
	s = handle_indices(s,INDICE_WILD|INDICE_COLON);
      }
      if(!align_given && (flag0 & F_DOUBLE) &&
	 (flags & (F_INTEGER|F_REAL|F_COMPLEX|F_LOGICAL))){
	error("Possible COMMON block alignment problem.");
	align_given = TRUE;
      }
      if(!mixed_given && (flags & F_CHAR) &&
	 (flags & (F_INTEGER|F_REAL|F_DOUBLE|F_COMPLEX|F_LOGICAL))){
	error("COMMON block mixes character and numeric variables.");
	mixed_given = TRUE;
      }
    }else{
      error("Bad variable name");
      break;
    }

/* Handle whatever is beyond the variable name. */

    if( *s == ',')
      s++;
    else if( *s != '\0'){
      error("Unexpected characters");
      break;
    }
  }

/* We have passed through the common. Check to see if there is a mix of
   numeric and character data in the common. */
}
/************************************************************************/
private void declaration_statement(s,f)
char *s;
STATEMENT *f;
/*
	Handle declaration and COMMON statements.
------------------------------------------------------------------------*/
{
  int flags;
  char variable[MAXVAR];
  STATEMENT statement;

  s += f->length;
  if(!extended &&
    ((f->flags & F_SPECIAL) || ((*s == '*') && !(f->flags & F_CHAR))))
    error("Non-ANSI declaration statement");
  if(*s == '*')s = handle_length(s);
  
  if(!strcmp(f->name,"doublecomplex")) error("Non-ANSI declaration statement");

/* Check if its a function declaration. */

  new_state = STATE_DECL;
  if(isfunction(s,f)){
    new_state = STATE_PROG_SUB_FUNC;
    statement.name = f->name;
    statement.flags = f->flags;
    statement.length = 8;
    prog_sub_func_statement(s,&statement);
  }else while(*s){
    s = get_name(s,variable);
    flags = f->flags;
    if( *s == '('){
      s = handle_indices(s,INDICE_WILD|INDICE_COLON);
      flags |= F_ARRAY;
    }
    if(*variable)
      (void)set_variable(variable,flags);
    else{
      error("Bad variable name");
      break;
    }

/* Handle whatever is beyond the variable name. */

    if(f->flags & F_CHAR)s = handle_length(s);
    if( *s == ',')
      s++;
    else if( *s != '\0'){
      error("Unexpected characters");
      break;
    }
  }
}
/************************************************************************/
private int issubstring(s)
char *s;
/*
  Determine if the following bracketting looks like substring specs.
------------------------------------------------------------------------*/
{
  int nesting;
  if(*s++ != '(')return(FALSE);
  nesting = 0;
  while( *s && (nesting || (*s != ':' && *s != ')'))){
    if(*s == '(')nesting++;
    else if(*s == ')')nesting--;
    else if(*s == '\''){
      s++;
      while(*s && (*s != '\''))s++;
      if(!*s)s--;
    }
    s++;
  }
  return(*s == ':');
}
/************************************************************************/
private int isfunction(s,f)
char *s;
STATEMENT *f;
/*
  Determine if we are looking at a FUNCTION declaration.
------------------------------------------------------------------------*/
{
  if( (state != 0) || !(f->flags & TYPE_MASK)
      || strncmp("function",s,8)     || !isalpha(*(s+8)) )return(FALSE);

  s += 9;
  while(issymbol(*s))s++;
  if( *s++ != '(')return(FALSE);
  if(isalpha(*s) || *s == ')' )return(TRUE);
  return(FALSE);
}
/************************************************************************/
private void parameter_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the parameter statement.
------------------------------------------------------------------------*/
{
  char name[MAXVAR];
  int type,dec;

  s += f->length;
  dec = (*s != '(');
  if(dec)s--;
  do{
    s = get_name(s+1,name);
    if(*name && *s++ == '='){
      s = handle_expression(s,&type);
      if(type)(void)set_variable(name,(dec?type|F_PARAMETER:F_PARAMETER));
    }
  }while(*s == ',');
  if(!dec && *s == ')')s++;
  if(*s)error("Bad PARAMETER statement");
  else if(dec)error("Non-ANSI PARAMETER statement");
}
/************************************************************************/
private void block_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle various block statements. Its pretty trivial.
------------------------------------------------------------------------*/
{
  s += f->length;
  if(*s)error("Unexpected characters at end of line");
  set_block(f->flags);
}
/************************************************************************/
private void do_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a DO statement (either the normal or VAX forms).
------------------------------------------------------------------------*/
{
  char *sd,name[MAXVAR];
  int type;

  s += f->length;
  if(isdigit(*s))s = handle_label(s,LABEL_DO);
  else set_block(BLOCK_DO);
  sd = s;
  s = handle_variable(s,name);
  if(name[0] && *s == '='){
    (void)set_variable(name,F_OUT);
    type = set_variable(name,F_IN);
    if(!(type & F_INTEGER))error("DO loop variable is not an integer");
    do{
      sd = s+1;
      s = handle_expression(sd,&type);
      if(!(type & F_INTEGER))error("DO loop expression is not integer valued");
    }while(s != sd && *s == ',');
    if(*s)error("Bad DO statement");
  }else{
    error("Unrecognized statement");
  }
}
/************************************************************************/
private void open_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the OPEN statement.
------------------------------------------------------------------------*/
{
#define def_key(name,flags) {name,flags,0,NULL,NULL}
  static char *defaults[]={"unit"};
  static SYMBOL keys[]={
    def_key("access",	F_CHAR|F_IN),
    def_key("blank",	F_CHAR|F_IN),
    def_key("err",	F_INTEGER|F_IN),
    def_key("file",	F_CHAR|F_IN),
    def_key("form",	F_CHAR|F_IN),
    def_key("iostat",	F_INTEGER|F_OUT),
    def_key("recl",	F_INTEGER|F_IN),
    def_key("status",	F_CHAR|F_IN),
    def_key("unit",	F_INTEGER|F_IN)};
  s = handle_keywords(s + f->length - 1,
			keys, sizeof(keys)/sizeof(SYMBOL),
			defaults,sizeof(defaults)/sizeof(char *));
  if(*s)error("Bad OPEN statement");
}
/************************************************************************/
private void close_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a CLOSE statement.
------------------------------------------------------------------------*/
{
  static char *defaults[]={"unit","err","iostat"};
  static SYMBOL keys[]={
    def_key("err",	F_INTEGER|F_IN),
    def_key("iostat",	F_INTEGER|F_OUT),
    def_key("status",	F_CHAR|F_IN),
    def_key("unit",	F_INTEGER|F_IN)};

  s = handle_keywords(s + f->length - 1,
			keys, sizeof(keys)/sizeof(SYMBOL),
			defaults,sizeof(defaults)/sizeof(char *));
  if(*s)error("Bad CLOSE statement");
}
/************************************************************************/
private void rewind_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a REWIND, BACKSPACE and ENDFILE statement.
------------------------------------------------------------------------*/
{
  static char *defaults[]={"unit","err","iostat"};
  static SYMBOL keys[]={
    def_key("err",	F_INTEGER|F_IN),
    def_key("iostat",	F_INTEGER|F_OUT),
    def_key("unit",	F_INTEGER|F_IN)};

  s = handle_keywords(s + f->length - 1,
			keys, sizeof(keys)/sizeof(SYMBOL),
			defaults,sizeof(defaults)/sizeof(char *));
  if(*s){ERROR((errmsg,"Bad %s statement.",f->name));}
}
/************************************************************************/
private void inquire_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a CLOSE statement.
------------------------------------------------------------------------*/
{
  static char *defaults[]={"unit"};
  static SYMBOL keys[]={
    def_key("access",	F_CHAR|F_OUT),
    def_key("blank",	F_CHAR|F_OUT),
    def_key("direct",	F_CHAR|F_OUT),
    def_key("err",	F_INTEGER|F_OUT),
    def_key("exist",	F_LOGICAL|F_OUT),
    def_key("file",	F_CHAR|F_IN),
    def_key("form",	F_CHAR|F_OUT),
    def_key("formatted",F_CHAR|F_OUT),
    def_key("iostat",	F_INTEGER|F_OUT),
    def_key("name",	F_CHAR|F_OUT),
    def_key("named",	F_LOGICAL|F_OUT),
    def_key("nextrec",	F_INTEGER|F_OUT),
    def_key("number",	F_INTEGER|F_OUT),
    def_key("opened",	F_LOGICAL|F_OUT),
    def_key("recl",	F_INTEGER|F_OUT),
    def_key("sequential", F_CHAR|F_OUT),
    def_key("unformatted",F_CHAR|F_OUT),
    def_key("unit",	F_INTEGER|F_IN)};

  s = handle_keywords(s + f->length - 1,
			keys, sizeof(keys)/sizeof(SYMBOL),
			defaults,sizeof(defaults)/sizeof(char *));
  if(*s)error("Bad INQUIRE statement");
}
/************************************************************************/
private void read_write_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle either a READ or WRITE statement.
------------------------------------------------------------------------*/
{
  int n;
  SYMBOL *p;

  static char *defaults[]={"unit","fmt","iostat","err","end"};
  static SYMBOL keys[]={
    def_key("end",	F_INTEGER|F_IN),
    def_key("err",	F_INTEGER|F_IN),
    def_key("fmt",	F_CHAR|F_INTEGER|F_IN),
    def_key("iostat",	F_INTEGER|F_OUT),
    def_key("rec",	F_INTEGER|F_IN),
    def_key("unit",	F_CHAR|F_INTEGER)};

  n = sizeof(keys)/sizeof(SYMBOL);
  p = &keys[n-1];
  p->flags = (TYPE_MASK & p->flags) | (IO_MASK ^ f->flags);
  s = handle_keywords(s + f->length - 1,
			keys, sizeof(keys)/sizeof(SYMBOL),
			defaults,sizeof(defaults)/sizeof(char *));

/* Handle the variables following READ/WRITE statement. This does not
   handle implied do loops. */

  s = handle_dio_list(s,f->flags);
  if(*s)error("Bad READ or WRITE statement");
}
/************************************************************************/
private void data_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a DATA statement.
------------------------------------------------------------------------*/
{
  char *sd;

  s += f->length;
  while( *s){
    sd = s;
    s = handle_dio_list(s,F_DATA);
    if(sd == s){
      error("Bad DATA statement");
      break;
    }else{
      if(*s == '/'){
	s++;
	while(*s != '/'  && *s ){if(*s == '\'')s = skip_string(s); else s++;}
      }
      if(*s == '/')s++;
      if(*s == ',')s++;
    }
  }
}
/************************************************************************/
private void call_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the call statement.
------------------------------------------------------------------------*/
{
  int type;
  s = handle_sub_func_call(s+f->length,F_VOID,&type);
  if(*s)error("Bad CALL syntax");
}  
/************************************************************************/
private int assignment_statement(s)
char *s;
/*
  Handle an assignment statement.
------------------------------------------------------------------------*/
{
  char variable[MAXVAR];
  int new_state,type,flags;

  s = handle_variable(s,variable);

  if(! *variable){
    new_state = (state == STATE_DATA ? STATE_DATA : STATE_DECL);
    s = handle_statement_func(s,variable,&type);
  } else if(*s == '='){
    s = handle_expression(s+1,&type);
    new_state = STATE_EXEC;
  }

  if(*s) {
   error("Bad assignment statement");
   new_state = 0;
  } else {
    flags = set_variable(variable,F_OUT);
    switch(type){
     case F_REAL:
     case F_INTEGER:
     case F_COMPLEX:
     case F_DOUBLE:	flags &= F_REAL|F_INTEGER|F_COMPLEX|F_DOUBLE; break;
     case F_LOGICAL:	flags &= F_LOGICAL;			      break;
     case F_CHAR:	flags &= F_CHAR;			      break;
    }
    if(!flags)error("Assignment type mismatch");
  }
  return(new_state);
}
/************************************************************************/
private void end_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the END statement. Find all the variables that have not been used.
------------------------------------------------------------------------*/
{

/* Check that this is a well formed END statement. */

  s += f->length;
  if(*s)error("Bad END statement");
  state = 0;
  the_end();
}
/************************************************************************/
private void the_end()
{
  SYMBOL *p, *q;
  int i,flags;
  char line[MAXLINE];

  if(state != 0)error("Missing END statement?");

/* Check up on the statement labels. */

  end_label();

/* Check up on the block structuring. */

  end_block();

/* See which variables have not been used or may be redundant. */

  if(unused || redundant){
    for(i=0;i<HASHSIZE;i++){
      p = vhash[i];
      while(p != NULL){
        if( !(p->flags & (F_COMMON|F_PARAMETER|F_EQUIV))){
	  if(!(p->flags & (IO_MASK|F_POUT)) ){
	    if(unused && !(p->flags & F_ARG))
	      {ERROR((errmsg,"Variable %s was never used.",p->name));}
	    else if(redundant && (p->flags & F_ARG))
	      {ERROR((errmsg,"Variable %s may be redundant.",p->name));}
	    }
	  else if( redundant && (p->flags & F_OUT) && 
	    !(p->flags & (F_IN|F_POUT|F_ARG|F_SUBARG)) )
	    {ERROR((errmsg,"Variable %s may be redundant.",p->name));}
        }
        /* check if variable has name of intrinsic function */
        if ( (q=find_symbol(p->name,rhash)) ) {  /* check if its a routine */
            if ( (q->flags & F_INTRINSIC) &&
                 !(p->flags & F_EXTERNAL) ) {
              {ERROR((errmsg,"Variable %s is also an intrinsic function.",
                    p->name));}
            }
        }
        p = p->fwd;
      }
    }
  }

/*  Generate cross reference map */

  if (cross && log) {
    fprintf(log,
    "------------------------------------------------------------------------\n");
    fprintf(log,"  Simple cross-reference map:\n");
    fprintf(log,"  ===========================\n");
    for(i=0;i<HASHSIZE;i++){
      p = vhash[i];
      while(p != NULL){
         fprintf(log,"  %-8s %s\n",p->name,crosssum(p->flags,line));
         p = p->fwd;
      }
    }
    fprintf(log,
    "------------------------------------------------------------------------\n");
  }

/* Send in what we seem to think the definition of the routine is. */

  if(routine.name != unknown){
    routine.flags |= (library_mode ? 0 : F_ACTUAL);
    for(p=routine.fwd;  p != NULL; p = p->fwd){
      flags = set_variable(p->name,0);
      if(!(flags & F_PIN))flags &= (~F_IN);
      else if(flags & F_POUT)flags &= (~IO_MASK);
      p->flags = ((TYPE_MASK|IO_MASK|ROUTE_MASK) & flags);
    }
    (void)set_routine(&routine);
  }

/* Now delete the table of symbols and externals. */

  state = 0;
  given = FALSE;
  routine.name = unknown;
  routine.narg = 0;
  clear_hash_table(&(routine.fwd),1);
  clear_hash_table(vhash,HASHSIZE);
}
/************************************************************************/
private void dowhile_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the dowhile statement.
------------------------------------------------------------------------*/
{
  int type;
  s = handle_expression(s+f->length-1,&type);
  if(!(type&F_LOGICAL))error("DOWHILE expression is not logical valued");
  set_block(BLOCK_DO);
  if(*s) error("Failed to find end of expression");
}
/************************************************************************/
private void if_elseif_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the IF statement.
------------------------------------------------------------------------*/
{
  char *s0;
  int type;

  s0 = s + f->length - 1;
  s = handle_expression(s0,&type);

/* Check for an error. */

  if(s == s0){
    error("Bad IF or ELSEIF expression");

/* Ignore the labels of an arithmetic IF statement. */

  }else if(*(f->name) == 'i' && isdigit(*s)){;

/* Handle logical IF or ELSEIF statement. */

  }else{
    if(!(type&F_LOGICAL))error("IF or ELSEIF expression is not logical valued");
    if(!strcmp(s,"then"))set_block(*f->name == 'i' ? BLOCK_IF : BLOCK_ELSEIF);
    else if(!(*s) || *(f->name) == 'e')error("Bad IF or ELSEIF statement");
    else parse_line(s);
  }
}
/************************************************************************/
private void goto_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the goto statement.
------------------------------------------------------------------------*/
{
  int type;

  s += f->length;
  if(*s == '('){
    do{
      s++;
      s = handle_label(s,LABEL_GOTO);
    }while(*s == ',');
    if(*s++ != ')')error("Bad computed GOTO statement");
    else {
      if(*s == ',')s++;
      s = handle_expression(s,&type);
      if(!(type & F_INTEGER))error("Non-integer expression for computed goto");
    }
  }else{
    s = handle_label(s,LABEL_GOTO);
  }
  if(*s)error("Bad GOTO statement");
}
/************************************************************************/
private void blockdata_statement(s,f)
char *s;
STATEMENT *f;
/*----------------------------------------------------------------------*/
{
  if( *(s+f->length) )error("Extra characters after BLOCK DATA statement");
}
/************************************************************************/
private void prog_sub_func_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a PROGRAM, SUBROUTINE or FUNCTION statement.
------------------------------------------------------------------------*/
{
  char name[MAXVAR];
  SYMBOL *p;

  if(state != 0)the_end();

  s = get_name(s + f->length,name);
  if(*name){
    routine.name = malloc(strlen(name)+1);
    strcpy(routine.name,name);
    routine.narg = 0;
    routine.flags = F_ROUTINE|f->flags;
    if(!(F_VOID & f->flags))
      (void)set_variable(routine.name,routine.flags|F_ARG);
    p = &routine;
    if(*s == '(' && *(f->name) != 'p'){
      do{
        s = get_name(s+1,name);
        if(name[0]){
	  p->fwd  = (SYMBOL *)malloc(sizeof(SYMBOL));
	  p = p->fwd;
          p->name = malloc(strlen(name)+1);
          strcpy(p->name,name);
          (void)set_variable(name,F_PIN|F_ARG);
          p->fwd = NULL;
	  p->narg = 0;
          routine.narg++;
        } else if(p->narg > 0 || *s != ')'){
	  error("Bad or missing routine argument");
        }
      }while(*s == ',');
    }
    if(*s == ')')s++;
  }
  if(*s) error("Bad SUBROUTINE or FUNCTION statement");
}
/************************************************************************/
private void vms_record_statement(s,f)
char *s;
STATEMENT *f;
/*
  Give warning about VMS record declarations.
------------------------------------------------------------------------*/
{
  error("VMS record structures are not supported");
}
/************************************************************************/
private void equivalence_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the EQUIVALENCE statement.
------------------------------------------------------------------------*/
{
  int flags,ok;
  char variable[MAXVAR];

  s += f->length;
  ok = TRUE;
  while(*s != 0){
    s = get_name(s,variable);
    if(*variable == 0)break;
    flags = F_EQUIV;
    if( *s == '('){
      s = handle_indices(s,0);
      flags |= F_ARRAY;
    }
    (void)set_variable(variable,flags);

/* Handle whatever is beyond the variable name. */

    if( *s == ',')s++;
    else if( *s == ')'){
      s++;
      if(*s == ',') {
	if(*(s+1) == '(')s += 2;
        else break;
      } else break;
    } else break;
  }
  if(*s != 0)error("Bad EQUIVALENCE statement");
}
/************************************************************************/
private void include_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle the INCLUDE statement.
------------------------------------------------------------------------*/
{
  char file[MAXVAR];
  int ok,n;

  ok = FALSE;
  s += f->length-1;
  if(*s++ == '\''){
    n = strlen(s)-1;
    if(n && *(s+n)=='\''){
      strncpy(file,s,n);
      file[n] = '\0';
      parse_file(file);
      ok = TRUE;
    }
  }
  if(!ok)error("Bad INCLUDE statement");
}    
/************************************************************************/
private void simple_statement(s,f)
char *s;
STATEMENT *f;
/*
  Handle a statement that consists of a single word (e.g. RETURN, STOP,
  etc). Check that the end-of-line occurs in the place that its expected.
------------------------------------------------------------------------*/
{
  s += f->length;
  if(*s){ERROR((errmsg,"Bad %s statement.",f->name));}
}
/************************************************************************/
private void ignore_statement(s,f)
char *s;
STATEMENT *f;
/*
  Some statement that I do not particularly care about. Just ignore it.
------------------------------------------------------------------------*/
{}
/************************************************************************/
private char *skip_expression(s)
char *s;
/*
  Skip over an expression.
------------------------------------------------------------------------*/
{
  int expecting_value,Token;
  char *sd;

  expecting_value = TRUE;

  sd = s;
  while(*s){
    s = skip_token(s,&Token);
    if(expecting_value){
      expecting_value = ( Token & UNARY_MASK );
      if(!(Token & (UNARY_MASK|TYPE_MASK)))break;
    }else{
      expecting_value = ( Token & BINARY_MASK );
      if(!(Token & BINARY_MASK) )break;
    }
    sd = s;
  }
  return(sd);
}
/************************************************************************/
private char *skip_token(s,Token)
char *s;
int *Token;
/*
  Get a token from the expression stream. Handle variables and function
  calls.
------------------------------------------------------------------------*/
{
  *Token = 0;

/* Variable, array or function call. */

  if(isalpha(*s) || *s == '%'){
    while(issymbol(*s))s++;
    if(*s == '(')s = skip_enclosed(s);
    if(*s == '(' && issubstring(s)) s = skip_enclosed(s);
    *Token = TYPE_MASK;

/* Arithmetic and character operators. */

  }else if(*s == '+' || *s == '-'){
    s++;
    *Token = TOKEN_PLUS_MINUS;
  }else if(*s == '*' || *s == '/'){
    if(*(s+1) == *s)s += 2;
    else	    s += 1;
    *Token = TOKEN_ARITH;

/* Logical or relational operator, or logical value. Use a linear
   search of an array. The array is ordered in roughly decreasing
   frequency of occurence of the operators. */

  }else if(*s == '.' && isalpha(*(s+1))){
    s = skip_logical(s,Token);

/* Numeric (excluding complex values). */

  }else if(isdigit(*s) || (*s == '.' && isdigit(*(s+1)))){
    s = skip_numeric(s,Token);

/* Complex value or left bracket. */

  }else if(*s == '('){
    s = skip_enclosed(s);
    *Token = TYPE_MASK;

/* Character string. */

  }else if(*s == '\''){
    s = skip_string(s);
    *Token = F_CHAR;
  }
  return(s);
}
/************************************************************************/
private char *skip_string(s)
char *s;
/*
  Skip over a string.
------------------------------------------------------------------------*/
{
  while(*s == '\''){
    for(s++; *s && *s != '\''; s++);
    if(*s == '\'')s++;
  }
  return(s);
}
/************************************************************************/
private char *skip_integer(s)
char *s;
/*
  Span across an integer.
------------------------------------------------------------------------*/
{
  while(isdigit(*s))s++;
  return(s);
}
/************************************************************************/
private char *skip_numeric(s,typed)
char *s;
int *typed;
/*
  Skip across an integer, real or double precision number. Determine the
  type as we go. Be careful of periods, otherwise we could screw us
  situations like "1..eq.variable" or "1.eq.variable".
------------------------------------------------------------------------*/
{
  char *sd;
  *typed = 0;
  sd = s;
  if(*s == '+' || *s == '-')s++;
  if(!isdigit(*s) && !(*s == '.' && isdigit(*(s+1))))return(sd);
  *typed = F_INTEGER;
  if(isdigit(*s))while(isdigit(*s))s++;
  if(*s == '.'){
    if(isalpha(*(s+1)) && isalpha(*(s+2)))return(s);
    s++;
    *typed = F_REAL;
    while(isdigit(*s))s++;
  }
  if(*s == 'd')*typed = F_DOUBLE;
  else if(*s == 'e')*typed = F_REAL;
  else return(s);
  s++;
  if(*s == '+' || *s == '-')s++;
  while(isdigit(*s))s++;
  return(s);
}
/************************************************************************/
private char *skip_logical(s,Token)
char *s;
int *Token;
/*
  Determine what the logical value, relational operator or logical
  operator in the input string is.
------------------------------------------------------------------------*/
{
  int j,length;
  char *sd;
  static struct OPERATORS {char *op; int length,token;}
	operators[16]= {{".and.",  5, TOKEN_LOGICAL},
			{".eq.",   4, TOKEN_RELATIONAL},
			{".eqv.",  5, TOKEN_LOGICAL},
			{".false.",7, F_LOGICAL},
			{".ge.",   4, TOKEN_RELATIONAL},
			{".gt.",   4, TOKEN_RELATIONAL},
			{".le.",   4, TOKEN_RELATIONAL},
			{".lt.",   4, TOKEN_RELATIONAL},
			{".ne.",   4, TOKEN_RELATIONAL},
			{".neqv.", 6, TOKEN_LOGICAL},
			{".not.",  5, TOKEN_NOT},
			{".or.",   4, TOKEN_LOGICAL},
			{".true.", 6, F_LOGICAL},
			{"~~~~",   4, 0},
			{"~~~~",   4, 0},
			{"~~~~",   4, 0}};
  sd = s;
  *Token = 0;
  s++;
  while(isalpha(*s))s++;
  if(*s++ != '.')return(sd);
  length = s - sd;
  if(length < 4)return(sd);
  j = 0;
  if(strncmp(sd,operators[j+8].op,4) >= 0)j += 8;
  if(strncmp(sd,operators[j+4].op,4) >= 0)j += 4;
  if(strncmp(sd,operators[j+2].op,4) >= 0)j += 2;
  if(strncmp(sd,operators[j+1].op,4) >= 0)j += 1;
  if(length != operators[j].length || strncmp(sd,operators[j].op,length))
    return(sd);
  *Token = operators[j].token;
  return(s);
}
/************************************************************************/
private char *skip_enclosed(s)
char *s;
/*
  Skip over a something (?) which is enclosed in brackets. So we keep
  track of bracket nesting and quotes.
------------------------------------------------------------------------*/
{
  int nesting;
  nesting = 0;
  do{
    if(*s == '(')nesting++;
    else if(*s == ')')nesting--;
    else if(*s == '\'')s = skip_string(s) - 1;
    s++;
  }while(*s && nesting);
  return(s);
}
/************************************************************************/
private void banish_hollerith(s)
char *s;
/*
  This crudely filters out Hollerith variables. This replaces the Hollerith
  value with an integer which has the same number of characters. A hollerith
  value split between two lines is not handled correctly. Also this
  expects holleriths to be treated as integers, whereas they might be
  treated as reals or double precisions. This will not cause problems except
  in subroutine argument type checking.
------------------------------------------------------------------------*/
{
  int special,i,n;
  char *s0;

  special = FALSE;
  do{
    if(isspace(*s));
    else if(*s == '(' || *s == '=' || *s == ',')special = TRUE;
    else if(isdigit(*s) && special){		  /* Possible hollerith. */
      s0 = skip_integer(s);
      if(*s0 == 'H' || *s0 == 'h'){		  /* Found a hollerith. */
	n = 0;
	while(isdigit(*s))n = 10*n + *s++ - '0';  /* Determine its length. */
	n++;
	for(i=0; i < n && *s0; i++)*s0++ = '0';	  /* Replace with an int. */
      }
      s = s0 - 1;
      special = FALSE;
    }else if(*s == '\''){
      s = skip_string(s) - 1;
      special = FALSE;
    }else special = FALSE;
    s++;
  }while(*s);
}
/************************************************************************/
private int get_arg_intent(name)
char *name;
/*
  This looks up the intent of an argument of the current subroutine or
  function.
------------------------------------------------------------------------*/
{
  SYMBOL *r,*p;
  int i;

/* Check if this subroutine "argument" is actually the value returned by
   a function. */

  if(!strcmp(routine.name,name))return(F_OUT);

/* Get the description of the current routine. If there is no info, just
   return immediately. */

  r = inquire_routine(routine.name);
  if(!r)return(0);

/* If the number of arguments does not agree, something is screwy, and its
   best to play it safe but admitting nothing. */

  if(r->narg != routine.narg) return(0);

/* Determine which argument we are looking for. */

  p = routine.fwd;
  for(i=0;i < routine.narg; i++){
    if(!strcmp(p->name,name))break;
    p = p->fwd;
  }

/* Did we find something. If so, return the good oil. */

  if(!p) return(0);
  return(*(r->args + i) & IO_MASK);
}
/************************************************************************/
private char *get_name(s,name)
char *s,*name;
/*
  Take as much of the input string as forms a valid FORTRAN variable name.
------------------------------------------------------------------------*/
{
  if(isalpha(*s) || *s == '%')while(issymbol(*s))*name++ = *s++;
  *name = '\0';
  return(s);
}
/************************************************************************/
private SYMBOL *get_keyword(name,keys,nkeys)
char *name;
int nkeys;
SYMBOL keys[];
/*
  Perform a binary search to locate a keyword in an array of keywords.
------------------------------------------------------------------------*/
{
  int j,k,m,x;

  if(name==NULL)return(NULL);
  k = 0; m = nkeys - 1;
  while( k <= m){
    j = (k+m)/2;
    x = strcmp(name,(keys[j]).name);
    if( x == 0 )return(&keys[j]);
    if( x < 0 )m = j - 1;
    else       k = j + 1;
  }
  return(NULL);
}
/************************************************************************/
private char *handle_keywords(s,keys,nkeys,defaults,ndefaults)
char *s;
int nkeys,ndefaults;
SYMBOL keys[];
char *defaults[];
/*
  Handle keyword-driven commands such as OPEN, CLOSE, INQUIRE,
  READ and WRITE.
------------------------------------------------------------------------*/
{
  char variable[MAXVAR],*name,*sd;
  int n,flags,type;
  SYMBOL *p;

  n = 0;
  if(*s != '(')error("Bad OPEN, CLOSE, READ, WRITE or INQUIRE statement");
  else do{
    sd = s + 1;
    s = get_name(sd,variable);
    name = variable;
    if(*s++ != '='){
      s = sd;
      if(n < ndefaults)	name = defaults[n];
    }
    n++;
    p = get_keyword(name,keys,nkeys);
    if( p == NULL ){
      ERROR((errmsg,"Keyword %s has been ignored.",name));
      s = skip_expression(s);
    }else if(*s =='*'){
      s++;
      if( strcmp("unit",name) && strcmp("fmt",name))
        {ERROR((errmsg,"Bad syntax for keyword %s.",name));}
    }else if(!strcmp("fmt",name) && isdigit(*s)){
      s = handle_label(s,LABEL_IO);
    }else if(!strcmp("err",name) && isdigit(*s)){
      s = handle_label(s,LABEL_GOTO);
    }else if(!strcmp("unit",name) && (p->flags & F_CHAR) ){
      if(isdigit(*s)){
        s = skip_integer(s);
      }else{
        s = handle_variable(s,variable);
	if(!variable[0]){
	  ERROR((errmsg,"Bad syntax for keyword %s.",p->name));
	  s = skip_expression(s);
        }else if(p->flags & F_IN){
	  (void)set_variable(variable,F_IN);
        }else{
	  flags = set_variable(variable,0);
          if(flags & F_INTEGER)(void)set_variable(variable,F_IN);
	  else if(flags & F_CHAR)(void)set_variable(variable,F_OUT);
	  else {ERROR((errmsg,"Incompatible data type: unit=%s.",variable));}
        }
      }
    }else if(p->flags & F_OUT){
      s = handle_variable(s,variable);
      if(!variable[0]){
	ERROR((errmsg,"Bad syntax for keyword %s.",p->name));
	s = skip_expression(s);
      }else if(!(TYPE_MASK & p->flags & set_variable(variable,F_OUT))){
        ERROR((errmsg,"%s=%s: Incompatible type.",p->name,variable));
      }
    }else{
      s = handle_expression(s,&type);
      if(!(type & p->flags))
	{ERROR((errmsg,"Keyword %s: Incompatible type.",p->name));}
    }
  }while(*s == ',');
  if(*s == ')')s++;
  return(s);
}
/************************************************************************/
private char *handle_statement_func(s,variable,type)
char *s,*variable;
int *type;
/*
  Handle a statement function definition. This does the unpleasant thing
  of temporarily turning off the initialisation checking algorithm.
------------------------------------------------------------------------*/
{
  SYMBOL route;
  char name[MAXVAR];
  SYMBOL *p;
  int init;

  s = get_name(s,variable);
  if(*variable){
    route.name = variable;
    route.narg = 0;
    route.flags = set_variable(variable,F_STATEMENT);
    p = &route;
    do{
      s = get_name(s+1,name);
      if(*name){
	p->fwd  = (SYMBOL *)malloc(sizeof(SYMBOL));
	p = p->fwd;
        p->name = malloc(strlen(name)+1);
        strcpy(p->name,name);
        p->flags = (TYPE_MASK & set_variable(name,0)) | F_IN;
        p->fwd = NULL;
	p->narg = 0;
        route.narg++;
      }
    }while(*s == ',');
  }
  if(*s != ')');
  else if(*(s+1) != '=');
  else {
    (void)set_routine(&route);
    clear_hash_table(&(route.fwd),1);

/* Fudge! Turn off initialisation checking, to prevent spurious error
   messages. */

    init = initialisation;
    initialisation = FALSE;
    s = handle_expression(s+2,type);
    initialisation = init;
  }
  return(s);  
}
/************************************************************************/
private char *handle_expression(s,typed)
char *s;
int *typed;
/*
  This analyses an expression to determine its data type (logical, real,
  etc). It also flags (as used) the variables used in the expression.
  This routine is also responsible for determining whether a symbol
  represents a function or an array.
  The routine does not to a complete check of the correctness of the
  expression, but if the expression is well formed, then the type
  given is correct.
------------------------------------------------------------------------*/
{
  int nesting,expecting_value,Token,flags;
  char *sd,*s0;

  expecting_value = TRUE;
  nesting = 0;
  flags = 0;

/* Build up a mask of all the operators and variable types used. */

  sd = s;
  s0 = s;
  while(*s){
    if(expecting_value){
      s = handle_value(s,&Token);
      expecting_value = ( Token & (UNARY_MASK|TOKEN_LEFT) );
      if(Token & TOKEN_LEFT)nesting++;
      else if(!(Token & (UNARY_MASK|TYPE_MASK)))break;
    }else{
      s = handle_operator(s,&Token);
      expecting_value = ( Token & BINARY_MASK );
      if((Token & TOKEN_RIGHT) && nesting)nesting--;
      else if(!(Token & BINARY_MASK) )break;
    }
    sd = s;
    flags |= Token;
  }

/* The nesting should be zero, and we should be expecting an operator. */

  if(nesting || expecting_value)sd = s0;

/* Now analyse the mask to determine the type of the expression. */

  if(flags & (TOKEN_RELATIONAL|TOKEN_LOGICAL|F_LOGICAL))*typed = F_LOGICAL;
  else if(flags & F_COMPLEX)*typed = F_COMPLEX;
  else if(flags & F_DOUBLE) *typed = F_DOUBLE;
  else if(flags & F_REAL)   *typed = F_REAL;
  else if(flags & F_INTEGER)*typed = F_INTEGER;
  else if(flags & F_CHAR)   *typed = F_CHAR;
  else *typed = 0;

  return(sd);
}
/************************************************************************/
private char *handle_value(s,Token)
char *s;
int *Token;
/*
  Scan through the input, getting the next value from it.
------------------------------------------------------------------------*/
{
  int flag;
  char *sd,name[MAXVAR];

  sd = s;
  *Token = 0;

/* Variable, array or function call. */

  if(isalpha(*s) || *s == '%'){
    s = handle_variable(sd,name);
    if(*name){
      *Token = set_variable(name,F_IN);
    }else{
      s = handle_sub_func_call(sd,0,Token);
    }
    if(s == sd)	*Token = 0;
    else	*Token &= TYPE_MASK;

/* Unary plus or minus. */

  }else if(*s == '+' || *s == '-'){
    s++;
    *Token = TOKEN_PLUS_MINUS;

/* Logical value or .not. */

  }else if(*s == '.' && isalpha(*(s+1))){
    s = skip_logical(s,Token);

/* Numeric (excluding complex values). */

  }else if(isdigit(*s) || (*s == '.' && isdigit(*(s+1)))){
    s = skip_numeric(s,Token);

/* Complex value or left bracket. */

  }else if(*s == '('){
    sd = ++s;
    *Token = TOKEN_LEFT;
    s = skip_numeric(s,&flag);
    if(*s++ != ',' || !flag )return(sd);
    s = skip_numeric(s,&flag);
    if(*s++ != ')' || !flag )return(sd);
    *Token = F_COMPLEX;

/* Character string. */

  }else if(*s == '\''){
    s = skip_string(s);
    *Token = F_CHAR;
  }
  return(s);
}
/************************************************************************/
private char *handle_operator(s,Token)
char *s;
int *Token;
/*
  Scan through the input, getting the next operator from it.
------------------------------------------------------------------------*/
{
  *Token = 0;

/* Arithmetic and character operators. */

  if(*s == '+' || *s == '-'){
    s++;
    *Token = TOKEN_PLUS_MINUS;
  }else if(*s == '*' || *s == '/'){
    if(*(s+1) == *s)s += 2;
    else	    s += 1;
    *Token = TOKEN_ARITH;

/* Logical or relational operator. */

  }else if(*s == '.'){
    s = skip_logical(s,Token);

/* Right bracket. */

  }else if(*s == ')'){
    s++;
    *Token = TOKEN_RIGHT;
  }
  return(s);
}
/************************************************************************/
private char *handle_dio_list(s,flags)
char *s;
int flags;
/*
  This parses a string of variable names or implied do loops, and sets the
  the flags of the appropriate variables.
------------------------------------------------------------------------*/
{
  int type,do_loop;
  char *sd,*s0,name[MAXVAR];

/* Loop over all variables in the list. */

  s--;
  do{
    sd = s + 1;

/* Explore a bit to see if we have an implied do loop. */

    if( *sd == '('){
      if(flags & F_IN){
	s0 = skip_expression(sd+1);
	do_loop = ( (s0 != sd) && (*s0 != ')') );
      }else{
	do_loop = TRUE;
      }
    }else do_loop = FALSE;

/* Handle an implied do loop, an expression or a variable. */

    if(do_loop){
      s = handle_implied_do_loop(sd,flags);
    }else if(flags & F_IN){
      s = handle_expression(sd,&type);
    }else{
      s = handle_variable(sd,name);
      if(*name)(void)set_variable(name,flags);
    }
  }while(s != sd && *s == ',');
  return(s);
}
/************************************************************************/
private char *handle_implied_do_loop(s,flags)
char *s;
int flags;
/*
  Handle an implied do loop.
------------------------------------------------------------------------*/
{
  char *ss,*se,*s0;
  char name[MAXVAR];
  int type;

/* Remember the begining, but find the end of the variable list. */

  ss = s;
  do{
    s0 = s + 1;
    s = skip_expression(s0);
  }while(s != s0 && *s == ',');
  if(*s != '=')return(ss);
  if(handle_variable(s0,name) != s)return(ss);
  if(!*name)return(ss);
  if(!(set_variable(name,F_IN|F_OUT) & F_INTEGER))
    error("Non-integer implied do loop index");

/* How handle the do loop limit expressions. */

  do{
    se = s + 1;
    s = handle_expression(se,&type);
    if(type != F_INTEGER)error("Non-integer implied do loop expression");
  }while(s != se && *s == ',');
  if(*s != ')')return(ss);
  se = s + 1;

/* Handle the variables, etc, within the implied do loop, and return
   a pointer to the begining or end, depending on success. */

  *(s0-1) = '\0';
  s = handle_dio_list(ss+1,flags);
  return(*s ? ss : se);
}
/************************************************************************/
private char *handle_length(s)
char *s;
/*
  Skip over a string length specification.
------------------------------------------------------------------------*/
{
  if(*s != '*')return(s);
  else if(isdigit(*(s+1)))return(skip_integer(s+1));
  else return(handle_indices(s+1,INDICE_1ONLY|INDICE_WILD));
}
/************************************************************************/
private char *handle_label(s,flags)
char *s;
int flags;
/*
  Decode a statement label number, and save the flags about this label.
------------------------------------------------------------------------*/
{
  int labno;
  labno = 0;
  while(isdigit(*s)){
    labno = 10*labno + *s - '0';
    s++;
  }
  if(labno > 0)(void)set_label(labno,flags);
  return s;
}
/************************************************************************/
private char *handle_indices(s,flags)
char *s;
int flags;
/*
  This analyses and skips over array index and substring specifications.
------------------------------------------------------------------------*/
{
  int type;
  char *s0;

  s0 = s;
  if(*s != '(')return(s0);
  do{
    s++;
    if((flags & INDICE_WILD) && (*s == '*')){
      flags |= INDICE_1ONLY;
      s++;
    }else{
      s = handle_expression(s,&type);
      if(type && type != F_INTEGER)
	error("Non-integer array or string subscript");
      if((type || (flags & INDICE_NULL)) && (flags & INDICE_COLON)
        && *s == ':'){
	if((flags & INDICE_WILD) && (*(s+1) == '*')){
	  flags |= INDICE_1ONLY;
	  s += 2;
	}else{
	  s = handle_expression(s+1,&type);
	  if(type && type != F_INTEGER)
	    error("Non-integer array or string subscript");
	}
      }
    }
  }while(*s == ',' && !(flags & INDICE_1ONLY));
  if(*s++ != ')')s = s0;
  return(s);
}
/************************************************************************/
private char *handle_variable(s,variable)
char *s,*variable;
/*
  This looks at the next bit of the code, to see if it represents a simple
  variable (possibly with indices, if the variable represents an array).
  Be careful! It might represent a function call. We can only tell this if
  it is not an array, yet is followed by a bracket. Character variables
  are trickier, because they can be followed by substring specs.
------------------------------------------------------------------------*/
{
  char *sd;
  int flags;

  sd = get_name(s,variable);
  if(*variable){
    if(*sd == '(')flags = inquire_variable(variable);
    else          flags = 0;
    if( !(flags & ROUTE_MASK)){
      if( *sd == '(' && (flags & F_ARRAY) )sd = handle_indices(sd,0);
      if( *sd == '(' && (flags & F_CHAR) && issubstring(sd) )
		sd = handle_indices(sd,INDICE_1ONLY|INDICE_COLON|INDICE_NULL);
    }
    if( *sd == '(' ) sd = s;
  }
  if(s == sd)*variable = '\0';
  return(sd);
}
/************************************************************************/
private char *handle_sub_func_call(s,rflags,type)
char *s;
int *type,rflags;
/*
  Handle a subroutine or function call.
------------------------------------------------------------------------*/
{
  char rname[MAXVAR],name[MAXVAR],*s1,*s2,*vname;
  int flags,*fa,mask,nargs;
  SYMBOL route,*p,*q;

  s1 = get_name(s,rname);
  route.name = rname;
  route.narg = 0;
  route.flags = rflags;
  route.fwd = NULL;
  p = &route;
  if(!strncmp(s1,"()",2)){
    s1 += 2;
  } else if(*s1 == '('){
    do{
      s2 = s1 + 1;

/* If its a simple variable name, determine (as best we can) whether it is
   passed in or out. */

      s1 = handle_variable(s2,name);
      if(*name && (*s1 == ')' || *s1 == ',')){
        vname = malloc(strlen(name)+1);
        strcpy(vname,name);
        flags = set_variable(name,F_SUBARG);

/* If the variable is a parameter, then it must be input. */

	if(flags & F_PARAMETER){
	  flags = (flags & TYPE_MASK) | F_IN;

/* If the variable is a subroutine arg, then scavenge info about it from
   any intent info available for this routine. */

	}else if( (flags & F_ARG) && !(flags & (F_OUT|F_EXTERNAL)) ){
	  flags &= TYPE_MASK;
	  switch(get_arg_intent(name)){
	    case F_IN:	flags |= F_IN;				break;
	    case F_OUT: flags |= (initialisation ? F_OUT : 0);	break;
	    default:						break;
	  }

/* If the variable has not been initialised, then it must be output
   by the routine. Skip this if we are not performing "initialisation"
   checking. Also skip it if the variable appears in a EQUIVALENCE
   SAVE or COMMON statement */

	}else if(initialisation &&
		!(flags & (F_OUT|F_EQUIV|F_COMMON|F_SAVE|F_DATA|
						F_PIN|F_EXTERNAL))){
	  flags = (flags & TYPE_MASK) | F_OUT;

/* Otherwise we have not a clue whether its input or output. */

        }else{ 
	  flags &= (TYPE_MASK | F_EXTERNAL);
	}

/* Expressions are always input. Determine the type of the expression. */

      }else{
        s1 = handle_expression(s2,&flags);
        flags |= F_IN;
        vname = NULL;
      }
      if(s2 != s1){
        p->fwd = (SYMBOL *)malloc(sizeof(SYMBOL));
        p = p->fwd;
        p->fwd = NULL;
	p->narg = 0;
        p->name = vname;
        p->flags = flags;
        route.narg++;
      } else {
        error("Missing or bad subroutine argument");
      }
    }while(*s1 == ',');
    if(*s1 == ')')s1++;
    else error("Bad CALL or function call syntax");
  }

/* Compare this with previous usage of this subroutine. */

  q = set_routine(&route);

/* Determine the type of the output of the function call. */

  *type = (q->flags & TYPE_MASK);
  if( (q->flags & F_GENERIC) && route.narg > 0 )
	*type = ( (route.fwd)->flags & TYPE_MASK);

/* Now indicate the use of simple variables passed to the subroutine.
   If we cannot determine what the use of the variable was,
   assume it was used both as input and output. */

  for(fa = q->args, p=route.fwd, nargs=q->narg; p != NULL && nargs > 0;
	p = p->fwd, fa++, nargs--){
    if(p->name != NULL){
      mask = IO_MASK & *fa;
      if(!mask) mask = F_POUT;
      (void)set_variable(p->name,mask);
    }
  }

/* Delete the things that this routine has allocated. */

  clear_hash_table(&(route.fwd),1);
  return(s1);
}
/************************************************************************/
private int set_label(labno,flags)
int labno,flags;
/*
  This keeps track of the statement labels.
------------------------------------------------------------------------*/
{
  int hashval;
  LABELS *p;

/* Locate the definition of this label. */

  hashval = labno % HASHSIZE;
  for(p = lhash[hashval]; p != NULL ; p = p->fwd)
    if(labno == p->labno)break;

/* If this label was not found, create it! */

  if(p == NULL){
    p = (LABELS *)malloc(sizeof(LABELS));
    p->flags = 0;
    p->labno = labno;
    p->fwd = lhash[hashval];
    lhash[hashval] = p;
  } 

/* Check for a few errors. Its an error it the label is present more than
   once, and if it is present before it is used in a DO statement. */

  if(flags & p->flags & LABEL_DEF)
    error("Statement label used more than once");
  if((p->flags & LABEL_DEF) & (flags & LABEL_DO))
    error("Statement label defined before used in a DO loop");
  p->flags |= flags;
  return(p->flags);
}
/************************************************************************/
private void end_label()
/*
  Processing associated with labels, when an END statement is reached.
------------------------------------------------------------------------*/
{
  int i;
  LABELS *p,*q;

  for(i=0; i < HASHSIZE; i++){
    p = lhash[i];
    lhash[i] = NULL;
    while(p != NULL){
      if(!(p->flags & LABEL_DEF)){
        ERROR((errmsg,
	  "Statement label %d used, but never defined.",p->labno));
      }
      if(!(p->flags & ~LABEL_DEF)){
	ERROR((errmsg,
	  "Statement label %d defined, but never used.",p->labno));
      }
      q = p;
      p = p->fwd;
      free((char *)q);
    }
  }
}
/************************************************************************/
private void set_block(block)
int block;
/*
  This keeps track of nested loops, so make sure they are correctly
  nested.
------------------------------------------------------------------------*/
{
  int b;
  char *s;

  if(block != BLOCK_DO    && block != BLOCK_IF){
    nblocks--;
    if(nblocks < 0){
      error("No start to this block structure");
      nblocks = 0;
    } else if(nblocks < MAXBLOCKS){
      b = blocks[nblocks];
      if((block == BLOCK_ENDDO	&& b == BLOCK_DO)			||
	 (block == BLOCK_ENDIF	&& (b == BLOCK_ELSE || b == BLOCK_ELSEIF
				|| b == BLOCK_IF))			||
	 ((block == BLOCK_ELSE || block == BLOCK_ELSEIF)
				&& (b == BLOCK_ELSEIF || b == BLOCK_IF)));
      else{
	if(b == BLOCK_DO)  s = "an ENDDO";
	else if(b == BLOCK_IF || b == BLOCK_ELSEIF)
			   s = "ELSEIF, ELSE or ENDIF";
	else		   s = "an ENDIF";
        ERROR((errmsg,
	  "Badly nested DO/ENDDO or IF/ENDIF structure, expecting %s.",s));
      }
    }
  } 
  if(block != BLOCK_ENDDO && block != BLOCK_ENDIF){
    if(nblocks < MAXBLOCKS) blocks[nblocks] = block;
    nblocks++;
  }
}
/************************************************************************/
private void end_block()
/*
  Processing associated with the block structure when an END statement
  is reached.
------------------------------------------------------------------------*/
{
/* Check that the block table is empty. */

  if(nblocks != 0)
    error("There are unclosed DO or IF blocks in this routine");
  nblocks = 0;
}
/************************************************************************/
private SYMBOL *inquire_routine(name)
char *name;
/*
  Find info about a particular routine.
------------------------------------------------------------------------*/
{
  return(find_symbol(name,rhash));
}
/************************************************************************/
private SYMBOL *set_routine(route)
SYMBOL *route;
/*
  Define a subroutine or function. First this looks up to see if the
  function has been defined before. If so, it compares the definitions
  and reports on any anomolies.
------------------------------------------------------------------------*/
{
  int *fa, old_io, new_io, i, init,flags,frozen,type_mask,mask,narg;
  SYMBOL *p,*q;
  char old_use[32],new_use[32];

/* Get an entry for this subroutine. It seems I have to search everywhere. */

  flags = route->flags;
  init = FALSE;
  p = find_symbol(route->name,vhash);
  if(p != NULL &&				   (
   ((p->flags & F_ARG) && !(p->flags & F_ROUTINE)) ||
    (p->flags & F_STATEMENT) 			   )){
    if(!(p->flags & F_EXTERNAL) && (p->flags & F_ARG)){
      ERROR((errmsg,"Routine %s does not appear in an external statement.",
	     route->name));
      p->flags |= F_EXTERNAL;
    }
    init = !(p->flags & (F_IN|F_OUT));
  } else p = find_symbol(route->name,rhash);

  if(p == NULL){
    p = create_symbol(route->name,rhash);
    init = TRUE;
  }

/* It might be the min,max,cmplx or abs function. These need special
   processing, as they do not fall into the scheme of things. */

  if(p->flags & F_SPECIAL){
    if(!strcmp("abs",p->name)){
      if(route->narg >= 1){
	flags = TYPE_MASK & route->fwd->flags;
	if(flags == F_COMPLEX)flags = F_REAL;
	p->flags = (p->flags & ~TYPE_MASK)|flags;
      }
    }else if(!strcmp("cmplx",p->name)) p->narg = min(max(route->narg,1),2);
    else			       p->narg = min(max(route->narg,2),MAXARG);
  }

/* If it is not an EXTERNAL, statement function or routine, it must be a 
   routine. */

  if(!(p->flags & ROUTE_MASK) ) p->flags |= F_ROUTINE;

/* If its not an intrinsic, indicate that the function has been used. */

  if(!(p->flags & F_INTRINSIC)){
    flags = set_variable(route->name,route->flags|F_IN|F_OUT|F_CALLED);
  }else{
    flags |= p->flags;
  }

/* Copy the new definition if needed. */

  if(init){
    p->flags = flags;
    p->narg = route->narg;
    if(p->narg > 0){
      p->args = (int *)malloc(sizeof(int)*(route->narg));
      fa = p->args;
      q = route->fwd;
      for(i=0;i<route->narg;i++){
        *fa++ = q->flags;
        q = q->fwd;
      }
    }
  }

/* See if this definition is frozen. */

  frozen = p->flags & F_INTRINSIC ;

/* Compare old and new definitions, and update them. */

  if(p->narg != route->narg)
    {ERROR((errmsg,"Routine %s; previously %d args; now %d args.",
      p->name,p->narg,route->narg));}
  if( !frozen )p->narg = min( route->narg, p->narg );
  narg = min( route->narg, p->narg );

  if( !(p->flags & flags & TYPE_MASK) )
    {ERROR((errmsg,"Routine %s; previously %s; now %s.",
	p->name,usage(p->flags,old_use),usage(flags,new_use)));}
  if(!frozen)p->flags |= flags;


  fa = p->args;
  q = route->fwd;
  type_mask = (( p->flags & F_GENERIC  && narg > 0 ? q->flags : TYPE_MASK)
     		& TYPE_MASK) | F_EXTERNAL;
  for(i=0; i < narg; i++){
    old_io = *fa & IO_MASK;   new_io = q->flags & IO_MASK;
    if( !(*fa & q->flags & type_mask) ||
         ( old_io && new_io && (old_io != new_io)) ){
      mask = (*fa & type_mask & TYPE_MASK) | (*fa & ~TYPE_MASK);
      if(q->name == NULL){
	ERROR((errmsg,"Arg %d of %s; previously %s; now %s.",i+1,
	  p->name,usage(mask,old_use),usage(q->flags,new_use)));
      }else{
	ERROR((errmsg,"Arg %d (%s) of %s; previously %s; now %s.",i+1,q->name,
	  p->name,usage(mask,old_use),usage(q->flags,new_use)));
      }
    }
    if(!frozen)
	*fa = (q->flags & (TYPE_MASK|F_EXTERNAL))|( new_io ? new_io : old_io );
    fa++;
    q = q->fwd;
  }
  return(p);	  
}
/************************************************************************/
private char *crosssum(flags,line)
int flags;
char *line;
/*
  Summarise the usage of this variable or whatever, for the cross reference
  listing.
------------------------------------------------------------------------*/
{
  (void)usage(flags,line);
  if(flags & F_ARRAY)	 strcat(line,",array");
  if(flags & F_PARAMETER)strcat(line,",parameter");
  if(flags & F_DATA)	 strcat(line,",data");
  if(flags & F_SAVE)	 strcat(line,",save");
  if(flags & F_COMMON)	 strcat(line,",common");
  if(flags & F_EQUIV)	 strcat(line,",equivalence");
  if(flags & F_ARG)	 strcat(line,",dummy arg");
  if(flags & F_SUBARG)	 strcat(line,",actual arg");
  if(flags & F_EXTERNAL) strcat(line,",external");
  strcat(line,".");
  return(line);
}
/************************************************************************/
private char *usage(flags,line)
int flags;
char *line;
/*
  Format the particular usage of this variable into something nice and neat.
------------------------------------------------------------------------*/
{
  char *type,*io,*spacer,*null,*cinline;

  null = "";

/* Handle functions and externals as special cases. */

  cinline = null;
  if(flags & (ROUTE_MASK|F_CALLED)){
    io = null;
    if(flags & F_STATEMENT) cinline="inline ";
    if(flags & (F_VOID|F_GENERIC) ) flags &= (F_VOID|F_GENERIC);
    switch(flags & TYPE_MASK){
      case F_LOGICAL:	type = "logical function";	break;
      case F_INTEGER:	type = "integer function";	break;
      case F_REAL:	type = "real function";		break;
      case F_DOUBLE:	type = "double function";	break;
      case F_COMPLEX:	type = "complex function";	break;
      case F_CHAR:	type = "character function";	break;
      case F_VOID:	type = "subroutine";		break;
      case F_GENERIC:	type = "intrinsic function";	break;
      default:		type = "external";		break;
    }
  }else{
    switch(flags & TYPE_MASK){
      case F_LOGICAL:	type = "logical";		break;
      case F_INTEGER:	type = "integer";		break;
      case F_REAL:	type = "real";			break;
      case F_DOUBLE:	type = "double";		break;
      case F_COMPLEX:	type = "complex";		break;
      case F_CHAR:	type = "character";		break;
      default:		type = null;			break;
    }
    switch(flags & IO_MASK){
      case F_IN:	io = "input";			break;
      case F_OUT:	io = "output";			break;
      case IO_MASK:	io = "input/output";		break;
      default:		io = null;			break;
    }
  }
  if(io == null && type == null)     spacer = "unknown";
  else if(io != null && type != null)spacer = " ";
  else				     spacer = null;

  strcpy(line,cinline);
  strcat(line,io);
  strcat(line,spacer);
  strcat(line,type);
  return(line);
}
/************************************************************************/
private int inquire_variable(name)
char *name;
/*
  Look at the characteristics of a variable which should be visible in the
  current routine.
------------------------------------------------------------------------*/
{
  SYMBOL *p;
  p = find_symbol(name,vhash);
  if(p == NULL) return(0);
  return(p->flags);
}
/************************************************************************/
private int set_variable(name,flags)
char *name;
int flags;
/*
	Set flags of a particular variable.
------------------------------------------------------------------------*/
{
  char *s;
  SYMBOL *p;

  p = find_symbol(name,vhash);

/* Create a symbol if necessary, and check that it is valid. */

  if(p == NULL){
    p = create_symbol(name,vhash);
    if(!extended){
      if(strlen(name) > MAXVARLEN){
        ERROR((errmsg,"Name %s is longer than %d characters.",name,MAXVARLEN));
      }
      s = name;
      while(*s && *s != '_' && *s != '$' && *s != '%')s++;
      if(*s){ERROR((errmsg,"Name %s contains nonstandard characters.",name));}
    }
  }

/* If a variable has already been used as output, ignore the possibility
   that its possibly been assigned by a subroutine. There is something
   more devious about this, but I cannot remember what. */

  if((p->flags & IO_MASK) == IO_MASK)flags &= ~F_POUT;

/* Check for doubly defined variables. */

  if( (p->flags & TYPES_MASK) && (flags & TYPES_MASK) && !(flags & ROUTE_MASK)){
    ERROR((errmsg,"Variable %s has been declared more than once.",p->name));
  }

/* Check for case of undefined variable. */

  p->flags |= flags;
  if( !(p->flags & TYPE_MASK) &&
     (!(p->flags & F_EXTERNAL) && !(flags & F_ARG)) ){
    if(declare  && *name != '%' ){
    ERROR((errmsg,"Variable or function %s was not declared.",p->name));}
    p->flags |= (( *(p->name) >= 'i' && *(p->name) <= 'n' ) ? F_INTEGER :
								F_REAL );
  }

/* Check for a variable which appears in both a common and DATA statement. */

  if((flags & (F_COMMON|F_DATA)) &&
    ((p->flags & (F_COMMON|F_DATA)) == (F_COMMON|F_DATA))){
    ERROR((errmsg,"Variable %s appears in both a COMMON and DATA statement.",name));
  }

/* Check for a variable which was used before it was initialised. */

  if( (p->flags & F_IN) &&
    !(p->flags & (F_DATA|F_EQUIV|F_SAVE|F_COMMON|F_OUT|F_PARAMETER|F_PIN|F_POUT))){
    if(initialisation)
      {ERROR((errmsg,"Variable %s has not been initialised.",name));}
    p->flags |= F_OUT;
  }

/* If it is assigned to before it is used as input, then it cannot
   possibly be input from the calling routine. Also this does not make
   sense if the variable appears in a DATA statement. */

  if( initialisation && (p->flags & F_OUT) &&
    !(p->flags & (F_POUT|F_IN))){
      if(p->flags & F_DATA)
        {ERROR((errmsg,"Data statement for %s seems useless.",p->name));}
      p->flags &= ~F_PIN;
    }

  return(p->flags);
}
/************************************************************************/
private void clear_hash_table(hash,nhash)
SYMBOL *hash[];
int nhash;
/*
    A subroutine has ended. Go through the variable table, deleting each
    variable entry, and giving warnings about unused variables.
------------------------------------------------------------------------*/
{
  int i;
  SYMBOL *p,*q;
  for(i=0; i<nhash; i++){
    p = hash[i];
    while( p != NULL ){
      q = p->fwd;
      if(p->name != NULL )free((char *)p->name);
      if(p->narg > 0)free((char *)(p->args));
      free((char *)p);
      p = q;
    }
    hash[i] = NULL;
  }
}
/************************************************************************/
private SYMBOL *create_symbol(name,hash)
char *name;
SYMBOL *hash[];
{
  char *s;
  int hashval,length;
  SYMBOL *p;

/* Copy all the data. */

  p = (SYMBOL *)malloc(sizeof(SYMBOL));
  p->flags = 0;
  p->narg  = 0;
  p->args  = NULL;
  length = strlen(name);
  p->name  = malloc(length+1);
  strcpy(p->name,name);

/* Now add the entry to the hash table. */
  
  s = name;
  hashval = 0;
  while(*s != '\0') hashval += *s++;
  hashval %= HASHSIZE;
  p->fwd = hash[hashval];
  hash[hashval] = p;
  return(p);
}
/************************************************************************/
private SYMBOL *add_symbol(p,hash)
SYMBOL *p,*hash[];
{
  char *s;
  int hashval;

/* Now add the entry to the hash table. */
  
  s = p->name;
  hashval = 0;
  while(*s != '\0') hashval += *s++;
  hashval %= HASHSIZE;
  p->fwd = hash[hashval];
  hash[hashval] = p;
  return(p);
}
/************************************************************************/
private SYMBOL *find_symbol(name,hash)
char *name;
SYMBOL *hash[];
/*
	This tries to find a symbol in the given hash table.
------------------------------------------------------------------------*/
{
  int hashval;
  char *s;
  SYMBOL *p;

  s = name;
  hashval = 0;
  while(*s != '\0') hashval += *s++;
  hashval %= HASHSIZE;

  for(p = hash[hashval]; p != NULL ; p = p->fwd){
    if(!strcmp(p->name,name))break;
  }

  return(p);
}
