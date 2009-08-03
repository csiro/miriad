/************************************************************************/
/*									*/
/*= intf2c -- A C preprocessor, to aid calling C from FORTRAN.		*/
/*& rjs									*/
/*: tools								*/
/*+
Intf2c is a C preprocessor for MIRIAD source code, intended to aid the
calling of C code by FORTRAN routines. For most C code, it merely
copies the input to the output. However when it encounters a subroutine
definition in a FORTRAN-like syntax, it translates the subroutine
interface into what would be the appearance of the C routine being called.
This is a system-dependent operation. In particular Intf2c simplifies the
perenial question of what FORTRAN character strings and subroutine names
appear as in a C routine.

Usage:

  intf2c -s system [-i type] [-r type] [-d type] [-l type] [-p type] [in] [out]

    system: One of "vms","hpux","sun","bsd","trace","alliant","convex",
            "unicos","alpha", "sgi", "linux". No default.

    in:     Input file. Default is standard input.
    out:    Output file. Default is standard output.

    -i,-r,-d,-l,-p Define the C datatype corresponding to FORTRAN
            INTEGER, REAL, DOUBLE PRECISION, LOGICAL and PTRDIFF
            respectively. The default is "int", "float", "double", "int"
            and "int" respectively.
    -c      Invoke code to convert between FORTRAN and C integers and logicals.

Intf2c and the C preprocessor:

intf2c runs before the C preprocessor. This is good and bad. The
good side of it is that it can produce code that is more machine
independent, as some of the text it includes are later expanded by
the C preprocessor. The bad side of it is that you cannot define
macros which contain unbalanced { and } characters (otherwise
you would screw up intf2c's tracking of the nesting depth) and you
cannot define anything that intf2c would expand as part of a macro, #define
or within a #include file.

Use:

As intf2c is a preprocessor of C code, most of the input source looks
like normal C. However, intf2c provides a syntax to describe a routine
interface. Additionally within the routine, you use special notations
when manipulating the dummy arguments.

A routine declaration always starts with the word "fortran" followed
by 
  subroutine sub-name(type arg1, type arg2, ...)

or

  type function sub-name(type arg1, type arg2, ...)

Here "type" can be any of integer, real, double, complex, logical or
character. sub-name is the routine name (case is unimportant) and
arg1, arg2, etc are names given to the routines dummy arguments.
Character and complex functions are not supported.

For example

  fortran subroutine output(character string)

defines a subroutine "output" which has a single character argument, called
"string". Similarly

  fortran integer function prime(integer n)

defines an integer function "prime" which has a single integer argument, "n".

Within the body of a routine, you access the routine arguments using a
notation suggestive of structs. For argument, arg,
	arg.val		gives the value of arg.
	arg.addr	gives the address of arg.
	arg.len		gives the FORTRAN length of arg, assuming
			arg is a character string.
	arg.zterm	gives a zero-terminated copy of arg, assuming
			arg is a blank padded character string. This
			copy should only be read.
	arg.pad		gives the address of arg. In doing so, it converts
			arg from a zero-terminated string to one that it
			blank padded (to arg.len).

Intf2c also generates #define statements for FORTRAN_TRUE and FORTRAN_FALSE,
which are the values that FORTRAN uses to represent .true. and .false.

For example, the following code copies a character string from one character
string to another.

fortran subroutine fstrcpy(character out,character in)
{
  strcpy(out.addr,in.zterm);
  out.pad;
}
			
									*/
/*--									*/
/*  History:								*/
/*    rjs  20dec92 Original version.					*/
/*    rjs  27jul93 Handle subroutines with zero args.			*/
/*    rjs   9aug93 Exit (rather than return) with 0 to appease VMS.     */
/*    rjs  20nov94 Added Alphas.					*/
/*    rjs  26jan95 Added f2c.						*/
/*    mrc  14jul06 Get it to compile with 'gcc -Wall' without warnings. */
/*    rjs  21jul09 Change in flags for types. Add FORTRAN PTRDIFF type. */
/************************************************************************/

#define VERSION_ID "version 1.0 21-Jul-09"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLEN 512
#define TRUE 1
#define FALSE 0

typedef void (*ROUTINE)();

/* The ROUTINEs are routines to generate the needed system-dependent
   output code. Their functions are:
    fname	Name (as seen by C) of a function called by FORTRAN.
    arg		Argument list (seen by C) of a function called
		by FORTRAN.
    len		Length of a character string.
    addr	Address of a argument.
    init	Initialisation routine -- called at the start
		of code generation.					*/

typedef struct { char *name,*fortran_true,*fortran_false;
		 ROUTINE fname,arg,len,addr,init;
					} SYSTEM;

void name_lower(),name_upper(),name_lower_();
void arg_vms(),arg_extra(),arg_norm(),arg_uni(),arg_gen();
void len_vms(),len_extra(),len_alliant(),len_uni();
void addr_vms(),addr_norm(),addr_uni();
void init_vms(),init_norm(),init_uni();
int Handle_Fortran();

SYSTEM systems[] = {
	{ "vms",  "-1","0",
	  name_lower,  arg_vms,  len_vms,    addr_vms, init_vms},
	{ "hpux",   "1","0",
	  name_lower,  arg_extra,len_extra,  addr_norm,init_norm},
	{ "linux",  "1","0",
	  name_lower_,  arg_extra,len_extra,  addr_norm,init_norm},
	{ "sun",    "1","0",
	   name_lower_,arg_extra,len_extra,  addr_norm,init_norm},
	{ "sgi",    "1","0",
	   name_lower_,arg_extra,len_extra,  addr_norm,init_norm},
	{ "f2c",    "1","0",
	   name_lower_,arg_extra,len_extra,  addr_norm,init_norm},
	{ "bsd",    "1","0",
	   name_lower_,arg_extra,len_extra,  addr_norm,init_norm},
	{ "trace",  "1","0",
	   name_lower_,arg_extra,len_extra,  addr_norm,init_norm},
	{ "alliant","1","0",
	   name_lower_,arg_norm, len_alliant,addr_norm,init_norm},
	{ "convex","-1","0",
	   name_lower_,arg_extra,len_extra,  addr_norm,init_norm},
	{ "alpha","-1","0",
	   name_lower_,arg_extra,len_extra,  addr_norm,init_norm},
	{ "unicos","_btol(1)","_btol(0)",
	   name_upper, arg_uni,  len_uni,    addr_uni, init_uni},
 		    };
#define NSYSTEMS (sizeof(systems)/sizeof(SYSTEM))

#define TYPE_REAL	0x01
#define TYPE_INTEGER	0x02
#define TYPE_CHAR	0x04
#define TYPE_LOGICAL	0x08
#define TYPE_DOUBLE	0x10
#define TYPE_CMPLX	0x20
#define TYPE_PTRDIFF    0x40
#define TYPE_VOID	0x80

#define MAXHASH 257
typedef struct arg { char *name;
		     int type;
		     struct arg *next;	} ARG;
typedef struct { ARG routine,*args,*ahash[MAXHASH];
		 int nargs,init;	} INTERFACE;
int arg_type();
ARG *arg_get();
char *type_label();
SYSTEM *set_system_type();
char *Get_Word(),*Get_Token(),*Lower_case();
void process(),Handle_Arg(),Interface_Release(),usage();
char *fortran_real,*fortran_integer,*fortran_logical,*fortran_double,*fortran_ptrdiff;

int lineno,nesting,cvtint,cvtlog;
char last_char;
/************************************************************************/
int main(argc,argv)
int argc;
char *argv[];
{
  char *s,c;
  int i;
  char *input,*output;
  SYSTEM *sys_type;
  FILE *file;

/* Handle the command line. Determine input and output files and the system. */

  fortran_real = "float";
  fortran_integer = "int";
  fortran_logical = "int";
  fortran_ptrdiff = "int";
  fortran_double  = "double";

  input = output = NULL; sys_type = NULL;
  cvtint = 0;
  for(i=1; i < argc; i++){
    s = argv[i];
    if(*s == '-'){
      s++;
      while((c = *s++))switch(c){
	case 's':
	  if(++i < argc)
	    sys_type = set_system_type(argv[i]);
	  break;
	case 'i': if(++i < argc)fortran_integer = argv[i]; break;
	case 'r': if(++i < argc)fortran_real    = argv[i]; break;
	case 'd': if(++i < argc)fortran_double  = argv[i]; break;
	case 'l': if(++i < argc)fortran_logical = argv[i]; break;
	case 'p': if(++i < argc)fortran_ptrdiff = argv[i]; break;
	case 'c': cvtint = 1; cvtlog = 1; break;
	case '?':
	  usage();
	  exit(0);
	default:
	  fprintf(stderr,"### Unrecognised flag %c ignored\n",c);
      }
    } else if(input == NULL) input  = argv[i];
    else		     output = argv[i];
  }

/* Check that we know the system. */

  if(sys_type == NULL){
    fprintf(stderr,"### A system type must be given with the -s flag\n");
    usage();
    exit(1);
  }

/* Fiddle the input file if needed. */

  if(input != NULL){
    file = freopen(input,"r",stdin);
    if(file == NULL){
      fprintf(stderr,"### Failed to open input file: %s\n",input);
      exit(1);
    }
  }


/* Fiddle the output file if needed. */

  if(output != NULL){
    file = freopen(output,"w",stdout);
    if(file == NULL){
      fprintf(stderr,"### Failed to open output file: %s\n",output);
      exit(1);
    }
  }

/* All is OK. Lets go for it!. */

  process(sys_type);

/* Close the input and output, just to be sure. */

  fclose(stdin);
  fclose(stdout);
  exit(0);
}
/************************************************************************/
void usage()
{
  int i;
  printf("intf2c: %s\n",VERSION_ID);
  printf("Usage:\n\n   intf2c [-x] [-y] -s system [input] [output]\n\n");
  printf("where system can be one of:");
  for(i=0; i < NSYSTEMS; i++)printf(" %s",systems[i].name);
  printf("\n");
}
/************************************************************************/
void process(sys_type)
SYSTEM *sys_type;
/*
  Process the input file. Read through, find a #fortran, expand this line,
  and look for occurrences of the subroutine arguments in the next block
  of code.
------------------------------------------------------------------------*/
{
  char *s;
  char buf[MAXLEN];
  int inside;
  INTERFACE rout;

  lineno = 1;
  nesting = 0;
  last_char = '\n';

  rout.init = FALSE;
  inside = FALSE;

/* Initialise the output file. */

  printf("\n/* System dependent initialisation. */\n\n");
  (*(sys_type->init))();
/*  printf("#define FORTRAN_TRUE  %s\n",sys_type->fortran_true);
  printf("#define FORTRAN_FALSE %s\n\n",sys_type->fortran_false); */
  if(cvtint)printf("#define FORTRAN_CVT_INT 1\n");
  else printf("#define FORTRAN_CVT_INT 0\n");
  if(cvtlog)printf("#define FORTRAN_CVT_LOG 1\n");
  else printf("#define FORTRAN_CVT_LOG 0\n");

  printf("typedef %s fort_integer;\n",fortran_integer);
  printf("typedef %s fort_logical;\n",fortran_logical);
  printf("typedef %s fort_ptrdiff;\n",fortran_ptrdiff);
  printf("typedef %s fort_real;\n",fortran_real);
  printf("typedef %s fort_double;\n",fortran_double);

  printf("char *zterm(char *string,int length);\n");
  printf("void pad(char *string,int length);\n");
  printf("int *iface_iarr(int n);\n");
  printf("void iface_f2c_icvt(fort_integer *in,int *out,int n);\n");
  printf("void iface_c2f_icvt(int *in,fort_integer *out,int n);\n");
  printf("int *iface_larr(int n);\n");
  printf("void iface_f2c_lcvt(fort_logical *in,int *out,int n);\n");
  printf("void iface_c2f_lcvt(int *in,fort_logical *out,int n);\n");

  while((s = Get_Word(buf)) != NULL){
    inside = inside && nesting > 0;
    if(inside){
      if(arg_get(&rout,s) != NULL)Handle_Arg(&rout,sys_type,s);
      else printf("%s",s);
    } else if(nesting == 0 && !strcmp(s,"fortran")){
      if(rout.init) Interface_Release(&rout);
      inside = Handle_Fortran(&rout);
      if(inside){
        printf("%s ",type_label(rout.routine.type));
	(*(sys_type->fname))(rout.routine.name);
	(*(sys_type->arg))(&rout);
      }
    } else printf("%s",s);
  }
}
/************************************************************************/
int Handle_Fortran(rout)
INTERFACE *rout;
/*
  Parse and handle an apparent FORTRAN subroutine definition.
------------------------------------------------------------------------*/
{
  ARG *arg,*targ,**a;
  char *s,buf[MAXLEN];
  int i,more,type,hash;

/* Initialise the routine structure. */

  rout->init = TRUE;
  rout->nargs = 0;
  for(i=0; i < MAXHASH; i++) rout->ahash[i] = NULL;
  rout->args = NULL;
  rout->routine.name = NULL;

/* Parse the fortran statement. */

  s = Lower_case(Get_Token(buf));
  if(!strcmp(s,"integer"))         rout->routine.type = TYPE_INTEGER;
  else if(!strcmp(s,"real"))       rout->routine.type = TYPE_REAL;
  else if(!strcmp(s,"logical"))    rout->routine.type = TYPE_LOGICAL;
  else if(!strcmp(s,"double"))	   rout->routine.type = TYPE_DOUBLE;
  else if(!strcmp(s,"subroutine")) rout->routine.type = TYPE_VOID;
  else {
    fprintf(stderr,"Line %d: fortran not followed by function or routine definition\n",lineno);
    return(FALSE);
  }

/* Skip the word "function" if appropriate. */

  if(rout->routine.type != TYPE_VOID){
    s = Lower_case(Get_Token(buf));
    if(strcmp(s,"function")){
      fprintf(stderr,"Line %d: Apparent fortran function not correctly formed\n",lineno);
      return(FALSE);
    }
  }

/* Now get the subroutine name. It must be an alphanumeric thingo! */

  s = Lower_case(Get_Token(buf));
  if(!isalpha(*s)){
    fprintf(stderr,"Line %d: Missing fortran routine name?\n",lineno);
    return(FALSE);
  }
  rout->routine.name = malloc((unsigned)(strlen(s)+1));
  strcpy(rout->routine.name,s);

  s = Get_Token(buf);
  if(*s != '('){
    fprintf(stderr,"Line %d: Missing ( in fortran routine declaration?\n",lineno);
    return(FALSE);
  }

/* Get and save the description of this variable. */

  arg = NULL;
  more = TRUE;
  while(more){
    s = Lower_case(Get_Token(buf));
    if(!strcmp("integer",s))	   type = TYPE_INTEGER;
    else if(!strcmp("logical",s))  type = TYPE_LOGICAL;
    else if(!strcmp("real",s))     type = TYPE_REAL;
    else if(!strcmp("double",s))   type = TYPE_DOUBLE;
    else if(!strcmp("complex",s))  type = TYPE_CMPLX;
    else if(!strcmp("ptrdiff",s))  type = TYPE_PTRDIFF;
    else if(!strcmp("character",s))type = TYPE_CHAR;
    else break;

    s = Get_Token(buf);
    if(isalpha(*s)){
      targ = (ARG *)malloc(sizeof(ARG));
      if(arg == NULL) rout->args = targ;
      else 	      arg->next  = targ;
      arg = targ;
      arg->next = NULL;
      arg->type = type;
      arg->name = malloc(strlen(s)+1);
      strcpy(arg->name,s);
      rout->nargs = rout->nargs + 1;
    } else {
      fprintf(stderr,"Line %d: Bad variabele name %s\n",lineno,s);
      return(FALSE);
    }

    s = Get_Token(buf);
    more = (*s == ',');
  }

  if(*s != ')'){
    fprintf(stderr,"Line %d: Bad fortran declaration - got %s\n",lineno,s);
    return(FALSE);
  }

/* All looks good. Put the names of all the arguments in the hash table. */

  for(arg = rout->args; arg != NULL; arg = arg->next){
    s = arg->name;
    hash = 0;
    while(*s) hash += *s++;
    hash %= MAXHASH;
    a = rout->ahash;
    while(a[hash] != NULL) hash = (hash + 1) % MAXHASH;
    a[hash] = arg;
  }
  return(TRUE);
}
/************************************************************************/
void Handle_Arg(rout,sys_type,argname)
INTERFACE *rout;
SYSTEM *sys_type;
char *argname;
/*
  Handle a subroutine argument "macro".
------------------------------------------------------------------------*/
{
  char buf[MAXLEN],*s;

  s = Get_Token(buf);
  if(*s != '.'){
    fprintf(stderr,"Line %d: Argument %s not followed by period\n",lineno,argname);
    printf("%s %s",argname,s);
    return;
  }

  s = Get_Token(buf);

  if(!strcmp(s,"addr")) (*(sys_type->addr))(rout,argname);
  else if(!strcmp(s,"val")){printf("*"); (*(sys_type->addr))(rout,argname);}
  else if(!strcmp(s,"len")){
    if(arg_type(rout,argname) != TYPE_CHAR)
      fprintf(stderr,"Line %d: Argument %s is not a string\n",lineno,argname);
    (*(sys_type->len))(rout,argname);
  } else if(!strcmp(s,"zterm") ||  !strcmp(s,"pad")){
    if(arg_type(rout,argname) != TYPE_CHAR)
      fprintf(stderr,"Line %d: Argument %s is not a string\n",lineno,argname);
    printf("%s(",s);
    (*(sys_type->addr))(rout,argname);
    putchar(',');
    (*(sys_type->len))(rout,argname);
    putchar(')');    
  } else {
    fprintf(stderr,"Line %d: Do not understand %s.%s\n",lineno,argname,s);
    printf("%s.%s",argname,s);
  }
}
/************************************************************************/
ARG *arg_get(rout,argname)
INTERFACE *rout;
char *argname;
/*
  Check if a name is an argument name.
------------------------------------------------------------------------*/
{
  int hash;
  ARG **a;
  char *s;

/* Determine the hash index. */

  hash = 0;
  s = argname;
  while(*s) hash += *s++;
  hash %= MAXHASH;

/* See if this name is in the hash table. */

  a = rout->ahash;
  while(a[hash] != NULL){
    if(!strcmp((a[hash])->name,argname))return(a[hash]);
    hash++;
    if(hash >= MAXHASH) hash = 0;
  }
  return(NULL);
}
/************************************************************************/
int arg_type(rout,argname)
INTERFACE *rout;
char *argname;
{
  ARG *arg;
  arg = arg_get(rout,argname);
  if(arg == NULL){
    fprintf(stderr,"### Variable %s is not a routine argument\n",argname);
    exit(1);
  }
  return(arg->type);
}
/************************************************************************/
char *type_label(type)
int type;
{
  char *s;
  switch(type){
    case TYPE_REAL:	s = "fort_real";	break;
    case TYPE_INTEGER:	s = "fort_integer";	break;
    case TYPE_CHAR:	s = "char";		break;
    case TYPE_DOUBLE:	s = "fort_double";	break;
    case TYPE_LOGICAL:	s = "fort_logical";	break;
    case TYPE_PTRDIFF:  s = "fort_ptrdiff";	break;
    case TYPE_CMPLX:	s = "fort_real";	break;
    case TYPE_VOID:	s = "void";		break;
    default:
      fprintf(stderr,"### Unrecognised type code (%d) in type_label\n",type);
      exit(1);
  }
  return(s);
}
/************************************************************************/
SYSTEM *set_system_type(systype)
char *systype;
{
  SYSTEM *s;
  int i;

  for(i=0; i < NSYSTEMS; i++){
    s = &(systems[i]);
    if(!strcmp(systype,s->name))return(s);
  }
  fprintf(stderr,"### Unrecognised system %s\n",systype);
  exit(1);
  return(NULL);
}
/************************************************************************/
void Interface_Release(rout)
INTERFACE *rout;
/*
  Delete all the memory associated with this interface description.
------------------------------------------------------------------------*/
{
  ARG *arg,*targ;

  if(rout->routine.name != NULL)free(rout->routine.name);
  arg = rout->args;
  while(arg != NULL){
    targ = arg;
    arg = arg->next;
    free(targ->name);
    free((char *)targ);
  }
}
/************************************************************************/
char *Lower_case(buf)
char *buf;
/*
  Convert a string to lower case.
------------------------------------------------------------------------*/
{
  char *s;

  for(s = buf; *s; s++) if(isupper(*s)) *s = tolower(*s);
  return(buf);
}
/************************************************************************/
char *Get_Token(buf)
char *buf;
/*
  Retrieve the next token (a word or a special char, but not white
  chars) from stdin. NOTHING is echoed to stdout. NEVER digest
  #, ", ', { or } characters -- return an empty string in preference.

  The global variables lineno and last_char are updated.
------------------------------------------------------------------------*/
{
  char *s;
  int c;

  c = getchar(); if(c == '\n') lineno++;
  while(isspace(c)){
    last_char = c;
    c = getchar(); if(c == '\n') lineno++;
  }

  s = buf;
  if(c == EOF || c == '#' || c == '\"' || c == '\'' || c == '{' || c == '}'){
    *s++ = 0;
    if(c != EOF) ungetc(c,stdin);
  } else if(isalpha(c) || c == '_'){
    while(isalnum(c) || c == '_'){
      *s++ = last_char = c;
      c = getchar();
    }
    *s++ = 0;
    ungetc(c,stdin);
  } else {
    *s++ = last_char = c;
    *s++ = 0;
  }
  return(buf);
}    
/************************************************************************/
char *Get_Word(buf)
char *buf;
/*
  Retrieve the next word from stdin. This skips over comments, quoted
  strings, and CPP directives, and returns an alpha-numeric word (starting
  with an alphabetic character). Everything up to (but not including) the
  next word are echoed to stdout.

  The global variables nesting, lineno and last_char are updated.
------------------------------------------------------------------------*/
{
  char last_nonwhite,quote,*s;
  int more,c;

  more = TRUE;
  while(more){
    c = getchar(); if(c == '\n') lineno++;

/* Handle EOF. */

    if(c == EOF){
      more = FALSE;
      s = NULL;

/* Handle a word of some type. */

    } else if(isalpha(c) || c == '_'){
      s = buf;
      while(isalnum(c) || c == '_'){
        *s++ = last_char = c;
	c = getchar();
      }
      *s++ = 0;
      s = buf;
      if(c != EOF) ungetc(c,stdin);
      more = FALSE;

/* Handle a comment. */

    } else if(c == '*' && last_char == '/'){
      putchar(c);
      last_char = 0;
      while(more){
	c = getchar(); if(c == '\n') lineno++;
	more = (c != EOF && (c != '/' || last_char != '*'));
	last_char = c;
	if(c != EOF) putchar(c);
      }
      more = TRUE;
	
/* Handle a cpp directive. */

    } else if(c == '#' && last_char == '\n'){
      putchar(c);
      last_nonwhite = 0;
      while(more){
	c = getchar(); if(c == '\n') lineno++;
	more = (c != EOF && (c != '\n' || last_nonwhite == '\\'));
	last_char = c;
	if(!isspace(c)) last_nonwhite = c;
	if(c != EOF) putchar(c);
      }
      more = TRUE;

/* Handle a quoted string. */

    } else if(c == '"' || c == '\''){
      quote = c;
      putchar(c);
      c = getchar(); if(c == '\n') lineno++;
      while(c != EOF && c != quote){
	putchar(c);
	if(c == '\\'){
	  c = getchar(); if(c == '\n')lineno++;
	  if(c != EOF) putchar(c);
	}
        c = getchar(); if(c == '\n') lineno++;
      }
      if(c != EOF) putchar(c);
      last_char = c;

/* Handle everything else. */

    } else {
      if(c == '{')     nesting++;
      else if(c == '}')nesting--;
      putchar(c);
      last_char = c;
    }
  }
  return(s);
}
/************************************************************************/
/*									*/
/*	The system-dependent routines.					*/
/*									*/
/************************************************************************/
void name_lower(name)
char *name;
{
  printf("%s",name);
}
/************************************************************************/
void name_lower_(name)
char *name;
{
  printf("%s_",name);
}
/************************************************************************/
void name_upper(name)
char *name;
{
  char c;

  while(*name){
    c = *name++;
    if(islower(c)) c = toupper(c);
    putchar(c);
  }
}
/************************************************************************/
void init_norm(){}
void init_vms()
{
  printf("#include <descrip.h>\n");
}
void init_uni()
{
  printf("#include <fortran.h>\n");
}
/************************************************************************/
void arg_vms(rout)
INTERFACE *rout;
{
  arg_gen(rout,"struct dsc$descriptor *");
}
void arg_uni(rout)
INTERFACE *rout;
{
  arg_gen(rout,"_fcd ");
}
void arg_norm(rout)
INTERFACE *rout;
{
  arg_gen(rout,"char *");
}
/************************************************************************/
void arg_gen(rout,char_descriptor)
INTERFACE *rout;
char *char_descriptor;
{
  ARG *arg;

  printf("(");
  for(arg = rout->args; arg != NULL; arg = arg->next){
    printf("%s",arg->name);
    if(arg->next != NULL) printf(",");
  }
  printf(")");
  for(arg = rout->args; arg != NULL; arg = arg->next){
    if(arg->type & TYPE_CHAR)printf("\n%s%s;",char_descriptor,arg->name);
    else		     printf("\n%s *%s;",type_label(arg->type),arg->name);
  }
}
/************************************************************************/
void arg_extra(rout)
INTERFACE *rout;
{
  ARG *arg;

  printf("(");
  for(arg = rout->args; arg != NULL; arg = arg->next){
    printf("%s",arg->name);
    if(arg->next != NULL) printf(",");
  }
  for(arg = rout->args; arg != NULL; arg = arg->next){
    if(arg->type & TYPE_CHAR)
      printf(",%s_len",arg->name);
  }
  printf(")");
  for(arg = rout->args; arg != NULL; arg = arg->next){
    printf("\n%s *%s;",type_label(arg->type),arg->name);
    if(arg->type & TYPE_CHAR)
      printf("\nint %s_len;",arg->name);
  }
}
/************************************************************************/
void len_vms(rout,argname)
INTERFACE *rout;
char *argname;
{
  printf("((int)(%s->dsc$w_length))",argname);
}
void len_uni(rout,argname)
INTERFACE *rout;
char *argname;
{
  printf("(_fcdlen(%s))",argname);
}
void len_extra(rout,argname)
INTERFACE *rout;
char *argname;
{
  printf("%s_len",argname);
}
void len_alliant(rout,argname)
INTERFACE *rout;
char *argname;
{
  int offset;
  offset = rout->nargs + 1;
  printf("(**((int **)&%s-%d))",argname,offset);
}
/************************************************************************/
void addr_vms(rout,argname)
INTERFACE *rout;
char *argname;
{
  if(arg_type(rout,argname) == TYPE_CHAR){
    printf("(%s->dsc$a_pointer)",argname);
  } else {
    printf("%s",argname);
  }
}
void addr_uni(rout,argname)
INTERFACE *rout;
char *argname;
{
  if(arg_type(rout,argname) == TYPE_CHAR){
    printf("((char *)_fcdtocp(%s))",argname);
  } else {
    printf("%s",argname);
  }
}
void addr_norm(rout,argname)
INTERFACE *rout;
char *argname;
{
  printf("%s",argname);
}
