/************************************************************************/
/*                                                                      */
/*  This is a simple shell to run the MIRIAD, Werong and NEMO systems.  */
/*                                                                      */
/************************************************************************/
/*									*/
/*= miriad -- Simple Miriad front-end for dumb terminals.		*/
/*& rjs									*/
/*: tools 								*/
/*+
  "miriad" is a command-line front-end to run Miriad tasks from a dumb
  terminal. The commands that you give it are somewhat AIPS-like.
  Unrecognised commands are passed to the host command interpreter.

  The following are the recognised commands:

    set <key> <value>          Set or show a keyword.
    <key>=<value>              Set a keyword.
    unset <key> [<key> ... ]   Unset keyword(s).
    er <key>                   Line-edit a keyword value.
    task [<task>]              Set/show default task.
    inp [<task>]               Show settings of keywords to task.
    go [-b] [<task>]           Run a task.
    help [<task>] [-k key]     Help on a task or topic.
    view [<task>]              Edit keyword file for task.
    save/load [file]           Save/load global keyword file.
    tget/tput [-l] [<task>]    Save/load task keyword file.
    setenv env value           Set environment variable.
    unsetenv env               Unset environment variable.
    cd <dir>                   Change and/or show current directory
    source <file>              Read commands from <file>
    exit                       Exit miriad and save variables.
    quit                       Exit miriad and do not save variables.

  Type
    help tasks
  for more information about Miriad tasks.

/*-- 									*/


#define VERSION_ID "version 1.0 11-Sep-92"

/************************************************************************/
/*  COMPILE DEFINE OPTIONS:                                             */
/*                                                                      */
/*  Use as:  -Doption (unix) or /DEFINE=(option=1) (VMS) in CC          */
/*                                                                      */
/*   <option>      <explanation>                                        */
/*   --------      -------------                                        */
/*  PATHSEARCH     if set, full $PATH is searched instead of $MIRBIN    */
/*  INTERRUPT      if set, interrupts like ^\, ^C, ^Y are caught        */
/*  GETENV         set this if your OS has no char *getenv()            */
/*  READLINE	   GNU readline library is available. In this case, link*/
/*		   with -lreadline -ltermcap.				*/
/*  DO_CSHRC	   Shed csh with just the -c flag (not -cf).		*/
/*									*/
/*  vms		   Set if this is being compiled on VMS.		*/
/*  sun		   Equivalent to -DPATHSEARCH -DINTERRUPT		*/
/*  convex	   Equivalent to -DPATHSEARCH -DINTERRUPT		*/
/*  hpux           Set if this is being compiled on a HP-UX machine.	*/
/************************************************************************/

#ifdef sun
#  define PATHSEARCH 1
#  define INTERRUPT  1
#endif
#ifdef convex
#  define PATHSEARCH 1
#  define INTERRUPT  1
#endif
#ifdef hpux
#  define PATHSEARCH 1
#  define INTERRUPT  1
#endif

/************************************************************************/
/*                                                                      */
/*  History:                                                            */
/*    rjs  Dark-ages Original version.                                  */
/*    pjt   4dec89   Warning when no write permission in save_vars.     */
/*    rjs  21feb90   Some minor enhancements, suggested by Brian        */
/*                   Glendenning.                                       */
/*    pjt  26feb90   Changed environment variables, plus more           */
/*                   descriptive messages.                              */
/*    rjs   5mar90   Merged PJT and RJS versions.                       */
/*    pjt  12mar90   don't write keyfile when not needed                */
/*    pjt  15mar90   'gob' is same as 'go' with backgrounding           */
/*    pjt  16mar90   added save, and help with no options               */
/*    pjt   9apr90   some more help                                     */
/*    rjs  26apr90   Looks in local directory for .doc files. On VMS,   */
/*                   it checks for the foreign command definitiion,     */
/*                   before overwriting it with its own.                */
/*    pjt   6may90   compile option to search $PATH in Unix (execvp)    */
/*    pjt  13may90   -b BIN -d DEF -p PDOC options                      */
/*    pjt  15jun90   added TASK command - to set default task           */
/*                   and SETENV, thinking on tget/tput command          */
/*    pjt  20jun90   quit/exit is now different - load/save have def    */
/*    pjt  10jul90   catch a few signals                                */
/*    rjs  13aug90   Different pager for help command.			*/
/*    rjs  21mar91   The "er" command.					*/
/*    rjs  22may91   Fixed the "er" command!				*/
/*    rjs  22may91   Stole various things from pjt's version.		*/
/*    rjs  20jun91   Various mods to the help command.			*/
/*    rjs  29aug91   Auto TPUT on "go" to $MIRDEF, TGET from $MIRDEF,   */
/*    rjs  16sep91   Better error message.				*/
/*    rjs   9oct91   -l flag for tget and tput.				*/
/*    rjs   1may92   Wait for subprocess to finish, in docommand.	*/
/*    rjs  21may92   Warn when unrecognised variables are set.		*/
/*    rjs   4jun92   Increased some buffers. Better bounds checking.    */
/*                   Removed much pjt bs.                               */
/*    rjs  11sep92   View puts files in MIRDEF.				*/
/*    rjs  19dec92   -Dhpux. csh -cf when shedding a command.		*/
/*    rjs  05dec92   -Dhpux is the only thing to shed with csh -cf.	*/
/*    rjs  16sep93   Background tasks ignore signals.			*/
/*    rjs  20nov93   More args. Better treatment if a environment var   */
/*		     missing.						*/
/*    rjs   1dec93   go command can redirect standard output.		*/
/*    rjs   4oct94   All machines shed csh with -cf.			*/
/*									*/
/*    ToDo anyhow:                                                      */
/*      check earlier if lastexit can be written, otherwise complain    */
/*    Complaints/Wishes from users:                                     */
/*      - why cannot I use aliases                                      */
/*    LGM's ideas:                                                      */
/*      - also remember per keyword which program used it, such that    */
/*        they can be used next to each other                           */
/*                                                                      */
/************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <ctype.h>
#include <string.h>
#if defined(INTERRUPT)
#include <signal.h>
#endif

#ifndef R_OK
#define R_OK 4
#endif
#ifndef X_OK
#define X_OK 1
#endif

#define TRUE  1
#define FALSE 0
#define MAXARGS   64
#define MAXBUF   512
#define MAXARG    32
#define HASHSIZE 127
#define MAXINPUT  10
typedef struct variable { 
    char *name;
    char *value; 
    int user,taught;
    struct variable *fwd; 
} VARIABLE;


VARIABLE *hashtable[HASHSIZE];

struct {
    char *name;
    char *value;
} args[MAXARGS];

char buffer[MAXBUF];

char taskname[MAXBUF];          /* current default name of task */
int echo;
int Qkeys = 0;      /* 0: keys were not updated     1: were */
int input_level = 0;	     /* nesting level of INPUT command */
FILE *fpinput[MAXINPUT];

/* forward references to make (ansi) compilers happy */

#ifndef vms
char *getenv();
void dosetenv(),dounsetenv();
#endif
void get_vars(),save_vars(),doset(),dounset(),doinp(),dogo(),dohelp(),doq(),
     dotask(), dot(), dosource(), doer(), docd(), doload(), dosave(),
     docommand(), doview(), dotput(), dotget(),motd();
void filename(), bug();
int  getline(),task_args();
#if defined(INTERRUPT)
void review();
char *expand(),*translate();
#endif

#ifdef vms
#include <errno.h>
#else
extern int errno;       /* or <errno.h> */
extern char **environ;  /* point to environment */
#endif
/************************************************************************/
main(ac,av)
int ac;
char *av[];
{
  int i,more,argc;
  char *argv[MAXARG];

  echo = 0;
  printf("Miriad shell %s\n",VERSION_ID);
#if defined(INTERRUPT)
  signal(SIGTERM, review);            /* catch interrupts */
  signal(SIGQUIT, review);            /* for review */
  signal(SIGINT,  review);            /* and ^C also */ 
#endif
  for(i=0; i<HASHSIZE; i++){        /* Initialise the hash table. */
        hashtable[i] = NULL;
  }

  get_vars("lastexit");             /* Read "lastexit". */
  motd();			    /* Give the message of the day. */
  strcpy(taskname,"miriad");	    /* The default tawsk is "miriad" */

  more = 1;
  while(more) {                      /* Loop to get a command. */
    argc = getline(argv);
    if(!argc);
    else if(!strcmp(argv[0],"set"))      {echo = input_level == 0;
					  doset(argc,argv);
					  echo = 0;
					  Qkeys++; }
    else if(!strcmp(argv[0],"unset"))    {dounset(argc,argv); Qkeys++; }
    else if(!strcmp(argv[0],"inp"))      {doinp(argc,argv); }
    else if(!strcmp(argv[0],"go"))       {dogo(argc,argv); }
#ifndef vms
    else if(!strcmp(argv[0],"er"))	 {doer(argc,argv);   Qkeys++; }
    else if(!strcmp(argv[0],"setenv"))   {dosetenv(argc,argv); }
    else if(!strcmp(argv[0],"unsetenv")) {dounsetenv(argc,argv); }
#endif
    else if(!strcmp(argv[0],"help"))     {dohelp(argc,argv); }
    else if(!strcmp(argv[0],"view"))     {doview(argc,argv); Qkeys++; }
    else if(!strcmp(argv[0],"save"))     {dosave(argc,argv); }
    else if(!strcmp(argv[0],"load"))     {doload(argc,argv); }
    else if(!strcmp(argv[0],"tget"))     {dotget(argc,argv); }
    else if(!strcmp(argv[0],"tput"))     {dotput(argc,argv); }
    else if(!strcmp(argv[0],"source"))   {dosource(argc,argv);}
    else if(!strcmp(argv[0],"task"))     {dotask(argc,argv); }
    else if(!strcmp(argv[0],"exit"))     {more = 0; }
    else if(!strcmp(argv[0],"quit"))     {Qkeys = 0; more = 0; }
    else if(!strcmp(argv[0],"cd"))	 {docd(argc,argv); }
    else                                 {docommand(argc,argv); }

    if(input_level>0 && more==0) {  /* if exit from input file */
        input_level--;                /* decrease stack of input filesx */
        fclose(fpinput[input_level]); /* close that file */
        more=1;                     /* and keep on trucking */
    }
  } /* while */
  if (Qkeys)
      save_vars("lastexit");     /* Save all the parameters in lastexit. */
  else
      fprintf(stderr,"### Warning: Variables not saved in lastexit\n");

}
/************************************************************************/
void motd()
/*
  Give the message of the day.
------------------------------------------------------------------------*/
{
  char path[MAXBUF];
  static char *command[] = { "help", "motd" };

  filename(path,"MIRPDOC","motd",".doc");
  if(!access(path,R_OK))dohelp(2,command);
}
/************************************************************************/
int getline(argv)
char *argv[];
/*
  This prompts and reads a line from STDIN. It breaks it into tokens.
  If the second token is an equals sign, then this makes it into a
  "set" command.
------------------------------------------------------------------------*/
{
  int n,inter,doset,i,l;
  char prompt[MAXBUF],buffer2[MAXBUF];
  char *s;
  char *expand(),*translate();
#ifdef READLINE
  char *readline();
#endif

/* Get a line from the user. */

  if (input_level==0) {
    s = taskname;
#ifdef READLINE
    sprintf(prompt,"%s%% ",s);
    s = readline(prompt);
    if(s != NULL){
      l = strlen(s);
      while(l-- > 0 && *(s+l) == ' ') *(s+l) = 0;
      if(*s)add_history(s);
      strcpy(buffer2,s);
      free(s);
    }
#else
    printf("%s%% ",s);
    s = gets(buffer2);
#endif
    if(s == NULL) strcpy(buffer2,"exit");

/* Get a line from an input file. */

  } else if(fgets(buffer2,MAXBUF-1,fpinput[input_level-1])==NULL) {
    input_level--;
    fclose(fpinput[input_level]);
    buffer2[0] = 0;
  }

/* Expand anything starting with a $ character. */

  s = expand(buffer,buffer2);
  if(s == NULL)buffer[0] = 0;
  else	       *s = 0;

/* Break the line into words. */

  s = buffer;
  inter = 1;
  n = 0;
  doset = 0;
  while(*s){
    if(*s == ' ' || *s == '\t' || *s == '\n' ){
      inter = 1;
      *s = 0;
    } else if(*s == '#' || *s == '!'){
      *s-- = 0;
    } else if(*s == '=' && n == 1){
      doset = 1;
      inter = 1;
      *s = 0;
    } else if(inter){
      argv[n++] = s;
      inter = 0;
    }
    s++;
  }

/* If it was a set command, shift everything down by one, add the "set"
   to the top of the list, and increase the count. */

  if(doset){
    for(i=n; i>0; i--) argv[i] = argv[i-1];
    n++;
    argv[0] = "set";
  }
  return(n);
}
/************************************************************************/
char *expand(s,t)
char *s,*t;
/*
  Expand any $ characters into the equivalent text or environment variables.
------------------------------------------------------------------------*/
{
  char var[MAXBUF],value[MAXBUF],*u;

  while(s != NULL && *t){
    if(*t == '$'){
      t++;
      u = var;
      while( isalnum(*t) || *t == '_' ) *u++ = *t++;
      *u = 0;
      u = translate(var);
      s = (u == NULL ? NULL : expand(s,u));
    } else *s++ = *t++;
  }
  return(s);
}
/************************************************************************/
char *translate(var)
char *var;
/*
  Return the value of a symbol or environment variable.
------------------------------------------------------------------------*/
{
  int hashval;
  char *t;
  VARIABLE *v;

/* Find the variable. */

  hashval = 0;
  t = var;
  while(*t) hashval += *t++;
  v = hashtable[hashval % HASHSIZE];
  while(v != NULL){
    if(!strcmp(var,v->name)) break;
    v = v->fwd;
  }
  if(v != NULL) return(v->value);

/* If we failed, check for an environment variable. */

#ifndef vms
  t = getenv(var);
#else
  t = NULL;
#endif

  if(t == NULL) fprintf(stderr,"### No such variable: %s\n",var);
  return(t);
}
/************************************************************************/
void doset(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
  char *s,*t,prev;
  int hashval,found,i,len;
  VARIABLE *v;

/* Check the arguments. */
  if(argc == 1) {           /* print all values */
    for(i=0;i<HASHSIZE;i++) {
      v = hashtable[i];
      while(v) {
	if(v->value != NULL)printf("%8s = %s\n",v->name,v->value);
	v = v->fwd;
      }
    }
    return;
  } 

/* Find the value of the parameter, stored in the hash table. */

  hashval = 0;
  t = argv[1];
  while(*t) hashval += *t++;
  v = hashtable[hashval % HASHSIZE];
  while(v != NULL){
    if(!strcmp(argv[1],v->name)) break;
    v = v->fwd;
  }

  if (argc == 2) {          /* print value of one variable */
    if (v!= NULL && v->value != NULL)
        printf("%8s = %s\n",v->name,v->value);
    else
        fprintf(stderr,"### Variable %s not been set yet\n",argv[1]);
    return;
  }

/* Create the variable if needed. Fill in the value. */

  if(v == NULL){
    v = (VARIABLE *)malloc(sizeof(VARIABLE));
    v->fwd = hashtable[hashval % HASHSIZE];
    hashtable[hashval % HASHSIZE] = v;
    v->name = (char *)malloc(strlen(argv[1])+1);
    strcpy(v->name,argv[1]);
    v->user = FALSE;
    v->taught = FALSE;
    v->value = NULL;
  }
  if(!v->user && !v->taught && echo)
     printf("[Creating new variable %s]\n",argv[1]);
  v->user = TRUE;
  if(v->value != NULL) free(v->value);

  len = 0;                          /* figure out how long value is */
  for (i=2; i<argc; i++)
    len += strlen(argv[i])+1;
  v->value = (char *)malloc(len);   /* allocate space for value */
  strcpy(v->value,argv[2]);         /* copy first one */
  for (i=3; i<argc; i++) {          /* and catenate all other ones */
    strcat(v->value,",");
    strcat(v->value,argv[i]);
  }

/* Strip out repeated commas. */

  prev = ',';
  s = t = v->value;
  while(*t){
    if( *t != ',' || prev != ',') *s++ = *t;
    prev = *t++;
  }
  *s++ = 0;
}
/************************************************************************/
void dounset(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
  int i,hashval;
  char *t;
  VARIABLE *v;

  for(i=1; i < argc; i++){
    t = argv[i];
    hashval = 0;
    while(*t) hashval += *t++;
    v = hashtable[hashval % HASHSIZE];
    while(v != NULL && strcmp(argv[i],v->name)) v = v->fwd;
    if(v == NULL) fprintf(stderr,"### Symbol %s was not found.\n",argv[i]);
    else {
      free(v->value);
      v->value = NULL;
      v->user = FALSE;
    }
  }
}
#ifndef vms
/************************************************************************/
void doer(argc,argv)
int argc;
char *argv[];
/*
  A quick edit of various keywords.
------------------------------------------------------------------------*/
{
  char *t,outpath[MAXBUF],inpath[MAXBUF];
  int hashval,pid;
  VARIABLE *v;
  FILE *fd;

/* Check the arguments. */

  if(argc != 2){
    fprintf(stderr,"### Incorrect number arguments\n");
    return;
  }

/* Find the variable. */

  hashval = 0;
  t = argv[1];
  while(*t) hashval += *t++;
  v = hashtable[hashval % HASHSIZE];
  while(v != NULL){
    if(!strcmp(argv[1],v->name)) break;
    v = v->fwd;
  }

/* If the variable was not found, give an error message. */

  if(v == NULL || v->value == NULL){
    fprintf(stderr,"### Variable %s was not found\n",argv[1]);
    return;
  }

/* Write the variable to the line-editors file. */

  filename(inpath,"HOME","cle_in","");
  filename(outpath,"HOME","cle_out","");
  fd = fopen(inpath,"w");
  if(fd == NULL){
    fprintf(stderr,"### Could not write the needed file as %s\n",inpath);
    return;
  }
  fprintf(fd,"     1 %s=%s\n",v->name,v->value);
  fclose(fd);

/* Spawn the line editor. */

  system("cle_exe");

/* Retrieve the variable. */

  get_vars(outpath);  
  unlink(inpath);
  unlink(outpath);
}
#endif
/************************************************************************/
void doinp(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
  int i,n;
  char *task;

  if(argc > 2) fprintf(stderr,"### Extra arguments on line ignored.\n");
  task = ( argc > 1 ? argv[1] : taskname);
  n = task_args(task);
  if(n < 0){
    fprintf(stderr,"### Found no documentation on task %s.\n",task);
  } else if( n == 0){
    printf("There are no parameters for task %s\n",task);
  } else {
    printf("  Task:   %s\n",task);
    for(i=0; i<n; i++)
      printf("  %-9s= %s\n",args[i].name,(args[i].value == NULL ?
                                          " " : args[i].value));
  }
}
/************************************************************************/
void dosource(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
  int i,n;

  if(argc > 2) fprintf(stderr,"### Extra arguments on line ignored.\n");

  if(argc==1) return;
  if(input_level+1 > MAXINPUT) {
	fprintf(stderr,"### Too many nested inputs in %s\n",argv[1]);
	return;
  }
  fpinput[input_level] = fopen(argv[1],"r");
  if (fpinput[input_level] == NULL) {
    fprintf(stderr,"## File %s not found\n",argv[1]);
    return;
  }
  input_level++;
}
/************************************************************************/
void doview(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
  int i,n;
  FILE *fd;
  char name[MAXBUF],command[MAXBUF],*viewer,*task;

  if(argc > 2) fprintf(stderr,"### Extra arguments on line ignored.\n");
  task = (argc > 1 ? argv[1] : taskname);
  n = task_args(task);
  if(n < 0){
    fprintf(stderr,"### Found no documenation on task %s.\n",task);
  } else if( n == 0){
    printf("There are no parameters for task %s\n",task);
  } else {
    filename(name,"MIRDEF",task,".def");
    fd = fopen(name,"w");
    if(fd == NULL){
      fprintf(stderr,"### Failed to open %s\n",name);
      return;
    }
    for(i=0; i<n; i++)
      fprintf(fd,"%-9s= %s\n",args[i].name,(args[i].value == NULL ?
                                          "" : args[i].value));
    fclose(fd);
#ifdef vms
    viewer = "edit";
#else
    if((viewer = getenv("VISUAL")) == NULL)
        if((viewer = getenv("EDITOR")) == NULL) viewer = "vi";
#endif
    sprintf(command,"%s %s",viewer,name);
    system(command);
    get_vars(name);
  }
}
/************************************************************************/
void dotput(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
  int i,n,dolocal;
  FILE *fd;
  char path[MAXBUF],command[MAXBUF],*task,*s;

  task = NULL;
  dolocal = FALSE;
  for(i=1; i < argc; i++){
    s = argv[i];
    if(*s == '-')while(*++s)switch(*s){
      case 'l':	dolocal = TRUE; break;
      default:	fprintf(stderr,"### Unrecognised flag %c ignored\n",*s);
    } else if(task == NULL) task = s;
    else fprintf(stderr,"### Ignoring %s\n",s);
  }
  if(task == NULL) task = taskname;
  if(dolocal)filename(path,"",task,".def");
  else       filename(path,"MIRDEF",task,".def");

  n = task_args(task);
  if(n < 0){
    fprintf(stderr,"### Found no documenation on task %s.\n",task);
    return;
  } else if( n == 0){
    printf("There are no parameters for task %s\n",task);
  } else {
    fd = fopen(path,"w");
    if(fd == NULL){
      fprintf(stderr,"### Failed to open %s\n",path);
      return;
    }
    for(i=0; i<n; i++)
      if(args[i].value != NULL)fprintf(fd,"%-9s= %s\n",args[i].name,args[i].value);
    fclose(fd);
  }
}
#ifdef vms
/************************************************************************/
void dogo(argc,argv)
int argc;
char *argv[];
/*  VMS version (no backgrounding/spawning yet)
------------------------------------------------------------------------*/
{
  FILE *fd;
  int i,n,table;
  char line[MAXBUF],parameter[MAXBUF],*task;
  struct {int length; char *pnt; } name,value;
#define LIB$K_CLI_GLOBAL_SYM 2
#define assign(descriptor,string) descriptor.length = strlen(string);\
                                  descriptor.pnt    = string

  if(argc < 1) return;
  if(argc > 2) fprintf(stderr,"### Extra arguments on line ignored.\n");
  task = ( argc > 1 ? argv[1] : taskname);
  n = task_args(task);
  if(n < 0){
    fprintf(stderr,"### Found no documentation on task %s.\n",task);
  } else {
/* Write out the "TPUT" file. */

    filename(path,"MIRDEF",task,".def");
    fd = fopen(path,"w");
    if(fd == NULL){
      fprintf(stderr,"### Failed to open %s\n",name);
    } else {
      for(i=0; i<n; i++)
        if(args[i].value != NULL)
	  fprintf(fd,"%-9s= %s\n",args[i].name,args[i].value);
      fclose(fd);
    }
/* Check if the foreign command is defined. If not, define it. */
    assign(name,task);
    value.length = MAXBUF; value.pnt = line;
    if(lib$get_symbol(&name,&value) != 1){
      table = LIB$K_CLI_GLOBAL_SYM;
      sprintf(line,"$MIRBIN:%s.exe",task);
      assign(name,task); assign(value,line);
      lib$set_symbol(&name,&value,&table);
    }

/* Build up the command line. */

    strcpy(line,task);
    for(i=0; i<n; i++){                 /* CHECK IF THIS STILL WORKS 15-jun-90 PJT */
      if(args[i].value != NULL){
        sprintf(parameter," %s=%s",args[i].name,args[i].value);
        strcat(line,parameter);
      }
    }
    system(line);
  }
}
#else
/************************************************************************/
void dogo(argc,argv)
int argc;
char *argv[];
/*      Unix version
------------------------------------------------------------------------*/
{
  FILE *fd;
  int i,n,pid,bg,length,fh;
  char *arg[MAXARGS+3],path[MAXBUF],parameter[MAXBUF],*s,**t,*runner;
  char *task,*output;

  if(argc < 1) return;

/* Process command line arguments. */

  bg = 0;
  task   = NULL;
  runner = NULL;
  output = NULL;
  for(i=1; i < argc; i++){
    s = argv[i];
    if(*s == '-')while(*++s != 0)switch(*s){
      case 'r':	if(++i < argc) runner = argv[i]; break;
      case 'b': bg = 1; break;
      default:
	fprintf(stderr,"### Unrecognised flag %c ignored\n",*s);
    } else if( *s == '>'){
      if(*(s+1) != 0) output = s + 1;
      else if(++i < argc) output = argv[i];
    } else {
      task = argv[i];
    }
  }
  if ( task == NULL) task = taskname;
  n = task_args(task);
  if(n < 0){
    fprintf(stderr,"### Found no documentation on task %s.\n",task);
  } else {

/* Write out the "TPUT" file. */

    filename(path,"MIRDEF",task,".def");
    fd = fopen(path,"w");
    if(fd == NULL){
      fprintf(stderr,"### Failed to open %s\n",path);
    } else {
      for(i=0; i<n; i++)
        if(args[i].value != NULL)
	  fprintf(fd,"%-9s= %s\n",args[i].name,args[i].value);
      fclose(fd);
    }

/* Determine the name of the thing that is the exec file. */

    t = arg;
    if(runner != NULL){
      *t++ = runner;
      strcpy(path,runner);
    } else {
#if defined(PATHSEARCH)
      strcpy(path,task);       /* let shell search for exe file */
#else
      filename(path,"MIRBIN",task,"");     /* full name of exe file */
#endif
    }
    *t++ = task;

/* Build the argument list. */

    length = 0;
    for(i=0; i<n; i++){
      if(args[i].value != NULL){
	s = parameter + length;
        length += strlen(args[i].name) + strlen(args[i].value) + 2;
        if(length > MAXBUF){
	  fprintf(stderr,"### Internal bug: Argument list too long\n");
 	  return;
	}
        sprintf(s,"%s=%s",args[i].name,args[i].value);
        *t++ = s;
      }
    }
    *t = NULL;

/* Spawn off the command. */

    pid = fork();
    if(pid < 0) bug("go: Failed to fork a child process");
    if(pid == 0) {         /* inside child now */
      if (bg){
        fprintf(stderr,"[Job %s running in background]\n",task);
#if defined(INTERRUPT)
        signal(SIGTERM, SIG_IGN);
        signal(SIGQUIT, SIG_IGN);
        signal(SIGINT,  SIG_IGN); 
#endif
      }
      if(output != NULL){
	fh = open(output,O_WRONLY|O_CREAT|O_TRUNC,0644);
	if(fh < 0)fprintf(stderr,"### Unable to open redirected output\n");
	dup2(fh,1);
	close(fh);
      }

#if defined(PATHSEARCH)
      execvp(path,arg);         /* let shell look for exe */
#else
      execv(path,arg);          /* path contains full name of exe */
#endif
      perror("go");
      bug("go: Failed to exec the subprocess");
    } else if (!bg) while(pid != wait(NULL));
  }
}
#endif
/************************************************************************/
void dohelp(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
  char path[MAXBUF],command[MAXBUF],*pager,*task;
  int i,k;
  void showbin();

/* Determine the thing we want help on. */

  task = taskname;  k = 0;
  if(argc > 1) if(*(argv[1]) != '-'){ task = argv[1]; k = 1; }

/* Determine the pathname of the help file. */

  if(task_args(task) < 0){
    fprintf(stderr,"### Cannot find and/or read help for %s\n",task);
  }else{
    sprintf(path,"%s.doc",task);
    if(access(path,R_OK))filename(path,"MIRPDOC",task,".doc");
#ifdef vms
    pager = "type/page";
    sprintf(command, "%s %s",pager,path);
#else
    if ( (pager = getenv("PAGER")) == NULL) pager = "more";
    sprintf(command,"docfmt %s",path);
    for(i=k+1; i < argc; i++){
      strcat(command," ");
      strcat(command,argv[i]);
    }
    strcat(command," | "); strcat(command,pager);
#endif
    system(command);
  }
}
/************************************************************************/
void dosave(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
  char path[MAXBUF];

  if(argc > 1) {
    save_vars(argv[1]);
  } else {
    filename(path,"",taskname,".def");
    save_vars(path);
  }
}
/************************************************************************/
void doload(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
  char path[MAXBUF];

  if(argc > 1) {
    get_vars(argv[1]);
  } else {
    filename(path,"",taskname,".def");
    get_vars(path);
  }
}
/************************************************************************/
void dotget(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
  char path[MAXBUF],*vals[MAXARGS+1],*task,*s;
  int n,narg,i,dolocal;

  task = NULL;
  dolocal = FALSE;
  for(i=1; i < argc; i++){
    s = argv[i];
    if(*s == '-')while(*++s)switch(*s){
      case 'l':	dolocal = TRUE;	break;
      default:	fprintf(stderr,"### Unrecognised flag %c ignored\n",*s);
    } else if(task == NULL) task = s;
    else fprintf(stderr,"### Ignoring %s\n",s);
  }
  if(task == NULL) task = taskname;
  if(dolocal)filename(path,"",task,".def");
  else       filename(path,"MIRDEF",task,".def");

  if(!access(path,R_OK)){
    n = task_args(task);
    if(n < 0){
      fprintf(stderr,"### Found no documentation on task %s.\n",task);
      return;
    } else {
      narg = 1;
      vals[0] = "unset";
      for(i=0; i<n; i++)if(args[i].value != NULL)vals[narg++] = args[i].name;
      if(narg > 1)dounset(narg,vals);
    }
    get_vars(path);
    if(task != taskname)strcpy(taskname,task);    
    doinp(1,"inp");
  } else fprintf(stderr,"### Could not read %s.def\n",task);
}
/************************************************************************/
void docd(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
    if (argc == 2) {     /* change directory */
        if (chdir(argv[1]) != 0)
            fprintf(stderr,"### Failed to change directory %s\n",argv[1]);
    	/* printf("Current directory is: ***\n"); */
    } else {            
        if (argc == 1) {    /* if one arg: show current dir */
#ifdef VMS
            system("show default");
#else
            system("pwd");
#endif
        } else
	    fprintf(stderr, "### Incorrect number of arguments\n");
    }
}
/************************************************************************/
void dotask(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
  if(argc > 1) {
    strcpy(taskname,argv[1]);
  } else {
    printf("Current default task is: %s\n",taskname);
  }
}
#ifndef vms
/************************************************************************/
void dosetenv(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
    if ( argc < 3) {
        fprintf(stderr,"### Usage: setenv env_var value\n");
        return;
    }
    newenv(argv[1],argv[2]);
}
/************************************************************************/
void dounsetenv(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
    if ( argc < 2) {
        fprintf(stderr,"### Usage: unsetenv env_var\n");
        return;
    }
    newenv(argv[1],"");
}
#endif
/************************************************************************/
void docommand(argc,argv)
int argc;
char *argv[];
/*
------------------------------------------------------------------------*/
{
  char *s, buffer[MAXBUF];
  int i,pid;

  buffer[0] = '\0';
  for (i=0; i<argc; i++) {      /* accumulate all stuff into buffer */
    strcat(buffer,argv[i]);
    strcat(buffer," ");
  }
  s = buffer;
  while (*s == ' ' || *s == '\t')   /* skip whitespace */
    s++;
  if (! *s || (*s == '#')) return;
#ifdef vms
  system(buffer);         /* and execute it by the host cmd.interpreter */
#else
                        /* In UNIX: pass it such that aliases are known */
  pid = fork();
#ifdef DO_CSHRC
  if (pid == 0) execl("/bin/csh","csh","-c",buffer,NULL);
#else
  if (pid == 0) execl("/bin/csh","csh","-cf",buffer,NULL);
#endif
#if defined(INTERRUPT)
  signal(SIGTERM, SIG_IGN);            /* ignore interrupts by the parent */
  signal(SIGQUIT, SIG_IGN);            /* till the baby dies */
  signal(SIGINT,  SIG_IGN);
#endif
  while(pid !=  wait(NULL));
#if defined(INTERRUPT)
  signal(SIGTERM, review);            /* restore status */
  signal(SIGQUIT, review);            /* of signals */
  signal(SIGINT,  review);
#endif
#endif
}
/************************************************************************/
void get_vars(name)
char *name;
{
  FILE *fd;
  char *argv[3],line[MAXBUF],*s;

  fd = fopen(name,"r");
  if(fd == NULL){
    if(strcmp(name,"lastexit"))fprintf(stderr,"### Error opening %s\n",name);
    return;
  }
  argv[0] = "set";
  while(fgets(line,sizeof(line),fd) != NULL){
    s = line;
    while(*s == ' ' || *s == '\t')s++;
    argv[1] = s;
    while(*s && *s != ' ' && *s != '\t' && *s != '=')s++;
    while(*s && (*s == ' ' || *s == '\t' || *s == '='))*s++ = 0;
    argv[2] = s;
    while(*s && *s != ' ' && *s != '\t' && *s != '\n')s++;
    *s = 0;
    if(*argv[2])doset(3,argv);
  }
  fclose(fd);
}
/************************************************************************/
void save_vars(name)
char *name;
{
  int i;
  VARIABLE *v;
  FILE *fd;
  char line[MAXBUF];

  Qkeys = 0;
  for(i=0; i<HASHSIZE; i++) /* walk through keywords to see if any are there */
    for(v = hashtable[i]; v != NULL; v = v->fwd)
        if(v->value != NULL) Qkeys++;
  if (Qkeys==0) {             /* if none found */
    fprintf(stderr,"[Variables not saved - no changes were made]\n");
    return;                 /* don't write them out */
  }
  /* First a check if writing the file is OK, we don't want to overwrite
   * a file that doesn't seem to look like a keyword file, surely */

  fd=fopen(name,"r");
  if (fd!=NULL) {               /* check that file !! */
    while(fgets(line,sizeof(line),fd) != NULL){
        if ((int)strlen(line) > 0) {
          if (strchr(line,'=')==NULL) {
            fprintf(stderr,"[Will not overwrite keywords to a file (%s)",name);
            fprintf(stderr," which does not look like a keyword file]\n");
            return;
          } else
            break;      /* OK, found an '=', assume its OK to overwrite */
        }
    }
  fclose(fd);   /* and close file again */
  }

  fd = fopen(name,"w");     /* open to write */
  if (fd==NULL) {
    fprintf(stderr,"### Warning: could not write file %s\n",name);
    return;
  }
  for(i=0; i<HASHSIZE; i++)
    for(v = hashtable[i]; v != NULL; v = v->fwd)
      if(v->value != NULL)fprintf(fd,"%s=%s\n",v->name,v->value);
  fclose(fd);
  fprintf(stderr,"[Variables saved in %s]\n",name);
}
/************************************************************************/
int task_args(task)
char *task;
/*
  This gets the arguments, and their values, for a particular task.
------------------------------------------------------------------------*/
{
  char line[MAXBUF],path[MAXBUF],keyword[MAXBUF],*s,*t;
  FILE *fd;
  VARIABLE *v;
  int n,hashval,found;

/* Check both the local directory, and the standard directory for the .doc
   file. */

  sprintf(path,"%s.doc",task);
  fd = fopen(path,"r");
  if(!fd){
    filename(path,"MIRPDOC",task,".doc");
    fd = fopen(path,"r");
  }
  if(!fd)return(-1);

/* Scan the .doc file for the keywords. */

  n = 0;
  while( fgets(line,sizeof(line),fd) != NULL){
    if(sscanf(line,"%%N %s",keyword) == 1){
      strcpy(taskname,keyword);
    } else if(sscanf(line,"%%A %s",keyword) == 1){

/* Find the value of the parameter, stored in the hash table. */

      hashval = 0;
      t = keyword;
      while(*t) hashval += *t++;
      v = hashtable[hashval % HASHSIZE];
      while(v != NULL && strcmp(keyword,v->name))v = v->fwd;
      if(v == NULL){
        v = (VARIABLE *)malloc(sizeof(VARIABLE));
        v->fwd = hashtable[hashval % HASHSIZE];
        hashtable[hashval % HASHSIZE] = v;
        v->name = (char *)malloc(strlen(keyword)+1);
        strcpy(v->name,keyword);
        v->user = FALSE;
        v->value = NULL;
      }
      v->taught = TRUE;

/* Save the name and value. */

      if(n >= MAXARGS){
	fprintf(stderr,"### Internal bug: Too many arguments...aborting\n");
	exit(0);
      }
      args[n].value = v->value;
      args[n].name = v->name;
      n++;
    }
  }
  fclose(fd);
  return(n);
}
/************************************************************************/
void filename(out,envvar,name,type)
char *out,*envvar,*name,*type;
/*
  This makes a filename from the input components.
------------------------------------------------------------------------*/
{
#ifdef vms
  if(envvar && *envvar)sprintf(out,"%s:%s%s",envvar,name,type);
  else       sprintf(out,"%s%s",name,type);
#else
  char *s;
  if(envvar && *envvar){
    s = getenv(envvar);
    if(s == NULL || *s == 0) sprintf(out,"%s%s",name,type);
    else sprintf(out,"%s/%s%s",s,name,type);
  }else sprintf(out,"%s%s",name,type);
#endif
}
/************************************************************************/
void bug(message)
char *message;
/*
  This prints an error message, then exits.
------------------------------------------------------------------------*/
{
  fprintf(stderr,"%s\n",message);
  exit(1);
}
#ifndef vms
/************************************************************************/
newenv(var,value)
char *var, *value;
{
    char **ep, **newep, **epfrom, **epto, *cp;
    int  elen, vlen, nev, i;
/*
        enter a new environment variable. If 'value' is NULL, erase it.

    input:  var         name of env. variable
            value       value of env. variable
------------------------------------------------------------------------*/
    vlen = strlen(var);

    for (nev=0, ep=environ; *ep != NULL; ep++) {            /* loop all vars */
        nev++;                                               /* count # vars */
        elen = strlen(*ep);                       /* length of this variable */
        cp = strchr(*ep,'=');                        /* look for an '=' sign */
        if ( (int)(cp - *ep) == vlen && strncmp(*ep,var,vlen)==0) { /* found */
            if (vlen+(int)strlen(value)+2 > elen) {                 /* reallocate */
                cp = malloc(vlen+strlen(value)+2);
                if (cp==NULL) {
                    fprintf(stderr,"### No space to expand variable %s\n",var);
                    return;
                }
                *ep = cp;
            }
            sprintf(*ep,"%s=%s",var,value);
            return;                                  /* and return to caller */
        }
    }
    /* if it got here: add it as a new environment variable */

    fprintf(stderr,"[Creating environment variable %s=%s]\n",var,value);

    newep = (char **)  malloc( (nev+2) * sizeof(char **) );   /* allocate new */
    for (i=0, epfrom=environ, epto=newep; i<nev; i++)     /* copy old stuff */
       *epto++ = *epfrom++;   
    cp = malloc(vlen+strlen(value)+2);          /* allocate for new one */
    if (cp==NULL) {
        fprintf(stderr,"### No memory to add environment %s=%s\n",var,value);
        return;
    }
    sprintf(cp,"%s=%s",var,value);
    *epto++ = cp;       /* put new thing in array */
    *epto = NULL;       /* terminate array */
    environ = newep;    /* set new environment */
}
/************************************************************************/
/* getenv: a local version, in case *getenv() is not supplied by your OS */

#if defined(GETENV)
char *getenv(var)
char *var;
{
    char **ep, *cp;
    int vlen, elen;

    vlen = strlen(var);
    for(ep=environ; *ep != NULL; ep++) {
        elen = strlen(*ep);                       /* length of this variable */
        cp = strchr(*ep,'=');                       /* look for an '=' sign */
        if ( (int)(cp - *ep) == vlen && strncmp(*ep,var,vlen)==0) { /* found */
            fprintf(stderr,"Found %s\n",*ep);
            cp++;
            return(cp);
        }
    }
    return(NULL);
}

#endif
#endif
/**********************************************************************/
#if defined(INTERRUPT)
void review()
{
  fprintf(stderr,
       "Miriad shell cannot be interrupted, type 'help miriad' for info\n");
}
#endif

