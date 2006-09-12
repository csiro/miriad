/************************************************************************/
/*									*/
/*  newer is a command which searches a directory, looking for source	*/
/*  code files, that are younger than a particular "timefile". When a	*/
/*  file is found, it is passed to the command given by the -e flag,	*/
/*  (or echoed to stdout if no -e flag present). If a file with the	*/
/*  root name is found in multiple directories, only the first is	*/
/*  considered.								*/
/*									*/
/*  Usage:								*/
/*    newer [-i] [-e command] -x exclude timefile dirs ...		*/
/*									*/
/*  History:								*/
/*    rjs  3mar90 Original version.					*/
/*    rjs  9mar90 Fixed Cray-2 portability problem.			*/
/*    rjs 23mar90 Fixed bug in variable initialisation in hashcheck.	*/
/*		  Added -x flag.					*/
/*    rjs  4jan93 Support .f2c files.					*/
/*    rjs  5oct95 -i flag.						*/
/*									*/
/*  Compiling:								*/
/*    You must specify one (and only one) of the following "defines"	*/
/*    when compiling -- to indicate the machine type.			*/
/*    This dictates the way directories are search.			*/
/*      posix		The new POSIX style of handling directories.	*/
/*      sysv		Old System V (without opendir, etc).		*/
/*      bsd		Old BSD -- not the posix directory setup.	*/
/*      vms		Vms-style routines.				*/
/*----------------------------------------------------------------------*/

#define TRUE 1
#define FALSE 0
#define MAXBUF 256
#define MAXDIRS 128
#include <stdio.h>
#include <ctype.h>

/* Some definitions, etc, to help make BSD, VAX-C and System-V look alike. */

#ifdef vms
#  include <descrip.h>
#  include <stat.h>
#  define time_t int
   struct dirent {char d_name[MAXBUF]; int d_namlen;};
   typedef struct { struct dsc$descriptor_s dir;
		 char *contxt,name[MAXBUF];} DIR;
   DIR *opendir();
   struct direct *readdir();
   void closedir();
#endif

#ifdef posix
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <dirent.h>
#endif

#ifdef bsd
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <sys/dir.h>
#  define dirent direct
#endif

#ifdef sysv
#  include <sys/types.h>
#  include <sys/stat.h>
#  include <sys/param.h>
#  ifndef DEV_BSIZE
#    define DEV_BSIZE 512
#  endif

#  define DIRBLKSIZ DEV_BSIZE
#  define MAXNAMLEN 255

  struct dirent{
    long d_ino;
    short d_reclen;
    short d_namlen;
    char d_name[MAXNAMLEN+1]; } ;

#  undef DIRSIZ
#  define DIRSIZ(dp) \
     ((sizeof(struct dirent) - (MAXNAMLEN+1)) \
     + (((dp)->d_namlen+1 +3) & ~3))

  typedef struct { int dd_fd;
		 long dd_loc;
		 long dd_size;
		 char dd_buf[DIRBLKSIZ]; } DIR;
  DIR *opendir();
  struct dirent *readdir();
  void closedir();
#endif

  

#define private static

private void process(),process2();
private int hashcheck();

#define HASHSIZE 251
typedef struct hashent { char *name; struct hashent *fwd; int level;} HASHENT;
HASHENT *hashtable[HASHSIZE];

char *extensions[] = {"for","f","c","s","f2c","mar","h","csh",
		      "icon","latex","txt","msg","macro","doc",
		      "toc","aux","nul","awk","C","cc"};
#define NEXT (sizeof(extensions)/sizeof(char *))
/************************************************************************/
int main(argc,argv)
int argc;
char *argv[];
{
  int ndir,i,doind;
  char *dirs[MAXDIRS],*command,*timefile,*s;
  struct stat buf;
  time_t mtime;

/* Initialise the hash table. */

  for(i=0; i < HASHSIZE; i++) hashtable[i] = NULL;

/* Process the command line arguments. */

  doind = FALSE;
  ndir = 0;
  timefile = command = NULL;
  for(i=1; i < argc; i++){
    s = argv[i];
    if(*s == '-'){
      while(*++s)switch(*s){
	case 'e': if(++i < argc) command = argv[i];
		  break;
	case 'x': if(++i < argc) (void)hashcheck(argv[i],-1);
		  break;
        case 'i': doind = TRUE;
		  break;
	default:  fprintf(stderr,"Unrecognised flag %c ignored\n",*s);
		  break;
      }
    } else if(timefile == NULL) timefile = argv[i];
    else 			dirs[ndir++] = argv[i];
  }

/* Check that we were given enough info. */

  if(ndir == 0 || timefile == NULL){
    fprintf(stderr,"Usage:\n");
    fprintf(stderr,"  mirnewer [-i] [-e command] [-x exclude] timefile dirs ...\n");
    exit(1);
  }

/* Check the date on the timefile. */

  if(!stat(timefile,&buf)) mtime = buf.st_mtime;
  else			   mtime = 0;
/*  printf("Mtime = %d\n",mtime); */


/* Process all the directories. */

  if(doind){
    for(i=0; i < ndir; i++) process2(mtime,command,dirs[i]);
  } else {
    for(i=0; i < ndir; i++) process(mtime,command,i,dirs[i]);
  }
  return(0);
}
/************************************************************************/
private void process(mtime,command,level,dir)
char *command,*dir;
time_t mtime;
int level;
/*
  This goes through all the files in a directory, picking those that look
  like candidates to process. For each of these, it checks if they are
  already in the hash table, and what there modification time is. If they
  are not in the hash table, and they are younger than the time given by
  mtime, then they are processed by "command".
------------------------------------------------------------------------*/
{
  char full[MAXBUF],name[MAXBUF],ext[MAXBUF],root[MAXBUF],*t,*dotpnt;
  char line[MAXBUF],added[MAXBUF],c;
  int found,i,length,ll,status;
  DIR *dirp;
  struct dirent *dp;
  struct stat buf;
  

/* Form the root directory spec. */

  strcpy(root,dir);
  c = *(dir + strlen(dir) - 1);
  if(isalnum(c) || c == '.')strcat(root,"/");
  added[0] = 0;

/* Open the directory. */

  dirp = opendir(dir);
  if(dirp == NULL){
    fprintf(stderr,"Failed to open %s, continuing ...\n",dir);
    return;
  }
  for( dp = readdir(dirp); dp != NULL; dp = readdir(dirp)){

/* Strip off any directory specification. Do this by searching backwards
   for the first :, ] or /. */

    strcpy(full,root);
    strcat(full,dp->d_name);
/*    printf("Full name: %s\n",full); */

/* Scan forward to the first dot and the first semicolon. */

    t = dp->d_name;
    ll = strlen(dp->d_name);
    *(t+ll) = 0;
    dotpnt = NULL;
    while(*t){
      if(dotpnt == NULL && *t == '.') dotpnt = t;
      t++;
    }

/* Reject names without name parts or extension parts. */

    if(dotpnt == NULL || dotpnt == dp->d_name)continue;

/* Get the main part and the extension. */

    length = dotpnt - dp->d_name;
    strncpy(name,dp->d_name,length);
    name[length] = 0;
    length = dp->d_name + ll - dotpnt - 1;
    strncpy(ext,dotpnt+1,length);
    ext[length] = 0;
/*    printf("%s|%s\n",name,ext); */

/* We have the full name, the name and the extension. Check if the extension
   is one of the ones we are interested in. */

    found = FALSE;
    for(i=0; i < NEXT; i++) found = found || !strcmp(ext,extensions[i]);
    if(!found) continue;

/* Check if it is in a lower level directory. */

    if(hashcheck(name,level)) continue;

/* Remember ones that we have just added to the hash file. */

/* Check if its modification time is more recent than the timefile. If
   so, process this file. */

    if(stat(full,&buf)) continue;
/*    printf("%s: %ld\n",full,buf.st_mtime); */
    if(buf.st_mtime < mtime) continue;
    if( command == NULL ) printf("%s\n",full);
    else {
      sprintf(line,"%s %s",command,full);
/*    printf("%s\n",line); */
      status = system(line);
      if(status){
	printf("### %s: returned with status=%d\n",line,status);
        exit(1);
      }
    }
  }
  closedir(dirp);
}
/************************************************************************/
private void process2(mtime,command,full)
time_t mtime;
char *full,*command;
{
  struct stat buf;
  char line[MAXBUF];

  if(stat(full,&buf)) return;
  if(buf.st_mtime < mtime) return;
  if( command == NULL ) printf("%s\n",full);
  else {
    sprintf(line,"%s %s",command,full);
    system(line);
  }
}
/************************************************************************/
private int hashcheck(name,level)
char *name;
/*
  This returns TRUE if the name was found at a lower level, and
  FALSE if the name was not found, or found at this level. Regardless,
  when this returns, this name is remembered at the lowest level that
  it was found at.
------------------------------------------------------------------------*/
{
  int k;
  char *s;
  HASHENT *h;

/* Determine the hash entry. */

  s = name;
  k = 0;
  while(*s) k += *s++;

/* Check the hash table, and return if it is found. */

  for(h = hashtable[k % HASHSIZE]; h != NULL; h = h->fwd)
    if(!strcmp(h->name,name)) return( h->level < level );

/* It was not in the hash table. Add it. */

  h = (HASHENT *)malloc(sizeof(HASHENT));
  if(h == NULL){
    fprintf(stderr,"Failed to add an entry to the hash table, aborting\n");
    exit(1);
  }
  h->name = (char *)malloc(strlen(name)+1);
  if(h->name == NULL){
    fprintf(stderr,"Failed to add an entry to the hash table, aborting\n");
    exit(1);
  }
  h->fwd = hashtable[k % HASHSIZE];
  h->level = level;
  hashtable[k % HASHSIZE] = h;
  strcpy(h->name,name);
  return(FALSE);
}
#ifdef vms
/************************************************************************

  The following are VMS-specific routines that in some way emulate the
  equivalent UNIX routines.

*************************************************************************/
DIR *opendir(name)
char *name;
{
  DIR *dirp;
  dirp = (DIR *)malloc(sizeof(DIR));
  strcpy(dirp->name,name);
  dirp->dir.dsc$w_length = strlen(dirp->name);
  dirp->dir.dsc$b_dtype  = DSC$K_DTYPE_T;
  dirp->dir.dsc$b_class  = DSC$K_CLASS_S;
  dirp->dir.dsc$a_pointer = dirp->name;
  dirp->contxt = NULL;
  return(dirp);
}
/************************************************************************/
void closedir(dirp)
DIR *dirp;
{
  lib$find_file_end(&dirp->contxt);
  free((char *)dirp);
}
/************************************************************************/
struct dirent *readdir(dirp)
DIR *dirp;
{
  char *p,path[MAXBUF];
  static struct dirent d;
  struct dsc$descriptor_s Path = 
   {sizeof(path)-1,DSC$K_DTYPE_T,DSC$K_CLASS_S,path};
  struct dsc$descriptor_s asterisk = 
   {5,DSC$K_DTYPE_T,DSC$K_CLASS_S,"*.*;0"};

  if(lib$find_file(&dirp->dir,&Path,&dirp->contxt,&asterisk)%2){
    p = path + Path.dsc$w_length;

/* Trim back across blanks and the version. Zero terminate it.*/

    while(*--p != ';');
    if(*(p-1) == '.') p--;
    *p = 0;

/* Trim back to the last ] or :. */

    p--;
    while(*p != ':' && *p != ']') p--;
    p++;

/* Return with the goodies. */

    strcpy(d.d_name,p);
    p = d.d_name;
    while(*p){
      if(isupper(*p)) *p = *p + 'a' - 'A';
      p++;
    }
    d.d_namlen = strlen(d.d_name);
    return(&d);

  } else return(NULL);
}
#endif
#ifdef sysv
/************************************************************************/
/*									*/
/*	This is a set of routines which emulate the BSD directory	*/
/*	reading routines, for SysV systems.				*/
/*	Copied from: Appendix D, "Portable C and UNIX system		*/
/*	programming", J.E.Lapin, Prentice Hall Software Series.		*/
/*									*/
/*----------------------------------------------------------------------*/
#define ODIRSIZ 14
struct olddirect{ short od_ino;
 		  char od_name[ODIRSIZ]; };

/**********************************************************************/
DIR *opendir(name)
char *name;
/*
	Open a directory.
----------------------------------------------------------------------*/
{
  DIR *dirp;
  int fd;

  if((fd = open(name,0)) == -1) return(NULL);

  if((dirp = (DIR *)malloc(sizeof(DIR))) == NULL){
    close(fd);
    return(NULL);
  }
  dirp->dd_fd = fd;
  dirp->dd_loc = 0;
  return(dirp);
}
/**********************************************************************/
struct dirent *readdir(dirp)
DIR  *dirp;
/*
  Get next entry in a directory.
----------------------------------------------------------------------*/
{
  struct olddirect *dp;
  static struct dirent dir;
  for(;;){
    if(dirp->dd_loc == 0){
      dirp->dd_size = read(dirp->dd_fd,dirp->dd_buf,DIRBLKSIZ);
      if(dirp->dd_size <= 0)return(NULL);
    }

    if(dirp->dd_loc >= dirp->dd_size){
      dirp->dd_loc = 0;
      continue;
    }
    dp = (struct olddirect *)(dirp->dd_buf+dirp->dd_loc);
    dirp->dd_loc += sizeof(struct olddirect);

    if(dp->od_ino == 0) continue;

    dir.d_ino = dp->od_ino;
    strncpy(dir.d_name,dp->od_name,ODIRSIZ);
    dir.d_name[ODIRSIZ] = ' ';
    dir.d_namlen = strlen(dir.d_name);
    dir.d_reclen = DIRSIZ(&dir);
    return(&dir);
  }
}
/**********************************************************************/
void closedir(dirp)
DIR *dirp;
/*
  Closea directory.
----------------------------------------------------------------------*/
{
  close(dirp->dd_fd);
  free((char *)dirp);
}
#endif
