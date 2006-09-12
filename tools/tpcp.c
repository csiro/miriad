/************************************************************************/
/*									*/
/*= tpcp -- Tape copy.							*/
/*& rjs									*/
/*: tools								*/
/*+
tpcp is a utility to copy to or from a tape.

Usage:
  tpcp [-b blocksize] in out

where "in" is the input, which may be either a disk file or a tape
device. Similarly "out" may be either a file or a tape device. 

If the input is a file and the output is a tape device, the "-b"
flag should be used to set the block size of write operations.

Example:
  Tape-to-tape copy:    tpcp /dev/nrst0 /dev/nrst1
  Tape-to-disk copy:	tpcp /dev/nrst0 data.fits
  Disk-to-tape copy:	tpcp -b 2880 data.fits /dev/nrst0
									*/
/*--									*/
/*  History:								*/
/*    rjs   8nov93 Original Miriad version.				*/
/*----------------------------------------------------------------------*/
#define VERSION_ID "version 1.0 8-Nov-93"

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#define MAXBUF 45000

int idec();
void usage();
/************************************************************************/
main(argc,argv)
int argc;
char *argv[];
{
  char buf[MAXBUF],*in,*out,*s,c;
  int fin,fout,length,maxbuf,i;

/* Parse the command line. */

  in = out = NULL;
  maxbuf = MAXBUF;

  for(i=1; i < argc; i++){
    s = argv[i];
    if(*s == '-'){
      s++;
      while(c = *s++)switch(c){
	case 'b':
	  if(++i < argc)maxbuf = idec(argv[i]);
	  break;
	case '?':
	  usage();
	  exit(0);
	default:
	  fprintf(stderr,"### Unrecognised flag %c ignored\n",c);
      }
    } else if(in == NULL) in = argv[i];
    else		  out = argv[i];
  }
  if(in == NULL || out == NULL){
    usage();
    fprintf(stderr,"### An input and output must be given\n");
    exit(0);
  }

  printf("Opening %s\n",in);
  fin = open(in,O_RDONLY,0644);
  if(fin <= 0){perror("tpcp");exit(1);}
  printf("Opening %s\n",out);
  fout = open(out,O_WRONLY|O_CREAT|O_TRUNC,0644);
  if(fout <= 0){perror("tpcp");exit(1);}

  while((length = read(fin,buf,maxbuf)) > 0){
    length = write(fout,buf,length);
    if(length <= 0)break;
  }
  if(length != 0)perror("tpcp");
  close(fin);
  close(fout);
  exit(length);
}
/************************************************************************/
void usage()
{
  fprintf(stderr,"TPCP: Version %s\n",VERSION_ID);
  fprintf(stderr,"Usage:\n");
  fprintf(stderr,"   tpcp [-b blocksize] in out\n");
}
/************************************************************************/
int idec(s)
char *s;
{
  int l;

  l = 0;
  while(*s) l = 10*l + *s++ - '0';
  return l;
}  

