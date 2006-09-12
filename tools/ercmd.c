/************************************************************************

	A simple command line editor -- derived from code
	written by Tony Beasley.

	Usage:
	  ercmd args ....
	The arguments are echoed back to /dev/tty and the user
	is allowed to edit them. When a newline is typed, the
	resultant line is echoed to stdout.

   History:
    rjs  16feb95 Derived from "er" command.

------------------------------------------------------------------------*/
#include <sgtty.h>
#include <signal.h>
#include <sys/file.h>
#include <sys/ioctl.h>

# define LENGTH 256 	/* Maximum length of command */

main(argc,argv)
int argc;
char *argv[];
{ 
  char ch,buffer[LENGTH],line[LENGTH+20];
  int j,ct,i,in_length,c_length;
  struct sgttyb b,old_b;

/* If there is no argument, just exit. */

   if(argc <= 1)return(0);

/* Ignore control-c etc.- Use control-x to abort */

   signal(SIGINT,SIG_IGN);
   signal(SIGQUIT,SIG_IGN);
   signal(SIGTERM,SIG_IGN);
   signal(SIGTSTP,SIG_IGN);

/* Set the required terminal characteristics.....*/

  ct = open("/dev/tty",O_RDWR);
  ioctl(ct,TIOCGETP,&old_b);
  ioctl(ct,TIOCGETP,&b);
  b.sg_flags = CRMOD|CBREAK;
  ioctl(ct,TIOCSETP,&b);

/* Open the terminal for standard i/o. */
  
  strcpy(buffer,argv[1]);
  for(j=2; j < argc; j++){
    strcat(buffer," ");
    strcat(buffer,argv[j]);
  }

  output(ct,"\15\33[K");output(ct,buffer); 
  c_length = in_length = strlen(buffer);

  while((ch = mychar(ct)) != '\n'){ 
    switch (ch){
     case '': if ((ch = mychar(ct)) != '[') {}    /*The arrows....*/

                 else { switch ((ch = mychar(ct)))
                      { case 'A': break;
                        case 'B': break;
                        case 'C': if ( c_length < in_length)
				  {output(ct,"\33[C"); c_length++;};break;
                        case 'D': if ( c_length > 0)
				  {output(ct,"\33[D"); c_length--;};break;
                        default : break;};
                      }; break;
     case '':					/* Backspace */
     case '': if (c_length == 0) break;	/* Delete */
		wipe(buffer,c_length,1);
	        in_length--;
		c_length--;
                output(ct,"\10\33[1P"); break;

/*     case '': c_length = 0; output(ct,"\15"); break; */   /* Backspace */
     case '': output(ct,"\15"); output(ct,buffer);
		c_length = in_length; break; /* EOL */
     case '':
     case '': ioctl(ct,TIOCSETP,&old_b);
	        output(ct,"\n"); exit(0); break;   /* Quit, resetting terminal */
     case '': while(c_length > 0){
		 c_length--;
		 output(ct,"\33[D");
		}
		break;
     case '': for (j=1;
		!(buffer[c_length+j] == ' ' && buffer[c_length+j+1] != ' ')
	        && (c_length+j+1 <= in_length); j++) { output(ct,"\33[C");}; 
	        if(c_length+j <= in_length)
		{output(ct,"\33[C");
		c_length += j; break;}; break; /* Skip forwards on words */

     case '': for (j=1;
		!(buffer[c_length-j] == ' ' && buffer[c_length-j-1] != ' ')
	        && (c_length-j-1 >= 0); j++) { output(ct,"\33[D");}; 
	        if(c_length-j >= 0)
		{output(ct,"\33[D"); 
		c_length -= j; break;}; break; /* Skip backwards on words */

      default : if (c_length == LENGTH-1) break ;
		if(ch == '\t') ch = ' ';
		insert(buffer,c_length,ch);     /* Insert */
	        in_length++;
	        c_length +=1;
	        output(ct,"\0337\15");output(ct,buffer);output(ct,"\0338\33[C");
	        break;
      }
    }
    output(ct,"\n");
    ioctl(ct,TIOCSETP,&old_b);

    printf("%s\n",buffer);
    return(0);
}
/************************************************************************/
int mychar(fd)
int fd;
{
  char ch;
  if(read(fd,&ch,1) != 1) return(0);
  else return(ch);
}
/************************************************************************/
output(fd,line)
int fd;
char *line;
{
  int length;
  length = strlen(line);
  write(fd,line,length);
}
/************************************************************************/
insert(line,coord,ch)
char *line;
int coord;
char ch;
/*
  Insert character ch in line[coord], shifting the rest of the array
------------------------------------------------------------------------*/
{
  char temp[LENGTH];
  int i,j;
  for ( j = 0; j < coord; j++) temp[j] = line[j];
  temp[coord] = ch;
  for ( i = coord + 1;i <= strlen(line)+1; i++){temp[i] = line[i-1];};
  strcat(temp,"\0");
  strcpy(line,temp);
}
/************************************************************************/
wipe(line,coord,size)
char *line;
int coord,size;
/* 
    Wipe out line[coord] to line[coord+size]
------------------------------------------------------------------------*/
{       
  char temp[LENGTH];
  int i,j;
  for ( j = 0; j < (coord - 1) ; j++) temp[j] = line[j];
  for ( i = 0;  (i + coord + size -1) <= strlen(line) ; i++){
    temp[i + coord - 1] = line[i + coord + size -1];
  }
  strcat(temp,"\0");
  strcpy(line,temp);
}
