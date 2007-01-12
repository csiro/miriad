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
    rjs  23apr99 Bring it into line with modern UNIX.
    rjs  02jan07 Some standardizing.

------------------------------------------------------------------------*/
#include <string.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>
#include <signal.h>

# define LENGTH 256 	/* Maximum length of command */

static int mychar();
static void output(),insert(),wipe();

/*------------------------------------------------------------------------*/
int main(argc,argv)
int argc;
char *argv[];
{ 
  char ch,buffer[LENGTH],line[LENGTH+20];
  int j,ct,i,in_length,c_length;
  struct termios b,old_b;

/* If there is no argument, just exit. */

   if(argc <= 1)return(0);

/* Ignore control-c etc.- Use control-x to abort */

   signal(SIGINT,SIG_IGN);
   signal(SIGQUIT,SIG_IGN);
   signal(SIGTERM,SIG_IGN);
   signal(SIGTSTP,SIG_IGN);

/* Set the required terminal characteristics.....*/

  ct = open("/dev/tty",O_RDWR);
  tcgetattr(ct,&old_b);
  tcgetattr(ct,&b);
  b.c_iflag &= ~(BRKINT|IGNBRK);
  b.c_lflag &= ~(ECHO|ICANON);
  b.c_cc[VERASE] = 0;
  b.c_cc[VMIN] = 1;
  b.c_cc[VTIME] = 0;
  tcsetattr(ct,TCSANOW,&b);

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

/* Arrow keys. */     
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

/* Delete character. */
     case '':	if(c_length == in_length)break;	/* Ctrl/D */
	        wipe(buffer,c_length+1,1);
		in_length--;
		output(ct,"\33[C\10\33[1P"); break;

/* Delete character backwards. */

     case '':					/* Backspace */
     case '': if (c_length == 0) break;	/* Delete */
		wipe(buffer,c_length,1);
	        in_length--;
		c_length--;
                output(ct,"\10\33[1P"); break;

/* End of line. */

     case '': output(ct,"\15"); output(ct,buffer);
		c_length = in_length; break; /* EOL */

/* Exit. */
     case '':
     case 0:
     case '': tcsetattr(ct,TCSANOW,&old_b);
	        output(ct,"\n"); exit(0); break;   /* Quit, resetting terminal */

/* Beginning of line. */

     case '': while(c_length > 0){
		 c_length--;
		 output(ct,"\33[D");
		}
		break;

/* Back character. */

     case '': if ( c_length > 0 )
		{output(ct,"\33[D"); c_length--;}break;

/* Forward character. */

     case '': if ( c_length < in_length)
		{output(ct,"\33[C"); c_length++;} break;

/* Skip forward word. */

     case '': for (j=1;
		!(buffer[c_length+j] == ' ' && buffer[c_length+j+1] != ' ')
	        && (c_length+j+1 <= in_length); j++) { output(ct,"\33[C");}; 
	        if(c_length+j <= in_length)
		{output(ct,"\33[C");
		c_length += j; break;}; break; /* Skip forwards on words */

/* Skip backwards word. */

     case '': for (j=1;
		!(buffer[c_length-j] == ' ' && buffer[c_length-j-1] != ' ')
	        && (c_length-j-1 >= 0); j++) { output(ct,"\33[D");}; 
	        if(c_length-j >= 0)
		{output(ct,"\33[D"); 
		c_length -= j; break;}; break; /* Skip backwards on words */

/* Transpose characters. */

     case '': if ( c_length == 0 || c_length == in_length)break;
		ch = buffer[c_length-1];
		wipe(buffer,c_length,1);
		insert(buffer,c_length,ch);     /* Insert */

	        output(ct,"\10\33[1P\33[C\0337\15");
		output(ct,buffer);
		output(ct,"\0338\33[C\33[D");
		break;

/* Insert character. */

      default : if (c_length == LENGTH-1) break ;
		if(ch == '\t') ch = ' ';
		if(ch >= ' ' && ch < 127) {
		  insert(buffer,c_length,ch);     /* Insert */
	          in_length++;
	          c_length +=1;
	          output(ct,"\0337\15");output(ct,buffer);output(ct,"\0338\33[C");
		}
	        break;
      }
    }
    output(ct,"\n");
    tcsetattr(ct,TCSANOW,&old_b);
    close(ct);

    printf("%s\n",buffer);
    return(0);
}
/************************************************************************/
static int mychar(fd)
int fd;
{
  char ch;
  if(read(fd,&ch,1) != 1) return(0);
  else return(ch);
}
/************************************************************************/
static void output(fd,line)
int fd;
char *line;
{
  int length;
  length = strlen(line);
  write(fd,line,length);
}
/************************************************************************/
static void insert(line,coord,ch)
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
static void wipe(line,coord,size)
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
