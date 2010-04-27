/****************************************************************************/
/*= doc -- Strip preamble comments out of miriad source files               */
/*& rjs                                                                     */
/*: tools                                                                   */
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
/*-                                                                         */

/*****************************************************************************
*
*  History:
*    Long ago rjs  Original version.
*    02mar90  bpw  Full rewrite with extended functionality.
*    19nov98  rjs  Full rewrite.
*    21jul99  rjs  Improved indenting algorithm somewhat. Added X command.
*     6feb00  rjs  Support for Perl.
*    22may06  rjs  Change to appear cygwin.
*    14jul06  mrc  Get it to compile with 'gcc -Wall' without warnings.
*
* $Id: doc.c,v 1.4 2010/04/27 07:58:01 cal103 Exp $
*****************************************************************************/

#define VERSION "Doc: version 1.4 2010/04/27"
#define private static
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE 128

#define TRUE 1
#define FALSE 0

private void usage(void);
private void getmodes(char *name, char **infile);
private void process(FILE *fin, FILE *fout, char *infile, char *outdir,
                     int doex);
private void getindent(char *indent, char *line);
private int  parseline(char *buf, char *line);

#define NXT 9
static char *exts[] = {"?", "sh", "csh", "pl", "c",  "cc", "C",  "f", "for"};
static char *com1[] = {"?", "#",  "#",   "#",  "/*", "/*", "/*", "C", "C"};
static char *com2[] = {"?", "#",  "#",   "#",  "//", "//", "//", "c", "c"};
char *cs1, *cs2;

/****************************************************************************/
int main(int argc, char *argv[])

{
  char *infile, *outfile, *s, *outdir;
  int i, nin, redir, doex;
  FILE *infd, *outfd;

  /* Process the command line. */
  doex = 1;
  redir = 0;
  outdir  = NULL;
  outfile = NULL;
  nin = 0;
  for (i=1; i < argc; i++) {
    s = argv[i];

    /* Flags and switches. */
    if (*s == '-') {
      argv[i] = NULL;
      while (*++s) {
        switch(*s) {
        case '?':
          usage();
          exit(0);
        case 'p':
          break;
        case 'x':
          doex = 0;
          break;
        case 'f':
          redir = 1;
          break;
        case 'O':
        case 'o':
          if (++i < argc) {
            outdir  = argv[i];
            argv[i] = NULL;
          }
          break;
        default:
          fprintf(stderr, "Unrecognised flag %c\n", *s);
        }
      }

    } else if (*s == '>') {
      /* An output file */
      argv[i++] = NULL;
      if (i < argc) {
        outfile = argv[i];
        argv[i] = NULL;
      }

    } else {
      /* An input file. */
      nin++;
    }
  }

  /* Open the output file, if required. */
  if (redir) {
    if (outfile == NULL) {
      outfd = stdout;
    } else {
      outfd = fopen(outfile, "w");
    }

    if (outfd == NULL) {
      fprintf(stderr, "### Error redirecting output to %s\n", outfile);
      exit(1);
    }
  } else {
    outfd = NULL;
  }

  /* Process each of the input files. */
  if (nin > 0) {
    for (i=1; i<argc; i++) {
      if (argv[i] != NULL) {
        getmodes(argv[i], &infile);
        infd = fopen(argv[i], "r");
        if (infd == NULL) {
          fprintf(stderr, "### Error opening %s\n", argv[i]);
          exit(1);
        }

        process(infd, outfd, infile, outdir, doex);
        fclose(infd);
      }
    }
  }

  exit(0);
}


/****************************************************************************/
private void usage(void)

{
  printf("%s\n", VERSION);
  printf("This strips preamble comments from Miriad source files.\n\n");
  printf("Usage:\n");
  printf("doc [-?] [-f] [-O outdir] infile > outfile\n");
}


/*****************************************************************************
* Determine the language type of the input file from its extension:
*
*    Extension      Language    Comment Char
*   ------------    --------    ------------
*   .sh,.csh,.pl    Shells      #
*   .c, .cc         C or C++    Normal C comments.
*   .f, .for        Fortran     C  or c
*---------------------------------------------------------------------------*/
private void getmodes(char *name, char **infile)

{
  char *s;
  int l, i;

  s = name + strlen(name);
  while (s > name && *(s-1) != ']' && *(s-1) != '/') s--;
  *infile = s;
  if (*s == 0) {
    fprintf(stderr, "### Invalid file name: %s\n", name);
    exit(1);
  }

  while (*s && *s != '.') s++;
  if (*s) {
    s++;
    l = -1;

    for (i = 1; i < NXT; i++) {
      if (strcmp(s, exts[i]) == 0) l = i;
    }

    if (l < 0) {
      fprintf(stderr, "### Unrecognised file extension for: %s\n", name);
      exit(1);
    }

  } else {
    /* No file extension, assume it's a shell script for now. */
    l = 0;
  }

  cs1 = com1[l];
  cs2 = com2[l];
}


/*****************************************************************************
* Parse a Miriad .doc file (FILE *fin), writing output to (FILE *fout).
*
* Inputs:
*   fin         File descriptor of the input.
*   fout        File descriptor of the output.
*   doex        Obey the "exclude" command.
*---------------------------------------------------------------------------*/
private void process(FILE *fin, FILE *fout, char *infile, char *outdir,
  int doex)

{

#define OUTSIDE  0
#define PREAMBLE 1
#define MAIN     2
#define EXCLUDE  3

  char author[MAXLINE], buf[MAXLINE], cat[MAXLINE], indent[MAXLINE],
       line[MAXLINE], one[MAXLINE], outname[MAXLINE], task[MAXLINE];
  char *t, *u, *v;
  char c;
  int  dosub = 0, doslash, mode, s;
  FILE *foutd;

  *indent = 0;

  /* Process the input file. */
  doslash = FALSE;
  if (outdir) {
    s = *(outdir + strlen(outdir) - 1);
    doslash = isalnum(s);
  }
  *task = *one = *cat = *author = *indent = 0;
  foutd = NULL;
  mode = OUTSIDE;

  if ((t = fgets(buf, MAXLINE, fin)) == NULL) return;

  if (*cs1 == '?') {
    /* Check whether we do, in fact, have a shell script. */
    if (strncmp(t, "#!", 2)) {
      fprintf(stderr, "### Cannot determine file type: %s\n", infile);
      exit(1);
    }

    cs1 = com1[1];
    cs2 = com2[1];
  }

  while ((s = parseline(buf, line))) {
    t = line;
    while (isspace(*t)) t++;

    switch(s) {
    case '=':
    case '*':
      /* Start of metadata. */
      if (mode == EXCLUDE ) {
        mode = OUTSIDE;
      } else if (mode == OUTSIDE) {
        dosub = (s == '*');
        u = task;
        while (*t != ' ' && *t != '-' && *t != 0 ) {
          c = *t++;
          if (c >= 'A' && c <= 'Z') c = c - 'A' + 'a';
          *u++ = c;
        }
        *u = 0;
        if (*task) mode = PREAMBLE;
        while (*t == ' ' || *t == '-') t++;
        strcpy(one, t);
      }
      break;

    case '&':
      /* Author. */
      if (mode == PREAMBLE) strcpy(author, t);
      break;

    case ':':
      /* Category. */
      if (mode == PREAMBLE) strcpy(cat, t);
      break;

    case '+':
      /* End of preamble. */
      if (mode == PREAMBLE) {
        if (fout != NULL ) {
          foutd = fout;
        } else {
          if (outdir) {
            strcpy(outname, outdir);
            if (doslash) strcat(outname, "/");
            strcat(outname, task);
          } else {
            strcpy(outname, task);
          }

          strcat(outname, ".doc");
          foutd = fopen(outname, "w");
          if (foutd == NULL) {
            fprintf(stderr, "### Error opening output: %s\n", outname);
            exit(1);
          }
        }

        if (dosub) {
          fprintf(foutd, "%%N %s (file: %s)\n", task, infile);
        } else {
          fprintf(foutd, "%%N %s\n", task);
        }

        if (*one)    fprintf(foutd, "%%D %s\n", one);
        if (*author) fprintf(foutd, "%%P %s\n", author);
        if (*cat)    fprintf(foutd, "%%: %s\n", cat);
        fprintf (foutd, "%%B\n");
        if (*t) fprintf(foutd, "%s\n", line);
        mode = MAIN;
      }
      break;

    case '@':
    case '<':
      /* Keyword. */
      if (mode == MAIN) {
        fprintf(foutd, "%%A %s\n", t);
        if (s == '<') {
          fprintf(foutd, "%sStandard keyword %s. See the help on \"%s\" "
                         "for more information.\n", indent, t, t);
        }
      }
      break;

    case '$':
      /* RCS revision information. */
      if (mode == MAIN) {
        t = index(t, ' ');
        if (t) t = index(t+1, ' ');
        u = t;
        if (u) u = index(u+1, ' ');
        if (u) {
          *u = '\0';
          u++;
        }
        v = u;
        if (v) v = index(v+1, ' ');
        if (v) v = index(v+1, ' ');
        if (v) *(v++) = '\0';
        fprintf(foutd, "%%R%s, %s UTC\n", t, u);
      }
      break;

    case '-':
    case 'X':
      /* End. */
      if (foutd != NULL && fout == NULL) fclose(foutd);
      *task = *one = *cat = *author = 0;
      foutd = NULL;
      mode = OUTSIDE;
      if (s == 'X' && doex) mode = EXCLUDE;
      break;

    default:
      /* Text. */
      if (mode == MAIN) {
        getindent(indent, line);
        fprintf(foutd, "%s\n", line);
      }
      break;
    }

    if ((t = fgets(buf, MAXLINE, fin)) == NULL) break;
  }

  if (foutd != NULL && fout == NULL) fclose(fout);
}


/****************************************************************************/
private int parseline(char *buf, char *line)

{
  char *s, *t, *u;
  int  i, indent;
  char tcom[3];

  strcpy(tcom, "*");
  strcat(tcom, "/");

  /* Detab the line. */
  s = buf;
  t = line;
  indent = 1;
  while (*s) {
    if (*s == '\t') {
      do {
        *t++ = ' ';
      } while ((indent++)%8);
      s++;
    } else {
      *t++ = *s++;
      indent++;
    }
  }
  *t = 0;

  /* Strip trailing blanks. */
  t--;
  while (t >= line && isspace(*t)) t--;
  *++t = 0;

  if (t-2 >= line) {
    if (strncmp(t-2, tcom, 2) == 0) {
      t -= 2;
      *t = 0;
    }
  }

  t--;
  while (t >= line && isspace(*t)) t--;
  *++t = 0;

  /* Is it a comment character in the language? */
  s = cs1;
  t = line;
  u = buf;
  while (*s++ && *t) *u++ = *t++;
  *u = 0;
  if (strcmp(buf, cs1) == 0 || strcmp(buf, cs2) == 0) {
    s = cs1;
    t = line;
    while (*s++) *t++ = ' ';
    if (*t && *(t+1) && !isspace(*(t+1)) && !isalpha(*(t+1)) &&
             *(t+1) != '-') {
      i = ' ';
    } else if (*t) {
      i = *t;
    } else {
      i = ' ';
    }

    if (*t && i != ' ') *t = ' ';
  } else {
    i = ' ';
  }

  return i;
}


/****************************************************************************/
private void getindent(char *indent, char *line)

{
  int i;
  i = 0;
  while (isspace(*line)) {
    line++;
    i++;
  }

  if (*line) {
    while (i--) *indent++ = ' ';
    *indent++ = 0;
  }
}
