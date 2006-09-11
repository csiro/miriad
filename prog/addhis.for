      PROGRAM addhis
c
c------------------doc section -----------------------------------------
c= addhis - Add comments to the history of a dataset
c& pjt
c: utility
c+
c   ADDHIS is a MIRIAD task to add information to the history item 
c   of a miriad dataset. 
c@ in
c   Name of the dataset to which information is added
c@ comment
c   Comments to be added, one line only.
c   Commas and tabs are treated like spaces,
c   and multiple spaces are treated like a single space.
c@ file
c   Name of a text file whose contents is added to the history.
c   Default: not used. Use the keyword ``lines'' to select
c   only certain lines.
c@ lines
c   Linenumbers to select from input file. Only used if a file
c   is specified. Default: all. Linenumbers are specified by
c   given the first and last line that are to be added, e.g.
c   lines=20,25 will copy lines 20 through 25. Multiple doublets
c   can be given.
c--
c@ edit
c   Allow editing of the history before it is finalized?
c   Default: false.
c   Note it is officially not allowed to delete information from 
c   the history, but here is your chance!!
c--
c
c  History:
c    pjt  25jul91  Created by suggestion of Lauren
c    pjt  26jul91  got rid of edit= keyword
c-----------------------------------------------------------------------
c parameters:
      INTEGER MAXLIN, MAXCOM
      PARAMETER (MAXLIN=100, MAXCOM=32)
c local variables:
      INTEGER lin, lfile, iostat, i, l, l1, l2, ncomment, nline
      INTEGER idx, nadd
      INTEGER lineno(MAXLIN+1)
      LOGICAL dofile, add
      CHARACTER in*80, file*80, line*132, aline*132, comment(MAXCOM)*20
c externals:
      INTEGER len1
      LOGICAL keyprsnt
c-----------------------------------------------------------------------
c
c Announce:
c
      CALL output( 'ADDHIS: Version 1.0 25-jul-91' )
c
c Get the input parameters.
c
      CALL keyini
      CALL keyf ('in', in, ' ')
      CALL mkeya('comment',comment,MAXCOM,ncomment)         
      IF (keyprsnt('file')) THEN
         dofile = .TRUE.
         CALL keyf('file',file,' ')
         CALL mkeyi('lines',lineno,MAXLIN,nline)
      ELSE
         dofile = .FALSE.
      ENDIF
      CALL keyfin
c
c Some error checking on the input parameters
c
      IF (in.EQ.' ') CALL bug ('f', 'No input dataset (in=) supplied')
      IF (dofile) THEN
         IF (nline.GT.0 .AND. MOD(nline,2).NE.0) THEN
            lineno(nline+1) = lineno(nline)
            nline=nline+1
            CALL bug('i','Copied last from lines= to get even number')
         ENDIF
      ENDIF
c
c  Open the input file and it's history file in append mode
c
      CALL hopen (lin, in, 'old', iostat)
      IF (iostat.NE.0) CALL bug ('f', 
     *      'Error opening dataset '//in)
      CALL hisopen (lin, 'append')
c
c Add history if a comment given from the keyword 'comment='
c
      IF (ncomment.GT.0) THEN
         CALL output('Adding command line history')
         aline = 'ADDHIS: '// comment(1)
         DO i=2,ncomment
            l1=len1(aline)
            l2=len1(comment(i))
            aline(l1+1:) = ' ' // comment(i)(1:l2)
         ENDDO
         CALL hiswrite(lin,aline)
      ENDIF
c
c Add history from a text file
c
      IF (dofile) THEN
         CALL output('Adding file history from: '//file)
         CALL txtopen(lfile, file, 'old', iostat)
         IF (iostat.NE.0) CALL bug ('f', 
     *                              'Error opening file '//file)
         idx=1
         l=0
         nadd = 0
c                               loop reading lines from textfile
         DOWHILE (iostat.EQ.0)
            CALL txtread(lfile,line,l1,iostat)
            IF (iostat.EQ.0) THEN
               l=l+1
               add = .FALSE.
               IF (nline.EQ.0) add=.TRUE.
               IF (l.GE.lineno(idx).AND.l.LE.lineno(idx+1)) THEN
                  add = .TRUE.
               ENDIF
               IF (add) THEN
                  aline = 'ADDHIS: '//line
                  CALL hiswrite(lin,aline)
                  nadd = nadd + 1
               ENDIF
               IF(nline.GT.0) THEN
                  IF (l.GE.lineno(idx+1)) THEN
                     IF (idx+2.LT.nline) THEN
                        idx = idx + 2
                     ELSE
                        iostat=1
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         CALL txtclose(lfile)
         WRITE(line,'(''Added '',I4,'' line(s).'')') nadd
         CALL output(line)
      ENDIF
c
c  Close all files
c
      CALL hisclose (lin)
      CALL hclose (lin)
      END
