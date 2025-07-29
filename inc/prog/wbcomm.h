c Common block for wblod

      integer MAXCO, ATCAMAX, MAXCBUF
      parameter (maxco=2000,ATCAMAX=6,maxcbuf=4000000)
      integer mant(ATCAMAX), fgbadpos, fgbadtmp, fgbadch
      integer sant(ATCAMAX),sbuf(ATCAMAX)
      double precision sut(ATCAMAX,MAXCBUF)
      real tut(ATCAMAX,MAXCO),tra(ATCAMAX,MAXCO)
      real sra(ATCAMAX,MAXCBUF),sdec(ATCAMAX,MAXCBUF)
      real tdec(ATCAMAX,MAXCO), tsysif1(ATCAMAX), tsysif2(ATCAMAX)
      real tsysk1(ATCAMAX),tsysk2(ATCAMAX)
      real xit
      character*132 posin

      common/wbcomm/tut,tra,tdec,mant,xit,tsysif1,tsysif2,tsysk1,
     *              tsysk2
      common/wbpos/ fgbadpos, fgbadtmp, fgbadch
      common/wbcbuf/sant,sbuf,sut,sra,sdec
      common/wbcos/ posin
