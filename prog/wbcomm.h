c Common block for wblod

      integer MAXCO, ATCAMAX
      parameter (maxco=2000,ATCAMAX=6)
      integer mant(ATCAMAX), fgbadpos, fgbadtmp, fgbadch
      real tut(ATCAMAX,MAXCO),tra(ATCAMAX,MAXCO)
      real tdec(ATCAMAX,MAXCO), tsysif1(ATCAMAX), tsysif2(ATCAMAX)
      real xit

      common/wbcomm/tut,tra,tdec,mant,xit,tsysif1,tsysif2
      common/wbpos/ fgbadpos, fgbadtmp, fgbadch
