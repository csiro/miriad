c************************************************************************
      program uvclip
      implicit none
c
c= UVCLIP - determine data to flag from channel-0 uv dataset
c& njt
c: uv analysis
c+
c	UVCLIP flags UV channel 0 data set by statistical clipping 
c@ vis
c	The input UV dataset name. No default.
c
c@ select
c	This selects the data to be processed, using the standard uvselect
c	format. Default is all data.
c
c@ options
c       noamp .... no visbility amplitude clipping
c       notrip ... no triple visibility phase closure clipping
c       doall .... include all data (flagged and unflagged)
c       noapply .. do not apply flagging
c
c@ interval
c       Data clipping interval in minutes. The maximum interval between 
c       data points for continuous clipping.  Must be greater than the
c       integration time.  The default is infinite.       
c
c@ clip
c       Clipping level.  Number of rms beyond which data is flagged.
c       Default is 3.
c
c@ log
c	The list output file name. The default is the terminal.
c--
c
c  History:
c   09sep94 njt  - Created
c------------------------------------------------------------------------

      include 'maxdim.h'

      character version*(*)
      parameter (version='Uvclip: version 1.0 09-Sep-94')
      integer   NOPTS, MAXVIS, MAXCYC, MAXDAT, MAXSELS
      parameter (NOPTS=4, MAXVIS=128, MAXCYC=768, MAXSELS=512,
     -           MAXDAT=MAXCYC*MAXVIS)

      character log*64,
     -          line*80

      complex   data(MAXCHAN)

      logical   flags(MAXCHAN),
     -          doapply,
     -          doall,
     -          doamp,
     -          dotrip

      double precision preamble(4),         
     -          oldjd,                      
     -          delmin,                     
     -          minday,                   
     -          interval,                 
     -          tx(MAXCYC),
     -          ft(MAXDAT)

      complex   vis(MAXVIS,MAXCYC),        
     -          tv(MAXVIS,MAXCYC)           

      integer   tno,                        
     -          nchan,                      
     -          nrec,                      
     -          ip,                        
     -          na, nc,                
     -          i1, i2,                     
     -          a1(MAXBASE), a2(MAXBASE),  
     -          mb, nb, ib,                  
     -          i, j, k, n,
     -          vx(MAXCYC),               
     -          px(MAXVIS,MAXCYC),         
     -          bx(MAXVIS,MAXCYC),         
     -          tn(MAXCYC),                
     -          tb(3,MAXVIS,MAXCYC),       
     -          tp(MAXVIS,MAXCYC),        
     -          flg,
     -          fp(MAXDAT)

      integer   fb(MAXDAT)

      character opts(NOPTS)*9,
     -          calday*18,
     -          file*64

      logical   present(NOPTS),
     -          baseline(MAXBASE),
     -          gotone

      real      m(6,-8:4),
     -          clip,
     -          sels(MAXSELS)

      character type(-8:4)*2
 
c     external declarations

      integer   len1
 
c     initialise some variables

      data opts / 'noapply  ','doall    ','notrip    ','noamp    ' /

      data minday / 1440.0d0 / 

      data type / 'YX', 'XY', 'YY', 'XX', 'LR', 'RL', 'LL', 'RR',
     -            '  ', 'I ', 'Q ', 'U ', 'V ' /  

      call output(version)          

c     +++ read inputs

      call keyini                   
      call keya('vis',file,' ')
      call selinput('select',sels,maxsels) 
      call options('options',opts,present,nopts)
      call keyd('interval',interval,0.0d0)
      call keyr('clip',clip,3.0)
      call keya('log',log,' ')      
      call keyfin                   

c     setup option logicals

      doapply = .not.present(1)
      doall = present(2)
      dotrip = .not.present(3)
      doamp = .not.present(4)

c     open the data file and apply selection criteria

      call uvopen(tno,file,'old')
     
      call uvset(tno,'coord','wavelength',0,0.0,0.0,0.0)
      call uvset(tno,'planet',' ',0,0.0,0.0,0.0) 

      call selapply(tno,sels,.true.)

c     open logfile

      call logopen(log,' ')
      call logwrit(version)

c     initialise variables

      do i = 1, mb
        baseline(i) = .false.
        a1(i) = 0
        a2(i) = 0
      end do 
      do i = 1, MAXCYC
        vx(i) = 0.0
      end do

      nrec = 0
      nc = 1
      flg = 0

c     +++ begin processing uv data
 
      call uvread(tno,preamble,data,flags,maxchan,nchan)

      call uvrdvri(tno,'nants',na,0)  
      mb = na * (na - 1) / 2

      oldjd = preamble(3)
      call julday(oldjd,'H',calday)
      line = '! new interval @ '//calday
      call logwrit(line(1:len1(line)))

      do while (nchan.gt.0)

        if(flags(nchan).or.doall) then

          nrec = nrec + 1            
           
c     process previous integration cycle

          delmin = (preamble(3) - oldjd) * minday 
          oldjd = preamble(3)
  
          if(delmin.ne.0.0d0) then
         
            nb = 0
            do i = 1, mb
              if(baseline(i)) then
                nb = nb + 1
              end if 
            end do
            call triple(nc,nb,a1,a2,vx,bx,px,vis,tn,tb,tp,tv)
            do i = 1, mb
              baseline(i) = .false.
              a1(i) = 0
              a2(i) = 0
            end do     

            nc = nc + 1   

          end if

          if(delmin.gt.interval .and. interval.gt.0.0d0) then

            nc = nc - 1

            write(line,'(a,i3.3)') '! # accumulated cycles = ', nc
            call logwrit(line(1:len1(line)))
                      
            call uvstat(nc,vx,px,vis,tn,tp,tv,m) 
 
            if(doamp) call clipamp(clip,nc,m,vx,tx,px,bx,vis,
     -       flg,ft,fp,fb)

            if(dotrip) call cliptrp(clip,mb,nc,m,tx,tn,tb,tp,tv,
     -       flg,ft,fp,fb)
  
c     initialise interval accumulation variables   

            nc = 1                      
            do i = 1, MAXCYC
              vx(i) = 0.0
            end do

            call julday(oldjd,'H',calday)
            write(line,'(a,a18)') '! new interval @ ', calday
            call logwrit(line(1:len1(line)))
 
          end if

c     record baseline and antenna / cycle - all polarizations

          i2 = nint(preamble(4))
          i1 = i2 / 256
          i2 = i2 - 256 * i1
          ib = (i2 - 1) * (i2 - 2) / 2 + i1
          if(.not.baseline(ib)) baseline(ib) = .true.
          a1(ib) = i1
          a2(ib) = i2 

c     setup time ordered visibility indices and data

          call uvrdvri(tno,'pol',ip,0)
          if(ip.eq.0) call bug('f','% bad polarization index read')

          tx(nc) = oldjd                 
          vx(nc) = vx(nc) + 1            
          px(vx(nc),nc) = ip              
          bx(vx(nc),nc) = ib            
          vis(vx(nc),nc) = data(nchan)  
 
        end if
 
c     read next record
        
        call uvread(tno,preamble,data,flags,MAXCHAN,nchan)

      end do

c     process any remaining data

      if(nc.gt.0) then
    
        write(line,'(a,i3.3)') '! # accumulated cycles = ', nc
        call logwrit(line(1:len1(line)))
         
        nb = 0
        do i = 1, mb
          if(baseline(i)) then
            nb = nb + 1
          end if 
        end do
   
        call triple(nc,nb,a1,a2,vx,bx,px,vis,tn,tb,tp,tv)
            
        call uvstat(nc,vx,px,vis,tn,tp,tv,m) 
     
        if(doamp) call clipamp(clip,nc,m,vx,tx,px,bx,vis,
     -   flg,ft,fp,fb)

        if(dotrip) call cliptrp(clip,mb,nc,m,tx,tn,tb,tp,tv,
     -   flg,ft,fp,fb)
   
      end if

      write(line,'(a,i5.5)') '! # records processed = ', nrec
      call logwrit(line(1:len1(line)))

c     +++ begin flagging the data

      if(flg.gt.0) then

c     remove duplicate flags from amp and triple flagging

        do i = 1, flg - 1
          j = i + 1
          do while(j.le.flg)
            gotone = ft(i).eq.ft(j) .and. fp(i).eq.fp(j) .and.
     -       fb(i).eq.fb(j)
            if(gotone) then
              do k = j, flg - 1
                ft(k) = ft(k+1)
                fp(k) = fp(k+1)
                fb(k) = fb(k+1)
              end do
              flg = flg - 1
            else
              j = j + 1
            end if
          end do
        end do

c     rewind to the beginning 

        call uvrewind(tno)

        nrec = 0
        n = 0
      
        call uvread(tno,preamble,data,flags,MAXCHAN,nchan)
        
        do while (nchan.gt.0)
             
          if(flags(nchan).or.doall) then

            nrec = nrec + 1 

            call uvrdvri(tno,'pol',ip,0)
            if(ip.eq.0) call bug('f','% bad polarization index read')

            i2 = nint(preamble(4))
            i1 = i2 / 256
            i2 = i2 - 256 * i1
            ib = (i2 - 1) * (i2 - 2) / 2 + i1

            gotone = .false.
            i = 0
            do while(.not.gotone .and. i.le.flg)
              i = i + 1 
              gotone = ft(i).eq.preamble(3) .and. fp(i).eq.ip .and.
     -         fb(i).eq.ib
            end do
            if(gotone) then
              n = n + 1
              if(doapply) then 
                flags(nchan) = .false.
                call uvflgwr(tno,flags)                        
              else 
                call julday(preamble(3),'H',calday)
                write(line,'(a,a18,x,a2,2(x,i2.2))') '> FLAGGED ',
     -           calday, type(ip), i1, i2 
                call logwrit(line(1:len1(line)))
              end if 
            end if 

          end if

          call uvread(tno,preamble,data,flags,MAXCHAN,nchan)

        end do
  
        write(line,'(a,i5.5)') '! # flagged = ', n
        call logwrit(line(1:len1(line)))
   
      end if

c     close up the log
 
      call uvclose(tno)
      call logclose

      stop

      end

c     ----------------------------------------------------------------72
c     +++ compute uv data statistics over the previous interval, returns
c     (1) the median of visibility amplitude,
c     (2) the mean and rms of the absolute visibility 

      subroutine uvstat(nc,vx,px,vis,tn,tp,tv,m)
      implicit none   

      integer   MAXVIS, MAXCYC, MAXDAT
      parameter (MAXVIS=128, MAXCYC=768, MAXDAT=MAXVIS*MAXCYC)

      integer   i, k, nc,
     -          ip, ic, iv, it,
     -          tn(MAXCYC),
     -          n1(-8:4),
     -          n2(-8:4),
     -          px(MAXVIS,MAXCYC),
     -          tp(MAXVIS,MAXCYC),
     -          vx(MAXCYC)

      real      m(6,-8:4),
     -          tmp,
     -          sumamp(-8:4),
     -          sumphs(-8:4),
     -          sumsqua(-8:4),
     -          sumsqup(-8:4),
     -          amp(MAXDAT,-8:4),
     -          phs(MAXDAT,-8:4)

      complex   data,
     -          vis(MAXVIS,MAXCYC),
     -          tv(MAXVIS,MAXCYC)

      logical   even

      character type(-8:4)*2,
     -          line*80

      real      select

      integer   len1

      data type / 'YX', 'XY', 'YY', 'XX', 'LR', 'RL', 'LL', 'RR',
     -            '  ', 'I ', 'Q ', 'U ', 'V ' /  

c     initialise arrays

      do ip = -8, 4 

        sumamp(ip) = 0.0 
        sumsqua(ip) = 0.0 
        sumphs(ip) = 0.0
        sumsqup(ip) = 0.0
        n1(ip) = 0
        n2(ip) = 0

      end do

c     loop through integration cycles summing amplitudes &c.

      do ic = 1, nc

        do iv = 1, vx(ic)  

          ip = px(iv,ic)
          data = vis(iv,ic)
 
          n1(ip) = n1(ip) + 1

          call amphase(data,amp(n1(ip),ip),tmp)

          sumamp(ip) = sumamp(ip) + amp(n1(ip),ip)
          sumsqua(ip) = sumsqua(ip) + amp(n1(ip),ip) ** 2

        end do
 
        do it = 1, tn(ic)
 
          ip = tp(it,ic)
          data = tv(it,ic)

          n2(ip) = n2(ip) + 1

          call amphase(data,tmp,phs(n2(ip),ip))
          
          sumphs(ip) = sumphs(ip) + phs(n2(ip),ip)
          sumsqup(ip) = sumsqup(ip) + phs(n2(ip),ip) ** 2

        end do
 
      end do

      do ip = -8, 4

c     compute amplitude statistics / polarization

        if(n1(ip).gt.0) then

          k = n1(ip) / 2.0
          tmp = select(k,n1(ip),amp(1,ip))
          even = (k - int(k)).eq.0
          if(even) then
            k = k + 1
            tmp = tmp + select(k,n1(ip),amp(1,ip))
            tmp = tmp / 2.0
          end if
          m(1,ip) = tmp
 
          m(2,ip) = sumamp(ip) / n1(ip)
          m(3,ip) = sqrt(sumsqua(ip) / n1(ip) - m(2,ip) ** 2)
 
          write(line,'(a,x,a2,x,a,3(x,e9.3),x,i5.5)') '>',
     -     type(ip), 'amp ( Jy)', (m(i,ip),i=1,3), n1(ip)
          call logwrit(line(1:len1(line)))
    
        end if

c     compute visibility triple phase statistics / polarization

        if(n2(ip).gt.0) then

          k = n2(ip) / 2.0
          tmp = select(k,n2(ip),phs(1,ip))
          even = (k - int(k)).eq.0
          if(even) then
            k = k + 1
            tmp = tmp + select(k,n2(ip),phs(1,ip))
            tmp = tmp / 2.0
          end if
          m(4,ip) = tmp

          m(5,ip) = sumphs(ip) / n2(ip)
          m(6,ip) = sqrt(sumsqup(ip) / n2(ip) - m(5,ip) ** 2)

          write(line,'(a,x,a2,x,a,3(x,e9.3),x,i5.5)') '>',
     -     type(ip), 'phs (deg)', (m(i,ip),i=4,6), n2(ip)
          call logwrit(line(1:len1(line)))

        end if

      end do

      return

      end

c     ----------------------------------------------------------------72
c     +++ compute closure relation of visibility triples for previous
c     integration cycle

      subroutine triple(nc,nb,a1,a2,vx,bx,px,vis,tn,tb,tp,tv)
      implicit none

      include 'maxdim.h'

      integer   MAXTRP, MAXVIS, MAXCYC
      parameter (MAXTRP=MAXANT*(MAXANT-1)*(MAXANT-2)/6, MAXVIS=128,
     -          MAXCYC=768)

      integer   a1(MAXBASE), a2(MAXBASE),
     -          i, j, k,
     -          ip,
     -          nb,
     -          nc

      logical   same,
     -          zero,
     -          comb
  
      complex   vis(MAXVIS,MAXCYC),
     -          tmp,
     -          data(MAXBASE,-8:4),
     -          tv(MAXVIS,MAXCYC)

      integer   dn(-8:4),
     -          tn(MAXCYC),
     -          tb(3,MAXVIS,MAXCYC),
     -          tp(MAXVIS,MAXCYC),
     -          bx(MAXVIS,MAXCYC),
     -          px(MAXVIS,MAXCYC),
     -          vx(MAXCYC),
     -          ib

c     initialise arrays

      tn(nc) = 0

      do i = -8, 4
        dn(i) = 0
      end do

      do i = 1, vx(nc)
        ip = px(i,nc)
        ib = bx(i,nc)
        dn(ip) = dn(ip) + 1
        data(ib,ip) = vis(i,nc)
      end do

c     form triple product combinations and compute triple visibility

      do ip = -8, 4
    
        if(dn(ip).gt.0) then 

          do i = 1, nb - 2
            do j = i + 1, nb - 1
              do k = i + 2, nb

                same = i.eq.j .and. j.eq.k
                zero = a1(i).eq.0 .and. a2(i).ne.0 .and. a2(j).ne.0
                comb = a1(i).eq.a1(j) .and. a2(i).eq.a1(k) .and.
     -           a2(j).eq.a2(k)

                if(.not.same .and. .not.zero .and. comb) then

                  tn(nc) = tn(nc) + 1

                  tmp = data(i,ip) * conjg(data(j,ip)) * data(k,ip)
               
                  tb(1,tn(nc),nc) = i
                  tb(2,tn(nc),nc) = j
                  tb(3,tn(nc),nc) = k
 
                  tp(tn(nc),nc) = ip
                  tv(tn(nc),nc) = tmp

                end if
            
              end do
            end do                                                
          end do
    
        end if 

      end do

      return
      end

c     ----------------------------------------------------------------72
c     +++ returns the k'th smallest value in the input array, used for
c     determining the median with k = (n+1)/2, however, if n is even
c     then the median is the mean of the elements k = n/2 and k = n/2+1

      FUNCTION select(k,n,arr)
      INTEGER k,n
      real select,arr(n)
      INTEGER i,ir,j,l,mid
      real a,temp
      l=1
      ir=n
1     if(ir-l.le.1)then
        if(ir-l.eq.1)then
          if(arr(ir).lt.arr(l))then
            temp=arr(l)
            arr(l)=arr(ir)
            arr(ir)=temp
          endif
        endif
        select=arr(k)
        return
      else
        mid=(l+ir)/2
        temp=arr(mid)
        arr(mid)=arr(l+1)
        arr(l+1)=temp
        if(arr(l+1).gt.arr(ir))then
          temp=arr(l+1)
          arr(l+1)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l).gt.arr(ir))then
          temp=arr(l)
          arr(l)=arr(ir)
          arr(ir)=temp
        endif
        if(arr(l+1).gt.arr(l))then
          temp=arr(l+1)
          arr(l+1)=arr(l)
          arr(l)=temp
        endif
        i=l+1
        j=ir
        a=arr(l)
3       continue
          i=i+1
        if(arr(i).lt.a)goto 3
4       continue
          j=j-1
        if(arr(j).gt.a)goto 4
        if(j.lt.i)goto 5
        temp=arr(i)
        arr(i)=arr(j)
        arr(j)=temp
        goto 3
5       arr(l)=arr(j)
        arr(j)=a
        if(j.ge.k)ir=j-1
        if(j.le.k)l=i
      endif
      goto 1
      END
C  (C) Copr. 1986-92 Numerical Recipes Software #p21E6W)1.1&iE10(9p#.

c     ----------------------------------------------------------------72
c     +++ determine visibilities to clip based on amplitude statistics

      subroutine clipamp(clip,nc,m,vx,tx,px,bx,vis,flg,ft,fp,fb)
      implicit none

      integer   MAXCYC, MAXVIS, MAXDAT
      parameter (MAXCYC=768, MAXVIS=128, MAXDAT=MAXCYC*MAXVIS)

      integer   nc,
     -          vx(MAXCYC),
     -          px(MAXVIS,MAXCYC),
     -          bx(MAXVIS,MAXCYC)
       
      real      m(6,-8:4)

      double precision tx(MAXCYC),
     -          ft(MAXDAT)

      complex   vis(MAXVIS,MAXCYC)

      integer   ip, ib,
     -          i, j,
     -          flg,
     -          fp(MAXDAT),
     -          fb(MAXDAT)

      real      lower,
     -          upper,
     -          clip,
     -          amp,
     -          tmp

      character calday*18

      do i = 1, nc

        call julday(tx(i),'H',calday)

        do j = 1, vx(i)

          ip = px(j,i)
          ib = bx(j,i) 

          call amphase(vis(j,i),amp,tmp)

          lower = m(1,ip) - clip * m(3,ip)
          upper = m(1,ip) + clip * m(3,ip)

          if(amp.lt.lower .or. upper.lt.amp) then
           
            flg = flg + 1
            ft(flg) = tx(i)
            fp(flg) = ip
            fb(flg) = ib
 
          end if

        end do
 
      end do

      return
      end 
   
c     ----------------------------------------------------------------72
c     +++ determine visibilities to clip based on closure phase stats

      subroutine cliptrp(clip,mb,nc,m,tx,tn,tb,tp,tv,flg,ft,fp,fb)
      implicit none

      include 'maxdim.h'

      integer   MAXCYC, MAXVIS, MAXDAT
      parameter (MAXCYC=768, MAXVIS=128, MAXDAT=MAXCYC*MAXVIS)

      integer   mb,
     -          nc,
     -          tn(MAXCYC),
     -          tb(3,MAXVIS,MAXCYC),
     -          tp(MAXVIS,MAXCYC)

      real      m(6,-8:4)

      double precision tx(MAXCYC),
     -          ft(MAXDAT)

      complex   tv(MAXVIS,MAXCYC)

      integer   i, j, k,
     -          ip,
     -          ib,
     -          nok(-8:4),
     -          tok(MAXVIS,-8:4),
     -          flg,
     -          fp(MAXDAT),
     -          fb(MAXDAT)

      real      clip,
     -          tmp,
     -          phs,
     -          upper,
     -          lower

      logical   ok,
     -          flag(MAXBASE,-8:4),
     -          doip(-8:4)

      character calday*18

c     loop through all collected cycles
      
      do i = 1, nc

        call julday(tx(i),'H',calday)

        do ip = -8, 4
          nok(ip) = 0
          doip(ip) = .false.
        end do

c     separate good triples / polarization from the rest

        do j = 1, tn(i)

          ip = tp(j,i)
          doip(ip) = .true.
          
          call amphase(tv(j,i),tmp,phs)

          lower = m(4,ip) - clip * m(6,ip)
          upper = m(4,ip) + clip * m(6,ip)

          ok = .not.(phs.lt.lower .or. upper.lt.phs)

          if(ok) then
            nok(ip) = nok(ip) + 1
            tok(nok(ip),ip) = j
          end if

        end do

c     keep baselines found in the good triples / polarization, flag the rest

        do ip = -8, 4

          if(doip(ip)) then  
          
            do ib = 1, mb
              flag(ib,ip) = .true.
            end do 

            do j = 1, nok(ip)
              do k = 1, 3
                ib = tb(k,tok(j,ip),nc)
                flag(ib,ip) = .false. 
              end do 
            end do

            do ib = 1, mb
              if(flag(ib,ip)) then
                flg = flg + 1
                ft(flg) = tx(i)
                fp(flg) = ip
                fb(flg) = ib
              end if
            end do

          end if
                                                        
        end do
        
      end do

      return
      end

