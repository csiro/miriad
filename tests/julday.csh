#!/bin/csh -ef

cat <<"EOF" >jul_test.for
c************************************************************************
      program test
      character string*50
      character fmt(5)*1
      integer j
      double precision jul, jul2
c
      data fmt / 'D', 'F', 'H', 'T', 'V'/
c
      call todayjul(jul)
      write (*,*) 'TodayJul => ', jul
      write (*,*) ' '
c
      do j = 1, 5
	write (*,*) 'Format => ', fmt(j)
	call julday(jul, fmt(j), string)
	write (*,*) 'JulDay => ', string
	call dayjul(string, jul2)
	write (*,*) 'DayJul => ', jul2
	write (*,*) ' '
      enddo
c
      string = '18/1/95'
      write (*,*) 'string => ', string
      call dayjul(string, jul2)
      write (*,*) 'DayJul => ', jul2
      write (*,*) ' '
c
      string = '98feb01'
      write (*,*) 'string => ', string
      call dayjul(string, jul2)
      write (*,*) 'DayJul => ', jul2
      write (*,*) ' '
c
c      write (*,*) '== Now test for mistakes (last one should bomb) =='
c      write (*,*) ' '
c      write (*,*) 'Format => X [Should not exist]'
c      call julday(jul, 'X', string)
c      write (*,*) 'JulDay => ', string
c      write (*,*) ' '
c      string = '97ocr11.5'
c      write (*,*) 'Incorrect Date String => ', string
c      call dayjul(string, jul2)
      end
"EOF"

fortran -o jul_test jul_test.for `mirlibs`
jul_test
