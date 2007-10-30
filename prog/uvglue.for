      program uvglue
c------------------------------------------------------------------------
c= uvglue - glue channels together
c& nebk
c: calibration
c+
c     UVGLUE glues together individual channels residing
c     in separate files into one multi-channel file
c
c     It is ASSUMED that all of the individual channel file are IDENTICAL
c     except for the values and flags of the correlations
c
c     The output contains only one spectral window with NCHAN channels
c
c@ vis
c	Root name of input visibility files. Files must be named
c	vis_i for ith channel.  No default.
c@ nchan
c	Number of channels. 
c@ out
c	Output file name. No default
c
c--
c     nebk 04mar97 Original version
c---------------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      character in*80, out*80, name*90, line*80
      double precision preamble(5)
      integer nchan, lin, lins, lout, nread, irec, ichan,
     +  offset, ioff, npol, pol
      logical gflags(maxchan)
      complex data(maxchan)
      real scratch(maxchan*3)

      character version*(*)
      parameter (version='UvGlue: Version 04-Mar-97')
      integer len1
      character itoaf*3
c-----------------------------------------------------------------------
      call output(version)
c
c Get inputs
c
      call keyini
c
      call keya ('vis', in, ' ')
      if (in.eq.' ') call bug ('f', 'Input root file name not given')
      call keyi ('nchan', nchan, 0)
      if (nchan.eq.0) call bug ('f', 'Number of channels not given')
      call keya ('out', out, ' ')
      if (out.eq.' ') call bug ('f', 'Output file name not given')
      call keyfin
c
c Open scratch file
c
      call scropen (lins)
c
c Loop over number of input files
c
      irec = 0
      do ichan = 1, nchan
        name = in(1:len1(in))//'_'//itoaf(ichan)
        call uvopen (lin, name, 'old')
        call output ('Opened input file '//name)
c
        call uvread (lin, preamble, data, gflags, maxchan, nread)
        do while (nread.gt.0) 
          if (nread.ne.1) call bug ('f', 
     +      'Input files must only have one channel')
          irec = irec + 1
          scratch(1) = real(data(1))
          scratch(2) = aimag(data(1))
          scratch(3) = -1
          if (gflags(1)) scratch(3) = 1
          offset = (irec-1)*nchan*3 + 3*(ichan-1)
          call scrwrite (lins, scratch, offset, 3)
c
          call uvread (lin, preamble, data, gflags, maxchan, nread)
        end do

        call uvclose (lin)
        write (line, 100) irec
100     format ('Read ', i8, ' records from this file')
        call output (line)
        call output (' ')
        irec = 0
      end do
      call output (' ')
      call output (' ')
c
c OK the scratch file is written.  Now pass through the channel
c 1 file again and use it as a template for the variables
c
      call output ('Copy scratch file to output')
      name = in(1:len1(in))//'_1'
      call uvopen (lin, name, 'old')
      call uvset(lin,'preamble','uvw/time/baseline',0,0.,0.,0.)
      call VarInit (lin, 'channel')
c
      call uvopen(lout,out,'new')
      call uvset(lout,'preamble','uvw/time/baseline',0,0.,0.,0.)
      call hdcopy(lin,lout,'history')
      call hisopen(lout,'append')
      call hiswrite(lout,'UVGLUE: Miriad '//version)
      call hisinput(lout,'UVGLUE:')
      call hisclose(lout)
      call VarOnit(lin,lout,'channel')
c
c Copy variables from channel 1 file, and copy data from
c scratch file
c 
      irec = 0
      call uvread (lin, preamble, data, gflags, maxchan, nread)
c
      do while (nread.gt.0) 
c
c Fish out spectrum from scratch file
c
        irec = irec + 1
        offset = (irec-1)*nchan*3
        call scrread (lins, scratch, offset, nchan*3)
        do ichan = 1, nchan
          ioff = (ichan-1)*3
          data(ichan) = cmplx(scratch(ioff+1),scratch(ioff+2))
          gflags(ichan) = .true.
          if (scratch(ioff+3).lt.0) gflags(ichan) = .false.
        end do
c
c Copy variables
c
        call VarCopy(lin, lout)
        call uvgetvri (lin, 'npol', npol, 1)
        call uvgetvri (lin, 'pol', pol, 1)

        call uvputvri(lout,'nschan', nchan, 1)
        call uvputvri (lout, 'npol', npol, 1)
        call uvputvri (lout, 'pol', pol, 1)
c
c Write data
c
        call uvwrite(lout,preamble,data,gflags,nchan)
c
c Read next record
c
        call uvread (lin, preamble, data, gflags, maxchan, nread)
      end do
c
c Bye bye
c
      call uvclose (lin)
      call uvclose (lout)
      call scrclose (lins)
c
      end
