      program uvglue
c------------------------------------------------------------------------
c= uvglue - glue channels together
c& nebk
c: calibration
c+
c     UVGLUE glues together individual multi-channel files into
c     one mega-multi-channel file.
c
c     The individual multi-channel files must be IDENTICAL except for 
c     the values and flags of the correlations.  I.e. they must have 
c     the same number of channels and the same preambles.
c
c     For example, if we have 3 files names "vis_1", "vis_2" and 
c     "vis_3", where each file contains N  channels, then the output
c     file will contain 3*N channels glued together in the order
c     vis_1(1:N), vis_2(1:N), vis_3(1:N)
c
c     The output contains only one spectral window with NFILES*N channels
c
c@ vis
c	Root name of input visibility files.   Files must be named
c	vis_i for the ith file.  No default. NOTE: any calibration
c	tables present in the input datasets are NOT applied in
c	forming the output.
c@ nfiles
c	Number of file to read.  
c@ out
c	Output file name. No default
c
c--
c     nebk 04mar97 Original version
c     nebk 10mar97 Handle multiple channels
c---------------------------------------------------------------------------
      implicit none
c
      include 'maxdim.h'
      character in*80, out*80, name*90, line*80
      double precision preamble(5)
      integer nchani, nchano, lin, lins, lout, nread, irec, ichan,
     +  ioff, npol, pol, nfiles, j, k, ifile
      ptrdiff offset
      logical gflags(maxchan), first
      complex data(maxchan)
      real scratch(maxchan*3)

      character version*(*)
      parameter (version='UvGlue: Version 10-Mar-97')
      integer len1
      character itoaf*5
      data first /.true./
c-----------------------------------------------------------------------
      call output(version)
c
c Get inputs
c
      call keyini
c
      call keya ('vis', in, ' ')
      if (in.eq.' ') call bug ('f', 'Input root file name not given')
      call keyi ('nfiles', nfiles, 0)
      if (nfiles.eq.0) call bug ('f', 'Number of files not given')
      call keya ('out', out, ' ')
      if (out.eq.' ') call bug ('f', 'Output file name not given')
      call keyfin
c
c Open output file now, rather than later, incase it already exists. We
c don't want to waste hours writing a scratch file and then have it fail
c
      call uvopen(lout,out,'new')
c
c Open scratch file
c
      call scropen (lins)
c
c Loop over number of input files
c
      irec = 0
      do ifile= 1, nfiles
        name = in(1:len1(in))//'_'//itoaf(ifile)
        call uvopen (lin, name, 'old')
c
c Read first record
c
        call uvread (lin, preamble, data, gflags, maxchan, nread)
        if (nread.eq.0) then
          call bug ('f', 'No data in file '//name)
        else
          if (first) then
            nchani = nread
            nchano = nfiles * nchani
c
            write (line,'(a,i5)') 'Number of input  channels = ', nchani
            call output (' ')
            call output (line)
            write (line,'(a,i5)') 'Number of output channels = ', nchano
            call output (line)
            call output (' ')
            first = .false.
          else
            if (nchani.ne.nread) 
     +         call bug ('f','Number of channels has changed')
          end if
        end if
c
        call output ('Processing input file '//name)
        do while (nread.gt.0)
          irec = irec + 1
c
c Load visibility into scratch buffer
c
          k = 1
          do j = 1, 3*nchani, 3
            scratch(j) = real(data(k))
            scratch(j+1) = aimag(data(k))
            scratch(j+2) = -1
            if (gflags(k)) scratch(j+2) = 1
            k = k + 1
          end do
c
c Write scratch file
c
          offset = (irec-1)
          offset = offset*nchano*3 + (ifile-1)*nchani*3
          call scrwrite (lins, scratch, offset, nchani*3)
c
          call uvread (lin, preamble, data, gflags, maxchan, nread)
        end do

        call uvclose (lin)
        write (line, 100) irec
100     format ('Read ', i8, ' records from this file')
c        call output (line)
        irec = 0
      end do
      call output (' ')
      call output (' ')
c
c OK the scratch file is written.  Now pass through the channel
c 1 file again and use it as a template for the variables
c whilst fishing out the glued data
c
      call output ('Copy scratch file to output')
      name = in(1:len1(in))//'_1'
      call uvopen (lin, name, 'old')
      call uvset(lin,'preamble','uvw/time/baseline',0,0.,0.,0.)
      call VarInit (lin, 'channel')
c
c Set up output file (its already open)
c
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
        offset = (irec-1)
        offset = offset*nchano*3
        call scrread (lins, scratch, offset, nchano*3)
        do ichan = 1, nchano
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

        call uvputvri(lout,'nschan', nchano, 1)
        call uvputvri (lout, 'npol', npol, 1)
        call uvputvri (lout, 'pol', pol, 1)
c
c Write data
c
        call uvwrite(lout,preamble,data,gflags,nchano)
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
