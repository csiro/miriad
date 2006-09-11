c************************************************************************
        program uvrot
        implicit none
c= uvrot -- Rotate uv coordinates.
c& rjs
c: uv analysis
c+
c	uvrot rotates the uv coordinates of a Miriad dataset by a given
c	angle.
c@ vis
c	The name of the input uv datasets. Several can be given
c	Wildcards are supported. No default.
c@ out
c	The name of the output uv data-set. There is no default name.
c@ select
c	The normal uv selection commands. The default is to select everything.
c@ angle
c	The angle to rotate the coordinates by, given in degrees. No default.
c@ options
c	This gives extra processing options. Several options can be given,
c	each separated by commas. They may be abbreviated to the minimum
c	needed to avoid ambiguity. Possible options are:
c	   nocal       Do not apply the gains table.
c	   nopass      Do not apply bandpass corrections.
c	   nopol       Do not apply polarization corrections.
c--
c  History:
c    10dec97 rjs  Created from uvjup, which this generalised and supercedes.
c    21jun99 rjs  Added options=replace.
c    09may06 rjs  Tidy up and rename.
c    18may06 rjs  Re-birth as uvunrot
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
c
        character version*(*)
        parameter(version='uvRot: version 1.0 18-May-06')
c
        character out*64,ltype*16,uvflags*16
        integer lin,lout
        logical first
        double precision angle
c
c  Externals.
c
        logical uvdatopn
c
c  Get the input parameters.
c
        call output(version)
        call keyini
        call getopt(uvflags)
        call uvdatinp('vis',uvflags)
        call keyd('angle',angle,0.0d0)
	call keya('out',out,' ')
	if(out.eq.' ')call bug('f','An output must be given')
        call keyfin
c
c  Open the inputs and the outputs.
c
        first = .true.
        dowhile(uvdatopn(lin))
          call uvdatgta('ltype',ltype)
          call varinit(lin,ltype)
c
          if(first)then
            call uvopen(lout,out,'new')
            call uvset(lout,'preamble','uvw/time/baseline',0,0.,0.,0.)
            call hdcopy(lin,lout,'history')
            call hisopen(lout,'append')
            call hiswrite(lout,'UVROT: Miriad '//version)
            call hisinput(lout,'UVROT')
            call hisclose(lout)
            first = .false.
          endif
          call varonit(lin,lout,ltype)
c
c  Do the work.
c
          call process(lin,lout,angle)
c
c  All said and done. Close up shop.
c
          call uvdatcls
        enddo
c
        if(first)call bug('f','No input datasets found')
        call uvclose(lout)
        end
c************************************************************************
        subroutine getopt(uvflags)
c
        implicit none
        character uvflags*(*)
c
c  Determine extra processing options.
c
c  Output:
c    uvflags
c------------------------------------------------------------------------
        integer nopts
        parameter(nopts=3)
        logical present(nopts)
        character opts(nopts)*8
c
        data opts/'nocal   ','nopol   ','nopass  '/
c
        call options('options',opts,present,nopts)
c
c  Determine the flags to pass to the uvDat routines.
c    d - Data selection.
c    3 - Return w (as well as u and v).
c    c - Apply gain table.
c    e - Apply leakage correction.
c    f - Apply bandpass correction.
c
        uvflags = 'd3'
        if(.not.present(1))uvflags(5:5) = 'c'
        if(.not.present(2))uvflags(6:6) = 'e'
        if(.not.present(3))uvflags(7:7) = 'f'
c
        end
c************************************************************************
        subroutine process(lin,lout,angle)
c
        implicit none
        integer lin,lout
        double precision angle
c
c  Do all the real work.
c
c  Input:
c    lIn
c    lOut
c    ll
c    tol
c------------------------------------------------------------------------
	include 'maxdim.h'
	include 'mirconst.h'
c
        integer nchan,npol,pol
        double precision preamble(5),uu,vv
        real cospa,sinpa
        logical flags(MAXCHAN)
        complex data(MAXCHAN)
c
	sinpa=sin(DPI/180.0*angle)
	cospa=cos(DPI/180.0*angle)
c
c  Read the first record.
c
        call uvdatrd(preamble,data,flags,maxchan,nchan)
        dowhile(nchan.gt.0)
c
c  Copy all the variables from the input to the output.
c
          call uvdatgti('npol',npol)
          if(npol.eq.0)
     *      call bug('f','Could not determine number of polarisations')
          call uvdatgti('pol',pol)
          call uvputvri(lout,'npol',npol,1)
          call uvputvri(lout,'pol',pol,1)
          call varcopy(lin,lout)
c
          uu = preamble(1)
          vv = preamble(2)
	  preamble(1) =  uu*cospa + vv*sinpa
	  preamble(2) = -uu*sinpa + vv*cospa
c
c  All done. Loop the loop.
c
          call uvwrite(lout,preamble,data,flags,nchan)
          call uvdatrd(preamble,data,flags,maxchan,nchan)
        enddo
c
        end
