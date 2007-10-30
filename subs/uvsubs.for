c----------------------------------------------------------------------c
c  File:  uvsubs.for
c
c    wh     nov88 Original version.
c    rjs  17may89 Declared all variables. Replaced some specific functions
c		  with the equivalent generic functions.
c    wh     jun 2 Fixed double precision bug in bselect. Deleted crakline.
c    mchw 23Feb90 added default width for wideband correlator data only
c    mchw 06Jul90 corrected bug in oneamp and changed to degrees.
c		  - worked on documentation.
c********1*********2*********3*********4*********5*********6*********7*c
c* Width
c: uv-data,visibility
c+
	subroutine width(nspect,sdf,nschan,wide)
c
	implicit none
	integer nspect,nschan(nspect)
	double precision sdf(nspect)
	real wide
c
c  Calculates the width of the wideband synthesized channel from
c  the correlator setup parameters.  This is used by UVHAT.
c
c  Input:
c    nspect	number of correlator spectra
c    sdf	width of correlator spectra
c    nschan	number of channels in each spectra
c  Output:
c    wide	width of synthesized wideband (ghz)
c--
c	nov 88 wh
c  06Jul90  mchw  changed dimension of nschan,sdf to nspect.
c------------------------------------------------------------------------c
	integer i
c
c  sum up the num of chans * channel width for all windows.
c
	wide = 0.
	do i=1,nspect
	  wide=wide + abs(sdf(i))*(nschan(i)-2)
	end do
	wide=wide/2.
	if (wide.eq.0.) wide = 0.16
	end
c********1*********2*********3*********4*********5*********6*********7*c
c* Token
c: i/o, user-interaction.
c+
	character*(*) function token(in,inext,iend,leng)
c
	implicit none
	character*(*) in
	integer inext,iend,leng
c
c  Obtain the next token from a string variable where the delimiters
c	are _ ()[].
c
c  Input:
c    in		string being decoded
c    inext	pointer into string in
c    iend	length of in.
c  Output:
c    leng	length of token
c--
c----------------------------------------------------------------------c
	integer i,j
	character*7 delim
	data delim /'_ ()[]-,'/
c
c  space over blanks.
c
	i=inext
	do while ((in(i:i).eq.' ' .or. in(i:i).eq.'_') .and. i.lt.iend)
	  i=i+1
	end do
c
c  now find the next delimiter. i points to first char in token.
c
	j=i
	do while (index(delim,in(j:j)).eq.0)
	  j=j+1
	end do
	  j=max(j-1,i)
c
c  j points to last char in token.
c  make up the output token and space along the pointers.
c
	token=in(i:j)
	inext=j+1
	leng=j-i+1
	end
c********1*********2*********3*********4*********5*********6*********7*c
c* Bselect
c: uv-data,uv-selection,antennas,baselines
c+
	logical function bselect(base,ants)
	implicit none
	double precision base
	character*(*) ants
c					!256*ant1 + ant2
c					!ants in config (string)
c
c  Compares the variable 'baseline' attached to each baseline with
c  the string 'ants' which describes the baseline configuration
c  that is being selected.  BSELECT returns true if base(from uvread)
c  is one of ants. The baseline configuration is described in the
c  VLA format:
c     antennas  WITH  antennas  (if WITH is missing, both sides are equal)
c      where antennas is a list of antenna numbers  ant1 ant2 ...  antn
c      or    antennas can be  *   which means all antennas
c      or    antennas can be  * - antn antm  which means all ants - some
c  Inputs:
c    base  	baseline selector from uvread.
c    ants 	antenna description string.
c--
c		-oct 88 wh
c----------------------------------------------------------------------c
	character*80 antsold
	logical expanded(30,2)
	integer i,il,iex,next,iant,ibase,i1,i2,l
	character*10 token
	character*30 tok
	logical submode
	data antsold /' '/
c
c  expand/decode ants if necessary
c
	if(ants.ne.antsold) then
	  antsold=ants
	  call lcase(ants)
	  do i=1,30
	   expanded(i,1) = .false.
	   expanded(i,2) = .false.
	  end do
	  il=len(ants)
	  iex=1
	  next=1
	  submode=.true.
c
c  decode line and build expanded(1) and (2)
c
	  do while (next.lt.il)
	    tok = token(antsold,next,il,l)
	    if (tok(1:1).eq.'w') then
c
c  with means switch to right
		iex=2
		submode=.false.
	    else if (tok(1:1).eq.'*') then
c
c  * means all antennas
		do i=1,30
		 expanded(i,iex) = .true.
		end do
	    else if (tok(1:1).eq.'-') then
c
c  - means remove next antennas
		submode=.false.
	    else
		read(tok(1:l),'(i2)') iant
c
c  add/sub one antenna
c
		if (iant.gt.0 .and. iant.le.30) then
		  expanded(iant,iex) = submode
		end if
	    end if
	  end do
c
c  if no 'with' then both sets are the same
c
	    if (iex.eq.1) then
		do i=1,30
		  expanded(i,2) = expanded(i,1)
		end do
	    end if
	end if
	ibase = int(base)
	i1=ibase/256
	i2=ibase-(ibase/256)*256
	bselect=.false.
	if (i1.lt.1 .or. i1.gt.30) return
	if (i2.lt.1 .or. i2.gt.30) return
	if (expanded(i1,1) .and. expanded(i2,2)  .or.
     .		expanded(i1,2) .and. expanded(i2,1)) then
	    bselect=.true.
	  else
	    bselect=.false.
	  end if
	end
c********1*********2*********3*********4*********5*********6*********7*c
c* Oneamp
c: complex-data, uvdata
c+
	real function oneamp(vis,flag)
c
	implicit none
	complex vis
	character*(*) flag
c
c  Converts a complex vis to 'Amp' 'Phase' 'Real' 'Imag' part.
c  
c  Input:
c    vis 	complex,input)   visibility
c    flag	One of : 'a' 'p' 'r' 'i'
c  Output:
c   		The amplitude, phase(degrees), real, or imaginary part.
c--
c----------------------------------------------------------------------c
	real pi,amp
	parameter (pi = 3.141592653589793)
c
	oneamp = 0.
	amp = real(vis)**2 + aimag(vis)**2
	if(amp.gt.0) then
	  call lcase(flag)
	  if(flag(1:1).eq.'a') then
	    oneamp = sqrt(amp)
	  else if(flag(1:1).eq.'p') then
	    oneamp = 180.0/pi * atan2(aimag(vis),real(vis))
	  else if (flag(1:1).eq.'r') then
	    oneamp = real(vis)
	  else if (flag(1:1).eq.'i') then
	    oneamp = aimag(vis)
	  endif
	endif
	end
c********1*********2*********3*********4*********5*********6*********7*c
c* Phase
c:complex-data,uv-data
c+
	real function phase(vis)
c
c  Extracts the phase (radians) from a complex number.
c
c  Input:
c    vis	complex number
c--
c----------------------------------------------------------------------c
	implicit none
	complex vis
	phase = atan2(aimag(vis),real(vis))
	end
c********1*********2*********3*********4*********5*********6*********7*c
c* Expi
c: complex-data, uv-data, visibility
c+
      complex function expi(x)
c
c  Return the complex exponent of input in radians.
c
c  Input:
c    x		real input (radians).
c--
c---------------------------------------------------------------------c
      implicit none
      real x
      expi=cmplx(cos(x),sin(x))
      end
c********1*********2*********3*********4*********5*********6*********7*c
c* Angles
c: units, conversion
c+
	character*13 function angles(angle)
c
        implicit none
	double precision angle
c
c  Convert an angle expressed in degrees or hours into a string.
c
c  Input:
c    angle	Angle in floating point format.
c--
c		-oct 88 wh
c----------------------------------------------------------------------c
	double precision deg,secs
	integer ideg,imin
	character*1 si
	character*13 line
c
	deg = abs(angle)
	if(deg.eq.angle) then
	  si=' '
	else
	  si='-'
	end if
c
	ideg=deg
	imin=(deg-real(ideg))*60.
	secs=(deg-real(ideg)-real(imin)/60)*3600.
	write(line,'(a,ss,i3.2,'':'',i2.2,'':'',f5.2)')
     .						 si,ideg,imin,secs
	if(line(9:9).eq.' ') line(9:9)='0'
	angles=line
	end
