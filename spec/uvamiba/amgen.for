
	subroutine aminit (config, xsctype, xparallactify,
     :                         xDextent, xoverhead,
     :                         xoffset, xlat, xfrq, xelmin,
     :                         xnant, err)


	implicit none

	include 'constants.inc'
	include 'amgen.h'


	character*(*) config
	character*(*) xsctype
	logical       xparallactify

	real xDextent
	real xoverhead
	real xoffset
	real xlat
	real xfrq
	real xelmin

	integer xnant
	logical err


*-------------------------------------------------------

	err = .false.

	lat   = xlat * degrad
	elmin = xelmin * degrad

	lambda = cvel * 1.E-6 / xfrq	! lambda in mm

	freq = xfrq

	if (index(xsctype, 'drift') .gt. 0) then
	  sctype = DRIFT
	else if (index(xsctype, 'point') .gt. 0) then
	  sctype = POINT
	else
	  write (6, '('' unknown scan type : '', A)') xsctype
	  err = .true.
	  RETURN
	end if

	parallactify = xparallactify


	Dextent  = xDextent
	overhead = xoverhead
	offset   = xoffset


	call load_param (config, err)
	if (err) RETURN

	xnant = plat(1).nant

	end


*********************************************************

	subroutine load_param (cfg_file, err)

*  load the antenna configuration

	implicit none

	include 'constants.inc'
	include 'amgen.h'

	character*(*) cfg_file
	logical err

	character*80 str

	real R3(3)

	integer num_ant
	integer ia

	integer ip1, ip2, ia1, ia2, ias
	integer ios

	real    scale /1.0/

*--------------------------------------------------------------


	open (unit=1, file=cfg_file, status='old', iostat=ios)
	if (ios .ne. 0) then
	  write (6, '('' file not found : '', A)') cfg_file
	  err = .true.
	  RETURN
	end if


	np = 0
	nb = 0
	nv = 0
	num_ant = 0

	do while (.true.)

	  read (1, '(A)', end=999) str

	  if (index (str, '#') .gt. 0) goto 100

	  if (index (str, '$platform') .gt. 0) then
	    np = np + 1

	    read (str(10:), *) R3
	    plat(np).xp.x = scale * R3(1) / lambda
	    plat(np).xp.y = scale * R3(2) / lambda
	    plat(np).xp.z = scale * R3(3) / lambda

	    plat(np).nant = 0

	  else if (index (str, '$antenna') .gt. 0) then
	    plat(np).nant = plat(np).nant + 1
	    ia = plat(np).nant

	    num_ant = num_ant + 1

	    read (str(9:), *) R3
	    plat(np).ant(ia).x = R3(1) / lambda
	    plat(np).ant(ia).y = R3(2) / lambda
	    plat(np).ant(ia).z = R3(3) / lambda
	    
	  end if
  
 100	end do

 999	continue
	close (1)


	  
	write (6, '(I4, '' platforms'')') np
	do ia = 1, np
	  write(6,'('' platform '', I2, '' has '', I4, '' antennas'')')
     :           ia, plat(ia).nant
	end do


*	build the mapping array

	nb = 0
	do ip1 = 1, np
	  do ip2 = ip1, np
	    do ia1 = 1, plat(ip1).nant
	      if (ip2 .eq. ip1) then
	        ias = ia1 + 1
	      else
	        ias = 1
	      end if

	      do ia2 = ias, plat(ip2).nant

	        nb = nb + 1
	        bmap(nb).pl1  = ip1
	        bmap(nb).ant1 = ia1
	        bmap(nb).pl2  = ip2
	        bmap(nb).ant2 = ia2

	      end do
	    end do
	  end do
	end do


	write (6, '(I5, '' baselines'')') nb


	end

***************************************************************

	subroutine plot_cfg 

*  plots the antenna layout(s)

	implicit none

	include 'amgen.h'

	real xp(MXA), yp(MXA), xmn, xmx, ymn, ymx

	integer ip, ia

	integer  n
	integer npl


	logical plt_plat /.true./

*--------------------------------------------------------------

*  plot the platforms

	if (plt_plat) then

	  if (np .gt. 1) then

	    do ip = 1, np
	      xp(ip) = plat(ip).xp.x
	      yp(ip) = plat(ip).xp.y
	    end do

	    call minmax (np, xp, xmn, xmx)
	    call minmax (np, yp, ymn, ymx)
	    xmn = min (xmn, ymn)
	    ymx = max (xmx, ymx)
	    call pgenv (xmn, xmx, xmn, xmx, 1, 0)
	    call pgpoint (np, xp, yp, 0)
	    call pglabel ('x (lambda)', 'y (lambda)', 
     :                           'Platform configuration')

	  else
	    write (6, '('' Only one platform'')')
	  end if

	end if


*  plot the platform layout

	npl = 0
	do ip = 1, np

	  do ia = 1, plat(ip).nant
	    npl = npl + 1
	    xp(npl) = plat(ip).ant(ia).x
	    yp(npl) = plat(ip).ant(ia).y
	  end do
	end do


	n = npl

	call minmax (n, xp, xmn, xmx)
	call minmax (n, yp, ymn, ymx)
	xmn = min (xmn, ymn)
	ymx = max (xmx, ymx)
	call pgenv (xmn, xmx, xmn, xmx, 1, 0)
	call pglabel ('x (lambda)', 'y (lambda)', 
     :                           'Antenna layout')
	call pgpoint (n, xp, yp, (ip+1))


	end

****************************************************************


	real function parallactic (ha, dec, lat)

*  compute the parallactic angle

	implicit none

	real ha, dec, lat		! all radians

	real sz, cz

*---------------------------------------------------

	sz = cos(lat)*sin(ha)
	cz = sin(lat)*cos(dec) - cos(lat)*sin(dec)*cos(ha)

	if ((sz .ne. 0.) .OR. (cz .ne. 0.)) then
	  parallactic = atan2 (sz, cz)

	else
	  parallactic = 0.

	end if

	end
c************************************************************************
	subroutine amDEc(xdec)
c
	implicit none
	double precision xdec
c
c  Feed in the declination.
c
c------------------------------------------------------------------------
	include 'amgen.h'
	dec = xdec
	end
c************************************************************************
	subroutine amComp (T, dT, uvw, pa,
     :                         az, el, onflag, err)


*  Given T [in range -12. to +12 h] return the uv coords of
*  each antenna.

*  return onflag = T if valid data (antenna ON source);
*                  F    antennas OFF source

*  return err = T on an error condition (eg, below horizon).


*  dT is the RA offset of the optical axis, relative to the
*  mid-point of the drift scan. (= 0 for point observations).

*  uvw(3,*)  is the array of uvw coords.
*  pa        is the position angle on the sky of platform's x-axis

	implicit none

	
	include 'constants.inc'
	include 'amgen.h'


	real    T
	real    dT

	real    uvw(3, *)
	real    pa

	logical onflag
	logical err


	integer ia

	real ha0
	real Tc
	real P
	integer N

	real az, el
	real ampv

	real h, hv

	real ha0_v(3), az_v(3)
	real bsh_v(3)

	real az2tel_m(3,3)
	real ha02az_m(3,3)
	real ha2uvw_m(3,3)

	real bs_fact

	real Delta
	real par

	real par_m(3,3)
	real antxy_v(3)


*  functions

	real sla_pa


*--------------------------------------------------------



	bs_fact = (2.*pi) / lambda

	onflag = .true.

*  create the baseline vector:

	err = .false.


	if (sctype .eq. POINT) then
	  dT = 0.
	  ha0 = T*15.0 * degrad

	else if (sctype .eq. DRIFT) then
	  P = Dextent + overhead
	  if (T .gt. 0) then
	    Delta = T - (P/2. + offset)
	    if (Delta .le. 0.) then
	      N = 0
	    else
	      N = int (Delta/P) + 1
	    end if
	    Tc = N*P + offset
	    dT = T - Tc 
	  else
	    Delta = abs(T) - (P/2. - offset)
	    if (Delta .le. 0.) then
	      N = 0
	    else
	      N = int (Delta/P) + 1
	    end if

	    Tc = N*P - offset
	    dT = Tc  - abs(T)
	  end if

	  ha0 = sign(Tc, T) *15.0 * degrad

	end if

	  
	nv = 0

	ampv = 1.
	hv =  ha0 + pi


	call Pol2V (ampv, hv, dec, ha0_v)
	call make_ha2az (HA0, dec, lat, ha02az_m)
	call MdotV (ha02az_m, ha0_v, az_v)
	call V2Pol (az_v, ampv, az, el)

	if (el .lt. elmin) then
	  write (6, '(''el below minimum.  HA = '', F10.1)') h
	  err = .true.
	  RETURN
	end if

	par = sla_pa (ha0, dec, lat)
	if (parallactify) then
	  call make_par_m (par, par_m)
	  pa = 0.
	else
	  pa = -par
	end if


	do ia = 1, plat(1).nant
	  if (parallactify) then
	    uvw(1,ia) = plat(1).ant(ia).x
	    uvw(2,ia) = plat(1).ant(ia).y
	    uvw(3,ia) = plat(1).ant(ia).z
	  else
	    uvw(1,ia) = cos(pa)*plat(1).ant(ia).x +
     :                  sin(pa)*plat(1).ant(ia).y
	    uvw(2,ia) = cos(pa)*plat(1).ant(ia).y +
     :                 -sin(pa)*plat(1).ant(ia).x
	    uvw(3,ia) = plat(1).ant(ia).z
	  end if
	end do

	end


***********************************************************

	subroutine plot_data

	implicit none

	include 'amgen.h'

	real xmn, xmx, ymn, ymx
	real u, v, r
	integer nbin

	real bwf(8) /.94, .96, .98, 1.0, 1.02, 1.04, 1.06, 1.08/
	real ibf

	integer iv, ib

*----------------------------------------------------------

	xmn = vis(1).u(1).u
	xmx = vis(1).u(1).u
	ymn = vis(1).u(1).v
	ymx = vis(1).u(1).v
	
	do iv = 1, nv
	  do ib = 1, nb
	    xmn = min (xmn, vis(iv).u(ib).u)
	    xmx = max (xmx, vis(iv).u(ib).u)
	    ymn = min (ymn, vis(iv).u(ib).v)
	    ymx = max (ymx, vis(iv).u(ib).v)
	  end do
	end do

	xmn = min (xmn, ymn)
	xmx = max (xmx, ymx)
	xmx = max (xmx, abs(xmn))
	xmn = -1.1 * xmx
	xmx = 1.1 * xmx

	call pgenv (xmn, xmx, xmn, xmx, 1, 0)
	call pglabel ('u', 'v', ' ')

	ir = 0
	do iv = 1, nv
	  do ib = 1, nb
	    u = vis(iv).u(ib).u
	    v = vis(iv).u(ib).v
	    r = sqrt (u*u + v*v)
	    do ibf = 1, 8
	      call pgpoint (1, u*bwf(ibf), v*bwf(ibf), 1)
	      call pgpoint (1, -u*bwf(ibf), -v*bwf(ibf), 1)

	      ir = ir + 1
	      rad(ir) = r * bwf(ibf)		! for the histogram
	    end do

	  end do
	end do

	nbin = 100
	call minmax (ir, rad, xmn, xmx)
	xmn = 0.
	call pghist (ir, rad, xmn, xmx, nbin, 0)
	call pglabel (' |u|', ' ', ' visibility distribution')
	
	end

*************************************************************


	subroutine make_par_m (par, par_m)


*  create the platform rotation matrix


	implicit none

	real par

	real par_m(3,3)

*----------------------------------------------------

	par_m(1,1) = cos(par)
	par_m(1,2) = sin(par)
	par_m(1,3) = 0.

	par_m(2,1) = -sin(par)
	par_m(2,2) = cos(par)
	par_m(2,3) = 0.

	par_m(3,1) = 0.
	par_m(3,2) = 0.
	par_m(3,3) = 1.

	end



************************************************************


      real  FUNCTION sla_PA (HA, DEC, PHI)
*+
*     - - -
*      P A
*     - - -
*
*  HA, Dec to Parallactic Angle 
*
*  Given:
*     HA     d     hour angle in radians (geocentric apparent)
*     DEC    d     declination in radians (geocentric apparent)
*     PHI    d     observatory latitude in radians (geodetic)
*
*  The result is in the range -pi to +pi
*
*  Notes:
*
*  1)  The parallactic angle at a point in the sky is the position
*      angle of the vertical, i.e. the angle between the direction to
*      the pole and to the zenith.  In precise applications care must
*      be taken only to use geocentric apparent HA,Dec and to consider
*      separately the effects of atmospheric refraction and telescope
*      mount errors.
*
*  2)  At the pole a return HA
*
*  P.T.Wallace   Starlink   11 February 1992
*-

      IMPLICIT NONE

      real HA, DEC, PHI


      real SLAT, CLAT, SDC, CDC, SQSZ, CQSZ

*---------------------------------------------------------


      SLAT=SIN(PHI)
      CLAT=COS(PHI)
      SDC=SIN(DEC)
      CDC=COS(DEC)

      SQSZ=CLAT*SIN(HA)
      if (abs(sqsz) .lt. 1.e-6) sqsz = 0.

c      CQSZ=(SLAT-SDC*(SDC*SLAT+CDC*CLAT*COS(HA)))/CDC
      CQSZ = SLAT*CDC - SDC*CLAT*COS(HA)
      if (abs(cqsz) .lt. 1.e-6) cqsz = 0.

      IF ((SQSZ.EQ.0D0) .AND. (CQSZ.EQ.0D0)) then
	sla_PA = HA
      else
        sla_PA=ATAN2(SQSZ,CQSZ)
      end if

      END
