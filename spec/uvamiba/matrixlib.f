

*  a collection of matrix/vector routines

*  all matrices should have an _m suffix
*  all vectors  should have a  _v suffix

*  routines:

*  V2Pol (v, amp, lon, lat)    	v -> polar
*  Pol2V (amp, lon, lat, v)
*  VdotV (v1, v2, s)		dot product v1 . v2 -> s
*  modV (v)			return the length of teh evctor

* 
*  VdotM (v1, m, v2)		v1 . m -> v2
*  MdotV (m, v1, v2)		m . v1 -> v2
*  invM  (m1, m2)		invert a unit matrix

*  make_ha2az (ha, dc, lat, ha2az_m)
*  make_az2tel (az, el, az2tel_m)
*  make_ha2uvw (ha, dc, ha2_uvw_m)

*  The conventions:

*	^ j
*	|
*	|
*	|
*	|------>  i

* lon is measured clockwise from the j axis
* lat is measured up from the (i,j) plane

* M(i,j) --> first index is row, second is the column

************************************************

	subroutine V2Pol (v, amp, lon, lat)

*  convert the right hand vector (v) to polar (lon, lat)

	implicit none

	real v(3)
	real amp
	real lon
	real lat

	real se
	real ceca, cesa

*--------------------------------------------------

	amp = sqrt (v(1)*v(1) + v(2)*v(2) + v(3)*v(3))

	if (amp .eq. 0) then
	  lon = 0.
	  lat = 0.
	  write (6, '('' zero length vector'')')
	  RETURN
	end if


	cesa = v(1) / amp
	ceca = v(2) / amp
	se   = v(3) / amp

	lat = asin (se)

	if ((cesa .ne. 0.) .AND. (ceca .ne. 0.)) then
	  lon = atan2 (cesa, ceca)
	else
	  lon = 0.
	end if

	end

***********************************************************


	subroutine Pol2V (amp, lon, lat, v)

* reverse of V2Pol :  given a polar description, return the vector V


	implicit none

	real amp
	real lon
	real lat

	real v(3)

*----------------------------------------------------------

	v(1) = amp * sin(lon) * cos(lat)
	v(2) = amp * cos(lon) * cos(lat)
	v(3) = amp * sin(lat)

	end

************************************************************

	subroutine VdotV (v1, v2, s)


*  vector dot product

	implicit none

	real v1(3)
	real v2(3)
	real s

	integer i

*----------------------------------------------------------

	s = 0.
	do i = 1, 3
	  s = s + v1(i)*v2(i)
	end do

	end

**********************************************************


	subroutine VdotM (v1, m, v2)


*  left Vector * Matrix product - yields a vector

	implicit none

	real v1(3)
	real m(3,3)
	real v2(3)

	integer col, row

*---------------------------------------------------------- 	

	do col = 1, 3
	  v2(col) = 0.
	  do row = 1, 3
	    v2(col) = v2(col) + v1(row)*m(row,col)
	  end do
	end do

	end

***********************************************************

	subroutine MdotV (m, v1, v2)

* right Matrix * vector product - yields a vector

	implicit none

	real m(3,3)
	real v1(3)
	real v2(3)

	integer row, col

*----------------------------------------------------------

	do row = 1, 3

	  v2(row) = 0.

	  do col = 1, 3
	    v2(row) = v2(row) + m(row,col)*v1(col)
	  end do

	end do

	end

**********************************************************

	subroutine invM (m1, m2)

*  invert a unit (rotation) matrix

	implicit none

	real m1(3,3)
	real m2(3,3)

	integer row, col

*-------------------------------------------------------

	do row = 1, 3
	  do col = 1, 3
	    m2(col,row) = m1(row,col)
	  end do
	end do

	end


***************************************************

	subroutine make_ha2az (ha, dc, lat, m)


*  create the rotation matrix for conversion between (ha/dec) and (az/el)


* 		az_v = ha2az_m * ha_v
* 		ha_v = az_v * ha2az_m


	implicit none

	real ha, dc, lat
	real m(3,3)

*--------------------------------------------------


	m(1,1) = 1.0
	m(1,2) = 0.
	m(1,3) = 0.
	m(2,1) = 0.
	m(3,1) = 0.

	m(2,2) = sin(lat)
	m(2,3) = cos(lat)

	m(3,2) = -cos(lat)
	m(3,3) = sin(lat)

	end


*************************************************

	subroutine make_az2tel (az, el, m)

* transforms from the telescope frame to the observatory frame

* k - directed along the optical axis (outwards)
* i - in the aperture plane, parallel to the el axis; (East)
* j - in the aperture plane, normal to the el axis (North)

*		tel_v = az2tel_m * az_v
*		az_v  = tel_v * az2tel_m

	implicit none

	real az, el
	real m(3,3)

*------------------------------------------------

	m(1,1) = cos(az)
	m(1,2) = -sin(az)
	m(1,3) = 0.

	m(2,1) = sin(el)*sin(az)
	m(2,2) = sin(el)*cos(az)
	m(2,3) = -cos(el)

	m(3,1) = cos(el)*sin(az)
	m(3,2) = cos(el)*cos(az)
	m(3,3) = sin(el)

	end
*************************************************

	subroutine make_ha2uvw (ha, dc, m)

* transforms from the ha/dec frame to the (uvw) frame

* (ha/dc) defines the optical azxis

* k - directed along the optical axis (outwards)
* i - in the equatorial plane
* j - in the aperture plane, normal to the el axis (North)

*		uvw_v = ha2uvw_m * ha_v
*		ha_v  = uvw_v * ha2uvw_m

	implicit none

	real ha, dc
	real m(3,3)

*------------------------------------------------

	m(1,1) = -cos(ha)
	m(1,2) =  sin(ha)
	m(1,3) =  0.

	m(2,1) = -sin(dc)*sin(ha)
	m(2,2) = -sin(dc)*cos(ha)
	m(2,3) = -cos(dc)

	m(3,1) = -cos(dc)*sin(ha)
	m(3,2) = -cos(dc)*cos(ha)
	m(3,3) =  sin(dc)

	end


*********************************************************

	real function modV (v)


*  return the magnitude of the vector (v)

	implicit none

	real v(3)

*-----------------------------------------------------

	modV = sqrt (v(1)*v(1) + v(2)*v(2) + v(3)*v(3))

	end
	
