c************************************************************************
c  A collection of routines to generate gridding and gridding correction
c  functions.
c
c  History:
c    rjs Dark-ages  Original version.
c    rjs   8sep89   Improved documentation.
c    rjs  03jul96   Guard against 0**0 in gcffun.
c    rjs  07mar09   Corrrect error in one of the numbers. Found by
c                   Tsuyoshi Sawada at Nobeyama.
c************************************************************************
c*CorrFun -- Generate the gridding convolution correction function.
c: gridding,interpolation
c+
	subroutine corrfun(func,phi,n,width,alpha)
c
	implicit none
	integer n
	character func*(*)
	integer width
	real phi(n),alpha
c
c  Calculate the gridding correction function.
c
c  Inputs:
c    func	Gridding function used. Currently this can only be
c		'spheroidal'.
c    n		Width of image.
c    width	Width of gridding function used.
c    alpha	An extra parameter specifying the gridding function.
c
c  Output:
c    phi	Gridding correction function.
c
c  References:
c    F.R. Schwab "Optimal gridding of visibility data in radio interferometry",
c      Indirect Imaging, J.A. Roberts, ed., Cambridge Univ. Press, 1984.
c--
c------------------------------------------------------------------------
	integer i,i0
	real x,dx
c
c  Externals.
c
	real spheroid
c
c  Do the spheroid correction function.
c
	if(func.eq.'spheroidal')then
	  dx = 2./real(n)
	  i0 = n/2+1
	  do i=1,n
	    x = (i-i0)*dx
	    phi(i) = spheroid(x,width,alpha)
	  enddo
	else
	  call bug('f','Unknown gridding correction function')
	endif
	end
c************************************************************************
c*GcfFun -- Generate the gridding convolution function.
c: gridding,interpolation
c+
	subroutine gcffun(func,phi,n,width,alpha)
c
	implicit none
	integer n
	character func*(*)
	integer width
	real phi(n),alpha
c
c  Calculate the gridding function. Calculate only half the function, and
c  then use its evenness to reflect the rest.
c
c  Inputs:
c    func	The gridding function type. Currently only 'spheroidal'
c		is supported.
c    width	The function width, in pixels. Currently must be integral.
c    alpha	Some gridding function specific parameter.
c    n		Size of the table required.
c  Outputs:
c    phi	The output gridding table.
c
c  References:
c    F.R. Schwab "Optimal gridding of visibility data in radio interferometry",
c      Indirect Imaging, J.A. Roberts, ed., Cambridge Univ. Press, 1984.
c--
c------------------------------------------------------------------------
	integer i,j
	real p,x
c
c  Externals.
c
	real spheroid
c
c  Spheroidal function. Hopefully P is the nearest half integer
c  to ALPHA. Evaluate separately for j=0, to guard against 0**0.
c
	if(func.eq.'spheroidal')then
	  j = nint(2.*alpha)
	  p = 0.5 * real(j)
	  if(j.eq.0)then
	    do i=1,n
	      x = real(2*(i-1)-(n-1))/real(n-1)
	      phi(i) = 			spheroid(x,width,p)
	    enddo
	  else
	    do i=1,n
	      x = real(2*(i-1)-(n-1))/real(n-1)
	      phi(i) = sqrt(1-x*x)**j * spheroid(x,width,p)
	    enddo
	  endif
	else
	  call bug('f','Unknown gridding function type')
	endif
c
	end
c************************************************************************
	real function spheroid(eta,m,alpha)
c
	implicit none
	real eta,alpha
	integer m
c
c  This routine calculates the spheriodal wave functions, for use
c  if convolutional gridding. Schwab has show that these are, in a
c  sense, an optimal choice.
c
c  Alpha can only take on the values 0,0.5,1,1.5 or 2.
c  M can only take on the values 4,5,6
c  An error is detected whenever one of these fails.
c
c  These rational approximations are taken from Schwab "Optimal Gridding",
c  Indirect Imaging, ed J.A. Robert, 1984. If you want to understand
c  what is happening here, see that reference.
c
c  The tables storing the coefficients of the polys are 4 dimensional.
c
c  First dimension is:	coefficient order.
c  Second		nint(2*Alpha)	(in range 0 to 4)
c  Third		m		(in range 4 to 6)
c  Fourth		Approximation region (either 1 or 2)
c
c  The fourth dimension is not used for m=4 or m=5. Some of the poly
c  coefficients are zero. The arrays NNUM and NDENOM give the actual
c  orders of the polys. ETALIM gives the region where the coefficients
c  of the polys change.
c
c  Horners rule is used to evaluate the polys.
c
c------------------------------------------------------------------------
	integer twoalp,i,j,np,nq,ip
	real x,num,denom
	real etalim(4:8)
	real p(7,0:4,4:8,2),q(3,0:4,4:8,2)
	integer nnum(4:8),ndenom(4:8)
c
	save etalim,nnum,ndenom,p,q
c
c  The limiting value of ETA, after which to need to switch approximations.
c  A function of M only.
c
	data etalim/1.,1.,0.75,0.775,0.775/
c
c  The number of nonzero terms in the polys. A function of M only.
c
	data nnum/5,7,5,5,6/
	data ndenom/3,2,3,3,3/
c
c  M=4, ALPHA=0,2,0.5, ETA < ETALIM
c
	data ((p(i,j,4,1),i=1,7),j=0,4)/
     *		 1.584774E-2,-1.269612E-1, 2.333851E-1,
     *		-1.636744E-1, 5.014648E-2, 2*0.0,
     *		 3.101855E-2,-1.641253E-1, 2.385500E-1,
     *		-1.417069E-1, 3.773226E-2, 2*0.0,
     *		 5.007900E-2,-1.971357E-1, 2.363775E-1,
     *		-1.215569E-1, 2.853104E-2, 2*0.0,
     *		 7.201260E-2,-2.251580E-1, 2.293715E-1,
     *		-1.038359E-1, 2.174211E-2, 2*0.0,
     *		 9.585932E-2,-2.481381E-1, 2.194469E-1,
     *		-8.862132E-2, 1.672243E-2, 2*0.0/
c
c  M=5, ALPHA=0,2,0.5, ETA < ETALIM
c
	data ((p(i,j,5,1),i=1,7),j=0,4)/
     *		 3.722238E-3,-4.991683E-2, 1.658905E-1,-2.387240E-1,
     *		 1.877469E-1,-8.159855E-2, 3.051959E-2,
     *		 8.182649E-3,-7.325459E-2, 1.945697E-1,-2.396387E-1,
     *		 1.667832E-1,-6.620786E-2, 2.224041E-2,
     *		 1.466325E-2,-9.858686E-2, 2.180684E-1,-2.347118E-1,
     *		 1.464354E-1,-5.350728E-2, 1.624782E-2,
     *		 2.314317E-2,-1.246383E-1, 2.362036E-1,-2.257366E-1,
     *		 1.275895E-1,-4.317874E-2, 1.193168E-2,
     *		 3.346886E-2,-1.503778E-1, 2.492826E-1,-2.142055E-1,
     *		 1.106482E-1,-3.486024E-2, 8.821107E-3/
c
c  M=6, ALPHA=0,2,0.5, ETA < ETALIM
c
	data ((p(i,j,6,1),i=1,7),j=0,4)/
     * 		 5.613913E-2,-3.019847E-1, 6.256387E-1,
     *		-6.324887E-1, 3.303194E-1, 2*0.0,
     *		 6.843713E-2,-3.342119E-1, 6.302307E-1,
     *		-5.829747E-1, 2.765700E-1, 2*0.0,
     *		 8.203343E-2,-3.644705E-1, 6.278660E-1,
     *          -5.335581E-1, 2.312756E-1, 2*0.0,
     *           9.675562E-2,-3.922489E-1, 6.197133E-1,
     *          -4.857470E-1, 1.934013E-1, 2*0.0,
     *           1.124069E-1,-4.172349E-1, 6.069622E-1,
     *          -4.405326E-1, 1.618978E-1, 2*0.0 /
c
c  M=7, ALPHA=0,2,0.5, ETA < ETALIM
c
	data((p(i,j,7,1),i=1,7),j=0,4)/
     *		 2.460495e-2,-1.640964e-1, 4.340110e-1,
     *		-5.705516e-1, 4.418614e-1, 2*0.0,
     *		 3.070261e-2,-1.879546e-1, 4.565902e-1,
     *		-5.544891e-1, 3.892790e-1, 2*0.0,
     *		 3.770526e-2,-2.121608e-1, 4.746423E-1,
     *		-5.338058e-1, 3.417026e-1, 2*0.0,
     *		 4.559398e-2,-2.362670e-1, 4.881998e-1,
     *		-5.098448e-1, 2.991635e-1, 2*0.0,
     *		 5.432500e-2,-2.598752e-1, 4.974791e-1,
     *		-4.837861e-1, 2.614838e-1, 2*0.0/
c
c  M=8, ALPHA=0,2,0.5, ETA < ETALIM.
c
	data((p(i,j,8,1),i=1,7),j=0,4)/
     *		 1.378030e-2,-1.097846e-1, 3.625283e-1,
     *		-6.522477e-1, 6.684458e-1,-4.703556e-1,0.0,
     *		 1.721632e-2,-1.274981e-1, 3.917226e-1,
     *		-6.562264e-1, 6.305859e-1,-4.067119e-1,0.0,
     *		 2.121871e-2,-1.461891e-1, 4.185427e-1,
     *		-6.543539e-1, 5.904660e-1,-3.507098e-1,0.0,
     *		 2.580565e-2,-1.656048e-1, 4.426283e-1,
     *		-6.473472e-1, 5.494752e-1,-3.018936e-1,0.0,
     *		 3.098251e-2,-1.854823e-1, 4.637398e-1,
     *		-6.359482e-1, 5.086794e-1,-2.595588e-1,0.0/
 
c
c  M=6, ALPHA=0,2,0.5, ETA > ETALIM
c
	data ((p(i,j,6,2),i=1,7),j=0,4)/
     *		 8.531865E-4,-1.616105E-2, 6.888533E-2,
     *          -1.109391E-1, 7.747182E-2, 2*0.,
     *           2.060760E-3,-2.558954E-2, 8.595213E-2,
     *          -1.170228E-1, 7.094106E-2, 2*0.,
     *           4.028559E-3,-3.697768E-2, 1.021332E-1,
     *          -1.201436E-1, 6.412774E-2, 2*0.,
     *           6.887946E-3,-4.994202E-2, 1.168451E-1,
     *          -1.207733E-1, 5.744210E-2, 2*0.,
     *           1.071895E-2,-6.404749E-2, 1.297386E-1,
     *          -1.194208E-1, 5.112822E-2, 2*0./
c
c  M=7, ALPHA=0,2,0.5, ETA > ETALIM.
c
	data ((p(i,j,7,2),i=1,7),j=0,4)/
     *		 1.924318e-4,-5.044864e-3, 2.979803e-2,
     *		-6.660688e-2, 6.792268e-2, 2*0.0,
     *		 5.030909e-4,-8.639332e-3, 4.018472e-2,
     *		-7.595456e-2, 6.696215e-2, 2*0.0,
     *		 1.059406e-3,-1.343605e-2, 5.135360e-2,
     *		-8.386588e-2, 6.484517e-2, 2*0.0,
     *		 1.941904e-3,-1.943727e-2, 6.288221e-2,
     *		-9.021607e-2, 6.193000e-2, 2*0.0,
     *		 3.224785e-3,-2.657664e-2, 7.438627e-2,
     *		-9.500554e-2, 5.850884e-2, 2*0.0/
c
c  M=8, ALPHA=0,2,0.5, ETA > ETALIM.
c
	data ((p(i,j,8,2),i=1,7),j=0,4)/
     *		 4.290460e-5,-1.508077e-3, 1.233763e-2,
     *		-4.091270e-2, 6.547454e-2,-5.664203e-2,0.0,
     *		 1.201008e-4,-2.778372e-3, 1.797999e-2,
     *		-5.055048e-2, 7.125083e-2,-5.469912e-2,0.0,
     *		 2.698511e-4,-4.628815e-3, 2.470890e-2,
     *		-6.017759e-2, 7.566434e-2,-5.202678e-2,0.0,
     *		 5.259595e-4,-7.144198e-3, 3.238633e-2,
     *		-6.946769e-2, 7.873067e-2,-4.889490e-2,0.0,
     *		 9.255826e-4,-1.038126e-2, 4.083176e-2,
     *		-7.815954e-2, 8.054087e-2,-4.552077e-2,0.0/
c
c  M=4, ALPHA=0,2,0.5, ETA < ETALIM.
c
	data ((q(i,j,4,1),i=1,3),j=0,4)/
     *		1., 4.845581E-1, 7.457381E-2,
     *		1., 4.514531E-1, 6.458640E-2,
     *		1., 4.228767E-1, 5.655715E-2,
     *		1., 3.978515E-1, 4.997164E-2,
     *		1., 3.756999E-1, 4.448800E-2/
c
c  M=5, ALPHA=0,2,0.5, ETA < ETALIM
C
	data ((q(i,j,5,1),i=1,3),j=0,4)/
     *		1., 2.418820E-1, 0.0,
     *		1., 2.291233E-1, 0.0,
     *		1., 2.177793E-1, 0.0,
     *		1., 2.075784E-1, 0.0,
     *		1., 1.983358E-1, 0.0/
c
c  M=6, ALPHA=0,2,0.5, ETA < ETALIM
C
	data ((q(i,j,6,1),i=1,3),j=0,4)/
     *		1., 9.077644E-1, 2.535284E-1,
     *		1., 8.626056E-1, 2.291400E-1,
     *		1., 8.212018E-1, 2.078043E-1,
     *		1., 7.831755E-1, 1.890848E-1,
     *		1., 7.481828E-1, 1.726085E-1 /
c
c  M=7, ALPHA=0,2,0.5, ETA < ETALIM
C
	data ((q(i,j,7,1),i=1,3),j=0,4)/
     *		1., 1.124957e00, 3.784976e-1,
     *		1., 1.075420e00, 3.466086e-1,
     *		1., 1.029374e00, 3.181219e-1,
     *		1., 9.865496e-1, 2.926441e-1,
     *		1., 9.466891e-1, 2.698218e-1/
c
c  M=7, ALPHA=0,2,0.5, ETA < ETALIM
C
	data ((q(i,j,8,1),i=1,3),j=0,4)/
     *		1., 1.076975e00, 3.394154e-1,
     *		1., 1.036132e00, 3.145673e-1,
     *		1., 9.978025e-1, 2.920529e-1,
     *		1., 9.617584e-1, 2.715949e-1,
     *		1., 9.278774e-1, 2.530051e-1/
c
c  M=6,ALPHA=0,2,0.5, ETA > ETALIM
C
	data((q(i,j,6,2),i=1,3),j=0,4)/
     *		1., 1.101270   , 3.858544E-1,
     *		1., 1.025431   , 3.337648E-1,
     *		1., 9.599102E-1, 2.918724E-1,
     *		1., 9.025276E-1, 2.575337E-1,
     *		1., 8.517470E-1, 2.289667E-1 /
c
c  M=7,ALPHA=0,2,0.5, ETA > ETALIM
C
	data((q(i,j,7,2),i=1,3),j=0,4)/
     *		1., 1.450730e00, 6.578684e-1,
     *		1., 1.353872e00, 5.724332e-1,
     *		1., 1.269924e00, 5.032139e-1,
     *		1., 1.196177e00, 4.460948e-1,
     *		1., 1.130719e00, 3.982785e-1/
c
c  M=8,ALPHA=0,2,0.5, ETA > ETALIM
C
	data((q(i,j,8,2),i=1,3),j=0,4)/
     *		1., 1.379457e00, 5.786953e-1,
     *		1., 1.300303e00, 5.135748e-1,
     *		1., 1.230436e00, 4.593779e-1,
     *		1., 1.168075e00, 4.135871e-1,
     *		1., 1.111893e00, 3.744076e-1/
c------------------------------------------------------------------------
c
c  Check.
c
	twoalp = nint(2*alpha)
	if(abs(eta).gt.1)
     *	  call bug('f','Abs(ETA) exceeds 1 in SPHERIODAL')
	if(twoalp.lt.0.or.twoalp.gt.4)
     *	  call bug('f','Illegal value of ALPHA in SPHERIODAL')
	if(m.lt.4.or.m.gt.8)
     *	  call bug('f','Illegal value of M in SPHERIODAL')
c
c  Go to the appropiate approximation.
c
	if(abs(eta).gt.etalim(m))then
	  ip = 2
	  x = eta*eta - 1
	else
	  ip = 1
	  x = eta*eta - etalim(m)*etalim(m)
	endif
c
c  Get numerator - Horners rule.
c
	np = nnum(m)
	num = p(np,twoalp,m,ip)
	do i=np-1,1,-1
	  num = num * x +  p(i,twoalp,m,ip)
	enddo
c
c  Get denominator - Horners rule.
c
	nq = ndenom(m)
	denom = q(nq,twoalp,m,ip)
	do i=nq-1,1,-1
	  denom = denom * x + q(i,twoalp,m,ip)
	enddo
c
c  Return the value.
c
	spheroid = num/denom
	end
