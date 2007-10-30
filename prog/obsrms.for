c**********************************************************************c
      program OBSRMS
      implicit none
c
c= OBSRMS - Calculate theoretical rms for telescope array observations.
c& mchw
c: image analysis
c+
c	OBSRMS - Calculate theoretical rms for telescope array observations.
c	The MIRIAD task TELEPAR provides typical values for Tsys and Jy/K.
c	for several observatories.
c@ tsys
c	SSB system temperature (Kelvin). Default=300 K.
c	Tsys includes the loss through the atmosphere.
c@ jyperk
c	Antenna gain in units Jy/K. Default=150 Jy/K.
c@ antdiam
c	Antenna diameter (m), and aperture efficiency can be used to calculate 
c	jyperk.	If only one value is given then the aperture efficiency used
c	is 0.6.	Default=0,0.6 i.e. use jyperk value.
c@ lambda
c	wavelength in millimeters. Default=3 mm.
c@ freq
c	freq (GHz) can be used to compute lambda. Default=0 GHz, uses lambda.
c@ theta
c	Beamsize in arcsec. Two values may be given for the beam FWHM.
c	If only one value is given it is used for both axes. Default=1,1
c@ nants
c	Number of antennas in array. Single antenna is OK too. Default=9. 
c@ bw
c	Bandwidth in MHz used for continuum observations. Default=800 MHz.
c@ deltav
c	Velocity resolution in km/s  can be used to compute bandwidth for
c	spectral line observations. Default=0 km/s, i.e. use bandwidth value.
c@ inttime
c	Total Integration time on source in minutes. Default=1 minute.
c@ coreta
c	Correlator efficiency factor. A perfect correlator has coreta=1.
c	2-, 3-, and 4-level correlators have coreta=0.64, 0.81, and 0.88
c	respectively. (see Thompson, Moran, and Swenson, p229).
c	Default=0.88
c@ rmsphase
c	RMS phase noise (degrees). Phase noise reduces the signal, effectively
c	increasing the observed RMS noise in the data by a factor
c	   exp(rmsphase(radians)**2/2.). For strong sources atmospheric
c	phase noise can be removed by selfcalibration.
c	Default=0. degrees. i.e. perfect data.
c--
c History
c  mchw 28may96  Original version.
c  mchw 29may96  Added some efficiency factors.
c  mchw 11jun96  Echo inputs to user.
c  mchw 15aug96  Format change for smaller inttime.
c  mchw 25sep96  Calculate deltav from bw if input; format 0.1" theta 
c  mchw 25mar97  Change format and units for consistency and for VLBI.
c
c Possible development:
c
c@ telescop
c	Name of the telescope. This provides default values for tsys and Jy/K.
c----------------------------------------------------------------------c
	character version*(*)
	parameter(version='version 25-MAR-97')
        include 'mirconst.h'
	character telescop*20,line*80
	real tsys,jyperk,lambda,theta(2),nants,deltav,inttime
	real rms_Jy,rms_Tb,bw,omega,freq,antdiam,anteta,coreta,rmsphase
c
c  Get the input parameters.
c
	call output('OBSRMS: '//version)
	call keyini
	call keya('telescop',telescop,' ')
	call keyr('tsys',tsys,300.)
	call keyr('jyperk',jyperk,150.)
	call keyr('antdiam',antdiam,0.)
	call keyr('antdiam',anteta,0.6)
	call keyr('lambda',lambda,3.)
	call keyr('freq',freq,0.)
	call keyr('theta',theta(1),1.)
	call keyr('theta',theta(2),theta(1))
	call keyr('nants',nants,9.)
	call keyr('bw',bw,800.)
	call keyr('deltav',deltav,0.)
	call keyr('inttime',inttime,1.)
	call keyr('coreta',coreta,0.88)
	call keyr('rmsphase',rmsphase,0.)
	call keyfin
c
c Conversion factors.
c
	if(antdiam*anteta.gt.0.)then
	  jyperk=2.*KMKS * 1.e26 / (PI/4. * antdiam**2 * anteta)
	endif
c
	if(freq.gt.0.)then
	  lambda=1.e-6*CMKS/freq
	else if(lambda.gt.0.)then
	  freq=1.e-6*CMKS/lambda
	else
	  call bug('f','you must specify either lambda or freq')
	endif
c
	if(deltav.gt.0.)then
	  bw=deltav/lambda
	else
	  deltav=bw*lambda
	endif
c
c Echo inputs to user.
c
	call bug('i','inttime is now in minutes')
c********1*********2*********3*********4*********5*********6*********7*c
        write(line,'(a,a)') ' tsys  jyperk  freq  ',
     *    'lambda  deltav   bw  inttime  nants  theta  coreta rmsphase'
        call output(line)
        write(line,'(f6.0,f6.1,f7.1,2f8.2,2f7.2,f7.0,2f5.1,f6.2,f6.0)') 
     *	    tsys, jyperk, freq,
     *      lambda, deltav, bw, inttime, nants, theta, coreta, rmsphase
        call output(line)
c
c Convert to MKS units.
c
	lambda=lambda*1.e-3
	bw=bw*1.e6
	inttime=inttime*60.
	theta(1)=theta(1)*PI/180./3600.
	theta(2)=theta(2)*PI/180./3600.
	if(nants.lt.2.)nants=2.
c
c Efficiency factors.
c
	if(coreta.ne.0.) tsys=tsys/coreta
	if(rmsphase.ne.0.) tsys=tsys*exp((PI/180.*rmsphase)**2/2.)
c
c Calculate rms_Jy
c
	rms_Jy = tsys*jyperk/sqrt(2.*bw*inttime*nants*(nants-1)/2.)
c
c Calculate rms_Tb
c
        omega = PI * theta(1) * theta(2) /(4.*log(2.))
	rms_Tb = lambda**2 * rms_Jy * 1.e-26 / (2.*KMKS*omega)
        write(line,'(a,g12.3,a,a,g12.3,a)') 
     *			'Rms Flux density:', rms_Jy*1000., ' mJy/beam;',
     *			'    Rms Brightness:', rms_Tb, ' K'
	call output(line)
      end
c********1*********2*********3*********4*********5*********6*********7*c
