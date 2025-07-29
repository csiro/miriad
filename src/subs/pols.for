c************************************************************************
c  Some subroutines to help manage between polarisation mnemonics and
c  polarisation codes.
c
c  History:
c    rjs  13feb91 Original version.
c    rjs  18may91 Recognise upper and lower case mnemonics. Always return
c		  upper case mnemonics.
c    rjs  31oct91 Added PolsPara function.
c    rjs  16sep93 Rename bsrch to binsrch.
c************************************************************************
c* PolsPara -- Check if the polarization is a intensity-type.
c& rjs
c: polarization
c+
	logical function PolsPara(code)
c
	implicit none
	integer code
c
c  Check whether the polarisation is one of I,RR,LL,XX or YY.
c
c  Input:
c    code	Polarisation code, in teh range -8 to 4 (excluding 0).
c  Output:
c    PolsPara	True if the code represents one of I,XX,YY,RR or LL.
c--
c------------------------------------------------------------------------
	integer PolI,PolXX,PolYY,PolRR,PolLL
	parameter(PolI=1,PolRR=-1,PolLL=-2,PolXX=-5,PolYY=-6)
c
	PolsPara = code.eq.PolI.or.code.eq.PolRR.or.code.eq.PolLL.or.
     *				   code.eq.PolXX.or.code.eq.PolYY
c
	end	
c************************************************************************
c* PolsC2P -- Convert polarization code to a polarization mnemonic.
c& rjs
c: polarization
c+
	character*(*) function PolsC2P(code)
c
	implicit none
	integer code
c
c  Convert polarization codes to polarization mnemonic.
c
c  Input:
c    code	Polarization code, in the range -8 to 4 (excluding 0).
c  Output:
c    PolsC2P	Polarization mnemonic.
c--
c------------------------------------------------------------------------
	character mnemos(-8:4)*2
	data mnemos/'YX','XY','YY','XX','LR','RL','LL','RR','xx',
     *		    'I ','Q ','U ','V '/
c
	if(code.lt.-8.or.code.gt.4.or.code.eq.0)
     *	  call bug('f','Illegal polarization code')
c
	PolsC2P = mnemos(code)
	end
c************************************************************************
c* PolsP2C -- Convert polarization mnemonic to a polarization code.
c& rjs
c: polarization
c+
	integer function PolsP2C(mnemo)
c
	implicit none
	character mnemo*(*)
c
c  Determine the polarization code, given the mnemonic.
c
c  Input:
c    mnemo	The polarization mnemonic, being one of 'XX','YY','XY','YX',
c		'RR','LL','RL','LR','I','Q','U','V'.
c  Output
c    PolsP2C	The corresponding polarization code.
c--
c------------------------------------------------------------------------
	integer codes(24)
	character mnemos(24)*2,line*64
	integer j
c
c  Externals.
c
	integer binsrcha
c
	data mnemos/'I ','LL','LR','Q ','RL','RR','U ','V ',
     *		    'XX','XY','YX','YY',
     *		    'i ','ll','lr','q ','rl','rr','u ','v ',
     *		    'xx','xy','yx','yy'/
	data codes/   1,  -2,  -4,  2,   -3,  -1,  3,   4,
     *		     -5,  -7,  -8,  -6,
     *		      1,  -2,  -4,  2,   -3,  -1,  3,   4,
     *		     -5,  -7,  -8,  -6/
c
c  Perform a binary search to find the polarisation code.
c
	j = binsrcha(mnemo,mnemos,24)  
	if(j.eq.0)then
	  line = 'Unrecognized polarization mnemonic: '//mnemo
	  call bug('f',line)
	endif
	PolsP2C = codes(j)
	end
