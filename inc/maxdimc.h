/*
	-------------------------------------------------------------
	maxdimc.h - include file for C code containing MIRIAD-wide
		    parameters

	MAXDIM .... maximum number of elements in any one plane
		    (ie, maximum dimensionality of a map)
	MAXANT .... maximum number of antennae
	MAXBASE ... maximum number of baselines
	MAXCHAN ... maximum number of channels in spectral data

	History:

	04aug91 mjs   Original version.
	05aug91 mjs   Put parentheses around MAXBASE defined value.
        bpw  20jul91  Created as xyzio.h
        mjs  08apr92  Minor mod to compile on VAX
        rjs  23feb93  Merged maxdimc.h and xyzio.h. Include MAXNAX.
	rjs   9sep94  Add MAXWIN
	-------------------------------------------------------------
*/
#define		MAXDIM		512
#define		MAXANT		30
#define		MAXBASE		((MAXANT * (MAXANT + 1)) / 2)
#define		MAXCHAN		512
#define		MAXNAX		7
#define		MAXWIN		16
#define		MAXWIDE		18
#define		MAXBUF		4194304
