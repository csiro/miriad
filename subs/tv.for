c************************************************************************
c
c  Software to drive various TV devices and servers.
c
c  History:
c    rjs Dark_ages Created
c    rjs  9jun89   Improved Ultra support. Added SSS reset function.
c    rjs 26jun89   More changes for the Ultra frame buffer.
c    rjs 18jul89   Fixed up IVAS setup after a reset.
c    rjs 20sep89   Improved TVLUT, support for FILE and VFILE,
c		   TVFLUSH works for IVSERVE and SSS.
c    rjs 10oct89   Replaced pack16,unpack16 with packi2,unpacki2.
c     jm 07may90   Fixed two bugs in TVVIEW for device SSS.
c    rjs 10sep90   Added support for SSS to TVLUT.
c    mjs 21nov90   Added support for MSSS (mostly, copied from SSS, the
c                  main difference being the port number assignment)
c     jm 17jan91   Added tvzoom, tvtext, tveras, tvselpt, and tvwind.
c     jm 29jan91   Added tvscrl, tvtopx, pxtotv, and modified tvzoom.
c                  This involved adding another common block to tv.h
c                  and adding a few setup lines in TvOpen.
c     jm  2feb91   Initialized button=0 in TvCursor as doc implied.
c    rjs 10apr91   Added SSS RZscr function.
c    mjs 11apr91   Removed ultra/cray and raster/fx support
c                  (no longer available).
c     jm 16apr91   Replaced ultra/cray and raster/fx support but
c                  added additional ifdef's to insulate from devices
c                  that do not support these libraries.  To insert
c                  either ultra or raster, include "-D ULTRA" or
c                  "-D RASTER" on the command line to RATTY.
c     jm 20apr91   Added mxas driver code.
c     jm 24apr91   Changed TvZoom/TvScrl/Tvtopx/Pxtotv to take advantage
c                  of routine TvRZscr.  This allows removal of all but
c                  three items in the 29jan91 common block in tv.h
c    rjs  7jul91   Fixed multiple bugs in Sun version of TvView
c     jm 20sep91   Added stupid TvSelect subroutine to determine if
c                  the routine TvSelpt may be called.
c     jm 25apr92   Modified XAS calls for new i/o structure.
c    rjs 12jun92   Repaired tvreset for other than XAS and SSS.
c     jm 12jun92   Removed rjs-fix to tvreset and fixed TvRZscr function
c                  by adding common variables LastMag, LastX, and LastY.
c                  These are set by TvOpen, only changed by TvView, and
c                  read (only if needed) by TvRZscr.  Also, added a
c                  range check to the zoom value in TvZoom.
c     jm 15jun92   Modified TVchar for XAS to remove redundant server
c                  calls.  The four parameters requested never change,
c                  so one call is sufficient to set the parameters.
c                  Successive calls retrieve local saved values.
c                  Also fixed a bug in TVwrtofm that I introduced when
c                  I merged the SSS call into this once XAS-only routine.
c     jm 04jul92   Added tvscale subroutine.
c     jm 15jul92   Added tvgraph and tvvect subroutines.  Also modified
c                  the tvselpt subroutine to return the mouse button
c                  number used to perform the selection.  Fixed tvscale
c                  so that range is always 0<=int<=0xFFFF.  Finally,
c                  added tvopen option to handle server name xmtv.
c     jm 18jul92   Modified TvZoom so minimum value is 1 not 0.  Also
c                  modified TvZoom and TvScrl to read current values from
c                  the server rather than rely on internal variables.
c    rjs 23jul92   Fixed bug in tvscale when determining the scale
c                  factors for negative numbers.
c     jm 31oct92   Added port number option to device open name.  Also
c                  removed references to obsolete mxas server and added
c                  a message to user.  Also added warning to tvlocal for
c                  devices which do not support this option.
c************************************************************************
c* TvOpen -- Open an image display device.
c& jm
c: tv,display
c+
	subroutine tvopen(device)
c
	implicit none
	character device*(*)
c
c  Open an image display device. See TvChar for a description of the
c  Tv model.
c
c  Input:
c    device	Name of device. This is of the form:
c		  type@name
c		or
c		  type:port@name
c		Here "name" is the physical device name, or the
c		name of the server (for network display servers). If
c               the server type permits alternate port numbers, they
c               may be specified using the second input form.  "Type"
c		is the device or server type. Legitimate values are:
c		  'ivas'	IVAS server (on VMS only)
c		  'sss'		Sun screen server.
c		  'xmtv'	X-window screen server with buttons.
c		  'ivserve'	Ivas server.
c		  'file'	Dump to a file (UNICOS only).
c		  'vfile'	Dump to file - variable size (UNICOS only).
c		  'ultra'	Ultra frame buffer (on UNICOS only).
c		  'raster'	Rastertech device (on FX only).
c		Examples:
c		  ivas@ixa0:      An IVAS with device name IXA0:
c		  ivserve@castor  An IVAS server on machine castor.
c		  xmtv@colo       A XMTV server on machine colo.
c		  xmtv:5001@astro A XMTV server on machine astro
c                                 communicating via port number 5001.
c
c--
c------------------------------------------------------------------------
	include 'tv.h'
	integer i,length
	integer port
	character name*32,type*32
	logical okay
#ifdef vms
	logical sys$alloc
#endif
c
c  Externals.
c
	integer len1
c
c  Determine the name and type of the display device.
c
	length = len1(device)
	i = index(device,'@')
	if(i.le.1.or.i.ge.length)
     *	  call bug('f','TV device names must be of the form type@name')
	name = device(i+1:length)
	type = device(1:i-1)
	call Lcase(type)
	length = length - i
c
	okay = .FALSE.
	i = index(type,':')
	if(i.gt.1.and.i.lt.len1(type)) then
	  call atoif(type(i+1:),port,okay)
	  if(.not.okay)
     *	    call bug('f','TV device port number incorrectly formatted.')
	  type = device(1:i-1)
	endif
c
c  Determine the appropriate type, and initialise it.
c
c  MIRIAD's version of Sun Screen Server.
c  ======================================
c
	if((type.eq.'sss').or.(type.eq.'msss'))then
	  protocol = Sss
	  call tvconn(handle,Sssport,name(1:length))
	  buffer(1) = SssOpen
	  do i=2,6
	    buffer(i) = 0
	  enddo
	  BufLen = 6
	  Nack = 1
	  MxZoom = -15
c
c  MIRIAD's Athena widget X-window Screen Server.
c  ===============================================
c
	else if(type.eq.'xmtv')then
	  protocol = Xas
	  if (.not.okay) port = Xasport
	  call tvconn(handle,port,name(1:length))
	  buffer(1) = XasOpen
	  do i=2,6
	    buffer(i) = 0
	  enddo
	  BufLen = 6
	  Nack = 1
c
c  MIRIAD's version of the X-window Screen Server.
c  ===============================================
c
	else if((type.eq.'xas').or.(type.eq.'mxas'))then
	  call bug('f','MXAS is no longer supported; use XMTV instead.')
c	  protocol = Xas
c	  call tvconn(handle,Xasport,name(1:length))
c	  buffer(1) = XasOpen
c	  do i=2,6
c	    buffer(i) = 0
c	  enddo
c	  BufLen = 6
c	  Nack = 1
c
c  IVAS server.
c  ============
c
	else if(type.eq.'ivserve')then
	  protocol = Ivserve
	  call tvconn(handle,Ivport,name(1:length))
	  buffer(2) = IvGPHset
	  buffer(3) = 0
	  buffer(4) = 0
	  buffer(5) = 1024
	  buffer(6) = 1024
	  buffer(7) = 0
	  BufLen = 7
	  Nack = 1
	  MxZoom = -15
c
c  IVAS.
c  =====
#ifdef vms
	else if(type.eq.'ivas')then
	  protocol = Ivas
	  call ucase(name(1:length))
	  if(.not.sys$alloc(name(1:length),,,,))
     *	    call bug('f','Failed to allocate the IVAS')
	  call fivasopen(handle,name(1:length),length)
	  call fivasGPHset(0,0,1024,1024,0)
	  MxZoom = -15
#endif
c
c  Ultra frame buffer.
c  ===================
c
#ifdef cft
#ifdef ULTRA
	else if(type.eq.'ultra')then
	  protocol = Ultra
	  call Uinit(name(1:length))
	  MxZoom = -15
#endif
#endif
c
c  Dump to file.
c  =============
c
#ifdef cft
	else if(type.eq.'file'.or.type.eq.'vfile')then
	  protocol = File
	  call TVFinit(name(1:length),type.eq.'file')
	  MxZoom = -15
#endif
c
c  RasterTech frame buffer.
c  ========================
c
#ifdef FX
#ifdef RASTER
	else if(type.eq.'raster')then
	  protocol = Raster
	  call RSopen(name(1:length))
	  MxZoom = -15
#endif
#endif
c
c  None of the known devices.
c  ==========================
c
	else
	  call bug('f','Unrecognised TV device type')
	endif
c
	TvScrx = 0
	TvScry = 0
	LastMag = 1
	LastX = 0
	LastY = 0
	end
c************************************************************************
c* TvLocal -- Put the display device into an interactive loop with the user.
c& jm
c: tv,display
c+
	subroutine tvlocal
c
	implicit none
c
c  Allow the TV to do its own thing. This allows the user to interact
c  directly with the TV in some device-dependent fashion.
c--
c------------------------------------------------------------------------
	include 'tv.h'
	if(protocol.eq.Ivserve)then
	  call TVcheck(1)
	  buffer(BufLen+1) = IvLocal
	  BufLen = BufLen + 1
	  call TVread(1)
#ifdef vms
	else if(protocol.eq.Ivas)then
	  call IvFiddle
#endif
#ifdef cft
#ifdef ULTRA
	else if(protocol.eq.Ultra)then
	  call Ulocal
#endif
#endif
	else
	  call bug('w','TVLOCAL not supported on this device!')
	endif
	end
c************************************************************************
c* TvLut -- Load a image display device's lookup table.
c& jm
c: tv,display
c+
	subroutine TVLut(table)
c
	implicit none
	character table*(*)
c
c  This allows the caller to load a particular colour lookup table into
c  the image display device.
c
c  Inputs:
c    table	The name of the table to load. Valid values are:
c		  'B&W'
c		  'Colour'
c		  'Rainbow'
c------------------------------------------------------------------------
	include 'tv.h'
c
c  Data for various ofms. ofm contains the colour table to be loaded.
c    ofm	Colour table data.
c		  ofm(*,1) - red for colour ofm.
c		  ofm(*,2) - green for colour ofm.
c		  ofm(*,3) - blue for colour ofm.
c		  ofm(*,4) - red, green and blue for a black-white ofm.
c		  ofm(*,5) - red for Tody's table.
c		  ofm(*,6) - green for Tody's table.
c		  ofm(*,7) - blue for Tody's table.
c
	integer ofm(256,10)
	integer i,red,green,blue
c
c  Red.
c
	data (ofm(i,1),i=  1,128)/128*0/
	data (ofm(i,1),i=129,256)/
     *	   0,  54,  64,  71,  76,  81,  86,  89,
     *	  93,  96,  99, 101, 104, 107, 109, 111,
     *	 114, 116, 118, 119, 121, 123, 125, 127,
     *	 129, 131, 132, 134, 136, 138, 140, 141,
     *	 143, 144, 145, 147, 149, 150, 151, 153,
     *	 155, 156, 157, 158, 160, 162, 163, 164,
     *	 166, 167, 168, 170, 171, 172, 174, 175,
     *	 176, 178, 179, 179, 181, 182, 183, 185,
     *	 186, 187, 188, 189, 191, 192, 193, 194,
     *	 195, 197, 198, 198, 200, 201, 203, 203,
     *	 205, 206, 207, 208, 209, 211, 211, 213,
     *	 214, 214, 216, 217, 218, 219, 220, 221,
     *	 223, 223, 224, 226, 226, 228, 228, 230,
     *	 231, 232, 233, 234, 235, 236, 237, 239,
     *	 239, 241, 241, 243, 243, 245, 245, 247,
     *	 247, 249, 249, 251, 251, 253, 253, 255/
c
c  Green.
c
	data (ofm(i,2),i=  1,128)/
     *	   0,   4,   7,  10,  13,  15,  18,  20,
     *	  22,  24,  26,  28,  30,  32,  34,  36,
     *	  38,  39,  41,  43,  44,  46,  48,  49,
     *	  51,  53,  54,  56,  57,  59,  61,  62,
     *	  64,  65,  66,  68,  69,  71,  72,  74,
     *	  75,  76,  78,  79,  81,  82,  84,  85,
     *	  86,  88,  89,  90,  92,  93,  94,  96,
     *	  97,  98, 100, 101, 102, 103, 105, 106,
     *	 107, 109, 109, 111, 112, 113, 115, 116,
     *	 117, 118, 120, 120, 122, 123, 124, 125,
     *	 127, 128, 129, 130, 132, 133, 134, 135,
     *	 136, 137, 139, 140, 141, 142, 143, 144,
     *	 146, 146, 148, 149, 150, 151, 152, 153,
     *	 155, 155, 157, 158, 159, 160, 161, 162,
     *	 163, 165, 165, 167, 167, 169, 170, 171,
     *	 172, 173, 174, 175, 176, 177, 178, 179/
	data (ofm(i,2),i=129,256)/
     *	 180, 180, 181, 181, 182, 181, 182, 182,
     *	 183, 183, 183, 183, 184, 184, 185, 184,
     *	 185, 185, 185, 186, 185, 186, 186, 186,
     *	 186, 186, 187, 187, 187, 187, 187, 187,
     *	 187, 187, 188, 187, 188, 188, 187, 188,
     *	 188, 188, 188, 188, 188, 188, 188, 188,
     *	 187, 188, 187, 187, 187, 187, 187, 187,
     *	 186, 187, 187, 186, 186, 185, 186, 186,
     *	 185, 185, 184, 184, 184, 184, 184, 183,
     *	 183, 182, 182, 182, 181, 181, 180, 180,
     *	 179, 179, 178, 178, 176, 176, 175, 175,
     *	 174, 173, 172, 172, 171, 170, 169, 169,
     *	 167, 167, 165, 165, 163, 162, 161, 159,
     *	 158, 156, 155, 153, 151, 150, 148, 147,
     *	 144, 142, 140, 137, 134, 132, 128, 125,
     *	 121, 117, 111, 106,  99,  89,  75,   0/
c
c  Blue.
c
	data (ofm(i,3),i=  1,128)/
     *	   0,  15,  22,  27,  31,  35,  38,  41,
     *	  44,  46,  49,  51,  54,  55,  58,  59,
     *	  61,  63,  65,  66,  68,  70,  71,  72,
     *	  74,  75,  76,  78,  79,  80,  81,  82,
     *	  83,  84,  86,  87,  88,  89,  89,  91,
     *	  91,  92,  93,  94,  95,  96,  96,  97,
     *	  98,  99,  99, 100, 100, 101, 102, 102,
     *	 103, 104, 104, 104, 105, 105, 106, 106,
     *	 107, 107, 107, 108, 108, 109, 109, 109,
     *	 109, 110, 110, 110, 110, 111, 111, 111,
     *	 111, 111, 111, 111, 111, 111, 111, 111,
     *	 111, 111, 111, 111, 111, 110, 110, 110,
     *	 109, 109, 109, 109, 108, 107, 107, 106,
     *	 106, 105, 104, 103, 102, 101, 101, 100,
     *	  98,  97,  96,  94,  93,  91,  89,  87,
     *	  84,  81,  78,  74,  69,  63,  53,   0/
	data (ofm(i,3),i=129,256)/128*0/
c
c  Black and white.
c
	data (ofm(i,4),i=  1,128)/
     *	   0,   1,   2,   3,   4,   5,   6,   7,
     *	   8,   9,  10,  11,  12,  13,  14,  15,
     *	  16,  17,  18,  19,  20,  21,  22,  23,
     *	  24,  25,  26,  27,  28,  29,  30,  31,
     *	  32,  33,  34,  35,  36,  37,  38,  39,
     *	  40,  41,  42,  43,  44,  45,  46,  47,
     *	  48,  49,  50,  51,  52,  53,  54,  55,
     *	  56,  57,  58,  59,  60,  61,  62,  63,
     *	  64,  65,  66,  67,  68,  69,  70,  71,
     *	  72,  73,  74,  75,  76,  77,  78,  79,
     *	  80,  81,  82,  83,  84,  85,  86,  87,
     *	  88,  89,  90,  91,  92,  93,  94,  95,
     *	  96,  97,  98,  99, 100, 101, 102, 103,
     *	 104, 105, 106, 107, 108, 109, 110, 111,
     *	 112, 113, 114, 115, 116, 117, 118, 119,
     *	 120, 121, 122, 123, 124, 125, 126, 127/
	data (ofm(i,4),i=129,256)/
     *	 128, 129, 130, 131, 132, 133, 134, 135,
     *	 136, 137, 138, 139, 140, 141, 142, 143,
     *	 144, 145, 146, 147, 148, 149, 150, 151,
     *	 152, 153, 154, 155, 156, 157, 158, 159,
     *	 160, 161, 162, 163, 164, 165, 166, 167,
     *	 168, 169, 170, 171, 172, 173, 174, 175,
     *	 176, 177, 178, 179, 180, 181, 182, 183,
     *	 184, 185, 186, 187, 188, 189, 190, 191,
     *	 192, 193, 194, 195, 196, 197, 198, 199,
     *	 200, 201, 202, 203, 204, 205, 206, 207,
     *	 208, 209, 210, 211, 212, 213, 214, 215,
     *	 216, 217, 218, 219, 220, 221, 222, 223,
     *	 224, 225, 226, 227, 228, 229, 230, 231,
     *	 232, 233, 234, 235, 236, 237, 238, 239,
     *	 240, 241, 242, 243, 244, 245, 246, 247,
     *	 248, 249, 250, 251, 252, 253, 254, 255/
c
c Doug Tody's "linear pseudo-colour" table.
c IRAF Red table.
c
	data (ofm(i,5),i=1,127)/127*0/
	data (ofm(i,5),i=128,168)/	      6,
     *	  12,  18,  24,  30,  36,  42,	48,  54,
     *	  60,  66,  72,  78,  85,  91,	97, 103,
     *	 109, 115, 121, 127, 133, 139, 145, 151,
     *	 157, 163, 170, 176, 182, 188, 194, 200,
     *	 206, 212, 218, 224, 230, 236, 242, 248/
	data (ofm(i,5),i=169,256)/88*255/
c
c IRAF Green table.
c
	data (ofm(i,6),i=  1,40)/40*0/
	data (ofm(i,6),i=41,88)/
     *	   0,	0,   0,   6,  12,  18,	24,  30,
     *	  36,  42,  48,  54,  60,  66,	72,  78,
     *	  85,  91,  97, 103, 109, 115, 121, 127,
     *	 133, 139, 145, 151, 157, 163, 170, 176,
     *	 182, 188, 194, 200, 206, 212, 218, 224,
     *	 230, 236, 242, 248, 255, 255, 255, 255/
	data (ofm(i,6),i=89,168)/80*255/
	data (ofm(i,6),i=169,256)/
     *	 255, 248, 242, 236, 230, 224, 218, 212,
     *	 206, 200, 194, 188, 182, 176, 170, 163,
     *	 157, 151, 145, 139, 133, 127, 121, 115,
     *	 109, 103,  97,  91,  85,  78,	72,  66,
     *	  60,  54,  48,  42,  36,  30,	24,  18,
     *	  12,	6,   0,   6,  12,  18,	24,  30,
     *	  36,  42,  48,  54,  60,  66,	72,  78,
     *	  85,  91,  97, 103, 109, 115, 121, 127,
     *	 133, 139, 145, 151, 157, 163, 170, 176,
     *	 182, 188, 194, 200, 206, 212, 218, 224,
     *	 230, 236, 242, 248, 255, 255, 255, 255/
c
c IRAF Blue table.
c
	data (ofm(i,7),i=  1,48)/
     *	   0,	6,  12,  18,  24,  30,	36,  42,
     *	  48,  54,  60,  66,  72,  78,	85,  91,
     *	  97, 103, 109, 115, 121, 127, 133, 139,
     *	 145, 151, 157, 163, 170, 176, 182, 188,
     *	 194, 200, 206, 212, 218, 224, 230, 236,
     *	 242, 248, 255, 255, 255, 255, 255, 255/
	data (ofm(i,7),i=49,88)/40*255/
	data (ofm(i,7),i=89,128)/
     *	 230, 224, 218, 212, 206, 200, 194, 188,
     *	 182, 176, 170, 163, 157, 151, 145, 139,
     *	 133, 127, 121, 115, 109, 103,	97,  91,
     *	  85,  78,  72,  66,  60,  54,	48,  42,
     *	  36,  30,  24,  18,  12,   6,	 0,   0/
	data (ofm(i,7),i=129,208)/80*0/
	data (ofm(i,7),i=209,256)/
     *	   0,	0,   0,   6,  12,  18,	24,  30,
     *	  36,  42,  48,  54,  60,  66,	72,  78,
     *	  85,  91,  97, 103, 109, 115, 121, 127,
     *	 133, 139, 145, 151, 157, 163, 170, 176,
     *	 182, 188, 194, 200, 206, 212, 218, 224,
     *	 230, 236, 242, 248, 255, 255, 255, 255/
c
	if(table.eq.'B&W')then
	    red = 4
	    green = 4
	    blue = 4
	else if(table.eq.'colour')then
	  red = 1
	  blue = 2
	  green = 3
	else if(table.eq.'rainbow')then
	  red = 5
	  blue = 6
	  green = 7
	endif
c
	if(protocol.eq.Sss)then
          call TVwrtofm(1,ofm(1,red))
          call TVwrtofm(2,ofm(1,blue))
          call TVwrtofm(3,ofm(1,green))
	else if(protocol.eq.Xas)then
          call TVwrtofm(1,ofm(1,red))
          call TVwrtofm(2,ofm(1,blue))
          call TVwrtofm(3,ofm(1,green))
#ifdef cft
#ifdef ULTRA
	else if(protocol.eq.Ultra)then
	  call uofm(ofm(1,red),ofm(1,blue),ofm(1,green))
#endif
	else if(protocol.eq.File)then
	  call tvfofm(ofm(1,red),ofm(1,blue),ofm(1,green))
#endif
	endif
	end
c************************************************************************
        subroutine TVwrtofm(n,table)
c
        implicit none
        integer n,table(256)
c
c  Private routine.
c  This sets up a colour table for the Sun and Xas screen servers.
c------------------------------------------------------------------------
        include 'tv.h'
        integer i,array(256)
        integer opcode
        integer xmax,ymax,channels,levels
c
        if((protocol.eq.Sss).or.(protocol.eq.Xas))then
          if(protocol.eq.Sss)opcode = SssWLut
          if(protocol.eq.Xas)opcode = XasWLut
c
          call TVchar(xmax,ymax,channels,levels)
          levels = max(64, min(256, levels))
          ymax = 256 / levels
          do i=1,levels
            array(i) = table(ymax*(i-1)+1)
          enddo
c
          call TVcheck((2*6)+levels)
          do i=1,channels
            buffer(BufLen+1) = opcode
            buffer(BufLen+2) = 0
            buffer(BufLen+3) = 0
            buffer(BufLen+4) = 0
            buffer(BufLen+1+n) = 1
            buffer(BufLen+5) = i
            buffer(BufLen+6) = levels
            BufLen = BufLen + 6
            call tvpack(array,levels)
          enddo
        endif
        end
c************************************************************************
c* TvFlush -- Flush data to the display device.
c& jm
c: tv,display
c+
	subroutine TVflush
c
	implicit none
c
c  TvFlush causes any data that has been buffered up, to be flushed to the
c  display device. TvFlush then waits for completion of the transfer.
c
c--
c------------------------------------------------------------------------
	include 'tv.h'
	if(protocol.eq.Ivserve.or.protocol.eq.Sss)then
	  call TVcheck(BufSize)
	else if(protocol.eq.Xas)then
	  call TVcheck(BufSize)
#ifdef cft
	else if(protocol.eq.File)then
	  call TVFflush
#ifdef ULTRA
	else if(protocol.eq.Ultra)then
	  call Uflush
#endif
#endif
	endif
	end
c************************************************************************
c* TvChar -- Get characteristics of the display device.
c& jm
c: tv,display
c+
	subroutine TVchar(xmax,ymax,channels,levels)
c
	implicit none
	integer xmax,ymax,channels,levels
c
c  This returns characteristics about the device we are using.
c
c  The display device is assumed to be ``xmax'' by ``ymax'' pixels
c  by ``channels'' images deep. Channels are number from 1 upwards
c  to ``channels''.  The pixel coordinate system has pixel (1,1)
c  in the lower left corner of the screen, and pixel (xmax,ymax)
c  in the top right corner.
c
c  Output:
c    xmax,ymax	Screen size in pixels.
c    channels	Number of image channels.
c    levels	Number of grey scale levels.
c--
c------------------------------------------------------------------------
	include 'tv.h'
c
        integer firstime,sxmax,symax,schan,slevl
        save firstime,sxmax,symax,schan,slevl
        data firstime /1/
c
	if(protocol.eq.Ivas.or.protocol.eq.Ivserve)then
	  xmax = 1024
	  ymax = 1024
	  channels = 3
	  levels = 256
	else if(protocol.eq.Sss)then
	  xmax = 1142
	  ymax = 844
	  channels = 2
	  levels = 64
	else if(protocol.eq.Xas)then
	  if(firstime.eq.1) then
	    call TVcheck(6)
	    buffer(BufLen+1) = XasIntgt
	    buffer(BufLen+2) = 0
	    buffer(BufLen+3) = 0
	    buffer(BufLen+4) = 0
	    buffer(BufLen+5) = 0
	    buffer(BufLen+6) = 0
	    BufLen = BufLen + 6
	    Nack = Nack + 1
	    call TVread(29)
	    sxmax = buffer(4)
	    symax = buffer(5)
	    schan = buffer(1)
	    slevl = buffer(6) + 1
	    MxZoom = buffer(12)
	  endif
	  xmax = sxmax
	  ymax = symax
	  channels = schan
	  levels = slevl
#ifdef FX
#ifdef RASTER
	else if(protocol.eq.Raster)then
	  call RSchar(xmax,ymax,channels,levels)
#endif
#endif
#ifdef cft
	else if(protocol.eq.File)then
	  xmax = 1024
	  ymax = 1024
	  channels = 1
	  levels = 256
#ifdef ULTRA
	else if(protocol.eq.Ultra)then
	  call Uchar(xmax,ymax,channels,levels)
#endif
#endif
	endif
c
        if(protocol.eq.Xas)then
          firstime = 0
        else
          firstime = 1
        endif
	end
c************************************************************************
c* TvClose -- Close the display device.
c& jm
c: tv,display
c+
	subroutine tvclose
	implicit none
c
c  TvClose closes up the image display device, flushing any buffers before
c  it finishes.
c--
c------------------------------------------------------------------------
	include 'tv.h'
c
c  Finish up with the server. Flush buffers and close up.
c
	if(protocol.eq.Ivserve)then
	  call TVcheck(1)
	  buffer(BufLen+1) = IvClose
	  BufLen = BufLen + 1
	  Nack = 0
	  call TVcheck(BufSize)
	  call tcpclose(handle)
	else if(protocol.eq.Sss)then
	  call TVcheck(BufSize)
	  call tcpclose(handle)
	else if(protocol.eq.Xas)then
	  call TVcheck(6)
	  buffer(BufLen+1) = XasClose
	  buffer(BufLen+2) = 0
	  buffer(BufLen+3) = 0
	  buffer(BufLen+4) = 0
	  buffer(BufLen+5) = 0
	  buffer(BufLen+6) = 0
	  BufLen = BufLen + 6
	  call TVcheck(BufSize)
	  call tcpclose(handle)
#ifdef vms
	else if(protocol.eq.Ivas)then
	  call fivasclose(handle)
#endif
#ifdef cft
	else if(protocol.eq.File)then
	  call TVFclose
#ifdef ULTRA
	else if(protocol.eq.Ultra)then
	  call Ufin
#endif
#endif
#ifdef FX
#ifdef RASTER
	else if(protocol.eq.Raster)then
	  call RSclose
#endif
#endif
	endif
	end
c************************************************************************
c* TvChan -- Display a given image channel.
c& jm
c: tv,display
c+
	subroutine TVchan(channel)
c
	implicit none
	integer channel
c
c  TvChan causes a particular image channel to appear on the display
c  screen. Channels are numbers from 1 to NCHAN (where NCHAN is the
c  maximum number of channels that the display possesses.
c
c  Input:
c    channel	Channel to turn on.
c--
c------------------------------------------------------------------------
	include 'tv.h'
	if(protocol.eq.Ivserve)then
	  call TVcheck(5)
	  buffer(BufLen+1) = IvVPsetU
	  buffer(BufLen+2) = channel - 1
	  buffer(BufLen+3) = channel - 1
	  buffer(BufLen+4) = channel - 1
	  buffer(BufLen+5) = 23
	  BufLen = BufLen + 5
	else if(protocol.eq.Sss)then
	  call TVcheck(6)
	  buffer(BufLen+1) = SssSplit
	  buffer(BufLen+2) = channel
	  buffer(BufLen+3) = channel
	  buffer(BufLen+4) = channel
	  buffer(BufLen+5) = channel
	  buffer(BufLen+6) = 0
	  BufLen = BufLen + 6
	  Nack = Nack + 1
	else if(protocol.eq.Xas)then
	  call TVcheck(6)
	  buffer(BufLen+1) = XasSplit
	  buffer(BufLen+2) = channel
	  buffer(BufLen+3) = channel
	  buffer(BufLen+4) = channel
	  buffer(BufLen+5) = channel
	  buffer(BufLen+6) = 0
	  BufLen = BufLen + 6
#ifdef vms
	else if(protocol.eq.Ivas)then
	  call fivasVPsetup(channel-1,channel-1,channel-1,23)
#endif
	endif
	end
c************************************************************************
c* TvRZScr -- Reads the current channel, zoom, and scroll registers.
c& jm
c: tv,display
c+
	subroutine TvRZScr(chan,mag,xscr,yscr)
c
	implicit none
	integer chan,mag,xscr,yscr
c
c  Read TV registers.
c
c  Output:
c    chan	The current channel being displayed.  If the current
c               device is not capable of returning these values, chan
c               is set to -1.
c    mag	The current magnification.
c    xscr	X scroll register (upper left relative).
c    yscr	Y scroll register (upper left relative).
c--
c------------------------------------------------------------------------
	include 'tv.h'
c
	chan = -1
	if(protocol.eq.Sss)then
	  call TVcheck(6)
	  buffer(BufLen+1) = SssRZScr
	  buffer(BufLen+2) = 0
	  buffer(BufLen+3) = 0
	  buffer(BufLen+4) = 0
	  buffer(BufLen+5) = 0
	  buffer(BufLen+6) = 0
	  BufLen = BufLen + 6
	  Nack = Nack + 1
	  call TVread(4)
	  chan = buffer(1)
	  mag  = buffer(2)
	  xscr = buffer(3)
	  yscr = buffer(4)
	else if(protocol.eq.Xas)then
	  call TVcheck(6)
	  buffer(BufLen+1) = XasRZScr
	  buffer(BufLen+2) = 0
	  buffer(BufLen+3) = 0
	  buffer(BufLen+4) = 0
	  buffer(BufLen+5) = 0
	  buffer(BufLen+6) = 0
	  BufLen = BufLen + 6
	  Nack = Nack + 1
	  call TVread(4)
	  chan = buffer(1)
	  mag  = buffer(2)
	  xscr = buffer(3)
	  yscr = buffer(4)
	else
	  mag  = LastMag
	  xscr = LastX
	  yscr = LastY
	endif
	end
c************************************************************************
c* TvCursor -- Read the location of the image display device's cursor.
c& jm
c: tv,display
c+
	subroutine TVcursor(x,y,button)
c
	implicit none
	integer x,y,button
c
c  Read the current cursor/track ball position, and return the number of
c  the last button pressed.
c
c  Output:
c    x,y	Cursor screen position (not image memory position).
c    button	The number of the last button pressed, This varies from
c		1 upwards. A value of zero indicates no button has been
c		pushed.
c--
c------------------------------------------------------------------------
	include 'tv.h'
c
	button = 0
	if(protocol.eq.Ivserve)then
	  call TVcheck(3)
	  buffer(BufLen+1) = IvMoStat
	  buffer(BufLen+2) = 7
	  buffer(BufLen+3) = 0
	  BufLen = BufLen + 3
	  call TVread(3)
	  button = buffer(1)
	  x = buffer(2)
	  y = buffer(3)
	  y = 1023 - y
c
	else if(protocol.eq.Sss)then
	  call TVcheck(12)
	  buffer(BufLen+1) = SssRCurs
	  buffer(BufLen+2) = 0
	  buffer(BufLen+3) = 0
	  buffer(BufLen+4) = 0
	  buffer(BufLen+5) = 0
	  buffer(BufLen+6) = 0
	  buffer(BufLen+7) = SssRButt
	  buffer(BufLen+8) = 0
	  buffer(BufLen+9) = 0
	  buffer(BufLen+10) = 0
	  buffer(BufLen+11) = 0
	  buffer(BufLen+12) = 0
	  BufLen = BufLen + 12
	  Nack = Nack + 1
	  call TVread(8)
	  x = buffer(1)
	  y = buffer(2)
	  if(buffer(5).gt.0) button = 1
	  if(buffer(6).gt.0) button = 2
	  if(buffer(7).gt.0) button = 3
	  if(buffer(8).gt.0) button = 4
	else if(protocol.eq.Xas)then
	  call TVcheck(6)
	  buffer(BufLen+1) = XasRCurb
	  buffer(BufLen+2) = 0
	  buffer(BufLen+3) = 0
	  buffer(BufLen+4) = 0
	  buffer(BufLen+5) = 0
	  buffer(BufLen+6) = 0
	  BufLen = BufLen + 6
	  Nack = Nack + 1
	  call TVread(6)
	  x = buffer(1)
	  y = buffer(2)
	  if(buffer(3).gt.0) button = 1
	  if(buffer(4).gt.0) button = 2
	  if(buffer(5).gt.0) button = 3
	  if(buffer(6).gt.0) button = 4
#ifdef vms
	else if(protocol.eq.Ivas)then
	  call fivasMOUSEstatus(button,x,y,7,0)
	  y = 1023 - y
#endif
#ifdef cft
#ifdef ULTRA
	else if(protocol.eq.Ultra)then
	  call Ucursor(x,y,button)
#endif
#endif
#ifdef FX
#ifdef RASTER
	else if(protocol.eq.Raster)then
	  call rscursor(x,y,button)
#endif
#endif
	endif
	end
c************************************************************************
c* TvView -- Change the viewing window to a subportion of image memory.
c& jm
c: tv,display
c+
	subroutine TVview(xmin,ymin,xmax,ymax)
c
	implicit none
	integer xmin,ymin,xmax,ymax
c
c  View only a subportion of the screen.
c
c  Input:
c    xmin,ymin,xmax,ymax Portion of the screen to view.
c--
c------------------------------------------------------------------------
	include 'tv.h'
	integer nx,ny
	integer xpix,ypix,channels,levels
c
	if(protocol.eq.Ivserve)then
	  nx = xmax - xmin + 1
	  ny = ymax - ymin + 1
	  call TVcheck(6)
	  buffer(BufLen+1) = IvVPZScr
	  buffer(BufLen+2) = xmin
	  buffer(BufLen+3) = 1023-(ymin+ny-1)
	  buffer(BufLen+4) = 1024/nx
	  buffer(BufLen+5) = 1024/ny
	  buffer(BufLen+6) = 1
	  BufLen = BufLen + 6
	  LastMag = buffer(BufLen+4)
	  LastX = buffer(BufLen+2)
	  LastY = buffer(BufLen+3)
	else if(protocol.eq.Sss)then
	  nx = min(1142/(xmax-xmin+1),844/(ymax-ymin+1))
	  nx = max(nx,1)
	  call TVcheck(6)
	  buffer(BufLen+1) = SssWZscr
	  buffer(BufLen+2) = 0
	  buffer(BufLen+3) = nx
	  buffer(BufLen+4) = xmin - (1142 - nx*(xmax-xmin+1))/(2*nx)
	  buffer(BufLen+5) = 844 - ymax - (844-nx*(ymax-ymin+1))/(2*nx)
	  buffer(BufLen+6) = 0
	  BufLen = BufLen + 6
	else if(protocol.eq.Xas)then
	  call TVchar(xpix,ypix,channels,levels)
	  nx = min(xpix/(xmax-xmin+1),ypix/(ymax-ymin+1))
	  nx = max(nx,1)
	  call TVcheck(6)
	  buffer(BufLen+1) = XasWZscr
	  buffer(BufLen+2) = 0
	  buffer(BufLen+3) = nx
	  buffer(BufLen+4) = xmin - (xpix-nx*(xmax-xmin+1))/(2*nx)
	  buffer(BufLen+5) = ypix - ymax-(ypix-nx*(ymax-ymin+1))/(2*nx)
	  buffer(BufLen+6) = 0
	  BufLen = BufLen + 6
#ifdef vms
	else if(protocol.eq.Ivas)then
	  nx = xmax - xmin + 1
	  ny = ymax - ymin + 1
	  call fivasVPzoomScroll(xmin,1023-(ymin+ny-1),
     *		1024/nx,1024/ny,1)
	  LastMag = 1024/nx
	  LastX = xmin
	  LastY = 1023-(ymin+ny-1)
#endif
#ifdef cft
#ifdef ULTRA
	else if(protocol.eq.Ultra)then
	  call Uchar(xpix,ypix,channels,levels)
	  nx = min(xpix/(xmax-xmin+1),ypix/(ymax-ymin+1))
	  nx = max(nx,1)
	  LastMag = nx
	  LastX = xmin - (xpix-nx*(xmax-xmin+1))/(2*nx)
	  LastY = ypix - ymax-(ypix-nx*(ymax-ymin+1))/(2*nx)
c  Are LastMag, LastX, and LastY set correctly for Ultra?? [12jun92 jm]
	  call UView(xmin,ymin,xmax,ymax)
	  call UFlush
#endif
#endif
	endif
	end
c************************************************************************
c* TvZoom -- Zoom a region of an image on a TV device.
c& jm
c: TV, display
c+
      subroutine tvzoom(zoom, xc, yc)
      implicit none
      integer zoom, xc, yc
c
c  Input:
c    zoom    Integer zoom value.
c    xc      Integer position of the desired X center of expansion.
c    yc      Integer position of the desired Y center of expansion.
c
c            range for zoom is 1 (no zoom) to MAXZOOM (typically 16).
c            ranges for xc and yc are as follows:
c              (1 <= xc <= MAXTVX) and (1 <= yc <= MAXTVY) with
c              xc = 1 at the LHS and yc = 1 at the bottom of the TV
c              (MAXTVX and MAXTVY are found with TvChar).
c
c  Output:
c    None
c
c--
c------------------------------------------------------------------------
c
      include 'tv.h'
c
      integer blcx, blcy, trcx, trcy
      integer xpix, ypix, mxchan, lev
      integer mag, xscr, yscr
      real tmag
c
c  Get amount of scroll without zoom.
c
      xscr = TvScrx
      yscr = TvScry
      call TvChar(xpix, ypix, mxchan, lev)
c
c  Determine new zoom/scroll values.
c
      mag = zoom
      mag = min(max(mag, 1), abs(MxZoom))
      if (MxZoom .gt. 0) mag = 2 ** (zoom - 1)
      tmag = mag
      blcx = (xc - 1.0) * (tmag - 1.0) / tmag
      blcy = (ypix - yc) * (tmag - 1.0) / tmag
      blcx = blcx + xscr
      blcy = blcy + yscr
      if (blcx .lt. 0) blcx = blcx + xpix
      if (blcx .ge. xpix) blcx = blcx - xpix
      if (blcy .lt. 0) blcy = blcy + ypix
      if (blcy .ge. ypix) blcy = blcy - ypix
      trcx = blcx + (xpix / mag) - 1
      trcy = blcy + (ypix / mag) - 1
      call TvView(blcx, blcy, trcx, trcy)
      return
      end
c************************************************************************
c* TvScrl -- Scroll a display on a TV device.
c& jm
c: TV, display
c+
       subroutine tvscrl(scrolx, scroly)
       implicit none
       integer scrolx, scroly
c
c  Input:
c    scrolx  Amount of X scroll (>0 scroll to the right).
c    scroly  Amount of Y scroll (>0 scroll upwards).
c
c  Output:
c    None
c
c--
c------------------------------------------------------------------------
c
      include 'tv.h'
c
      integer iscx, iscy
      integer blcx, blcy, trcx, trcy
      integer xpix, ypix, mxchan, lev
      integer chan, mag, xscr, yscr
c
c  Get center of zoom without scroll.
c
      blcx = 0
      blcy = 0
      trcx = 0
      trcy = 0
      call TvWind(blcx, blcy, trcx, trcy)
      call TvChar(xpix, ypix, mxchan, lev)
      call TvRZScr(chan, mag, xscr, yscr)
      blcx = ((blcx - 1) + (trcx - 1) - 1) / 2
      blcy = ((ypix - blcy) + (ypix - trcy) - 1) / 2
      blcx = blcx - (blcx / mag)
      blcy = blcy - (blcy / mag)
c
c  Determine new zoom/scroll values.
c
      iscx = mod(((8 * xpix) - scrolx), xpix)
      iscy = mod(((8 * ypix) + scroly), ypix)
      blcx = blcx + iscx
      blcy = blcy + iscy
      if (blcx .lt. 0) blcx = blcx + xpix
      if (blcx .ge. xpix) blcx = blcx - xpix
      if (blcy .lt. 0) blcy = blcy + ypix
      if (blcy .ge. ypix) blcy = blcy - ypix
      trcx = blcx + (xpix / mag) - 1
      trcy = blcy + (ypix / mag) - 1
      call TvView(blcx, blcy, trcx, trcy)
      TvScrx = iscx
      TvScry = iscy
      return
      end
c***********************************************************************
c* TvtoPx -- Convert TV device position to image pixels.
c& jm
c: TV, display
c+
      subroutine tvtopx(xtv, ytv, pixx, pixy)
c
      implicit none
      integer xtv, ytv
      integer pixx, pixy
c
c  Convert TV device position to image pixels and take into account
c  zoom and scroll.  The input is usually the value returned by a
c  call to TvCursor and is converted so that the position can be
c  compared with the unzoomed, unpanned image.
c
c  Inputs:
c    xtv     I     Screen x position before zoom and scroll.
c    ytv     I     Screen y position before zoom and scroll.
c
c  Output:
c    pixx    I     Pixel x position after zoom and scroll.
c    pixy    I     Pixel y position after zoom and scroll.
c
c--
c-----------------------------------------------------------------------
c
      integer xpix, ypix, mxchn, lev
      integer chan, mag, xscr, yscr
      integer ix, iy
      real x, y, tmag
c
      call TvChar(xpix, ypix, mxchn, lev)
      call TvRZScr(chan, mag, xscr, yscr)
      tmag = mag
      ix = xtv - ((mag - 1) / 2)
      iy = ytv - ((mag - 1) / 2)
      x = ((ix - 1.0) / tmag) + 1.0
      y = ((iy - 1.0) / tmag) + 1.0
      y = y + (ypix - (ypix / tmag))
      x = mod((x + xscr - 1.0 + (10.0 * xpix)), real(xpix)) + 1.0
      y = mod((y - yscr - 1.0 + (10.0 * ypix)), real(ypix)) + 1.0
      pixx = x + 0.5
      pixy = y + 0.5
      return
      end
c***********************************************************************
c* PxtoTv -- Convert image pixels to TV device positions.
c& jm
c: TV, display
c+
      subroutine pxtotv(pixx, pixy, xtv, ytv)
c
      implicit none
      integer xtv, ytv
      integer pixx, pixy
c
c  Convert image pixels to TV device positions and take into account
c  zoom and scroll.  The input is usually the value of an image pixel
c  that is going to be put on a zoomed and panned image.
c
c  Inputs:
c    pixx    I     Pixel x position after zoom and scroll.
c    pixy    I     Pixel y position after zoom and scroll.
c
c  Output:
c    xtv     I     Screen x position before zoom and scroll.
c    ytv     I     Screen y position before zoom and scroll.
c
c--
c-----------------------------------------------------------------------
c
      integer xpix, ypix, mxchn, lev
      integer chan, mag, xscr, yscr
      integer ix, iy
      real x, y, tmag
c
      call TvChar(xpix, ypix, mxchn, lev)
      call TvRZScr(chan, mag, xscr, yscr)
      tmag = mag
      x = mod((pixx - xscr - 1.0 + (10.0 * xpix)), real(xpix)) + 1.0
      y = mod((pixy + yscr - 1.0 + (10.0 * ypix)), real(ypix)) + 1.0
      y = y - (ypix - (ypix / tmag))
      ix = ((x - 1.0) * tmag) + 1.0
      iy = ((y - 1.0) * tmag) + 1.0
      xtv = ix + ((mag - 1) / 2)
      ytv = iy + ((mag - 1) / 2)
      return
      end
c************************************************************************
c* TvEras -- Erase a channel on the image display device.
c& jm
c: tv,display
c+
        subroutine tveras(channel)
        implicit none
        integer channel
c
c  TvEras causes a particular image channel to be erased.
c  Channels are numbers from 1 to NCHAN (where NCHAN is the
c  maximum number of channels that the display possesses.
c  NOTE: If a device is unable to erase only one particular
c  channel, then it initializes all channels (via TvReset).
c
c  Input:
c    channel	Channel number to erase.
c
c--
c------------------------------------------------------------------------
        include 'tv.h'
c
        if(protocol.eq.Sss)then
          call TVcheck(6)
          buffer(BufLen+1) = SssClear
          buffer(BufLen+2) = channel
          buffer(BufLen+3) = 0
          buffer(BufLen+4) = 0
          buffer(BufLen+5) = 0
          buffer(BufLen+6) = 0
          BufLen = BufLen + 6
          Nack = Nack + 1
        else if(protocol.eq.Xas)then
          call TVcheck(6)
          buffer(BufLen+1) = XasClear
          buffer(BufLen+2) = channel
          buffer(BufLen+3) = 0
          buffer(BufLen+4) = 0
          buffer(BufLen+5) = 0
          buffer(BufLen+6) = 0
          BufLen = BufLen + 6
        else
	  call TvReset
        endif
        end
c************************************************************************
c* TvReset -- Reset the image display device.
c& jm
c: tv,display
c+
	subroutine TVreset
	implicit none
c
c  TVReset causes the image display device to be reset. The image memories
c  are cleared, the default lookup table loaded, pan and zoom set to
c  0, and the device generally put into its default state.
c--
c------------------------------------------------------------------------
	include 'tv.h'
c
	integer xpix, ypix, mxchn, lev
c
	if(protocol.eq.Ivserve)then
	  call TVcheck(8)
	  buffer(BufLen+1) = IvInit
	  buffer(BufLen+2) = IvGPHset
	  buffer(BufLen+3) = 0
	  buffer(BufLen+4) = 0
	  buffer(BufLen+5) = 1024
	  buffer(BufLen+6) = 1024
	  buffer(BufLen+7) = 0
	  BufLen = BufLen+7
	  Nack = 1
	else if(protocol.eq.Sss)then
	  call TVcheck(6)
	  buffer(BufLen+1) = SssClear
	  buffer(BufLen+2) = 0
	  buffer(BufLen+3) = 0
	  buffer(BufLen+4) = 0
	  buffer(BufLen+5) = 0
	  buffer(BufLen+6) = 0
	  BufLen = BufLen + 6
	  Nack = Nack + 1
	else if(protocol.eq.Xas)then
	  call TVcheck(6)
	  buffer(BufLen+1) = XasClear
	  buffer(BufLen+2) = 0
	  buffer(BufLen+3) = 0
	  buffer(BufLen+4) = 0
	  buffer(BufLen+5) = 0
	  buffer(BufLen+6) = 0
	  BufLen = BufLen + 6
#ifdef vms
	else if(protocol.eq.Ivas)then
	  call fivasINITall(0)
	  call fivasGPHset(0,0,1024,1024,0)
#endif
#ifdef cft
	else if(protocol.eq.File)then
	  call TVFreset
#ifdef ULTRA
	else if(protocol.eq.ultra)then
	  call UReset
#endif
#endif

#ifdef FX
#ifdef RASTER
	else if(protocol.eq.Raster)then
	  call RSreset
#endif
#endif
	endif
c
	xpix = 0
	ypix = 0
	call TVScrl(xpix, ypix)
	call TVChar(xpix, ypix, mxchn, lev)
	xpix = xpix / 2
	ypix = ypix / 2
	call TVZoom(1, xpix, ypix)
c
	end
c***********************************************************************
c* TvSelect -- Determine if this device has TvSelpt capabilities.
c& jm
c: tv,display
c+
      subroutine tvselect(yesno)
c
      implicit none
      integer yesno
c
c  TVSELECT determines if the current display device can use the
c  interactive point or range selection on a TV screen via the
c  MIRIAD subroutine TVSELPT.
c
c  Input:
c    (none)
c
c  Output:
c    yesno    Equal to 1 if TVSELPT may be called; 0 otherwise.
c
c--
c-----------------------------------------------------------------------
      include 'tv.h'
c
      yesno = 0
      if(protocol.eq.Sss)then
        yesno = 1
      else if(protocol.eq.Xas)then
        yesno = 1
      endif
c
      return
      end
c***********************************************************************
c* TvSelpt -- Interactive point/range selection on a display device.
c& jm
c: tv,display
c+
      subroutine tvselpt(channel, type, x1, y1, x2, y2, ch)
c
      implicit none
      integer channel, type, x1, y1, x2, y2, ch
c
c  TVSELPT allows interactive point or range selection on a TV screen.
c  If the device is capable of creating different types of selection
c  operations, then the input variable ``type'' sets that style.  If
c  necessary (see ``type''), four input positions must be given.  The
c  first position is registered when the left mouse button is pushed.
c  Moving the mouse from that position alters the drawing according
c  to ``type''.  When the left button is pushed again, the operation
c  ends and returns the first and last mouse positions.  The operation
c  is reset if the middle button is pushed before hitting the left
c  button a second time or the mouse is moved outside of the window
c  selected.  Selecting either the right button or hitting the STOP
c  key (L1 on a Sun) aborts the operation.
c
c  Input:
c    channel  Tv Channel to act on.
c    type     Style of selection operation (if used by the device):
c               If ``type'' = 0, then a point and click selects a point.
c               If ``type'' = 1, then a line from (x1,y1) to (x2,y2)
c                 is drawn.
c               If ``type'' = 2, then a box centered on (x1,y1) is
c                 drawn.
c               If ``type'' = 3, then a box with (x1,y1) at one corner
c                 and (x2,y2) at the other corner is drawn.
c               If ``type'' = 4, then a "V" is drawn from the start
c                 point (x1,y1) to the present cursor position to
c                 the end point (x2,y2).
c               NOTE:  ``x1, x2, y1, y2'' MUST be input if ``type'' = 4.
c
c  Input/Output:
c    x1, y1   Coordinates of start location (not zoomed corrected).
c    x2, y2   Coordinates of end location (not zoomed corrected).
c    ch       Which mouse button was pushed to cause selection.
c
c--
c-----------------------------------------------------------------------
      include 'tv.h'
      integer n, flags
c
      if(protocol.eq.ivserve)then
        continue
c
      else if(protocol.eq.Sss)then
c  Flags = 0 means Click-Drag-Release
c  Flags = 1 means Click-Release-Drag-Click
        flags = 0
c--jm   flags = 1
        n = 0
        if (type .eq. 4) n = 2 * 4
        call TVcheck(6+((n+1)/2))
        buffer(BufLen+1) = SssSelpt
        buffer(BufLen+2) = channel
        buffer(BufLen+3) = type
        buffer(BufLen+4) = flags
        buffer(BufLen+5) = 0
        buffer(BufLen+6) = n
        BufLen = BufLen + 6
        if (type .eq. 4) then
          buffer(BufLen+1) = x1
          buffer(BufLen+2) = y1
          buffer(BufLen+3) = x2
          buffer(BufLen+4) = y2
          BufLen = BufLen + 4
        endif
c  Screwy fix to match Harold's code.  I need to look at the status and
c  ndata words returned from the socket; so Nack cannot be used here.
c  So, read the first two int*2's to determine if there will be any
c  other data coming down the line.
        call TvRead(2)
        if ((buffer(1) .ge. 0) .and. (buffer(2) .gt. 0)) then
          call TvRead(6)
c  The first two elements are a modifier flag and the key pressed.
c  The modifier flag is ignored by this routine.
c  CH is modified here to represent the number of the button pushed.
c  (32544 is BUT(1) code number in /usr/include/sundev/vuid_event.h).
          ch = buffer(2) - 32544 + 1
          if ((ch .lt. 1) .or. (ch .gt. 3)) ch = 1
c  The next four elements are the (not zoomed corrected) positions.
          x1 = buffer(3)
          y1 = buffer(4)
          x2 = buffer(5)
          y2 = buffer(6)
        endif
      else if(protocol.eq.Xas)then
c  Flags = 0 means Click-Drag-Release
c  Flags = 1 means Click-Release-Drag-Click
        flags = 0
        n = 0
        if (type .eq. 4) n = 2 * 4
        call TVcheck(6+((n+1)/2))
        buffer(BufLen+1) = XasSelpt
        buffer(BufLen+2) = channel
        buffer(BufLen+3) = type
        buffer(BufLen+4) = flags
        buffer(BufLen+5) = 0
        buffer(BufLen+6) = n
        BufLen = BufLen + 6
        if (type .eq. 4) then
          buffer(BufLen+1) = x1
          buffer(BufLen+2) = y1
          buffer(BufLen+3) = x2
          buffer(BufLen+4) = y2
          BufLen = BufLen + 4
        endif
        call TvRead(7)
        if (buffer(1).eq.0) then
c  The first element is a status value; the second the data length.
c  If the status is zero, all went well; <0 means some sort of error.
c  buffer(7) is the number of the button pushed.
          x1 = buffer(3)
          y1 = buffer(4)
          x2 = buffer(5)
          y2 = buffer(6)
          ch = buffer(7)
        else
          x1 = 0
          y1 = 0
          x2 = 0
          y2 = 0
          ch = 0
        endif
#ifdef vms
      else if(protocol.eq.Ivas)then
        continue
#endif
c
      endif
      end
c***********************************************************************
c* TvWind -- set/read current window (viewport) of a display device.
c& jm
c: tv,display
c+
      subroutine tvwind(x1, y1, x2, y2)
c
      implicit none
      integer x1, y1, x2, y2
c
c  TVWIND allows the current viewport on a TV screen to be set/read.
c  If the device is capable of adjusting its viewport and the input
c  positions are non-zero, the viewport is set.  In both cases, the
c  current viewport is returned.
c
c  Input/Output:
c    x1, y1   Coordinates of the lower left corner in TV pixels.
c    x2, y2   Coordinates of the top right corner in TV pixels.
c
c  NOTE:  x1, x2, y1, y2 should ALL be set equal to 0 if resizing is NOT
c         desired (ie. just do a viewport read).
c
c--
c-----------------------------------------------------------------------
      include 'tv.h'
c
      if(protocol.eq.ivserve)then
        x1 = 1
        y1 = 1
        x2 = 1024
        y2 = 1024
c
      else if(protocol.eq.Sss)then
        call TVcheck(6)
        buffer(BufLen+1) = SssWindo
        buffer(BufLen+2) = x1
        buffer(BufLen+3) = y1
        buffer(BufLen+4) = x2
        buffer(BufLen+5) = y2
        buffer(BufLen+6) = 0
        BufLen = BufLen + 6
        Nack = Nack + 1
        call TvRead(4)
        x1 = buffer(1)
        y1 = buffer(2)
        x2 = buffer(3)
        y2 = buffer(4)
      else if(protocol.eq.Xas)then
        call TVcheck(6)
        buffer(BufLen+1) = XasWindo
	buffer(BufLen+2) = x1
	buffer(BufLen+3) = y1
	buffer(BufLen+4) = x2
	buffer(BufLen+5) = y2
        buffer(BufLen+6) = 0
        BufLen = BufLen + 6
        Nack = Nack + 1
        call TvRead(4)
        x1 = buffer(1)
        y1 = buffer(2)
        x2 = buffer(3)
        y2 = buffer(4)
#ifdef vms
      else if(protocol.eq.Ivas)then
        x1 = 1
        y1 = 1
        x2 = 1024
        y2 = 1024
#endif
c
      endif
      end
c************************************************************************
c* TvLine -- Write a raster line to the image display device.
c& jm
c: tv,display
c+
	subroutine tvline(x,y,channel,array,n)
c
	implicit none
	integer x,y,channel,n,array(n)
c
c  Write a line to the TV screen.
c
c  Integer:
c   x,y	  	Coordinates of start location.
c   channel	Channel to load.
c   array	Array of pixels.
c   n		Number of pixels.
c--
c------------------------------------------------------------------------
	include 'tv.h'
c
	if(protocol.eq.ivserve)then
	  call TVcheck(11+(n+1)/2)
	  buffer(BufLen+1) = IvMaImag
	  buffer(BufLen+2) = 0
	  buffer(BufLen+3) = n
	  buffer(BufLen+4) = PassIn
	  buffer(BufLen+5) = PassByte
	  buffer(BufLen+6) = n
	  buffer(BufLen+7) = x
	  buffer(BufLen+8) = 1023 - y
	  buffer(BufLen+9) = channel - 1
	  buffer(BufLen+10) = -1
	  buffer(BufLen+11) = 0
	  BufLen = BufLen + 11
	  call tvpack(array,n)
c
	else if(protocol.eq.Sss)then
	  call TVcheck(6+(n+1)/2)
	  buffer(BufLen+1) = SssImWrt
	  buffer(BufLen+2) = x
	  buffer(BufLen+3) = y
	  buffer(BufLen+4) = channel
	  buffer(BufLen+5) = 0
	  buffer(BufLen+6) = n
	  BufLen = BufLen + 6
	  call tvpack(array,n)
	else if(protocol.eq.Xas)then
	  call TVcheck(6+(n+1)/2)
	  buffer(BufLen+1) = XasImWrt
	  buffer(BufLen+2) = x
	  buffer(BufLen+3) = y
	  buffer(BufLen+4) = channel
	  buffer(BufLen+5) = 0
	  buffer(BufLen+6) = n
	  BufLen = BufLen + 6
	  call tvpack(array,n)
#ifdef vms
	else if(protocol.eq.Ivas)then
	  call fivasMAimage(array,n,PassIn,PassInt,n,x,1023-y,
     *						  channel-1,-1,0)
#endif
c
#ifdef cft
	else if(protocol.eq.File)then
	  call TVFline(x,y,array,n)
#ifdef ULTRA
	else if(protocol.eq.Ultra)then
	  call Uline(x,y,array,n)
#endif
#endif
c
#ifdef FX
#ifdef RASTER
	else if(protocol.eq.Raster)then
	  call RSline(x,y,array,n)
#endif
#endif
	endif
	end
c***********************************************************************
c* TvGraph -- Turns the TV graphics planes on and off.
c& jm
c: tv, display
c+
      subroutine tvgraph(chan, onoff)
c
      implicit none
      integer chan, onoff
c
c   Inputs:
c     chan    Graphics Tv Channel to draw on (typically 1 to 4).
c     onoff   1 turns the graphics planes on; 0 turns them off.
c
c   Output:
c     (none)
c
c--
c-----------------------------------------------------------------------
      include 'tv.h'
c
      if(protocol.eq.Sss)then
        call TVcheck(6)
        buffer(BufLen+1) = SssGraph
        buffer(BufLen+2) = 1
        buffer(BufLen+3) = onoff
        buffer(BufLen+4) = 0
        buffer(BufLen+5) = 0
        buffer(BufLen+6) = 0
        BufLen = BufLen + 6
        Nack = Nack + 1
      else if(protocol.eq.Xas)then
        call TVcheck(6)
        buffer(BufLen+1) = XasGraph
        buffer(BufLen+2) = chan
        buffer(BufLen+3) = onoff
        buffer(BufLen+4) = 0
        buffer(BufLen+5) = 0
        buffer(BufLen+6) = 0
        BufLen = BufLen + 6
      endif
c
      return
      end
c***********************************************************************
c* TvVect -- Writes a line segment to the TV.
c& jm
c: tv, display
c+
      subroutine tvvect(chan, onoff, x1, y1, x2, y2)
c
      implicit none
      integer chan, onoff, x1, x2, y1, y2
c
c   Inputs:
c     chan    Graphics Tv Channel to draw on (typically 1 to 4).
c     onoff   1 draws the line; 0 erases it.
c     x1      start X position
c     y1      start Y position
c     x2      end X position
c     y2      end Y position
c
c   Output:
c     (none)
c
c--
c-----------------------------------------------------------------------
      include 'tv.h'
      integer length, npts, ix, iy, ichan
      integer startx, endx, starty, endy
      integer deltax, deltay, step
      integer xpix, ypix, mxchan, lev
      integer onbuf(BufSize), offbuf(BufSize)
      real pos, posinc
      data onbuf  /BufSize * 1/
      data offbuf /BufSize * 0/
c
      call TvChar(xpix, ypix, mxchan, lev)
      if (((x1 .lt. 1) .or. (x1 .gt. xpix)) .or.
     *    ((y1 .lt. 1) .or. (y1 .gt. ypix)) .or.
     *    ((x2 .lt. 1) .or. (x2 .gt. xpix)) .or.
     *    ((y2 .lt. 1) .or. (y2 .gt. ypix))) then
        call bug('w', 'TvVect: Position input out of range for device.')
        return
      endif
c  Make sure this graphics channel is turned on!
      call TvGraph(chan, 1)

      startx = x1
      starty = y1
      endx = x2
      endy = y2
      step = 1
      deltax = abs(endx - startx) + 1
      deltay = abs(endy - starty) + 1
      ichan = mxchan + chan
c
c  Check drawing order.
      if (startx .gt. endx) then
        startx = x2
        endx = x1
        starty = y2
        endy = y1
      endif
      if (starty .gt. endy) step = -1
      posinc = real(deltax) / real(deltay)
      if (startx .eq. endx) posinc = 0.0
      pos = real(startx)
c  Manual DO WHILE loop.
      iy = starty
   10 continue
        ix = nint(pos)
        pos = pos + posinc
        length = nint(pos) - ix
        if (length .lt. 1) length = 1
        npts = length
        if (npts .gt. BufSize) npts = BufSize
        if (onoff .ne. 0) then
          call tvline(ix, iy, ichan, onbuf, npts)
        else
          call tvline(ix, iy, ichan, offbuf, npts)
        endif
        if (length .eq. npts) iy = iy + step
      if (((step .lt. 0) .and. (iy .ge. endy)) .or.
     *    ((step .gt. 0) .and. (iy .le. endy))) goto 10
      return
      end
c************************************************************************
c* TvText -- Write a character string to the image display device.
c& jm
c: tv,display
c+
      subroutine tvtext(x, y, channel, string, n, color, dir)
c
      implicit none
      integer x, y, channel, n
      integer color, dir
      character string*(*)
c
c  Write a line of text to the TV screen.
c
c  Integer:
c     x, y      Device coordinates of starting location.
c     channel   Channel to load text.
c     string    Text to write.
c     n         Number of characters in ``string''.
c     color     Color of text (Either 0 or 1 for graphics plane).
c               0 means off; 63 means full on.
c     dir       Direction (0: L->R; 1: T->D; 2: R->L; 3: B->T).
c--
c-----------------------------------------------------------------------
      include 'tv.h'
c
      if(protocol.eq.ivserve)then
        continue
c
      else if(protocol.eq.Sss)then
        call TVcheck(6+(n+1)/2)
        buffer(BufLen+1) = SssText
        buffer(BufLen+2) = channel
        buffer(BufLen+3) = x
        buffer(BufLen+4) = y
        buffer(BufLen+5) = (mod(color, 64) * 1024) + mod(dir, 4)
        buffer(BufLen+6) = n
        BufLen = BufLen + 6
        Nack = Nack + 1
        call tvcpack(string,n)
#ifdef vms
      else if(protocol.eq.Ivas)then
        continue
#endif
c
#ifdef cft
      else if(protocol.eq.File)then
        continue
#ifdef ULTRA
      else if(protocol.eq.Ultra)then
        continue
#endif
#endif
c
#ifdef FX
#ifdef RASTER
      else if(protocol.eq.Raster)then
        continue
#endif
#endif
      endif
      end
c************************************************************************
c* TvScale -- Send the scaling factors of an image to the display.
c& jm
c: tv,display
c+
      subroutine tvscale(bzero, bscale)
c
      real bzero, bscale
c
c     Sends the zero and scale factors to the server.  This is used for
c     server display purposes only to print approximate pixel values
c     when displaying cursor positions.
c
c  Input:
c    bzero    Value of pixel can be determined by the expression:
c    bscale   Real value = bzero + (bscale * pixel)
c
c------------------------------------------------------------------------
      include 'tv.h'
c
      integer TOOBIG
      parameter (TOOBIG=32767)
c
      integer ezero, mzero
      integer escale, mscale
c
      if(bzero.eq.0.0)then
        ezero = 0
        mzero = 0
      else
        ezero = log10(abs(bzero)) - 6
        mzero = nint(bzero * (10.0 ** (-ezero)))
        dowhile (abs(mzero) .gt. TOOBIG)
          ezero = ezero + 1
          mzero = nint(bzero * (10.0 ** (-ezero)))
        enddo
      endif
c
      if(bscale.eq.0.0)then
        escale = 0
        mscale = 0
      else
        escale = log10(abs(bscale)) - 6
        mscale = nint(bscale * (10.0 ** (-escale)))
        dowhile (abs(mscale) .gt. TOOBIG)
          escale = escale + 1
          mscale = nint(bscale * (10.0 ** (-escale)))
        enddo
      endif
c
      if(protocol.eq.Xas)then
        call TVcheck(6)
        buffer(BufLen+1) = XasScale
        buffer(BufLen+2) = ezero
        buffer(BufLen+3) = mzero
        buffer(BufLen+4) = escale
        buffer(BufLen+5) = mscale
        buffer(BufLen+6) = 0
        BufLen = BufLen + 6
        Nack = Nack + 1
      endif
      end
c************************************************************************
	subroutine tvpack(array,n)
c
	integer n,array(n)
c
c  Pack pixels into the buffer.
c
c  Input:
c    array	Array containing the pixels.
c    n		Number of pixels to pack.
c
c------------------------------------------------------------------------
	integer i
	include 'tv.h'
c
	do i=1,n-1,2
	  BufLen = BufLen + 1
	  buffer(BufLen) = 256*array(i) + array(i+1)
	enddo
	if(2*(n/2).ne.n)then
	  BufLen = BufLen + 1
	  buffer(BufLen) = 256*array(n)
	endif
	end
c************************************************************************
      subroutine tvcpack(string, n)
c
      integer n
      character string*(*)
c
c  Pack characters into the buffer.
c
c  Input:
c     string    Array containing the characters.
c     n         Length of string to pack.
c
c------------------------------------------------------------------------
      integer i
      include 'tv.h'
c
      do i=1,n-1,2
        BufLen = BufLen + 1
        buffer(BufLen) = 256*ichar(string(i:i)) + ichar(string(i+1:i+1))
      enddo
      if(2*(n/2).ne.n)then
        BufLen = BufLen + 1
        buffer(BufLen) = 256*ichar(string(n:n))
      endif
      end
c************************************************************************
	subroutine tvconn(handle,port,name)
c
	integer handle,port
	character name*(*)
c
c  Connect to a TV server.
c
c  Input:
c    port	Port number of the server.
c    name	The name of the host supporting the server.
c
c  Output:
c    handle	Some handle used by the TCP/IP software.
c------------------------------------------------------------------------
	integer inet,status
c
c  Externals.
c
	integer TcpSock,TcpConn
c
	call tcpnode(name,inet)
	if(inet.eq.0)call bug('f','Host unknown')
	Status = TcpSock(handle)
	if(Status.ne.0)call bugno('f',Status)
	Status = TcpConn(handle,inet,port)
	if(Status.ne.0)call bugno('f',Status)
	end
c************************************************************************
	subroutine TVread(n)
c
	implicit none
	integer n
c
c  Read some data from the TV device.
c
c  Inputs:
c    n		Number of words to be read.
c
c------------------------------------------------------------------------
	include 'tv.h'
	integer Read,Nread,Status,iobuf(2*BufSize/BypWrd)
c
c  Externals.
c
	integer TcpRead
c
	if(n.gt.BufSize)call bug('f','Request overflows buffer')
	call TVcheck(BufSize)
c
	Read = 0
	dowhile(Read.lt.n)
	  Status = TcpRead(handle,iobuf,2*(N-Read),nread)
	  if(Status.ne.0.or.nread.eq.0)
     *	      call bug('f','Error reading from server')
	  call unpacki2(iobuf,buffer(read+1),nread/2)
	  read = read + nread/2
	enddo
	end
c************************************************************************
	subroutine TVcheck(n)
c
	implicit none
	integer n
c
c  This makes sure there is enough space in the buffer to hold the next
c  request. If there is not, the buffer is flushed to the socket.
c
c  Input:
c    n		Size required in the buffer.
c
c------------------------------------------------------------------------
	include 'tv.h'
	integer Status,nread,iobuf(2*BufSize/BypWrd)
	integer TcpWrite,TcpRead
c
c  Check for space, and flush the buffer if needed.
c
	if(n.gt.BufSize)call bug('f','Request overflows buffer')
	if((protocol.ne.ivserve.and.BufLen+n.gt.BufSize).or.
     *	   (protocol.eq.ivserve.and.Buflen+n.gt.BufSize+1))then
	  if(protocol.eq.ivserve)buffer(1) = BufLen
	  call packi2(buffer,iobuf,BufLen)
	  Status = TcpWrite(handle,iobuf,2*BufLen)
	  if(Status.ne.0)call bugno('f',Status)
	  
	  if(protocol.eq.ivserve)then
	    BufLen = 1
	  else
	    BufLen = 0
	  endif
c
c  Wait for acknowledgements.
c
	  if(protocol.eq.ivserve)then
	    nack = 2*nack
	  else
	    nack = 4*nack
	  endif
	  dowhile(Nack.gt.0)
	    Status = TcpRead(handle,buffer,min(2*Bufsize,Nack),nread)
	    if(Status.ne.0.or.nread.eq.0)
     *	      call bug('f','Error getting acknowledgement from server')
	    Nack = Nack - nread
	  enddo
	  if(protocol.eq.ivserve)then
	    nack = 1
	  else
	    nack = 0
	  endif
	endif
	end
