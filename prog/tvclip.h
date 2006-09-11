c
c  "tvflags.h"
c
c  Include file for display editing commands.
c
c    NULL         NULL COMMAND
c    CURSOR       PAN AROUND THE SCREEN OR STRETCH LUT
c    ZOOMIN       ZOOM IN DISPLAY
c    ZOOMOUT      ZOOM OUT DISPLAY
c    FIDPAN       TOGGLE BETWEEN PANNING AND STRETCHING
c    SELECT       SELECT REGION FOR FLAGGING OPERATION
c    CHANNEL      SELECT A CHANNEL FOR FLAGGING OPERATION
c    TIME         SELECT A TIME SLOT FOR FLAGGING OPERATION
c    FLAGOOD      FLAG RANGE AS GOOD
c    FLAGBAD      FLAG RANGE AS BAD
c    DIFF         SUBTRACT RUNNING MEAN
c    RESCALE      RESCALE THE DISPLAY
c    CLIP         CLIP ALL PIXELS ABOVE SET THRESHOLD IN SIGMAS

c    LIST         LIST CHANGES MADE FOR A BASELINE
c    PIXVAL       LIST PIXEL VALUE
c    LUT          TOGGLE BETWEEN DIFFERENT COLOR LUT'S FOR MAP
c    RESET        RESTORE INTENSITY RANGE AND ZERO ZOOM AND PAN
c    QUIT         STOP EDIT AND DO NOT SAVE CHANGES
c    EXIT         STOP EDIT AND SAVE CHANGES
c    UNDO         UNDO LAST FLAG CHANGE
c    ABORT        STOP EDIT AND DO NOT SAVE CHANGES AND TERMINATE PROGRAM
c    HELP         DISPLAY THE EDIT MENU
c
      integer iNULL, iHELP, iINITALL, iINITBL
      integer iQUIT, iEXIT, iUNDO, iABORT
      integer iZOOMIN, iZOOMOUT, iFIDPAN, iCURSOR
      integer iSELECT, iCHANNEL, iTIME
      integer iFLAGOOD, iFLAGBAD, iLIST, iPIXVAL
      integer iLUT, iRESET, iRESCALE, iDIFF, iCLIP
c
      parameter (iINITALL = -1) 
      parameter (iINITBL  = -2) 
      parameter (iNULL    =  0)
c  DO NOT REMOVE THE ABOVE PARAMETER AND PLACE NO OTHERS BEFORE IT.
      parameter (iCURSOR  =  1)
      parameter (iZOOMIN  =  2)
      parameter (iZOOMOUT =  3)
      parameter (iFIDPAN  =  4)
      parameter (iSELECT  =  5)
      parameter (iCHANNEL =  6)
      parameter (iTIME    =  7)
      parameter (iFLAGOOD =  8)
      parameter (iFLAGBAD =  9)
      parameter (iRESCALE = 10)
      parameter (iDIFF    = 11)
      parameter (iCLIP    = 12)
      parameter (iLIST    = 13)
      parameter (iPIXVAL  = 14)
      parameter (iLUT     = 15)
      parameter (iRESET   = 16)
      parameter (iQUIT    = 17)
      parameter (iEXIT    = 18)
      parameter (iUNDO    = 19)
      parameter (iABORT   = 20)
c  KEEP THE NEXT PARAMETER AS THE HIGHEST TERM AND PLACE NONE AFTER IT.
      parameter (iHELP    = 21)
c
      integer NCOMS
      parameter (NCOMS = iHELP)
c
      integer iEDIT(0:NCOMS)
      character cEDIT(0:NCOMS)*1, sEDIT(0:NCOMS)*10, tEDIT(NCOMS)*40
      common / iEDITcom / iEDIT
      common / cEDITcom / cEDIT, sEDIT, tEDIT
c
