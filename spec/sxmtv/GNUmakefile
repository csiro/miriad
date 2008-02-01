#-----------------------------------------------------------------------------
# GNUmakefile for compiling Miriad utility sxmtv.
#
# Original: 2006/09/15, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.2 2008/02/01 05:50:58 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

SXMTVSRCS := $(filter %.c, $(MIRSRCS))
SXMTVINCS := $(filter %.h, $(MIRSRCS)) ../xmtv/xmtv.icon

show ::
	-@ echo ""
	-@ echo ""
	-@ echo "Variables defined in the sxmtv GNUmakefile"
	-@ echo "=========================================="
	-@ echo ""
	-@ echo "SXMTVSRCS  = $(SXMTVSRCS)"
	-@ echo "SXMTVINCS  = $(SXMTVINCS)"

ifeq "$(MAKEMODE)" "system"
  # System-oriented rules.
  #-----------------------
  CFLAGS   := $(CPPOPT) -I. -I../xmtv -I$(X11INCD) $(CCOPT) $(LDOPT)

  # Static and static pattern rules.
  #---------------------------------
  allsys :: $(MIRBIND)/sxmtv.exe

  $(MIRBIND)/sxmtv.exe : $(SXMTVSRCS) $(SXMTVINCS)
	-@ echo ""
	-@ $(RM) $@
	   $(CC) $(CFLAGS) -o $@ $(SXMTVSRCS) $(X11LIB) $(XTRALIBS)
	-@ $(RM) *.o
	-@ chmod 775 $@

  help ::
	-@ echo ""
	-@ echo "Targets defined in the sxmtv GNUmakefile"
	-@ echo "----------------------------------------"
	-@ echo "     allsys: recompile sxmtv."

else
  # Programmer-oriented rules.
  #---------------------------
  CFLAGS   := $(CPPDBG) -I. -I../xmtv
  CFLAGS   += -I$(CODEDIR) -I$(CODEDIR)/../xmtv -I$(X11INCD) $(CCDBG)
  CFLAGS   += $(LDDBG)
  VPATH    := .:$(CODEDIR)

  # Static and static pattern rules.
  #---------------------------------
  all :: $(PGMRBIND)/sxmtv.exe

  $(PGMRBIND)/sxmtv.exe : $(SXMTVSRCS) $(SXMTVINCS)
	$(CC) $(CFLAGS) -o $@ $(filter %.c,$^) $(X11LIB) $(XTRALIBS)

  clean ::
	$(RM) *.o

  cleanest :: clean
	$(RM) $(PGMRBIND)/sxmtv*

  help ::
	-@ echo ""
	-@ echo "Targets defined in the sxmtv GNUmakefile"
	-@ echo "----------------------------------------"
	-@ echo "        all: recompile sxmtv."

  show ::
	-@ echo ""
	-@ echo "VPATH    = $(VPATH)"
	-@ echo "PGMREXES = $(PGMREXES)"
endif
