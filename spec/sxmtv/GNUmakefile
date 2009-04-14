#-----------------------------------------------------------------------------
# GNUmakefile for compiling Miriad utility sxmtv.
#
# Original: 2006/09/15, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.3 2009/04/14 02:29:44 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

SXMTVSRCS := $(filter %.c, $(MIRSRCS))
SXMTVINCS := $(filter %.h, $(MIRSRCS)) ../xmtv/xmtv.icon

ifneq "$(X11INCD)" ""
  X11INC := -I$(X11INCD)
endif

show ::
	-@ echo ""
	-@ echo ""
	-@ echo "Variables defined in the sxmtv GNUmakefile"
	-@ echo "=========================================="
	-@ echo ""
	-@ echo "SXMTVSRCS  = $(SXMTVSRCS)"
	-@ echo "SXMTVINCS  = $(SXMTVINCS)"
	-@ echo "X11INC     = $(X11INC)"

ifeq "$(MAKEMODE)" "system"
  # System-oriented rules.
  #-----------------------
  CFLAGS   := $(CPPOPT) -I. -I../xmtv $(X11INC) $(CCOPT) $(LDOPT)

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
  CFLAGS   += -I$(CODEDIR) -I$(CODEDIR)/../xmtv $(X11INC) $(CCDBG)
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
