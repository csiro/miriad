#-----------------------------------------------------------------------------
# GNUmakefile for compiling Miriad utility xmtv.
#
# Original: 2006/09/15, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.1 2006/09/15 07:38:37 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

XMTVSRCS := $(filter %.c, $(MIRSRCS))
XMTVINCS := $(filter %.h %.icon, $(MIRSRCS))

show ::
	-@ echo ""
	-@ echo ""
	-@ echo "Variables defined in the xmtv GNUmakefile"
	-@ echo "=========================================="
	-@ echo ""
	-@ echo "XMTVSRCS  = $(XMTVSRCS)"
	-@ echo "XMTVINCS  = $(XMTVINCS)"

ifeq "$(MAKEMODE)" "system"
  # System-oriented rules.
  #-----------------------
  CFLAGS   := $(CPPOPT) -I$(X11INCD) $(CCOPT) $(LDOPT)

  # Static and static pattern rules.
  #---------------------------------
  allsys :: $(MIRBIND)/xmtv.exe $(MIRBIND)/xmtv $(MIRCATD)/XMtv \
    $(MIRDOCD)/xmtv.doc $(MIRSDOCD)/xmtv.doc

  $(MIRBIND)/xmtv.exe : $(XMTVSRCS) $(XMTVINCS)
	-@ echo ""
	-@ $(RM) $@
	   $(CC) $(CFLAGS) -o $@ $(XMTVSRCS) $(X11LIBS) $(XTRALIBS)
	-@ $(RM) *.o
	-@ chmod 775 $@

  $(MIRBIND)/xmtv : $(CODEDIR)/xmtv.csh
	-@ $(RM) $@
	   cp $< $@
	-@ chmod 775 $@

  $(MIRCATD)/XMtv : $(CODEDIR)/XMtv.ad
	-@ $(RM) $@
	   cp $< $@
	-@ chmod 664 $@

  $(MIRDOCD)/xmtv.doc : $(CODEDIR)/xmtv.doc
	-@ $(RM) $@
	   cp $< $@
	-@ chmod 664 $@

  $(MIRSDOCD)/xmtv.doc : $(CODEDIR)/xmtv.doc
	-@ $(RM) $@
	   cp $< $@
	-@ chmod 664 $@

  help ::
	-@ echo ""
	-@ echo "Targets defined in the xmtv GNUmakefile"
	-@ echo "----------------------------------------"
	-@ echo "     allsys: recompile xmtv."

else
  # Programmer-oriented rules.
  #---------------------------
  CFLAGS   := $(CPPDBG) -I../xmtv -I$(X11INCD) $(CCDBG) $(LDDBG)
  VPATH    := .:$(CODEDIR)

  # Static and static pattern rules.
  #---------------------------------
  all :: $(PGMRBIND)/xmtv.exe

  $(PGMRBIND)/xmtv.exe : $(XMTVSRCS) $(XMTVINCS)
	$(CC) $(CFLAGS) -o $@ $(XMTVSRCS) $(X11LIBS) $(XTRALIBS)

  clean ::
	$(RM) *.o

  cleanest :: clean
	$(RM) $(PGMRBIND)/xmtv*

  help ::
	-@ echo ""
	-@ echo "Targets defined in the xmtv GNUmakefile"
	-@ echo "----------------------------------------"
	-@ echo "        all: recompile xmtv."

  show ::
	-@ echo ""
	-@ echo "VPATH    = $(VPATH)"
	-@ echo "PGMREXES = $(PGMREXES)"
endif
