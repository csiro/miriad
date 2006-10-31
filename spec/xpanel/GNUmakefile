#-----------------------------------------------------------------------------
# GNUmakefile for compiling Miriad utility xpanel.
#
# Original: 2006/09/15, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.2 2006/10/31 01:34:36 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

show ::
	-@ echo ""
	-@ echo ""
	-@ echo "Variables defined in the xpanel GNUmakefile"
	-@ echo "==========================================="
	-@ echo ""
	-@ echo "(none)"

ifeq "$(MAKEMODE)" "system"
  # System-oriented rules.
  #-----------------------
  CFLAGS   := $(CPPOPT) -I../xmtv -I$(X11INCD) $(CCOPT) $(LDOPT)

  # Static and static pattern rules.
  #---------------------------------
  allsys :: $(MIRBIND)/xpanel.exe $(MIRBIND)/xpanel

  $(MIRBIND)/xpanel.exe : $(CODEDIR)/xpanel.c
	-@ $(RM) $@
	   $(CC) $(CFLAGS) -o $@ $< $(X11LIBS) $(XTRALIBS)
	-@ chmod 775 $@

  $(MIRBIND)/xpanel : $(CODEDIR)/xpanel.csh
	-@ $(RM) $@
	   cp $< $@
	-@ chmod 775 $@

  help ::
	-@ echo ""
	-@ echo "Targets defined in the xpanel GNUmakefile"
	-@ echo "-----------------------------------------"
	-@ echo "     allsys: recompile xpanel."

else
  # Programmer-oriented rules.
  #---------------------------
  CFLAGS   := $(CPPDBG) -I../xmtv -I$(CODEDIR)/../xmtv -I$(X11INCD) $(CCDBG)
  CFLAGS   += $(LDDBG)
  VPATH    := .:$(CODEDIR)

  # Static and static pattern rules.
  #---------------------------------
  all :: $(PGMRBIND)/xpanel.exe

  $(PGMRBIND)/xpanel.exe : xpanel.c
	$(CC) $(CFLAGS) -o $@ $< $(X11LIBS) $(XTRALIBS)

  clean ::
	$(RM) *.o

  cleanest :: clean
	$(RM) $(PGMRBIND)/xpanel*

  help ::
	-@ echo ""
	-@ echo "Targets defined in the xpanel GNUmakefile"
	-@ echo "-----------------------------------------"
	-@ echo "        all: recompile xpanel."

  show ::
	-@ echo ""
	-@ echo "VPATH    = $(VPATH)"
	-@ echo "PGMREXES = $(PGMREXES)"
endif
