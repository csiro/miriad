#-----------------------------------------------------------------------------
# GNUmakefile for bbcal.
#
# $Id: GNUmakefile,v 1.1.1.1 2009/04/22 13:10:24 wie017 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

show ::
	-@ echo ""
	-@ echo ""
	-@ echo "Variables defined in the xpanel GNUmakefile"
	-@ echo "==========================================="
	-@ echo ""

ifeq "$(MAKEMODE)" "system"
  # System-oriented rules.
  #-----------------------

  # Static and static pattern rules.
  #---------------------------------
  allsys :: $(MIRBIND)/bbcal

  $(MIRBIND)/bbcal : $(CODEDIR)/bbcal.csh
	-@ $(RM) $@
	   cp $< $@
	-@ chmod 775 $@

  help ::
	-@ echo ""
	-@ echo "Targets defined in the bbcal GNUmakefile"
	-@ echo "-----------------------------------------"
	-@ echo "     allsys: install bbcal."

else
  # Programmer-oriented rules.
  #---------------------------

  # Static and static pattern rules.
  #---------------------------------
  all :: $(PGMRBIND)/bbcal

  $(PGMRBIND)/bbcal :  bbcal.csh
	-@ $(RM) $@
	   cp $< $@
	-@ chmod 775 $@

  clean ::

  cleanest :: clean
	$(RM) $(PGMRBIND)/bbcal

  help ::
	-@ echo ""
	-@ echo "Targets defined in the xpanel GNUmakefile"
	-@ echo "-----------------------------------------"
	-@ echo "        all: install bbcal."

  show ::
	-@ echo ""
	-@ echo "PGMREXES = $(PGMREXES)"
endif
