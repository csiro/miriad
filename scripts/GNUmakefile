#-----------------------------------------------------------------------------
# GNUmakefile used to install Miriad scripts.
#
# Original: 2006/09/18, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.1 2006/09/20 08:18:37 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

ifeq "$(MAKEMODE)" "system"
  MIRSCRPS := $(basename $(filter %.csh,$(MIRSRCS)))

  show ::
	-@ echo ""
	-@ echo "Variables defined in the scripts GNUmakefile"
	-@ echo "============================================"
	-@ echo ""
	-@ echo "MIRSCRPS = $(MIRSCRPS)"

  # Pattern rules.
  #---------------
  $(MIRBIND)/% : $(MIRSCRPD)/%.csh
	-@ $(RM) $%
	   cp $< $@
	-@ chmod 775 $@


  # Static and static pattern rules.
  #---------------------------------
  allsys :: $(MIRSCRPS:%=$(MIRBIND)/%)

else
  # Programmer-oriented rules.
  #---------------------------
  help ::
	-@ echo ""
	-@ echo "No programmer-oriented rules are defined in the scripts"
	-@ echo "GNUmakefile."
endif
