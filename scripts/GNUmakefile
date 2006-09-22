#-----------------------------------------------------------------------------
# GNUmakefile used to install Miriad scripts.
#
# Original: 2006/09/18, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.2 2006/09/22 04:41:27 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

ifeq "$(MAKEMODE)" "system"
  # Note that scripts without a .csh or .sh suffix are not installed.
  MIRSCRPS := $(basename $(filter %.csh,$(MIRSRCS)))

  # Only a few of the scripts have doc markup.
  MIRDOCS  := mirbug mirhelp mirversion

  show ::
	-@ echo ""
	-@ echo "Variables defined in the scripts GNUmakefile"
	-@ echo "============================================"
	-@ echo ""
	-@ echo "MIRSCRPS = $(MIRSCRPS)"
	-@ echo "MIRDOCS  = $(MIRDOCS)"

  # Pattern rules.
  #---------------
  $(MIRBIND)/% : $(MIRSCRPD)/%.csh
	-@ $(RM) $%
	   cp $< $@
	-@ chmod 775 $@

  # Extract preamble comments into a Miriad "doc" file.
  $(MIRDOCD)/%.doc : %.csh
	-@ $(RM) $(MIRDOCD)/$*.doc
	   $(MIRBIND)/doc -f $< > $@
	-@ chmod 664 $@


  # Static and static pattern rules.
  #---------------------------------
  allsys :: bin doc
  
  bin : $(MIRSCRPS:%=$(MIRBIND)/%) ;

  doc : $(MIRDOCS:%=$(MIRDOCD)/%.doc) ;

  help ::
	-@ echo ""
	-@ echo "Targets defined in the prog makefile"
	-@ echo "===================================="
	-@ echo ""
	-@ echo "System"
	-@ echo "------"
	-@ echo "     allsys: install all scripts."
	-@ echo "        bin: install all scripts."
	-@ echo "        doc: generate doc files for selected scripts."

else
  # Programmer-oriented rules.
  #---------------------------
  help ::
	-@ echo ""
	-@ echo "No programmer-oriented rules are defined in the scripts"
	-@ echo "GNUmakefile."
endif
