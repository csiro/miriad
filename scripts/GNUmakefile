#-----------------------------------------------------------------------------
# GNUmakefile used to install Miriad scripts.
#
# Original: 2006/09/18, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.7 2007/03/06 01:51:16 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

ifeq "$(MAKEMODE)" "system"
  # Scripts are not installed unless listed explicitly.
  MIRSCRPS := mirarch mirbug mirexport mirhelp mirimport mirmerge mirsync
  MIRSCRPS += mirtester mirtree mirversion pntfix

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
  $(MIRBIND)/% : $(MIRSCRPD)/%
	-@ $(RM) $%
	   cp $< $@
	-@ chmod 775 $@

  # Extract preamble comments into a Miriad "doc" file.
  $(MIRDOCD)/%.doc : %
	-@ $(RM) $(MIRDOCD)/$*.doc
	   $(MIRBIND)/doc -f $< > $@
	-@ chmod 664 $@


  # Static and static pattern rules.
  #---------------------------------
  allsys :: bin doc $(MIRARCH)
  
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
