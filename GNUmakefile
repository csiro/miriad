#-----------------------------------------------------------------------------
# GNUmakefile used to compile Miriad.
#
# Original: 2006/08/28, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.2 2006/09/15 07:52:16 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

ifeq "$(MAKEMODE)" "system"
  # Subdirectories in which to invoke "allsys", in order.
  #------------------------------------------------------
  ALLSYSD  := $(findstring linpack,$(SUBDIRS))
  ALLSYSD  += tools subs prog spec

  # Static and static pattern rules.
  #---------------------------------
  allsys :: MIRRC MIRRC.sh sysdirs $(ALLSYSD)

  show ::
	-@ echo ""
	-@ echo "Variables defined in the top-level GNUmakefile"
	-@ echo "=============================================="
	-@ echo ""
	-@ echo "ALLSYSD  = $(ALLSYSD)"

else
  # Programmer-oriented rules.
  #---------------------------
  help ::
	-@ echo ""
	-@ echo "No programmer-oriented rules are defined in the top-level"
	-@ echo "GNUmakefile."
endif
