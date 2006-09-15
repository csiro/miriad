#-----------------------------------------------------------------------------
# GNUmakefile used to compile special Miriad programs.
#
# Original: 2006/09/15, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.1 2006/09/15 07:36:10 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

ifeq "$(MAKEMODE)" "system"
  # Subdirectories in which to invoke "allsys", in order.
  #------------------------------------------------------
  ALLSYSD  := xpanel xmtv sxmtv

  # Static and static pattern rules.
  #---------------------------------
  allsys :: sysdirs $(ALLSYSD)

  show ::
	-@ echo ""
	-@ echo "Variables defined in the spec GNUmakefile"
	-@ echo "========================================="
	-@ echo ""
	-@ echo "ALLSYSD  = $(ALLSYSD)"

else
  # Programmer-oriented rules.
  #---------------------------
  help ::
	-@ echo ""
	-@ echo "No programmer-oriented rules are defined in the spec"
	-@ echo "GNUmakefile."
endif
