#-----------------------------------------------------------------------------
# GNUmakefile used to compile Miriad.
#
# Original: 2006/08/28, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.38 2011/08/17 05:25:36 cal103 Exp $
#-----------------------------------------------------------------------------
ifeq "$(MIR)" ""
  # Try to deduce basic Miriad environment variables.  Obviously this only
  # works if make is invoked in the top-level Miriad directory.
  export MIR     := $(shell pwd)
  export MIRARCH := $(shell $(MIR)/scripts/mirarch)
  export MIRBIN  := $(MIR)/$(MIRARCH)/bin
endif

# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

ifeq "$(MAKEMODE)" "system"
  # Subdirectories in which to invoke "allsys", in order.
  #------------------------------------------------------
  ALLSYSD  += $(findstring linpack,$(SUBDIRS))
  ALLSYSD  += tools scripts inc subs prog spec guides

  show ::
	-@ echo ""
	-@ echo "Variables defined in the top-level GNUmakefile"
	-@ echo "=============================================="
	-@ echo ""
	-@ echo "ALLSYSD  = $(ALLSYSD)"

  # Static and static pattern rules.
  #---------------------------------
  .PHONY : initial

  allsys :: initial MIRRC MIRRC.sh $(ALLSYSD)

  # Announce what we're about to do.
  initial :: FORCE
	-@ echo ""
	-@ echo "Rebuilding/updating Miriad for $(MIRARCH) machines."
	-@ $(TIMER)

  MIRRC : scripts/MIRRC.in
	-@ echo ""
	   sed -e "s,@MIRROOT@,$(MIR)," $< > $@
	-@ chmod 644 $@

  MIRRC.sh : scripts/MIRRC.sh.in
	-@ echo ""
	   sed -e "s,@MIRROOT@,$(MIR)," $< > $@
	-@ chmod 644 $@

  cleansys ::
	   $(RM) -r $(MIRTMPD)/*

  help ::
	-@ echo ""
	-@ echo "Targets defined in the top-level GNUmakefile"
	-@ echo "--------------------------------------------"
	-@ echo "     allsys: recursively rebuild or update Miriad."

  ifdef MIRATNF
    # Software management rules for ATNF use only.
    -include $(MIR)/GNUatnfdefs
  endif

else
  # Programmer-oriented rules.
  #---------------------------
  help ::
	-@ echo ""
	-@ echo "No programmer-oriented rules are defined in the top-level"
	-@ echo "GNUmakefile."
endif
