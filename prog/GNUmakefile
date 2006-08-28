#-----------------------------------------------------------------------------
# GNUmakefile for compiling Miriad tasks.
#
# Original: 2006/08/19, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.1 2006/08/28 01:56:40 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

MIRINCS  := -I $(MIRPROGD) -I $(MIRINCD)
MIRLIBR  := $(MIRLIBD)/libmir.a
XTRNLIBS := $(RPFITSLIB) $(LINPACKLIB) $(PGPLOTLIB) $(X11LIB)
MIREXES  := $(basename $(filter %.for,$(MIRSRCS)))

# Dependency analysis.
#---------------------
# See below for an explanation of the dependency analysis scheme.
DPNDLIST := $(MIRAUXD)/prog.list
DPNDTIME := $(MIRAUXD)/prog.time
DPNDLOCK := $(MIRAUXD)/prog.lock

# Command sequence for applying ratty, extracting Fortran dependencies,
# compiling, and linking.  The wait-lock and update-time command sequences are
# defined in GNUmakedefs.
define depend-for
     $(wait-lock)
     $(RATTY) $(RATFLAGS) $(MIRINCS) $< $(MIRTMPD)/$*.f
   @ sed -n -e 's#^c >>> $(MIRROOT)/##p' $(MIRTMPD)/$*.f | $(backsl)
       sort -u | $(backsl)
       sed -e 's#^inc#$$(DPNDTIME)($*) : $$(MIRINCD)#' $(backsl)
           -e 's#^prog#$$(DPNDTIME)($*) : $$(MIRPROGD)#' | $(backsl)
       sort -m - $(DPNDLOCK) > $(DPNDLIST)
     $(FC) $(FCOPT) $(LDOPT) -o $(MIRBIND)/$* $(MIRTMPD)/$*.f $(MIRLIBR) $(XTRNLIBS)
  -@ if [ -f "$(MIRBIND)/$*" ] ; then $(backsl)
       chmod 775 $(MIRBIND)/$* ; $(backsl)
     else $(backsl)
       $(RM) $(DPNDLOCK) ; $(backsl)
     fi
     $(update-time)
endef


# Programmer variables.
#----------------------
ifeq "$(MAKEMODE)" "programmer"
  VPATH    := .:$(CODEDIR)

  PGMRINCS := -I . -I $(PGMRINCD) $(MIRINCS)
  PGMRLIBR := $(wildcard $(PGMRLIBD)/libmir.a)
  PGMRLIBS := $(wildcard $(PGMRLIBD)/libmir.a) $(XTRNLIBS)
  PGMREXES := $(basename $(wildcard *.for))
endif 

# Pattern rules (rule-chaining, and dependency analysis).
#--------------------------------------------------------
# Dependency analysis for Miriad tasks is simpler in some respects than for
# Miriad subroutines and functions because there are no .f2c or .c sources
# so the requirement for rule-chaining is limited.
#
# As an example, the dependency chain for atlod would look like
#
#   $MIR/linux/bin/atlod <- $MIR/linux/aux/prog.time(atlod)
#                        <- $MIR/prog/atlod.for
#
# The chain is evaluated in reverse order, starting with $MIR/prog/atlod.for,
# as indicated by the arrows.
#
# The first step essentially does everything leaving the second step as a
# no-op. make generates atlod's dependency timestamp, which for tidiness is
# stored in an archive library as $MIR/linux/aux/prog.time(atlod), by running
# ratty to generate $MIR/linux/tmp/atlod.f.  It uses atlod.f to determine
# atlod's dependencies, adds these to $MIR/linux/aux/prog.list, which is
# included at the end of this makefile, and then touches atlods's timstamp.
# Finally it compiles and links atlod.f to produce $MIR/linux/bin/atlod.
#
# This mechanism is efficient because ratty always has to be run when
# compiling Miriad Fortran so the only overhead is in scanning atlod.f for
# included files.
#
# atlod's dependency list consists of the following entries in
# $MIR/linux/aux/prog.list (where $(DPNDTIME) is the makefile variable used
# for $MIR/linux/aux/prog.time):
#
#   $(DPNDTIME)(atlod) : $(MIRINCD)/maxdim.h
#   $(DPNDTIME)(atlod) : $(MIRINCD)/mirconst.h
#   $(DPNDTIME)(atlod) : $(MIRINCD)/rpfits.inc
#   $(DPNDTIME)(atlod) : $(MIRPROGD)/atlod.h
#
# This means that if maxdim.h is touched then make will consider that atlod's
# timestamp is out-of-date and needs to be remade.  Similarly if atlod.for was
# touched.

$(MIRBIND)/% : $(DPNDTIME)(%) ;

$(DPNDTIME)(%) : $(CODEDIR)/%.for $(MIRLIBR)
	$(depend-for)

ifeq "$(MAKEMODE)" "programmer"
  % : $(PGMRBIND)/% ;

  $(PGMRBIND)/% : $(PGMRBIND)/%.f $(PGMRLIBR) $(MIRLIBR)
	$(FC) $(FCDBG) -o $@ $< $(PGMRLIBS)

  $(PGMRBIND)/%.f : %.for
	$(RATTY) $(RATFLAGS) $(PGMRINCS) $< $@

  # For convenience only.
  %.f : $(PGMRBIND)/%.f
	-@ $(RM) $@
	   ln -s $< $@
endif


# Static and static pattern rules.
#---------------------------------
.PRECIOUS : $(DPNDTIME)(%)

.PHONY : bin dependsys

allsys :: bin

bin : $(MIREXES:%=$(MIRBIND)/%) ;

$(DPNDLIST) : dependsys ;

dependsys : $(MIREXES:%=$(DPNDTIME)(%)) ;

cleansys ::
	$(RM) $(MIRAUXD)/prog.*

show ::
	-@ echo ""
	-@ echo "Variables defined in the prog GNUmakefile"
	-@ echo "========================================="
	-@ echo ""
	-@ echo "MIRINCS  = $(MIRINCS)"
	-@ echo "MIRLIBR  = $(MIRLIBR)"
	-@ echo "XTRNLIBS = $(XTRNLIBS)"
	-@ echo "MIREXES  = $(MIREXES)"
	-@ echo ""
	-@ echo "DPNDLIST = $(DPNDLIST)"
	-@ echo "DPNDTIME = $(DPNDTIME)"
	-@ echo "DPNDLOCK = $(DPNDLOCK)"

help ::
	-@ echo ""
	-@ echo "Targets defined in the prog makefile"
	-@ echo "===================================="
	-@ echo ""
	-@ echo "System"
	-@ echo "------"
	-@ echo "        bin: compile and link all programs."
	-@ echo "  dependsys: regenerate dependency lists."


ifeq "$(MAKEMODE)" "programmer"
  .PRECIOUS : $(PGMRBIND)/% $(PGMRBIND)/%.f

  all :: $(addprefix $(PGMRBIND)/, $(PGMREXES))

  clean ::
	$(RM) *.o

  cleaner :: clean
	$(RM) $(PGMRBIND)/*.[fo]
	find . -maxdepth 1 -name "*.f" -type l -exec $(RM) {} \;

  cleanest :: clean
	$(RM) $(PGMRBIND)/*

  show ::
	-@ echo ""
	-@ echo "VPATH    = $(VPATH)"
	-@ echo "PGMRINCS = $(PGMRINCS)"
	-@ echo "PGMRLIBS = $(PGMRLIBS)"
	-@ echo "PGMREXES = $(PGMREXES)"
endif

# Hand-code the few executables that use RPFITS.
#$(MIRBIND)/atlod   : $(RPFITSLIB)
#$(MIRBIND)/fakeotf : $(RPFITSLIB)
#$(MIRBIND)/mopfix  : $(RPFITSLIB)
#$(MIRBIND)/rpfread : $(RPFITSLIB)
#$(MIRBIND)/wblod   : $(RPFITSLIB)

ifndef NODEP
  -include $(DPNDLIST)
endif
