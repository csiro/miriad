#-----------------------------------------------------------------------------
# GNUmakefile for compiling Miriad tasks.
#
# Original: 2006/08/19, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.2 2006/09/15 07:50:14 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

MIRINCS  := -I $(MIRPROGD) -I $(MIRINCD)
MIRLIBR  := $(MIRLIBD)/libmir.a
XTRNLIBS := $(RPFITSLIB) $(LINPACKLIB) $(PGPLOTLIB) $(X11LIB) $(XTRALIBS)
MIREXES  := $(basename $(filter %.for,$(MIRSRCS)))

show ::
	-@ echo ""
	-@ echo "Variables defined in the prog GNUmakefile"
	-@ echo "========================================="
	-@ echo ""
	-@ echo "MIRINCS  = $(MIRINCS)"
	-@ echo "MIRLIBR  = $(MIRLIBR)"
	-@ echo "XTRNLIBS = $(XTRNLIBS)"
	-@ echo "MIREXES  = $(MIREXES)"


# Dependency analysis.
#---------------------
# See below for a detailed explanation of the dependency analysis scheme.
# Dependency analysis for Miriad programs only catches changed include files,
# not modified object modules in the Miriad library.  For this reason the
# executables are made to depend on the library as a whole.  System and
# programmer dependency analysis is essentially the same, only using different
# directories and timestamps.  The variables used are
#
#   DPNDAUXD: Full pathname of the directory where dependency files reside.
#   DPNDLIST: Full pathname of the dependency list.
#   DPNDTIME: Full pathname of the dependency timestamp archive.
#   DPNDLOCK: Full pathname of the dependency lock file.
#   DPNDTMPD: Full pathname of the area where intermediate files are staged.
#   DPNDINCS: Sequence of -I include path.
#   DPNDOPT : Compile option, either "OPT" (optimized) or "DBG" (debug).  This
#             is used as a computed variable, e.g. if DPNDOPT is "DBG" then
#             $(FC$(DPNDOPT)) evaluates to $(FCDBG), the debug Fortran compile
#             options set in GNUmakedefs.
#
# These variables differ for system and programmer compilations and are
# defined separately below.

DPNDLIST  = $(DPNDAUXD)/prog.list
DPNDTIME  = $(DPNDAUXD)/prog.time
DPNDLOCK  = $(DPNDAUXD)/prog.lock

# The wait-lock and update-time command sequences are defined in GNUmakedefs.
# f-filter and i-filter differ in small but important ways for system and
# programmer compilation and are defined below.

# Command sequence for applying ratty, extracting Fortran dependencies,
# compiling, and linking.
define depend-for
     $(wait-lock)
     $(RATTY) $(RATFLAGS) $(DPNDINCS) $< $(DPNDTMPD)/$*.f
     $(f-filter)
     $(update-time)
endef


ifeq "$(MAKEMODE)" "system"
  # System-oriented rules.
  #-----------------------
  # VPATH is not needed for system compilation because the source code resides
  # in the current directory except for header files in $(MIRINCD).  These are
  # specified via a full pathname in the dependency list and are located by
  # ratty via the -I include paths.

  # Dependency files go into the system aux directory, while preprocessor
  # output is staged in the tmp directory and deleted after use.
  DPNDAUXD := $(MIRAUXD)
  DPNDTMPD := $(MIRTMPD)

  # System include directory search path and optimized compilation.
  DPNDINCS := $(MIRINCS)
  DPNDOPT  := OPT

  show ::
	-@ echo ""
	-@ echo "DPNDAUXD = $(DPNDAUXD)"
	-@ echo "DPNDLIST = $(DPNDLIST)"
	-@ echo "DPNDTIME = $(DPNDTIME)"
	-@ echo "DPNDLOCK = $(DPNDLOCK)"
	-@ echo "DPNDTMPD = $(DPNDTMPD)"
	-@ echo ""
	-@ echo "DPNDINCS = $(DPNDINCS)"
	-@ echo "DPNDOPT  = $(DPNDOPT)"

  # This command sequence identifies include files in the ratty output and
  # makes each of them a prerequisite of the program's timestamp.  In turn,
  # rule-chaining also makes them a prerequisite of the executable itself.
  # An example of the type of output it produces is shown below.
  define f-filter
     @ sed -n -e 's#^c >>> $(MIRROOT)/##p' $(DPNDTMPD)/$*.f | $(backsl)
         sed -e 's#^inc#$$(DPNDTIME)($*) : $$(MIRINCD)#' $(backsl)
             -e 's#^prog#$$(DPNDTIME)($*) : $$(MIRPROGD)#' | $(backsl)
         sort -u | sort -m - $(DPNDLOCK) > $(DPNDLIST)
  endef


  # Pattern rules (rule-chaining, and dependency analysis).
  #--------------------------------------------------------
  # Dependency analysis for Miriad tasks is simpler in some respects than for
  # Miriad subroutines and functions because it need only deal with Fortran
  # source code, and there are no architecture-specific variants.  Therefore
  # the possibilities for rule-chaining are limited.
  #
  # As an example, the dependency chain for atlod would look like
  #
  #   $MIR/linux/bin/atlod <- $MIR/linux/aux/prog.time(atlod) and
  #                           $MIR/linux/tmp/atlod.f
  #                        <- $MIR/prog/atlod.for
  #
  # The chain is evaluated in reverse order, starting with
  # $MIR/prog/atlod.for, as indicated by the arrows.
  #
  # In the first step make generates atlod's dependency timestamp, stored in
  # archive library $MIR/linux/aux/prog.time(atlod) for tidiness, by running
  # ratty to generate $MIR/linux/tmp/atlod.f.  It uses atlod.f to determine
  # atlod's dependencies, adds these to $MIR/linux/aux/prog.list, which is
  # included at the end of this makefile, and then touches atlod's timestamp.
  #
  # atlod.f and atlod's timestamp are therefore generated by the same rule.
  # The second and final step compiles and links atlod.f to produce
  # $MIR/linux/bin/atlod.
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
  # This means that if maxdim.h is touched then make will consider that
  # atlod's timestamp is out-of-date and needs to be remade.  Similarly if
  # atlod.for was touched.
  #
  # make automatically deletes the intermediate products of the rule-chain
  # except for those marked as a dependency of the pseudo-rule ".PRECIOUS"
  # This is used to protect the dependency timestamps themselves.

  .PRECIOUS : $(DPNDTIME)(%)

  $(MIRBIND)/% : $(DPNDTMPD)/%.f $(MIRLIBR)
	   $(FC) $(FCOPT) $(LDOPT) -o $@ $< $(MIRLIBR) $(XTRNLIBS)
	-@ chmod 775 $@
	-@ $(RM) $<

  $(DPNDTIME)(%) $(DPNDTMPD)/%.f : %.for
	   $(depend-for)

  # This little rule tells make not to fret if it can't find a header file.
  # It may be important if a header file is deleted but remains as a
  # prerequisite for some of the object modules in the dependency list.
  %.h : ;

  # Extract preamble comments into a Miriad "doc" file.
  $(MIRDOCD)/%.doc : %.for
	-@ $(RM) $(MIRDOCD)/$*.doc
	   $(MIRDOC) -f $< > $@
	-@ chmod 664 $@


  # Static and static pattern rules.
  #---------------------------------
  .PHONY : bin dependsys

  allsys :: bin doc

  bin : $(MIREXES:%=$(MIRBIND)/%)
	-@ echo ""

  doc : $(MIREXES:%=$(MIRDOCD)/%.doc) ;

  depend : $(MIREXES:%=$(DPNDTIME)(%))
	$(RM) $(DPNDTMPD)/*.f

  cleansys ::
	$(RM) $(DPNDTMPD)/*.f
	$(RM) $(DPNDAUXD)/prog.*

  help ::
	-@ echo ""
	-@ echo "Targets defined in the prog makefile"
	-@ echo "===================================="
	-@ echo ""
	-@ echo "System"
	-@ echo "------"
	-@ echo "     allsys: compile and link all programs."
	-@ echo "        bin: compile and link all programs."
	-@ echo "        doc: generate doc files for all programs."
	-@ echo "     depend: regenerate dependency lists."

else
  # Programmer-oriented rules.
  #---------------------------
  # Look for source files in the current directory first where a programmer
  # may be working on them, then in the relevant Miriad directory.
  VPATH    := .:$(CODEDIR):$(PGMRINCD):$(MIRINCD)

  # Tell ratty to look for include files in the programmer directories first,
  # then in the relevant Miriad directories.
  PGMRINCS := -I . -I $(PGMRINCD) $(MIRINCS)
  PGMRLIBR := $(wildcard $(PGMRLIBD)/libmir.a)
  PGMRLIBS := $(wildcard $(PGMRLIBD)/libmir.a) $(XTRNLIBS)
  PGMREXES := $(basename $(wildcard *.for))

  # Dependency files go into the programmer's aux directory, while ratty
  # output is preserved in the bin directory where the debugger can find it.
  DPNDAUXD := $(PGMRAUXD)
  DPNDTMPD := $(PGMRBIND)

  # Programmer include directory search path and debug compilation.
  DPNDINCS := $(PGMRINCS)
  DPNDOPT  := DBG

  show ::
	-@ echo ""
	-@ echo "VPATH    = $(VPATH)"
	-@ echo "PGMRINCS = $(PGMRINCS)"
	-@ echo "PGMRLIBS = $(PGMRLIBS)"
	-@ echo "PGMREXES = $(PGMREXES)"
	-@ echo ""
	-@ echo "DPNDAUXD = $(DPNDAUXD)"
	-@ echo "DPNDLIST = $(DPNDLIST)"
	-@ echo "DPNDTIME = $(DPNDTIME)"
	-@ echo "DPNDLOCK = $(DPNDLOCK)"
	-@ echo "DPNDTMPD = $(DPNDTMPD)"
	-@ echo ""
	-@ echo "DPNDINCS = $(DPNDINCS)"
	-@ echo "DPNDOPT  = $(DPNDOPT)"

  # This command sequence identifies include files in the ratty output and
  # makes each of them a prerequisite of the object module's timestamp.  In
  # turn, rule-chaining also makes them a prerequisite of the object module
  # itself.
  #
  # It differs in several important ways from the system version.  Header
  # files in the programmer directories listed as prerequisites are specified
  # via a full pathname.  If one of these header files was subsequently
  # deleted the prerequisite would not be satisfied and a recompile forced,
  # whereupon the checked-in Miriad file would be found via VPATH.
  #
  # On the other hand, header files in the Miriad directories are given
  # without pathname.  If one of these was subsequently checked out into a
  # programmer directory it would be found first via VPATH and, being newer,
  # would force a recompile.
  #
  # For similar reasons the source file itself (.for) must also be mentioned
  # as a prerequisite in the programmer dependency list but is not needed in
  # the system list.
  define f-filter
     @ echo $< | $(backsl)
         sed -e 's#^\([^/]\)#$(THISDIR)/\1#' | $(backsl)
         cat - $(DPNDTMPD)/$*.f | $(backsl)
         sed -n -e '{1p;s#^c >>> ##p;}' | $(backsl)
         sed -e 's#^$(PGMRROOT)/#$$(DPNDTIME)($*) : $$(PGMRROOT)/#' $(backsl)
             -e 's#^$(MIRROOT)/.*/#$$(DPNDTIME)($*) : #' | $(backsl)
         sort -u | sort -m - $(DPNDLOCK) > $(DPNDLIST)
  endef


  # Pattern rules.
  #---------------
  # These are essentially the same as for system compilation.
  .PRECIOUS : $(DPNDTIME)(%) $(PGMRBIND)/% $(DPNDTMPD)/%.f

  % : $(PGMRBIND)/% ;

  # Making the timestamp a prerequisite of the Fortran executable prevents
  # rule-chaining from stopping prematurely at this point.  This could happen
  # if a .f file preserved from a previous compilation is newer than the
  # target though constructed from header files that are now stale.  The stale
  # header files will be listed as prerequisites of the timestamp in the
  # dependency list included at the end of this makefile.
  $(PGMRBIND)/% : $(DPNDTMPD)/%.f $(DPNDTIME)(%) $(PGMRLIBR) $(MIRLIBR)
	   $(FC) $(FCDBG) $(LDDBG) -o $@ $< $(PGMRLIBR) $(MIRLIBR) $(XTRNLIBS)
	-@ chmod 775 $@
  
  $(DPNDTIME)(%) $(DPNDTMPD)/%.f : %.for
	   $(depend-for)

  # Don't worry if a source file in the dependency list can't be located.
  $(THISDIR)/%.for : ;

  # Don't worry if a header file in the dependency list can't be located.
  %.h : ;

  # Convenience rules for generating preprocessor output.
  %.f : $(DPNDTMPD)/%.f ;

  # Extract preamble comments into a Miriad "doc" file.
  %.doc : %.for
	-@ $(RM) $*.doc
	   $(MIRDOC) -f $< > $@
	-@ chmod 664 $@


  # Static and static pattern rules.
  #---------------------------------
  .PHONY : depend doc

  all :: $(addprefix $(PGMRBIND)/, $(PGMREXES))

  doc : $(PGMREXES:%=%.doc) ;

  depend : $(PGMREXES:%=$(DPNDTIME)(%)) ;

  clean ::
	$(RM) *.o
	$(RM) $(DPNDTMPD)/*.o
	$(RM) $(DPNDLOCK)

  cleaner :: clean
	$(RM) *.doc
	$(RM) $(DPNDTMPD)/*.f
	$(RM) $(DPNDAUXD)/prog.*

  cleanest :: clean
	$(RM) $(PGMRBIND)/*

  help ::
	-@ echo ""
	-@ echo "Targets defined in the prog GNUmakefile"
	-@ echo "---------------------------------------"
	-@ echo "        all: compile sources in this directory only."
	-@ echo "        doc: extract doc comments from sources in this"
	-@ echo "             directory only."
	-@ echo "     depend: update programmer dependency lists."
	-@ echo "        %.f: generate ratty preprocessor output."
	-@ echo "          %: compile and link %.for."
	-@ echo "      %.doc: extract doc comments from %.for."
endif

# Hand-code the few executables that use RPFITS.
$(MIRBIND)/atlod   : $(RPFITSLIB)
$(MIRBIND)/fakeotf : $(RPFITSLIB)
$(MIRBIND)/mopfix  : $(RPFITSLIB)
$(MIRBIND)/rpfread : $(RPFITSLIB)
$(MIRBIND)/wblod   : $(RPFITSLIB)

ifndef NODEP
  -include $(DPNDLIST)
endif
