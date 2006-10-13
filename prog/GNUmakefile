#-----------------------------------------------------------------------------
# GNUmakefile for compiling Miriad tasks.
#
# Original: 2006/08/19, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.5 2006/10/13 04:26:43 cal103 Exp $
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
# There are two dependency analyses:
#
#   1) Dependencies on included files; this is done for system and programmer.
#   2) Dependencies of executables on object modules in the Miriad library;
#      this is handled here using a list of symbols constructed in the subs
#      GNUmakefile.
#
# System and programmer dependency analysis for included files is essentially
# the same, only using different directories and lists.  Dependency analysis
# for object modules is only done for the system.  
#
# The variables used are:
#
#   DPNDAUXD: Full pathname of the directory where dependency files reside.
#   DPNDINCD: Full pathname of the directory containing include-file
#             dependency lists for each task.
#   DPNDINCL: Full pathname of the concatenation of include-file dependency
#             lists.
#   DPNDLIBR: Blank if the object-module dependency analysis is done,
#             otherwise the name of the Miriad library for use as a
#             dependency.
#   DPNDLOCK: Full pathname of the dependency lock file.
#   DPNDOBJD: Full pathname of the directory containing object-module
#             dependency lists for each task.
#   DPNDOBJL: Full pathname of the concatenation of object-module dependency
#             lists.
#   DPNDSYML: Full pathname of the list of symbols defined by all object
#             modules (constructed by the subs GNUmakefile).
#   DPNDTMPD: Full pathname of the area where intermediate files are staged.
#
#   DPNDINCS: -I include path sequence.
#   DPNDOPT : Compile option, either "OPT" (optimized) or "DBG" (debug).  This
#             is used as a computed variable, e.g. if DPNDOPT is "DBG" then
#             $(FC$(DPNDOPT)) evaluates to $(FCDBG), the debug Fortran compile
#             options set in GNUmakedefs.
#
# Certain of these variables differ for system and programmer compilations and
# are defined separately below.

DPNDINCD  = $(DPNDAUXD)/prog.inc
DPNDINCL  = $(DPNDAUXD)/prog.inclist
DPNDLOCK  = $(DPNDAUXD)/prog.lock
DPNDOBJD  = $(DPNDAUXD)/prog.obj
DPNDOBJL  = $(DPNDAUXD)/prog.objlist

# Dependency analysis for included files:
#----------------------------------------
# The wait-lock command sequence is defined in GNUmakedefs.  f-filter and
# i-filter differ in small but important ways for system and programmer
# compilation and are defined below.

# Command sequence for applying ratty, extracting Fortran dependencies,
# compiling, and linking.
define depend-for
     $(wait-lock)
     $(RATTY) $(RATFLAGS) $(DPNDINCS) $< $(DPNDTMPD)/$*.f
     $(f-filter)
     $(update-list)
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
  DPNDSYML := $(DPNDAUXD)/subs.symlist
  DPNDTMPD := $(MIRTMPD)

  ifeq "$(wildcard $(DPNDINCD))" ""
    $(shell mkdir -m 2775 -p $(DPNDINCD))
  endif

  ifeq "$(wildcard $(DPNDOBJD))" ""
    $(shell mkdir -m 2775 -p $(DPNDOBJD))
  endif

  # System include directory search path and optimized compilation.
  DPNDINCS := $(MIRINCS)
  DPNDOPT  := OPT

  show ::
	-@ echo ""
	-@ echo "DPNDAUXD = $(DPNDAUXD)"
	-@ echo "DPNDINCD = $(DPNDINCD)"
	-@ echo "DPNDINCL = $(DPNDINCL)"
	-@ echo "DPNDLOCK = $(DPNDLOCK)"
	-@ echo "DPNDOBJD = $(DPNDOBJD)"
	-@ echo "DPNDOBJL = $(DPNDOBJL)"
	-@ echo "DPNDSYML = $(DPNDSYML)"
	-@ echo "DPNDTMPD = $(DPNDTMPD)"
	-@ echo ""
	-@ echo "DPNDINCS = $(DPNDINCS)"
	-@ echo "DPNDLIBR = $(DPNDLIBR)"
	-@ echo "DPNDOPT  = $(DPNDOPT)"

  # Dependency analysis for included files:
  #----------------------------------------
  # This command sequence identifies include files in the ratty output and
  # makes each of them a prerequisite of the program's dependency list.  In
  # turn, rule-chaining also makes them a prerequisite of the executable
  # itself.  An example of the type of output it produces is shown below.
  define f-filter
     @ sed -n -e 's#^c >>> $(MIRROOT)/##p' $(DPNDTMPD)/$*.f | $(backsl)
         sed -e 's#^inc#$$(MIRBIND)/$* : $$(MIRINCD)#' $(backsl)
             -e 's#^prog#$$(MIRBIND)/$* : $$(MIRPROGD)#' | $(backsl)
         sort -u > $(DPNDINCD)/$*
  endef

  # Dependency analysis for object modules:
  #----------------------------------------
  # The nm-syms command sequence must be defined in makedefs to list the
  # global symbols defined by an object module.  If undefined this analysis is
  # skipped and each executable is made to depend on the library as a whole.
  ifneq "$(nm-syms.$(MIRARCH))" ""
    define depend-objs
    -@ mkdir -p $(DPNDOBJD)
    -@ $(RM) $(DPNDOBJD)/$*
    -@ OBJ="$@" ; $(nm-syms.$(MIRARCH)) | $(backsl)
         sort | $(backsl)
         sort -m - $(DPNDSYML) | $(backsl)
         awk $(backsl)
           'BEGIN {sym = ""} $(backsl)
            NF == 1 {sym = $$1} $(backsl)
            NF == 2 {if ($$1 == sym) print $$2}' | $(backsl)
         sort -u | $(backsl)
         sed -e 's|\(.*\)|$$(MIRBIND)/$* : $$(MIRLIBR)(\1)|' > $(DPNDOBJD)/$*
     -@ $(RM) $(DPNDOBJL)
      @ cat $(DPNDOBJD)/* > $(DPNDOBJL)
    endef

    DPNDLIBR :=
  else
    define depend-objs
    endef

    DPNDLIBR := $(MIRLIBR)
  endif


  # Pattern rules (rule-chaining, and dependency analysis).
  #--------------------------------------------------------
  # Dependency analysis for Miriad tasks is simpler in some respects than for
  # Miriad subroutines and functions because it need only deal with Fortran
  # source code and there are no architecture-specific variants.  Therefore
  # the possibilities for rule-chaining are limited.
  #
  # As an example, in linux, the dependency chain for atlod would look like
  #
  #   $MIR/linux/bin/atlod <- $MIR/linux/tmp/atlod.f and
  #                           $MIR/linux/aux/prog.inc/atlod
  #                        <- $MIR/prog/atlod.for
  #
  # The chain is evaluated in reverse order, starting with
  # $MIR/prog/atlod.for, as indicated by the arrows.
  #
  # In the first step make generates atlod's dependency list, stored in a
  # subdirectory as $MIR/linux/aux/prog.inc/atlod for tidiness, by running
  # ratty to generate $MIR/linux/tmp/atlod.f.  It uses atlod.f to determine
  # atlod's dependencies and adds these to the full include-file dependency
  # list, $MIR/linux/aux/prog.inclist, which is included at the end of this
  # makefile.
  #
  # atlod.f and atlod's dependency list are therefore generated by the same
  # rule.  The second and final step compiles and links atlod.f to produce
  # $MIR/linux/bin/atlod.  Global symbols are extracted from the executable
  # and compared with the list of global symbols defined by the object modules
  # in the Miriad library to generate the object-module dependency list.  The
  # executable is then (optionally) stripped of symbols.
  #
  # This mechanism is efficient because ratty always has to be run when
  # compiling Miriad Fortran so the only overhead is in scanning atlod.f for
  # included files.
  #
  # atlod's include-file dependency list consists of the following entries in
  # $MIR/linux/aux/prog.inclist:
  #
  #   $(MIRBIND)/atlod : $(MIRINCD)/maxdim.h
  #   $(MIRBIND)/atlod : $(MIRINCD)/mirconst.h
  #   $(MIRBIND)/atlod : $(MIRINCD)/rpfits.inc
  #   $(MIRBIND)/atlod : $(MIRPROGD)/atlod.h
  #
  # This means that if maxdim.h is touched then make will consider that
  # atlod's dependency list is out-of-date and needs to be remade.  Similarly
  # if atlod.for was touched.
  #
  # make automatically deletes the intermediate products of the rule-chain
  # except for those marked as a dependency of the pseudo-rule ".PRECIOUS"
  # This is used to protect the dependency lists themselves.

  .PRECIOUS : $(DPNDINCD)/%

  $(MIRBIND)/% : $(DPNDTMPD)/%.f $(DPNDINCD)/% $(DPNDLIBR)
	   $(FC) $(FCOPT) $(LDOPT) -o $@ $< $(MIRLIBR) $(XTRNLIBS)
	   $(depend-objs)
	   $(wait-unlock)
	-@ $(STRIP) $@
	-@ chmod 775 $@
	-@ $(RM) $<

  $(DPNDTMPD)/%.f $(DPNDINCD)/% : %.for
	   $(depend-for)

  # This little rule tells make not to fret if it can't find a header file.
  # It may be important if a header file is deleted but remains as a
  # prerequisite for some of the object modules in the include-file dependency
  # list.
  %.h : ;

  # Extract preamble comments into a Miriad "doc" file.
  $(MIRDOCD)/%.doc : %.for
	-@ $(RM) $(MIRDOCD)/$*.doc
	   $(MIRBIND)/doc -f $< > $@
	-@ chmod 664 $@


  # Static and static pattern rules.
  #---------------------------------
  .PHONY : bin dependsys

  allsys :: bin doc

  bin : $(MIREXES:%=$(MIRBIND)/%)

  doc : $(MIREXES:%=$(MIRDOCD)/%.doc) ;

  depend : $(MIREXES:%=$(DPNDINCD)/%)
	$(RM) $(DPNDTMPD)/*.f

  cleansys ::
	$(RM) $(DPNDTMPD)/*.f
	$(RM) -r $(DPNDAUXD)/prog.*

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

  ifeq "$(wildcard $(DPNDINCD))" ""
    $(shell mkdir -p $(DPNDINCD))
    $(shell chmod u+rwx $(DPNDINCD))
  endif

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
	-@ echo "DPNDINCD = $(DPNDINCD)"
	-@ echo "DPNDINCL = $(DPNDINCL)"
	-@ echo "DPNDLOCK = $(DPNDLOCK)"
	-@ echo "DPNDTMPD = $(DPNDTMPD)"
	-@ echo ""
	-@ echo "DPNDINCS = $(DPNDINCS)"
	-@ echo "DPNDOPT  = $(DPNDOPT)"

  # This command sequence identifies include files in the ratty output and
  # makes each of them a prerequisite of the object module's dependency list.
  # In turn, rule-chaining also makes them a prerequisite of the object module
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
  # as a prerequisite in the programmer include-file dependency list but is
  # not needed in the system list.
  define f-filter
     @ echo $< | $(backsl)
         sed -e 's#^\([^/]\)#$(THISDIR)/\1#' | $(backsl)
         cat - $(DPNDTMPD)/$*.f | $(backsl)
         sed -n -e '{1p;s#^c >>> ##p;}' | $(backsl)
         sed -e 's#^$(PGMRROOT)/#$$(PGMRBIND)/$* : $$(PGMRROOT)/#' $(backsl)
             -e 's#^$(MIRROOT)/.*/#$$(PGMRBIND)/$* : #' | $(backsl)
         sort -u > $(DPNDINCD)/$*
  endef


  # Pattern rules.
  #---------------
  # These are essentially the same as for system compilation.
  .PRECIOUS : $(DPNDINCD)/% $(PGMRBIND)/% $(DPNDTMPD)/%.f

  % : $(PGMRBIND)/% ;

  # Making the dependency list a prerequisite of the Fortran executable
  # prevents rule-chaining from stopping prematurely at this point.  This
  # could happen if a .f file preserved from a previous compilation is newer
  # than the target though constructed from header files that are now stale.
  # The stale header files will be listed as prerequisites of the dependency
  # list in the full include-file dependency list included at the end of this
  # makefile.
  $(PGMRBIND)/% : $(DPNDTMPD)/%.f $(DPNDINCD)/% $(PGMRLIBR) $(MIRLIBR)
	   $(FC) $(FCDBG) $(LDDBG) -o $@ $< $(PGMRLIBR) $(MIRLIBR) $(XTRNLIBS)
	   $(wait-unlock)
	-@ chmod 775 $@
  
  $(DPNDTMPD)/%.f $(DPNDINCD)/% : %.for
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
	   $(MIRBIND)/doc -f $< > $@
	-@ chmod 664 $@


  # Static and static pattern rules.
  #---------------------------------
  .PHONY : depend doc

  all :: $(addprefix $(PGMRBIND)/, $(PGMREXES))

  doc : $(PGMREXES:%=%.doc) ;

  depend : $(PGMREXES:%=$(DPNDINCD)/%) ;

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

# Dependency lists.  GNU make allows wildcarding in the include file
# specification so it should be possible to include $(DPNDINCD)/*.  However,
# it then apparently considers that the .f intermediary of the
# "$(DPNDTMPD)/%.f $(DPNDINCD)/%" pattern rule "ought to exist" and recompiles
# the executable if it doesn't.
-include $(DPNDINCL)
-include $(DPNDOBJL)
