#-----------------------------------------------------------------------------
# GNUmakefile for compiling the Miriad subroutine library.  Does full
# dependency analysis for all source file types.
#
# Original: 2006/08/19, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.4 2006/09/26 05:29:23 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

# Add architecture-specific source files.  We assume that there are no header
# files in these directories.
MIROS1D  := $(MIRSUBSD)/unix
MIROSRCS := $(notdir $(wildcard $(MIROS1D)/*.*))

MIROS2D  :=
ifneq "$(SUBS$(ARCH))" ""
  MIROS2D  := $(MIROS1D)/$(SUBS$(ARCH))
  MIROSRCS := $(sort $(MIROSRCS) $(notdir $(wildcard $(MIROS2D)/*.*)))
endif

MIRINCS  := -I $(MIRSUBS) -I $(MIRINC)
MIRLIBR  := $(MIRLIBD)/libmir.a
MIROBJS  := $(subst .for,.o,$(filter %.for,$(MIRSRCS) $(MIROSRCS)))
MIROBJS  += $(subst .f2c,.o,$(filter %.f2c,$(MIRSRCS) $(MIROSRCS)))
MIROBJS  += $(subst   .c,.o,$(filter   %.c,$(MIRSRCS) $(MIROSRCS)))
MIROBJS  := $(sort $(MIROBJS))

show ::
	-@ echo ""
	-@ echo "Variables defined in the subs GNUmakefile"
	-@ echo "========================================="
	-@ echo ""
	-@ echo "MIROS1D  = $(MIROS1D)"
	-@ echo "MIROS2D  = $(MIROS2D)"
	-@ echo "MIROSRCS = $(MIROSRCS)"
	-@ echo ""
	-@ echo "MIRINCS  = $(MIRINCS)"
	-@ echo "MIRLIBR  = $(MIRLIBR)"
	-@ echo "MIROBJS  = $(MIROBJS)"


# Dependency analysis.
#---------------------
# See below for a detailed explanation of the dependency analysis scheme.
# There are two dependency analyses:
#
#   1) Dependencies on included files; this is done for system and programmer.
#   2) Dependencies of executables on object modules in the Miriad library;
#      this is handled by the prog GNUmakefile using a list of symbols
#      constructed here.
#
# System and programmer dependency analysis for included files is essentially
# the same, only using different directories and timestamps.  Dependency
# analysis for object modules is only done for the system.  
#
# The variables used are:
#
#   DPNDAUXD: Full pathname of the directory where dependency files reside.
#   DPNDINCL: Full pathname of the dependency list for included files.
#   DPNDSYMS: Full pathname of the list of symbols defined by the object
#             modules.
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

DPNDINCL  = $(DPNDAUXD)/subs.incl
DPNDLOCK  = $(DPNDAUXD)/subs.lock
DPNDSYMS  = $(DPNDAUXD)/subs.syms
DPNDTIME  = $(DPNDAUXD)/subs.time

# Dependency analysis for included files:
#----------------------------------------
# The wait-lock and update-time command sequences are defined in GNUmakedefs.
# f-filter and i-filter differ in small but important ways for system and
# programmer compilation and are defined below.

# Command sequence for applying ratty, extracting Fortran dependencies, and
# compiling.
define depend-for
     $(wait-lock)
     $(RATTY) $(RATFLAGS) $(DPNDINCS) $< $(DPNDTMPD)/$*.f
     $(f-filter)
     $(update-time)
endef

# Command sequence for applying intf2c followed by cpp, extracting C
# dependencies from cpp output, and compiling.
define depend-f2c
     $(wait-lock)
     $(INTF2C) $(F2CFLAGS) $< $(DPNDTMPD)/$*.c
  -@ $(RM) $(DPNDTMPD)/$*.i
     $(CPP) $(CPP$(DPNDOPT)) $(DPNDINCS) $(DPNDTMPD)/$*.c > $(DPNDTMPD)/$*.i
     $(depend-i)
endef

# Command sequence for applying cpp, extracting C dependencies from cpp
# output, and compiling.
define depend-c
     $(wait-lock)
  -@ $(RM) $(DPNDTMPD)/$*.i
     $(CPP) $(CPP$(DPNDOPT)) $(DPNDINCS) $< > $(DPNDTMPD)/$*.i
     $(depend-i)
endef

# Command sequence for extracting C dependencies from cpp output and
# compiling.
define depend-i
     $(i-filter)
     $(update-time)
endef


ifeq "$(MAKEMODE)" "system"
  # System-oriented rules.
  #-----------------------
  # Resolve paths for operating-system specific source code.
  ifneq "$(MIROS2D)" ""
    VPATH    := $(MIROS2D):
  endif
  VPATH    := $(VPATH)$(MIROS1D):$(CODEDIR):$(MIRINCD)

  # Dependency files go into the system aux directory, while preprocessor
  # output is staged in the tmp directory and deleted after use.
  DPNDAUXD := $(MIRAUXD)
  DPNDTMPD := $(MIRTMPD)

  # System include directory search path and optimized compilation.
  DPNDINCS := $(MIRINCS)
  DPNDOPT  := OPT

  show ::
	-@ echo ""
	-@ echo "VPATH    = $(VPATH)"
	-@ echo ""
	-@ echo "DPNDAUXD = $(DPNDAUXD)"
	-@ echo "DPNDINCL = $(DPNDINCL)"
	-@ echo "DPNDLOCK = $(DPNDLOCK)"
	-@ echo "DPNDSYMS = $(DPNDSYMS)"
	-@ echo "DPNDTIME = $(DPNDTIME)"
	-@ echo "DPNDTMPD = $(DPNDTMPD)"
	-@ echo ""
	-@ echo "DPNDINCS = $(DPNDINCS)"
	-@ echo "DPNDOPT  = $(DPNDOPT)"

  # Dependency analysis for included files:
  #----------------------------------------
  # This command sequence identifies include files in the ratty output and
  # makes each of them a prerequisite of the object module's timestamp.  In
  # turn, rule-chaining also makes them a prerequisite of the object module
  # itself.  An example of the type of output it produces is shown below.
  define f-filter
     @ sed -n -e 's#^c >>> $(MIRROOT)/##p' $(DPNDTMPD)/$*.f | $(backsl)
         sed -e 's#^inc#$$(DPNDTIME)($*) : $$(MIRINCD)#' $(backsl)
             -e 's#^subs#$$(DPNDTIME)($*) : $$(MIRSUBSD)#' | $(backsl)
         sort -u | sort -m - $(DPNDLOCK) > $(DPNDINCL)
  endef

  # As above, this command sequence identifies include files in the cpp output
  # and makes each of them a prerequisite of the object module's timestamp.
  define i-filter
     @ sed -n -e 's#.*$(MIRROOT)/\(.*\.h\)".*#\1#p' $(DPNDTMPD)/$*.i | $(backsl)
         sed -e 's#^inc#$$(DPNDTIME)($*) : $$(MIRINCD)#' $(backsl)
             -e 's#^subs#$$(DPNDTIME)($*) : $$(MIRSUBSD)#' | $(backsl)
         sort -u | sort -m - $(DPNDLOCK) > $(DPNDINCL)
  endef

  # Dependency analysis for object modules:
  #----------------------------------------
  # The nm-syms command sequence must be defined in makedefs to list the
  # global symbols defined by an object module.  If undefined this analysis is
  # skipped and each executable is made to depend on the library as a whole.
  # As in the analysis of included files, the lock file is used to store
  # intermediaries.
  ifdef nm-syms.$(ARCH)
    define list-syms
    -@ if [ -f "$(DPNDSYMS)" ] ; then $(backsl)
         sed -e '/ $*.o$$/d' $(DPNDSYMS) > $(DPNDLOCK) ; $(backsl)
       else $(backsl)
         cat /dev/null > $(DPNDLOCK) ; $(backsl)
       fi
    -@ $(RM) $(DPNDSYMS)
    -@ OBJ="$<" ; $(nm-syms.$(ARCH)) | $(backsl)
         sed -e 's|$$| $*.o|' | $(backsl)
         sort -m - $(DPNDLOCK) > $(DPNDSYMS)
    endef
  else
    define list-syms
    -@ $(RM) $(DPNDSYMS)
    endef
  endif


  # Pattern rules (rule-chaining, and dependency analysis).
  #--------------------------------------------------------
  # This sequence of pattern rules forms many chains.  Which chain is used to
  # resolve a particular target depends on the precedence of the rules in the
  # chain (established by their order of definition here) and the existence of
  # dependencies that can satisfy them.
  #
  # For example, in linux, 'gmake lib(uvdat)' would be resolved by the
  # following chain rooted at $MIR/subs/uvdat.for: 
  #
  #   lib(uvdat) <- $MIR/linux/lib/libmir.a(uvdat.o)
  #              <- $MIR/linux/tmp/uvdat.o
  #              <- $MIR/linux/aux/subs.time(uvdat) and
  #                 $MIR/linux/tmp/uvdat.f
  #              <- $MIR/subs/uvdat.for
  #
  # The chain is evaluated in reverse order, starting with
  # $MIR/subs/uvdat.for, as indicated by the arrows.
  #
  # The first step is pretty interesting; make generates uvdat's dependency
  # timestamp, which for tidiness is stored in an archive library as
  # $MIR/linux/aux/subs.time(uvdat), by running ratty to generate
  # $MIR/linux/tmp/uvdat.f.  It uses uvdat.f to determine uvdat's
  # dependencies, adds these to $MIR/linux/aux/subs.incl, which is included at
  # the end of this makefile, and then touches uvdat's timstamp.
  #
  # uvdat.f and uvdat's timestamp are therefore generated by the same rule.
  # This mechanism is efficient because ratty always has to be run when
  # compiling Miriad Fortran so the only overhead is in scanning uvdat.f for
  # included files (the filter-f command sequence).  C sources are treated in
  # precisely the same way with the C preprocessor (cpp) taking ratty's place.
  # Likewise for .f2c sources, except that intf2c is invoked on these before
  # cpp.  
  #
  # The last two steps compile uvdat.f to produce $MIR/linux/tmp/uvdat.o and
  # then insert that into the object library.  The list of global symbols
  # defined by the object module is then updated.
  #
  # uvdat's dependency list for included files consists of the following
  # entries in $MIR/linux/aux/subs.incl (where $(DPNDTIME) is the makefile
  # variable used for $MIR/linux/aux/subs.time):
  #
  #   $(DPNDTIME)(uvdat) : $(MIRINCD)/maxdim.h
  #   $(DPNDTIME)(uvdat) : $(MIRINCD)/mirconst.h
  #   $(DPNDTIME)(uvdat) : $(MIRSUBSD)/uvdat.h
  #
  # This means that if maxdim.h is touched then make will consider that
  # uvdat's timestamp is out-of-date and needs to be remade.  Similarly if
  # uvdat.for was touched.
  #
  # make automatically deletes the intermediate products of the rule-chain
  # except for those marked as a dependency of the pseudo-rule ".PRECIOUS".
  # This is used to protect the dependency timestamps.  Other intermediates
  # are deleted explicitly to save having them hanging around.

  .PRECIOUS : $(DPNDTIME)(%)

  # Firstly we have a convenience rule for building a specified library module
  # with, say, 'gmake math' or 'gmake lib(math)' - it is not used within the
  # makefile itself.  After the $(MIRLIBR)(%.o) rule replaces the module in
  # the object library this rule ranlibs it.
  % lib(%) lib(%.o) : $(MIRLIBR)(%.o) ranlib ;

  # Insert an object module in libmir.a and extract the list of global symbols
  # that it defines.
  $(MIRLIBR)(%.o) : $(MIRTMPD)/%.o
	 @ $(AR) $(ARFLAGS) $@ $<
	   $(list-syms)
	   $(wait-unlock)
	-@ $(RM) $<

  # Rules for compiling the preprocessor output generated as a by-product of
  # the dependency analysis.
  $(MIRTMPD)/%.o : $(DPNDTMPD)/%.f $(DPNDTIME)(%)
	   $(FC) $(FCOPT) -c -o $@ $<
	-@ $(RM) $<
  
  $(MIRTMPD)/%.o : $(DPNDTMPD)/%.i $(DPNDTIME)(%)
	   $(CC) $(COPT) -c -o $@ $<
	-@ $(RM) $<
  
  # As explained above, the preprocessor output and dependency timestamp are
  # generated at the same time, the timestamp is stored in an archive file for
  # tidiness.  Rule-chaining will resolve the object module to the appropriate
  # source file, and the dependency list, $MIR/linux/aux/subs.incl, may add
  # further dependencies.
  #
  # GNU make considers the $(DPNDTIME)(%) pattern rules in sequence, looking
  # for the first it can use to make the target.  Since the rules for
  # dependency analysis are complicated and are used repeatedly they are
  # implemented via canned rule definitions, above.
  #
  # Precedence must be given to the lowest level of the subroutine hierarchy
  # which depends on the particular platform, e.g. for linux it's
  # $MIR/subs/unix/alpha_linux/...  This is taken care of by VPATH defined
  # above.
  $(DPNDTIME)(%) $(DPNDTMPD)/%.f : %.for
	$(depend-for)

  $(DPNDTIME)(%) $(DPNDTMPD)/%.i : %.f2c
	$(depend-f2c)

  $(DPNDTIME)(%) $(DPNDTMPD)/%.i : %.c
	$(depend-c)

  # This little rule tells make not to fret if it can't find a header file.
  # It may be important if a header file is deleted but remains as a
  # prerequisite for some of the object modules in the dependency list.
  %.h : ;


  # Static and static pattern rules.
  #---------------------------------
  .PHONY : depend lib ranlib

  allsys :: $(MIRLIBR)

  $(MIRLIBR) : $(MIROBJS:%=$(MIRLIBR)(%)) ;
     ifneq "$(RANLIB)" ":"
	    $(RANLIB) $@
     endif

  # Forcibly ranlib the library.
  ranlib : FORCE
	   ranlib $(MIRLIBR)

  depend : $(MIROBJS:%.o=$(DPNDTIME)(%)) ;

  cleansys ::
	$(RM) $(MIRTMPD)/*.o
	$(RM) $(DPNDTMPD)/*.[cfi]
	$(RM) $(DPNDAUXD)/subs.*
	$(RM) $(MIRLIBR)

  help ::
	-@ echo ""
	-@ echo "Targets defined in the subs GNUmakefile"
	-@ echo "---------------------------------------"
	-@ echo "     allsys: update $(MIRLIBR)."
	-@ echo "     ranlib: run ranlib on $(MIRLIBR) (forced)."
	-@ echo "     depend: update system dependency lists."
	-@ echo "   cleansys: delete $(MIRLIBR), dependency lists, etc."

else
  # Programmer-oriented rules.
  #---------------------------
  # Resolve paths for operating-system specific source code.  Look first in
  # programmer directories then in the relevant Miriad directories.
  ifneq "$(MIROS2D)" ""
    PGMROSD  := ./unix/$(SUBS$(ARCH))
    VPATH    := $(PGMROSD):$(MIROS2D):
  endif
  PGMROSD  += ./unix
  VPATH    := $(VPATH)./unix:$(MIROS1D):.:$(CODEDIR):$(PGMRINCD):$(MIRINCD)

  PGMROSRC := $(sort $(notdir \
                $(foreach DIR,$(PGMROSD),$(wildcard $(DIR)/*.*))))

  # Tell ratty and intf2c to look for include files in the programmer
  # directories first, then in the relevant Miriad directories.
  PGMRINCS := -I . -I $(PGMRINCD) $(MIRINCS)
  PGMRLIBR := $(PGMRLIBD)/libmir.a
  PGMROBJS := $(subst .for,.o,$(wildcard *.for) $(filter %.for,$(PGMROSRC)))
  PGMROBJS += $(subst .f2c,.o,$(wildcard *.f2c) $(filter %.f2c,$(PGMROSRC)))
  PGMROBJS += $(subst   .c,.o,$(wildcard *.c)   $(filter %.c,  $(PGMROSRC)))
  PGMROBJS := $(sort $(PGMROBJS))

  # Dependency files go into the programmer's aux directory, while ratty and
  # intf2c output is preserved in the lib directory where the debugger can
  # find it.
  DPNDAUXD := $(PGMRAUXD)
  DPNDTMPD := $(PGMRLIBD)

  # Programmer include directory search path and debug compilation.
  DPNDINCS := $(PGMRINCS)
  DPNDOPT  := DBG

  show ::
	-@ echo ""
	-@ echo "VPATH    = $(VPATH)"
	-@ echo ""
	-@ echo "PGMROSD  = $(PGMROSD)"
	-@ echo "PGMROSRC = $(PGMROSRC)"
	-@ echo ""
	-@ echo "PGMRINCS = $(PGMRINCS)"
	-@ echo "PGMRLIBR = $(PGMRLIBR)"
	-@ echo "PGMROBJS = $(PGMROBJS)"
	-@ echo ""
	-@ echo "DPNDAUXD = $(DPNDAUXD)"
	-@ echo "DPNDINCL = $(DPNDINCL)"
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
  # For similar reasons the source file itself (.for, .f2c, or .c) must also
  # be mentioned as a prerequisite in the programmer dependency list for
  # included files but is not needed in the system list.
  define f-filter
     @ echo $< | $(backsl)
         sed -e 's#^\([^/]\)#$(THISDIR)/\1#' | $(backsl)
         cat - $(DPNDTMPD)/$*.f | $(backsl)
         sed -n -e '{1p;s#^c >>> ##p;}' | $(backsl)
         sed -e 's#^$(PGMRROOT)/#$$(DPNDTIME)($*) : $$(PGMRROOT)/#' $(backsl)
             -e 's#^$(MIRROOT)/.*/#$$(DPNDTIME)($*) : #' | $(backsl)
         sort -u | sort -m - $(DPNDLOCK) > $(DPNDINCL)
  endef

  # As above, this command sequence identifies include files in the cpp output
  # and makes each of them a prerequisite of the object module's timestamp.
  # The difference is in the second sed filter alone.
  define i-filter
     @ echo $< | $(backsl)
         sed -e 's#^\([^/]\)#$(THISDIR)/\1#' | $(backsl)
         cat - $(DPNDTMPD)/$*.i | $(backsl)
         sed -n -e 1p $(backsl)
                -e 's#.*\($(PGMRROOT)/.*\.h\)".*#\1#p' $(backsl)
                -e 's#.*\($(MIRROOT)/.*\.h\)".*#\1#p' | $(backsl)
         sed -e 's#^$(PGMRROOT)/#$$(DPNDTIME)($*) : $$(PGMRROOT)/#' $(backsl)
             -e 's#^$(MIRROOT)/.*/#$$(DPNDTIME)($*) : #' | $(backsl)
         sort -u | sort -m - $(DPNDLOCK) > $(DPNDINCL)
  endef


  # Pattern rules.
  #---------------
  # These are essentially the same as for system compilation.
  .PRECIOUS : $(DPNDTIME)(%) $(PGMRLIBR)(%.o) $(DPNDTMPD)/%.f $(DPNDTMPD)/%.i

  % lib(%) lib(%.o) : $(PGMRLIBR)(%.o) ranlib ;

  $(PGMRLIBR)(%.o) : $(PGMRLIBD)/%.o
	 @ $(AR) $(ARFLAGS) $@ $<
	   $(wait-unlock)
	-@ $(RM) $<

  # Making the timestamp a prerequisite of the Fortran object module prevents
  # rule-chaining from stopping prematurely at this point.  This could happen
  # if a .f file preserved from a previous compilation is newer than the
  # target though constructed from header files that are now stale.  The stale
  # header files will be listed as prerequisites of the timestamp in the
  # dependency list included at the end of this makefile.
  $(PGMRLIBD)/%.o : $(DPNDTMPD)/%.f $(DPNDTIME)(%)
	   $(FC) $(FCDBG) -c -o $@ $<
  
  # Although .i files are not preserved they can be generated on demand and
  # there is always the possibility of a stale one causing the same problem
  # as as a stale .f.  Hence the timestamp must also be a prerequisite here.
  $(PGMRLIBD)/%.o : $(DPNDTMPD)/%.i $(DPNDTIME)(%)
	   $(CC) $(CDBG) -c -o $@ $<
	-@ $(RM) $<

  $(DPNDTIME)(%) $(DPNDTMPD)/%.f : %.for
	$(depend-for)

  $(DPNDTIME)(%) $(DPNDTMPD)/%.i : %.f2c
	$(depend-f2c)

  $(DPNDTIME)(%) $(DPNDTMPD)/%.i : %.c
	$(depend-c)

  # Don't worry if a source file in the dependency list can't be located.
  $(THISDIR)/%.for : ;
  $(THISDIR)/%.f2c : ;
  $(THISDIR)/%.c : ;

  # Don't worry if a header file in the dependency list can't be located.
  %.h : ;

  # Convenience rules for generating preprocessor output.
  %.f : $(DPNDTMPD)/%.f ;

  %.c : $(DPNDTMPD)/%.c ;

  %.i : $(DPNDTMPD)/%.i ;

  $(DPNDTMPD)/%.c : %.f2c
	$(INTF2C) $(F2CFLAGS) $< $@


  # Static and static pattern rules.
  #---------------------------------
  .PHONY : depend mylib lib ranlib

  all :: mylib
  
  mylib : $(PGMROBJS:%=$(PGMRLIBR)(%)) ranlib ;

  lib : $(MIROBJS:%=$(PGMRLIBR)(%)) ranlib ;

  ranlib : $(PGMRLIBD)/.last_ranlib ;

  $(PGMRLIBD)/.last_ranlib : $(PGMRLIBR)
	-@ if [ -n "$?" ] ; then \
	     echo "$(RANLIB) $<" ; \
	     $(RANLIB) $< ; \
	     touch $@ ; \
	   fi

  # Don't fret it the library doesn't exist.
  $(PGMRLIBR) : ;

  depend : $(PGMROBJS:%.o=$(DPNDTIME)(%)) ;

  clean ::
	$(RM) *.o
	$(RM) $(DPNDTMPD)/*.[io]
	$(RM) $(DPNDLOCK)

  cleaner :: clean
	$(RM) $(DPNDTMPD)/*.[cf]
	$(RM) $(DPNDAUXD)/subs.*

  cleanest :: cleaner
	$(RM) $(PGMRLIBD)/.last_ranlib
	$(RM) $(PGMRLIBR)

  help ::
	-@ echo ""
	-@ echo "Targets defined in the subs GNUmakefile"
	-@ echo "---------------------------------------"
	-@ echo "        all: compile sources in this directory only."
	-@ echo "      mylib: compile sources in this directory only."
	-@ echo "        lib: build $(PGMRLIBR) in its entirety, for"
	-@ echo "             debugging purposes."
	-@ echo "     ranlib: run ranlib on the library if necessary."
	-@ echo "     depend: update programmer dependency lists."
	-@ echo "        %.f: generate ratty preprocessor output."
	-@ echo "        %.c: generate intf2c preprocessor output."
endif

# Dependency lists.
-include $(DPNDINCL)
