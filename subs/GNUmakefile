#-----------------------------------------------------------------------------
# GNUmakefile for compiling the Miriad subroutine library.  Does full
# dependency analysis for all source file types.
#
# Original: 2006/08/19, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.1 2006/08/28 01:54:59 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

# Add architecture-specific source files.
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


# Dependency analysis.
#---------------------
# See below for an explanation of the dependency analysis scheme.
DPNDLIST := $(MIRAUXD)/subs.list
DPNDTIME := $(MIRAUXD)/subs.time
DPNDLOCK := $(MIRAUXD)/subs.lock

# Command sequence for applying ratty, extracting Fortran dependencies, and
# compiling.  The wait-lock and update-time command sequences are defined in
# GNUmakedefs.
define depend-for
     $(wait-lock)
     $(RATTY) $(RATFLAGS) $(MIRINCS) $< $(MIRTMPD)/$*.f
   @ sed -n -e 's#^c >>> $(MIRROOT)/##p' $(MIRTMPD)/$*.f | $(backsl)
       sort -u | $(backsl)
       sed -e 's#^inc#$$(DPNDTIME)($*) : $$(MIRINCD)#' $(backsl)
           -e 's#^subs#$$(DPNDTIME)($*) : $$(MIRSUBSD)#' | $(backsl)
       sort -m - $(DPNDLOCK) > $(DPNDLIST)
     $(FC) $(FCOPT) -c -o $(MIRTMPD)/$*.o $(MIRTMPD)/$*.f
  -@ if [ ! -f "$(MIRTMPD)/$*.o" ] ; then $(backsl)
       $(RM) $(DPNDLOCK) ; $(backsl)
     fi
     $(update-time)
endef

# Command sequence for applying intf2c followed by cpp, extracting C
# dependencies from cpp output, and compiling.
define depend-f2c
     $(wait-lock)
     $(INTF2C) $(F2CFLAGS) $< $(MIRTMPD)/$*.c
     $(CPP) $(CPPOPT) $(MIRINCS) -o $(MIRTMPD)/$*.i $(MIRTMPD)/$*.c
     $(depend-i)
endef

# Command sequence for applying cpp, extracting C dependencies from cpp
# output, and compiling.
define depend-c
     $(wait-lock)
     $(CPP) $(CPPOPT) $(MIRINCS) -o $(MIRTMPD)/$*.i $<
     $(depend-i)
endef

# Command sequence for extracting C dependencies from cpp output and
# compiling.
define depend-i
   @ sed -n -e 's#.*$(MIRROOT)/\(.*\.h\)".*#\1#p' $(MIRTMPD)/$*.i | $(backsl)
       sort -u | $(backsl)
       sed -e 's#^inc#$$(DPNDTIME)($*) : $$(MIRINCD)#' $(backsl)
           -e 's#^subs#$$(DPNDTIME)($*) : $$(MIRSUBSD)#' | $(backsl)
       sort -m - $(DPNDLOCK) > $(DPNDLIST)
     $(CC) $(COPT) -c -o $(MIRTMPD)/$*.o $(MIRTMPD)/$*.i
  -@ if [ ! -f "$(MIRTMPD)/$*.o" ] ; then $(backsl)
       $(RM) $(DPNDLOCK) ; $(backsl)
     fi
     $(update-time)
endef


# Programmer variables.
#----------------------
ifeq "$(MAKEMODE)" "programmer"
  ifneq "$(MIROS2D)" ""
    PGMROSD  := ./unix/$(SUBS$(ARCH))
    VPATH    := $(PGMROSD):$(MIROS2D):
  endif
  PGMROSD  += ./unix
  VPATH    := $(VPATH)./unix:$(MIROS1D):.:$(CODEDIR)

  PGMROSRC := $(sort $(notdir \
                $(foreach DIR,$(PGMROSD),$(wildcard $(DIR)/*.*))))

  PGMRINCS := -I . -I $(PGMRINCD) $(MIRINCS)
  PGMRLIBR := $(PGMRLIBD)/libmir.a
  PGMROBJS := $(subst .for,.o,$(wildcard *.for) $(filter %.for,$(PGMROSRC)))
  PGMROBJS += $(subst .f2c,.o,$(wildcard *.f2c) $(filter %.f2c,$(PGMROSRC)))
  PGMROBJS += $(subst   .c,.o,$(wildcard *.c)   $(filter %.c,  $(PGMROSRC)))
  PGMROBJS := $(sort $(PGMROBJS))
endif


# Pattern rules (rule-chaining, and dependency analysis).
#--------------------------------------------------------
# This sequence of pattern rules forms many chains.  Which chain is used to
# resolve a particular target depends on the precedence of the rules in the
# chain (established by their order of definition here) and the existence of
# dependencies that can satisfy them.
#
# For example, in linux, 'gmake lib(uvdat)' would be resolved by the following
# chain rooted at $MIR/subs/uvdat.for: 
#
#   lib(uvdat) <- $MIR/linux/lib/libmir.a(uvdat.o)
#              <- $MIR/linux/tmp/uvdat.o
#              <- $MIR/linux/aux/subs.time(uvdat)
#              <- $MIR/subs/uvdat.for
#
# The chain is evaluated in reverse order, starting with $MIR/subs/uvdat.for,
# as indicated by the arrows.
#
# The first step is pretty interesting; make generates uvdat's dependency
# timestamp, which for tidiness is stored in an archive library as
# $MIR/linux/aux/subs.time(uvdat), by running ratty to generate
# $MIR/linux/tmp/uvdat.f.  It uses uvdat.f to determine uvdat's dependencies,
# adds these to $MIR/linux/aux/subs.list, which is included at the end of this
# makefile, and then touches uvdat's timstamp.  Finally it compiles uvdat.f to
# produce $MIR/linux/tmp/uvdat.o.
#
# The rule to generate uvdat.o from uvdat's timestamp is therefore a no-op
# because uvdat.o was a by-product of generating the timestamp.  This
# mechanism is efficient because ratty always has to be run when compiling
# Miriad Fortran so the only overhead is in scanning uvdat.f for included
# files.  C sources are treated in precisely the same way with the C
# preprocessor (cpp) taking ratty's place.  Likewise for .f2c sources, except
# that intf2c is invoked on these before cpp.  
#
# uvdat's dependency list consists of the following entries in
# $MIR/linux/aux/subs.list (where $(DPNDTIME) is the makefile variable used
# for $MIR/linux/aux/subs.time):
#
#   $(DPNDTIME)(uvdat) : $(MIRINCD)/maxdim.h
#   $(DPNDTIME)(uvdat) : $(MIRINCD)/mirconst.h
#   $(DPNDTIME)(uvdat) : $(MIRSUBSD)/uvdat.h
#
# This means that if maxdim.h is touched then make will consider that uvdat's
# timestamp is out-of-date and needs to be remade.  Similarly if uvdat.for was
# touched.
#
# make automatically deletes the intermediate products of the rule-chain, i.e.
# the .f and the .o, except for those marked as a dependency of the pseudo-
# rule ".PRECIOUS", which is used to protect the dependency timestamps
# themselves.

# Firstly we have a convenience rule for building a specified library module
# with, say, 'gmake lib(math)' - it is not used within the makefile itself.
# After the $(MIRLIBR)(%.o) rule replaces the module in the object library
# this rule ranlibs it.
lib(%) lib(%.o) : $(MIRLIBR)(%.o)
	$(RANLIB) $(MIRLIBR)

# Insert an object module in libmir.a.
$(MIRLIBR)(%.o) : $(MIRTMPD)/%.o
	-@ $(AR) $(ARFLAGS) $@ $<

# As explained above, the object module depends on the dependency timestamp
# which is stored in an archive file for tidiness.  Rule-chaining will resolve
# the timestamp to the appropriate source file, and the dependency list,
# $MIR/linux/aux/subs.list, may add further dependencies.
$(MIRTMPD)/%.o : $(DPNDTIME)(%) ;

# GNU make considers the $(DPNDTIME)(%) pattern rules in sequence, looking for
# the first it can use to make the target.  Since the rules for dependency
# analysis are complicated and are used repeatedly they are implemented via
# canned rule definitions, above.
#
# Precedence must be given to the lowest level of the subroutine hierarchy
# which depends on the particular platform, e.g. for linux it's
# $MIR/subs/unix/alpha_linux/...
ifneq "$(SUBS$(ARCH))" ""
  $(DPNDTIME)(%) : $(MIROS2D)/%.for
	$(depend-for)

  $(DPNDTIME)(%) : $(MIROS2D)/%.f2c
	$(depend-f2c)

  $(DPNDTIME)(%) : $(MIROS2D)/%.c
	$(depend-c)
endif

# ...followed by the intermediate level, $(MIRSUBSD)/unix/...
$(DPNDTIME)(%) : $(MIROS1D)/%.for
	$(depend-for)

$(DPNDTIME)(%) : $(MIROS1D)/%.f2c
	$(depend-f2c)

$(DPNDTIME)(%) : $(MIROS1D)/%.c
	$(depend-c)

# ...and the topmost level, $(MIRSUBSD), last.
$(DPNDTIME)(%) : $(MIRSUBSD)/%.for
	$(depend-for)

$(DPNDTIME)(%) : $(MIRSUBSD)/%.f2c
	$(depend-f2c)

$(DPNDTIME)(%) : $(MIRSUBSD)/%.c
	$(depend-c)



ifeq "$(MAKEMODE)" "programmer"
  % : $(PGMRLIBR)(%.o) ;
	$(RANLIB) $(PGMRLIBR)

  $(PGMRLIBR)(%.o) : $(PGMRLIBD)/%.o ;
	$(AR) $(ARFLAGS) $@ $<

  $(PGMRLIBD)/%.o : $(PGMRLIBD)/%.f
	$(FC) $(FCDBG) -c -o $@ $<

  $(PGMRLIBD)/%.f : %.for
	$(RATTY) $(RATFLAGS) $(PGMRINCS) $< $@

  $(PGMRLIBD)/%.c : %.f2c
	$(INTF2C) $(F2CFLAGS) $< $@

  $(PGMRLIBD)/%.o : $(PGMRLIBD)/%.c
	$(CC) $(CPPDBG) $(FCDBG) $(PGMRINCS) -c -o $@ $<

  $(PGMRLIBD)/%.o : %.c
	$(CC) $(CPPDBG) $(FCDBG) $(PGMRINCS) -c -o $@ $<

  # For convenience only.
  %.f : $(PGMRLIBD)/%.f
	-@ $(RM) $@
	   ln -s $< $@
endif

# This little rule tells make not to fret if it can't find a header file.
# It may be important if a header file is deleted but dependencies on it
# remain in the dependency list.
%.h : ;


# Static and static pattern rules.
#---------------------------------
.PRECIOUS : $(DPNDTIME)(%)

.PHONY : lib

allsys :: lib

lib : $(MIRLIBR)

$(MIRLIBR) : $(MIROBJS:%=$(MIRLIBR)(%))
	$(RANLIB) $(MIRLIBR)

#$(DPNDLIST) : dependsys ;

dependsys : $(MIROBJS:%.o=$(DPNDTIME)(%)) ;

cleansys ::
	$(RM) $(MIRAUXD)/subs.*
	$(RM) $(MIRLIBR)

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
	-@ echo ""
	-@ echo "DPNDLIST = $(DPNDLIST)"
	-@ echo "DPNDTIME = $(DPNDTIME)"
	-@ echo "DPNDLOCK = $(DPNDLOCK)"

# Programmer rules.
ifeq "$(MAKEMODE)" "programmer"
  .PHONY : mylib

  .PRECIOUS : $(PGMRLIBD)/(%.o) $(PGMRLIBD)/%.f $(PGMRLIBD)/%.c

  all :: $(PGMROBJS:%=$(PGMRLIBR)(%))

  mylib : $(MIROBJS:%=$(PGMRLIBR)(%))
	$(RANLIB) $(PGMRLIBR)

  clean ::
	$(RM) *.o

  cleaner :: clean
	$(RM) $(PGMRLIBD)/*.[fco]
	find . -maxdepth 1 -name "*.f" -type l -exec $(RM) {} \;

  cleanest :: cleaner
	$(RM) $(PGMRLIBR)

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
endif

ifndef NODEP
  -include $(DPNDLIST)
endif
