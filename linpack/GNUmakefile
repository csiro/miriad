#-----------------------------------------------------------------------------
# GNUmakefile for compiling the LINPACK subroutine library in Miriad.
#
# Original: 2006/08/25, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.2 2006/09/15 07:49:42 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

# Note that the linpack source files are not checked in.
LINLIBR  := $(MIRLIBD)/liblinpack.a
LINOBJS  := $(subst .for,.o,$(wildcard *.for))

show ::
	-@ echo ""
	-@ echo "Variables defined in the linpack GNUmakefile"
	-@ echo "============================================"
	-@ echo ""
	-@ echo "LINLIBR  = $(LINLIBR)"
	-@ echo "LINOBJS  = $(LINOBJS)"

ifeq "$(MAKEMODE)" "system"
  # System-oriented rules.
  #-----------------------

  # Pattern rules.
  #---------------
  # Insert an object module in liblinpack.a.
  $(LINLIBR)(%.o) : $(MIRTMPD)/%.o
	 @ $(AR) $(ARFLAGS) $@ $<
	-@ $(RM) $<

  # Compile a Linpack Fortran source file.
  $(MIRTMPD)/%.o : $(MIRROOT)/linpack/%.for
	-@ echo ""
	   $(FC) $(FCOPT) -c -o $@ $<

  # Static and static pattern rules.
  #---------------------------------
  allsys :: $(LINLIBR)

  $(LINLIBR) : $(LINOBJS:%=$(LINLIBR)(%)) ;
     ifneq "$(RANLIB)" ":"
	    $(RANLIB) $@
     endif

  # Forcibly ranlib the library.
  ranlib : FORCE
	   ranlib $(LINLIBR)

  cleansys ::
	$(RM) $(LINLIBR)

  help ::
	-@ echo ""
	-@ echo "Targets defined in the linpack GNUmakefile"
	-@ echo "------------------------------------------"
	-@ echo "     allsys: recompile $(LINLIBR)."
	-@ echo "     ranlib: run ranlib on $(LINLIBR) (forced)."
	-@ echo "   cleansys: delete $(LINLIBR)."

else
  # Programmer-oriented rules.
  #---------------------------
  help ::
	-@ echo ""
	-@ echo "No programmer-oriented rules are defined in the linpack"
	-@ echo "GNUmakefile."
endif
