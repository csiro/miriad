#-----------------------------------------------------------------------------
# GNUmakefile for compiling the LINPACK subroutine library in Miriad.
#
# Original: 2006/08/25, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.1 2006/08/28 01:50:28 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

LINLIBR  := $(MIRLIBD)/liblinpack.a
LINOBJS  := $(subst .for,.o,$(filter %.for,$(MIRSRCS)))


# Pattern rules.
#---------------
# Insert an object module in liblinpack.a.
$(LINLIBR)(%.o) : $(MIRTMPD)/%.o
	$(AR) $(ARFLAGS) $@ $<

$(MIRTMPD)/%.o : $(MIRROOT)/linpack/%.for
	$(FC) $(FCOPT) -c -o $@ $<

# Static and static pattern rules.
#---------------------------------
allsys :: $(LINLIBR)

$(LINLIBR) : $(LINOBJS:%=$(LINLIBR)(%))
	$(RANLIB) $(LINLIBR)

cleansys ::
	$(RM) $(LINLIBR)

show ::
	-@ echo ""
	-@ echo "Variables defined in the linpack GNUmakefile"
	-@ echo "============================================"
	-@ echo ""
	-@ echo "LINLIBR  = $(LINLIBR)"
	-@ echo "LINOBJS  = $(LINOBJS)"
