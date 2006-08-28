#-----------------------------------------------------------------------------
# GNUmakefile for compiling Miriad tools.
#
# Original: 2006/08/25, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.1 2006/08/28 01:52:36 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

MIREXES  := $(basename $(filter %.c,$(MIRSRCS)))

ifeq "$(MAKEMODE)" "programmer"
  VPATH    := .:$(CODEDIR)

  PGMREXES := $(basename $(wildcard *.c))
endif

# Pattern rules.
#---------------
$(MIRBIND)/% : $(CODEDIR)/%.c
	$(CC) $(CCOPT) -o $@ $<

ifeq "$(MAKEMODE)" "programmer"
  % : $(PGMRBIND)/% ;

  $(PGMRBIND)/% : %.c
	$(CC) $(CCDBG) -o $@ $<
endif

# Static and static pattern rules.
#---------------------------------
allsys :: $(MIREXES:%=$(MIRBIND)/%)

$(MIRBIND)/mirnewer : $(CODEDIR)/mirnewer.c
	$(CC) -Dposix $(CCOPT) -o $@ $<

cleansys ::
	-@ $(RM) $(MIREXES:%=$(MIRBIND)/%)

show ::
	-@ echo ""
	-@ echo "Variables defined in the tools GNUmakefile"
	-@ echo "=========================================="
	-@ echo ""
	-@ echo "MIREXES  = $(MIREXES)"

ifeq "$(MAKEMODE)" "programmer"
  .PRECIOUS : $(PGMRBIND)/%

  all :: $(addprefix $(PGMRBIND)/, $(PGMREXES))

  $(PGMRBIND)/mirnewer : mirnewer.c
	$(CC) -Dposix $(CCDBG) -o $@ $<

  clean ::
	$(RM) *.o

  cleanest :: clean
	cd $(PGMRBIND) && $(RM) $(sort $(PGMREXES) $(MIREXES))

  show ::
	-@ echo ""
	-@ echo "VPATH    = $(VPATH)"
	-@ echo "PGMREXES = $(PGMREXES)"
endif
