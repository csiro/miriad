#-----------------------------------------------------------------------------
# GNUmakefile for compiling Miriad tools.
#
# Original: 2006/08/25, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.4 2006/09/22 02:06:43 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

MIREXES  := $(basename $(filter %.c,$(MIRSRCS)))

show ::
	-@ echo ""
	-@ echo ""
	-@ echo "Variables defined in the tools GNUmakefile"
	-@ echo "=========================================="
	-@ echo ""
	-@ echo "MIREXES  = $(MIREXES)"

ifeq "$(MAKEMODE)" "system"
  # System-oriented rules.
  #-----------------------

  # Pattern rules.
  #---------------
  $(MIRBIND)/% : $(CODEDIR)/%.c
	   $(CC) $(CCOPT) -o $@ $<
	-@ chmod 775 $@

  # Extract preamble comments into a Miriad "doc" file.
  $(MIRDOCD)/%.doc : %.c
	-@ $(RM) $(MIRDOCD)/$*.doc
	   $(MIRBIND)/doc -f $< > $@
	-@ chmod 664 $@

  # Static and static pattern rules.
  #---------------------------------
  allsys :: bin doc
  
  bin : $(MIREXES:%=$(MIRBIND)/%) ;

  ifneq "$(READLINELIB)" ""
    $(MIRBIND)/miriad : $(CODEDIR)/miriad.c
	   $(CC) -DREADLINE $(CCOPT) -o $@ $< $(READLINELIB)
	-@ chmod 775 $@
  endif

  $(MIRBIND)/mirnewer : $(CODEDIR)/mirnewer.c
	   $(CC) -Dposix $(CCOPT) -o $@ $<
	-@ chmod 775 $@

  doc : $(MIREXES:%=$(MIRDOCD)/%.doc) ;

  help ::
	-@ echo ""
	-@ echo "Targets defined in the tools GNUmakefile"
	-@ echo "----------------------------------------"
	-@ echo "     allsys: recompile MIREXES."
	-@ echo "        bin: compile and link all programs."
	-@ echo "        doc: generate doc files for all programs."

else
  # Programmer-oriented rules.
  #---------------------------
  VPATH    := .:$(CODEDIR)

  PGMREXES := $(basename $(wildcard *.c))

  # Pattern rules.
  #---------------
  .PRECIOUS : $(PGMRBIND)/%

  % : $(PGMRBIND)/% ;

  $(PGMRBIND)/% : %.c
	$(CC) $(CCDBG) -o $@ $<

  # Static and static pattern rules.
  #---------------------------------
  all :: $(addprefix $(PGMRBIND)/, $(PGMREXES))

  ifneq "$(READLINELIB)" ""
    $(PGMRBIND)/miriad : miriad.c
	$(CC) -DREADLINE $(CCDBG) -o $@ $< $(READLINELIB)
  endif

  $(PGMRBIND)/mirnewer : mirnewer.c
	$(CC) -Dposix $(CCDBG) -o $@ $<

  clean ::
	$(RM) *.o

  cleanest :: clean
	cd $(PGMRBIND) && $(RM) $(sort $(PGMREXES) $(MIREXES))

  help ::
	-@ echo ""
	-@ echo "Targets defined in the tools GNUmakefile"
	-@ echo "----------------------------------------"
	-@ echo "          %: recompile the specified program."

  show ::
	-@ echo ""
	-@ echo "VPATH    = $(VPATH)"
	-@ echo "PGMREXES = $(PGMREXES)"
endif
