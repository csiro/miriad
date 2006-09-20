#-----------------------------------------------------------------------------
# GNUmakefile for compiling Miriad documentation.
#
# Original: 2006/09/18, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.3 2006/09/20 08:18:03 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

# All doc files in this directory.
MIRDOCS  := $(wildcard *.doc)

# Special doc files are not converted to man pages.
MIRMDOCS := $(filter-out $(notdir $(wildcard $(MIRSDOCD)/*.doc)),$(MIRDOCS))

# All doc files associated with a Miriad task or other executable.
MIREXES  := $(notdir $(wildcard $(MIRBIND)/*))
MIRTDOCS := $(filter $(MIREXES:%=%.doc),$(MIRDOCS))

show ::
	-@ echo ""
	-@ echo ""
	-@ echo "Variables defined in the tools GNUmakefile"
	-@ echo "=========================================="
	-@ echo ""
	-@ echo "MIRDOCS  = $(MIRDOCS)"
	-@ echo "MIRMDOCS = $(MIRMDOCS)"
	-@ echo "MIREXES  = $(MIREXES)"
	-@ echo "MIRTDOCS = $(MIRTDOCS)"

ifeq "$(MAKEMODE)" "system"
  # System-oriented rules.
  #-----------------------

  # Pattern rules.
  #---------------
  $(MIRHTMLD)/%.html : $(MIRDOCD)/%.doc
	-@ $(RM) $@
	-@ echo "doc2html $*.doc ../html/taskref.txt > ../html/$*.html"
	 @ ./doc2html $< $(MIRHTMLD)/taskref.txt > $@
	-@ chmod 664 $@

  $(MIRMAND)/man1/%.1 : $(MIRDOCD)/%.doc
	-@ $(RM) $@
	-@ echo "doc2man $*.doc > ../man/man1/$*.1"
	 @ $(MIRBIND)/doc2man $< > $@
	-@ chmod 664 $@

  %.html : $(MIRHTMLD)/%.html ;
  %.man : $(MIRMAND)/man1/%.1 ;


  # Static and static pattern rules.
  #---------------------------------
  .PHONY : cheat html man taskindex tasks

  allsys :: cheat tasks taskindex html man

  cheat : $(MIRDOCD)/cheat.doc ;

  $(MIRDOCD)/cheat.doc : $(MIRTDOCS)
	-@ $(RM) $(MIRTMPD)/cheat.doc
	-@ echo "Buildcheat > cheat.doc"
	 @ ./Buildcheat > $(MIRTMPD)/cheat.doc
	 @ if [ -f "$(MIRTMPD)/cheat.doc" ] ; then \
	     mv -f $(MIRTMPD)/cheat.doc $@ ; \
	     chmod 664 $@ ; \
	     $(RM) $(MIRSDOCD)/cheat.doc ; \
	     cp $@ $(MIRSDOCD)/cheat.doc ; \
	   fi

  tasks : $(MIRDOCD)/tasks.doc ;

  $(MIRDOCD)/tasks.doc : $(MIRTDOCS)
	-@ $(RM) $(MIRTMPD)/tasks.doc
	-@ echo "Buildtasks > tasks.doc"
	 @ ./Buildtasks > $(MIRTMPD)/tasks.doc
	 @ if [ -f "$(MIRTMPD)/tasks.doc" ] ; then \
	     mv -f $(MIRTMPD)/tasks.doc $@ ; \
	     chmod 664 $@ ; \
	     $(RM) $(MIRSDOCD)/tasks.doc ; \
	     cp $@ $(MIRSDOCD)/tasks.doc ; \
	   fi

  taskindex : $(MIRHTMLD)/taskindex.html ;

  $(MIRHTMLD)/taskindex.html : $(MIRTDOCS)
	-@ echo "Buildhtmltask"
	 @ ./Buildhtmltask

  html : $(MIRDOCS:%.doc=$(MIRHTMLD)/%.html) ;

  man : $(MIRMDOCS:%.doc=$(MIRMAN)/man1/%.1)
	-@ echo ""
	   $(MANDB) $(MIRMAND)

  help ::
	-@ echo ""
	-@ echo "Targets defined in the doc GNUmakefile"
	-@ echo "--------------------------------------"
	-@ echo "     allsys: regenerate documentation from doc files."
	-@ echo "      cheat: regenerate Miriad cheat sheet."
	-@ echo "      tasks: regenerate Miriad task list."
	-@ echo "  taskindex: regenerate Miriad HTML task index."
	-@ echo "       html: regenerate html files."
	-@ echo "     %.html: regenerate specified html file."
	-@ echo "        man: regenerate man pages."
	-@ echo "      %.man: regenerate specified man page."

else
  # Programmer-oriented rules.
  #---------------------------
  help ::
	-@ echo ""
	-@ echo "No programmer-oriented rules are defined in the doc"
	-@ echo "GNUmakefile."
endif
