#-----------------------------------------------------------------------------
# GNUmakefile for compiling Miriad documentation.
#
# Original: 2006/09/18, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.4 2006/09/22 06:32:34 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

# All doc files in the MIRSDOCD and MIRDOCD directories.
MIRSDOCS := $(notdir $(wildcard $(MIRSDOCD)/*.doc))
MIRDOCS  := $(filter-out $(MIRSDOCS),$(notdir $(wildcard $(MIRDOCD)/*.doc)))

# Add cheat.doc and tasks.doc which may be generated for the first time here.
MIRSDOCS := $(sort $(MIRSDOCS) cheat.doc tasks.doc)

show ::
	-@ echo ""
	-@ echo ""
	-@ echo "Variables defined in the guides GNUmakefile"
	-@ echo "==========================================="
	-@ echo ""
	-@ echo "MIRSDOCS = $(MIRSDOCS)"
	-@ echo "MIRDOCS  = $(MIRDOCS)"

ifeq "$(MAKEMODE)" "system"
  # System-oriented rules.
  #-----------------------

  # Pattern rules.
  #---------------
  # Create a doc -> specdoc symlink.
  $(MIRDOCD)/%.doc :
	-@ $(RM) $@
	   ln -s ../specdoc/$*.doc $@

  # Generate HTML from doc.
  $(MIRHTMLD)/%.html : $(MIRDOCD)/%.doc
	-@ $(RM) $@
	-@ echo "doc2html ../doc/$*.doc ../html/taskref.txt > ../html/$*.html"
	 @ ./doc2html $< $(MIRHTMLD)/taskref.txt > $@
	-@ chmod 664 $@

  # Generate unix man page from doc.
  $(MIRMAND)/man1/%.1 : $(MIRDOCD)/%.doc
	-@ $(RM) $@
	-@ echo "doc2man ../doc/$*.doc > ../man/man1/$*.1"
	 @ $(MIRBIND)/doc2man $< > $@
	-@ chmod 664 $@

  .PRECIOUS : $(MIRHTMLD)/%.html $(MIRMAND)/man1/%.1
  %.html : $(MIRHTMLD)/%.html ;
  %.man : $(MIRMAND)/man1/%.1 ;


  # Static and static pattern rules.
  #---------------------------------
  .PHONY : cheat html man symlinks taskindex tasks

  allsys :: symlinks cheat tasks taskindex html man

  symlinks : $(MIRSDOCS:%=$(MIRDOCD)/%) ;

  cheat : $(MIRSDOCD)/cheat.doc ;

  $(MIRSDOCD)/cheat.doc : $(MIRDOCS:%=$(MIRDOCD)/%)
	-@ $(RM) $(MIRTMPD)/cheat.doc
	-@ echo "Buildcheat > $@"
	 @ ./Buildcheat > $(MIRTMPD)/cheat.doc
	 @ if [ -f "$(MIRTMPD)/cheat.doc" ] ; then \
	     mv -f $(MIRTMPD)/cheat.doc $@ ; \
	     chmod 664 $@ ; \
	   fi

  tasks : $(MIRSDOCD)/tasks.doc ;

  $(MIRSDOCD)/tasks.doc : $(MIRDOCS:%=$(MIRDOCD)/%)
	-@ $(RM) $(MIRTMPD)/tasks.doc
	-@ echo "Buildtasks > $@"
	 @ ./Buildtasks > $(MIRTMPD)/tasks.doc
	 @ if [ -f "$(MIRTMPD)/tasks.doc" ] ; then \
	     mv -f $(MIRTMPD)/tasks.doc $@ ; \
	     chmod 664 $@ ; \
	   fi

  taskindex : $(MIRHTMLD)/taskindex.html ;

  $(MIRHTMLD)/taskindex.html : $(MIRDOCS:%=$(MIRDOCD)/%)
	-@ echo "Buildhtmltask"
	 @ ./Buildhtmltask

  html : $(patsubst %.doc,$(MIRHTMLD)/%.html,$(MIRDOCS) $(MIRSDOCS)) ;

  # Special doc files are not converted to man pages.
  man : $(MIRDOCS:%.doc=$(MIRMAND)/man1/%.1)
	-@ echo ""
	   $(MANDB) $(MIRMAND)

  help ::
	-@ echo ""
	-@ echo "Targets defined in the doc GNUmakefile"
	-@ echo "--------------------------------------"
	-@ echo "     allsys: regenerate documentation from doc files."
	-@ echo "   symlinks: create doc -> specdoc symlinks."
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
