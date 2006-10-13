#-----------------------------------------------------------------------------
# GNUmakefile for compiling Miriad documentation.
#
# Original: 2006/09/18, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.9 2006/10/13 06:41:51 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

ifeq "$(MAKEMODE)" "system"
  # System-oriented rules.
  #-----------------------
  # Special doc files.
  MIRSDOCS := $(notdir $(wildcard $(MIRSDOCD)/*.doc))

  # Add cheat.doc and tasks.doc which may be generated for the first time here.
  MIRSDOCS := $(sort $(MIRSDOCS) cheat.doc tasks.doc)

  # Task and tool doc files.
  MIRDOCS  := $(filter-out $(MIRSDOCS),$(notdir $(wildcard $(MIRDOCD)/*.doc)))

  MIRGIFS  := $(filter %.gif,$(MIRSRCS))
  MIRHTMLS := $(filter %.html,$(MIRSRCS))

  show ::
	-@ echo ""
	-@ echo ""
	-@ echo "Variables defined in the guides GNUmakefile"
	-@ echo "==========================================="
	-@ echo ""
	-@ echo "MIRSDOCS = $(MIRSDOCS)"
	-@ echo "MIRDOCS  = $(MIRDOCS)"
	-@ echo ""
	-@ echo "MIRGIFS  = $(MIRGIFS)"
	-@ echo "MIRHTMLS = $(MIRHTMLS)"
	-@ echo ""
	-@ echo "GUIDES   = $(GUIDES)"


  # Pattern rules.
  #---------------
  # Create a doc -> specdoc symlink.
  $(MIRDOCD)/%.doc :
	-@ $(RM) $@
	   $(LN_S) ../specdoc/$*.doc $@

  # Generate unix man page from doc.
  $(MIRMAND)/man1/%.1 : $(MIRDOCD)/%.doc
	-@ $(RM) $@
	-@ echo "doc2man ../doc/$*.doc > ../man/man1/$*.1"
	 @ $(MIRBIND)/doc2man $< > $@
	-@ chmod 664 $@

  # Install an HTML file.
  $(MIRHTMLD)/%.html : %.html
	-@ $(RM) $@
	   cp $< $@
	-@ chmod 664 $@

  # Install a GIF file.
  $(MIRHTMLD)/%.gif : %.gif
	-@ $(RM) $@
	   cp $< $@
	-@ chmod 664 $@

  # Generate HTML from doc.  taskref.txt lists all references to Miriad tasks
  # in the userguide and is generated from the HTML version of the userguide.
  $(MIRHTMLD)/%.html : $(MIRDOCD)/%.doc taskref.txt
	-@ $(RM) $@
	-@ echo "doc2html ../doc/$*.doc taskref.txt > ../html/$*.html"
	 @ ./doc2html $< taskref.txt > $@ || $(RM) $@
	-@ chmod 664 $@


  # Static and static pattern rules.
  #---------------------------------
  .PHONY : cheat html man symlinks taskindex tasks

  # The User Guide must be built before HTML in order to generate taskref.txt.
  allsys :: symlinks cheat tasks man $(GUIDES) html

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

  # Special doc files are not converted to man pages.
  man : $(MIRDOCS:%.doc=$(MIRMAND)/man1/%.1)
	-@ echo ""
	   $(MANDB) $(MIRMAND)

  html : $(MIRHTMLD)/INSTALL.html $(MIRHTMLS:%=$(MIRHTMLD)/%) \
    $(MIRGIFS:%=$(MIRHTMLD)/%) taskindex \
    $(patsubst %.doc,$(MIRHTMLD)/%.html,$(MIRDOCS) $(MIRSDOCS))

  $(MIRHTMLD)/INSTALL.html : $(MIRROOT)/INSTALL.html
	-@ $(RM) $@
	   cp $< $@
	-@ chmod 664 $@

  taskindex : $(MIRHTMLD)/taskindex.html ;

  $(MIRHTMLD)/taskindex.html : $(MIRDOCS:%=$(MIRDOCD)/%)
	-@ echo "Buildhtmltask"
	 @ ./Buildhtmltask

  cleansys ::
	   $(RM) junk.doc2html.* junk.tmp.*

  help ::
	-@ echo ""
	-@ echo "Targets defined in the doc GNUmakefile"
	-@ echo "--------------------------------------"
	-@ echo "     allsys: regenerate documentation from doc files."
	-@ echo "   symlinks: create doc -> specdoc symlinks."
	-@ echo "      cheat: regenerate the Miriad cheat sheet."
	-@ echo "      tasks: regenerate the Miriad task list."
	-@ echo "        man: regenerate all man pages."
	-@ echo "       html: regenerate all top-level HTML files."
	-@ echo "  taskindex: regenerate the Miriad HTML task index."

else
  # Programmer-oriented rules.
  #---------------------------
  help ::
	-@ echo ""
	-@ echo "No programmer-oriented rules are defined in the doc"
	-@ echo "GNUmakefile."
endif
