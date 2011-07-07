#-----------------------------------------------------------------------------
# GNUmakefile used to compile Miriad.
#
# Original: 2006/08/28, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.37 2011/07/07 02:04:21 cal103 Exp $
#-----------------------------------------------------------------------------
ifeq "$(MIR)" ""
  # Try to deduce basic Miriad environment variables.  Obviously this only
  # works if make is invoked in the top-level Miriad directory.
  export MIR     := $(shell pwd)
  export MIRARCH := $(shell $(MIR)/scripts/mirarch)
  export MIRBIN  := $(MIR)/$(MIRARCH)/bin
endif

# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

ifeq "$(MAKEMODE)" "system"
  # Subdirectories in which to invoke "allsys", in order.
  #------------------------------------------------------
  ALLSYSD  += $(findstring linpack,$(SUBDIRS))
  ALLSYSD  += tools scripts inc subs prog spec guides

  show ::
	-@ echo ""
	-@ echo "Variables defined in the top-level GNUmakefile"
	-@ echo "=============================================="
	-@ echo ""
	-@ echo "ALLSYSD  = $(ALLSYSD)"

  # Static and static pattern rules.
  #---------------------------------
  .PHONY : initial

  allsys :: initial MIRRC MIRRC.sh $(ALLSYSD)

  # Announce what we're about to do.
  initial :: FORCE
	-@ echo ""
	-@ echo "Rebuilding/updating Miriad for $(MIRARCH) machines."
	-@ $(TIMER)

  MIRRC : scripts/MIRRC.in
	-@ echo ""
	   sed -e "s,@MIRROOT@,$(MIR)," $< > $@
	-@ chmod 644 $@

  MIRRC.sh : scripts/MIRRC.sh.in
	-@ echo ""
	   sed -e "s,@MIRROOT@,$(MIR)," $< > $@
	-@ chmod 644 $@

  cleansys ::
	   $(RM) -r $(MIRTMPD)/*

  help ::
	-@ echo ""
	-@ echo "Targets defined in the top-level GNUmakefile"
	-@ echo "--------------------------------------------"
	-@ echo "     allsys: recursively rebuild or update Miriad."


  # The following rules are for ATNF use only.
  ifdef MIRATNF
    # Files distributed separately.
    MIRFTPS  := DISCLAIMER INSTALL.html \
                progguide.ps.gz progguide_US.ps.gz \
                userguide.ps.gz userguide_US.ps.gz

    # The Miriad distribution is split into RCS, code, common runtime files,
    # and platform-specific binary kits.
    DISTRCS  := .rcs RCS */RCS */*/RCS */*/*/RCS
    DISTCODE := GNUmake* config configure configure.ac
    DISTCODE += guides inc linpack prog spec subs tests tools
    DISTCOMM := DISCLAIMER INSTALL.html VERSION progguide* userguide*
    DISTCOMM += cat doc html man scripts specdoc
    DISTBINS := $(subst /bin,,$(wildcard */bin))

    show ::
	-@ echo ""
	-@ echo "DISTRCS  = $(DISTRCS)"
	-@ echo ""
	-@ echo "MIRFTPS  = $(MIRFTPS)"
	-@ echo "DISTCODE = $(DISTCODE)"
	-@ echo "DISTCOMM = $(DISTCOMM)"
	-@ echo "DISTBINS = $(DISTBINS)"


    # Pattern rules.
    #---------------
    VPATH := $(MIRROOT):$(MIRGUIDD)/user:$(MIRGUIDD)/prog

    # For copying third-party libraries and associated utilities such as
    # PGPLOT, RPFITS, and WCSLIB into the Miriad system directories for
    # export.  Darwin requires that the library be ranlib'd if its time of
    # last modification changes.
    define mir-copy
      -@ $(RM) $@
         cp $< $@
      -@ chgrp miriad $@
      -@ case $@ in *.a) echo $(RANLIB) $@ ; $(RANLIB) $@ ;; esac
      -@ chmod 664 $@
    endef

    $(MIRLIBD)/% : /usr/local/lib/%
	   $(mir-copy)

    $(MIRLIBD)/%_mir.a : /usr/local/lib/%.a
	   $(mir-copy)

    $(MIRLIBD)/% : /usr/local/gnu/lib/%
	   $(mir-copy)

    $(MIRLIBD)/% : /usr/lib/%
	   $(mir-copy)

    $(MIRBIND)/% : /usr/local/bin/%
	   $(mir-copy)
	-@ chmod a+x $@

    $(MIRBIND)/% : /usr/bin/%
	   $(mir-copy)
	-@ chmod a+x $@

    # Don't worry if they can't be found.
    $(MIRLIBD)/% : ;
    $(MIRBIND)/% : ;

    # For installing the ftp distribution files.
    $(MIRFTPD)/% : %
	   $(mir-copy)

    $(MIRFTPD)/% : etc/%
	   $(mir-copy)

    $(MIRFTPD)/%.gz : %
	-@ $(RM) $@
	   cp $< $(MIRFTPD)/$*
	   gzip $(MIRFTPD)/$*
	-@ chgrp miriad $@
	-@ chmod 664 $@


    # Static and static pattern rules.
    #---------------------------------
    .PHONY : bookings dist pgplot rpfits updates wcslib

    # Update the copy of external libraries and include files via allsys.
    initial :: pgplot rpfits wcslib
      ifdef MIRRCS
        # Update local stuff and architecture-specific GNUmakedefs.
	-@ echo ""
	-@ echo gmake[0]: $(MAKE) -C cat chkout
	-@ $(MAKE) -C cat chkout
	-@ echo ""
	-@ echo gmake[0]: $(MAKE) -C etc chkout
	-@ $(MAKE) -C etc chkout
	-@ echo ""
	-@ echo gmake[0]: $(MAKE) -C inc chkout
	-@ $(MAKE) -C inc chkout
	-@ echo ""
	-@ echo gmake[0]: $(MAKE) -C scripts chkout
	-@ $(MAKE) -C scripts chkout
      endif

    pgplot : $(patsubst %,$(MIRLIBD)/lib%.a,$(PGPLOT) $(PNG) z) \
      $(addprefix $(MIRLIBD)/,grfont.dat rgb.txt) \
      $(addprefix $(MIRBIND)/,pgdisp pgxwin_server)

    rpfits : $(MIRINCD)/rpfits.inc $(MIRLIBD)/librpfits.a

    $(MIRINCD)/rpfits.inc : /usr/local/include/rpfits.inc
	-@ rcs -l $@ < /dev/null
	-@ $(RM) $@
	   cp $< $@
	 @ ci -u -m"Updated from /usr/local/include/rpfits.inc." $@

    wcslib : $(MIRINCD)/wcslib $(MIRLIBD)/libwcs.a \
      $(addprefix $(MIRBIND)/,HPXcvt fitshdr wcsgrid wcsware)

    $(MIRINCD)/wcslib : /usr/local/include/wcslib
	-@ mkdir -m 2775 -p -v $@
	   cd $< && rsync -v --recursive --perms --delete . $@

    ifeq "$(MIRARCH)" "sun4sol"
      # Regenerate the Miriad ftp distribution kits.  Requires the sun4sol
      # variant of tar.
      allsys :: $(MIRFTPS:%=$(MIRFTPD)/%)

      dist : allsys configure
	-@ echo ""
	-@ $(TIMER)
	   cd .. ; tar cf miriad/miriad-rcs.tar $(DISTRCS:%=miriad/%)
	   gzip miriad-rcs.tar
	-@ $(RM) $(MIRFTPD)/miriad-rcs.tar.gz
	   mv miriad-rcs.tar.gz $(MIRFTPD)/
	-@ echo ""
	-@ $(TIMER)
	-@ $(RM) .tarX
	 @ cd .. ; find miriad -path miriad/.mirsync -prune -o -name RCS | \
	             sort > miriad/.tarX
	 @ cd .. ; ls miriad/*/GNUmakedefs >> miriad/.tarX
	   cd .. ; tar cXf miriad/.tarX miriad/miriad-code.tar $(DISTCODE:%=miriad/%)
	   gzip miriad-code.tar
	-@ $(RM) $(MIRFTPD)/miriad-code.tar.gz
	   mv miriad-code.tar.gz $(MIRFTPD)/
	-@ echo ""
	-@ $(TIMER)
	-@ $(RM) VERSION
	   date -u +'%Y%m%d' > VERSION
	-@ chmod 444 VERSION
	   cd .. ; tar cXf miriad/.tarX miriad/miriad-common.tar $(DISTCOMM:%=miriad/%)
	-@ $(RM) VERSION
	   gzip miriad-common.tar
	-@ $(RM) $(MIRFTPD)/miriad-common.tar.gz
	   mv miriad-common.tar.gz $(MIRFTPD)/
	-@ echo ""
	 @ for bin in $(DISTBINS) ; do \
	     echo "" ; \
	     $(TIMER) ; \
	     echo "tar cf miriad-$$bin.tar $$bin" ; \
	     tar cXf .tarX miriad-$$bin.tar -C .. miriad/$$bin ; \
	     echo "gzip miriad-$$bin.tar" ; \
	     gzip miriad-$$bin.tar ; \
	     $(RM) $(MIRFTPD)/miriad-$$bin.tar.gz ; \
	     echo "mv miriad-$$bin.tar.gz $(MIRFTPD)/" ; \
	     chmod 664 miriad-$$bin.tar.gz ; \
	     mv miriad-$$bin.tar.gz $(MIRFTPD)/ ; \
	   done
	-@ $(RM) .tarX

      configure : configure.ac
	-@ echo ""
	-@ $(TIMER)
	   autoconf

      bookings :
	-@ echo ""
	-@ $(TIMER)
	-@ echo "Checking for users needing help..."
	 @ cd $(MIRROOT)/at_friends && ./CheckBookings

      updates :
	-@ echo ""
	-@ $(TIMER)
	-@ echo "Creating updates..."
	 @ $(MIRBIND)/mirexport
    endif

    help ::
	-@ echo "       dist: allsys, then generate distribution kits."
	-@ echo "   bookings: check disk bookings."
	-@ echo "    updates: create ftp update tar files."
  endif

else
  # Programmer-oriented rules.
  #---------------------------
  help ::
	-@ echo ""
	-@ echo "No programmer-oriented rules are defined in the top-level"
	-@ echo "GNUmakefile."
endif
