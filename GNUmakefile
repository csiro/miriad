#-----------------------------------------------------------------------------
# GNUmakefile used to compile Miriad.
#
# Original: 2006/08/28, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.23 2007/03/23 05:46:57 cal103 Exp $
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

  help ::
	-@ echo ""
	-@ echo "Targets defined in the top-level GNUmakefile"
	-@ echo "--------------------------------------------"
	-@ echo "     allsys: recursively rebuild or update Miriad."


  # The following rules are for ATNF use only.
  ifdef MIRATNF
    # Files distributed separately.
    MIRFTPS  := DISCLAIMER INSTALL.html README \
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
    # RPFITS & PGPLOT into the Miriad system directories for export.
    # Darwin requires that the library be ranlib'd if moved.
    define mir-copy
      -@ $(RM) $@
         cp $< $@
      -@ chgrp miriad $@
      -@ case $* in lib*.a) echo $(RANLIB) $@ ; $(RANLIB) $@ ;; esac
      -@ chmod 664 $@
    endef

    $(MIRLIBD)/% : /usr/local/lib/%
	   $(mir-copy)

    $(MIRLIBD)/% : /usr/local/gnu/lib/%
	   $(mir-copy)

    $(MIRLIBD)/% : /sw/lib/%
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
    .PHONY : bookings dist updates

    # Update the copy of the RPFITS library and include file via allsys.
    initial :: rpfits pgplot
      ifdef MIRRCS
        # Update local stuff and architecture-specific GNUmakedefs.
	-@ $(MAKE) -C etc chkout
	-@ $(MAKE) -C scripts chkout
	-@ $(MAKE) -C inc chkout
      endif

    rpfits : $(MIRINCD)/rpfits.inc $(MIRLIBD)/librpfits.a

    $(MIRINCD)/rpfits.inc : /usr/local/include/rpfits.inc
	-@ rcs -l $@ < /dev/null
	-@ $(RM) $@
	   cp $< $@
	 @ ci -u -m"Updated from /usr/local/include/rpfits.inc." $@

    pgplot : $(patsubst %,$(MIRLIBD)/lib%.a,pgplot png z) \
      $(addprefix $(MIRLIBD)/,grfont.dat rgb.txt) \
      $(addprefix $(MIRBIND)/,pgdisp pgxwin_server)

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
	 @ cd .. ; find miriad -name RCS | sort > miriad/.tarX
	   cd .. ; tar cXf miriad/.tarX miriad/miriad-code.tar $(DISTCODE:%=miriad/%)
	   gzip miriad-code.tar
	-@ $(RM) $(MIRFTPD)/miriad-code.tar.gz
	   mv miriad-code.tar.gz $(MIRFTPD)/
	 @ $(RM) .tarX
	-@ echo ""
	-@ $(TIMER)
	-@ $(RM) VERSION
	   date -u +'%Y%m%d' > VERSION
	-@ chmod 444 VERSION
	   cd .. ; tar cf miriad/miriad-common.tar $(DISTCOMM:%=miriad/%)
	-@ $(RM) VERSION
	   gzip miriad-common.tar
	-@ $(RM) $(MIRFTPD)/miriad-common.tar.gz
	   mv miriad-common.tar.gz $(MIRFTPD)/
	-@ echo ""
	 @ for bin in $(DISTBINS) ; do \
	     echo "" ; \
	     $(TIMER) ; \
	     echo "tar cf miriad-$$bin.tar $$bin" ; \
	     echo miriad/$$bin/GNUmakedefs > .tarX ; \
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
	 @ cd $(MIRROOT)/at_friends && ./CheckBookings.csh

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
