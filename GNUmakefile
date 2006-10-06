#-----------------------------------------------------------------------------
# GNUmakefile used to compile Miriad.
#
# Original: 2006/08/28, Mark Calabretta, ATNF
# $Id: GNUmakefile,v 1.7 2006/10/06 05:44:01 cal103 Exp $
#-----------------------------------------------------------------------------
# Get common makefile variables and rules.
include $(MIR)/GNUmakedefs

ifeq "$(MAKEMODE)" "system"
  # Subdirectories in which to invoke "allsys", in order.
  #------------------------------------------------------
  # Scripts must be done first to check out architecture-specific GNUmakedefs.
  ALLSYSD  := scripts
  ALLSYSD  += $(findstring linpack,$(SUBDIRS))
  ALLSYSD  += tools inc subs prog spec guides

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

  initial :: FORCE
	-@ echo "Rebuilding/updating Miriad for $(ARCH) machines."

  help ::
	-@ echo ""
	-@ echo "Targets defined in the top-level GNUmakefile"
	-@ echo "--------------------------------------------"
	-@ echo "     allsys: recursively rebuild or update Miriad."


  # The following rules are for ATNF use only.
  ifdef ATNF
    # Files distributed separately.
    MIRFTPS  := DISCLAIMER INSTALL.html progguide.ps.gz progguide_US.ps.gz \
                userguide.ps.gz userguide_US.ps.gz

    # The Miriad distribution is split into RCS, code, compiled documentation,
    # and platform-specific binary kits.
    DISTRCS  := .rcs ./RCS ./*/RCS ./*/*/RCS ./*/*/*/RCS
    DISTCODE := DISCLAIMER GNUmake* MIRRC* INSTALL.html cat guides inc linpack
    DISTCODE += prog scripts spec specdoc subs tests tools 
    DISTDOC  := doc html man progguide*.ps userguide*.ps
    DISTBINS := $(subst /bin,,$(wildcard */bin))

    show ::
	-@ echo ""
	-@ echo "DISTRCS  = $(DISTRCS)"
	-@ echo ""
	-@ echo "MIRFTPS  = $(MIRFTPS)"
	-@ echo "DISTCODE = $(DISTCODE)"
	-@ echo "DISTDOC  = $(DISTDOC)"
	-@ echo "DISTBINS = $(DISTBINS)"


    # Pattern rules.
    #---------------
    VPATH := $(MIRROOT):$(MIRGUIDD)/user:$(MIRGUIDD)/prog

    $(MIRFTPD)/% : %
	-@ $(RM) $@
	   cp -p $< $@
	-@ chmod 644 $@

    $(MIRFTPD)/%.gz : %
	-@ $(RM) $@
	   cp -p $< $(MIRFTPD)/$*
	   gzip $(MIRFTPD)/$*
	-@ chmod 644 $@


    # Static and static pattern rules.
    #---------------------------------
    .PHONY : bookings dist updates

    # Update the copy of the RPFITS library and include file via allsys.
    initial :: $(MIRINCD)/rpfits.inc $(MIRLIBD)/librpfits.a

    $(MIRLIBD)/librpfits.a : /usr/local/lib/librpfits.a
	-@ $(RM) $@
	   cp $< $@

    $(MIRINCD)/rpfits.inc : /usr/local/include/rpfits.inc
	-@ rcs -l $@ < /dev/null
	-@ $(RM) $@
	   cp $< $@
	 @ ci -u -m"Updated from /usr/local/include/rpfits.inc." $@

    # Regenerate the Miriad ftp distribution kits.
    dist : allsys $(MIRFTPS:%=$(MIRFTPD)/%)
	-@ echo ""
	   tar cf miriad-rcs.tar $(DISTRCS)
	   gzip miriad-rcs.tar
	-@ $(RM) $(MIRFTPD)/miriad-rcs.tar.gz
	 @ mv miriad-rcs.tar.gz $(MIRFTPD)/
	-@ $(RM) .rcs-list
	 @ find . -name RCS | sort > .rcs-list
	   tar cXf .rcs-list miriad-code.tar $(DISTCODE)
	   gzip miriad-code.tar
	-@ $(RM) $(MIRFTPD)/miriad-code.tar.gz
	 @ mv miriad-code.tar.gz $(MIRFTPD)/
	 @ $(RM) .rcs-list
	   tar cf miriad-doc.tar $(DISTDOC)
	   gzip miriad-doc.tar
	-@ $(RM) $(MIRFTPD)/miriad-doc.tar.gz
	 @ mv miriad-doc.tar.gz $(MIRFTPD)/
	 @ for bin in $(DISTBINS) ; do \
	     echo "tar cf miriad-$$bin.tar $$bin" ; \
	     tar cf miriad-$$bin.tar $$bin ; \
	     echo "gzip miriad-$$bin.tar" ; \
	     gzip miriad-$$bin.tar ; \
	     $(RM) $(MIRFTPD)/miriad-$$bin.tar.gz ; \
	     mv miriad-$$bin.tar.gz $(MIRFTPD)/ ; \
	   done

    bookings :
	-@ echo ""
	-@ echo "Checking for users needing help..."
	 @ cd $(MIRROOT)/at_friends && ./CheckBookings.csh

    updates :
	-@ echo ""
	-@ echo "Creating updates..."
	 @ $(MIRBIND)/mirexport

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
