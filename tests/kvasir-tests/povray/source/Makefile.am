##############################################################################
#               Makefile.am
#
# Process this file with automake to produce Makefile.in
#
# from Persistence of Vision(tm) Ray Tracer version 3.6.
# Copyright 1991-2003 Persistence of Vision Team
# Copyright 2003-2004 Persistence of Vision Raytracer Pty. Ltd.
#---------------------------------------------------------------------------
# NOTICE: This source code file is provided so that users may experiment
# with enhancements to POV-Ray and to port the software to platforms other
# than those supported by the POV-Ray developers. There are strict rules
# regarding how you are permitted to use this file. These rules are contained
# in the distribution and derivative versions licenses which should have been
# provided with this file.
#
# These licences may be found online, linked from the end-user license
# agreement that is located at http://www.povray.org/povlegal.html
#---------------------------------------------------------------------------
# This program is based on the popular DKB raytracer version 2.12.
# DKBTrace was originally written by David K. Buck.
# DKBTrace Ver 2.0-2.12 were written by David K. Buck & Aaron A. Collins.
##############################################################################

# Makefile.am for the source distribution of POV-Ray 3.6 for UNIX
# Written by Nicolas Calimet <pov4grasp@free.fr>

# Directories.
povlibdir = @datadir@/@PACKAGE@-@VERSION_BASE@
povdocdir = @datadir@/doc/@PACKAGE@-@VERSION_BASE@
povconfdir = @sysconfdir@/@PACKAGE@/@VERSION_BASE@
povconfuser = $(HOME)/.@PACKAGE@/@VERSION_BASE@

# Directories to build.
SUBDIRS = libraries source unix

# Additional files to distribute.
EXTRA_DIST = \
  bootstrap kde_install.sh \
  doc icons include ini libraries scenes scripts \
  povray.ini.in

# Additional files to clean with 'make distclean'.
# Note the little ';' trick for CONFIG_CLEAN_FILES.
DISTCLEANFILES = $(top_builddir)/povray.ini
CONFIG_CLEAN_FILES = $(top_builddir)/source/jversion.h

# Render a test scene for 'make check'.
# This is meant to run before 'make install'.
check: all
	$(top_builddir)/unix/povray +i$(top_srcdir)/scenes/advanced/biscuit.pov -f +d +p +v +w320 +h240 +a0.3 +L$(top_srcdir)/include

# Install scripts in povlibdir.
nobase_povlib_SCRIPTS = scripts/allanim.sh scripts/allscene.sh scripts/portfolio.sh scripts/render_anim.sh scripts/render_scene.sh scripts/rerunpov.sh scripts/runpov.sh

# Install documentation in povdocdir.
povdoc_DATA = AUTHORS ChangeLog NEWS

# Install configuration and INI files in povconfdir.
dist_povconf_DATA = povray.conf
povray.conf:

povconf_DATA = povray.ini
povray.ini:
	cat $(top_srcdir)/povray.ini.in | sed "s,__POVLIBDIR__,$(povlibdir),g" > $(top_builddir)/povray.ini

# Install man page.
dist_man_MANS = povray.1

# Remove all unwanted files in unix and libraries for 'make dist'.
# Make all files user read-writeable.
dist-hook:
	rm -f ./@PACKAGE@-@VERSION@/source/jversion.h
	rm -f `find ./@PACKAGE@-@VERSION@/libraries -name "*.o"`
	rm -f `find ./@PACKAGE@-@VERSION@/libraries -name "lib*.a"`
	rm -f `find ./@PACKAGE@-@VERSION@/libraries -name config.log`
	rm -f `find ./@PACKAGE@-@VERSION@/libraries -name config.status`
	rm -f `find ./@PACKAGE@-@VERSION@/libraries -name Makefile`
	rm -f ./@PACKAGE@-@VERSION@/libraries/tiff/libtiff/mkg3states
	rm -f ./@PACKAGE@-@VERSION@/libraries/tiff/libtiff/mkversion
	rm -f ./@PACKAGE@-@VERSION@/libraries/tiff/libtiff/port.h
	rm -f ./@PACKAGE@-@VERSION@/libraries/tiff/libtiff/tif_fax3sm.c
	rm -f ./@PACKAGE@-@VERSION@/libraries/tiff/libtiff/tiffvers.h
	rm -f ./@PACKAGE@-@VERSION@/libraries/tiff/port/install.sh
	chmod -R u+rw ./@PACKAGE@-@VERSION@
	chmod 655 ./@PACKAGE@-@VERSION@/scripts/*

# Manage various data files for 'make install'.
install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(povlibdir)
	@list='$(top_srcdir)/icons $(top_srcdir)/include $(top_srcdir)/ini $(top_srcdir)/scenes'; \
	echo "Creating data directories..."; \
	dirlist=`find $$list -type d | sed s,$(top_srcdir)/,,`; \
	for p in $$dirlist ; do \
	  $(mkinstalldirs) $(DESTDIR)$(povlibdir)/$$p ; \
	done; \
	echo "Copying data files..."; \
	filelist=`find $$list -type f | sed s,$(top_srcdir)/,,`; \
	for f in $$filelist ; do \
	  $(INSTALL_DATA) $(top_srcdir)/$$f $(DESTDIR)$(povlibdir)/$$f ; \
	done
	@echo "Creating documentation directories..."; \
	$(mkinstalldirs) $(DESTDIR)$(povdocdir); \
	dirlist=`find $(top_srcdir)/doc/ -type d | sed s,$(top_srcdir)/doc/,,`; \
	for p in $$dirlist ; do \
	  $(mkinstalldirs) $(DESTDIR)$(povdocdir)/$$p ; \
	done
	@echo "Copying documentation files..."; \
	filelist=`find $(top_srcdir)/doc/ -type f | sed s,$(top_srcdir)/doc/,,`; \
	for f in $$filelist ; do \
	  $(INSTALL_DATA) $(top_srcdir)/doc/$$f $(DESTDIR)$(povdocdir)/$$f ; \
	done
	@echo "Copying user configuration and INI files..."; \
	$(mkinstalldirs) $(povconfuser); \
	filelist='povray.conf povray.ini'; \
	for f in $$filelist ; do \
	  if test -e $(povconfuser)/$$f; then \
	    echo "Creating backup of $(povconfuser)/$$f"; \
	    mv -f $(povconfuser)/$$f $(povconfuser)/$$f.bak; \
	  fi; \
	done; \
	$(INSTALL_DATA) $(top_srcdir)/povray.conf $(povconfuser)/povray.conf ; \
	$(INSTALL_DATA) $(top_builddir)/povray.ini $(povconfuser)/povray.ini

# For 'make uninstall' we remove the whole POV-Ray data directory.
# We have to use uninstall-local and not uninstall-data-local.
uninstall-local:
	-rm -f -r $(DESTDIR)$(povlibdir)
	-rm -f -r $(DESTDIR)$(povdocdir)
	-rm -f $(DESTDIR)$(povconfdir)/povray.ini
	if test -d $(DESTDIR)$(povconfdir) && test -z "`ls -A $(DESTDIR)$(povconfdir)`"; then \
	  rm -f -r $(DESTDIR)$(povconfdir); \
	fi
	if test -d $(DESTDIR)$(sysconfdir)/@PACKAGE@ && test -z "`ls -A $(DESTDIR)$(sysconfdir)/@PACKAGE@`"; then \
	  rm -f -r $(DESTDIR)$(sysconfdir)/@PACKAGE@; \
	fi
	-rm -f $(HOME)/.@PACKAGE@/@VERSION_BASE@/povray.conf
	-rm -f $(HOME)/.@PACKAGE@/@VERSION_BASE@/povray.ini
	if test -d $(HOME)/.@PACKAGE@/@VERSION_BASE@ && test -z "`ls -A $(HOME)/.@PACKAGE@/@VERSION_BASE@`"; then \
	  rm -f -r $(HOME)/.@PACKAGE@/@VERSION_BASE@; \
	fi
	if test -d $(HOME)/.@PACKAGE@ && test -z "`ls -A $(HOME)/.@PACKAGE@`"; then \
	  rm -f -r $(HOME)/.@PACKAGE@; \
	fi
