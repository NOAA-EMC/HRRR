# libsrc/Makefile.  Generated from Makefile.in by configure.

# libsrc/ level makefile template

# Package-specific substitution variables
package = nceplibs sigio
version = v2.0.1
tarname = sigio
distdir = $(tarname)_$(version)

# Prefix-specific substitution variables
#prefix = /nwprod/lib/sorc/sigio_v2.0.1
prefix = .

# Mixed package and prefix substitution variables
installdir = $(prefix)/$(distdir)

# Compiler substitution variables
FC      = ifort
FCFLAGS = -O2 -g -xHOST -traceback -free -convert big_endian -assume byterecl -c
AR       = ar
ARFLAGS  = crvs
RANLIB   = ranlib
INSTALL      = /usr/bin/install -c
INSTALL_DATA = ${INSTALL} -m 644

# The library name
LIBRARY = lib$(tarname).a

# The file definitions. This include must occur before targets.
include make.filelist

# The targets
all: library

$(LIBRARY): $(OBJ_FILES)
	$(AR) $(ARFLAGS) $@ $(OBJ_FILES)
	$(RANLIB) $@

library: $(LIBRARY)

clean:
	-rm *.o *.mod *.a >/dev/null 2>&1

distclean:
	-rm Makefile >/dev/null 2>&1

check: library
	@echo "***THIS IS WHERE THE UNIT TEST INVOCATION GOES***"

# ...Gnu-style installation
install:
	$(INSTALL) -d $(DESTDIR)$(installdir)/include
	$(INSTALL_DATA) *.mod $(DESTDIR)$(installdir)/include
	$(INSTALL) -d $(DESTDIR)$(installdir)/lib
	$(INSTALL_DATA) $(LIBRARY) $(DESTDIR)$(installdir)/lib
	$(INSTALL_DATA) ../config.log $(DESTDIR)$(installdir)

# ...NCO-style installation
nco_install: FORCE
	$(INSTALL) -d $(DESTDIR)$(installdir)
	$(INSTALL_DATA) $(LIBRARY) $(DESTDIR)$(installdir)/lib$(distdir).a
	$(INSTALL) -d $(DESTDIR)$(installdir)/incmod/$(distdir)
	$(INSTALL_DATA) *.mod $(DESTDIR)$(installdir)/incmod/$(distdir)
	$(INSTALL_DATA) ../config.log $(DESTDIR)$(installdir)/config.log.$(distdir)

FORCE:
	@if [ -d $(DESTDIR)$(installdir) ]; then \
          echo; \
	  echo "*** NCO-style installation does not install into existing directories! ***"; \
	  echo "*** $(DESTDIR)$(installdir) already exists! ***"; \
          echo; \
	  exit 1; \
	fi

# ...Universal uninstallation
uninstall:
	-rm -fr $(DESTDIR)$(installdir) >/dev/null 2>&1

# ...Reconfig targets
#Makefile: Makefile.in ../config.status
#	cd .. && ./config.status libsrc/$@

#../config.status: ../configure
#	cd .. && ./config.status --recheck

# Specify targets that do not generate filesystem objects
.PHONY: all clean distclean check install nco_install uninstall

# Dependency include file
include make.dependencies

# Suffix rules
.SUFFIXES:
.SUFFIXES: .f .o
.f.o:
	$(FC) $(EXTRA_FCFLAGS) $(FCFLAGS) $<
