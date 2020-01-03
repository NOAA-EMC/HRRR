#
# Earth System Modeling Applications (ESMA) base makefile fragment.
# This fragment defines somewhat universal macros and compilation
# rules which are then costumized for each architecture in ESMA_arch.mk.
#
# REVISION HISTORY:
#
# 06Jun2003  da Silva  First Crack
# 04apr2005  da Silva  Max patch 
# 26apr2005  da Silva  Introduced MACH
# 26apr2005  da Silva  MADE BOPT=O the default
# 08Jun2006  Stassi    Added Check Environment section
# 26Jun2006  da SIlva  Removed Assert.pl related staff; see Assert.mk instead
#
#--------------------------------------------------------------------------

#                       ----------------
#                           Preamble
#                       ----------------

  SHELL	= /bin/sh

ifndef ARCH             # Architecture, e.g., IRIX64 
  ARCH := $(shell uname -s)
endif
ifndef MACH             # Hardware type, e.g., ia64
  MACH := $(shell uname -m)
endif
ifndef SITE             # Site name, e.g., halem3
  SITE := $(shell uname -n)
endif
ifndef NODE             # same as SITE name, except sometimes SITE comes predefined
  NODE := $(shell uname -n)
endif
ifndef BPREC
  BPREC := 64#  Build with "-r8"
endif

#                       ----------------
#                       Main Directories
#                       ----------------

# Installation Directories
# ------------------------
  ESMABIN = $(ESMADIR)/$(ARCH)/bin
  ESMALIB = $(ESMADIR)/$(ARCH)/lib
  ESMAINC = $(ESMADIR)/$(ARCH)/include
  ESMAMOD = $(ESMADIR)/$(ARCH)/include
  ESMAETC = $(ESMADIR)/$(ARCH)/etc
  ESMADOC = $(ESMADIR)/$(ARCH)/doc
  ESMACFG = $(ESMADIR)/$(ARCH)/Config
  ESMATST = $(ESMAETC)/testsuites

# Base Libraries and utilities
# ----------------------------
  BASEBIN = $(BASEDIR)/$(ARCH)/bin
  BASELIB = $(BASEDIR)/$(ARCH)/lib
  BASEINC = $(BASEDIR)/$(ARCH)/include
  BASEMOD = $(BASEDIR)/$(ARCH)/include
  BASEETC = $(BASEDIR)/$(ARCH)/etc

#                       ----------
#                       Utilities
#                       ----------

AR          = ar
AR_FLAGS    = cr
AR_EXTRACT  = -x 
AWK         = /bin/awk
CP          = /bin/cp -f
CAT         = /bin/cat
LN          = /bin/ln
MAKE        = gmake
MKDIR       = /bin/mkdir -p
PERL        = /usr/bin/perl
RANLIB      = /usr/bin/ranlib
RM          = /bin/rm -f
SED         = /bin/sed                       
TAR         = /bin/tar
GZIP        = gzip -v
BOPT        = O
M4          = m4
FDP         = $(ESMABIN)/fdp
FDP_FLAGS   = -v
STUB        = $(ESMABIN)/mapl_stub.pl
ACG         = $(ESMABIN)/mapl_acg.pl 
ACG_FLAGS   = -v
F90SPLIT    = $(ESMABIN)/f90split.x  # split f90 file by procedure
F90AIB      = $(ESMABIN)/f90aib.x    # automatic interface block
F2PY        = f2py   # python fortran extension builder
DLLEXT      = so     # extension for shared libraries
F2PYEXT     = so     # extension for python extensions

#                     -----------------------
#                      Documentation Support
#                     ----------------------

PROTEX       = $(ESMABIN)/protex
PROTEX_FLAGS = -g
LATEX        = latex
DVIPS        = dvips -Ppdf -G0 -f 
PS2PDF       = ps2pdf

#                     -----------------
#                      OPTIONAL TIMERS
#                     -----------------

ESMA_TIMER     = # command to time build steps (for compilation)
ESMA_TIMER_CI  = # command to start timer (for user to backet code segments)
ESMA_TIMER_CO  = # command to end   timer (for user to backet code segments)



#                     -----------------
#                         Libraries
#                     -----------------

LIB_SCI =
LIB_SYS =

DIR_HDF5 = $(BASEDIR)/$(ARCH)
INC_HDF5 = $(DIR_HDF5)/include/hdf5
LIB_HDF5 = $(wildcard $(foreach lib,hdf5_hl hdf5 z sz gpfs,\
           $(BASELIB)/lib$(lib).a) )

DIR_NETCDF = $(BASEDIR)/$(ARCH)
INC_NETCDF = $(DIR_NETCDF)/include/netcdf
ifeq ($(wildcard $(BASEBIN)/nc-config), )
    LIB_NETCDF = $(BASELIB)/libnetcdf.a $(LIB_HDF5)
else
    LIB_NETCDF := $(shell $(BASEBIN)/nc-config --flibs)
endif

DIR_HDF = $(BASEDIR)/$(ARCH)
INC_HDF = $(DIR_HDF)/include/hdf
LIB_HDF = $(wildcard $(foreach lib,mfhdf df hdfjpeg jpeg hdfz z sz,\
          $(BASELIB)/lib$(lib).a) )

ifeq ($(ESMA_SDF),hdf)
   INC_SDF = $(INC_HDF)
   LIB_SDF = $(LIB_HDF)
else
   INC_SDF = $(INC_NETCDF)
   LIB_SDF = $(LIB_NETCDF)
   ifneq ($(wildcard $(INC_SDF)/netcdf.inc), )
     ifneq ($(shell grep -c netcdf4 $(INC_SDF)/netcdf.inc),0)
        DEF_SDF += $(D)HAS_NETCDF4
     endif
     ifneq ($(shell grep -c 'netcdf version 3' $(INC_SDF)/netcdf.inc),0)
        DEF_SDF += $(D)HAS_NETCDF3
     endif
     ifneq ($(shell grep -c 'define H5_HAVE_PARALLEL 1' $(INC_HDF5)/H5pubconf.h),0)
        DEF_SDF += $(D)H5_HAVE_PARALLEL
     endif
   endif
endif

LIB_GCTP   = $(BASELIB)/libGctp.a
LIB_HDFEOS = $(BASELIB)/libhdfeos.a
LIB_EOS    = $(LIB_HDFEOS) $(LIB_GCTP)

DIR_ESMF = $(BASEDIR)
INC_ESMF = $(DIR_ESMF)/$(ARCH)/include/esmf
MOD_ESMF = $(DIR_ESMF)/$(ARCH)/include/esmf
LIB_ESMF = $(DIR_ESMF)/$(ARCH)/lib/libesmf.a

INC_MPI = /usr/include
LIB_MPI = -lmpi

DIR_THIS := $(shell basename `pwd`)
INC_THIS = $(ESMAINC)/$(DIR_THIS)
LIB_THIS = $(ESMALIB)/lib$(DIR_THIS).a

#                     -----------------------
#                     C Compiler/Loader Flags
#                     -----------------------

CDEFS     = -Dsys$(ARCH) -DESMA$(BPREC) $(USER_CDEFS)
CINCS     = $(foreach dir,$(INC_ESMF), $(I)$(dir)) $(USER_CINCS)

COPT0 = -O0
COPT1 = -O1
COPT2 = -O2
COPT3 = -O3
COPT4 = -O4
COPT5 = -O5
ifeq ("$(BOPT)","g")
   COPT   = -g
else
   COPT   = -O
endif

CC        = gcc
CXX       = g++
CPP       = cpp
PP        = -$(CPP)

CFLAGS    = $(CDEFS) $(CINCS) $(COPT) $(USER_CFLAGS)
CXXFLAGS  = $(CDEFS) $(CINCS) $(COPT) $(USER_CFLAGS)


#                     -------------------------
#                     f90 Compiler/Loader Flags
#                     -------------------------

I = -I# f90 compiler option for include file path
M = -I# f90 compiler option for module  file path
D = -D# f90 compiler option for cpp defines
DC = $(D)

FOPTG = -g
FOPT0 = -O0
FOPT1 = -O1
FOPT2 = -O2
FOPT3 = -O3
FOPT4 = -O4
FOPT5 = -O5
ifeq ("$(BOPT)","g")
   FOPT   = $(FOPTG)
else
   FOPT   = $(FOPT3)
endif

BIG_ENDIAN  =
BYTERECLEN  =
OMPFLAG     =
FREAL4      = 
FREAL8      = -r8
ifeq ( "$(BPREC)","32" )
      FREAL = $(FREAL4)
else
      FREAL = $(FREAL8)
endif
FINT4       = 
FINT8       = -i8
FINT        = $(FINT4)

FDEFS     = $(D)sys$(ARCH) $(D)ESMA$(BPREC) $(DEF_SDF) $(USER_FDEFS)
FINCS     = $(foreach dir,$(INC_ESMF), $(I)$(dir)) $(USER_FINCS)
FMODS     = $(foreach dir,$(INC_ESMF), $(M)$(dir)) $(USER_FMODS)
XFLAGS    = 

FC        = f90
fFLAGS    = $(FDEFS) $(FINCS) $(FMODS) $(FOPT) $(FREAL) $(FINT) $(XFLAGS) $(USER_FFLAGS)
f90FLAGS  = $(FDEFS) $(FINCS) $(FMODS) $(FOPT) $(FREAL) $(FINT) $(XFLAGS) $(USER_FFLAGS)
FFLAGS    = $(FDEFS) $(FINCS) $(FMODS) $(FOPT) $(FREAL) $(FINT) $(XFLAGS) $(USER_FFLAGS)
F90FLAGS  = $(FDEFS) $(FINCS) $(FMODS) $(FOPT) $(FREAL) $(FINT) $(XFLAGS) $(USER_FFLAGS)

FPP = /lib/cpp 
FPPFLAGS = -P $(DC)sys$(ARCH) $(FDEFS) $(FINCS) $(foreach dir,$(INC_MPI), $(I)$(dir))

LD = $(FC)
LDPATH  = -L$(BASELIB) -L$(ESMALIB)
LDFLAGS = $(LDPATH) $(USER_LDFLAGS)

#                     -----------------
#                     Compilation Rules
#                     -----------------

.SUFFIXES:
.SUFFIXES: .P90 .m4 .F90 .f90 .F .f .c .o .H .h .d .tex .dvi .pdf 

.c.o:
	$(ESMA_TIMER) $(CC) -c $(CFLAGS) $<

.C.o:
	$(ESMA_TIMER) $(CXX) -c $(CXXFLAGS) $<

.f.o:
	$(ESMA_TIMER) $(FC) -c $(fFLAGS) $<

.F.o:
	$(ESMA_TIMER) $(FC) -c $(FFLAGS) $<

.f90.o:
	$(ESMA_TIMER) $(FC) -c $(f90FLAGS) $<

.F90.o:
	$(ESMA_TIMER) $(FC) -c $(F90FLAGS) $<

.P90.o:
	@sed -e "/\!.*'/s/'//g" $< | $(CPP) -C -ansi -DANSI_CPP $(FPPFLAGS) > $*___.f90
	$(ESMA_TIMER) $(FC) -c $(f90FLAGS) -o $*.o $*___.f90
	@$(RM) $*___.f90

.H.h:
	$(FPP) $(FPPFLAGS) $*.H > $*.h

.m4.o:
	$(M4) $(M4FLAGS) $*.m4 > $*.F90
	$(FC) -c $(F90FLAGS) $*.F90
	$(RM) $*.F90

.c.d:
	@$(PERL) $(FDP) $(FDP_FLAGS) -c $<

.f.d:
	@$(PERL) $(FDP) $(FDP_FLAGS) -c $<

.F.d:
	-@$(CPP) $(FPPFLAGS) $< > $*___.f
	@$(PERL) $(FDP) -i $< $(FDP_FLAGS) -c $*___.f
	@$(RM) $*___.f

.f90.d:
	@$(PERL) $(FDP) $(FDP_FLAGS) -c $<

.F90.d:
	-@$(CPP) $(FPPFLAGS) $< > $*___.f90
	@$(PERL) $(FDP) -i $< $(FDP_FLAGS) -c $*___.f90
	@$(RM) $*___.f90

.P90.d:
	@$(PERL) $(FDP) -i $< $(FDP_FLAGS) -c $<

.m4.d:
	$(M4) $(M4FLAGS) $*.m4 > $*___.F90
	-@$(FPP) $(FPPFLAGS) $*___.F90 > $*___.f90
	@$(PERL) $(FDP) -i $< $(FDP_FLAGS) -c $*___.f90
	@$(RM) $*___.f90 $*___.F90


%___.lst : %.F90
	$(F90SPLIT) < $< > $*___.lst  

%Interfaces___.h : %.F90
	$(F90AIB) < $< | $(SED) -e "s/$*_L2/$*_L1/1" > $@


%.tex : %.f
	$(PROTEX) $(PROTEX_FLAGS) -f $< > $*.tex

%.tex : %.f90
	$(PROTEX) $(PROTEX_FLAGS) -f $< > $*.tex

%.tex : %.F
	$(CPP) -D__PROTEX__ $(FPPFLAGS) $(I)$(INC_ESMF) $< | \
        $(PROTEX) $(PROTEX_FLAGS) -f > $*.tex

%.tex : %.F90
	$(CPP) -D__PROTEX__ $(FPPFLAGS) $(I)$(INC_ESMF) $< | \
        $(PROTEX) $(PROTEX_FLAGS) -f > $*.tex

%.tex: %.h
	$(CPP) -D__PROTEX__ $(FPPFLAGS) $(I)$(INC_ESMF) $< | \
        $(PROTEX) $(PROTEX_FLAGS) -f > $*.tex

%.dvi: %.tex
	$(LATEX) $*
	$(LATEX) $*

%.pdf: %.dvi
	$(DVIPS) $*.dvi -o $*.ps # going thru ps tend to produce searchable PDF
	$(PS2PDF) $*.ps
	$(RM) -rf $*.ps

