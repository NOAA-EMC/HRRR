################################################################################
# 
#     Makefile for WRFBUFR code
#
#     Use:
#     make         -  build the executable
#     make clean   -  start with a clean slate
#
#     The following macros will be of interest:
#
#         TARGET   - name of the executable
#         FC       - name of Fortran compiler
#         CPP      - name of CPP
#         ARCH     - architecture
#         CPPFLAGS - CPP flags
#         OPTS     - compiler code optimizations
#         LIST     - source listing
#         SMP      - threading
#         TRAPS    - runtime traps for floating point exceptions
#         PROFILE  - source code profiling ( -pg )
#         DEBUG    - -g
#         MEM      - user data area and stack size
#         MAP      - load map
#         W3LIB    - w3lib
#         BACIO    - bacio lib
#         ESSL     - ESSL library
#         MASS     - MASS library
#         HPMLIB   - hpm lib
#         SEARCH   - library search location
#
#################################################################################
#
# Define the name of the executable
#
TARGET = ../../exec/hrrr_wrfbufr 
#
# CPP, Compiler, and Linker Options
#

NETCDF_LIB = ${NETCDF}/lib/libnetcdf.a

FC       = ftn 
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =
OPTS     = -O0 -convert big_endian
LIST     =
FREE     = -FR
TRAPS    =
PROFILE  =
DEBUG    = # -g # -traceback -fpe0 -ftrapuv # -check all
MEM      =
MAP      =

WRFLIB    = ./wrflibs/libwrflib.a  \
            ./wrflibs/pack_utils.o  \
            ./wrflibs/libfftpack.a \
            ./wrflibs/libio_grib1.a \
            ./wrflibs/libio_grib_share.a \
            ./wrflibs/libwrfio_int.a \
            ./wrflibs/libwrfio_nf.a \
            ./wrflibs/libesmf_time.a \
            ./wrflibs/librsl_lite.a

SEARCH   =
#
# Assemble Options
#
FFLAGS   = $(OPTS) $(LIST) $(TRAPS) $(PROFILE) $(DEBUG) $(WRFFLAGS) -I$(NEMSIO_INC)
FFLAGST  = $(OPTS) $(LIST) $(FREE) $(TRAPS) $(PROFILE) $(DEBUG) $(WRFFLAGS) -I$(NEMSIO_INC)
LDFLAGS  = $(MEM) $(MAP) $(SMP) $(PROFILE)
LIBS     = $(WRFLIB) \
           $(NEMSIO_LIB) \
           ${W3EMC_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} \
           ${NETCDF_LDFLAGS}

#
#
# free-format object files
#
OBJST=	module_internal_header_util.o getIVariableB.o getIVariable.o getVariableB.o getVariable.o \
        kinds_mod.o \
        count_recs_wrf_binary_file.o inventory_wrf_binary_file.o \
        next_buf.o retrieve_index.o
#
# fixed-format object files
#
OBJS=	VRBLS2D_mod.o VRBLS3D_mod.o MASKS_mod.o SOIL_mod.o \
 WRFBUFR.o PROF_EM.o PROF_NMM.o PROF_NMM_NET.o PROF_EM_NET.o \
iw3jdn.o w3fs26.o w3movdat.o w3reddat.o gdswiz.o gdswiz01.o \
gdswiz03.o gdswiz05.o

#
# Includes
#
INCLUDES= parmsoil
#
# Common Blocks
#
COMMS=

DEPS= $(COMMS) $(INCLUDES)

.SUFFIXES:	.F .f .o

.F.f:
	$(CPP) $(CPPFLAGS) $< > $*.f

$(TARGET):      $(OBJST) $(OBJS)
	$(FC) $(LDFLAGS) -o $@ $(OBJST) $(OBJS) $(LIBS)

$(OBJS):        $(DEPS)
	$(FC) -c $(FFLAGS) $(@:.o=.f)

$(OBJST):       $(DEPS)
	$(FC) -c $(FFLAGST) $(@:.o=.f)

clean:	
	/bin/rm -f  *.lst *.o *.mod
#
