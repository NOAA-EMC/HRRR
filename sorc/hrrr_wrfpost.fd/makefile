#
# Define the name of the executable
#
TARGET   = ../../exec/hrrr_wrfpost

#
# CPP, Compiler, and Linker Options
#
FC       = ftn
CPP      = /lib/cpp -P
CPPFLAGS = -DLINUX 
FREE     = -FR
DEBUG    = -g

FFLAGS = -O2 -convert_big_endian -traceback -g -fp-model source -fpp -qopenmp
CFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'
#

#WRF_VER=v1.1.0
#WRFPATH    = /gpfs/hps/nco/ops/nwprod/wrf_shared.$(WRF_VER)-intel
#WRF_INC    = $(WRFPATH)/external/io_quilt -I$(WRFPATH)/frame
#WRF_LIB    = $(WRFPATH)/main/libwrflib.a  \
             $(WRFPATH)/frame/pack_utils.o  \
             $(WRFPATH)/frame/module_internal_header_util.o \
             $(WRFPATH)/external/io_grib1/libio_grib1.a \
             $(WRFPATH)/external/io_grib_share/libio_grib_share.a \
             $(WRFPATH)/external/io_int/libwrfio_int.a \
             $(WRFPATH)/external/io_netcdf/libwrfio_nf.a \
             $(WRFPATH)/external/esmf_time_f90/libesmf_time.a \
             $(WRFPATH)/external/RSL_LITE/librsl_lite.a


NETCDFPATH = ${NETCDF}
NETCDF_LIB = -L $(NETCDFPATH)/lib -lnetcdf -lnetcdff
NETCDF_INC = $(NETCDFPATH)/include

#
# Assemble Options
#
FFLAGS   = $(OPTS) $(FREE) $(TRAPS) $(DEBUG) \
           -I$(WRF_IO_INC) \
           -I$(G2_INC4) \
           -I$(G2TMPL_INC) \
           -I$(NEMSIO_INC) \
           -I$(GFSIO_INC) \
           -I$(SIGIO_INC) \
           -I$(SFCIO_INC) \
           -I$(CRTM_INC) \
           -I$(NETCDF_INC)

LIBS     = $(WRF_IO_LIB) \
           $(G2TMPL_LIB) \
           $(G2_LIB4) \
           $(JASPER_LIB) \
           $(PNG_LIB) \
           $(Z_LIB) \
           $(NEMSIO_LIB) \
           $(GFSIO_LIB) \
           $(SIGIO_LIB) \
           $(SFCIO_LIB) \
           $(IP_LIB4) \
           $(SP_LIB4) \
           $(W3NCO_LIB4) \
           $(BACIO_LIB4) \
           $(CRTM_LIB) \
           $(NETCDF_LIB)


OBJS_FT = wrf_io_flags.o getVariable.o  \
          getIVariableN.o kinds_mod.o machine.o physcons.o \
          native_endianness.o \
          retrieve_index.o ZENSUN.o \
          CLDFRAC_ZHAO.o GFSPOST.o GETGBANDSCATTER.o \
          blockIO.o

# -----------
# Non-threaded object files
# -----------

OBJS_F =  VRBLS2D_mod.o VRBLS3D_mod.o VRBLS4D_mod.o MASKS_mod.o PMICRPH.o SOIL_mod.o CMASSI.o \
          CTLBLK.o GRIDSPEC.o \
          LOOKUP.o PARAMR.o RHGRD.o RQSTFLD.o xml_perl_data.o cuparm.o params.o svptbl.o get_postfilename.o grib2_module.o \
          SET_LVLSXML.o FILL_PSETFLD.o BNDLYR.o  BOUND.o  CALCAPE.o \
          CALDWP.o  CALDRG.o CALHEL.o  CALLCL.o  CALMCVG.o CALPOT.o  CALPW.o CALRH.o  CALRCH.o \
          CALRH_GSD.o CALSTRM.o CALTAU.o CALTHTE.o CALVIS.o CALVIS_GSD.o CALVOR.o CALWXT.o $(LINUX_OBJ)\
          CALWXT_RAMER.o CALWXT_BOURG.o CALWXT_REVISED.o CALRH_PW.o CALWXT_EXPLICIT.o \
          CALWXT_DOMINANT.o CLDRAD.o \
          CLMAX.o COLLECT.o  COLLECT_LOC.o DEWPOINT.o FDLVL.o  FGAMMA.o FIXED.o  FRZLVL.o  FRZLVL2.o \
          GET_BITS.o  GRIBIT.o INITPOST.o LFMFLD.o  MAPSSLP.o MISCLN.o MIXLEN.o MDL2P.o \
          MDLFLD.o MPI_FIRST.o  MPI_LAST.o NGMFLD.o NGMSLP.o  OTLFT.o OTLIFT.o SLP_new.o SLP_NMM.o \
          EXCH.o PARA_RANGE.o PROCESS.o INITPOST_NMM.o EXCH2.o READCNTRL.o  READ_xml.o \
          SET_OUTFLDS.o SCLFLD.o  SERVER.o \
          SETUP_SERVERS.o SMOOTH.o SURFCE.o SPLINE.o  TABLE.o  TABLEQ.o  TRPAUS.o  TTBLEX.o WETBULB.o \
          WRFPOST.o CALMICT.o MICROINIT.o GPVS.o MDL2SIGMA.o ETCALC.o CANRES.o \
          CALGUST.o WETFRZLVL.o SNFRAC.o MDL2AGL.o SNFRAC_GFS.o AVIATION.o DEALLOCATE.o \
          CALPBL.o MDL2SIGMA2.o INITPOST_GFS.o CALRH_GFS.o LFMFLD_GFS.o \
          CALRAD_WCLOUD_newcrtm.o MDL2THANDPV.o CALPBLREGIME.o POLEAVG.o INITPOST_NEMS.o \
          GETNEMSNDSCATTER.o ICAOHEIGHT.o INITPOST_GFS_NEMS.o \
          GEO_ZENITH_ANGLE.o GFIP3.o GRIDAVG.o CALUPDHEL.o MSFPS.o INITPOST_GFS_SIGIO.o\
          AllGETHERV_GSD.o SELECT_CHANNELS.o ALLOCATE_ALL.o INITPOST_NEMS_MPIIO.o ASSIGNNEMSIOVAR.o INITPOST_GFS_NEMS_MPIIO.o \
          INITPOST_NETCDF.o gtg_config.o gtg_filter.o gtg_trophts.o gtg_itfa.o gtg_indices.o \
          map_routines.o CALHEL2.o CALCAPE2.o

OBJS   = $(OBJS_FT) $(OBJS_F)


.SUFFIXES: .F .f .o .f90 .c

.F.f:
	$(CPP) $(CPPFLAGS) $< > $*.f

$(TARGET): $(OBJS)
	$(FC) -qopenmp -o $@ $(OBJST) $(OBJXML) $(OBJS) $(LIBS)

.f.o:
	$(FC) $(FFLAGS) -c $<

.f90.o:
	$(FC) $(FFLAGS) -c $<

.c.o :
	${CC} ${CFLAGS} -c $<


clean:
	/bin/rm -f  $(TARGET) *.lst *.o *.mod
