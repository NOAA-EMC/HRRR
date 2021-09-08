SHELL	=/bin/sh
 INCMOD1	=$(LIBDIR)/incmod/sigio_4
 INCMOD2	=$(LIBDIR)/incmod/sfcio_4
 INCMOD3	=$(LIBDIR)/incmod/landsfcutil_d
#INCMOD4	=$(LIBDIR)/incmod/gfsio_4
 INCMOD4	=/climate/save/wx20wa/gfsio/gfsio_20110819/incmod/gfsio_4
#

SRCM	=chgres.f
SRCS	=
OBJS	=nstio_module.o nsst_chgres.o gfsio_rst.o funcphys.o physcons.o machine_8.o surface_chgres.o sfcsub.o vintg_idea.o nrlmsise00_sub.o
FC	=$(FCMP)
FC90	=$(FCMP95)

FFLAGS	= -qnodbg -qrealsize=8 -qnosave -qsmp=noauto:omp -O3 -qmaxmem=-1
FFLAGS2	= -qnodbg -qrealsize=8 -qnosave -qsmp=noauto:omp -O3 -qmaxmem=-1
#FFLAGS	= -g -qnosave -qsmp=noauto -O0 -qmaxmem=-1 -qsmallstack=dynlenonheap
#FFLAGS2= -g -qnosave -qsmp=noauto -O0 -qmaxmem=-1 -qsmallstack=dynlenonheap
#FFLAGS	= -g -qnosave -qsmp=noauto -O0 -qmaxmem=-1 -qinitauto=7FBFFFFF -qflttrap=enable:overflow:zerodivide:invalid -qsigtrap=xl__trcedump -qcheck -qextchk

LDFLAGS	= -lesslsmp

LIBS	= -L$(LIBDIR) -lsigio_4 -llandsfcutil_d -lbacio_4 -lip_d -lw3_d \
           -lsp_d  -lsfcio_4 /climate/save/wx20wa/gfsio/gfsio_20110819/libgfsio_4.a

CMD	=global_chgres
$(CMD):	$(SRCM) $(OBJS)
	$(FC) -I $(INCMOD1) -I $(INCMOD2) -I $(INCMOD3) -I $(INCMOD4) $(FFLAGS) $(LDFLAGS) $(SRCM) $(OBJS) $(LIBS) -o $(CMD)


nstio_module.o : nstio_module.f
	$(FC) $(FFLAGS) -qfree=f90 -c nstio_module.f
nsst_chgres.o : nsst_chgres.f
	$(FC) $(FFLAGS) -qfree=f90 -c nsst_chgres.f
vintg_idea.o:	vintg_idea.f
	$(FC) $(FFLAGS2) -c vintg_idea.f
nrlmsise00_sub.o:	nrlmsise00_sub.f
	$(FC) $(FFLAGS2) -c nrlmsise00_sub.f
gfsio_rst.o:      gfsio_rst.f
	$(FC90) $(FFLAGS2) -c gfsio_rst.f
funcphys.o:     funcphys.f physcons.o machine_8.o
	$(FC) $(FFLAGS) -qfree=f90 -c funcphys.f
physcons.o:     physcons.f machine_8.o
	$(FC) $(FFLAGS) -qfree=f90 -c physcons.f
machine_8.o:      machine_8.f
	$(FC) $(FFLAGS) -qfree=f90 -c machine_8.f
sfcsub.o:      sfcsub.f
	$(FC) $(FFLAGS2) -c sfcsub.f
surface_chgres.o: sfcsub.o
	$(FC) $(FFLAGS) -d -WF,-DGFS=1 -I $(INCMOD3) -g -qfree=f90 -c surface_chgres.F
clean:
	-rm -f $(OBJS) *.mod
clobber:        clean
	-rm -f $(CMD)
void:   clobber
	-rm -f $(SRCS) $(SRCM) makefile
