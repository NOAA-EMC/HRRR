#
#  Changing this file requires mods to 
#	WRFV3_top/chem/KPP/util/wkc/change_chem_Makefile.c
#	Right now it looks at the line in MODULES with module_data_sorgam
#	AND it cares about the word DEPENDENCIES.  DO NOT EVEN CHANGE
#	SPACING ON THESE TWO LINES.

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f

MODULES =                                 \
        module_add_emiss_burn.o           \
        module_vertmx_wrf.o               \
        module_wetdep_ls.o                \
        module_zero_plumegen_coms.o       \
    	module_smoke_plumerise.o          \
        module_dep_simple.o               \
        module_input_smoke_data.o          \
        module_plumerise1.o               \

OBJS    =                           \
	smoke_init.o              \
	chem_driver.o               \
	emissions_driver.o          \
	dry_dep_driver.o            
#	aerosol_driver.o 

LIBTARGET    =  smoke
TARGETDIR    =  ./
$(LIBTARGET) :  MODULE DRIVERS
		$(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS)

MODULE  : $(MODULES)

DRIVERS : $(OBJS)

include ../configure.wrf

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

include depend.smoke

