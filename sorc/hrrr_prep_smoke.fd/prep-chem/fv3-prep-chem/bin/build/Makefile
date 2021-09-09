# Define path and compilation 

-include paths.mk
-include include.mk.$(OPT)

# Compiler commands.

INCLUDES   = -I$(LIB_RAMS_PATH)/include $(NETCDF_INC) $(HDF5_INC)
LIBS       = $(NETCDF_LIBS) $(HDF5_LIB)
#INCLUDES   = -I$(LIB_RAMS_PATH)/include $(NETCDF_INC)
#LIBS       = $(NETCDF_LIBS)
F_COMMAND  = $(F_COMP) -c $(F_OPTS) $(INCLUDES) 
C_COMMAND  = $(C_COMP) -c $(C_OPTS) $(INCLUDES) 

# Define objects.

-include objects_prepsrc.mk

# Define targets.

AER      ?= SIMPLE
EXE       = ../../../../workflow/emc-global/exec/prep_chem_sources_$(CHEM)_$(AER).exe
RAMSUTILS = ramsutils.a
RAMSLIB   = ramslib.a

all:  $(RAMSUTILS) $(RAMSLIB) $(WRFFIMUTILS) $(EXE)
	@echo ""
	@echo Finished building === $(EXE)
	@echo ""

$(RAMSUTILS): $(OBJ_RAMS_UTILS)
	$(ARCHIVE) $(RAMSUTILS) $(OBJ_RAMS_UTILS)

$(RAMSLIB): $(OBJ_RAMS)
	$(ARCHIVE) $(RAMSLIB) $(OBJ_RAMS)



$(EXE): $(OBJ_PREPSRC)
	$(LOADER) $(LOADER_OPTS) -o $(EXE) $(OBJ_PREPSRC) $(RAMSLIB) $(RAMSUTILS) $(LIBS) 


clean:
	@echo ""
	rm -f *.o *.mod *.a *.oo *.ipo $(LIBUTILS) $(PREPSOURCE) $(RAMSLIB) $(WRFFIMUTILS)  $(EXE) 
	@echo ""
      
# Define dependencies and rules

include depend_prepsrc.mk
