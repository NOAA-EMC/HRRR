# Define path and compilation 

include paths.mk
-include include.mk.$(OPT)

# Compiler commands.

INCLUDES   = -I$(LIB_RAMS_PATH)/include -I${NETCDF}/include #-I${HDF5_INC}
LIBS       = -L${NETCDF}/lib -lnetcdf -lnetcdff

F_COMMAND  = $(F_COMP) -c $(F_OPTS) $(INCLUDES) 
# NUWRF...Added CUNDERSCORE
#C_COMMAND  = $(C_COMP) -c $(C_OPTS) -D$(CMACH) -D$(CUNDERSCORE) $(INCLUDES)
C_COMMAND  = $(C_COMP) -c $(C_OPTS) -D$(CMACH) $(INCLUDES)


# Define objects.

include objects_prepsrc.mk

# Define targets.

EXE       = ../prep_chem_sources_$(CHEM)_$(AER).exe
RAMSUTILS = ramsutils.a
RAMSLIB   = ramslib.a
# NUWRF...Added WPS map projection library
WPSLIB    = libwpslib.a  

all:  $(RAMSUTILS) $(RAMSLIB) $(WPSLIB) $(WRFFIMUTILS) $(EXE)
	@echo ""
	@echo Finished building === $(EXE)
	@echo ""

$(RAMSUTILS): $(OBJ_RAMS_UTILS)
	$(ARCHIVE) $(RAMSUTILS) $(OBJ_RAMS_UTILS)

$(RAMSLIB): $(OBJ_RAMS)
	$(ARCHIVE) $(RAMSLIB) $(OBJ_RAMS)

# NUWRF...Added WPS map projection library
$(WPSLIB): $(OBJ_WPS)
	$(ARCHIVE) $(WPSLIB) $(OBJ_WPS)

# NUWRF...Changed dependencies for EXE
$(EXE): $(OBJ_PREPSRC) $(RAMSLIB) $(RAMSUTILS) $(WPSLIB)
	$(LOADER) $(LOADER_OPTS) -o $(EXE) $(OBJ_WPS) $(OBJ_PREPSRC) $(RAMSLIB) $(RAMSUTILS) $(WPSLIB) $(LIBS)


clean:
	@echo ""
	rm -f *.o *.mod *.a *.oo *.ipo $(LIBUTILS) $(PREPSOURCE) $(RAMSLIB) $(WPSLIB) $(WRFFIMUTILS)  $(EXE) 
	@echo ""
      
# Define dependencies and rules

include depend_prepsrc.mk
