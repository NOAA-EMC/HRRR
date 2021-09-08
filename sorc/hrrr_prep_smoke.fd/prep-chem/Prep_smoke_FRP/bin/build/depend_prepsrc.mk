################### WPS LIB ##############################

#constants_module.o : $(LIB_WPS_PATH)/constants_module.F
#	 cp -f $< constants_module.F
#	 $(F_COMMAND) constants_module.F
#	 rm -f constants_module.F
#
constants_module.o  : $(LIB_WPS_PATH)/constants_module.f90 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)

misc_definitions_module.o  : $(LIB_WPS_PATH)/misc_definitions_module.f90 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)

module_debug.o  : $(LIB_WPS_PATH)/module_debug.f90 parallel_module.o cio.o 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)

module_map_utils.o  : $(LIB_WPS_PATH)/module_map_utils.f90  module_debug.o \
                 misc_definitions_module.o constants_module.o
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)

cio.o : $(LIB_WPS_PATH)/cio.c 
	cp -f $< cio.c
	$(C_COMMAND) cio.c
	rm -f cio.c

llxy_module.o  : $(LIB_WPS_PATH)/llxy_module.f90  gridinfo_module.o \
                 list_module.o module_map_utils.o module_debug.o \
                 misc_definitions_module.o
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)

gridinfo_module.o : $(LIB_WPS_PATH)/gridinfo_module.f90 \
                    constants_module.o misc_definitions_module.o \
                    module_debug.o
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)

list_module.o : $(LIB_WPS_PATH)/list_module.f90 \
                module_debug.o
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)

parallel_module.o : $(LIB_WPS_PATH)/parallel_module.f90 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)

################### BRAMS LIB ############################

an_header.o  : $(LIB_RAMS_PATH)/an_header.f90 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)

charutils.o  : $(LIB_RAMS_PATH)/charutils.f90 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)

dateutils.o  : $(LIB_RAMS_PATH)/dateutils.f90 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
error_mess.o : $(LIB_RAMS_PATH)/error_mess.f90 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
getvar.o     : $(LIB_RAMS_PATH)/getvar.f90  an_header.o
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
interp_lib.o : $(LIB_RAMS_PATH)/interp_lib.f90 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
htint-opt.o  : $(LIB_RAMS_PATH)/htint-opt.f90 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
map_proj.o   : $(LIB_RAMS_PATH)/map_proj.f90 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
numutils.o   : $(LIB_RAMS_PATH)/numutils.f90 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
polarst.o    : $(LIB_RAMS_PATH)/polarst.f90
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
therm_lib.o  : $(LIB_RAMS_PATH)/therm_lib.f90 
	 cp -f $< $(<F:.f90=.f90)
#	 $(F_COMMAND) -pi $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
utils_f.o    : $(LIB_RAMS_PATH)/utils_f.f90 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
vformat.o    : $(LIB_RAMS_PATH)/vformat.f90 
	 cp -f $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
filelist.o   : $(LIB_RAMS_PATH)/filelist.F90 
	  cp -f $< $(<F:.F90=.F90)
	  $(F_COMMAND) -D$(CMACH) $(<F:.F90=.F90)
	  rm -f $(<F:.F90=.F90)
	  
rsys.o       : $(LIB_RAMS_PATH)/rsys.F90 
	  cp -f $< $(<F:.F90=.F90)
	  $(F_COMMAND) -D$(CMACH) $(<F:.F90=.F90)
	  rm -f $(<F:.F90=.F90)

parlibf.o : $(LIB_RAMS_PATH)/parlibf.F90
	cp -f $< $(<F:.F90=.F90)
	$(F_COMMAND) $(<F:.F90=.F90)
	rm -f $(<F:.F90=.F90)

dted.o       : $(LIB_RAMS_PATH)/dted.c 
	  $(C_COMMAND) $<
	  
parlib.o     : $(LIB_RAMS_PATH)/parlib.c 
	  $(C_COMMAND) $<
	  
tmpname.o    : $(LIB_RAMS_PATH)/tmpname.c 
	  $(C_COMMAND) $<
	  
utils_c.o    : $(LIB_RAMS_PATH)/utils_c.c 
	  $(C_COMMAND) $<
	  
eenviron.o   : $(LIB_RAMS_PATH)/eenviron.c
	  $(C_COMMAND) $<
	  
spAllocate.o : $(LIB_RAMS_PATH)/spAllocate.c
	  $(C_COMMAND) $<

spBuild.o : $(LIB_RAMS_PATH)/spBuild.c
	  $(C_COMMAND) $<

spFactor.o : $(LIB_RAMS_PATH)/spFactor.c
	  $(C_COMMAND) $<

spOutput.o : $(LIB_RAMS_PATH)/spOutput.c
	  $(C_COMMAND) $<

spSolve.o : $(LIB_RAMS_PATH)/spSolve.c
	  $(C_COMMAND) $<

spUtils.o : $(LIB_RAMS_PATH)/spUtils.c
	  $(C_COMMAND) $<

spFortran.o : $(LIB_RAMS_PATH)/spFortran.c
	  $(C_COMMAND) $<

##########################################################
################### RAMS LIB #############################


grid_dims.o : $(RAMS_PATH)/grid_dims.f90
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
io_params.o : $(RAMS_PATH)/io_params.f90
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
node_mod.o : $(RAMS_PATH)/node_mod.f90           
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
rconstants.o : $(RAMS_PATH)/rconstants.f90       
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
rams_grid.o : $(RAMS_PATH)/rams_grid.f90 rconstants.o
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
lllc_utils.o : $(RAMS_PATH)/lllc_utils.f90
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
adap_init_prepchem.o : $(RAMS_PATH)/adap_init_prepchem.f90
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
gridset_prepchem.o : $(RAMS_PATH)/gridset_prepchem.f90
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
mem_grid.o : $(RAMS_PATH)/mem_grid.f90 grid_dims.o
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
rcio.o : $(RAMS_PATH)/rcio.f90 mem_grid.o
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)

##########################################################
################### WRF ##########################
	
wrf_fim_utils.o : $(PREPSOURCE_SRC)/wrf_fim_utils.f90 chem1_list.o grid_dims_output.o gocart_background.o afwa_erod.o
	cp -f  $< $(<F:.f90=.F90)
	$(F_COMMAND) $(<F:.f90=.F90)
	rm -f $(<F:.f90=.F90)

wps_util.o : $(PREPSOURCE_SRC)/wps_util.f90 mem_grid.o module_map_utils.o \
        llxy_module.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

write_gocart_bg.o : $(PREPSOURCE_SRC)/write_gocart_bg.f90 gocart_background.o grid_dims_output.o prep_chem_sources_utils.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

##########################################################
################### PREPSOURCES ##########################
aer1_list.o : $(PREPSOURCE_SRC)/$(AER_DIR)/aer1_list.f90 
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90) 
	
chem1_list.o : $(PREPSOURCE_SRC)/$(CHEM)/chem1_list.f90 
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90) 
	rm -f $(<F:.f90=.f90) 
	
prep_chem_sources_utils.o : $(PREPSOURCE_SRC)/prep_chem_sources_utils.f90 volc_emissions.o chem1_list.o grid_dims.o mem_grid.o grid_dims_output.o 3bem_plumerise.o emission_fields.o aer1_list.o gfedv2_8days_emissions.o 3bem_emissions.o wrf_fim_utils.o an_header.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_retro_to_CB07.o : $(PREPSOURCE_SRC)/convert_retro_to_CB07.f90 retro_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)	

convert_AeM_to_CB07.o : $(PREPSOURCE_SRC)/convert_AeM_to_CB07.f90 fwb_awb_emissions.o grid_dims_output.o gfedv2_8days_emissions.o 3bem_emissions.o AeM_emission_factors.o emission_fields.o 
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_bioge_to_CB07.o : $(PREPSOURCE_SRC)/convert_bioge_to_CB07.f90 biogenic_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_retro_to_RACM.o : $(PREPSOURCE_SRC)/convert_retro_to_RACM.f90 retro_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)	

convert_AeM_to_RACM.o : $(PREPSOURCE_SRC)/convert_AeM_to_RACM.f90 fwb_awb_emissions.o grid_dims_output.o gfedv2_8days_emissions.o 3bem_emissions.o AeM_emission_factors.o emission_fields.o  
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_bioge_to_RACM.o : $(PREPSOURCE_SRC)/convert_bioge_to_RACM.f90 biogenic_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)		

convert_retro_to_RACM_REAC.o : $(PREPSOURCE_SRC)/convert_retro_to_RACM_REAC.f90 retro_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_AeM_to_RACM_REAC.o : $(PREPSOURCE_SRC)/convert_AeM_to_RACM_REAC.f90 fwb_awb_emissions.o grid_dims_output.o gfedv2_8days_emissions.o 3bem_emissions.o AeM_emission_factors.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_bioge_to_RACM_REAC.o : $(PREPSOURCE_SRC)/convert_bioge_to_RACM_REAC.f90 biogenic_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_retro_to_RELACS.o : $(PREPSOURCE_SRC)/convert_retro_to_RELACS.f90 retro_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_AeM_to_RELACS.o : $(PREPSOURCE_SRC)/convert_AeM_to_RELACS.f90 fwb_awb_emissions.o grid_dims_output.o gfedv2_8days_emissions.o 3bem_emissions.o AeM_emission_factors.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_bioge_to_RELACS.o : $(PREPSOURCE_SRC)/convert_bioge_to_RELACS.f90 biogenic_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_retro_to_RELACS_REAC.o : $(PREPSOURCE_SRC)/convert_retro_to_RELACS_REAC.f90 retro_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_AeM_to_RELACS_REAC.o : $(PREPSOURCE_SRC)/convert_AeM_to_RELACS_REAC.f90 fwb_awb_emissions.o grid_dims_output.o gfedv2_8days_emissions.o 3bem_emissions.o AeM_emission_factors.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_bioge_to_RELACS_REAC.o : $(PREPSOURCE_SRC)/convert_bioge_to_RELACS_REAC.f90 biogenic_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)
	
convert_AeM_to_aer.o : $(PREPSOURCE_SRC)/convert_AeM_to_aer.f90 emission_fields.o aer1_list.o \
	grid_dims_output.o 3bem_plumerise.o AeM_emission_factors.o fwb_awb_emissions.o gfedv2_8days_emissions.o 
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

afwa_erod.o : $(PREPSOURCE_SRC)/afwa_erod.f90 mem_grid.o grid_dims_output.o grid_dims.o
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
convert_retro_to_RADM_WRF_FIM.o : $(PREPSOURCE_SRC)/convert_retro_to_RADM_WRF_FIM.f90 retro_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)	

convert_AeM_to_RADM_WRF_FIM.o : $(PREPSOURCE_SRC)/convert_AeM_to_RADM_WRF_FIM.f90 fwb_awb_emissions.o grid_dims_output.o gfedv2_8days_emissions.o 3bem_emissions.o AeM_emission_factors.o emission_fields.o   
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_bioge_to_RADM_WRF_FIM.o : $(PREPSOURCE_SRC)/convert_bioge_to_RADM_WRF_FIM.f90 biogenic_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)		

convert_retro_to_RADM_WRF_FIM_REAC.o : $(PREPSOURCE_SRC)/convert_retro_to_RADM_WRF_FIM_REAC.f90 retro_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_AeM_to_RADM_WRF_FIM_REAC.o : $(PREPSOURCE_SRC)/convert_AeM_to_RADM_WRF_FIM_REAC.f90 fwb_awb_emissions.o grid_dims_output.o gfedv2_8days_emissions.o 3bem_emissions.o AeM_emission_factors.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

convert_bioge_to_RADM_WRF_FIM_REAC.o : $(PREPSOURCE_SRC)/convert_bioge_to_RADM_WRF_FIM_REAC.f90 biogenic_emissions.o emission_fields.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

#>>> DEFINED 

retro_emissions.o : $(PREPSOURCE_SRC)/retro_emissions.f90 cetesb_update.o util_geometry.o mem_grid.o grid_dims_output.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

diurnal_cycle.o : $(PREPSOURCE_SRC)/diurnal_cycle.f90 retro_emissions.o grid_dims_output.o util_geometry.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

diurnal_cycle_fwb.o : $(PREPSOURCE_SRC)/diurnal_cycle_fwb.f90 fwb_awb_emissions.o grid_dims_output.o util_geometry.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

fwb_awb_emissions.o : $(PREPSOURCE_SRC)/fwb_awb_emissions.f90 aer1_list.o AeM_emission_factors.o grid_dims_output.o grid_dims.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

wb_emissions.o : $(PREPSOURCE_SRC)/wb_emissions.f90 grid_dims_output.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

fire_properties.o : $(PREPSOURCE_SRC)/fire_properties.f90 
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)
	
3bem_emissions.o : $(PREPSOURCE_SRC)/3bem_emissions.f90 fire_properties.o AeM_emission_factors.o \
          grid_dims_output.o 3bem_plumerise.o mem_grid.o llxy_module.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

volc_emissions.o : $(PREPSOURCE_SRC)/volc_emissions.f90 grid_dims_output.o mem_grid.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

gfedv2_8days_emissions.o : $(PREPSOURCE_SRC)/gfedv2_8days_emissions.f90 grid_dims_output.o 3bem_plumerise.o AeM_emission_factors.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)
	
biogenic_emissions.o : $(PREPSOURCE_SRC)/biogenic_emissions.f90 grid_dims_output.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)
	
gocart_background.o : $(PREPSOURCE_SRC)/gocart_background.f90 mem_grid.o grid_dims_output.o grid_dims.o
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)
	 
streets_emissions.o : $(PREPSOURCE_SRC)/streets_emissions.f90 grid_dims_output.o cetesb_update.o mem_grid.o
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)

seac4rs_emissions.o : $(PREPSOURCE_SRC)/seac4rs_emissions.f90 grid_dims_output.o cetesb_update.o mem_grid.o
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)

cetesb_update.o : $(PREPSOURCE_SRC)/cetesb_update.f90
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

util_geometry.o : $(PREPSOURCE_SRC)/util_geometry.f90
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

AeM_emission_factors.o : $(PREPSOURCE_SRC)/AeM_emission_factors.f90
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

3bem_plumerise.o : $(PREPSOURCE_SRC)/3bem_plumerise.f90 AeM_emission_factors.o
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)
	
emission_fields.o : $(PREPSOURCE_SRC)/emission_fields.f90 3bem_plumerise.o grid_dims_output.o 
	cp -f  $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

grid_dims_output.o : $(PREPSOURCE_SRC)/grid_dims_output.f90 grid_dims.o
	 cp -f  $< $(<F:.f90=.f90)
	 $(F_COMMAND) $(<F:.f90=.f90)
	 rm -f $(<F:.f90=.f90)

prep_chem_sources.o : $(PREPSOURCE_SRC)/prep_chem_sources.f90  wrf_fim_utils.o \
grid_dims_output.o mem_grid.o an_header.o diurnal_cycle.o diurnal_cycle_fwb.o \
chem1_list.o aer1_list.o emission_fields.o \
streets_emissions.o seac4rs_emissions.o \
retro_emissions.o biogenic_emissions.o gfedv2_8days_emissions.o \
3bem_emissions.o 3bem_plumerise.o fwb_awb_emissions.o volc_emissions.o \
wb_emissions.o wps_util.o write_gocart_bg.o
	cp -f $< $(<F:.f90=.F90)
	$(F_COMMAND) $(<F:.f90=.F90)
	rm -f $(<F:.f90=.F90)
	
##########################################################
