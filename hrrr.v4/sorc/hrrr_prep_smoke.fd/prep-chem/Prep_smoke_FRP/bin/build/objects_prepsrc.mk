################### RAMS LIB ############################

OBJ_RAMS = \
grid_dims.o \
io_params.o \
node_mod.o \
mem_grid.o \
rconstants.o \
rams_grid.o \
lllc_utils.o   \
adap_init_prepchem.o   \
gridset_prepchem.o \
rcio.o  

##########################################################
################### UTILS LIB ############################

OBJ_RAMS_UTILS =   \
      an_header.o  \
      charutils.o  \
      dateutils.o  \
      dted.o       \
      eenviron.o   \
      error_mess.o \
      filelist.o   \
      getvar.o     \
      htint-opt.o  \
      interp_lib.o \
      map_proj.o   \
      numutils.o   \
      polarst.o    \
      rsys.o       \
      therm_lib.o  \
      tmpname.o    \
      utils_c.o    \
      utils_f.o    \
      vformat.o   
      
##########################################################
################### PREP SOURCE ##########################

OBJ_PREPSRC=          \
\
prep_chem_sources.o \
\
aer1_list.o \
chem1_list.o \
\
prep_chem_sources_utils.o \
convert_retro_to_CB07.o \
convert_AeM_to_CB07.o \
convert_bioge_to_CB07.o \
convert_retro_to_RACM.o \
convert_AeM_to_RACM.o \
convert_bioge_to_RACM.o \
convert_retro_to_RACM_REAC.o \
convert_AeM_to_RACM_REAC.o \
convert_bioge_to_RACM_REAC.o \
convert_retro_to_RELACS.o \
convert_AeM_to_RELACS.o \
convert_bioge_to_RELACS.o \
convert_retro_to_RELACS_REAC.o \
convert_AeM_to_RELACS_REAC.o \
convert_bioge_to_RELACS_REAC.o \
convert_AeM_to_aer.o \
convert_retro_to_RADM_WRF_FIM.o \
convert_AeM_to_RADM_WRF_FIM.o \
convert_bioge_to_RADM_WRF_FIM.o \
convert_retro_to_RADM_WRF_FIM_REAC.o \
convert_AeM_to_RADM_WRF_FIM_REAC.o \
convert_bioge_to_RADM_WRF_FIM_REAC.o \
retro_emissions.o \
diurnal_cycle.o \
diurnal_cycle_fwb.o \
fwb_awb_emissions.o \
fire_properties.o \
3bem_emissions.o \
volc_emissions.o\
gfedv2_8days_emissions.o \
biogenic_emissions.o \
gocart_background.o \
afwa_erod.o\
streets_emissions.o \
seac4rs_emissions.o \
cetesb_update.o  \
util_geometry.o \
wb_emissions.o \
AeM_emission_factors.o \
3bem_plumerise.o \
emission_fields.o \
grid_dims_output.o \
wrf_fim_utils.o \
wps_util.o \
write_gocart_bg.o 

#########################################################
 
#########################################################
# NU-WRF WPS map projection code
#########################################################

OBJ_WPS = \
cio.o \
constants_module.o \
gridinfo_module.o \
list_module.o \
llxy_module.o \
misc_definitions_module.o \
module_debug.o \
module_map_utils.o \
parallel_module.o

