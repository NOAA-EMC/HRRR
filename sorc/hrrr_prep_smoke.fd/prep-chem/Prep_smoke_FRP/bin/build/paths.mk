#Makefile include paths.mk

#Prepsource related

LIB_RAMS_PATH =../../aux_src/utils
RAMS_PATH     =../../aux_src/brams
PREPSOURCE_SRC=../../src
LIB_WPS_PATH  =../../aux_src/wps

AER_DIR=$(AER)
ifeq ($(CHEM), RADM_WRF_FIM)
AER_DIR=SIMPLE
endif
ifeq ($(CHEM), RADM_FV3)
AER_DIR=SIMPLE
endif
