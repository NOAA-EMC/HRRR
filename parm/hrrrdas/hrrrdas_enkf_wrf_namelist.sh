# define namelist for community EnKF release version 1.0 

export enkf_namelist="

 &nam_enkf
  datestring          = ${gdate},
  datapath            = './',
  analpertwtnh        = ${POSTERIOR_INFLATION_FACTOR},
  analpertwtsh        = ${POSTERIOR_INFLATION_FACTOR},
  analpertwttr        = ${POSTERIOR_INFLATION_FACTOR},
  lupd_satbiasc       = .false.,
  zhuberleft          = 1.e10,
  zhuberright         = 1.e10,
  huber               = .false.,
  varqc               = .false.,
  covinflatemax       = 1.e2,
  covinflatemin       = 1.0,

  covinflatenh=${COVINFLATENH},
  covinflatesh=${COVINFLATESH},
  covinflatetr=${COVINFLATETR},
  lnsigcovinfcutoff=6,

  pseudo_rh           = .true.,
  corrlengthnh        = ${CORRLENGTHNH},
  corrlengthsh        = ${CORRLENGTHSH},
  corrlengthtr        = ${CORRLENGTHTR},
  obtimelnh           = 1.e30,
  obtimelsh           = 1.e30,
  obtimeltr           = 1.e30,
  iassim_order        = 0,
  lnsigcutoffnh       = 0.5,
  lnsigcutoffsh       = 0.5,
  lnsigcutofftr       = 0.5,
  lnsigcutoffsatnh    = 0.5,
  lnsigcutoffsatsh    = 0.5,
  lnsigcutoffsattr    = 0.5,
  lnsigcutoffpsnh     = 0.5,
  lnsigcutoffpssh     = 0.5,
  lnsigcutoffpstr     = 0.5,
  simple_partition    = .true.,
  nlons               = $NLONS,
  nlats               = $NLATS,
  smoothparm          = -1,
  readin_localization = .false.,
  saterrfact          = 1.0,
  numiter             = 6,
  sprd_tol            = 1.e30,
  paoverpb_thresh     = 2.0,
  reducedgrid         = .false.,
  nlevs               = $NLEVS,
  nanals              = $NMEM_ENKF,
  nbackgrounds        = 1,
  deterministic       = .true.,
  sortinc             = .true.,
  univaroz            = .true.,
  regional            = .true., 
  adp_anglebc         = .true.,
  angord              = 4,
  use_edges           = .false.,
  emiss_bc            = .true.,
  biasvar             = -500 
/
&satobs_enkf
/
&ozobs_enkf
/
&nam_wrf
  arw                 = .true.,
  nmm                 = .false.,
 / 
"
