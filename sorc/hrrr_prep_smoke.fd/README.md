GSDCHEM and WRF-Smoke Preparation Tools
=======================================

This repository contains tools to set up initial states of GSDCHEM or
WRF-Smoke, and some workflow scripts for the EMC global models to run
it.  This document explains how to build the tools and how to run the
scripts.  It does not describe the inner workings of the programs, nor
the source code.

HRRR and RAP WRF-Smoke Tools
----------------------------

### fires_ncfmake

Pastes prep-chem-sources output into a wrfinput_d01 file, adding any
missing variables.  To compile on Jet:

    cd prep-chem/fires_ncfmake
    ./mk-wrf-jet

To compile on WCOSS Cray

    cd prep-chem/fires_ncfmake
    ./mk-wrf-wcoss-cray

The executable will be:

    prep-chem/fires_ncfmake/fires_ncfmake.x

### prep-chem-sources

Generates WRF-Smoke information in binary format, for input to
fires_ncfmake.  To compile on Jet:

    cd prep-chem/Prep_smoke_FRP/bin/build
    ./mk-wrf

To compile on WCOSS Cray:

    cd prep-chem/Prep_smoke_FRP/bin/build
    ./mk-wrf-wcoss

The executable will be:

    prep-chem/Prep_smoke_FRP/bin/prep_chem_sources_RADM_WRF_FIM_.exe

### Observation Processing Tools

There are two sets of these: one for RAP and one for HRRR.

    module load intel # or whatever you need to get the ifort command
    cd process-obs/HRRR-Smoke/src
    make
    cd ../../RAP-Smoke/src/
    make

Executables will be found in the src directories.


FV3-GSDCHEM Global Model Tools
------------------------------

### Variables

Apart from the usual NCEP environment variables, these additional ones are needed:

* `$FIXchem` -- location of the chemistry fix files.  See below.
* `$PARMchem` -- location of the chemistry parm files.  See below.
* `$EXchem` -- directory that contains `exglobal_prep_chem.bash`
* `$BBEM_WFABBA_DIR_YESTERDAY` -- directory in dcom with yesterday's wfabba files
* `$BBEM_WFABBA_DIR_TODAY` -- directory in dcom with today's wfabba files
* `$BBEM_MODIS_DIR_YESTERDAY` -- directory in dcom with yesterday's modis fire data files
* `$BBEM_MODIS_DIR_TODAY` -- directory in dcom with today's modis fire data files
* `$GBBEPX_DATA_DIR` -- directory in dcom with today's gbbepx files
* `$COMOUTchem` -- output directory for chemistry files
* `$CHEM_OUTPUT_FORMAT` -- filename pattern for output files within `$COMOUTchem`.  See JGLOBAL_PREP_CHEM for details on this variable.
* 

### File Locations

* `$PARMchem` -- small parameter files, which can be found in
  `workflow/emc-global/parm`
* `$FIXchem` can be found in two locations
  * WCOSS Surge, Gyre, Venus: `/gpfs/dell2/emc/obsproc/noscrub/Samuel.Trahan/prep_chem/FIXchem/`
  * RDHPCS Theia: `/scratch4/BMC/wrfruc/Samuel.Trahan/prep-chem/FIXchem`

### `prep_chem_sources_RADM_FV3_SIMPLE.exe`

This program generates initial conditions for GSDCHEM.

    cd prep-chem/fv3-prep-chem
    ./compile.sh

The executable will be:

    workflow/emc-global/exec/prep_chem_sources_RADM_FV3_SIMPLE.exe

### `workflow/emc-global/jobs/JGLOBAL_PREP_CHEM`

This is a wrapper around `exglobal_prep_chem.bash` and is designed to
be called directly by ecFlow.  It sets up the environment to match
what the lower-level script expects.  Certain lines will need to be
modified:

    #export BBEM_MODIS_DIR_TODAY=${BBEM_MODIS_DIR_TODAY:-$DCOMROOT/us003007/$PDY/path/to/modisfire}
    #export BBEM_MODIS_DIR_YESTERDAY=${BBEM_MODIS_DIR_YESTERDAY:-$DCOMROOT/us003007/$PDYm1/path/to/modisfire}
    #export BBEM_WFABBA_DIR_TODAY=${BBEM_WFABBA_DIR_TODAY:-$DCOMROOT/us003007/$PDY/path/to/wf_abba}
    #export BBEM_WFABBA_DIR_YESTERDAY=${BBEM_WFABBA_DIR_YESTERDAY:-$DCOMROOT/us003007/$PDYm1/path/to/wf_abba}
    #export GBBEPX_DATA_DIR=${GBBEPX_DATA_DIR:-$DCOMROOT/us00307/$PDYm1/path/to/gbbepx/data}

To disable a data source, set its location to an invalid path like `/dev/null`

If you rename the executable, this line must be changed:

    PREP_CHEM_SOURCES_EXE="$EXECchem/prep_chem_sources_RADM_FV3_SIMPLE.exe"

On WCOSS Cray, an aprun command is prepended.  If you want to change
how the program is executed on WCOSS Cray, this block must be updated:

    if [[ -d /gpfs && -s /etc/SuSE-release ]] && ( which aprun ) ; then
       # On CRAY in a batch job, the prep_chem_sources must be run via aprun:
       PREP_CHEM_SOURCES_EXE="aprun -n 1 -j 1 $PREP_CHEM_SOURCES_EXE"
    fi

### `workflow/emc-global/scripts/exglobal_prep_chem.bash`

Executes the prep-chem program.  Detailed information can be found at
the top of that script.  This is meant to be an internal
implementation of the `JGLOBAL_PREP_CHEM` rather than a script of its
own.  However, if the instructions at the top of the script are
followed, then it can be run independently, such as for regression
testing.

### `workflow/emc-global/ush/global_link_chem.bash`

Links the prep-chem output to a local directory called `EMISDIR`.  Called like so:

    $USHchem/global_link_chem.bash /com/gens/para/gefs.20190601/chem/gefs.t00z.chem_

where `/com/gens/para/gefs.20190601/chem/gefs.t00z.chem_` is the
prefix to all chemistry files.

The contents of this script must match the filenames in
JGLOBAL_PREP_CHEM in the `$CHEM_OUTPUT_FORMAT` variable.

### `workflow/emc-global/test/test_JGLOBAL_PREP_CHEM.bash`

A test script for `JGLOBAL_PREP_CHEM` and `global_link_chem.bash`.  Executed like so:

    cd workflow/emc-global/test
    scrub=/gpfs/gp2/ptmp/Samuel.Trahan/multiprep/wcoss-dell-p3
    fix=/gpfs/dell2/emc/obsproc/noscrub/Samuel.Trahan/prep_chem/FIXchem
    ./test_JGLOBAL_PREP_CHEM.bash 2019060100 "$scrub" "$fix"

When the script finishes, you should see lines like these describing
the critical directories:

    Success!
    EMISDIR: /scratch4/BMC/wrfruc/Samuel.Trahan/scrub/testchem/test.070d3bff/nwtmp/test-link-chem/EMISDIR
    Chemistry COM: /scratch4/BMC/wrfruc/Samuel.Trahan/scrub/testchem/test.070d3bff/com
    Scrub files: /scratch4/BMC/wrfruc/Samuel.Trahan/scrub/testchem/test.070d3bff/nwtmp

The final results are in the `Chemistry COM` directory, which contains
output to COM from prep-chem, and the `EMISDIR`, which contains the
input to the forecast job.
