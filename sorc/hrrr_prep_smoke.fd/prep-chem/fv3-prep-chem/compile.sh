#! /bin/sh

cd bin/build

if [[ -d /scratch3 && -d /scratch4 ]] ; then
    # assume Theia
    make clean
    ./mk-fv3-theia
elif [[ -d /ptmpp1 ]] ; then
    # WCOSS Phase 1 or 2
    make clean
    ./mk-fv3-wcoss12
elif [[ -s /etc/SuSE-release && -d /gpfs/hps ]] ; then
    # WCOSS Cray
    make clean
    ./mk-fv3-wcoss-cray
elif [[ -d /gpfs/dell2 ]] && ( readlink /usrx 2> /dev/null | grep dell > /dev/null 2>&1 ) ; then
    # WCOSS Phase 3
    make clean
    ./mk-fv3-wcoss-dell-p3
else
    echo WARNING: Unknown system.  Resorting to fallback build script.
    ./mk-fv3
fi