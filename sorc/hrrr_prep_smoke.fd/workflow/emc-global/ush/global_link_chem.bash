#! /bin/bash
# This is a bash script; not ksh or sh.  Do not change the above line.

set -xue

filename_prefix="$1" # ie.: /com/gens/para/gefs.20190601/chem/gefs.t00z.chem_

if [[ -d EMISDIR ]] ; then
    rm -rf EMISDIR
fi
mkdir EMISDIR

count=0
for file in $filename_prefix*.tile[1-6].dat ; do
    if [[ $file =~ chem_([a-zA-Z0-9_-]+).(tile[1-6]).dat ]] ; then
	if [[ ! -d "EMISDIR/${BASH_REMATCH[2]}" ]] ; then
	    mkdir "EMISDIR/${BASH_REMATCH[2]}"
	fi
	cp -fp "$file" "EMISDIR/${BASH_REMATCH[2]}/${BASH_REMATCH[1]}.dat"
	count=$(( count + 1 ))
    else
	echo Ignoring unrecognized filename $file
    fi
done

echo "Linked $count chemistry source files."
