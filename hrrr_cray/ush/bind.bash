#! /bin/bash

# parpia@us.ibm.com 2014/04/30

# This parameter can change with each
# processor generation and node type;
# it should be adjusted appropriately

CORES_PER_NODE=16

# Ensure that the job is running under
# IBM Parallel Environment

if [ -z "${MP_CHILD}" ] || [ -z "${MP_PROCS}" ]; then
  echo "$0 must be run under IBM Parallel Environment"
  exit 1
fi

# Check that the list of target cores is set

if [ -z "${TARGET_CORE_LIST}" ]; then
  echo "Environment variable TARGET_CORE_LIST is not set."
  exit 1
fi

# Usage check

if [ $# -eq 0 ]; then
  echo "Usage: $0 executable [options] [I/O redirections]"
  exit 1
fi

# Check that the number of cores matches the
# parallel process count; this is specific
# to IBM Parallel Environment

N_TARGET_CORES=`echo ${TARGET_CORE_LIST} | wc -w`
if [ "${MP_PROCS}" -ne "${N_TARGET_CORES}" ]; then
  echo "The number of cores specified in environment variable TARGET_CORE_LIST does not match the parallel process count."
  exit 1
fi

# Bind the current process to a specified core
# (two hyperthreads) using the taskset utility;
# the syntax of the command is unusual

KOUNT=0
for i in ${TARGET_CORE_LIST}; do
  if [ "${MP_CHILD}" -eq "${KOUNT}" ]; then
    HT0=${i}
    HT1=`expr "${i}" + ${CORES_PER_NODE}`
    /bin/taskset -c -p "${HT0}","${HT1}" $$
  fi
  KOUNT=`expr "${KOUNT}" + 1`
done

exec $*
