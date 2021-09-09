set -x
export n=02
while [ $n -le 36 ]
do
  cp jhrrr_post_f01.ecf jhrrr_post_f${n}.ecf
  perl -pi -e s/01/$n/g jhrrr_post_f${n}.ecf
  n=`expr $n + 1`
  n=`printf %02d $n`
done
