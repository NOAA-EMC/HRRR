set -x
export n=02
while [ $n -le 17 ]
do
  cp jhrrr_post_f0100.ecf jhrrr_post_f${n}00.ecf
  perl -pi -e s/01/$n/g jhrrr_post_f${n}00.ecf
  n=`expr $n + 1`
  n=`printf %02d $n`
done
