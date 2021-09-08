int equals_ixx(int *inum,int number1,int number2) {
  if(number1==number2) {
    *inum=number1;
    return 1;
  }
  return 0;
}
