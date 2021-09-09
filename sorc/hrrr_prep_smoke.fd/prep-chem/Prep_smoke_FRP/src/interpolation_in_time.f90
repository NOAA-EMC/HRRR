!###############################################################################!
!  CCATT-BRAMS/MCGA-CPTEC/WRF emission model    CPTEC/INPE                      !
!  Version 1.0.0: 12/nov/2010                                                   !
!  Coded by Saulo Freitas and Karla Longo                                       !
!  Contact: gmai@cptec.inpe.br - http://meioambiente.cptec.inpe.br              !
!###############################################################################!

integer :: imon,iday,iyear,julday1970
imon=10
iday=20
iyear=2005
jday=julday1970(imon,idate,iyear)

!case 1
!------------------Xi--------y--------------Xj-----------------------X -------------------X-----------
!iday > 15
!a1= (jday(y)-jday(Xi))/(jday(Xj)-jday(Xi))
!a2=1-a1
!v(y)=v(Xi)*a2+v(Xj)*a1

if(iday >=15) then
  a1=(julday1970(imon,iday,iyear)-julday1970(imon,15,iyear))/julday1970(imon,15,year



!------------------Xi-1 --------------y-----------Xi-----------------------X -------------------X-----------
!iday< 15j

end
function julday1970 (imonth,iday,iyear)

! compute the julian day (from 1970) from a normal date w/4 digit yr

julday1970=0
do i=1970,iyear

   imm=12
   idd=31
   if(i==iyear)then
      imm=imonth
      idd=iday
   endif

   jd= idd  &
      + min(1,max(0,imm-1))*31  &
      + min(1,max(0,imm-2))*(28+(1-min(1,mod(i,4))))  &
      + min(1,max(0,imm-3))*31  &
      + min(1,max(0,imm-4))*30  &
      + min(1,max(0,imm-5))*31  &
      + min(1,max(0,imm-6))*30  &
      + min(1,max(0,imm-7))*31  &
      + min(1,max(0,imm-8))*31  &
      + min(1,max(0,imm-9))*30  &
      + min(1,max(0,imm-10))*31  &
      + min(1,max(0,imm-11))*30  &
      + min(1,max(0,imm-12))*31

   julday1970=julday1970+jd

enddo    
