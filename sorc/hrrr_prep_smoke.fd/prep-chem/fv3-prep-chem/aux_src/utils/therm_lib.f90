!############################# Change Log ##################################
! 2.0.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000, 2003 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!###########################################################################

MODULE mod_therm_lib

  IMPLICIT NONE

  PRIVATE

  PUBLIC td,   &
         rs,   &
         esat

CONTAINS

  !     ******************************************************************

  REAL FUNCTION td(p,rs)

    REAL , INTENT(IN) :: p
    REAL , INTENT(IN) :: rs

    REAL :: rr,es,esln

    rr=rs+1e-8
    es=p*rr/(.622+rr)
    esln=LOG(es)
    td=(35.86*esln-4947.2325)/(esln-23.6837)
    
    RETURN
  END FUNCTION td

  !     ******************************************************************

  REAL FUNCTION rs(p,t)
   
    REAL , INTENT(IN) :: p
    REAL , INTENT(IN) :: t
    
    REAL :: es
    
    es=610.78*EXP(17.269*(t-273.15)/(t-35.86))
    rs=.622*es/(p-es)
    
    RETURN
  END FUNCTION rs

  !     ******************************************************************

  REAL FUNCTION  esat(t)

    REAL, INTENT(IN) :: t

    !     esat(millibars),t(kelvin)

    REAL, PARAMETER :: abz=273.15
    REAL :: tc

    tc=t-abz
    esat=6.1078*EXP((17.2693882*tc)/(tc+237.3))

    RETURN
  END FUNCTION esat

  !     ******************************************************************

END MODULE mod_therm_lib





























FUNCTION rslf(p,t)

!     This function calculates the liquid saturation vapor mixing ratio as
!     a function of pressure and Kelvin temperature

IMPLICIT NONE
REAL esl,rslf,x,t,p,c0,c1,c2,c3,c4,c5,c6,c7,c8
PARAMETER (c0= .6105851e+03,c1= .4440316e+02,c2= .1430341e+01)
PARAMETER (c3= .2641412e-01,c4= .2995057e-03,c5= .2031998e-05)
PARAMETER (c6= .6936113e-08,c7= .2564861e-11,c8=-.3704404e-13)

x=MAX(-80.,t-273.15)

esl=c0+x*(c1+x*(c2+x*(c3+x*(c4+x*(c5+x*(c6+x*(c7+x*c8)))))))
rslf=.622*esl/(p-esl)

RETURN
END

!     ******************************************************************

REAL FUNCTION rsif(p,t)

!     This function calculates the ice saturation vapor mixing ratio as a
!     function of pressure and Kelvin temperature

IMPLICIT NONE
REAL esi,x,t,p,c0,c1,c2,c3,c4,c5,c6,c7,c8
PARAMETER (c0= .6114327e+03,c1= .5027041e+02,c2= .1875982e+01)
PARAMETER (c3= .4158303e-01,c4= .5992408e-03,c5= .5743775e-05)
PARAMETER (c6= .3566847e-07,c7= .1306802e-09,c8= .2152144e-12)

x=MAX(-80.,t-273.15)
esi=c0+x*(c1+x*(c2+x*(c3+x*(c4+x*(c5+x*(c6+x*(c7+x*c8)))))))
rsif=.622*esi/(p-esi)

RETURN
END

!     ******************************************************************

REAL FUNCTION rslif(p,t)
IMPLICIT NONE
REAL :: p,t
REAL, EXTERNAL :: rslf,rsif

!     This function calculates the saturation vapor mixing ratio, over
!     liquid or ice depending on temperature, as a function of pressure 
!     and Kelvin temperature

IF (t >= 273.15) THEN
   rslif = rslf(p,t)
ELSE
   rslif = rsif(p,t)
ENDIF

RETURN
END

!     ******************************************************************

REAL FUNCTION eslf(t)

!     This function calculates the liquid saturation vapor pressure as a
!     function of Celcius temperature

IMPLICIT NONE
REAL :: x,t
REAL, PARAMETER ::c0= .6105851e+03,c1= .4440316e+02,c2= .1430341e+01
REAL, PARAMETER ::c3= .2641412e-01,c4= .2995057e-03,c5= .2031998e-05
REAL, PARAMETER ::c6= .6936113e-08,c7= .2564861e-11,c8=-.3704404e-13

x=MAX(-80.,t)
eslf=c0+x*(c1+x*(c2+x*(c3+x*(c4+x*(c5+x*(c6+x*(c7+x*c8)))))))

RETURN
END

!     ******************************************************************

REAL FUNCTION esif(t)

!     This function calculates the ice saturation vapor pressure as a
!     function of Celsius temperature

IMPLICIT NONE
REAL :: x,t
REAL, PARAMETER ::c0= .6114327e+03,c1= .5027041e+02,c2= .1875982e+01
REAL, PARAMETER ::c3= .4158303e-01,c4= .5992408e-03,c5= .5743775e-05
REAL, PARAMETER ::c6= .3566847e-07,c7= .1306802e-09,c8= .2152144e-12

x=MAX(-80.,t)
esif=c0+x*(c1+x*(c2+x*(c3+x*(c4+x*(c5+x*(c6+x*(c7+x*c8)))))))

RETURN
END

!     ******************************************************************

REAL FUNCTION eslpf(t)

!     This function calculates the partial derivative of liquid saturation vapor
!     pressure with respect to temperature as a function of Celsius temperature

IMPLICIT NONE
REAL :: x,t
REAL, PARAMETER ::d0= .4443216e+02,d1= .2861503e+01,d2= .7943347e-01
REAL, PARAMETER ::d3= .1209650e-02,d4= .1036937e-04,d5= .4058663e-07
REAL, PARAMETER ::d6=-.5805342e-10,d7=-.1159088e-11,d8=-.3189651e-14

x=MAX(-80.,t)
eslpf=d0+x*(d1+x*(d2+x*(d3+x*(d4+x*(d5+x*(d6+x*(d7+x*d8)))))))

RETURN
END

!     ******************************************************************

REAL FUNCTION esipf(t)

!     This function calculates the partial derivative of ice saturation vapor
!     pressure with respect to temperature as a function of Celsius temperature

IMPLICIT NONE
REAL  :: x,t
REAL, PARAMETER ::d0= .5036342e+02,d1= .3775758e+01,d2= .1269736e+00
REAL, PARAMETER ::d3= .2503052e-02,d4= .3163761e-04,d5= .2623881e-06
REAL, PARAMETER ::d6= .1392546e-08,d7= .4315126e-11,d8= .5961476e-14

x=MAX(-80.,t)
esipf=d0+x*(d1+x*(d2+x*(d3+x*(d4+x*(d5+x*(d6+x*(d7+x*d8)))))))

RETURN
END

!     ******************************************************************

!     This function calculates the partial derivative of liquid saturation vapor
!     mixing ratio with respect to temperature as a function of pressure and
!     Kelvin temperature

REAL FUNCTION rslfp(p,t)

IMPLICIT NONE
REAL :: eslpf,x,t,p
REAL, PARAMETER ::d0= .4443216e+02,d1= .2861503e+01,d2= .7943347e-01
REAL, PARAMETER ::d3= .1209650e-02,d4= .1036937e-04,d5= .4058663e-07
REAL, PARAMETER ::d6=-.5805342e-10,d7=-.1159088e-11,d8=-.3189651e-14

x=MAX(-80.,t-273.15)
eslpf=d0+x*(d1+x*(d2+x*(d3+x*(d4+x*(d5+x*(d6+x*(d7+x*d8)))))))
rslfp=.622*eslpf/(p-eslpf)

RETURN
END

!     ******************************************************************

!     This function calculates the partial derivative of ice saturation vapor
!     mixing ratio with respect to temperature as a function of pressure and
!     Kelvin temperature

REAL FUNCTION rsifp(p,t)

IMPLICIT NONE
REAL :: esipf,x,t,p
REAL, PARAMETER ::d0= .5036342e+02,d1= .3775758e+01,d2= .1269736e+00
REAL, PARAMETER ::d3= .2503052e-02,d4= .3163761e-04,d5= .2623881e-06
REAL, PARAMETER ::d6= .1392546e-08,d7= .4315126e-11,d8= .5961476e-14

x=MAX(-80.,t-273.15)
esipf=d0+x*(d1+x*(d2+x*(d3+x*(d4+x*(d5+x*(d6+x*(d7+x*d8)))))))
rsifp=.622*esipf/(p-esipf)

RETURN
END

!     ******************************************************************

SUBROUTINE mrsl(n1,p,t,rsl)

IMPLICIT NONE
INTEGER :: n,n1
REAL :: rsl(n1),rslf,p(n1),t(n1)

DO n=1,n1
   rsl(n)=rslf(p(n),t(n))
ENDDO

RETURN
END

!     ******************************************************************

SUBROUTINE mrsi(n1,p,t,rsi)

IMPLICIT NONE
INTEGER :: n,n1
REAL :: rsi(n1),rsif,p(n1),t(n1)

DO n=1,n1
   rsi(n)=rsif(p(n),t(n))
ENDDO

RETURN
END

!     ******************************************************************

SUBROUTINE thvtoth(nn,theta,rv,rtp)

IMPLICIT NONE
INTEGER :: nn,k
REAL :: theta(nn),rv(nn),rtp(nn)

DO k=1,nn
  theta(k)=theta(k)*(1.+rtp(k))/(1.+1.61*rv(k))
ENDDO

RETURN
END

!     ******************************************************************

REAL FUNCTION td(p,rs)

IMPLICIT NONE

REAL , INTENT(IN) :: p
REAL , INTENT(IN) :: rs

REAL :: rr,es,esln

rr=rs+1e-8
es=p*rr/(.622+rr)
esln=LOG(es)
td=(35.86*esln-4947.2325)/(esln-23.6837)

RETURN
END

!     ******************************************************************

REAL FUNCTION rs(p,t)

IMPLICIT NONE

REAL , INTENT(IN) :: p
REAL , INTENT(IN) :: t

REAL :: es

es=610.78*EXP(17.269*(t-273.15)/(t-35.86))
rs=.622*es/(p-es)

RETURN
END

!     ******************************************************************

SUBROUTINE thetae(p,t,rv,the)
IMPLICIT NONE
REAL :: p,t,rv,the

REAL, PARAMETER :: cp=1004.,g=9.8,r=287.,alvl=2.35e6,cpg=cp/g
REAL :: pit,tupo,ttd,dz,tupn,tmn
INTEGER :: itter
REAL, EXTERNAL :: td

pit=p
tupo=t
ttd=td(p,rv)
dz=cpg*(t-ttd)
IF(dz.LE.0.) GOTO 20
DO itter=1,50
   tupn=t-g/cp*dz
   tmn=(tupn+t)*.5*(1.+.61*rv)
   pit=p*EXP(-g*dz/(r*tmn))
   IF(ABS(tupn-tupo).LT.0.001) GOTO 20
   ttd=td(pit,rv)
   tupo=tupn
   dz=dz+cpg*(tupn-ttd)
ENDDO
STOP 10
20 CONTINUE
the=tupo*(1e5/pit)**.286*EXP(alvl*rv/(cp*tupo))

RETURN
END

!     ******************************************************************

REAL FUNCTION  tw( rvp,thet,p)
IMPLICIT NONE
REAL :: rvp,thet,p
REAL :: press,rvap,piter,temper,x,aos
REAL, EXTERNAL :: os,tsa,tmr
INTEGER :: id

!     abs is absolute value
!     all arguments and tw (kelvin)

press=p*1.e-2
rvap=rvp*1.e3
piter =  press
DO id=1,10
   temper=thet*(piter*1.e-3)**.286
   x  =  .02*( tmr(rvap,piter) - temper)
   IF( ABS(x).LT. 0.01  ) GOTO 5
   piter = piter* ( 2.**(x)  )
ENDDO
5    temper=thet*(piter*1.e-3)**.286

aos  =   os(temper,piter)
tw   =  tsa( aos,press)

RETURN
END

!     ******************************************************************

REAL FUNCTION   os(t,p)
IMPLICIT NONE
REAL :: t,p
REAL,EXTERNAL :: w

!     os and t (kelvin) , p (millibars )

os=t*((1000./p)**.286)/(EXP(-2.6518986*w(t,p)/t))

RETURN
END

!     ******************************************************************

REAL FUNCTION tsa(os,p)
IMPLICIT NONE
REAL :: os,p
REAL :: a,tq,d,x
REAL,EXTERNAL :: w
INTEGER :: id

!     tsa and os(kelvin),p(millibars)
!     sign(a,b) rreplaces the algebretic sign of a with the sign of b

a  =  os
tq =  253.16
d  =  120
DO id= 1,12
   d = d/2.
!     if the temperature difference,x, is small,exit this loop
   x=a*EXP(-2.6518986*w(tq,p)/tq)-tq*((1000./p)**.286)
   IF(ABS(x).LT.0.01) GOTO 2
   tq = tq + SIGN(d,x)
ENDDO
2    tsa=tq

RETURN
END

!     ******************************************************************

REAL FUNCTION  esat(t)
IMPLICIT NONE
REAL, INTENT(IN) :: t

!     esat(millibars),t(kelvin)

REAL, PARAMETER :: abz=273.15
REAL :: tc

tc=t-abz
esat=6.1078*EXP((17.2693882*tc)/(tc+237.3))

RETURN
END

!     ******************************************************************

REAL FUNCTION  w(t,p)
IMPLICIT NONE
REAL :: t,p,x
REAL,EXTERNAL :: esat

!     w(grams water vapor/kilogram dry air ), p(millibar )

IF(t.GE.999.) GOTO 10
x  =  esat(t)
w  =  622.*x/(p-x)
RETURN
10 w=0.0

RETURN
END

!     ******************************************************************

REAL FUNCTION  tmr(w,p)
IMPLICIT NONE
REAL :: w,p,x

!     tmr(kelvin),w(grams water vapor/kilogram dry air),p(millibar)
!     log10  15   log to the base  ten.

x =  LOG10(   w*p/(622.+ w)  )
tmr=10.**(.0498646455*x+2.4082965)-7.07475+38.9114*((10.**(  &
  .0915*x ) - 1.2035 )**2 )

RETURN
END

!     ******************************************************************

SUBROUTINE the2t(the,p,th,t,r)
IMPLICIT NONE
REAL :: the,p,th,t,r

REAL, PARAMETER :: cp=1004.,alvl=2.350e6
REAL :: pi,to,tn
INTEGER :: itter
REAL, EXTERNAL :: rs

pi=(p*1e-5)**.286
to=the/EXP(alvl*.012/(cp*295.))*pi
DO itter=1,50
   r=rs(p,to)
   th=the/EXP(alvl*r/(cp*to))
   tn=th*pi
   IF(ABS(to-tn).LT.0.005) GOTO 12
   to=to+(tn-to)*.3
ENDDO
WRITE(6,1) the,p,to,tn,th,r
1 FORMAT(' stop in routine the2t '/' the,p,to,tn,th,r',6e15.6)
STOP 10
12 CONTINUE
t=tn

RETURN
END

!     ******************************************************************

SUBROUTINE qtk(q,tempk,fracliq)
IMPLICIT NONE
REAL, INTENT(IN)  :: q
REAL, INTENT(OUT) :: tempk
REAL, INTENT(OUT) :: fracliq
REAL,PARAMETER :: r4186=1./4186.,r2093=1./2093.,r334000=1./334000.

!     Input:
!        q        internal energy [J/kg]
!     Outputs:
!       tempk    temperature [K]
!       fracliq  liquid fraction [dimensionless]
!     Local Constants:
!       4186     specific heat of liquid [J/(kg K)]
!       2093     specific heat of ice [J/(kg K)]
!       334000   latent heat of fusion [J/kg]
!       273.15   conversion from temp [C] to temp [K]

IF (q .LE. 0.) THEN
   fracliq = 0.
   tempk = q * r2093 + 273.15
ELSEIF (q .GE. 334000.) THEN
   fracliq = 1.
   tempk = q * r4186 + 193.36
ELSE
   fracliq = q * r334000
   tempk = 273.15
ENDIF

RETURN
END

!     ******************************************************************

SUBROUTINE qtc(q,tempc,fracliq)
IMPLICIT NONE
REAL :: q,tempc,fracliq
REAL,PARAMETER :: r4186=1./4186.,r2093=1./2093.,r334000=1./334000.

!     Input:
!        q        internal energy [J/kg]
!     Outputs:
!        tempc    temperature [C]
!        fracliq  liquid fraction [dimensionless]
!     Local Constants:
!        4186     specific heat of liquid [J/(kg K)]
!        2093     specific heat of ice [J/(kg K)]
!        334000   latent heat of fusion [J/kg]
!        273.15   conversion from temp [C] to temp [K]

IF (q .LE. 0.) THEN
   fracliq = 0.
   tempc = q * r2093
ELSEIF (q .GE. 334000.) THEN
   fracliq = 1.
   tempc = q * r4186 - 80.
ELSE
   fracliq = q * r334000
   tempc = 0.
ENDIF

RETURN
END

!     ******************************************************************

SUBROUTINE qwtk(qw,w,dryhcap,tempk,fracliq)
  IMPLICIT NONE
  REAL :: qw,w,dryhcap,tempk,fracliq
  REAL, PARAMETER :: r4186=1./4186.,r2093=1./2093.,r334000=1./334000.
  REAL :: qwliq0
  !     Inputs:
  !        qw       internal energy [J/m^2] or [J/m^3]
  !        w        mass [kg/m^2] or [kg/m^3]
  !        dryhcap  heat capacity of nonwater part [J/(m^2 K)] or [J/(m^3 K)]
  !     Outputs:
  !        tempk    temperature [K]
  !        fracliq  liquid fraction [dimensionless]
  !     Local Constants:
  !        4186     specific heat of liquid [J/(kg K)]
  !        2093     specific heat of ice [J/(kg K)]
  !        334000   latent heat of fusion [J/kg]
  !        273.15   conversion from temp [C] to temp [K]

  qwliq0 = w * 334000.
  IF (qw .LE. 0.) THEN
     fracliq = 0.
     tempk = qw / (2093. * w + dryhcap) + 273.15
  ELSEIF (qw .GE. qwliq0) THEN
     fracliq = 1.
     tempk = (qw - qwliq0) / (4186. * w + dryhcap) + 273.15
  ELSE
     fracliq = qw / qwliq0
     tempk = 273.15
  ENDIF
  RETURN
END SUBROUTINE qwtk
