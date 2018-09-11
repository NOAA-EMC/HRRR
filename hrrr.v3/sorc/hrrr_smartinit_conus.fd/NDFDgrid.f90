      SUBROUTINE NDFDgrid(psfc,zsfc,t,z,q,u,v,p,t2,q2, &
                 d2,u10,v10,land_hrrr,tnew,dewnew,unew,vnew, &
                 qnew,pnew,topo_ndfd,validpt)
      use rdgrib
      use grddef
      USE GRIB_MOD
      USE pdstemplates

! only need to go up to 10 levels or so for this application 
      PARAMETER(IM=2145,JM=1377,LM=20)
      PARAMETER (CAPA=0.28589641,P1000=1000.E2)
      REAL, PARAMETER :: SPVAL=9.9E10
      PARAMETER (GAMD=0.01,GAMi=0.0,GAMsubj=-0.0400, &
                 CPD_P=1004.705243,ROVCP_P=0.285714286, &
                 RD_P=287.0586407,G0_P=9.80665,CPOVR_P=3.5, &
                 CONST=5.255303,pi=3.141592653589793)
      PARAMETER (A2=17.2693882,A3=273.16,A4=35.86,PQ0=379.90516)
      PARAMETER(MBUF=2000000,JF=1000000)
      CHARACTER CBUF(MBUF)
      CHARACTER CBUF2(MBUF)
      CHARACTER*11 ENVVAR
      CHARACTER*80 FNAME
      LOGICAL*1 VALIDPT(IM,JM)

      real exn(IM,JM) 
      real T(IM,JM,LM),Q(IM,JM,LM),U(IM,JM,LM),V(IM,JM,LM), &
              Z(IM,JM,LM),P(IM,JM,LM)
      real T2(IM,JM),Q2(IM,JM),PSFC(IM,JM),ZSFC(IM,JM),D2(IM,JM), &
               U10(IM,JM), V10(IM,JM)
      real TNEW(IM,JM),DEWNEW(IM,JM),UNEW(IM,JM),VNEW(IM,JM), &
           PNEW(IM,JM),SFCHTNEW(IM,JM),ROUGH_MOD(IM,JM), &
           QNEW(IM,JM)
      real VEG_NDFD(IM,JM),LAND_HRRR(IM,JM),TOPO_NDFD(IM,JM)
      real TTMP(IM,JM),DTMP(IM,JM),UTMP(IM,JM),VTMP(IM,JM)
      real exn0,exn1, wsp

      integer i,j, ierr,k,ib,jb, ivar, imax,jmax, ix,iy
      integer ibuf, ia,ja,iw,jw,id,n_rough_yes,n_rough_no
      integer m_rough_yes,m_rough_no,vegf,vegfi,ltopo,ltopoi
      INTEGER :: IRET, ISTAT, ISSREF, ITOT, KRET, IGDN, NUMVAL
      INTEGER :: JDISC,JPDTN,JGDTN,LPOS
      INTEGER,DIMENSION(:) :: KPDS(200),KGDS(200)
      INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
      real zs,qv,qq,t1,e,enl,dwpt,z5,t5,gam,gamd,gami,tsfc,td
      real const,tddep,td_orig,zdif_max,tup, qvdif2m5m,qv2m
      real qc,qvc,thetavc,uc,vc,ratio,speed,speedc,frac
      real tmean,dz,theta1,theta6

      TYPE(GRIBFIELD) :: GFLD
      TYPE (GINFO)  ::  GDIN

      print *, '***********************************'
      print *, 'Into NDFDgrid'
      print *, '***********************************'

!  read in 2.5 km topography data set
      LTOPO=46
      LTOPOI=47
      CALL RDHDRS_g2(LTOPO,LTOPOI,IGDN,GDIN,NUMVAL)
      IMAX=GDIN%IMAX;JMAX=GDIN%JMAX
      ITOT=IMAX*JMAX
      ALLOCATE (GRID(ITOT),MASK(ITOT),STAT=kret)

      JDISC = -1
      JIDS = -9999
      JPDTN = -1
      JPDT = -9999
      JGDTN = -1
      JGDT = -9999
      ISSREF = 0
      K = 1
      J = K - 1

      CALL SETVAR_g2(LTOPO,LTOPOI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT, &
                   JGDTN,JGDT,KF,K,KPDS,KGDS,MASK,GRID,TOPO_NDFD,GFLD, &
                   ISSREF,IRET,ISTAT)
       print*,'minval topo_ndfd',minval(topo_ndfd)
       print*,'maxval topo_ndfd',maxval(topo_ndfd)


!  read in 2.5 km veg type 
      LVEG=48
      LVEGI=49
      CALL RDHDRS_g2(LVEG,LVEGI,IGDN,GDIN,NUMVAL)
      IMAX=GDIN%IMAX;JMAX=GDIN%JMAX
      ITOT=IMAX*JMAX
      ALLOCATE (GRID(ITOT),MASK(ITOT),STAT=kret)

      JDISC = -1
      JIDS = -9999
      JPDTN = -1
      JPDT = -9999
      JGDTN = -1
      JGDT = -9999
      ISSREF = 0
      K = 1
      J = K - 1
        
      CALL SETVAR_g2(LVEG,LVEGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT, &
                   JGDTN,JGDT,KF,K,KPDS,KGDS,MASK,GRID,VEG_NDFD,GFLD, &
                   ISSREF,IRET,ISTAT)
       print*,'minval veg_ndfd',minval(veg_ndfd)
       print*,'maxval veg_ndfd',maxval(veg_ndfd)
      
       zdif_max = -1000.
       n_rough_yes=0
       n_rough_no =0

! ****************************************************************
! -- Now let's start reducing to NDFD topo elevation.
! ****************************************************************

! Check first to make sure the terrain elevation does not exceed the highest model level read in
! at each gridpoint.
      do 110 j=1,jm
      do 110 i=1,im
        If(.not. validpt(i,j)) GOTO 110
        if(topo_ndfd(i,j) > z(i,j,lm))then
          write(6,*)' '
          write(6,*)'ERROR - terrain elevation:',topo_ndfd(i,j), &
            ' at gridpoint',i,j, &
            'is greater than highest model level read in:',z(i,j,lm), &
            ' total model levels is:', lm

          STOP 'STOP - Input terrain has higher elevation than &
            maximum model level read in.  Input additional model levels'
        endif
110   continue

        do j=1,jm
        do i=1,im
          if (zsfc(i,j) .lt. 0.) then
          zsfc(i,j)=0.0
          endif
          tnew(i,j)=SPVAL
          qnew(i,j)=SPVAL
          dewnew(i,j)=SPVAL
          unew(i,j)=SPVAL
          vnew(i,j)=SPVAL
          pnew(i,j)=SPVAL
        enddo
        enddo

        do 120 j=1,jm
        do 120 i=1,im
          IF(.NOT. VALIDPT(I,J)) GOTO 120
!   do not do any terrain adjustment at water points
          IF(LAND_HRRR(I,J).EQ.0.)THEN 
            TNEW(I,J)=T2(I,J)
            DEWNEW(I,J)=D2(I,J)
            QNEW(I,J)=Q2(I,J)
            UNEW(I,J)=U10(I,J)
            VNEW(I,J)=V10(I,J)
            PNEW(I,J)=PSFC(I,J)
            GOTO 120
          ENDIF
      
          sfchtnew(i,j) = topo_ndfd(i,j)
          exn(i,j) = cpd_p*(psfc(i,j)/P1000)**rovcp_p

!      ---   z = surface elevation
          zs = zsfc(i,j)

          if (topo_ndfd(i,j)-zs .gt. zdif_max) then
            zdif_max = max(zdif_max,topo_ndfd(i,j)-zs)
            imax = i
            jmax = j
          end if

! --- q = specific humidity at 2m from RAP model sfc
!          qq = q2(i,j)
!          qv = qq/(1.-qq)
!          qv2m = qv
!          e=psfc(i,j)/100.*qv/(0.62197+qv)
! --- dew-point temperature at original sfc
!        ENL = ALOG(E)
!        DWPT = (243.5*ENL-440.8)/(19.48-ENL)
!        td_orig = dwpt+273.15
         td_orig=d2(i,j)

! --- dewpoint depression
          tddep = max(0.,t2(i,j) - td_orig )
          qv= q(i,j,1)
          QQ = QV/(1.+QV)
!          theta1=((P1000/P(I,J,1))**CAPA)*T(I,J,1)
!          T1 = theta1*EXN(i,j)/(CPD_P*(1.+0.6078*QQ))
!          T1 = theta1*EXN(i,j)/CPD_P 
           T1=T(I,J,1)

! --- 2m specific humidity
!          qnew(i,j) = q
          
! --- Base Td on 2m q

          qv = qq/(1.-qq)
!          qvdif2m5m = qv2m - psfc(i,j)

!      ---   get values at level 5 for lapse rate calculations

          QQ = Q(I,J,5)/(1.+Q(i,j,5))

          exn(i,j) = cpd_p*(p(i,j,5)/P1000)**rovcp_p
!          theta5=((P1000/P(I,J,5))**CAPA)*T(I,J,5)
!          T5 = theta5*EXN(i,j)/(CPD_P*(1.+0.6078*QQ))
!          T5 = theta5*EXN(i,j)/CPD_P

! --- Change the lapse rate calculation to use 5 model levels (~300 m)
! --- instead of 6 model levels (~460 m).

          T5=T(I,J,5)
          Z1=Z(I,J,1)
          Z5=Z(I,J,5)
          GAM = (T1-T5)/(Z5-Z1)

!============================================
          if (topo_ndfd(i,j).le.zs ) then
!============================================
          GAM = MIN(GAMD,MAX(GAM,GAMi))

!      --- temperature at NDFD topo
! -- again, use 2m T at RAP regular terrain from similarity
!      theory for derivation of 2m T at topomini elevation
          tsfc = t2(i,j) + (zs-topo_ndfd(i,j))*gam

!  Don't let reduced valley temps be
!     any lower than RAP 2m temp minus 10K.
          tsfc = max(t2(i,j)-10.,tsfc)
!  Can't let valley temps go below RAP dewpoint temps.
          tsfc = max (tsfc,td_orig)

! --- pressure at NDFD topo
          tmean = (tsfc+t2(i,j)) * 0.5
          dz = zs-topo_ndfd(i,j)
          pnew(i,j) = psfc(i,j) * exp(g0_p*dz/(rd_p*tmean))

! --- temperature
          tnew(i,j) = tsfc

! --- Use the 2mT if the terrain differences are <= 1
!         zdiff = abs(zs-topo_ndfd(i,j))
!         if(zdiff .le. 1.0)then
!           tnew(i,j)=t2(i,j)
!         endif

!       Set dewpoint depression to that at original sfc

! --- dew-pt at topomini
          dewnew(i,j) = tsfc - tddep

! --- surface winds
! -- use 10 m wind values derived from similarity theory
!   gsm  use u and v of level 1 or 10m???
          unew(i,j) = u10(i,j)
          vnew(i,j) = v10(i,j)


!============================================
          ELSE if (topo_ndfd(i,j).gt.zs) then
!============================================
! ----       Now only if topo_NDFD is above the RAP model elevation

!         Here, when topo-NDFD > topo-RAP, we allow a small
!        subisothermal lapse rate with slight warming with height.

!         GAM = MIN(GAMD,MAX(GAM,GAMsubj))
! Constrain local lapse rate to be between dry adiabatic and isothermal
          GAM = MIN(GAMD,MAX(GAM,GAMi))

          DO K=1,LM
           if (z(i,j,k) .gt. topo_ndfd(i,j)) go to 781
          ENDDO 
781       continue

          if (k .eq. 1) then
           frac = (topo_ndfd(i,j)-zs) / (z(i,j,k)-zs)
           exn1 = (psfc(i,j)/P1000)**rovcp_p
           exn0 = (p(i,j,k)/P1000)**rovcp_p
! --- pressure at NDFD topo
           pnew(i,j) = P1000* ((exn1 +frac * (exn0 - exn1)) **cpovr_p)
           thetak=((P1000/P(i,j,k))**CAPA)*T(i,j,k)
           thetak1=((P1000/PSFC(i,j))**CAPA)*t2(i,j)
           thetavc = thetak1+frac * (thetak-thetak1)
           qvc = Q2(i,j)+frac * (Q(i,j,k)-Q2(i,j))
           qc = qvc/(1.+qvc)

          else 
           frac = (topo_ndfd(i,j)-z(i,j,k-1)) / (z(i,j,k)-z(i,j,k-1))
           exn1 = (p(i,j,k-1)/P1000)**rovcp_p
           exn0 = (p(i,j,k)/P1000)**rovcp_p
! --- pressure at NDFD topo
           pnew(i,j) = P1000* ((exn1 +frac * (exn0 - exn1)) **cpovr_p)
           thetak=((P1000/P(i,j,k))**CAPA)*T(i,j,k)
           thetak1=((P1000/P(i,j,k-1))**CAPA)*T(i,j,k-1)
           thetavc = thetak1+frac * (thetak-thetak1)
           qvc = Q(i,j,k-1)+frac * (Q(i,j,k)-Q(i,j,k-1))
           qc = qvc/(1.+qvc)
          endif
! --- temperature
!         tup = thetavc*(pnew(i,j)/P1000)**rovcp_p / (1.+0.6078*qc)
!          tup=thetavc
!         alttup=t2(i,j)+frac*(t(i,j,k)-t2(i,j))

! original temperature computation
!         alttup=tup
            
!  provisional 2m temp at NDFD topo
!         tnew(i,j) = t2(i,j) + (alttup-t1)

!         zdiff = abs(topo_ndfd(i,j)-zs)
!         if(zdiff .le. 1.0)then
! --- Use the 2mT if the terrain differences are <= 1
!           tnew(i,j)=t2(i,j)
!         else
! --- Smoothly adjust the downscaled temperature
!           tnew(i,j) = t2(i,j) + ((alttup-t1)*(tanh(zdiff-2*pi)+1)/2)
!         endif

! --- Don't let extrapolated temp to be any larger than
!       the value at the RAP terrain level.
!     This will avoid the problem with NDFD temp values
!       being set to be much warmer than RAP 2m temp.

          tsfc=t2(i,j) + (zs-topo_ndfd(i,j))*gam

!         if (tnew(i,j) .gt. t2(i,j)) then
!          tnew(i,j) = min(tnew(i,j),tsfc)
!         endif

! lapse rate suggestion from Guoqing
          tnew(i,j)=tsfc
          if (tnew(i,j) .gt. t2(i,j)) then
           tnew(i,j) = t2(i,j)
          endif

           qv=q2(i,j)
          e=pnew(i,j)/100.*qv/(0.62197+qv)
! --- dew-point temperature at original sfc
        ENL = ALOG(E)
        DWPT = (243.5*ENL-440.8)/(19.48-ENL)
        td = dwpt + 273.15
! --- dewpoint temperature
        dewnew(i,j) = tnew(i,j) - tddep
!        dewnew(i,j) = min(td,tnew(i,j))
        if (k .eq. 1) then
!         u10(i,j) = u(i,j,1) - 2.
!         v10(i,j) = v(i,j,1) - 2.
         uc = u10(i,j)+frac * (u(i,j,k)-u10(i,j))
         vc = v10(i,j)+frac * (v(i,j,k)-v10(i,j))
        else
         uc = u(i,j,k-1)+frac * (u(i,j,k)-u(i,j,k-1))
         vc = v(i,j,k-1)+frac * (v(i,j,k)-v(i,j,k-1))
        endif

! -- 0.7 factor is a wag at surface effects on wind speed
!     when interpolating from the free atmosphere to
!     the NDFD topo.
          speedc = 0.7*sqrt(uc*uc+vc*vc)
          speed = sqrt(u(i,j,1)**2 + v(i,j,1)**2)
          ratio = max(1.,speedc/(max(0.001,speed)) )
          unew(i,j) = ratio*(u(i,j,1))
          vnew(i,j) = ratio*(v(i,j,1))

!============================================
        END IF
!============================================

 120     continue

!============================================
! -- use vegtype to get better temps/dewpoint/winds
!      near coastlines.
!    Use nearest neighbor adjustment where RAP 
!      land-water mask does not mask NDFD land-water mask 
!============================================

!  create temporary holder for u,v,t,td so that the "real"
!   values don't get shifted around in the adjustment
       do j=1,jm
       do i=1,im
         ttmp(i,j)=tnew(i,j)
         dtmp(i,j)=dewnew(i,j)
         utmp(i,j)=unew(i,j)
         vtmp(i,j)=vnew(i,j)
         rough_mod(i,j) = land_hrrr(i,j)
       end do
       end do

! ----------------------------------------------------
! -- Adjust to rough_mod iteratively for land to water
! ----------------------------------------------------

       do k=1,15
         nmod = 0
         write (6,*)' Iteration for land adj, k=',k
       do j=1,jm
        jm1 = max(1,j-1)
        jp1 = min(jm,j+1)
       do i=1,im
        im1 = max(1,i-1)
        ip1 = min(im,i+1)
        if (veg_ndfd(i,j).lt.0.05 .and. rough_mod(i,j).gt.0.05) then
           iadj = 0
         do j1=jm1,jp1
         do i1=im1,ip1
           if (rough_mod(i1,j1).lt.0.05) then
             iadj = 1
           end if
         end do
         end do
         if (iadj.eq.1) then
           rough_mod(i,j) = 0.
           nmod = nmod + 1
         end if
          end if
       end do
       end do
         write (6,*)' No. pts changed, land-to-water=',nmod
       end do
! ----------------------------------------------------
! -- Adjust to rough_mod iteratively for water to land
! ----------------------------------------------------
       do k=1,15
         nmod = 0
         write (6,*)' Iteration for wat adj, k=',k
       do j=1,jm
        jm1 = max(1,j-1)
        jp1 = min(jm,j+1)
       do i=1,im
        im1 = max(1,i-1)
        ip1 = min(im,i+1)
           if (veg_ndfd(i,j).gt.0.05 .and. rough_mod(i,j).lt.0.05) then
           iadj = 0
         do j1=jm1,jp1
         do i1=im1,ip1
            if (rough_mod(i1,j1).gt.0.05) then
             iadj = 1
            endif
         end do
         end do
         if (iadj.eq.1) then
           rough_mod(i,j) = 1.0 
           nmod = nmod + 1
!          write (6,*)'Changed water to land',i,j
         end if
          end if
       end do
       end do
         write (6,*)k,'No. pts changed, water-to-land=',nmod
       end do

       do j=1,jm
       do i=1,im
         if (land_hrrr(i,j).gt.0.05 .and. rough_mod(i,j).lt.0.05) then
! -----------------------------------------------------------------
! -- i.e.  NDFD grid-point is over WATER (per rough_mod)
!          HRRR-interp grid-point is over LAND 
! -----------------------------------------------------------------
              
          do ibuf=1,10
           ia = max(1,i-ibuf)
           ib = min(im,i+ibuf)
           ja = max(1,j-ibuf)
           jb = min(jm,j+ibuf)
               
            do jw = ja,jb
              id = 1
            do iw = ia,ib,id
              if (land_hrrr(iw,jw).lt.0.05) then 
               IF (VALIDPT(IW,JW)) THEN
                 unew(i,j) = utmp(iw,jw)
                 vnew(i,j) = vtmp(iw,jw)
                 tnew(i,j) = ttmp(iw,jw)
                 dewnew(i,j) = dtmp(iw,jw)
                 go to 883
               ENDIF
              end if
            end do
            end do
               
          end do
          n_rough_no = n_rough_no+1
          go to 884
883       continue
          n_rough_yes = n_rough_yes+1
884       continue
               
          end if
          if (land_hrrr(i,j).lt.0.05 .and. rough_mod(i,j).gt.0.05) then
! -----------------------------------------------------------------
! -- i.e.  NDFD grid-point is over LAND (per rough_mod)
!          RAP-interp grid-point is over WATER
! -----------------------------------------------------------------
               
          do ibuf=1,10
          ia = max(1,i-ibuf)
          ib = min(im,i+ibuf)
          ja = max(1,j-ibuf)
          jb = min(jm,j+ibuf)
               
            do jw = ja,jb
              id = 1
            do iw = ia,ib,id
              if (land_hrrr(iw,jw).gt.0.05) then
               IF (VALIDPT(I,J)) THEN
                unew(i,j) = utmp(iw,jw)
                vnew(i,j) = vtmp(iw,jw)
                tnew(i,j) = ttmp(iw,jw)
                dewnew(i,j) = dtmp(iw,jw)
                go to 783
               ENDIF
              end if
            end do
            end do
          end do
          m_rough_no = m_rough_no+1
          go to 784
783       continue
          m_rough_yes = m_rough_yes+1
784       continue
          end if
       end do
       end do

       do j=1,jm
       do i=1,im
!        if (VALIDPT(I,J) .and. (PQ0/PSFC(I,J)*EXP(A2*(dewnew(I,J)-A3)/
!     &      (dewnew(I,J)-A4))) .lt. 9.9E10) THEN
        if (VALIDPT(I,J)) THEN
         qnew(i,j)=PQ0/PNEW(I,J)*EXP(A2*(dewnew(I,J)-A3)/ &
                   (dewnew(I,J)-A4))
        else
          qnew(i,j)=SPVAL
          tnew(i,j)=SPVAL
          dewnew(i,j)=SPVAL
          unew(i,j)=SPVAL
          vnew(i,j)=SPVAL
          pnew(i,j)=SPVAL
        endif
       end do
       end do

       return
       END SUBROUTINE NDFDgrid
