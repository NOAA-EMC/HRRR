      subroutine NDFDgrid(tnew,dewnew,unew,vnew,qnew,pnew,gdin,im,jm,lm,domain,model)

      use constants
      use grddef
      use aset3d             ! Define 3-d grids
      use aset2d
      use rdgrib
      USE GRIB_MOD
      USE pdstemplates

      TYPE (GINFO) :: GDIN
      TYPE(GRIBFIELD)::GFLD

      real TNEW(im,jm),DEWNEW(im,jm),UNEW(im,jm),VNEW(im,jm)
      real QNEW(im,jm),PNEW(im,jm)

      real ROUGH_MOD(IM,JM)
      real VEG_NDFD(IM,JM)
      real TTMP(IM,JM),DTMP(IM,JM),UTMP(IM,JM),VTMP(IM,JM)
      real exn0,exn1

      integer i,j,k,ib,jb,imax,jmax
      integer ibuf,ia,ja,iw,jw,id,n_rough_yes,n_rough_no
      integer m_rough_yes,m_rough_no,ltopo,ltopoi
      INTEGER JIDS(200),JPDT(200),JGDT(200)
      INTEGER KPDS(200),KGDS(200)
      real zs,qv,t1,e,enl,dwpt,zupr,tupr,tsfc,td
      real tddep,td_orig,tup
      real qc,qvc,thetavc,uc,vc,ratio,speed,speedc,frac
      real tmean,dz

      character*2 domain
      character*4 model

      print *, '***********************************'
      print *, 'Into NDFDgrid for domain ',domain
      print *, '***********************************'

! read in NDFD land mask

      LVEG=48
      LVEGI=49
      KSKIP=0
      J=0
      write(0,*) 'call RDHDRS NDFD veg'
      CALL RDHDRS_g2(LVEG,LVEGI,IGDNUM,GDIN,NUMVAL)
      GDIN%KMAX=MAXLEV
      IMAX=GDIN%IMAX;JMAX=GDIN%JMAX;KMAX=GDIN%KMAX
      NUMLEV=GDIN%KMAX
      ITOT=IMAX*JMAX
      print *,'imax,jmax,kmax,numlev,igdnum,numval'
      print *,imax,jmax,kmax,numlev,igdnum,numval
      ALLOCATE (GRID(ITOT),MASK(ITOT),STAT=kret)

      JIDS=-9999
      JPDTN=-1
      JPDT=-9999
      JGDTN=-1
      JPDTN=0
      ISSREF=0

      JDISC=2
      JPDT(1)=000
      JPDT(2)=000
      JPDT(10)=-9999

      CALL SETVAR_g2(LVEG,LVEGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,&
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,VEG_NDFD,GFLD, &
                     ISSREF,IRET,ISTAT)


      DO I=1,IM
        DO J=1,JM
          IF (VEG_NDFD(I,J).LE. 0.) THEN
            VEG_NDFD(I,J) = 16.
          ELSE
            VEG_NDFD(I,J) = VEG_NDFD(I,J)
          ENDIF
        ENDDO
      ENDDO

! read in NDFD topography data set
      LTOPO=46
      LTOPOI=47
      KSKIP=0
      J=0
      write(0,*) 'call RDHDRS NDFD topo'
      CALL RDHDRS_g2(LTOPO,LTOPOI,IGDNUM,GDIN,NUMVAL)
      GDIN%KMAX=MAXLEV
      IMAX=GDIN%IMAX;JMAX=GDIN%JMAX;KMAX=GDIN%KMAX
      NUMLEV=GDIN%KMAX
      ITOT=IMAX*JMAX
      print *,'imax,jmax,kmax,numlev,igdnum,numval'
      print *,imax,jmax,kmax,numlev,igdnum,numval
      
      JIDS=-9999
      JPDTN=-1
      JPDT=-9999
      JGDTN=-1
      JPDTN=0
      JDISC=0
      ISSREF=0
      
      JPDT(1)=003
      JPDT(2)=006
      JPDT(10)=001

      CALL SETVAR_g2(LTOPO,LTOPOI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN,&
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,TOPO_NDFD,GFLD, &
                     ISSREF,IRET,ISTAT)

      n_rough_yes=0
      n_rough_no =0

! Check first to make sure the terrain elevation does not exceed the highest model level read in
! at each gridpoint. Only done for the HRRR, since only 20 levels are read in rather than the
! complete set of vertical levels.
      if (trim(model) == 'HRRR') then
        jloop: do j=1,jm
        iloop: do i=1,im
          If(.not. validpt(i,j)) cycle iloop 
          if(topo_ndfd(i,j) > hght(i,j,lm))then
            write(6,*)' '
            write(6,*)'ERROR - terrain elevation:', &
              topo_ndfd(i,j),' at gridpoint',i,j, &
              'is greater than highest model level read in:', &
              hght(i,j,lm), ' total model levels is:', lm

            STOP 'STOP - Input terrain has higher elevation than &
            maximum model level read in.  Input additional model levels'
          endif
        end do iloop
        end do jloop
      endif

! ****************************************************************
! -- Now let's start reducing to NDFD topo elevation.
! ****************************************************************
      print*,'before zsfc'
      do j=1,jm
      do i=1,im
        if (zsfc(i,j).lt. 0.) then
          zsfc(i,j)=0.0
        endif
      enddo
      enddo
      print*,'end zsfc'
      do j=1,jm
      do i=1,im
        tnew(i,j)=SPVAL
        qnew(i,j)=SPVAL
        dewnew(i,j)=SPVAL
        unew(i,j)=SPVAL
        vnew(i,j)=SPVAL
        pnew(i,j)=SPVAL
        gam(i,j)=spval
      enddo
      enddo
      print*,'end spval'

      print*,'before 120'
      jloop2: do j=1,jm
      iloop2: do i=1,im
        IF(.NOT. VALIDPT(I,J)) cycle iloop2
        if (trim(model) == 'HRRR' .and. coast(i,j) == 0.) then
          TNEW(I,J)=T2(I,J)
          DEWNEW(I,J)=D2(I,J)
          QNEW(I,J)=Q2(I,J)
          UNEW(I,J)=U10(I,J)
          VNEW(I,J)=V10(I,J)
          PNEW(I,J)=PSFC(I,J)
          T1=T(I,J,1)
          tupr=T(I,J,5)
          zupr=HGHT(I,J,5)
          Z1=HGHT(I,J,1)
! local lapse rate
          GAM(i,j) = (T1-tupr)/(zupr-Z1)
! local lapse rate constrained by dry adiabatic and isothermal
          GAM(i,j) = MIN(GAMD,MAX(GAM(i,j),GAMi))
          cycle iloop2
        ENDIF

! ---   z = surface elevation
        zs = zsfc(i,j)

! --- dew-point temperature at original sfc
        td_orig=d2(i,j)

! --- dewpoint depression
        tddep = max(0.,t2(i,j) - td_orig )
        T1=T(I,J,1)

! ---   get values at level 6 for lapse rate calculations

        if (trim(model) == 'HRRR') then
          tupr=T(I,J,5)
          zupr=HGHT(I,J,5)
        else
          tupr=T(I,J,6)
          zupr=HGHT(I,J,6)
        endif
        
        Z1=HGHT(I,J,1)
! local lapse rate
        GAM(i,j) = (T1-tupr)/(zupr-Z1)

!============================================
        if (topo_ndfd(i,j).le.zs ) then
!============================================
! local lapse rate constrained by dry adiabatic and isothermal
          GAM(i,j) = MIN(GAMD,MAX(GAM(i,j),GAMi))

! --- temperature at NDFD topo
! -- again, use 2m T at model regular terrain from similarity
!      theory for derivation of 2m T at topomini elevation
          tsfc = t2(i,j) + (zs-topo_ndfd(i,j))*gam(i,j)

! Don't let reduced valley temps be
! any lower than model 2m temp minus 10K.
          tsfc = max(t2(i,j)-10.,tsfc)
! Can't let valley temps go below model dewpoint temps.
          tsfc = max (tsfc,td_orig)

! --- pressure at NDFD topo
          tmean = (tsfc+t2(i,j)) * 0.5
          dz = zs-topo_ndfd(i,j)
          pnew(i,j) = psfc(i,j) * exp(g0_p*dz/(rd_p*tmean))

! --- temperature
          tnew(i,j) = tsfc

! Set dewpoint depression to that at original sfc

! --- dew-pt at topomini
          dewnew(i,j) = tsfc - tddep

! --- surface winds
! -- use 10 m wind values derived from similarity theory
! gsm  use u and v of level 1 or 10m???
          unew(i,j) = u10(i,j)
          vnew(i,j) = v10(i,j)


!============================================
        ELSE if (topo_ndfd(i,j).gt.zs) then
!============================================
! ----  Now only if topo_NDFD is above the model elevation

! Here, when topo-NDFD > topo-RAP, we allow a small
! subisothermal lapse rate with slight warming with height.

          GAM(i,j) = MIN(GAMD,MAX(GAM(i,j),GAMsubj))
! Constrain local lapse rate to be between dry adiabatic and isothermal
!         if (trim(model) == 'HRRR') then
            GAM(i,j) = MIN(GAMD,MAX(GAM(i,j),GAMi))
!         endif

          DO K=1,LM
           if (hght(i,j,k) .gt. topo_ndfd(i,j)) exit
          ENDDO 

          if (k .eq. 1) then
           frac = (topo_ndfd(i,j)-zs) /&
               (hght(i,j,k)-zs)
           exn1 = (psfc(i,j)/P1000)**rovcp_p
           exn0 = (pmid(i,j,k)/P1000)**rovcp_p
! --- pressure at NDFD topo
           pnew(i,j) = P1000* ((exn1 +frac *&
               (exn0 - exn1)) **cpovr_p)
           thetak=((P1000/PMID(i,j,k))**CAPA)*T(i,j,k)
           thetak1=((P1000/PSFC(i,j))**CAPA)*t2(i,j)
           thetavc = thetak1+frac * (thetak-thetak1)
           qvc = Q2(i,j)+frac * (Q(i,j,k)-Q2(i,j))
           qc = qvc/(1.+qvc)

          else 
           frac = (topo_ndfd(i,j)-hght(i,j,k-1)) /&
               (hght(i,j,k)-hght(i,j,k-1))
           exn1 = (pmid(i,j,k-1)/P1000)**rovcp_p
           exn0 = (pmid(i,j,k)/P1000)**rovcp_p
! --- pressure at NDFD topo
           pnew(i,j) = P1000* ((exn1 +frac *&
               (exn0 - exn1)) **cpovr_p)
           thetak=((P1000/PMID(i,j,k))**CAPA)*T(i,j,k)
           thetak1=((P1000/PMID(i,j,k-1))**CAPA)*T(i,j,k-1)
           thetavc = thetak1+frac * (thetak-thetak1)
           qvc = Q(i,j,k-1)+frac * (Q(i,j,k)-Q(i,j,k-1))
           qc = qvc/(1.+qvc)
          endif
! --- temperature
          tup = thetavc*(pnew(i,j)/P1000)**rovcp_p&
            / (1.+0.6078*qc)
          alttup=t2(i,j)+frac*(t(i,j,k)-t2(i,j))
            
!  provisional 2m temp at NDFD topo
          tnew(i,j) = t2(i,j) + (alttup-t1)

! --- Don't let extrapolated temp to be any larger than
!     the value at the model terrain level.
!     This will avoid the problem with NDFD temp values
!     being set to be much warmer than model 2m temp.

          tsfc=t2(i,j) + (zs-topo_ndfd(i,j))*gam(i,j)

!         if (trim(model) == 'RAP') then
!         if (tnew(i,j) .gt. t2(i,j)) then
!          tnew(i,j) = min(tnew(i,j),tsfc)
!         endif
!         else
          ! lapse rate suggestion from Guoqing
          tnew(i,j)=tsfc
          if (tnew(i,j) .gt. t2(i,j)) then
           tnew(i,j) = t2(i,j)
          endif
!         endif

! --- Just use q at model 1st level in this case.
!     should use q2, but the values don't look good
!     Obtain Td corresponding to NDFD pres otherwise.
          qv=q(i,j,1)
          e=pnew(i,j)/100.*qv/(0.62197+qv)
! --- dew-point temperature at original sfc
          ENL = ALOG(E)
          DWPT = (243.5*ENL-440.8)/(19.48-ENL)
          td = dwpt + 273.15
! --- dewpoint temperature
          if (domain == 'CS' .or. domain == 'AK') then
            dewnew(i,j) = tnew(i,j) - tddep
          else
            dewnew(i,j) = min(td,tnew(i,j))
          endif
          if (k .eq. 1) then
            uc = u10(i,j)+frac * (uwnd(i,j,k)-u10(i,j))
            vc = v10(i,j)+frac * (vwnd(i,j,k)-v10(i,j))
          else
            uc = uwnd(i,j,k-1)+frac * (uwnd(i,j,k)-uwnd(i,j,k-1))
            vc = vwnd(i,j,k-1)+frac * (vwnd(i,j,k)-vwnd(i,j,k-1))
          endif

! -- 0.7 factor is a wag at surface effects on wind speed
!    when interpolating from the free atmosphere to
!    the NDFD topo.
          speedc = 0.7*sqrt(uc*uc+vc*vc)
          if (trim(model) == 'RAP') then
            speed = sqrt(uwnd(i,j,1)**2 + vwnd(i,j,1)**2)
          else
            speed = sqrt(u10(i,j)**2 + v10(i,j)**2)
          endif
          ratio = max(1.,speedc/(max(0.001,speed)) )
          if (trim(model) == 'RAP') then
            unew(i,j) = ratio*(uwnd(i,j,1))
            vnew(i,j) = ratio*(vwnd(i,j,1))
          else
            unew(i,j) = ratio*(u10(i,j))
            vnew(i,j) = ratio*(v10(i,j))
          endif

!============================================
        END IF
!============================================

      end do iloop2
      end do jloop2

!============================================
! -- use vegtype to get better temps/dewpoint/winds
!    near coastlines.
!    Use nearest neighbor adjustment where model 
!    land-water mask does not mask NDFD land-water mask 
!============================================

! create temporary holder for u,v,t,td so that the "real"
! values don't get shifted around in the adjustment
      print*,'Begin land/sea adjustment'
      do j=1,jm
      do i=1,im
        ttmp(i,j)=tnew(i,j)
        dtmp(i,j)=dewnew(i,j)
        utmp(i,j)=unew(i,j)
        vtmp(i,j)=vnew(i,j)
        rough_mod(i,j) = coast(i,j)
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
            if (veg_ndfd(i,j).eq.16. .and.&
              rough_mod(i,j).gt.0.05) then
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
            if (veg_ndfd(i,j).ne.16. .and.&
              rough_mod(i,j).lt.0.05) then
              iadj = 0
              do j1=jm1,jp1
                do i1=im1,ip1
                  if (rough_mod(i1,j1).gt.0.05) then
                    iadj = 1
                  end if
                end do
              end do
              if (iadj.eq.1) then
                rough_mod(i,j) = 0.1
                nmod = nmod + 1
              end if
            end if
          end do
        end do
        write (6,*)k,'No. pts changed, water-to-land=',nmod
      end do

      jloop3: do j=1,jm
      iloop3: do i=1,im
        if (coast(i,j).gt.0.05 .and.&
           rough_mod(i,j).lt.0.05) then
! -----------------------------------------------------------------
! -- i.e.  NDFD grid-point is over WATER (per rough_mod)
!          model-interp grid-point is over LAND 
! -----------------------------------------------------------------
               
          do ibuf=1,10
            ia = max(1,i-ibuf)
            ib = min(im,i+ibuf)
            ja = max(1,j-ibuf)
            jb = min(jm,j+ibuf)
               
            do jw = ja,jb
              id = 1
              do iw = ia,ib,id
                if (coast(iw,jw).lt.0.05) then 
                  IF (VALIDPT(IW,JW)) THEN
                    unew(i,j) = utmp(iw,jw)
                    vnew(i,j) = vtmp(iw,jw)
                    tnew(i,j) = ttmp(iw,jw)
                    dewnew(i,j) = dtmp(iw,jw)
                    n_rough_yes = n_rough_yes+1
                    cycle iloop3
                  ENDIF
                end if
              end do
            end do
               
          end do
          n_rough_no = n_rough_no+1
               
        end if
        if (coast(i,j).lt.0.05 .and.&
          rough_mod(i,j).gt.0.05) then
! -----------------------------------------------------------------
! -- i.e.  NDFD grid-point is over LAND (per rough_mod)
!          model-interp grid-point is over WATER
! -----------------------------------------------------------------
               
          do ibuf=1,10
            ia = max(1,i-ibuf)
            ib = min(im,i+ibuf)
            ja = max(1,j-ibuf)
            jb = min(jm,j+ibuf)
               
            do jw = ja,jb
              id = 1
              do iw = ia,ib,id
                if (coast(iw,jw).gt.0.05) then
                  IF (VALIDPT(I,J)) THEN
                    unew(i,j) = utmp(iw,jw)
                    vnew(i,j) = vtmp(iw,jw)
                    tnew(i,j) = ttmp(iw,jw)
                    dewnew(i,j) = dtmp(iw,jw)
                    m_rough_yes = m_rough_yes+1
                    cycle iloop3
                  ENDIF
                end if
              end do
            end do
          end do
          m_rough_no = m_rough_no+1
        end if
      end do iloop3
      end do jloop3

      do j=1,jm
      do i=1,im
        if (VALIDPT(I,J)) THEN
          qnew(i,j)=PQ0/pnew(I,J)*EXP(A2*(dewnew(I,J)-A3)/&
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

      deallocate (grid,mask)

      return
      end
