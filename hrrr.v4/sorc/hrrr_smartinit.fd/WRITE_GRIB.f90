      subroutine write_grib(date,fhr,gdin,gfld,itot,maxlev,domain,model)

        use grddef
        use aset3d
        use aset2d
        use asetdown
        use rdgrib
        use constants
        use GRIB_MOD
        use pdstemplates

      TYPE (GINFO) :: GDIN
      TYPE(GRIBFIELD)::GFLD

      CHARACTER *80 FNAMEOUT
      character*4 model
      character*2 domain
      integer fhr

      im=gdin%imax
      jm=gdin%jmax

      FNAMEOUT='fort.  '

      LUB = 71
! Open grib2 file for writing
      WRITE(FNAMEOUT(6:7),FMT='(I2)')LUB
      write(0,*) 'call baopen: ', lub
      CALL BAOPEN(LUB,FNAMEOUT,IRET)

      print *,'IRET from BAOPEN of 70: ',IRET

! Write 2-m temperature to grib2

      DEC=-2.0
      CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNT)
      print *,'template number',GFLD%ipdtmpl
      print *,'idrtemplate number',GFLD%idrtmpl
      GFLD%ipdtnum=0
      GFLD%ipdtmpl(1)=0
      GFLD%ipdtmpl(2)=0
      GFLD%ipdtmpl(9)=FHR
      GFLD%ipdtmpl(10)=1
      GFLD%ipdtmpl(12)=0

      if(trim(model) == 'HRRR')then
       DO KK = 1, ITOT
         IF(MOD(KK,IM).EQ.0) THEN
           M=IM
           N=INT(KK/IM)
         ELSE
           M=MOD(KK,IM)
           N=INT(KK/IM) + 1
         ENDIF
         IF (DOWNT(M,N).EQ. 9.9E10) THEN
           BITMAP(KK)=.FALSE.
         ELSE
           BITMAP(KK)=.TRUE.
         ENDIF
       ENDDO
      GFLD%ibmap=0
      GFLD%bmap=BITMAP
      DEC=-4.0
      GFLD%ipdtmpl(10)=103
      GFLD%ipdtmpl(12)=2
      endif
      print *,'template number after',GFLD%ipdtmpl

      GFLD%idrtnum=40 !JPEG2000
      GFLD%idrtmpl(2)=DEC
      GFLD%idrtmpl(5)=0
      GFLD%idrtmpl(6)=0
      GFLD%idrtmpl(7)=-1
      GFLD%idrtmpl(1)=0

      if (trim(model) == 'RAP') GFLD%ibmap=255

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write 2-m dew point to grib2

      DEC=-2.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNDEW)

      GFLD%ipdtmpl(1)=0
      GFLD%ipdtmpl(2)=6
      GFLD%ipdtmpl(10)=1
      GFLD%ipdtmpl(12)=0
      if(trim(model) == 'HRRR')then
      GFLD%ibmap=0
      DEC=-4.0
      GFLD%ipdtmpl(10)=103
      GFLD%ipdtmpl(12)=2
      endif

      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write 2-m Q to grib2
! Changed precision of specific humidity
      DEC=6.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNQ)

      GFLD%ipdtmpl(1)=1
      GFLD%ipdtmpl(2)=0
      GFLD%ipdtmpl(10)=1
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      if(trim(model) == 'HRRR')then
      GFLD%ibmap=0
      GFLD%ipdtmpl(10)=103
      GFLD%ipdtmpl(12)=2
      endif

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write 10-m u wind to grib2
      DEC=-2.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNU)

      GFLD%ipdtmpl(1)=2
      GFLD%ipdtmpl(2)=002
      GFLD%ipdtmpl(10)=1
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      if(trim(model) == 'HRRR')then
      GFLD%ibmap=0
      GFLD%ipdtmpl(10)=103
      GFLD%ipdtmpl(12)=10
      endif

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write 10-m v wind to grib2
      DEC=-2.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNV)

      GFLD%ipdtmpl(1)=2
      GFLD%ipdtmpl(2)=003
      GFLD%ipdtmpl(10)=1
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      if(trim(model) == 'HRRR')then
      GFLD%ibmap=0
      GFLD%ipdtmpl(10)=103
      GFLD%ipdtmpl(12)=10
      endif

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write wind gust to grib2
! Increased precision for wind gust
      DEC=-4.0

      if(trim(model) == 'HRRR')then
      CALL FILL_FLD(GFLD,ITOT,IM,JM,GUST)
      GFLD%ibmap=0
      else
      CALL FILL_FLD(GFLD,ITOT,IM,JM,WGUST)
      endif

      GFLD%ipdtmpl(1)=2
      GFLD%ipdtmpl(2)=022
      GFLD%ipdtmpl(10)=1
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

      if (trim(model) == 'RAP') then
      if (domain == 'CS' .or. domain == 'AK') then
! Write snow depth to grib2
        DEC=3.0

        CALL FILL_FLD(GFLD,ITOT,IM,JM,SNOD)

        GFLD%ipdtmpl(1)=1
        GFLD%ipdtmpl(2)=11
        GFLD%ipdtmpl(10)=1
        GFLD%ipdtmpl(12)=0
        GFLD%idrtmpl(2)=DEC

        CALL set_scale(gfld,DEC)
        CALL PUTGB2(71,GFLD,IRET)
      endif
      endif

! Write surface pressure to grib2
! Change DEC from 3.0 to 6.0 for more precision
      DEC=6.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNP)

      if (trim(model) == 'HRRR') then
      GFLD%ibmap=0
      else
      GFLD%ibmap=255
      endif
      GFLD%ipdtmpl(1)=3
      GFLD%ipdtmpl(2)=0
      GFLD%ipdtmpl(10)=1
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write topography to grib2
      DEC=-2.0

      if(trim(model) == 'HRRR')then
      DEC=3.0
      endif

      CALL FILL_FLD(GFLD,ITOT,IM,JM,TOPO_NDFD)

      GFLD%ipdtmpl(1)=3
      GFLD%ipdtmpl(2)=5
      GFLD%ipdtmpl(10)=1
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write computed TCLD to grib2
      DEC=3.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,SKY)

      GFLD%ipdtmpl(1)=6
      GFLD%ipdtmpl(2)=1
      GFLD%ipdtmpl(10)=1
      GFLD%ipdtmpl(12)=0
      if(trim(model) == 'HRRR')then
       DEC=-3.0
       GFLD%ipdtmpl(10)=200
      endif
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write cloud base to grib2

      if(trim(model) == 'RAP')then
      GFLD%ibmap=255
      DO KK = 1, ITOT
        IF(MOD(KK,IM).EQ.0) THEN
          M=IM
          N=INT(KK/IM)
        ELSE
          M=MOD(KK,IM)
          N=INT(KK/IM) + 1
        ENDIF
        IF (BASEZ(M,N).EQ. 0.) THEN
          BASEZ(M,N)=20000.01
        ENDIF
      ENDDO
      else
      DO KK = 1, ITOT
        IF(MOD(KK,IM).EQ.0) THEN
          M=IM
          N=INT(KK/IM)
        ELSE
          M=MOD(KK,IM)
          N=INT(KK/IM) + 1
        ENDIF
         IF (BASEZ(M,N).EQ. 0.) THEN
           BITMAP(KK)=.FALSE.
         ELSE
           BITMAP(KK)=.TRUE.
         ENDIF
      ENDDO
       GFLD%ibmap=0
       GFLD%bmap=BITMAP
      endif
      DEC=-3.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,BASEZ)

      GFLD%ipdtmpl(1)=3
      GFLD%ipdtmpl(2)=5
      GFLD%ipdtmpl(10)=2
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write ceiling height to grib2

      if(trim(model) == 'RAP')then
! Initially assume no bitmap
      GFLD%ibmap=255
      DO KK = 1, ITOT
        IF(MOD(KK,IM).EQ.0) THEN
          M=IM
          N=INT(KK/IM)
        ELSE
          M=MOD(KK,IM)
          N=INT(KK/IM) + 1
        ENDIF
        IF (CEIL(M,N).EQ. 0.) THEN
          CEIL(M,N)=20000.01
        ENDIF
      ENDDO
      else
      DO KK = 1, ITOT
        IF(MOD(KK,IM).EQ.0) THEN
          M=IM
          N=INT(KK/IM)
        ELSE
          M=MOD(KK,IM)
          N=INT(KK/IM) + 1
        ENDIF
         IF (CEIL(M,N).EQ. 0.) THEN
           BITMAP(KK)=.FALSE.
         ELSE
           BITMAP(KK)=.TRUE.
         ENDIF
       ENDDO
      GFLD%ibmap=0
      GFLD%bmap=BITMAP
      endif
      DEC=-3.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,CEIL)

      GFLD%ipdtmpl(1)=3
      GFLD%ipdtmpl(2)=5
      GFLD%ipdtmpl(10)=215
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write SLP to grib2

! Do not need to change precision, it matches the NDFD grid
      if (trim(model) == 'RAP') GFLD%ibmap=255
      DEC=-3.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,SLP)

      GFLD%ipdtmpl(1)=3
      GFLD%ipdtmpl(2)=198
      GFLD%ipdtmpl(10)=101
      GFLD%ipdtmpl(12)=0

      if(trim(model) == 'HRRR')then
       DO KK = 1, ITOT
         IF(MOD(KK,IM).EQ.0) THEN
           M=IM
           N=INT(KK/IM)
         ELSE
           M=MOD(KK,IM)
           N=INT(KK/IM) + 1
         ENDIF
         IF (DOWNT(M,N).EQ. 9.9E10) THEN
           BITMAP(KK)=.FALSE.
         ELSE
           BITMAP(KK)=.TRUE.
         ENDIF
       ENDDO
      DEC=-0.1
      GFLD%ipdtmpl(2)=1
      GFLD%ibmap=0
      GFLD%bmap=BITMAP
      endif
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

!     if (domain == 'AK') then
! Write fields needed for radiance data assimilation in RTMA [4 Jan 2018]
! pressure, temperature, mixing ratio, u, v, height at model level 1
! pressure, temperature, and mixing ratio at model level 2
! and roughness length for Alaska nest only

! Write these fields for all grids - 11 Apr 2019

! Pressure at model level 1
        DEC=6.0

        CALL FILL_FLD(GFLD,ITOT,IM,JM,PMID(:,:,1))

        GFLD%discipline=0
        GFLD%ipdtnum=0
        GFLD%ipdtmpl(1)=3
        GFLD%ipdtmpl(2)=000
        GFLD%ipdtmpl(10)=105
        GFLD%ipdtmpl(12)=1
        GFLD%idrtmpl(2)=DEC

        CALL set_scale(gfld, DEC)
        CALL PUTGB2(71,GFLD,IRET)  ! Pres at model level 1
        write(0,*) 'IRET for PRES at model level 1: ', IRET
        write(0,*) 'maxval(PMID),minval(PMID) at level 1: ', maxval(PMID(:,:,1)),minval(PMID(:,:,1))
        print*,'maxval(PMID),minval(PMID): at level 1', maxval(PMID(:,:,1)),minval(PMID(:,:,1))

! Pressure at model level 2
        DEC=6.0

        CALL FILL_FLD(GFLD,ITOT,IM,JM,PMID(:,:,2))

        GFLD%discipline=0
        GFLD%ipdtnum=0
        GFLD%ipdtmpl(1)=3
        GFLD%ipdtmpl(2)=000
        GFLD%ipdtmpl(10)=105
        GFLD%ipdtmpl(12)=2
        GFLD%idrtmpl(2)=DEC

        CALL set_scale(gfld, DEC)
        CALL PUTGB2(71,GFLD,IRET)  ! Pres at model level 2
        write(0,*) 'IRET for PRES at model level 2: ', IRET
        write(0,*) 'maxval(PMID),minval(PMID) at level 2: ', maxval(PMID(:,:,2)),minval(PMID(:,:,2))
        print*,'maxval(PMID),minval(PMID): at level 2', maxval(PMID(:,:,2)),minval(PMID(:,:,2))

! Temperature at model level 1
        DEC=-4.0

        CALL FILL_FLD(GFLD,ITOT,IM,JM,T(:,:,1))

        GFLD%discipline=0
        GFLD%ipdtnum=0
        GFLD%ipdtmpl(1)=000
        GFLD%ipdtmpl(2)=000
        GFLD%ipdtmpl(10)=105
        GFLD%ipdtmpl(12)=1
        GFLD%idrtmpl(2)=DEC

        CALL set_scale(gfld, DEC)
        CALL PUTGB2(71,GFLD,IRET)  ! Temp at model level 1
        write(0,*) 'IRET for T at model level 1: ', IRET
        write(0,*) 'maxval(T),minval(T) at level 1: ', maxval(T(:,:,1)),minval(T(:,:,1))
        print*,'maxval(T),minval(T): at level 1', maxval(T(:,:,1)),minval(T(:,:,1))

! Temperature at model level 2
        DEC=-4.0

        CALL FILL_FLD(GFLD,ITOT,IM,JM,T(:,:,2))

        GFLD%discipline=0
        GFLD%ipdtnum=0
        GFLD%ipdtmpl(1)=000
        GFLD%ipdtmpl(2)=000
        GFLD%ipdtmpl(10)=105
        GFLD%ipdtmpl(12)=2
        GFLD%idrtmpl(2)=DEC

        CALL set_scale(gfld, DEC)
        CALL PUTGB2(71,GFLD,IRET)  ! Temp at model level 2
        write(0,*) 'IRET for T at model level 2: ', IRET
        write(0,*) 'maxval(T),minval(T) at level 2: ', maxval(T(:,:,2)),minval(T(:,:,2))
        print*,'maxval(T),minval(T): at level 2', maxval(T(:,:,2)),minval(T(:,:,2))

! Specific humidity at model level 1
        DEC=6.0

        CALL FILL_FLD(GFLD,ITOT,IM,JM,Q(:,:,1))

        GFLD%discipline=0
        GFLD%ipdtnum=0
        GFLD%ipdtmpl(1)=001
        GFLD%ipdtmpl(2)=000
        GFLD%ipdtmpl(10)=105
        GFLD%ipdtmpl(12)=1
        GFLD%idrtmpl(2)=DEC

        CALL set_scale(gfld, DEC)
        CALL PUTGB2(71,GFLD,IRET)  ! Q at model level 1
        write(0,*) 'IRET for Q at model level 1: ', IRET
        write(0,*) 'maxval(Q),minval(Q) at level 1: ', maxval(Q(:,:,1)),minval(Q(:,:,1))
        print*,'maxval(Q),minval(Q): at level 1', maxval(Q(:,:,1)),minval(Q(:,:,1))

! Specific humidity at model level 2
        DEC=6.0

        CALL FILL_FLD(GFLD,ITOT,IM,JM,Q(:,:,2))

        GFLD%discipline=0
        GFLD%ipdtnum=0
        GFLD%ipdtmpl(1)=001
        GFLD%ipdtmpl(2)=000
        GFLD%ipdtmpl(10)=105
        GFLD%ipdtmpl(12)=2
        GFLD%idrtmpl(2)=DEC

        CALL set_scale(gfld, DEC)
        CALL PUTGB2(71,GFLD,IRET)  ! Q at model level 2
        write(0,*) 'IRET for Q at model level 2: ', IRET
        write(0,*) 'maxval(Q),minval(Q) at level 2: ', maxval(Q(:,:,2)),minval(Q(:,:,2))
        print*,'maxval(Q),minval(Q): at level 2', maxval(Q(:,:,2)),minval(Q(:,:,2))

! U at model level 1
        DEC=-4.0

        CALL FILL_FLD(GFLD,ITOT,IM,JM,UWND(:,:,1))

        GFLD%discipline=0
        GFLD%ipdtnum=0
        GFLD%ipdtmpl(1)=002
        GFLD%ipdtmpl(2)=002
        GFLD%ipdtmpl(10)=105
        GFLD%ipdtmpl(12)=1
        GFLD%idrtmpl(2)=DEC

        CALL set_scale(gfld, DEC)
        CALL PUTGB2(71,GFLD,IRET)  ! U at model level 1
        write(0,*) 'IRET for U at model level 1: ', IRET
        write(0,*) 'maxval(U),minval(U) at level 1: ', maxval(UWND(:,:,1)),minval(UWND(:,:,1))
        print*,'maxval(U),minval(U): at level 1', maxval(UWND(:,:,1)),minval(UWND(:,:,1))

! V at model level 1
        DEC=-4.0

        CALL FILL_FLD(GFLD,ITOT,IM,JM,VWND(:,:,1))

        GFLD%discipline=0
        GFLD%ipdtnum=0
        GFLD%ipdtmpl(1)=002
        GFLD%ipdtmpl(2)=003
        GFLD%ipdtmpl(10)=105
        GFLD%ipdtmpl(12)=1
        GFLD%idrtmpl(2)=DEC

        CALL set_scale(gfld, DEC)
        CALL PUTGB2(71,GFLD,IRET)  ! V at model level 1
        write(0,*) 'IRET for V at model level 1: ', IRET
        write(0,*) 'maxval(V),minval(V) at level 1: ', maxval(VWND(:,:,1)),minval(VWND(:,:,1))
        print*,'maxval(V),minval(V): at level 1', maxval(VWND(:,:,1)),minval(VWND(:,:,1))

! HGHT at model level 1
        DEC=-3.0
        if(trim(model) == 'HRRR')DEC=-5.0

        CALL FILL_FLD(GFLD,ITOT,IM,JM,HGHT(:,:,1))

        GFLD%discipline=0
        GFLD%ipdtnum=0
        GFLD%ipdtmpl(1)=003
        GFLD%ipdtmpl(2)=005
        GFLD%ipdtmpl(10)=105
        GFLD%ipdtmpl(12)=1
        GFLD%idrtmpl(2)=DEC

        CALL set_scale(gfld, DEC)
        CALL PUTGB2(71,GFLD,IRET)  ! HGHT at model level 1
        write(0,*) 'IRET for HGHT at model level 1: ', IRET
        write(0,*) 'maxval(HGHT),minval(HGHT) at level 1: ', maxval(HGHT(:,:,1)),minval(HGHT(:,:,1))
        print*,'maxval(HGHT),minval(HGHT): at level 1', maxval(HGHT(:,:,1)),minval(HGHT(:,:,1))

! SFC Roughness
        DEC=2.7
        if(trim(model) == 'HRRR')DEC=4.0

        CALL FILL_FLD(GFLD,ITOT,IM,JM,SFCR)

        GFLD%discipline=2
        GFLD%ipdtnum=0
        GFLD%ipdtmpl(1)=000
        GFLD%ipdtmpl(2)=001
        GFLD%ipdtmpl(10)=001
        GFLD%ipdtmpl(12)=0
        GFLD%idrtmpl(2)=DEC

        CALL set_scale(gfld, DEC)
        CALL PUTGB2(71,GFLD,IRET)  ! SFCR
        write(0,*) 'IRET for SFCR at model level 1: ', IRET
        write(0,*) 'maxval(SFCR),minval(SFCR) at level 1: ', maxval(SFCR),minval(SFCR)
        print*,'maxval(SFCR),minval(SFCR): at level 1', maxval(SFCR),minval(SFCR)

! Skin Temperature/SST
       DEC=-2.0
       if(trim(model) == 'HRRR')DEC=-4.0
       DEC=-4.0

       CALL FILL_FLD(GFLD,ITOT,IM,JM,SST)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=0
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=103
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(71,GFLD,IRET)  ! SST
       write(0,*) 'IRET for SST: ', IRET
       write(0,*) 'maxval(SST),minval(SST): ', maxval(SST),minval(SST)
       print*,'maxval(SST),minval(SST): ', maxval(SST),minval(SST)

! Local lapse-rate
!      DEC=-2.0
!      if(trim(model) == 'HRRR')DEC=-4.0
       DEC=4.0

       CALL FILL_FLD(GFLD,ITOT,IM,JM,GAM)

       GFLD%discipline=0
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=0
       GFLD%ipdtmpl(2)=8
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld, DEC)
       CALL PUTGB2(71,GFLD,IRET)  ! GAM
       write(0,*) 'IRET for GAM: ', IRET
       write(0,*) 'maxval(GAM),minval(GAM): ', maxval(GAM),minval(GAM)
       print*,'maxval(GAM),minval(GAM): ', maxval(GAM),minval(GAM)

! Write model surface pressure to grib2
      DEC=6.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,PSFC)

      if (trim(model) == 'HRRR') then
      GFLD%ibmap=0
      else
      GFLD%ibmap=255
      endif
      GFLD%ipdtmpl(1)=3
      GFLD%ipdtmpl(2)=0
      GFLD%ipdtmpl(10)=103
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

!     endif

      if (trim(model) == 'RAP')then
!      Write model total cloud to grib2
      DEC=-3.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,TCLD)

      GFLD%discipline=0
      GFLD%ipdtmpl(1)=6
      GFLD%ipdtmpl(2)=1
      GFLD%ipdtmpl(10)=200
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write low cloud to grib2
      DEC=-3.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,LCLD)

      GFLD%ipdtmpl(1)=6
      GFLD%ipdtmpl(2)=3
      GFLD%ipdtmpl(10)=214
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write middle cloud to grib2
      DEC=-3.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,MCLD)

      GFLD%ipdtmpl(1)=6
      GFLD%ipdtmpl(2)=4
      GFLD%ipdtmpl(10)=224
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write high cloud to grib2
      DEC=-3.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,HCLD)

      GFLD%ipdtmpl(1)=6
      GFLD%ipdtmpl(2)=5
      GFLD%ipdtmpl(10)=234
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write composite reflectivity to grib2
      DEC=3.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,REFC)

      GFLD%ipdtmpl(1)=16
      GFLD%ipdtmpl(2)=196
      GFLD%ipdtmpl(10)=200
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

!========================================================================
! calcSnowLevel - takes sounding of the wetbulb temperature and finds the
! lowest elevation (above ground) where wetbulb crosses from
! above freezing to below freezing. When top wetbulb is above
! freezing - puts in height of top level.   We now use this
! field straight out of the NAM.
!
! Write wet bulb zero to grib2
      DEC=3.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,WETFRZ)

      GFLD%ipdtmpl(1)=3
      GFLD%ipdtmpl(2)=5
      GFLD%ipdtmpl(10)=245
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)
      endif

! VISIBILITY
! Write visibility to grib2
      DEC=2.7

      CALL FILL_FLD(GFLD,ITOT,IM,JM,VIS)

      GFLD%discipline=0
      GFLD%ipdtmpl(1)=19
      GFLD%ipdtmpl(2)=0
      GFLD%ipdtmpl(10)=1
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      if (trim(model) == 'HRRR') then
       DO KK = 1, ITOT
         IF(MOD(KK,IM).EQ.0) THEN
           M=IM
           N=INT(KK/IM)
         ELSE
           M=MOD(KK,IM)
           N=INT(KK/IM) + 1
         ENDIF
         IF (DOWNT(M,N).EQ. 9.9E10) THEN
           BITMAP(KK)=.FALSE.
         ELSE
           BITMAP(KK)=.TRUE.
         ENDIF
       ENDDO
      GFLD%ibmap=0
      GFLD%bmap=BITMAP
      endif
      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

      if (trim(model) == 'RAP') then
! Write PBL wind direction to grib2
      DEC=3.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,DIRTRANS)

      GFLD%ipdtmpl(1)=2
      GFLD%ipdtmpl(2)=0
      GFLD%ipdtmpl(10)=220
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write PBL wind speed to grib2
      DEC=-3.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,MGTRANS)

      GFLD%ipdtmpl(1)=2
      GFLD%ipdtmpl(2)=1
      GFLD%ipdtmpl(10)=220
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write PBL RH to grib2
      DEC=3.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,BLR)

      GFLD%ipdtmpl(1)=1
      GFLD%ipdtmpl(2)=1
      GFLD%ipdtmpl(10)=220
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write PBL height to grib2
      DEC=-3.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,MIXHGT)

      GFLD%ipdtmpl(1)=3
      GFLD%ipdtmpl(2)=6
      GFLD%ipdtmpl(10)=220
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

! Write best lifted index to grib2
      DEC=2.0

      CALL FILL_FLD(GFLD,ITOT,IM,JM,LAL)

      GFLD%ipdtmpl(1)=7
      GFLD%ipdtmpl(2)=11
!     GFLD%ipdtmpl(2)=193 ! or 195 - CWDI(Convective Weather Detection Index)?
      GFLD%ipdtmpl(10)=1
      GFLD%ipdtmpl(12)=0
      GFLD%idrtmpl(2)=DEC

      CALL set_scale(gfld,DEC)
      CALL PUTGB2(71,GFLD,IRET)

      endif

      CALL BACLOSE(71,iret)

      print *, 'completed write_grib'

      return
      end


