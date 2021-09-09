MODULE module_dry_dep_driver
  IMPLICIT NONE

CONTAINS

    subroutine dry_dep_driver( id,ktau,dtstep,config_flags,                     &
                               dz8w,rho_phy,rel_hum,exch_h,hfx,pbl,           &
                               rmol,ust,                                   &
                               z_at_mid,z_at_w,                          &
                               lu_index,ddflx,                                         &
                               frp, min_fplume, coef_bb_dc,         &
                              ! depvelocity,                                               &
                              ! dep_vel,num_vert_mix,                                      &
                               chem,      &
                               ids,ide, jds,jde, kds,kde,                                 &
                               ims,ime, jms,jme, kms,kme,                                 &
                               its,ite, jts,jte, kts,kte                                  )
!----------------------------------------------------------------------
  USE module_model_constants
  USE module_configure
  USE module_state_description
  USE module_domain_type, only : domain
!  USE module_dep_simple
  USE module_vertmx_wrf
!  USE module_data_sorgam
!  USE module_aerosols_sorgam
!  USE module_gocart_settling
!  USE module_vash_settling
!  USE module_gocart_drydep
!  USE module_smoke_drydep  ! RAR
!  USE module_mosaic_drydep, only:  mosaic_drydep_driver
!  USE module_mixactivate_wrappers, only: mosaic_mixactivate, sorgam_mixactivate, &
!                                         sorgam_vbs_mixactivate, soa_vbs_mixactivate
  !USE module_aer_drydep
  !USE module_aerosols_soa_vbs, only: soa_vbs_depdriver

!  USE module_cam_mam_drydep, only:  cam_mam_drydep_driver
  
!  use module_cam_support, only: pcnst => pcnst_runtime
!  USE module_data_cam_mam_asect, only: lptr_chem_to_q, lptr_chem_to_qqcw !Balwinder.Singh@pnnl.gov: Added to avoid mixing of CHEM array constituents multiple times
!  USE modal_aero_data,         only: numptr_amode, lmassptr_amode, ntot_amode, nspec_amode !Added by Balwinder.Singh@pnnl.gov to avoid mixing of CHEM array constituents multiple times
!  USE module_cam_mam_drydep, only:  cam_mam_drydep_driver
!  use module_scalar_tables,     only: chem_dname_table !Balwinder.Singh@pnnl.gov:Added for MAM aerosols dry deposition
!  USE module_aerosols_sorgam_vbs, only: sorgam_vbs_depdriver
  
  IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
!   LOGICAL, INTENT(IN)                           :: is_CAMMGMP_used !BSINGH:01/31/2013: Added is_CAMMGMP_used for MAM drydep
   INTEGER,      INTENT(IN   ) :: id,                               &
!                                  sf_urban_physics,             &
                                 ! current_month,                &
                                  ids,ide, jds,jde, kds,kde,    &
                                  ims,ime, jms,jme, kms,kme,    &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) :: ktau
!   REAL(KIND=8), INTENT(IN   ) :: curr_secs
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),        &
!         INTENT(IN ) ::                                   moist
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_scalar ),       &
!         INTENT(INOUT ) ::                               scalar
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),         &
         INTENT(INOUT ) ::                                 chem
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_tracer ),         &
!         INTENT(INOUT ) ::                                 tracer

!   REAL, DIMENSION( ims:ime, 1:config_flags%kemit, jms:jme,num_emis_ant),&
!         INTENT(IN ) ::                                    emis_ant

!   REAL, DIMENSION( ims:ime, 1, jms:jme, num_ebu_in ),                     &
!         INTENT(INOUT ) ::                                   ebu_in
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) ::   frp, lu_index, rmol, coef_bb_dc
   INTEGER, DIMENSION( ims:ime, jms:jme ), INTENT(IN ) :: min_fplume

  ! REAL, DIMENSION( ims:ime, config_flags%kdepvel, jms:jme, config_flags%ndepvel ), &
  !       INTENT(INOUT ) ::                                   dep_vel

!   REAL, DIMENSION( ims:ime, config_flags%kdvel, jms:jme, num_dvel ), &
!         INTENT(INOUT ) ::                                   dvel

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) ::   dz8w,rel_hum,   &
                                                             z_at_mid, z_at_w ,  &
                                                             exch_h, rho_phy
!   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
!          INTENT(INOUT) ::                                      &
!               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2,    &
!               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2
   REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: pbl, ust, hfx !, snowh

!   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,        &
!          INTENT(INOUT ) ::                                     &
!                    cldfra,     & ! cloud fraction current timestep
!                    cldfra_old    ! cloud fraction previous timestep
!   REAL,  DIMENSION( ims:ime , jms:jme, 5 )                   ,    &
!          INTENT(IN) ::            seasin,dustin
!   REAL,  DIMENSION( ims:ime , jms:jme )                   ,    &
!          INTENT(OUT) ::                                      &
!                                                     dep_vel_o3
   REAL,  DIMENSION( ims:ime ,jms:jme ), INTENT(INOUT) ::  ddflx    !dry deposition flux, accumulated
                    !ddlen, & !dry deposition length

!   REAL, INTENT(OUT), dimension(ims:ime,kms:kme,jms:jme) :: nsource, &
!!	     ccn1,ccn2,ccn3,ccn4,ccn5,ccn6  ! number conc of aerosols activated at supersat

      REAL, INTENT(IN) :: dtstep    !,dx

      REAL ::  clwchem,  dvfog, ta, vegfrac, z1,zntt
      REAL ::  old, new, fac, dvpart, curr_frp !, kpart

      INTEGER :: n, nr, ipr, jpr, nvr   !,   &
                 !idrydep_onoff, aer_mech_id
      INTEGER :: l2,m, l

      REAL, DIMENSION( ims:ime, jms:jme, num_chem ) ::  ddvel
!   REAL, DIMENSION( num_chem )                   ::   ddmassn

 !  REAL, DIMENSION( ims:ime, jms:jme, num_chem ) ::   qsrflx ! dry deposition flux of aerosols (explicit aq.-phase cases)

!   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) :: dryrho_phy
   REAL,  DIMENSION( kms:kme ) :: dryrho_1d

! turbulent transport
   real :: pblst(kts:kte),ekmfull(kts:kte+1),zzfull(kts:kte+1),zz(kts:kte)
   integer :: kk,i,j,k,nv
   integer :: k_a, k_c, kmax, m_mam

   REAL, PARAMETER :: epsilc=1.E-16, kpart=500.  ! For now we assume no lu dependence for kpart

   INTEGER, SAVE :: icall
! .. Intrinsic Functions ..
   INTRINSIC max, min

 !  idrydep_onoff = 0
!   drydep_select: SELECT CASE(config_flags%gas_drydep_opt)
 !    CASE ( WESELY )
!
        ! RAR: deposition velocites for the smoke tracers
!sm_dep:   IF ( config_flags%chem_opt == CHEM_SMOKE .AND. config_flags%aer_drydep_opt == 111 ) then

 !           call wesely_driver(id,                               &
 !                   config_flags,current_month,                                &
 !                   julday,rh,moist,p8w,t8w,raincv,                     &
 !                   ddvel,aer_res_def,                &
 !                   ivgtyp,tsk,gsw,vegfra,pbl,                   &
 !                   rmol,ust,znt,z,z_at_w,                          &
 !                   snowh, numgas,                                             &
 !                   ids,ide, jds,jde, kds,kde,                                 &
 !                   ims,ime, jms,jme, kms,kme,                                 &
 !                   its,ite, jts,jte, kts,kte                                  )

 !           call smoke_drydep_driver(config_flags%debug_chem,numgas,          &
 !                   p8w,t8w,rmol,        &
 !                   rho_phy,dz8w,ddvel,xland,hfx,     &
 !                   ivgtyp,tsk,vegfra,pbl,ust,znt,               &
 !                   ids,ide, jds,jde, kds,kde,                   &
 !                   ims,ime, jms,jme, kms,kme,                   &
 !                   its,ite, jts,jte, kts,kte                    )

 !       END IF   sm_dep
 
       !     IF (icall<10 .AND. config_flags%debug_chem) then
       !         WRITE(6,*) 'dry_dep_driver: numgas ',numgas
       !     END IF

       do 100 j=jts,jte
          do 100 i=its,ite

            do k=kts,kte+1
              zzfull(k)=z_at_w(i,k,j)-z_at_w(i,kts,j)
            enddo
            do k=kts,kte
              zz(k)=z_at_mid(i,k,j)-z_at_w(i,kts,j)
              ekmfull(k)=max(1.e-6,exch_h(i,k,j))
            enddo
            ekmfull(kts)=0.
            ekmfull(kte+1)=0.

check_mx: IF (config_flags%enh_vermix) THEN
!    if (p_e_co>= param_first_scalar )then
!       if (emis_ant(i,kts,j,p_e_co) .gt. 0.) then             ! RAR: anthropogenic emissions
!            ekmfull(kts:kts+2) = max(ekmfull(kts:kts+2),1.)

!         IF (icall<100 .AND. config_flags%debug_chem) then
!            WRITE(6,*) 'dry_dep_driver: ekmfull(kts:kts+10) ',ekmfull(kts:kts+10)
!            WRITE(6,*) 'dry_dep_driver: i,j,emis_ant(i,kts,j,p_e_co) ',i,j,emis_ant(i,kts,j,p_e_co)
!            !icall=icall+1
!         END IF
!       endif
!    endif

!    if (p_ebu_in_oc >= param_first_scalar ) then
!       if (ebu_in(i,1,j,p_ebu_in_oc) .gt. .001) then   !.AND. ekmfull(kts+1)<2.) then
        !ekmfull(kts:kte/2) = max(ekmfull(kts:kte/2),2.)

       !  if (frp(i,j) > 1.) then
       !     ekmfull(kts:kts+4) = max(ekmfull(kts:kts+4),5.)    ! RAR: updated for nighttime the HRRR vertical grid
        ! elseif (frp(i,j) > .1) then
        !    ekmfull(kts:kts+3) = max(ekmfull(kts:kts+3),2.)    ! RAR: updated for nighttime the HRRR vertical grid
       !  endif

! After implementation of mass flux for smoke, this part will be eliminated
         if (frp(i,j) > 1.) then   ! .AND. hfx(i,j)>0.) then
            do kk=1,4
              ekmfull(kts+kk) = MAX(1.1*ekmfull(kts+kk),10.)
            enddo 
         endif 

         ! Flaming intensive fires 
         curr_frp= coef_bb_dc(i,j)* frp(i,j)
         if (curr_frp>250.) then
            do kk=1,5
              ekmfull(kts+kk) = MAX(1.1*ekmfull(kts+kk),20.)
            end do
         endif  
        
         IF (icall<3000 .AND. config_flags%debug_chem) then
          IF (i==its .AND. j==jts) THEN
            WRITE(6,*) 'dry_dep_driver: coef_bb_dc(i,j),frp(i,j) ',coef_bb_dc(i,j),frp(i,j)
            WRITE(6,*) 'dry_dep_driver: after correction- ekmfull(kts:kts+10) ',ekmfull(kts:kts+10)
          END IF
         END IF

  ENDIF check_mx

         do k=kts,kte
            zz(k)=z_at_mid(i,k,j)-z_at_w(i,kts,j)
         enddo
!
!   vertical mixing routine (including deposition)
!   need to be careful here with that dumm tracer in spot 1
!   do not need lho,lho2
!   (03-may-2006 rce - calc dryrho_1d and pass it to vertmx)
!
!      dep_vel_o3(i,j)=ddvel(i,j,p_o3)
loop_c: do nv=2,num_chem

         do k=kts,kte
            pblst(k)= max(epsilc,chem(i,k,j,nv))
            dryrho_1d(k) = rho_phy(i,k,j)
         enddo

         ddvel(i,j,nv) = 0.0        ! by default no dry dep.=0.

sm_dep:  IF (config_flags%aer_drydep_opt == 111 ) then

          ! IF (lu_index(i,j)<6.) THEN
          !    kpart= 100.
          ! ELSE
          !    kpart= 500.
          ! ENDIF

         ! RAR: this is taken from module_dep_simple
            dvpart = ust(i,j)/kpart

            IF (rmol(i,j)<0.) THEN       ! UNSTABLE LAYERING CORRECTION
               dvpart = dvpart*(1.+(-300.*rmol(i,j))**0.66667)
            ENDIF

            IF (rel_hum(i,1,j)>0.8) THEN    ! HIGH RELATIVE HUMIDITY CORRECTION
               dvpart = dvpart*(1.+0.37*exp((rel_hum(i,1,j)-0.8)/0.2))
            END IF
            ddvel(i,j,nv) = MIN(0.50,dvpart)        ! m/s

         ENDIF   sm_dep

         call vertmx(dtstep,pblst,ekmfull,dryrho_1d,zzfull,zz,ddvel(i,j,nv),kts,kte)

        IF (icall<100 .AND. config_flags%debug_chem) then
         if (i==its .AND. j==jts) then
            WRITE(*,*) 'dry_dep_driver: 2) nv,ims,ime,jms,jme,kms,kme ',nv,ims,ime,jms,jme,kms,kme 
            WRITE(*,*) 'dry_dep_driver: 2) i,j,lu_index(i,j) ',i,j,lu_index(i,j)
            WRITE(*,*) 'dry_dep_driver: 2) pblst(1:5),pblst(kte-4:kte) ',pblst(1:5),pblst(kte-4:kte)
         endif
        END IF

        IF (icall<4000 .AND. config_flags%debug_chem) then       
         if (i==its .AND. j==jts) then
            WRITE(*,*) 'dry_dep_driver: 2) dvpart, ddvel(i,j,nv) ',dvpart,ddvel(i,j,nv)
            icall=icall+1
         endif
        END IF

         ! chem is in ppmv
         ! dry deposition is combined with vertical mixing, but column independent.
         ! Hence, all molecules lost per column must be dry deposited.

         ! old and new column totals (mol/m2 or ug/m2)
         old = 0.0
         new = 0.0

         do k=kts,kte
           ! fac = 1.0
           ! if (nv <= numgas) then
           !   fac = 1e-6 * dryrho_1d(k) * 1./(mwdry*1.e-3) * dz8w(i,k,j)
           ! else
              fac = dryrho_1d(k) * dz8w(i,k,j)
           ! endif

            old = old + max(epsilc,chem(i,k,j,nv)) * fac
            new = new + max(epsilc,pblst(k)) * fac
         enddo

         ! we ignore (spurious) and add new dry deposition to
         ! existing field (accumulated deposition!)
         ddflx(i,j)=  ddflx(i,j) + max( 0.0, (old - new) )   ! we have one chem variable only, accumulated quantity

         do k=kts,kte-1
            chem(i,k,j,nv)=max(epsilc,pblst(k))
         enddo
      enddo     loop_c

      IF (icall<1000 .AND. config_flags%debug_chem) then
        if (i==its .AND. j==jts) then
          WRITE(6,*) 'dry_dep_driver: 2)fac ',fac
          WRITE(6,*) 'dry_dep_driver: 2)old,new,ddflx(i,j) ',old,new,ddflx(i,j)
        endif
      END IF

100   continue

!   IF((config_flags%dust_opt .EQ. 1) .OR. (config_flags%dust_opt .GE. 3) .OR. &
!      (config_flags%seas_opt .GE. 1) ) THEN
!   settling_select: SELECT CASE(config_flags%chem_opt)
!!! TUCCELLA
!   CASE (DUST,GOCART_SIMPLE,GOCARTRACM_KPP,MOZCART_KPP,RADM2SORG,RADM2SORG_AQ, &
!         RADM2SORG_AQCHEM,RACMSORG_AQCHEM_KPP,RACM_ESRLSORG_AQCHEM_KPP,RACM_SOA_VBS_AQCHEM_KPP)
!       CALL wrf_debug(15,'call gocart settling routine')
!         call gocart_settling_driver(dtstep,config_flags,t_phy,moist,  &
!         chem,rho_phy,dz8w,p8w,p_phy,         &
!         dustin,seasin,dx,g, &
!         dustgraset_1,dustgraset_2,dustgraset_3,                           &
!         dustgraset_4,dustgraset_5,                                        &
!         setvel_1,setvel_2,setvel_3,setvel_4,setvel_5, imod,               &
!         ids,ide, jds,jde, kds,kde,                                        &
!         ims,ime, jms,jme, kms,kme,                                        &
!         its,ite, jts,jte, kts,kte                                         )
!   CASE (CHEM_VASH, CHEM_VOLC, CHEM_VOLC_4BIN)
!       CALL wrf_debug(15,'call vash settling routine')
!         call vash_settling_driver(dtstep,config_flags,t_phy,moist,        &
!         chem,rho_phy,dz8w,p8w,p_phy,                                      &
!         ash_fall,dx,g,                                                    &
!         ids,ide, jds,jde, kds,kde,                                        &
!         ims,ime, jms,jme, kms,kme,                                        &
!         its,ite, jts,jte, kts,kte                                         )
!   CASE DEFAULT
!       CALL wrf_debug(15,'no settling routine')
!   END SELECT settling_select
!   ENDIF

   CALL wrf_debug(15,'end of dry_dep_driver')

END SUBROUTINE dry_dep_driver

END MODULE module_dry_dep_driver
