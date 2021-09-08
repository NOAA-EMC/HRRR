subroutine calctends_model(u,v,t,q,oz,cw,pri,phi,phi_x,phi_y,u_x,u_y,v_x,v_y,t_x,t_y,pri_x,pri_y,&
                           q_x,q_y,oz_x,oz_y,cw_x,cw_y,z,mype,u_t,v_t,t_t,q_t,oz_t,cw_t,ps_t)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    calctends       calculate u,v,t,p tendencies
!   prgmmr: kleist           org: np20                date: 2005-09-29
!
! abstract: compute tendencies for pressure, wind, and virtual 
!           temperature
!
! program history log:
!   2005-09-29  kleist
!   2005-10-17  kleist - changes to improve computational efficiency
!   2005-11-21  kleist - add tracer tendencies, use new module
!   2006-04-12  treadon - replace sigi with bk5
!   2006-04-21  kleist - add divergence tendency
!   2006-07-31  kleist - changes to use ps instead of ln(ps)
!   2007-04-16  kleist - remove divergence tendency bits to outside
!   2007-05-08  kleist - add bits for fully generalized coordinate
!   2007-06-21  rancic - add pbl 
!   2007-07-02  derber - move calculation of z_x, z_y into routine
!   2007-07-26 cucurull - add pri in argument list, call getprs_horiz;
!                         move getprs outside calctends;
!                         remove ps from argument list
!   2007-08-08  derber - optimize, remove calculation of t_over* and dp_over* unless needed.
!   2008-06-05  safford - rm unused vars and uses
!   2010-02-24  rancic - adjust for use in 4dvar perturbation model 
!
! usage:
!   input argument list:
!     u        - zonal wind on subdomain
!     v        - meridional wind on subdomain
!     t        - virtual temperature on subdomain
!     z        - sfc terrain height
!     u_x      - zonal derivative of u
!     u_y      - meridional derivative of u
!     v_x      - zonal derivative of v
!     v_y      - meridional derivative of v
!     t_x      - zonal derivative of t
!     t_y      - meridional derivative of t
!     pri_x    - zonal derivative of 3d pressure
!     pri_y    - meridional derivative of 3d pressure
!     q_x      - zonal derivative of q
!     q_y      - meridional derivative of q
!     oz_x     - zonal derivative of ozone
!     oz_y     - meridional derivative of ozone
!     cw_x     - zonal derivative of cloud water
!     cw_y     - meridional derivative of cloud water
!     mype     - task id
!
!   output argument list:
!     u_t      - time tendency of u
!     v_t      - time tendency of v
!     t_t      - time tendency of t
!xxxx p_t      - time tendency of 3d prs  xxxxxxxxxxxxxxxxxx
!     ps_t     - replaced back by Rancic in order to increase efficiecy
!     q_t      - time tendency of q
!     oz_t     - time tendency of ozone
!     cw_t     - time tendency of cloud water
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds,only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,istart,rlats,nlat,idvc5,bk5,&
     jstart,region_lat,region_lon,eta2_ll,wrf_nmm_regional,nlon,regional,&
     corlats
  use constants, only: ione,zero,half,one,two,rearth,rd,rcp,omega,grav,cp,pi
  use tendsmod, only: what9,prsth9,r_prsum9,r_prdif9,prdif9,pr_xsum9,pr_xdif9,pr_ysum9,&
     pr_ydif9,coriolis,ctph0,stph0,tlm0
  use tends4pertmod, only: prsum9,curvfct,rdtop9
  implicit none

! Declare passed variables
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(in   ):: u,v,t,u_x,u_y,v_x,v_y,&
     t_x,t_y,q,oz,cw,q_x,q_y,oz_x,oz_y,cw_x,cw_y,phi,phi_x,phi_y
  real(r_kind),dimension(lat2,lon2,nsig+ione)          ,intent(in   ):: pri_x,pri_y
  real(r_kind),dimension(lat2,lon2)          ,intent(in   ) :: z
  integer(i_kind)                            ,intent(in   ) :: mype
  real(r_kind),dimension(lat2,lon2,nsig)     ,intent(  out) :: u_t,v_t,t_t,q_t,oz_t,cw_t
  real(r_kind),dimension(lat2,lon2)          ,intent(  out) :: ps_t
  real(r_kind),dimension(lat2,lon2,nsig+ione),intent(in   ) :: pri

! Declare local variables
  real(r_kind),dimension(lat2,lon2,nsig):: div
  real(r_kind),dimension(lat2,lon2,nsig):: prdif9u,prdif9v
  real(r_kind),dimension(lat2,lon2,nsig):: prdif9u_x,prdif9v_y2
  real(r_kind),dimension(lat2,lon2,nsig):: pgf_x,pgf_y,pgf_xx,pgf_yy
  real(r_kind),dimension(lat2,lon2,nsig):: pgf1_x,pgf1_y,pgf1_xx,pgf1_yy
  real(r_kind),dimension(lat2,lon2):: rdrdx2,rdrdy2
  real(r_kind),dimension(nsig):: t_over_p,dp_over_p
  real(r_kind) tmp,tmp2,tmp9,count,count0,kap
  integer(i_kind) i,j,k,ix,jx
  real(r_kind) relm,crlm,aph,sph,cph,cc,tph
  real(r_kind) rr,dlam,dphi,wpdar
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  real(r_kind), parameter:: dts=600.,eps_damp=0.2,fac_nk_damp=2.6
  integer(i_kind) k_top,nk_damp,k_damp
  real(r_kind) dampwt,rdampwt,pihalf,arg,rnk_damp
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
  real(r_kind),dimension(lat2):: dslam,rlat_deg
  real(r_kind),dimension(lon2):: rlon_deg
  integer(i_kind) unit_number
  character(len=4) ch_mype
  character(len=9) fname
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

! NOTES:
!  - equations taken from NCEP Office Note 445 (Juang 2005)
!  - this is the nonlinear routine, which currently in the GSI is only
!    called based on the current guess solution.  As such, basic state
!    variables that are needed for the TLM are loaded here (in the *9
!    arrays)


  what9=zero

! constants
  if(wrf_nmm_regional) then
    do j=1,lon2
      jx=j+jstart(mype+ione)-2_i_kind
      jx=min(max(1,jx),nlon)
      do i=1,lat2
        ix=istart(mype+ione)+i-2_i_kind
        ix=min(max(ix,1),nlat)
        coriolis(i,j)=two*omega*sin(region_lat(ix,jx))
        relm=region_lon(ix,jx)-tlm0
        crlm=cos(relm)
        aph=region_lat(ix,jx)
        sph=sin(aph)
        cph=cos(aph)
        cc=cph*crlm
        tph=asin(ctph0*sph-stph0*cc)
        curvfct(i,j)=tan(tph)/rearth
      end do
    end do
  else
    do j=1,lon2
      do i=1,lat2
        ix=istart(mype+ione)+i-2_i_kind
        ix=min(max(ix,2),nlat-1)
        coriolis(i,j)=corlats(ix)
        curvfct(i,j)=tan(rlats(ix))/rearth
      end do
    end do
  end if

  rr=one/rearth**2
  dlam=two*pi/nlon
  dphi=pi/nlat      
  wpdar=  zero

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!TEST    TEST     TEST    TEST    TEST    TEST     TEST    TEST   TEST    TEST
!.............................................................................
!   write(ch_mype,'(i4.4)') mype
!   fname='dlat.'//ch_mype
!   unit_number=401+mype
!
!   do i=1,lat2
!     ix=istart(mype+1)+i-2
!     ix=min(max(ix,2),nlat-1)
!       dslam(i)=rearth*cos(rlats(ix))*dlam
!       rlat_deg(i)=rlats(ix)*180./pi
!   end do      
!   open(unit_number,file=fname,form='formatted')
!   do i=1,lat2
!     write(unit_number,1000) mype,dslam(i),rlat_deg(i),i
!   end do
! 1000 format(i6,2x,f15.4,2x,f10.4,2x,i5)
!   close(unit_number)
!   stop
!.............................................................................
!TEST    TEST     TEST    TEST    TEST    TEST     TEST    TEST   TEST    TEST
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    do j=1,lon2
      do i=1,lat2
        ix=istart(mype+ione)+i-2_i_kind
        ix=min(max(ix,2),nlat-1)
        rdrdx2(i,j)=rr/(cos(rlats(ix))*dlam)**2
        rdrdy2(i,j)=rr/dphi**2
      end do
    end do

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        pr_xsum9(i,j,k)=pri_x(i,j,k)+pri_x(i,j,k+ione)
        pr_xdif9(i,j,k)=pri_x(i,j,k)-pri_x(i,j,k+ione)
        pr_ysum9(i,j,k)=pri_y(i,j,k)+pri_y(i,j,k+ione)
        pr_ydif9(i,j,k)=pri_y(i,j,k)-pri_y(i,j,k+ione)
        prdif9u(i,j,k)=prdif9(i,j,k)*u(i,j,k)
        prdif9v(i,j,k)=prdif9(i,j,k)*v(i,j,k)
      end do
    end do
  end do

! 0) Define A-grid modification for divergence 
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
        rdtop9(i,j,k)=-rd*t(i,j,k)*r_prsum9(i,j,k)
        pgf_x(i,j,k)=-pr_xsum9(i,j,k)*rdtop9(i,j,k)+phi_x(i,j,k)
        pgf_y(i,j,k)=-pr_ysum9(i,j,k)*rdtop9(i,j,k)+phi_y(i,j,k)
      end do
    end do
  end do
   
!!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMB

!!  call mp_compact_dlon2(pgf_x,pgf_xx,.false.,nsig,mype)
!!  call mp_compact_dlat2(pgf_y,pgf_yy,.false.,nsig,mype)

!!  do k=1,nsig
!!    do j=1,lon2-1
!!      do i=1,lat2
!!        pgf1_x(i,j,k)= rdrdx2(i,j)*( rd*(t(i,j+ione,k)+t(i,j,k)) * &
!!                         (prsum9(i,j+ione,k)-prsum9(i,j,k))/   &
!!                         (prsum9(i,j+ione,k)+prsum9(i,j,k)) + &
!!                           (phi(i,j+ione,k)-phi(i,j,k)) )
!!      end do
!!    end do
!!  end do

!!  do k=1,nsig
!!    do j=1,lon2
!!      do i=1,lat2-1
!!        pgf1_y(i,j,k)= rdrdy2(i,j)*( rd*(t(i+ione,j,k)+t(i,j,k)) * &
!!                         (prsum9(i+ione,j,k)-prsum9(i,j,k))/   &
!!                         (prsum9(i+ione,j,k)+prsum9(i,j,k)) + &
!!                            (phi(i+ione,j,k)-phi(i,j,k)) )
!!      end do
!!    end do
!!  end do


!!  do k=1,nsig
!!    do j=2,lon2-1
!!      do i=2,lat2-1
!!        pgf1_xx(i,j,k)=pgf1_x(i,j,k)-pgf1_x(i,j-1,k)
!!        pgf1_yy(i,j,k)=pgf1_y(i,j,k)-pgf1_y(i-1,j,k)
!!      end do
!!    end do
!!  end do
!!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMME

  call mp_compact_dlon2(prdif9u,prdif9u_x,.false.,nsig,mype)
  call mp_compact_dlat2(prdif9v,prdif9v_y2,.true.,nsig,mype)

  div(:,:,:)=zero 
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
!!    do j=2,lon2-1
!!      do i=2,lat2-1
!!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMB
!!        div(i,j,k)=wpdar*( pgf1_xx(i,j,k) + pgf1_yy(i,j,k)  &
!!                         - pgf_xx(i,j,k) -  pgf_yy(i,j,k) ) 
!!MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMME
        div(i,j,k)=div(i,j,k)+prdif9u_x(i,j,k)+prdif9v_y2(i,j,k)
      end do
    end do
  end do
   

! 1) Compute horizontal part of tendency for 3d pressure (so dps/dt is the same
!    as prsth9(i,j,1) . . . also note that at the top, dp/dt=0
!    or: prsth9(i,j,nsig+ione)=0

  do j=1,lon2
    do i=1,lat2
      prsth9(i,j,nsig+ione)=zero
    end do
  end do
  do k=nsig,1,-1
    do j=1,lon2
      do i=1,lat2
         prsth9(i,j,k)=prsth9(i,j,k+ione) - div(i,j,k)
      end do
    end do
  end do   

! 1.1) Compute horizontal part of tendency for T (needed for vertical velocity in hybrid 
!      theta coordinates)

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2
         tmp9=-rdtop9(i,j,k)
         t_t(i,j,k)=-u(i,j,k)*t_x(i,j,k) - v(i,j,k)*t_y(i,j,k)
         t_t(i,j,k)=t_t(i,j,k) + tmp9*rcp * ( u(i,j,k)*pr_xsum9(i,j,k) + &
             v(i,j,k)*pr_ysum9(i,j,k) + prsth9(i,j,k) + prsth9(i,j,k+ione) )
      end do  
    end do   
  end do  

! 2) calculate vertical velocity term:  z(dp/dz) (zero at top/bottom interfaces)
! if running global, and there is a c(k) coefficient, we call the vvel subroutine

  if ( (.not.regional) .AND. (idvc5.eq.3)) then
    call getvvel(t,t_t,prsth9,prdif9,what9,1,lon2)
  else
    do k=2,nsig
      do j=1,lon2
        do i=1,lat2
          if(wrf_nmm_regional) then
            what9(i,j,k)=prsth9(i,j,k)-eta2_ll(k)*prsth9(i,j,1)
          else
            what9(i,j,k)=prsth9(i,j,k)-bk5(k)*prsth9(i,j,1)
          end if
        end do
      end do
    end do
  end if ! end if on 

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!   pihalf=pi*half

!   k_top=nsig
!   nk_damp=k_top/fac_nk_damp+ione
!   k_damp=k_top-nk_damp
!   rnk_damp=one/nk_damp

!    do k=2,nsig
!      do j=1,lon2
!        do i=1,lat2
!            if(k.ge.k_damp) then
!              arg=pihalf*(k-k_damp)*rnk_damp
!              dampwt=eps_damp*dts**sin(arg)**2
!              rdampwt=one/(one+dampwt)
!            else 
!              rdampwt=one
!            end if
!          what9(i,j,k)=what9(i,j,k)*rdampwt
!        end do
!      end do
!    end do

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

! 3) load actual dp/dt here now, as prsth9 is reused in 
!    what9(i,k,1) & what9(i,j,nsig+ione) = zero
!    p_t(i,j,1) is the same as the surface pressure tendency

!!mr  do k=1,nsig+ione
    do j=1,lon2
      do i=1,lat2
!!mr      p_t(i,j,k)=prsth9(i,j,k)-what9(i,j,k)
        ps_t(i,j)=prsth9(i,j,1)-what9(i,j,1)
      end do
    end do
!!mr  end do
 
  do k=1,nsig
    do j=1,lon2
      do i=1,lat2

! horizontal part of momnetum equations
        u_t(i,j,k)= &
           -u(i,j,k)*u_x(i,j,k) - v(i,j,k)*u_y(i,j,k) &
           + coriolis(i,j)*v(i,j,k)  &
           + curvfct(i,j)*(u(i,j,k)*v(i,j,k))  &
           - pgf_x(i,j,k)

        v_t(i,j,k)= &
          -u(i,j,k)*v_x(i,j,k) - v(i,j,k)*v_y(i,j,k)   &
         - coriolis(i,j)*u(i,j,k) &
         - curvfct(i,j)*(u(i,j,k)*u(i,j,k))   &
         - pgf_y(i,j,k)

! horizontal advection of "tracer" quantities
        q_t(i,j,k) = -u(i,j,k)*q_x(i,j,k) - v(i,j,k)*q_y(i,j,k)
        oz_t(i,j,k) = -u(i,j,k)*oz_x(i,j,k) - v(i,j,k)*oz_y(i,j,k)
        cw_t(i,j,k) = -u(i,j,k)*cw_x(i,j,k) - v(i,j,k)*cw_y(i,j,k)
      end do  !end do i
    end do    !end do j
  end do      !end do k

  do k=1,nsig
    do j=1,lon2
      do i=1,lat2

! vertical flux terms
        if (k.gt.1) then
          tmp = half*what9(i,j,k)*r_prdif9(i,j,k)
          u_t(i,j,k) = u_t(i,j,k) - tmp*(u(i,j,k-1)-u(i,j,k))
          v_t(i,j,k) = v_t(i,j,k) - tmp*(v(i,j,k-1)-v(i,j,k))
          t_t(i,j,k) = t_t(i,j,k) - tmp*(t(i,j,k-1)-t(i,j,k))
          q_t(i,j,k) = q_t(i,j,k) - tmp*(q(i,j,k-1)-q(i,j,k))
          oz_t(i,j,k) = oz_t(i,j,k) - tmp*(oz(i,j,k-1)-oz(i,j,k))
          cw_t(i,j,k) = cw_t(i,j,k) - tmp*(cw(i,j,k-1)-cw(i,j,k))
        end if
        if (k.lt.nsig) then
          tmp = half*what9(i,j,k+ione)*r_prdif9(i,j,k)
          u_t(i,j,k) = u_t(i,j,k) - tmp*(u(i,j,k)-u(i,j,k+ione))
          v_t(i,j,k) = v_t(i,j,k) - tmp*(v(i,j,k)-v(i,j,k+ione))
          t_t(i,j,k) = t_t(i,j,k) - tmp*(t(i,j,k)-t(i,j,k+ione))
          q_t(i,j,k) = q_t(i,j,k) - tmp*(q(i,j,k)-q(i,j,k+ione))
          oz_t(i,j,k) = oz_t(i,j,k) - tmp*(oz(i,j,k)-oz(i,j,k+ione))
          cw_t(i,j,k) = cw_t(i,j,k) - tmp*(cw(i,j,k)-cw(i,j,k+ione))
        end if        

      end do  !end do i
    end do    !end do j
  end do      !end do k

  call hdiff(u_x,u_y,v_x,v_y,t_x,t_y,u_t,v_t,t_t,mype)
  call sfcdrag(u,v,t,pri,u_t,v_t)
 
  if(.not.wrf_nmm_regional)then
    do k=1,nsig
      do j=1,lon2
        do i=1,lat2
          ix=istart(mype+ione)+i-2_i_kind
          if (ix == 1 .OR. ix == nlat) then
            u_t(i,j,k)=zero
            v_t(i,j,k)=zero
          end if
        end do 
      end do
    end do  !end do k
  end if

  return
end subroutine calctends_model
