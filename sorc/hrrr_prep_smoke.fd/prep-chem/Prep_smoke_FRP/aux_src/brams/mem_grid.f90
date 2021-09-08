!
! Copyright (C) 1991-2004  ; All Rights Reserved ; Colorado State University
! Colorado State University Research Foundation ; ATMET, LLC
! 
! This file is free software; you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software 
! Foundation; either version 2 of the License, or (at your option) any later version.
! 
! This software is distributed in the hope that it will be useful, but WITHOUT ANY 
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
! PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with this 
! program; if not, write to the Free Software Foundation, Inc., 
! 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!======================================================================================
!############################# Change Log ##################################
! 5.0.0
!
!###########################################################################


Module mem_grid

   use grid_dims

   Type grid_vars
                            
      ! Variables to be dimensioned by (nxp,nyp)
   real, pointer, dimension(:,:) :: &
                 topt,  topu,  topv,  topm, topma, topta  &
              ,  rtgt,  rtgu,  rtgv,  rtgm  &
              ,  f13t,  f13u,  f13v,  f13m, f23t , f23u , f23v , f23m  &
              ,   dxt,   dxu,   dxv,   dxm,  dyt ,  dyu ,  dyv ,  dym  & 
              , fmapt, fmapu, fmapv, fmapm,fmapti,fmapui,fmapvi,fmapmi &
              ,  glat,  glon, topzo
              
      !  Variables for the ADAP coordinate
   real, pointer, dimension(:,:,:)    :: aru,arv,arw,volu,volv,volw,volt
   integer, pointer, dimension(:,:)   :: lpu,lpv,lpw
      
      
   End Type
   
   type (grid_vars), allocatable :: grid_g(:), gridm_g(:)

   !-------------------------------------------------------------------------------
   character(len=64) :: expnme
   !-------------------------------------------------------------------------------
   integer :: ngrids,ngridsh
   !-------------------------------------------------------------------------------
   integer, target, dimension(maxgrds) :: nnxp,nnyp,nnzp                              

   !-------------------------------------------------------------------------------
   integer, dimension(maxgrds) :: nnx,nnx1,nnx2,nny,nny1,nny2,nnz,nnz1  &
                              ,nnxyzp,nnxysp,nnxyp
   !-------------------------------------------------------------------------------
   real, dimension(nzpmax,maxgrds) :: htn,ht2n,ht4n,hwn,hw2n,hw4n
   !-------------------------------------------------------------------------------
   real, dimension(nzpmax,maxgrds) :: dztn,dzmn,dzt2n,dzm2n,ztn,zmn
   real, dimension(nxpmax,maxgrds) :: xtn,xmn
   real, dimension(nypmax,maxgrds) :: ytn,ymn
   !-------------------------------------------------------------------------------
   integer :: nxp,nx,nx1,nx2,nyp,ny,ny1,ny2,nzp,nzpp,nz,nz1  &
             ,nxyzp,nxyp,nxysp,nscl,nsttop,nstbot,ndtrat,jdim
   !-------------------------------------------------------------------------------
   real                    :: deltax,deltay,deltaz
   real, dimension(nzpmax) :: ht,ht2,ht4,hw,hw2,hw4,zt,zm,dzt,dzm,dzt2,dzm2
   real, dimension(nxpmax) :: xt,xm
   real, dimension(nypmax) :: yt,ym
   
   integer :: nzg,nzs,npatch 

   integer :: if_adap,itopo,ihtran

   !-------------------------------------------------------------------------------
   integer :: ngrid,ngridc,ngrido,iscr1,iscr2
   !-------------------------------------------------------------------------------
   integer                    :: memsize,iounit,maxpro,memscr,memind  &
                                ,iogrid,maxpts,maxnzp,maxnxp,maxnyp,i2dvar
   integer,dimension(maxgrds) :: memgrd

   !-------------------------------------------------------------------------------
   real                     :: time,ztop,dzrat,dzmax,eps
   integer                  :: impl,ideltat,iyear1,imonth1,idate1,ihour1  &
                              ,itime1,nacoust,initial,iflag
   integer, dimension(maxgrds) :: nnacoust                              
   real, dimension(maxgrds) :: deltazn,deltaxn,deltayn,dtlongn,dimove,djmove  &
                              ,gridu,gridv
   real, dimension(nzpmax)  :: zz

   real                     :: dtlong,sspct,polelat,polelon
   real, dimension(maxgrds) :: platn,plonn,centlat,centlon  &
                              ,cflxy,cflz
   !-------------------------------------------------------------------------------
   character (len=16) :: runtype
   character(len=1)   :: timeunit
   !-------------------------------------------------------------------------------
   integer :: isstp,istp
   real    :: timmax,dts,dtlt,dtlv

   integer                   :: nestz1,nestz2
   integer, dimension(maxgrds) :: nndtrat,nstratx,nstraty  &
                                 ,ngbegun,nxtnest,nnsttop,nnstbot  &
                                 ,ninest,njnest,nknest
   integer, dimension(nzpmax)  :: nstratz1,nstratz2
   
   !-------------------------------------------------------------------------------
   integer, parameter                     :: maxsched=2000,maxschent=5
   integer                                :: nsubs
   integer, dimension(maxsched,maxschent) :: isched

   !---------------------------------------------------------------------------
   integer                            :: nrzflg
   integer, dimension(nxpmax,maxgrds) :: ipm
   integer, dimension(nypmax,maxgrds) :: jpm 
   integer, dimension(nzpmax,maxgrds) :: nrz,kpm 
   real, dimension(nxpmax,maxgrds)    :: ei1,ei2,ei3,ei4,ei5,ei6,ei7 
   real, dimension(nypmax,maxgrds)    :: ej1,ej2,ej3,ej4,ej5,ej6,ej7
   real, dimension(nzpmax,maxgrds)    :: ek1,ek2,ek3,ek4,ek5,ek6,ek7 
   real, dimension(nzpmax,maxgrds,4)  :: fbcf
   !---------------------------------------------------------------------------
   !-------------------------------------------------------------------------------
   integer                  :: iadvl,iadvf,lsflg,ibnd,jbnd,icorflg,nfpt
   !-------------------------------------------------------------------------------
   integer                  :: naddsc,iversion
   !-------------------------------------------------------------------------------
   real                     :: distim ,cphas
   ! 
   !-------------------------------------------------------------------------------
   !-------------------------------------------------------------------------------
   !-------------------------------------------------------------------------------
   integer                  :: nhemgrd2
   integer                     :: nhemt,nhemu,nhemv  
   integer, dimension(4,maxhp) :: ihem1tt,jhem1tt,ihem1uu,jhem1uu  &
                                    ,ihem1uv,jhem1uv,ihem1vu,jhem1vu  &
                                 ,ihem1vv,jhem1vv
   integer, dimension(maxhp)   :: ihem2tt,jhem2tt,ihem2uu,jhem2uu  &
                                 ,ihem2uv,jhem2uv,ihem2vu,jhem2vu  &
                                 ,ihem2vv,jhem2vv
   real, dimension(maxhp)      :: hlatt,hlatu,hlatv,hlont,hlonu,hlonv
   real, dimension(4,maxhp)    :: whem1tt,whem1uu,whem1uv,whem1vu,whem1vv
   !-------------------------------------------------------------------------------
   ! Lambert-conformal stuff
   real :: stdlat1,stdlat2
   real :: stdlon ! EMK TEST

   !-------------------------------------------------------------------------------
Contains

   subroutine alloc_grid(grid,n1,n2,n3,ng,if_adap)

   implicit none
   type (grid_vars) :: grid
   integer, intent(in) :: n1,n2,n3,ng,if_adap

! Allocate arrays based on options (if necessary)
      
     allocate (grid%topt(n2,n3))
     allocate (grid%topu(n2,n3))
     allocate (grid%topv(n2,n3))
     allocate (grid%topm(n2,n3))
     allocate (grid%topma(n2,n3))
     allocate (grid%topta(n2,n3))
     allocate (grid%rtgt(n2,n3))
     allocate (grid%rtgu(n2,n3))
     allocate (grid%rtgv(n2,n3))
     allocate (grid%rtgm(n2,n3))
     allocate (grid%f13t(n2,n3))
     allocate (grid%f13u(n2,n3))
     allocate (grid%f13v(n2,n3))
     allocate (grid%f13m(n2,n3))
     allocate (grid%f23t(n2,n3))
     allocate (grid%f23u(n2,n3))
     allocate (grid%f23v(n2,n3))
     allocate (grid%f23m(n2,n3))
     allocate (grid%dxt(n2,n3))
     allocate (grid%dxu(n2,n3))
     allocate (grid%dxv(n2,n3))
     allocate (grid%dxm(n2,n3))
     allocate (grid%dyt(n2,n3))
     allocate (grid%dyu(n2,n3))
     allocate (grid%dyv(n2,n3))
     allocate (grid%dym(n2,n3))
     allocate (grid%fmapt(n2,n3))
     allocate (grid%fmapu(n2,n3))
     allocate (grid%fmapv(n2,n3))
     allocate (grid%fmapm(n2,n3))
     allocate (grid%fmapti(n2,n3))
     allocate (grid%fmapui(n2,n3))
     allocate (grid%fmapvi(n2,n3))
     allocate (grid%fmapmi(n2,n3))
     allocate (grid%glat(n2,n3))
     allocate (grid%glon(n2,n3))
     allocate (grid%topzo(n2,n3))
     
     if (if_adap == 1) then
        allocate (grid%aru(n1,n2,n3)) 
        allocate (grid%arv(n1,n2,n3)) 
        allocate (grid%arw(n1,n2,n3))
        allocate (grid%volu(n1,n2,n3))
        allocate (grid%volv(n1,n2,n3))
        allocate (grid%volw(n1,n2,n3))
        allocate (grid%volt(n1,n2,n3))
     endif
     allocate (grid%lpu(n2,n3)) 
     allocate (grid%lpv(n2,n3)) 
     allocate (grid%lpw(n2,n3))
    
   return
   end subroutine


   subroutine nullify_grid(grid)

   implicit none
   type (grid_vars) :: grid

   nullify (grid%topt)  ;  nullify (grid%topu)  ;  nullify (grid%topv)
   nullify (grid%topm)  ;  nullify (grid%topma) ;  nullify (grid%topta)   
   nullify (grid%rtgt)  ;  nullify (grid%rtgu)
   nullify (grid%rtgv)  ;  nullify (grid%rtgm)  ;  nullify (grid%f13t)
   nullify (grid%f13u)  ;  nullify (grid%f13v)  ;  nullify (grid%f13m)
   nullify (grid%f23t)  ;  nullify (grid%f23u)  ;  nullify (grid%f23v)
   nullify (grid%f23m)  ;  nullify (grid%dxt)   ;  nullify (grid%dxu)
   nullify (grid%dxv)   ;  nullify (grid%dxm)   ;  nullify (grid%dyt)
   nullify (grid%dyu)   ;  nullify (grid%dyv)   ;  nullify (grid%dym)
   nullify (grid%fmapt) ;  nullify (grid%fmapu) ;  nullify (grid%fmapv)
   nullify (grid%fmapm) ;  nullify (grid%fmapti);  nullify (grid%fmapui)
   nullify (grid%fmapvi);  nullify (grid%fmapmi);  nullify (grid%glat)
   nullify (grid%glon)  ;  nullify (grid%topzo)
   
   nullify (grid%aru)   ;  nullify (grid%arv)   ;  nullify (grid%arw)
   nullify (grid%volu)  ;  nullify (grid%volv)  ;  nullify (grid%volw)
   nullify (grid%volt)  
   nullify (grid%lpu)   ;  nullify (grid%lpv)   ;  nullify (grid%lpw)

   return
   end subroutine

   subroutine dealloc_grid(grid)

   implicit none
   type (grid_vars) :: grid
   
    if (associated(grid%topt)  )    deallocate (grid%topt)
    if (associated(grid%topu)  )    deallocate (grid%topu)
    if (associated(grid%topv)  )    deallocate (grid%topv)
    if (associated(grid%topm)  )    deallocate (grid%topm)
    if (associated(grid%topma) )    deallocate (grid%topma)
    if (associated(grid%topta) )    deallocate (grid%topta)
    if (associated(grid%rtgt)  )    deallocate (grid%rtgt)
    if (associated(grid%rtgu)  )    deallocate (grid%rtgu)
    if (associated(grid%rtgv)  )    deallocate (grid%rtgv)
    if (associated(grid%rtgm)  )    deallocate (grid%rtgm)
    if (associated(grid%f13t)  )    deallocate (grid%f13t)
    if (associated(grid%f13u)  )    deallocate (grid%f13u)
    if (associated(grid%f13v)  )    deallocate (grid%f13v)
    if (associated(grid%f13m)  )    deallocate (grid%f13m)
    if (associated(grid%f23t)  )    deallocate (grid%f23t)
    if (associated(grid%f23u)  )    deallocate (grid%f23u)
    if (associated(grid%f23v)  )    deallocate (grid%f23v)
    if (associated(grid%f23m)  )    deallocate (grid%f23m)
    if (associated(grid%dxt)   )    deallocate (grid%dxt)
    if (associated(grid%dxu)   )    deallocate (grid%dxu)
    if (associated(grid%dxv)   )    deallocate (grid%dxv)
    if (associated(grid%dxm)   )    deallocate (grid%dxm)
    if (associated(grid%dyt)   )    deallocate (grid%dyt)
    if (associated(grid%dyu)   )    deallocate (grid%dyu)
    if (associated(grid%dyv)   )    deallocate (grid%dyv)
    if (associated(grid%dym)   )    deallocate (grid%dym)
    if (associated(grid%fmapt) )    deallocate (grid%fmapt)
    if (associated(grid%fmapu) )    deallocate (grid%fmapu)
    if (associated(grid%fmapv) )    deallocate (grid%fmapv)
    if (associated(grid%fmapm) )    deallocate (grid%fmapm)
    if (associated(grid%fmapti))    deallocate (grid%fmapti)
    if (associated(grid%fmapui))    deallocate (grid%fmapui)
    if (associated(grid%fmapvi))    deallocate (grid%fmapvi)
    if (associated(grid%fmapmi))    deallocate (grid%fmapmi)
    if (associated(grid%glat)  )    deallocate (grid%glat)
    if (associated(grid%glon)  )    deallocate (grid%glon)
    if (associated(grid%topzo) )    deallocate (grid%topzo)
    
    if (associated(grid%aru)   )    deallocate (grid%aru) 
    if (associated(grid%arv)   )    deallocate (grid%arv) 
    if (associated(grid%arw)   )    deallocate (grid%arw)
    if (associated(grid%volu)  )    deallocate (grid%volu)
    if (associated(grid%volv)  )    deallocate (grid%volv)
    if (associated(grid%volw)  )    deallocate (grid%volw)
    if (associated(grid%volt)  )    deallocate (grid%volt)
    if (associated(grid%lpu)   )    deallocate (grid%lpu) 
    if (associated(grid%lpv)   )    deallocate (grid%lpv) 
    if (associated(grid%lpw)   )    deallocate (grid%lpw)

   return
   end subroutine


subroutine filltab_grid(grid,gridm,imean,n1,n2,n3,ng)

!use var_tables
!
!   implicit none
!   type (grid_vars) :: grid,gridm
!   integer, intent(in) :: imean,n1,n2,n3,ng
!   integer :: npts
!   real, pointer :: var,varm

! Fill pointers to arrays into variable tables
!
!   npts=n2*n3
!   if (associated(grid%topt)) &
!      call vtables2 (grid%topt(1,1),gridm%topt(1,1),ng,npts,imean,  &
!		  'TOPT :2:hist:anal:mpti')	 
!   if (associated(grid%topu)) &
!      call vtables2 (grid%topu(1,1),gridm%topu(1,1),ng, npts, imean,  &
!		  'TOPU :2:mpti')      
!   if (associated(grid%topv)) &
!      call vtables2 (grid%topv(1,1),gridm%topv(1,1),ng, npts, imean,  &
!		  'TOPV :2:mpti')      
!   if (associated(grid%topm)) &
!      call vtables2 (grid%topm(1,1),gridm%topm(1,1),ng, npts, imean,  &
!		  'TOPM :2:mpti')      
!   if (associated(grid%topma)) &
!      call vtables2 (grid%topma(1,1),gridm%topma(1,1),ng, npts, imean,  &
!		  'TOPMA :2:hist:anal:mpti')	  
!   if (associated(grid%topta)) &
!      call vtables2 (grid%topta(1,1),gridm%topta(1,1),ng, npts, imean,  &
!		  'TOPTA :2:hist:anal:mpti')	  
!   if (associated(grid%rtgt)) &
!      call vtables2 (grid%rtgt(1,1),gridm%rtgt(1,1),ng, npts, imean,  &
!		  'RTGT :2:mpti')      
!   if (associated(grid%rtgu)) &
!      call vtables2 (grid%rtgu(1,1),gridm%rtgu(1,1),ng, npts, imean,  &
!		  'RTGU :2:mpti')      
!   if (associated(grid%rtgv)) &
!      call vtables2 (grid%rtgv(1,1),gridm%rtgv(1,1),ng, npts, imean,  &
!		  'RTGV :2:mpti')      
!   if (associated(grid%rtgm)) &
!      call vtables2 (grid%rtgm(1,1),gridm%rtgm(1,1),ng, npts, imean,  &
!		  'RTGM :2:mpti')      
!   if (associated(grid%f13t)) &
!      call vtables2 (grid%f13t(1,1),gridm%f13t(1,1),ng, npts, imean,  &
!		  'F13T :2:mpti')      
!   if (associated(grid%f13u)) &
!      call vtables2 (grid%f13u(1,1),gridm%f13u(1,1),ng, npts, imean,  &
!		  'F13U :2:mpti')      
!   if (associated(grid%f13v)) &
!      call vtables2 (grid%f13v(1,1),gridm%f13v(1,1),ng, npts, imean,  &
!		  'F13V :2:mpti')      
!   if (associated(grid%f13m)) &
!      call vtables2 (grid%f13m(1,1),gridm%f13m(1,1),ng, npts, imean,  &
!		  'F13M :2:mpti')      
!   if (associated(grid%f23t)) &
!      call vtables2 (grid%f23t(1,1),gridm%f23t(1,1),ng, npts, imean,  &
!		  'F23T :2:mpti')      
!   if (associated(grid%f23u)) &
!      call vtables2 (grid%f23u(1,1),gridm%f23u(1,1),ng, npts, imean,  &
!		  'F23U :2:mpti')      
!   if (associated(grid%f23v)) &
!      call vtables2 (grid%f23v(1,1),gridm%f23v(1,1),ng, npts, imean,  &
!		  'F23V :2:mpti')      
!   if (associated(grid%f23m)) &
!      call vtables2 (grid%f23m(1,1),gridm%f23m(1,1),ng, npts, imean,  &
!		  'F23M :2:mpti')      
!   if (associated(grid%dxt)) &
!      call vtables2 (grid%dxt(1,1),gridm%dxt(1,1),ng, npts, imean,  &
!		  'DXT :2:mpti')      
!   if (associated(grid%dxu)) &
!      call vtables2 (grid%dxu(1,1),gridm%dxu(1,1),ng, npts, imean,  &
!		  'DXU :2:mpti')      
!   if (associated(grid%dxv)) &
!      call vtables2 (grid%dxv(1,1),gridm%dxv(1,1),ng, npts, imean,  &
!		  'DXV :2:mpti')      
!   if (associated(grid%dxm)) &
!      call vtables2 (grid%dxm(1,1),gridm%dxm(1,1),ng, npts, imean,  &
!		  'DXM :2:mpti')      
!   if (associated(grid%dyt)) &
!      call vtables2 (grid%dyt(1,1),gridm%dyt(1,1),ng, npts, imean,  &
!		  'DYT :2:mpti')      
!   if (associated(grid%dyu)) &
!      call vtables2 (grid%dyu(1,1),gridm%dyu(1,1),ng, npts, imean,  &
!		  'DYU :2:mpti')      
!   if (associated(grid%dyv)) &
!      call vtables2 (grid%dyv(1,1),gridm%dyv(1,1),ng, npts, imean,  &
!		  'DYV :2:mpti')      
!   if (associated(grid%dym)) &
!      call vtables2 (grid%dym(1,1),gridm%dym(1,1),ng, npts, imean,  &
!		  'DYM :2:mpti')      
!   if (associated(grid%fmapt)) &
!      call vtables2 (grid%fmapt(1,1),gridm%fmapt(1,1),ng, npts, imean,  &
!		  'FMAPT :2:mpti')	
!   if (associated(grid%fmapu)) &
!      call vtables2 (grid%fmapu(1,1),gridm%fmapu(1,1),ng, npts, imean,  &
!		  'FMAPU :2:mpti')	
!   if (associated(grid%fmapv)) &
!      call vtables2 (grid%fmapv(1,1),gridm%fmapv(1,1),ng, npts, imean,  &
!		  'FMAPV :2:mpti')	
!   if (associated(grid%fmapm)) &
!      call vtables2 (grid%fmapm(1,1),gridm%fmapm(1,1),ng, npts, imean,  &
!		  'FMAPM :2:mpti')	
!   if (associated(grid%fmapti)) &
!      call vtables2 (grid%fmapti(1,1),gridm%fmapti(1,1),ng, npts, imean,  &
!		  'FMAPTI :2:mpti')	 
!   if (associated(grid%fmapui)) &
!      call vtables2 (grid%fmapui(1,1),gridm%fmapui(1,1),ng, npts, imean,  &
!		  'FMAPUI :2:mpti')	 
!   if (associated(grid%fmapvi)) &
!      call vtables2 (grid%fmapvi(1,1),gridm%fmapvi(1,1),ng, npts, imean,  &
!		  'FMAPVI :2:mpti')	 
!   if (associated(grid%fmapmi)) &
!      call vtables2 (grid%fmapmi(1,1),gridm%fmapmi(1,1),ng, npts, imean,  &
!		  'FMAPMI :2:mpti')	 
!   if (associated(grid%glat)) &
!      call vtables2 (grid%glat(1,1),gridm%glat(1,1),ng, npts, imean,  &
!		  'GLAT :2:mpti:anal')      
!   if (associated(grid%glon)) &
!      call vtables2 (grid%glon(1,1),gridm%glon(1,1),ng, npts, imean,  &
!		  'GLON :2:mpti:anal')      
!   if (associated(grid%topzo)) &
!      call vtables2 (grid%topzo(1,1),gridm%topzo(1,1),ng, npts, imean,  &
!		  'TOPZO :2:mpti')	
!
!   npts=n2*n3
!   if (associated(grid%lpu)) &
!      call vtables2 (grid%lpu(1,1),gridm%lpu(1,1),ng,npts,imean,  &
!		  'LPU :2:mpti')      
!   if (associated(grid%lpv)) &
!      call vtables2 (grid%lpv(1,1),gridm%lpv(1,1),ng,npts,imean,  &
!		  'LPV :2:mpti')      
!   if (associated(grid%lpw)) &
!      call vtables2 (grid%lpw(1,1),gridm%lpw(1,1),ng,npts,imean,  &
!		  'LPW :2:mpti')      
!
!   npts=n1*n2*n3
!   if (associated(grid%aru)) &
!      call vtables2 (grid%aru(1,1,1),gridm%aru(1,1,1),ng,npts,imean,  &
!		  'ARU :3:mpti')      
!   if (associated(grid%arv)) &
!      call vtables2 (grid%arv(1,1,1),gridm%arv(1,1,1),ng,npts,imean,  &
!		  'ARV :3:mpti')      
!   if (associated(grid%arw)) &
!      call vtables2 (grid%arw(1,1,1),gridm%arw(1,1,1),ng,npts,imean,  &
!		  'ARW :3:mpti')      
!
!   if (associated(grid%volu)) &
!      call vtables2 (grid%volu(1,1,1),gridm%volu(1,1,1),ng,npts,imean,  &
!		  'VOLU :3:mpti')
!   if (associated(grid%volv)) &
!      call vtables2 (grid%volv(1,1,1),gridm%volv(1,1,1),ng,npts,imean,  &
!		  'VOLV :3:mpti')
!   if (associated(grid%volw)) &
!      call vtables2 (grid%volw(1,1,1),gridm%volw(1,1,1),ng,npts,imean,  &
!		  'VOLW :3:mpti')
!   if (associated(grid%volt)) &
!      call vtables2 (grid%volt(1,1,1),gridm%volt(1,1,1),ng,npts,imean,  &
!                 'VOLT :3:anal:mpti')
               
                 
                 
                    
   return
   end subroutine

End Module mem_grid
