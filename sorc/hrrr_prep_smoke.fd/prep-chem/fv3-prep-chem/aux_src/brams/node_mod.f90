!############################# Change Log ##################################
! 5.0.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000, 2003 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!###########################################################################


module node_mod

  use grid_dims, only : maxgrds,maxmach

  !---------------------------------------------------------------------------
  integer :: mxp,myp,mzp,ia,iz,ja,jz,i0,j0,master_num,mchnum,ibcon,ipara  &
       ,ia_1,ia_2,ia_3,ia1,ia2,ia3,ja_1,ja_2,ja_3,ja1,ja2,ja3  &
       ,iz_1,iz_2,iz_3,iz1,iz2,iz3,jz_1,jz_2,jz_3,jz1,jz2,jz3  &
       ,izu,jzv,mynum
  !---------------------------------------------------------------------------
  integer, target, dimension(maxgrds) :: mmxp,mmyp,mmzp
  integer, dimension(maxgrds) :: mia,miz,mja,mjz  &
       ,mi0,mj0,mibcon,mnestflg,mfeednode
  !---------------------------------------------------------------------------
  integer                                 :: nmachs,load_bal
  integer, dimension(maxmach)             :: machs
  integer, dimension(maxmach,maxgrds)     :: nodemxp,nodemyp,nodemzp  &
       ,nodeia,nodeiz,nodeja,nodejz  &
       ,nodei0,nodej0,nodeibcon  &
       ,nodenestflg,nodefeednode
  integer, dimension(maxmach,maxgrds,4)   :: nodeconn

  integer, dimension(maxgrds,8)           :: nodebounds ! Reprod.-Saulo Barros

  integer, dimension(5,7,maxgrds,maxmach) :: ipaths
  integer, dimension(6,maxgrds,maxmach)   :: iget_paths
  !---------------------------------------------------------------------------
  integer                       :: newbuff_feed,nbuff_feed,newbuff_nest  &
       ,nbuff_nest
  !---------------------------------------------------------------------------
  integer, dimension(maxmach) :: irecv_req,isend_req
  !---------------------------------------------------------------------------

  type lbc_buffs
     real, pointer :: lbc_send_buff(:),lbc_recv_buff(:)
     integer :: nsend,nrecv
  end type lbc_buffs

  type (lbc_buffs) :: node_buffs(maxmach)

end module node_mod
