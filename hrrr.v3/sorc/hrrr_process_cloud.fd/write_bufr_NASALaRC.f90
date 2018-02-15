  subroutine write_bufr_NASALaRC(nlon,nlat,dx,index,w_pcld,w_tcld,w_frac,w_lwp,nlev_cld)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   write_bufr_NASALaRC 
!   prgmmr: hu           org: essl/gsd                date: 2008-12-01
!   
! abstract: write NASA LaRC in RR grid into bufr
!   
! program history log:
!   2009-09-18  Hu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  linux 
!
!$$$
    use constants, only: zero, one
    use kinds, only: r_kind,i_kind,r_single
    implicit none

!
    integer(i_kind), intent(in) :: nlon,nlat
    integer, intent(in)  ::   index(nlon,nlat)
    REAL(r_single), intent(in) ::   w_pcld(nlon,nlat)
    REAL(r_single), intent(in) ::   w_tcld(nlon,nlat)
    REAL(r_single), intent(in) ::   w_frac(nlon,nlat)
    REAL(r_single), intent(in) ::   w_lwp (nlon,nlat)
    REAL,           intent(in) ::   dx
    INTEGER(i_kind),   intent(in) ::   nlev_cld(nlon,nlat)

    real(r_kind) :: hdr(5),obs(1,5)
    character(80):: hdrstr='SID XOB YOB DHR TYP'
    character(80):: obsstr='POB'

    REAL(i_kind),PARAMETER ::  MXBF = 160000_i_kind
    INTEGER(i_kind) :: ibfmsg = MXBF/4_i_kind

    character(8) subset,sid
    integer(i_kind) :: ludx,lendian_in,idate

    INTEGER(i_kind)  ::  maxlvl, numref
    INTEGER(i_kind)  ::  i,j,n,k,iret


    open(12,file='nasaLaRC_cycle_date')
      read(12,*) idate
    close(12)
    write(6,*) 'cycle time is :', idate
!mhu    idate=2008120100
    subset='ADPUPA'
    sid='NASALaRC'
    ludx=22
    lendian_in=10

    open(ludx,file='prepobs_prep.bufrtable',action='read')
    open(lendian_in,file='NASALaRCCloudInGSI.bufr',action='write',form='unformatted')

    call datelen(10)
    call openbf(lendian_in,'OUT',ludx)
    maxlvl=5
    numref=0
!mhu    do j=1,nlat
!mhu    do i=1,nlon
! GSI has peroidic boundary which can crash the cloud analysis if there are data along the boundary.
! wait for GSI to fix this issue.
    do j=2,nlat-1
    do i=2,nlon-1
      if((index(i,j) .ge. 3) .or.  &
         (index(i,j) .ge. 1 .and. dx < 4000.0) ) then
        numref = numref + 1
        hdr(1)=transfer(sid,hdr(1))
        hdr(2)=float(i)/10.0_r_kind
        hdr(3)=float(j)/10.0_r_kind
        hdr(4)=0
        hdr(5)=500

        if( w_pcld(i,j) > 88888.0 .or. w_pcld(i,j) < -0.001) then
          obs(1,1)=9999.0
        else
          obs(1,1)=w_pcld(i,j)
        endif
        if( w_tcld(i,j) > 88888.0 .or. w_tcld(i,j) < -0.001) then
          obs(1,2)=9999.0
        else
          obs(1,2)=w_tcld(i,j)
        endif
        if( w_frac(i,j) > 88888.0 .or. w_frac(i,j) < -0.001) then
          obs(1,3)=9999.0
        else
          obs(1,3)=w_frac(i,j)*100.0
        endif
        if( w_lwp(i,j) > 88888.0 .or. w_lwp(i,j) < -0.001) then
          obs(1,4)=9999.0
        else
          obs(1,4)=w_lwp(i,j)*1000.0
        endif
        if( nlev_cld(i,j) > 88888.0 .or. nlev_cld(i,j) < -0.001) then
          obs(1,5)=9999.0
        else
          obs(1,5)=float(nlev_cld(i,j))
        endif


        call openmb(lendian_in,subset,idate)
        call ufbint(lendian_in,hdr,5,   1,iret,hdrstr)
        call ufbint(lendian_in,obs,1,maxlvl,iret,obsstr)
        call writsb(lendian_in,ibfmsg,iret)
      endif
    enddo   !i
    enddo   !j
    call closbf(lendian_in)
    write(6,*) 'write_bufr_nasaLaRC, DONE: write columns:',numref

end subroutine  write_bufr_NASALaRC
