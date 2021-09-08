module write_gocart_bg_mod

   ! Reset default behavior
   implicit none
   private

   ! Public routines
   public :: write_gocart_bg_driver

   ! Parameters
   integer,parameter :: DMS_OPTION = 1
   integer,parameter :: EROD_OPTION = 2
   integer,parameter :: GENERAL_OPTION = 3
   integer,parameter :: MAX_NUM_OPTIONS = 3 ! Increase if more options added

contains

   subroutine write_gocart_bg_driver(file_in,ng,ihour,iday,imon,iyear,iunit)
      
      ! Arguments
      character(len=*),intent(in) :: file_in
      integer,intent(in) :: ng
      integer,intent(in) :: ihour
      integer,intent(in) :: iday
      integer,intent(in) :: imon
      integer,intent(in) :: iyear
      integer,intent(in) :: iunit

      ! Local variables
      character(len=132) :: ctlfile
      character(len=132) :: binfile
      character(len=132) :: mapfile
      character(len=132) :: wfln(2)
      character(len=110),allocatable :: varstrings(:)
      integer :: xdim,ydim
      integer :: nvert
      integer :: nvars
      integer :: ioption
      real,allocatable :: data4d(:,:,:,:)
      integer :: bytes_in_float = 4 ! On Discover

      do ioption = 1, MAX_NUM_OPTIONS
         nvert = get_nvert(ioption)
!KY         call build_filenames(file_in,ctlfile,binfile,mapfile,ioption)
         call build_ctl_varstrings(nvars,varstrings,ioption)
         wfln(1) = trim(binfile)
         wfln(2) = trim(ctlfile)
         call write_ctl_3d(ng,iunit,wfln,ihour,iday,imon,iyear,nvert, &
              nvars,varstrings)
         deallocate(varstrings)
         call collect_gocart_data(ioption,ng,imon,nvert,xdim,ydim,nvars,data4d)
         call write_grads(iunit,binfile,xdim,ydim,nvert,nvars,data4d,&
              bytes_in_float)
         deallocate(data4d)
         call write_maproj(ng,iunit,mapfile)
      end do
      return
   end subroutine write_gocart_bg_driver

   function get_nvert(ioption) result (nvert)

      ! Modules
      use gocart_backgr, only: nspecies, spc_name, nlevels_netcdf
           
      ! Arguments
      integer, intent(in) :: ioption

      ! Return variable
      integer :: nvert
      integer :: ispc

      nvert = 0
      if (ioption == DMS_OPTION) then
         nvert = 1
      else if (ioption == EROD_OPTION) then
         do ispc = 1, nspecies
            if (trim(spc_name(ispc)) == "EROD" ) then
               nvert = nlevels_netcdf(ispc)
               exit
            end if
         end do
      else if (ioption == GENERAL_OPTION) then
         do ispc = 1, nspecies           
            if (trim(spc_name(ispc)) == "DMS") cycle
            if (trim(spc_name(ispc)) == "EROD") cycle            
            nvert = nlevels_netcdf(ispc)
            exit
         end do
      else
         print*,'ERROR, invalid option selected for GOCART BG data!'
         stop
      end if
   
      if (nvert == 0) then
         print*,'Internal error in get_nvert!'
         stop
      end if

      return
   end function get_nvert

   subroutine build_filenames(file_in,ctlfile,binfile,mapfile,option)
      
      ! Arguments
      character(len=132),intent(in) :: file_in
      character(len=132),intent(out) :: ctlfile
      character(len=132),intent(out) :: binfile
      character(len=132),intent(out) :: mapfile
      integer,intent(in) :: option

      ! Local variables
      character(len=20) :: abbrev

      if (option == DMS_OPTION) then
         abbrev = '-DMS'
      else if (option == EROD_OPTION) then
         abbrev = '-EROD'
      else if (option == GENERAL_OPTION) then
         abbrev = '-gocartBG'
      else
         print*,'ERROR, invalid option selected for GOCART BG data!'
         stop
      end if
      
      ctlfile = file_in(1:len_trim(file_in)-4)//trim(abbrev)//'.ctl'
      binfile = file_in(1:len_trim(file_in)-4)//trim(abbrev)//'.gra'
      mapfile = file_in(1:len_trim(file_in)-4)//trim(abbrev)//'.map'

      return
   end subroutine build_filenames

   function get_nvars(ioption) result (nvars)

      ! Modules
      use gocart_backgr, only: nspecies

      ! Arguments
      integer,intent(in) :: ioption

      ! Return variable
      integer :: nvars

      ! Local variables
      integer :: ispc

      ! Allocate and define varstrings depending on which gocart BG fields
      ! are requested
      if (ioption == DMS_OPTION) then ! Special case for DMS
         nvars = 1
      else if (ioption == EROD_OPTION) then ! Special case for EROD
         nvars = 1
      else if (ioption == GENERAL_OPTION) then ! General case
         nvars = nspecies - 2
      else
         print*,'ERROR, invalid option selected for GOCART BG data!'
         stop
      end if

      return
   end function get_nvars

   subroutine build_ctl_varstrings(nvars,varstrings,option)

      ! Modules
      use gocart_backgr, only: nspecies, spc_name

      ! Arguments
      integer,intent(out) :: nvars
      character(len=110),allocatable,intent(inout) :: varstrings(:)
      integer, intent(in) :: option

      ! Local variables
      character(len=10) :: name
      character(len=40) :: source
      character(len=8) :: units
      integer :: ispc
      integer :: i

      ! Allocate and define varstrings depending on which gocart BG fields
      ! are requested
      nvars = get_nvars(option)

      allocate(varstrings(nvars))

      source = 'gocart'

      if (option == DMS_OPTION) then
         name = 'DMS' 
         units = 'nmol/dm3'
         write(varstrings(1),1001) name(1:len_trim(name))//'_'//source, 0, &
              source,trim(units)
      else if (option == EROD_OPTION) then
         name = 'EROD' 
         units = 'm2'
         write(varstrings(1),1002) name(1:len_trim(name))//'_'//source, 0, &
                 source,trim(units)
      else if (option == GENERAL_OPTION) then
         i = 0
         do ispc = 1, nspecies
            if (trim(spc_name(ispc)) == "DMS") cycle
            if (trim(spc_name(ispc)) == "EROD") cycle

            if (trim(spc_name(ispc)) == "PS") then
               units = 'Pa'
            else
               units = 'm3/m3'
            endif

            i = i + 1
            write(varstrings(i),1003) &
                 spc_name(ispc)(1:len_trim(spc_name(ispc)))//'_'//source, 0, &
                 source,trim(units)
         end do
      end if

      1001 format (a15,i4,' 99    - DMS seawater concentration : ',a10, &
                '[',a,']')
      1002 format (a15,i4,' 99    - EROD erosion area          : ',a10, &
                '[',a,']')
      1003 format (a15,i4,' 99    - Background data            : ',a10, &
                '[',a,']')

      return
   end subroutine build_ctl_varstrings

   ! EMK...Write GrADS control file for 3D data.  Based on subroutine write_ctl
   subroutine write_ctl_3d(ng,iunit,wfln,ihour,iday,imon,iyear,nvert, &
        nvars,varstrings)

      ! Modules
      use grid_dims_out

      ! Arguments
      integer,intent(in) :: ng
      integer,intent(in) :: iunit
      character(len=*),intent(in) :: wfln(2)
      integer,intent(in) :: ihour
      integer,intent(in) :: iday
      integer,intent(in) :: imon
      integer,intent(in) :: iyear
      integer,intent(in) :: nvert
      integer,intent(in) :: nvars
      character(len=*),intent(in) :: varstrings(nvars)

      ! Local variables
      character(len=15) :: chdate, xchstep
      integer :: imin
      integer :: i

      ! Local parameters
      character(len=3), parameter :: cmo(12) = &
           (/'jan','feb','mar','apr','may','jun', &
             'jul','aug','sep','oct','nov','dec'/)

      ! Set GrADS date/time string
      imin=0
      chdate='00:00z00mmm1900'
      write(chdate(1:2),'(i2.2)') ihour
      write(chdate(4:5),'(i2.2)') imin
      write(chdate(7:8),'(i2.2)') iday
      chdate(9:11)=cmo(imon)(1:3)
      write(chdate(12:15),'(i4.2)') iyear
      xchstep='          1dy'

      ! Write the control file
      open(iunit,file=trim(wfln(2)),status='unknown')
      write(iunit,2001) '^'//trim(wfln(1)(1:len_trim(wfln(1))))
      write(iunit,2002) 'undef -9.99e33'
      write(iunit,2002) 'title 3D GOCART Background Field'
      write(iunit,2003) nxb(ng)-nxa(ng)+1,(dep_glon(i,ng),i=1,2)
      write(iunit,2004) nyb(ng)-nya(ng)+1,(dep_glat(i,ng),i=1,2)
      write(iunit,2005) nvert,0.0
      write(iunit,2006) 1,chdate,xchstep   
      write(iunit,2007) nvars
      do i = 1,nvars
         write(iunit,2002) trim(varstrings(i))
      end do
      write(iunit,2002) 'endvars'
      close(iunit)

      ! Format statements
2001  format('dset ',a)
2002  format(a)
2003  format('xdef ',i4,' linear ',2f15.3)
2004  format('ydef ',i4,' linear ',2f15.3)
2005  format('zdef ',i4,' levels ',60f10.1)
2006  format('tdef ',i4,' linear ',2a15)
2007  format('vars ',i4)

      return
   end subroutine write_ctl_3d

   subroutine collect_gocart_data(ioption,ng,imon,nvert,xdim,ydim,nvars,data4d)

      ! Modules
      use grid_dims_out
      use gocart_backgr, only: nspecies, spc_name, gocart_bg_g, nlevels_netcdf
      
      ! Arguments
      integer,intent(in) :: ioption
      integer,intent(in) :: ng
      integer,intent(in) :: imon
      integer,intent(in) :: nvert
      integer,intent(out) :: xdim
      integer,intent(out) :: ydim
      integer,intent(out) :: nvars
      real,allocatable,intent(out) :: data4d(:,:,:,:)

      ! Local variables
      integer :: i,j,k,n
      integer :: ispc

      xdim = nxb(ng)-nxa(ng)+1
      ydim = nyb(ng)-nya(ng)+1
      nvars = get_nvars(ioption)

      allocate(data4d(xdim,ydim,nvert,nvars))
      data4d(:,:,:,:) = 0

      if (ioption == DMS_OPTION) then
         if (nvert .ne. 1) then
            print*,'Internal error, nvert not 1 for DMS'
            print*,'nvert = ',nvert
            stop
         endif
         do ispc = 1, nspecies
            if (trim(spc_name(ispc)) == "DMS" ) then
               data4d(:,:,1,1) = gocart_bg_g(ispc)%src(:,:,imon)
               exit
            end if
         end do
      else if (ioption == EROD_OPTION) then
         if (nvert .ne. 3) then
            print*,'Internal error, nvert not 3 for EROD'
            print*,'nvert = ',nvert
            stop
         endif
         do ispc = 1, nspecies
            if (trim(spc_name(ispc)) == "EROD" ) then
               if (nvert .ne. nlevels_netcdf(ispc)) then
                  print*,'ERROR, vertical dimension mismatch'
                  print*,'nvert = ',nvert
                  print*,'nlevels_netcdf(ispc) = ',nlevels_netcdf(ispc)
                  stop
               end if
               data4d(:,:,1:nvert,1) = &
                    gocart_bg_g(ispc)%src(:,:,1:nvert)
               exit
            end if
         end do
      else if (ioption == GENERAL_OPTION) then
         i = 0
         do ispc = 1, nspecies
            if (trim(spc_name(ispc)) == "DMS") cycle
            if (trim(spc_name(ispc)) == "EROD") cycle

            if (nvert .ne. nlevels_netcdf(ispc)) then
               print*,'ERROR, vertical dimension mismatch'
               print*,'nvert = ',nvert
               print*,'nlevels_netcdf(ispc) = ',nlevels_netcdf(ispc)
               stop
            end if

            i = i + 1
            data4d(:,:,1:nvert,i) = &
                 gocart_bg_g(ispc)%src(:,:,1:nvert)
         end do
      else
         print*,'ERROR, invalid option for GOCART BG data!'
         stop
      end if
         
      return
   end subroutine collect_gocart_data

   subroutine write_grads(iunit,filename,xdim,ydim,zdim,nvars,data4d,&
        bytes_in_float)
      
      ! Arguments
      integer,intent(in) :: iunit
      character(len=*),intent(in) :: filename
      integer,intent(in) :: xdim
      integer,intent(in) :: ydim
      integer,intent(in) :: zdim
      integer,intent(in) :: nvars
      real,intent(in) :: data4d(xdim,ydim,zdim,nvars)
      integer,intent(in) :: bytes_in_float

      ! Local variables
      integer :: output_byte_size
      integer :: nrec
      integer :: i,j,k,n

      inquire(iolength=output_byte_size) bytes_in_float

      open(iunit,file=trim(filename),form="unformatted",access="direct", &
           status="unknown",recl=output_byte_size*xdim*ydim)

      nrec = 0
      do n = 1,nvars
         do k = 1,zdim
            nrec = nrec + 1
            write(iunit,rec=nrec) ((data4d(i,j,k,n),i=1,xdim),j=1,ydim)
         end do
      end do

      close(iunit)

      return
   end subroutine write_grads

end module write_gocart_bg_mod
