! module_nwp.f90
! Eric James
! NOAA/OAR/ESRL/GSD/ADB
! 12 Dec 2018
!
! This module defines HRRR and FVCOM forecast data structure and the method to
! read and write observations from and to those data structures.  It is used by
! process_FVCOM.f90.

module module_nwp

   use kinds, only: r_kind, r_single, i_short, rmissing
   use module_nwp_base, only: nwpbase
   use module_map_utils, only: map_util
   use module_ncio, only: ncio

   implicit none

   public :: fcst_nwp
   public :: nwp_type

   private
   type :: nwp_type
      character(len=5) :: datatype
      integer :: numvar, xlat, xlon, xtime
      integer :: i_mask, i_sst, i_ice, i_sfcT
      character(len=20), allocatable :: varnames(:)
      character(len=20), allocatable :: latname
      character(len=20), allocatable :: lonname
      character(len=20), allocatable :: dimnameEW
      character(len=20), allocatable :: dimnameNS
      character(len=20), allocatable :: dimnameTIME
      real(r_single), allocatable :: nwp_mask(:,:,:)
      real(r_single), allocatable :: nwp_sst(:,:,:)
      real(r_single), allocatable :: nwp_ice(:,:,:)
      real(r_single), allocatable :: nwp_sfcT(:,:,:)
   end type nwp_type

   type, extends(nwp_type) :: fcst_nwp
      type(nwpbase), pointer :: head => NULL()
      type(nwpbase), pointer :: tail => NULL()
      contains
         procedure :: initial => initial_nwp
         procedure :: list_initial => list_initial_nwp
         procedure :: read_n => read_nwp
         procedure :: finish => finish_nwp
   end type fcst_nwp

   type(ncio) :: ncdata
   type(map_util) :: map

   contains

      subroutine initial_nwp(this,itype)

!        This subroutine defines the number of variables and their names for
!        each NWP data type.  The indices of the variables are
!        also defined for later reference.

         class(fcst_nwp) :: this

         character(len=5), intent(in) :: itype

!        FVCOM grid

         if (itype=='FVCOM') then
            this%datatype = itype
            this%numvar = 3

            this%i_mask = 1
            this%i_sst = 2
            this%i_ice = 3
            this%i_sfcT = 0

            allocate(this%varnames(this%numvar))
            this%varnames(1) = 'LAKEMASK'
            this%varnames(2) = 'tsfc'
            this%varnames(3) = 'aice'

            allocate(this%latname)
            allocate(this%lonname)
            this%latname = 'XLAT_M'
            this%lonname = 'XLONG_M'

            allocate(this%dimnameEW)
            allocate(this%dimnameNS)
            allocate(this%dimnameTIME)
            this%dimnameEW = 'west_east'
            this%dimnameNS = 'south_north'
            this%dimnameTIME = 'time'

!        HRRR grid

         else if (itype==' HRRR') then
            this%datatype = itype
            this%numvar = 4

            this%i_mask = 1
            this%i_sst = 2
            this%i_ice = 3
            this%i_sfcT = 4

            allocate(this%varnames(this%numvar))
            this%varnames(1) = 'LANDMASK'
            this%varnames(2) = 'SST'
            this%varnames(3) = 'SEAICE'
            this%varnames(4) = 'TSK'

            allocate(this%latname)
            allocate(this%lonname)
            this%latname = 'XLAT'
            this%lonname = 'XLONG'

            allocate(this%dimnameEW)
            allocate(this%dimnameNS)
            allocate(this%dimnameTIME)
            this%dimnameEW = 'west_east'
            this%dimnameNS = 'south_north'
            this%dimnameTIME = 'Time'

!        If the data type does not match one of the known types, exit.

         else
            write(*,*) 'Unknown data type:', itype
            stop 1234
         end if

         this%head => NULL()
         this%tail => NULL()

         write(*,*) 'Finished initial_nwp'
         write(*,*) ' '

      end subroutine initial_nwp

      subroutine list_initial_nwp(this)

!        This subroutine lists the setup for NWP data that was done by
!        the initial_nwp subroutine.

         class(fcst_nwp) :: this

         integer :: k

         write(*,*) 'List initial setup for ', this%datatype
         write(*,*) 'number of variables ', this%numvar
         write(*,*) 'variable index: mask, sst, ice, sfcT'
         write(*,'(15x,10I3)') this%i_mask, this%i_sst, this%i_ice, &
      &      this%i_sfcT
         write(*,*) 'variable name:'
         do k=1,this%numvar
            write(*,*) k,trim(this%varnames(k))
         enddo

         write(*,*) 'Finished list_initial_nwp'
         write(*,*) ' '

      end subroutine list_initial_nwp

      subroutine read_nwp(this,filename,itype,numlon,numlat,numtimes,time_to_get,mask,sst,ice,sfcT)

!        This subroutine initializes arrays to receive the NWP data,
!        and opens the file and gets the data.

         class(fcst_nwp) :: this

         character(len=5), intent(in) :: itype
         character(len=*), intent(in) :: filename

         integer, intent(in) :: time_to_get
         integer, intent(inout) :: numlon, numlat, numtimes
         real(r_single), intent(inout) :: mask(:,:), sst(:,:), ice(:,:), sfcT(:,:)

!        Open the file using module_ncio.f90 code, and find the number of
!        lat/lon points

         call ncdata%open(trim(filename),'r',200)
         call ncdata%get_dim(this%dimnameEW,this%xlon)
         call ncdata%get_dim(this%dimnameNS,this%xlat)
         call ncdata%get_dim(this%dimnameTIME,this%xtime)

         write(*,*) 'number of longitudes for file ', filename, this%xlon
         numlon = this%xlon
         write(*,*) 'number of latitudes for file ', filename, this%xlat
         numlat = this%xlat
         write(*,*) 'number of times for file ', filename, this%xtime
         numtimes = this%xtime

!        Allocate all the arrays to receive data

         allocate(this%nwp_mask(this%xlon,this%xlat,this%xtime))
         allocate(this%nwp_sst(this%xlon,this%xlat,this%xtime))
         allocate(this%nwp_ice(this%xlon,this%xlat,this%xtime))
         allocate(this%nwp_sfcT(this%xlon,this%xlat,this%xtime))

!        Get variables from the data file, but only if the variable is
!        defined for that data type.

         if (this%i_mask .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_mask),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_mask)
            mask = this%nwp_mask(:,:,1)
         end if
         if (this%i_sst .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_sst),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_sst)
            sst = this%nwp_sst(:,:,time_to_get)
         end if
         if (this%i_ice .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_ice),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_ice)
            ice = this%nwp_ice(:,:,time_to_get)
         end if
         if (this%i_sfcT .gt. 0) then
            call ncdata%get_var(this%varnames(this%i_sfcT),this%xlon,  &
                                this%xlat,this%xtime,this%nwp_sfcT)
            sfcT = this%nwp_sfcT(:,:,time_to_get)
         end if

!        Close the netCDF file.

         call ncdata%close

         write(*,*) 'Finished read_nwp'
         write(*,*) ' '

      end subroutine read_nwp

      subroutine finish_nwp(this)

         class(fcst_nwp) :: this

         type(nwpbase), pointer :: thisobs,thisobsnext

         deallocate(this%varnames)
         deallocate(this%latname)
         deallocate(this%lonname)
         deallocate(this%dimnameEW)
         deallocate(this%dimnameNS)
         deallocate(this%dimnameTIME)
         deallocate(this%nwp_mask)
         deallocate(this%nwp_sst)
         deallocate(this%nwp_ice)
         deallocate(this%nwp_sfcT)

         thisobs => this%head
         if(.NOT.associated(thisobs)) then
            write(*,*) 'No memory to release'
            return
         endif
         do while(associated(thisobs))
!            write(*,*) 'destroy ==',thisobs%name

            thisobsnext => thisobs%next
            call thisobs%destroy()
            thisobs => thisobsnext
         enddo

         write(*,*) 'Finished finish_nwp'
         write(*,*) ' '

      end subroutine finish_nwp

end module module_nwp
