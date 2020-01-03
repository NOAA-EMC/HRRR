Subroutine write_netcdf_nsslref( nlvls,nlons,nlats,ref0,idate,xlon,ylat,msclev )
  use netcdf
  implicit none

  character (len = *), parameter :: FILE_NAME = "Gridded_ref.nc"
  integer :: ncid

  integer, parameter             :: NDIMS=3
  integer, parameter             :: NDIMS2D=2
  character (len = *), parameter :: LVL_NAME = "height"
  character (len = *), parameter :: LAT_NAME = "latitude"
  character (len = *), parameter :: LON_NAME = "longitude"
  integer :: lvl_dimid, lon_dimid, lat_dimid
  integer :: NLVLS, NLATS, NLONS

  real, dimension(NLONS, NLATS) :: xlon
  real, dimension(NLONS, NLATS) :: ylat
  real, dimension(NLONS, NLATS, NLVLS) :: ref0
  real, dimension(NLVLS) :: msclev

  integer :: lon_varid, lat_varid, dimids(NDIMS), dimids2d(NDIMS2D)
  integer :: ref_varid, lvl_varid
  integer :: idate

  character (len = *), parameter :: UNITS = "units"
  character (len = *), parameter :: REF_UNITS = "dBZ"
  character (len = *), parameter :: LAT_UNITS = "degrees_north"
  character (len = *), parameter :: LON_UNITS = "degrees_east"
  character (len = *), parameter :: LVL_UNITS = "meter"

  character (len = *), parameter :: REF_NAME="reflectivity"
  
  print*,"Start to write out"
  write(*,*) 'max min',maxval(ref0),minval(ref0)

  ! Create the file. 
  call check( nf90_create(FILE_NAME, nf90_clobber, ncid) )

  ! Define the dimensions. 
  call check( nf90_def_dim(ncid, LVL_NAME, NLVLS, lvl_dimid) )
  call check( nf90_def_dim(ncid, LAT_NAME, NLATS, lat_dimid) )
  call check( nf90_def_dim(ncid, LON_NAME, NLONS, lon_dimid) )

  dimids2d = (/ lon_dimid, lat_dimid /)

  ! Define the coordinate variables.
  call check( nf90_def_var(ncid, LAT_NAME, NF90_REAL, dimids2d, lat_varid) )
  call check( nf90_def_var(ncid, LON_NAME, NF90_REAL, dimids2d, lon_varid) )
  call check( nf90_def_var(ncid, LVL_NAME, NF90_REAL, lvl_dimid, lvl_varid) )

  ! Assign units attributes to coordinate variables.
  call check( nf90_put_att(ncid, lat_varid, UNITS, LAT_UNITS) )
  call check( nf90_put_att(ncid, lon_varid, UNITS, LON_UNITS) )
  call check( nf90_put_att(ncid, lvl_varid, UNITS, LVL_UNITS) )

  dimids = (/ lon_dimid, lat_dimid, lvl_dimid /)
  
  ! Define the netCDF variables for the reflectivity data.
  call check( nf90_def_var(ncid, REF_NAME, NF90_REAL, dimids, ref_varid) )

  ! Assign units attributes to the netCDF variables.
  call check( nf90_put_att(ncid, ref_varid, UNITS, REF_UNITS) )

  ! End define mode.
  call check( nf90_enddef(ncid) )

  ! Write the coordinate variable data. This will put the latitudes
  ! and longitudes of our data grid into the netCDF file.
  call check( nf90_put_var(ncid, lat_varid, ylat) )
  call check( nf90_put_var(ncid, lon_varid, xlon) )
  call check( nf90_put_var(ncid, lvl_varid, msclev) )

  ! Write the data.
  call check( nf90_put_var(ncid, ref_varid, ref0))

  ! Close the file.
  call check( nf90_close(ncid) )
   
  ! If we got this far, everything worked as expected. Yipee! 
  print *,"*** SUCCESS writing file "//trim(FILE_NAME)//"!"

contains
  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check

End subroutine write_netcdf_nsslref
