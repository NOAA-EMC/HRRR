!
!=================================Make Executable============================
!  Make executable:
!
!  Run program:
!      fires_ncfmake input_data_file_name
!
!  Initial version May 2004
!  Cindy Bruyere
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Following subroutines must be in the 'INCLUDEDIR' directory defined as an environment variable, sepcified in compile statement
include 'species.inc'
include 'netcdf_writroutines.inc'
include 'manipufiles.inc'
!-------------------------------------------------------------------------------------------
program fires_ncmake
  ! use species, only: plot_dim
  implicit none
  character (len=256)    :: input_file                        
  character (len=10)    :: option                        
  character (len=10)    :: plot_var
  integer               :: length_input, length_option, time1, time2
  ! integer               :: plot_dim(3)
  integer               :: numarg, i, idummy
  integer               :: ts_xy(3)
  real                  :: ts_ll(3)
  character (len=10)    :: ts_var(100)
  integer               :: ts_i
  character (len=2)     :: ts_type
  integer, external     :: iargc
  character (len=256)    :: dummy
  character (len=256)   :: binary_file

  numarg=command_argument_count()

  if(numarg<1 .or. numarg>2) then
     write(6,'(A)') 'Synopsis: copies binary fields into wrfinput file and makes a diagnostic file'
     write(6,'(A)') ' '
     write(6,'(A)') 'Format: fires_ncfmake.x /path/to/wrfinput_d01 [ /path/to/binary-file ]'
     write(6,'(A)') ' '
     write(6,'(A)') '  If second argument is absent, the $binary_file environment variable'
     write(6,'(A)') '  must contain the path to the binary file.'
     write(6,'(A)') ' '
     write(6,'(A)') '  The diagnostic  file will always be "wrffirechemi_d01"'
     stop 3
  endif

  call get_command_argument(1,dummy)
  input_file = trim(dummy)

  if(numarg>=2) then
     call get_command_argument(2,dummy)
     binary_file=trim(dummy)
  else if(len_trim(binary_file)<1) then
     CALL GETENV ('binary_file',binary_file)
  endif

  if (len_trim(input_file)<1) then
     write(*,*)' No input file selected, stopping'
     stop 2
  endif

  if (len_trim(binary_file)<1) then
     write(*,*)' No binary file selected, stopping'
     stop 2
  endif

  write(6,'(2A)')'INPUT FILE IS:  ',trim(input_file)
  write(6,'(2A)')'BINARY FILE IS: ',trim(input_file)
  print*," "
  ! Now read the file
  call file_manipulate (binary_file,"chemdiag.nc",input_file,length_input)
end program fires_ncmake
