program write_idate
!
!  how to compile:
!    ifort -assume byterecl -convert big_endian write_idate.f90
!
!  read ascii date from file and then write date to a binary file
!
 implicit none

!---------------------------------------------------------------------------------- 
! Integer types from GSI
  integer, parameter :: i_byte  = selected_int_kind(1)      ! byte  integer
  integer, parameter :: i_short = selected_int_kind(4)      ! short integer
  integer, parameter :: i_long  = selected_int_kind(8)      ! long  integer
  integer, parameter :: llong_t = selected_int_kind(16)     ! llong integer
  integer, parameter :: i_llong = max( llong_t, i_long )

! Expected 8-bit byte sizes of the integer kinds
  integer, parameter :: num_bytes_for_i_byte  = 1
  integer, parameter :: num_bytes_for_i_short = 2
  integer, parameter :: num_bytes_for_i_long  = 4
  integer, parameter :: num_bytes_for_i_llong = 8

! Define arrays for default definition
  integer, parameter :: num_i_kinds = 4
  integer, parameter, dimension( num_i_kinds ) :: integer_types = (/ &
       i_byte, i_short, i_long,  i_llong  /)
  integer, parameter, dimension( num_i_kinds ) :: integer_byte_sizes = (/ &
       num_bytes_for_i_byte, num_bytes_for_i_short, &
       num_bytes_for_i_long, num_bytes_for_i_llong  /)

! Default values
! **** CHANGE THE FOLLOWING TO CHANGE THE DEFAULT INTEGER TYPE KIND ***
  integer, parameter :: default_integer = 3  ! 1=byte,
                                             ! 2=short,
                                             ! 3=long,
                                             ! 4=llong
  integer, parameter :: i_kind = integer_types( default_integer )
  integer, parameter :: num_bytes_for_i_kind = &
       integer_byte_sizes( default_integer )

!----------------------------------------------------------------------------------

 integer(i_kind) :: idate
 integer :: unit_in=11, unit_out=10

 integer :: iyear, imonth, iday, ihour


! read date from ascii file
 open(unit_in, status='old', file='idate.input')
 read(unit_in, *) idate
 close(unit_in)

! print date
 iyear = idate / 1000000
 imonth = (idate - 1000000*iyear) / 10000
 iday =  (idate - 1000000*iyear - 10000*imonth) / 100
 ihour = idate - 1000000*iyear - 10000*imonth - 100*iday
 print*, 'date:'
 print*, idate
 print*, iyear
 print*, imonth
 print*, iday
 print*, ihour

! write date to binary file 
 open(unit_out, status='unknown', form='unformatted', position='rewind', file='idate.output')
 write(unit_out) idate
 close(unit_out)

end program

