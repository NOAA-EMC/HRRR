!############################# LICENSE ######################################
!
!   Copyright (C) <2010>  <David C. Dowell and Louis J. Wicker, NOAA>
!
!   This library is free software; you can redistribute it and/or
!   modify it under the terms of the GNU Lesser General Public
!   License as published by the Free Software Foundation; either
!   version 2.1 of the License, or (at your option) any later version.
!
!   This library is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   Lesser General Public License for more details.
!
!############################# LICENSE ######################################
!-----------------------------------------------------------------------------
!
! MODULE DART_MODULE
!
! This module contains variables and routines for reading and writing
! DART (Data Assimilation Research Testbed) observation files.
!
! David Dowell, 12/23/05
!
!
!
!-----------------------------------------------------------------------------
      MODULE DART_module

!     Indices and names for particular observation types

      integer obs_kind_Doppler_velocity              ! Doppler velocity (m/s)
      logical use_obs_kind_Doppler_velocity
      character(len=32) obs_name_Doppler_velocity
      parameter(obs_name_Doppler_velocity = 'DOPPLER_RADIAL_VELOCITY')

      integer obs_kind_reflectivity                  ! effective reflectivity factor (dBZ)
      logical use_obs_kind_reflectivity
      character(len=32) obs_name_reflectivity
      parameter(obs_name_reflectivity = 'RADAR_REFLECTIVITY')
      
      integer obs_kind_clearair_reflectivity          ! low-reflectivity observation created by filling data voids
      logical use_obs_kind_clearair_reflectivity
      character(len=32) obs_name_clearair_reflectivity
      parameter(obs_name_clearair_reflectivity = 'RADAR_CLEARAIR_REFLECTIVITY')

      integer obs_kind_zdr                           ! differential reflectivity (dB)
      logical use_obs_kind_zdr
      character(len=32) obs_name_zdr
      parameter(obs_name_zdr = 'DIFFERENTIAL_REFLECTIVITY')

      integer obs_kind_kdp                           ! specific differential phase shift (deg/km)
      logical use_obs_kind_kdp
      character(len=32) obs_name_kdp
      parameter(obs_name_kdp = 'SPECIFIC_DIFFERENTIAL_PHASE')

      integer obs_kind_u_10m                         ! u at 10 m AGL, in m/s
      logical use_obs_kind_u_10m
      character(len=32) obs_name_u_10m
      parameter(obs_name_u_10m = 'METAR_U_10_METER_WIND')

      integer obs_kind_v_10m                         ! v at 10 m AGL, in m/s
      logical use_obs_kind_v_10m
      character(len=32) obs_name_v_10m
      parameter(obs_name_v_10m = 'METAR_V_10_METER_WIND')

      integer obs_kind_T_2m                          ! temperature at 2 m AGL (K)
      logical use_obs_kind_T_2m
      character(len=32) obs_name_T_2m
      parameter(obs_name_T_2m = 'METAR_TEMPERATURE_2_METER')

      integer obs_kind_THETA_2m                      ! potential temperature at 2 m AGL (K)
      logical use_obs_kind_THETA_2m
      character(len=32) obs_name_THETA_2m
      parameter(obs_name_THETA_2m = 'METAR_POT_TEMP_2_METER')

      integer obs_kind_Td_2m                         ! dew point at 2 m AGL (K)
      logical use_obs_kind_Td_2m 
      character(len=32) obs_name_Td_2m
      parameter(obs_name_Td_2m = 'DEW_POINT_2_METER')

      integer obs_kind_qv_2m                         ! qv at 2 m AGL (g/g)
      logical use_obs_kind_qv_2m
      character(len=32) obs_name_qv_2m
      parameter(obs_name_qv_2m = 'METAR_SPECIFIC_HUMIDITY_2_METER')

!     Miscellaneous variables

      integer num_copies
      integer num_qc
      integer num_obs
      integer max_num_obs

      character(len=129), allocatable :: copy_meta_data(:)
      character(len=129), allocatable :: qc_meta_data(:)

      integer ob_index                               ! meta data index corresponding to observation
      integer truth_index                            ! meta data index corresponding to truth


      CONTAINS


!############################################################################
!
!     ##################################################################
!     ######                                                      ######
!     ######           SUBROUTINE DEFAULT_DART_OBS_KIND           ######
!     ######                                                      ######
!     ##################################################################
!
!
!     PURPOSE:
!
!     This subroutine initializes default values of DART obs_kind indices.
!
!############################################################################
!
!     Author:  David Dowell
!
!     Creation Date:  23 December 2005
!
!     Modifications:
!
!############################################################################

      subroutine default_DART_obs_kind(append_eval)

      implicit none

!---- Passed variables
      logical append_eval          ! .true. if "_EVAL" should be appended to the names of DART observation types

      obs_kind_Doppler_velocity = 11
      obs_kind_reflectivity = 12
      obs_kind_clearair_reflectivity = 13
      obs_kind_zdr = 300
      obs_kind_kdp = 301
      obs_kind_u_10m = 1
      obs_kind_v_10m = 2
      obs_kind_T_2m = 4
      obs_kind_THETA_2m = 6
      obs_kind_Td_2m = 9
      obs_kind_qv_2m = 5
      
      if (append_eval) then
        obs_kind_Doppler_velocity = obs_kind_Doppler_velocity + 100
        obs_kind_reflectivity = obs_kind_reflectivity + 100
        obs_kind_clearair_reflectivity = obs_kind_clearair_reflectivity + 100
        obs_kind_zdr = obs_kind_zdr + 100
        obs_kind_kdp = obs_kind_kdp + 100
        obs_kind_u_10m = obs_kind_u_10m + 100
        obs_kind_v_10m = obs_kind_v_10m + 100
        obs_kind_T_2m = obs_kind_T_2m + 100
        obs_kind_THETA_2m = obs_kind_THETA_2m + 100
        obs_kind_Td_2m = obs_kind_Td_2m + 100
        obs_kind_qv_2m = obs_kind_qv_2m + 100
      endif

      RETURN
      END SUBROUTINE default_DART_obs_kind


!############################################################################
!
!     ##################################################################
!     ######                                                      ######
!     ######                SUBROUTINE READ_DART_OB               ######
!     ######                                                      ######
!     ##################################################################
!
!
!     PURPOSE:
!
!     This subroutine reads a single observation in DART format from the
!     input file.
!
!############################################################################
!
!     Author:  David Dowell
!
!     Creation Date:  12 April 2005
!
!     Modifications:
!         19 December 2005 (David Dowell) -- updates for new DART ob format
!
!############################################################################

      subroutine read_DART_ob(fi, ob, truth, az, el, Nyquist_vel,      &
                              olat, olon, oheight, obs_kind, secs, days)

      implicit none

!---- Passed variables

      integer fi                            ! file unit number

!---- Returned variables

      real ob                               ! observation
      real truth                            ! true value
      real az                               ! azimuth angle (rad)
      real el                               ! elevation angle (rad)
      real Nyquist_vel                      ! Nyquist velocity (m/s)
      real(kind=8) olat, olon               ! observation lat and lon (rad)
      real(kind=8) oheight                  ! observation height (m MSL)
      integer obs_kind                      ! index corresponding to observation type
      integer secs, days                    ! observation time, in DART format (gregorian days and seconds)

!---- Local variables

      integer n
      real(kind=8) value
      integer which_vert
      real(kind=8) dir(3)

      integer prev_time, next_time
      integer cov_group
      real(kind=8) error_variance
      real(kind=8) rlat, rlon, rheight


      read(fi,*)           ! 'OBS...'
      do n=1, num_copies
        read(fi,*) value
        if (n.eq.ob_index) ob = value
        if (n.eq.truth_index) truth = value
      enddo
      do n=1, num_qc
        read(fi,*)
      enddo
      read(fi,*) prev_time, next_time, cov_group
      read(fi,*)           ! 'obdef...'
      read(fi,*)           ! 'loc3d'
      read(fi,*) olon, olat, oheight, which_vert
      if (which_vert.ne.3) then
        write(*,*) 'read_DART_ob:  unable to handle which_vert=', which_vert
        stop
      endif
      read(fi,*)           ! 'kind'
      read(fi,*) obs_kind

      if (obs_kind.eq.obs_kind_Doppler_velocity) then              ! Doppler velocity ob
        read(fi,*)           ! 'platform'
        read(fi,*)           ! 'loc3d' 
        read(fi,*) rlon, rlat, rheight, which_vert
        read(fi,*)           ! 'dir3d'
        read(fi,*) dir(1), dir(2), dir(3)
        el = asin(dir(3))
        if (abs(cos(el)).lt.0.001) then
          az = 0.0
        else
          az = atan2(dir(1)/cos(el), dir(2)/cos(el))
        endif
        read(fi,*) Nyquist_vel
        read(fi,*)          ! key
      endif

      read(fi,*) secs, days
      read(fi,*) error_variance

      RETURN
      END SUBROUTINE read_DART_ob


!############################################################################
!
!     ##################################################################
!     ######                                                      ######
!     ######                SUBROUTINE WRITE_DART_OB              ######
!     ######                                                      ######
!     ##################################################################
!
!
!     PURPOSE:
!
!     This subroutine writes a single observation in DART format to the
!     output file
!
!############################################################################
!
!     Author:  David Dowell
!
!     Creation Date:  23 March 2005
!
!     Modifications:
!         19 December 2005 (David Dowell) -- updates for new DART ob format
!
!############################################################################
!
      subroutine write_DART_ob(fi, o, ob_value, true_value,          &
                               olat, olon, oheight, which_vert, az, el, Nyquist_vel, key, &
                               rlat, rlon, rheight, &
                               obs_kind, secs, days, error_variance, qc_value)

      implicit none

!      include 'param.h'
      real pii; parameter (pii=3.14159265)

!---- Passed variables

      integer fi                            ! file unit number
      integer o                             ! current observation number
      real ob_value                         ! observation
      real true_value                       ! truth (error-free observation)
      real(kind=8) olat, olon               ! observation lat and lon (rad)
      real(kind=8) oheight                  ! observation height (m MSL)
      integer which_vert                    ! vertical coordinate
                                            !    -1 = model surface
                                            !     3 = height (m MSL)
      real az                               ! azimuth angle (deg)
      real el                               ! elevation angle (deg)
      real Nyquist_vel                      ! Nyquist velocity (m/s)
      integer key                           ! number of observations of this type so far
      real(kind=8) rlat, rlon               ! radar lat and lon (rad)
      real(kind=8) rheight                  ! radar height (m MSL)
      integer obs_kind                      ! index of observation type
      integer secs, days
      real(kind=8) error_variance
      real qc_value

!---- Local variables

      integer cov_group; parameter(cov_group=-1)
      integer prev_time, next_time
      real(kind=8) dir(3)


      write(fi,*) 'OBS ', o
      if (num_copies.ge.1) write(fi,*) ob_value
      if (num_copies.ge.2) write(fi,*) true_value

      if (num_qc.ge.1) write(fi,*) qc_value

      if (o.eq.1) then
        prev_time = -1
      else
        prev_time = o-1
      endif
      if (o.eq.max_num_obs) then
        next_time = -1
      else
        next_time = o+1
      endif
      write(fi,*) prev_time, next_time, cov_group

      write(fi,11)
 11   format('obdef')

!     write_location

      write(fi, '(''loc3d'')' ) 
      if (olon.lt.0.0) olon=olon + 2.0*pii
      write(fi,*) olon, olat, oheight, which_vert

!     write_kind

      write(fi, '(''kind'')' )
      write(fi,*) obs_kind

!     platform information

      if (obs_kind.eq.obs_kind_Doppler_velocity) then
        dir(1) = sin(az*pii/180.0)*cos(el*pii/180.0)
        dir(2) = cos(az*pii/180.0)*cos(el*pii/180.0)
        dir(3) = sin(el*pii/180.0)
        write(fi, '(''platform'')' )
        write(fi, '(''loc3d'')' )
        if (rlon.lt.0.0) rlon=rlon + 2.0*pii
        write(fi,*) rlon, rlat, rheight, which_vert
        write(fi, '(''dir3d'')' )
        write(fi,*) dir(1), dir(2), dir(3)
        write(fi,*) Nyquist_vel
        write(fi,*) key
      endif

!     write_time

      write(fi,*) secs, days

!     error_variance

      write(fi,*) error_variance

      RETURN
      END SUBROUTINE WRITE_DART_OB


!############################################################################
!
!     ##################################################################
!     ######                                                      ######
!     ######              SUBROUTINE READ_DART_HEADER             ######
!     ######                                                      ######
!     ##################################################################
!
!
!     PURPOSE:
!
!     This subroutine reads the header of a DART format observation file
!     and initializes some the variables in DART_module.
!
!############################################################################
!
!     Author:  David Dowell
!
!     Creation Date:  23 December 2005
!
!     Modifications:
!
!
!############################################################################

      subroutine read_DART_header(fi, append_eval)

      implicit none

!---- Passed variables

      integer fi                            ! file unit number
      logical append_eval                   ! .true. if "_EVAL" should be appended to the names of DART observation types

!---- Local variables

      character(len=16) label(2)
      integer n_obs_kind
      integer obs_kind
      character(len=32) obs_name
      integer n


      call default_DART_obs_kind(append_eval)

      read(fi,*)      ! "obs_sequence"
      read(fi,*)      ! "obs_kind_definitions"
      read(fi,*) n_obs_kind
      do n=1, n_obs_kind
        read(fi,*) obs_kind, obs_name
        select case (obs_name)
        case (obs_name_Doppler_velocity)
          obs_kind_Doppler_velocity = obs_kind
        case (obs_name_reflectivity)
          obs_kind_reflectivity = obs_kind
        case (obs_name_clearair_reflectivity)
          obs_kind_clearair_reflectivity = obs_kind
        case (obs_name_zdr)
          obs_kind_zdr = obs_kind
        case (obs_name_kdp)
          obs_kind_kdp = obs_kind
        case (obs_name_u_10m)
          obs_kind_u_10m = obs_kind
        case (obs_name_v_10m)
          obs_kind_v_10m = obs_kind
        case (obs_name_T_2m)
          obs_kind_T_2m = obs_kind
        case (obs_name_THETA_2m)
          obs_kind_THETA_2m = obs_kind
        case (obs_name_Td_2m)
          obs_kind_Td_2m = obs_kind
        case (obs_name_qv_2m)
          obs_kind_qv_2m = obs_kind
        case default
          write(*,*) 'unknown obs_name:  ', obs_name, obs_kind
        end select
      enddo

      read(fi,*) label(1), num_copies, label(2), num_qc
      read(fi,*) label(1), num_obs, label(2), max_num_obs

      allocate(copy_meta_data(num_copies))
      allocate(qc_meta_data(num_qc))
      ob_index = 0
      truth_index = 0

      do n=1, num_copies
        read(fi,'(a129)') copy_meta_data(n)
!        if (index(copy_meta_data(n),'observations').ne.0) ob_index = n
        if (index(copy_meta_data(n),'observation').ne.0) ob_index = n
        if (index(copy_meta_data(n),'truth').ne.0) truth_index = n
      enddo

      do n=1, num_qc
        read(fi,'(a129)') qc_meta_data(n)
      enddo

      read(fi,*)     ! first, last

      deallocate(copy_meta_data)
      deallocate(qc_meta_data)

      RETURN
      END SUBROUTINE READ_DART_HEADER


!############################################################################
!
!     ##################################################################
!     ######                                                      ######
!     ######             SUBROUTINE write_DART_ob_info            ######
!     ######                                                      ######
!     ##################################################################
!
!
!     PURPOSE:
!
!     This subroutine outputs the obs_kind and obs_name to the DART header,
!     with "_EVAL" appended to obs_name if append_eval is .true.
!
!############################################################################
!
!     Author:  David Dowell
!
!     Creation Date:  9 December 2011
!
!     Modifications:
!
!############################################################################

      subroutine write_DART_ob_info(fi, obs_kind, obs_name, append_eval)

      implicit none

!---- Passed variables

      integer fi                   ! file unit number
      integer obs_kind
      character(len=32) obs_name
      logical append_eval          ! .true. if "_EVAL" should be appended to the names of DART observation types

!---- Local variables

      integer ls                   ! string length
      character(len=32) temp_obs_name

      if (append_eval) then
        temp_obs_name = obs_name
        ls = index(obs_name, ' ') - 1
        ls = min(ls, 27)
        temp_obs_name(ls+1:ls+5) = '_EVAL'
      else
        temp_obs_name = obs_name
      endif
      write(fi,*) obs_kind, temp_obs_name

      RETURN
      END SUBROUTINE write_DART_ob_info


!############################################################################
!
!     ##################################################################
!     ######                                                      ######
!     ######              SUBROUTINE WRITE_DART_HEADER            ######
!     ######                                                      ######
!     ##################################################################
!
!
!     PURPOSE:
!
!     This subroutine writes the first part of a DART format observation file.
!
!     Note:  num_copies must be set before this subroutine is called.
!
!############################################################################
!
!     Author:  David Dowell
!
!     Creation Date:  17 August 2005
!
!     Modifications:
!         19 December 2005 (David Dowell) -- updates for new DART ob format
!
!############################################################################
!
      subroutine write_DART_header(fi, append_eval, qc_string)

      implicit none

!---- Passed variables

      integer fi                   ! file unit number
      logical append_eval          ! .true. if "_EVAL" should be appended to the names of DART observation types
      character(len=129) qc_string

!---- Local variables

      integer first_time
      integer last_time
      integer n


      call default_DART_obs_kind(append_eval)

      allocate(copy_meta_data(num_copies))
      allocate(qc_meta_data(num_qc))
!      if (num_copies.ge.1) copy_meta_data(1) = 'observations'
      if (num_copies.ge.1) copy_meta_data(1) = 'observation'
      if (num_copies.ge.2) copy_meta_data(2) = 'truth'
      if ( (num_copies.gt.2) .or. (num_copies.lt.0) ) then
        write(*,*) 'write_DART_header:  ERROR, invalid value of num_copies:  ', num_copies
        stop
      endif
      if (num_qc.eq.1) then
        qc_meta_data(1) = qc_string
      else if (num_qc.ne.0) then
        write(*,*) 'write_DART_header:  ERROR, invalid value of num_qc:  ', num_qc
        stop
      endif
      ob_index = 1
      truth_index = 2

      write(fi,*) 'obs_sequence'
      write(fi,11)
 11   format('obs_kind_definitions')

      n = 0
      if (use_obs_kind_Doppler_velocity) n = n + 1
      if (use_obs_kind_reflectivity) n = n + 1
      if (use_obs_kind_clearair_reflectivity) n = n + 1
      if (use_obs_kind_zdr) n = n + 1
      if (use_obs_kind_kdp) n = n + 1
      if (use_obs_kind_u_10m) n = n + 1
      if (use_obs_kind_v_10m) n = n + 1
      if (use_obs_kind_T_2m) n = n + 1
      if (use_obs_kind_THETA_2m) n = n + 1
      if (use_obs_kind_Td_2m) n = n + 1
      if (use_obs_kind_qv_2m) n = n + 1

      write(fi,*) n

      if (use_obs_kind_Doppler_velocity) then
        call write_DART_ob_info(fi, obs_kind_Doppler_velocity, obs_name_Doppler_velocity, append_eval)
      endif
      if (use_obs_kind_reflectivity) then
        call write_DART_ob_info(fi, obs_kind_reflectivity, obs_name_reflectivity, append_eval)
      endif
      if (use_obs_kind_clearair_reflectivity) then
        call write_DART_ob_info(fi, obs_kind_clearair_reflectivity, obs_name_clearair_reflectivity, append_eval)
      endif
      if (use_obs_kind_zdr) then
        call write_DART_ob_info(fi, obs_kind_zdr, obs_name_zdr, append_eval)
      endif
      if (use_obs_kind_kdp) then
        call write_DART_ob_info(fi, obs_kind_kdp, obs_name_kdp, append_eval)
      endif
      if (use_obs_kind_u_10m) then
        call write_DART_ob_info(fi, obs_kind_u_10m, obs_name_u_10m, append_eval)
      endif
      if (use_obs_kind_v_10m) then
        call write_DART_ob_info(fi, obs_kind_v_10m, obs_name_v_10m, append_eval)
      endif
      if (use_obs_kind_T_2m) then
        call write_DART_ob_info(fi, obs_kind_T_2m, obs_name_T_2m, append_eval)
      endif
      if (use_obs_kind_THETA_2m) then
        call write_DART_ob_info(fi, obs_kind_THETA_2m, obs_name_THETA_2m, append_eval)
      endif
      if (use_obs_kind_Td_2m) then
        call write_DART_ob_info(fi, obs_kind_Td_2m, obs_name_Td_2m, append_eval)
      endif
      if (use_obs_kind_qv_2m) then
        call write_DART_ob_info(fi, obs_kind_qv_2m, obs_name_qv_2m, append_eval)
      endif

      num_obs = max_num_obs
      write(fi,*) ' num_copies: ', num_copies, ' num_qc: ', num_qc
      write(fi,*) ' num_obs: ', num_obs,    ' max_num_obs: ', max_num_obs
        
      do n=1, num_copies
        write(fi, '(a129)') copy_meta_data(n)
      enddo

      do n=1, num_qc
        write(fi, '(a129)') qc_meta_data(n)
      enddo

      first_time = 1
      last_time = max_num_obs
      write(fi,*) ' first: ', first_time, ' last: ', last_time

      deallocate(copy_meta_data)
      deallocate(qc_meta_data)

      RETURN
      END SUBROUTINE WRITE_DART_HEADER

!###########################################################################                  
!                                                                                             
!     ##################################################################                      
!     ######                                                      ######                      
!     ######           SUBROUTINE SET_DATE_GREGORIAN              ######                      
!     ######                                                      ######                      
!     ##################################################################                      
!                                                                                             
!     PURPOSE:                                                                                
!                                                                                             
!     Computes time corresponding to date for gregorian calendar.                             
!                                                                                             
!############################################################################                 
!                                                                                             
!     Author:  Data Assimilation Research Testbed -- DART                                     
!              Data Assimilation Initiative, University Corporation for Atmospheric Research  
!                                                                                             
!     Created:  September 9, 2004                                                             
!                                                                                             
!     Modified:  May 31, 2005 (David Dowell)                                                  
!                                                                                             
!############################################################################

      subroutine set_date_gregorian(days, secs, year, month, day, hours, minutes, seconds)    

      implicit none

!---- returned variables

      integer :: days                  ! Gregorian day since beginning of base year
      integer :: secs                  ! seconds since beginning of day

!---- input variables

      integer :: day, month, year
      integer :: seconds, minutes, hours

!---- local variables

      integer(kind=4) :: julian_day
      integer :: nleapyr
      integer :: base_year = 1601


      call set_julian_day(julian_day, year, month, day)

      ! Need to check for bogus times

      if ( seconds > 59 .or. seconds  < 0 .or.    &
           minutes > 59 .or. minutes  < 0 .or.    &
           hours   > 23 .or. hours    < 0 ) then
        write(*,*) 'set_date_gregorian:  ',    &
                   seconds,minutes,hours,    &
                   ' not a valid time'
        stop
      endif

      ! Compute number of leap years fully past since base_year 

      nleapyr = (year - base_year) / 4 - (year - base_year) / 100 + (year - base_year) / 400  

      secs = seconds + 60*(minutes + 60 * hours)
      days = julian_day - 1 + 365*(year - base_year - nleapyr) + 366*(nleapyr)                

      return
      end subroutine set_date_gregorian

!###########################################################################                  
!                                                                                             
!     ##################################################################                      
!     ######                                                      ######                      
!     ######              SUBROUTINE SET_JULIAN_DAY               ######                      
!     ######                                                      ######                      
!     ##################################################################                      
!                                                                                             
!     PURPOSE:                                                                                
!                                                                                             
!     Computes julian day corresponding to input year, month, and day                         
!                                                                                             
!############################################################################                 
!                                                                                             
!     Author:  Data Assimilation Research Testbed -- DART                                     
!              Data Assimilation Initiative, University Corporation for Atmospheric Research  
!                                                                                             
!     Created:  September 9, 2004                                                             
!                                                                                             
!     Modified:  January 28, 2006 (David Dowell)                                              
!                                                                                             
!############################################################################

      subroutine set_julian_day(julian_day, year, month, day)

      implicit none

!---- returned variables

      integer(kind=4) julian_day

!---- input variables

      integer day, month, year

!---- local variables

      integer :: base_year = 1601
      integer :: ndays, m
      logical :: leap
      integer :: days_per_month(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)                 


      ! Need to check for bogus dates

      if (                   day      < 1 .or.             &
           month   > 12 .or. month    < 1 .or.             &
                             year     < base_year ) then    
        write(*,*) 'set_julian_day:  ', day,month,year,    &
                   ' not a valid date'
        stop
      endif

      if (month /= 2 .and. day > days_per_month(month)) then
         write(*,*) 'month (',month,') does not have ',day,' days'
         stop
      endif

      ! Is this a leap year? Gregorian calandar assigns each year evenly
      ! divisible by 4 that is not a century year unevenly divisible by 400
      ! as a leap-year. (i.e. 1700,1800,1900 are not leap-years, 2000 is)

      leap=(modulo(year,4).eq.0)
      if((modulo(year,100).eq.0).and.(modulo(year,400).ne.0))then
       leap=.false.
      endif

      ! Finish checking for day specification errors

      if (month == 2 .and. (day > 29 .or. ((.not. leap) .and. day > 28))) then                
        write(*,*) 'month (',month,') does not have ', &
                   day,' days in a lon-leap year'
        stop
      endif

      ! Count up days in this year

      ndays = 0
      do m=1,month-1
       ndays = ndays + days_per_month(m)
       if(leap .and. m == 2) ndays = ndays + 1
      enddo
      julian_day = ndays + day

      return
      end subroutine set_julian_day

      END MODULE DART_module

