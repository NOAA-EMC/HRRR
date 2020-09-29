program qc_modis
  ! Takes MODIS data into stdin, discards invalid lines, writes valid
  ! lines to stdout, and logs to stderr.

  implicit none

  ! Data fields read in, declared in order:
  REAL :: lat_m,lon_m,brightness,scan,track
  CHARACTER(len=10)  :: acq_date
  CHARACTER(len=5)   :: acq_time
  CHARACTER(len=1)   :: satellite
  INTEGER            :: confidence
  CHARACTER(len=6)   :: version
  REAL               :: bright_t31,frp_v
  CHARACTER(len=1)   :: dani

  ! Max length of a line
  integer, parameter :: LINE_LEN = 500

  ! Buffer for reading each line:
  CHARACTER(len=LINE_LEN) :: line

  ! For dismantling dates and times:
  CHARACTER(len=4)    :: yearc
  CHARACTER(len=2)    :: monc
  CHARACTER(len=2)    :: dayc
  CHARACTER(len=2)    :: hour
  CHARACTER(len=2)    :: minute
  INTEGER :: yy, mm, dday, dhh, dmm

  ! Count the lines read in:
  integer :: nread, nwrote

  ! Detect parser errors in data lines:
  integer :: ios     ! entire line
  integer :: iosd(3) ! subset of time or date

  READ(5,'(A500)',iostat=ios,err=100,end=101) line
  print '(A)',trim(line)

40 format("Discard line ",I0," ",A," ",G20.12)
50 format("Discard line ",I0," ",A," ",I0)
60 format("Discard line ",I0," ",A," ",A)
 
  nread=0
  nwrote=0
  line_loop: do
     READ(5,'(A)', iostat=ios, err=200, end=201) line
     ios=0
     READ(line, *, iostat=ios) &
          lat_m,lon_m,brightness,scan,track,acq_date,acq_time,satellite,confidence,version,bright_t31,frp_v,dani

     if(ios/=0) then
        write(0,50) nread+1,'with parser error iostat =',ios
        cycle
     endif

     nread=nread+1
     
     if ((frp_v<1.) .OR. (frp_v>10000.)) then
        write(0,40) nread+1,"with implausible frp_v value",frp_v
        cycle
     else if(lon_m<-180.0 .or. lon_m>180.0) then
        write(0,40) nread+1,'with longitude lon_m outside [-180,180]',lon_m
        cycle
     else if(lat_m>90.0 .or. lat_m<-90.0) then
        write(0,40) nread+1,"with invalid lat_m latitude",lat_m
        cycle
     endif

     yearc = acq_date(1:4)
     monc  = acq_date(6:7)
     dayc  = acq_date(9:10)
     iosd=0
     read(yearc(1:4),*,iostat=iosd(1)) yy
     read(monc(1:2),*,iostat=iosd(2))  mm
     read(dayc(1:2),*,iostat=iosd(3))  dday
     if(any(iosd/=0)) then
        write(0,60) nread+1,'with unparsable date string',acq_date
        cycle
     endif

     hour =  acq_time(1:2)
     minute= acq_time(4:5)
     iosd=0
     read(hour(1:2),*,iostat=iosd(1)) dhh
     read(minute(1:2),*,iostat=iosd(2)) dmm
     if(any(iosd/=0)) then
        write(0,60) nread+1,'with unparsable time string',acq_time
        cycle
     endif

     if(yy<1000 .or. yy>9999) then
        write(0,50) nread+1,"with non-four-digit year",yy
        cycle
     else if(mm<1 .or. mm>12) then
        write(0,50) nread+1,"with invalid month",mm
        cycle
     else if(dday<1 .or. dday>31) then
        write(0,50) nread+1,"with invalid day",dday
        cycle
     endif

     ! We only get here if all QC tests pass.
     print '(A)',trim(line)
     nwrote=nwrote+1
  end do line_loop

100 continue ! Error handling for EOF on stdin for header
  write(0,'(A)') 'No header line seen; file is empty.'
  stop 0

101 continue ! Error handling for IO error on header
  write(0,'(A,I0)') 'Unable to read header from input: iostat=',ios
  stop 0

200 continue ! Error handling for IO error on data lines
  write(0,'(A,I0)') 'Error reading from input: iostat=',ios
  write(0,210) nread,nwrote,nread-nwrote
  stop 0

201 continue ! EOF on stdin for data lines; normal exit
210 format("Data lines: read ",I0," wrote ",I0," discarded ",I0)
  write(0,210) nread,nwrote,nread-nwrote
  
end program qc_modis
