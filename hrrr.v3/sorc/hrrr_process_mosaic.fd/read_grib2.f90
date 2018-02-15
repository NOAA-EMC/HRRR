program read_grib2
!subroutine read_grib2()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_grib2  read grib2 file
!   prgmmr: Ming Hu          org: GSD                 date: 2015-05-20
!
! abstract: read grib2 file
!
!
! program history log:
!   2015-05-20  parrish, initial documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  Zeu
!
!$$$ end documentation block

!  use gridmod, only: idsl5,regional
  use grib_mod

  implicit none
  character*200  :: filename
  character*200  :: filenameall(200)
  integer        :: nlevel
!
!
  type(gribfield) :: gfld
  logical :: expand=.true.
  integer :: ifile
  character(len=1),allocatable,dimension(:) :: cgrib
  integer,parameter :: msk1=32000
  integer :: lskip, lgrib,iseek
  integer :: currlen
  integer :: icount , lengrib
  integer :: listsec0(3)
  integer :: listsec1(13)
  integer year, month, day, hour, minute, second, fcst

  integer :: numfields,numlocal,maxlocal,ierr
  integer :: grib_edition
  integer :: itot
  integer :: nx,ny
  real    :: dx,dy,lat1,lon1
  real    :: scale_factor
!
!
  integer :: nn,n,j,iret
  real :: fldmax,fldmin,sum
!
!
  open(10,file='filelist_mrms',form='formatted',err=30)
  rewind (10)
  do n=1,200
     read(10,'(a)',err=20,end=40) filenameall(n)
  enddo
  close(10)
40 nlevel=n-1

  scale_factor=1.0e6
  ifile=10
  loopfile: do nn=1,nlevel
     filename=filenameall(nn)
     write(6,*) 'read in grib2 file ', trim(filename)
  
     lskip=0
     lgrib=0
     iseek=0
     icount=0
     itot=0
! Open GRIB2 file 
     call baopenr(ifile,trim(filename),iret)

     if (iret.eq.0) then
        VERSION: do

         ! Search opend file for the next GRIB2 messege (record).
           call skgb(ifile,iseek,msk1,lskip,lgrib)

         ! Check for EOF, or problem
           if (lgrib.eq.0) then
              exit
           endif

         ! Check size, if needed allocate more memory.
           if (lgrib.gt.currlen) then
              if (allocated(cgrib)) deallocate(cgrib)
              allocate(cgrib(lgrib))
              currlen=lgrib
           endif

         ! Read a given number of bytes from unblocked file.
           call baread(ifile,lskip,lgrib,lengrib,cgrib)

           if(lgrib.ne.lengrib) then
              write(*,*) 'ERROR, read_grib2 lgrib ne lengrib', &
                    lgrib,lengrib
              stop 1234
           endif

           iseek=lskip+lgrib
           icount=icount+1

         ! Unpack GRIB2 field
           call gb_info(cgrib,lengrib,listsec0,listsec1, &
                     numfields,numlocal,maxlocal,ierr)
           if(ierr.ne.0) then
              write(6,*) 'Error querying GRIB2 message',ierr
              stop
           endif
           itot=itot+numfields

           grib_edition=listsec0(2)
           if (grib_edition.ne.2) then
              exit VERSION
           endif
!           write(*,*) 'listsec0=',listsec0
!           write(*,*) 'listsec1=',listsec1
!           write(*,*) 'numfields=',numfields

! get information form grib2 file
           n=1
           call gf_getfld(cgrib,lengrib,n,.FALSE.,expand,gfld,ierr)
           year  =gfld%idsect(6)     !(FOUR-DIGIT) YEAR OF THE DATA
           month =gfld%idsect(7)     ! MONTH OF THE DATA
           day   =gfld%idsect(8)     ! DAY OF THE DATA
           hour  =gfld%idsect(9)     ! HOUR OF THE DATA
           minute=gfld%idsect(10)    ! MINUTE OF THE DATA
           second=gfld%idsect(11)    ! SECOND OF THE DATA
!           write(*,*) 'year,month,day,hour,minute,second='
!           write(*,*) year,month,day,hour,minute,second
           
!           write(*,*) 'source center =',gfld%idsect(1)
!           write(*,*) 'Indicator of model =',gfld%ipdtmpl(5)
           write(*,*) 'observation level (m)=',gfld%ipdtmpl(12)
           write(*,*) 'map projection=',gfld%igdtnum
           if (gfld%igdtnum.eq.0) then ! Lat/Lon grid aka Cylindrical
                                       ! Equidistant
              nx = gfld%igdtmpl(8)
              ny = gfld%igdtmpl(9)
              dx = gfld%igdtmpl(17)/scale_factor
              dy = gfld%igdtmpl(18)/scale_factor
              lat1 = gfld%igdtmpl(12)/scale_factor
              lon1 = gfld%igdtmpl(13)/scale_factor 
              write(*,*) 'nx,ny=',nx,ny
              write(*,*) 'dx,dy=',dx,dy
              write(*,*) 'lat1,lon1=',lat1,lon1
           else
               write(*,*) 'unknown projection'
               stop 1235
           endif

           call gf_free(gfld)

         ! Continue to unpack GRIB2 field.
           NUM_FIELDS: do n = 1, numfields
           ! e.g. U and V would =2, otherwise its usually =1
             call gf_getfld(cgrib,lengrib,n,.true.,expand,gfld,ierr)
             if (ierr.ne.0) then
               write(*,*) ' ERROR extracting field gf_getfld = ',ierr
               cycle
             endif

!             write(*,*) 'gfld%ndpts=',n,gfld%ndpts
!             write(*,*) 'gfld%unpacked=',n,gfld%unpacked

             fldmax=gfld%fld(1)
             fldmin=gfld%fld(1)
             sum=gfld%fld(1)
             do j=2,gfld%ndpts
               if (gfld%fld(j).gt.fldmax) fldmax=gfld%fld(j)
               if (gfld%fld(j).lt.fldmin) fldmin=gfld%fld(j)
               sum=sum+gfld%fld(j)
             enddo ! gfld%ndpts
             write(*,*) 'max,min,avg=',fldmax,fldmin,sum/float(gfld%ndpts)

             call gf_free(gfld)
           enddo NUM_FIELDS

        enddo VERSION ! skgb
     endif

     CALL BACLOSE(ifile,ierr)
     nullify(gfld%local)
  enddo loopfile
  stop
30 write(6,*) 'read_grib2 open filelist_mrms failed '
   stop(555)
20 write(6,*) 'read_grib2 read msmr file failed ',n
   stop(555)

end
!end subroutine
