subroutine read_lightning_bufr  &
           (lightningfile,max_numStrike,cdateana,minute,trange_start,trange_end,&
            numStrike,rlon,rlat,itime,iStrike)
!
! read all observations out from prepbufr. 
! read bufr table from prepbufr file
!
 use constants, only: r60inv
 use kinds, only: r_kind,i_kind

 implicit none

 CHARACTER*180,intent(in) :: lightningfile
 INTEGER,intent(in)       :: max_numStrike
 CHARACTER*10,intent(in)  :: cdateana
 integer, intent(in)      :: minute
 real,intent(in)          :: trange_start,trange_end
 INTEGER,intent(out)      :: numStrike
 real,intent(out)         :: rlon(max_numStrike) 
 real,intent(out)         :: rlat(max_numStrike)
 integer,intent(out)      :: itime(max_numStrike) 
 integer,intent(out)      :: istrike(max_numStrike)

 integer, parameter :: mxmn=35, mxlv=1
 character(80):: hdstr='YEAR  MNTH  DAYS  HOUR  MINU CLATH  CLONH'
 character(80):: obstr='AMPLS  PLRTS  OWEP  NOFL'
 real(8) :: hdr(mxmn),obs(mxmn,mxlv)

 INTEGER        :: ireadmg,ireadsb

 character(8)   :: subset
 integer        :: unit_in,idate,nmsg,ntb,ntball

 integer(i_kind),dimension(5):: idate5
 integer(i_kind) minobs,minan,timediffmin
 integer        :: i,k,iret
!
!
!
 read(cdateana,'(I4,I2,I2,I2)') (idate5(i),i=1,4)
 idate5(5)=minute
 call w3fs21(idate5,minan)    !  analysis ref time in minutes
                              !  relative to historic date
 write(*,*) 'Analysis time=',idate5,minan
!
 unit_in=10
! open(24,file='lghtngbufr.table')
 open(unit_in,file=trim(lightningfile),form='unformatted',status='old')
 call openbf(unit_in,'IN',unit_in)
! call dxdump(unit_in,24)
 call datelen(10)
 nmsg=0
 ntb = 0
 ntball = 0
 msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     nmsg=nmsg+1
!     write(*,*)
!     write(*,'(3a,i10)') 'subset=',subset,' cycle time =',idate
     sb_report: do while (ireadsb(unit_in) == 0)
        ntball = ntball + 1
        call ufbint(unit_in,hdr,mxmn,1,iret,hdstr)
        call ufbint(unit_in,obs,mxmn,1,iret,obstr)

        idate5(1)=int(hdr(1))
        idate5(2)=int(hdr(2))
        idate5(3)=int(hdr(3))
        idate5(4)=int(hdr(4))
        idate5(5)=int(hdr(5))
        call w3fs21(idate5,minobs)    !  obs ref time in minutes relative
                                     !    to historic date
! Add obs reference time, then subtract analysis time to get obs
! time relative to analysis

        timediffmin=minobs-minan
        if(timediffmin > trange_start .and. timediffmin < trange_end ) then
           ntb = ntb+1
           if(ntb > max_numStrike) then
              write(*,*) 'Too many observations, need to increase ',&
                         ' max_numStrike!'
              stop 12345
           endif
           if(int(hdr(1)) > 2000) then
              itime(ntb)=(int(hdr(1))-2000)*100000000
           else
              itime(ntb)=(int(hdr(1))-1900)*100000000
           endif
           itime(ntb)=itime(ntb)+int(hdr(2))*1000000 + &
                   int(hdr(3))*10000+int(hdr(4))*100+int(hdr(5))
           rlat(ntb)=hdr(6)
           rlon(ntb)=hdr(7)
           if(subset=='NC007001') then
             if(obs(1,1) > 0.0) istrike(ntb)=int(obs(4,1))
           elseif(subset=='NC007002') then
             if(obs(1,1) > 0.0) istrike(ntb)=1 ! obs(4,1)=0 for NC007002
           else
              write(*,*) 'unkwon lightning type ', subset
              stop 1234
           endif   ! lightning data type
        endif  ! check time
!        write(*,*)
!        write(*,'(I10,8f14.1)') ntb,(hdr(i),i=1,7)
!        write(*,'(9f14.1)') (obs(i,1),i=1,4)
     enddo sb_report
 enddo msg_report
 call closbf(unit_in)

 numStrike=ntb
 write(*,*) 'There are ',numStrike,' out of total ',ntball,&
             ' lightning observations in time window ',trange_start,&
             ' to ',trange_end, ' minute'

end subroutine read_lightning_bufr
