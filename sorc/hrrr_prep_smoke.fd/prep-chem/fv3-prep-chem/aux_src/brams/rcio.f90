!############################# Change Log ##################################
! 5.0.0
!
!###########################################################################
!  Copyright (C)  1990, 1995, 1999, 2000, 2003 - All Rights Reserved
!  Regional Atmospheric Modeling System - RAMS
!###########################################################################

subroutine commio (cfile,io,iun)

use mem_grid

implicit none
integer :: iun
character(len=*) :: io,cfile

!     This routine reads or writes the history and analysis file common blocks.

integer, external :: cio_i,cio_f,cio_f8,cio_c
character(len=2) :: cng
integer :: irw,ie,ng

IF(IO.EQ.'READ') irw=1
IF(IO.EQ.'WRITE') irw=2
!      print*,'in commio',cfile,' ',io,' ',iun

ie=cio_i(iun,irw,'iversion',iversion,1)
ie=cio_c(iun,irw,'expnme',expnme,1)
!ie=cio_i(iun,irw,'ioutput',ioutput,1)
ie=cio_i(iun,irw,'if_adap',if_adap,1)
ie=cio_i(iun,irw,'ngrids',ngrids,1)
ie=cio_i(iun,irw,'nzg',nzg,1)
ie=cio_i(iun,irw,'nzs',nzs,1)
ie=cio_i(iun,irw,'naddsc',naddsc,1)
ie=cio_f(iun,irw,'time',time,1)
ie=cio_f(iun,irw,'ztop',ztop,1)
ie=cio_f(iun,irw,'polelat',polelat,1)
ie=cio_f(iun,irw,'polelon',polelon,1)
ie=cio_f(iun,irw,'dzrat',dzrat,1)
ie=cio_f(iun,irw,'dzmax',dzmax,1)

ie=cio_i(iun,irw,'nnxp',nnxp,ngrids)
ie=cio_i(iun,irw,'nnyp',nnyp,ngrids)
ie=cio_i(iun,irw,'nnzp',nnzp,ngrids)
!ie=cio_i(iun,irw,'nnqparm',nnqparm,ngrids)
ie=cio_i(iun,irw,'nndtrat',nndtrat,ngrids)
ie=cio_i(iun,irw,'nstratx',nstratx,ngrids)
ie=cio_i(iun,irw,'nstraty',nstraty,ngrids)
ie=cio_i(iun,irw,'ngbegun',ngbegun,ngrids)
ie=cio_i(iun,irw,'nnacoust',nnacoust,ngrids)
ie=cio_i(iun,irw,'nxtnest',nxtnest,ngrids)
ie=cio_i(iun,irw,'nnsttop',nnsttop,ngrids)
ie=cio_i(iun,irw,'nnstbot',nnstbot,ngrids)
ie=cio_i(iun,irw,'ninest',ninest,ngrids)
ie=cio_i(iun,irw,'njnest',njnest,ngrids)
ie=cio_i(iun,irw,'nknest',nknest,ngrids)
!ie=cio_i(iun,irw,'idiffk',idiffk,ngrids)
ie=cio_f(iun,irw,'gridu',gridu,ngrids)
ie=cio_f(iun,irw,'gridv',gridv,ngrids)
!ie=cio_f(iun,irw,'akmin',akmin,ngrids)
!ie=cio_f(iun,irw,'csz',csz,ngrids)
!ie=cio_f(iun,irw,'csx',csx,ngrids)
!ie=cio_f(iun,irw,'xkhkm',xkhkm,ngrids)
!ie=cio_f(iun,irw,'zkhkm',zkhkm,ngrids)
ie=cio_f(iun,irw,'centlat',centlat,ngrids)
ie=cio_f(iun,irw,'centlon',centlon,ngrids)
ie=cio_f(iun,irw,'dimove',dimove,ngrids)
ie=cio_f(iun,irw,'djmove',djmove,ngrids)
ie=cio_f(iun,irw,'deltazn',deltazn,ngrids)
ie=cio_f(iun,irw,'deltaxn',deltaxn,ngrids)
ie=cio_f(iun,irw,'deltayn',deltayn,ngrids)
ie=cio_f(iun,irw,'platn',platn,ngrids)
ie=cio_f(iun,irw,'plonn',plonn,ngrids)
ie=cio_f(iun,irw,'zz',zz,nnzp(1))

ie=cio_i(iun,irw,'nestz1',nestz1,1)
ie=cio_i(iun,irw,'nestz2',nestz2,1)
ie=cio_i(iun,irw,'nstratz1',nstratz1,nnzp(1))
ie=cio_i(iun,irw,'nstratz2',nstratz2,nnzp(1))

do ng=1,ngrids
   write(cng,1) ng
1       format(i2.2)
   ie=cio_f(iun,irw,'xmn'//cng,xmn(1,ng),nnxp(ng))
   ie=cio_f(iun,irw,'xtn'//cng,xtn(1,ng),nnxp(ng))
   ie=cio_f(iun,irw,'ymn'//cng,ymn(1,ng),nnyp(ng))
   ie=cio_f(iun,irw,'ytn'//cng,ytn(1,ng),nnyp(ng))
   ie=cio_f(iun,irw,'zmn'//cng,zmn(1,ng),nnzp(ng))
   ie=cio_f(iun,irw,'ztn'//cng,ztn(1,ng),nnzp(ng))
   ie=cio_f(iun,irw,'dzmn'//cng,dzmn(1,ng),nnzp(ng))
   ie=cio_f(iun,irw,'dztn'//cng,dztn(1,ng),nnzp(ng))
   !ie=cio_f(iun,irw,'u01dn'//cng,u01dn(1,ng),nnzp(ng))
   !ie=cio_f(iun,irw,'v01dn'//cng,v01dn(1,ng),nnzp(ng))
   !ie=cio_f(iun,irw,'pi01dn'//cng,pi01dn(1,ng),nnzp(ng))
   !ie=cio_f(iun,irw,'th01dn'//cng,th01dn(1,ng),nnzp(ng))
   !ie=cio_f(iun,irw,'dn01dn'//cng,dn01dn(1,ng),nnzp(ng))
   !ie=cio_f(iun,irw,'rt01dn'//cng,rt01dn(1,ng),nnzp(ng))
enddo

!ie=cio_i(iun,irw,'kroot',kroot,nvtyp+nvtyp_teb)

ie=cio_i(iun,irw,'itopo',itopo,1)
ie=cio_i(iun,irw,'initial',initial,1)
ie=cio_i(iun,irw,'impl',impl,1)
!ie=cio_i(iun,irw,'iinput',iinput,1)
ie=cio_i(iun,irw,'jdim',jdim,1)
ie=cio_i(iun,irw,'iadvl',iadvl,1)
ie=cio_i(iun,irw,'iadvf',iadvf,1)
!ie=cio_i(iun,irw,'lonrad',lonrad,1)
ie=cio_i(iun,irw,'lsflg',lsflg,1)
ie=cio_i(iun,irw,'ibnd',ibnd,1)
ie=cio_i(iun,irw,'jbnd',jbnd,1)
ie=cio_i(iun,irw,'icorflg',icorflg,1)

!ie=cio_i(iun,irw,'ilwrtyp',ilwrtyp,1)
!ie=cio_i(iun,irw,'iswrtyp',iswrtyp,1)
!ie=cio_i(iun,irw,'iref',iref,1)
!ie=cio_i(iun,irw,'jref',jref,1)
ie=cio_i(iun,irw,'ihtran',ihtran,1)
ie=cio_i(iun,irw,'nfpt',nfpt,1)
!ie=cio_i(iun,irw,'nsndg',nsndg,1)
ie=cio_i(iun,irw,'ideltat',ideltat,1)
ie=cio_i(iun,irw,'nacoust',nacoust,1)
ie=cio_i(iun,irw,'iflag',iflag,1)
!ie=cio_i(iun,irw,'ntopsmth',ntopsmth,1)
!ie=cio_i(iun,irw,'izflat',izflat,1)
ie=cio_i(iun,irw,'iyear1',iyear1,1)
ie=cio_i(iun,irw,'imonth1',imonth1,1)
ie=cio_i(iun,irw,'idate1',idate1,1)
ie=cio_i(iun,irw,'itime1',itime1,1)
!ie=cio_i(iun,irw,'isfcl',isfcl,1)
ie=cio_i(iun,irw,'npatch',npatch,1)
!ie=cio_i(iun,irw,'nvegpat',nvegpat,1)
!ie=cio_i(iun,irw,'level',level,1)
!ie=cio_i(iun,irw,'irain',irain,1)
!ie=cio_i(iun,irw,'ipris',ipris,1)
!ie=cio_i(iun,irw,'isnow',isnow,1)
!ie=cio_i(iun,irw,'iaggr',iaggr,1)
!ie=cio_i(iun,irw,'igraup',igraup,1)
!ie=cio_i(iun,irw,'icloud',icloud,1)
!ie=cio_i(iun,irw,'ihail',ihail,1)

!ie=cio_f(iun,irw,'brunt',brunt,1)
!ie=cio_f(iun,irw,'wcldbs',wcldbs,1)
!ie=cio_f(iun,irw,'drtcon',drtcon,1)
!ie=cio_f(iun,irw,'rmin',rmin,1)
!ie=cio_f(iun,irw,'radfrq',radfrq,1)
ie=cio_f(iun,irw,'distim',distim,1)
!ie=cio_f(iun,irw,'seatmp',seatmp,1)
!ie=cio_f(iun,irw,'confrq',confrq,1)
!ie=cio_f(iun,irw,'rmax',rmax,1)
ie=cio_f(iun,irw,'eps',eps,1)
!ie=cio_f(iun,irw,'albedo',albedo,1)
!ie=cio_f(iun,irw,'dthcon',dthcon,1)
ie=cio_f(iun,irw,'cphas',cphas,1)
!ie=cio_f(iun,irw,'topref',topref,1)
ie=cio_f(iun,irw,'sspct',sspct,1)
!ie=cio_f(iun,irw,'rparm',rparm,1)
!ie=cio_f(iun,irw,'pparm',pparm,1)
!ie=cio_f(iun,irw,'sparm',sparm,1)
!ie=cio_f(iun,irw,'aparm',aparm,1)
!ie=cio_f(iun,irw,'gparm',gparm,1)
!ie=cio_f(iun,irw,'cparm',cparm,1)
!ie=cio_f(iun,irw,'hparm',hparm,1)

!ie=cio_f(iun,irw,'cfmas',cfmas,nhcat)
!ie=cio_f(iun,irw,'pwmas',pwmas,nhcat)

!ie=cio_f(iun,irw,'us',us,maxsndg)
!ie=cio_f(iun,irw,'vs',vs,maxsndg)
!ie=cio_f(iun,irw,'ts',ts,maxsndg)
!ie=cio_f(iun,irw,'thds',thds,maxsndg)
!ie=cio_f(iun,irw,'ps',ps,maxsndg)
!ie=cio_f(iun,irw,'hs',hs,maxsndg)

!ie=cio_f(iun,irw,'slden',slden,nstyp)
!ie=cio_f(iun,irw,'slcpd',slcpd,nstyp)
!ie=cio_f(iun,irw,'slbs',slbs,nstyp)
!ie=cio_f(iun,irw,'slcond',slcond,nstyp)
!ie=cio_f(iun,irw,'slcons',slcons,nstyp)
!ie=cio_f(iun,irw,'slmsts',slmsts,nstyp)
!ie=cio_f(iun,irw,'slpots',slpots,nstyp)
!ie=cio_f(iun,irw,'ssand',ssand,nstyp)
!ie=cio_f(iun,irw,'sclay',sclay,nstyp)
!ie=cio_f(iun,irw,'sorgan',sorgan,nstyp)
!ie=cio_f(iun,irw,'sporo',sporo,nstyp)
!ie=cio_f(iun,irw,'soilcp',soilcp,nstyp)
!ie=cio_f(iun,irw,'slfc',slfc,nstyp)
!ie=cio_f(iun,irw,'emisg',emisg,nstyp)

!ie=cio_f(iun,irw,'emisv',emisv,nvtyp+nvtyp_teb)

!ie=cio_f(iun,irw,'root',root,nzgmax*(nvtyp+nvtyp_teb))
!ie=cio_f(iun,irw,'slz',slz,nzg)

!ie=cio_f(iun,irw,'cmin',cmin,1)
!ie=cio_f(iun,irw,'corg',corg,1)
!ie=cio_f(iun,irw,'cwat',cwat,1)
!ie=cio_f(iun,irw,'cair',cair,1)
!ie=cio_f(iun,irw,'cka',cka,1)
!ie=cio_f(iun,irw,'ckw',ckw,1)

return
end

!---------------------------------------------------------

subroutine cio_pos_file(iun,cstr,ierr)
implicit none
integer :: iun,ierr
character(len=*) :: cstr
character(len=256) :: line,csearch

integer :: nl,nc,iend,ilen

!      print*,'cio_pos:',iun,cstr

iend=0
1    continue
do nl=1,1000000
   read(iun,10,end=100) line
10      format(a)
   ilen=len(cstr)
   csearch='__'//cstr(1:ilen)
   nc=index(line,csearch(1:ilen+2) )
!         print*,'cio_pos:',nl,nc,line
   if(nc.eq.1) then
      ierr=0
!            print*,'---- Name found on header file:',cstr
      return
   endif
enddo

100  continue
if(iend.eq.1) then
   ierr=1
   print*,'---- Name NOT found on header file:',cstr
   rewind(iun)
   return
endif
rewind(iun)
iend=1
goto 1

end

!---------------------------------------------------------

integer function cio_i(iun,irw,cstr,ia,n)
implicit none
integer :: iun,irw,n
integer ia(*)
character(len=*) :: cstr
character(len=256) :: string
integer :: nn,i

if (irw.eq.1) then
   call cio_pos_file (iun,cstr,cio_i)
   if(cio_i.eq.1) return
   read(iun,*) nn
   read(iun,*) (ia(i),i=1,nn)
elseif(irw.eq.2) then
   write(iun,20) cstr
20      format('__',a)
   write(iun,*) n
   write(iun,11) (ia(i),i=1,n)
11      format(i6)
   cio_i=0
endif

return
end

!---------------------------------------------------------

integer function cio_f(iun,irw,cstr,ia,n)
implicit none
integer :: iun,irw,n
real ia(*)
character(len=*) :: cstr
character(len=256) :: string
integer :: nn,i

if (irw.eq.1) then
   call cio_pos_file (iun,cstr,cio_f)
   if(cio_f.eq.1) return
   read(iun,*) nn
   read(iun,*) (ia(i),i=1,nn)
elseif(irw.eq.2) then
   write(iun,20) cstr
20      format('__',a)
   write(iun,*) n
   write(iun,11) (ia(i),i=1,n)
11      format(e16.8)
   cio_f=0
endif

return
end

!---------------------------------------------------------

integer function cio_f8(iun,irw,cstr,ia,n)
implicit none
integer :: iun,irw,n
real(kind=8) :: ia(*)
character(len=*) :: cstr
character(len=256) :: string
integer :: nn,i

if (irw.eq.1) then
   call cio_pos_file (iun,cstr,cio_f8)
   if(cio_f8.eq.1) return
   read(iun,*) nn
   read(iun,*) (ia(i),i=1,nn)
elseif(irw.eq.2) then
   write(iun,20) cstr
20      format('__',a)
   write(iun,*) n
   write(iun,11) (ia(i),i=1,n)
11      format(e24.16)
   cio_f8=0
endif

return
end

!---------------------------------------------------------

integer function cio_c(iun,irw,cstr,ia,n)
implicit none
integer :: iun,irw,n
character(len=*) :: ia(*)
character(len=*) :: cstr
character(len=256) :: string
integer :: nn,i

if (irw.eq.1) then
   call cio_pos_file (iun,cstr,cio_c)
   if(cio_c.eq.1) return
   read(iun,*) nn
   read(iun,10) (ia(i),i=1,nn)
elseif(irw.eq.2) then
   write(iun,20) cstr
20      format('__',a)
   write(iun,*) n
   write(iun,10) (ia(i),i=1,n)
10      format(a)
   cio_c=0
endif

return
end


