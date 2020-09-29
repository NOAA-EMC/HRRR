!------------------------------------------------------------------------------
subroutine initialize_wrfinput_2d(cdfid,filename)
  use species
  implicit none
  interface
     subroutine nferr_0_or_die(ierr,filename,action,variable,attribute)
       character(len=*),intent(in) :: filename,action
       character(len=*),intent(in),optional :: variable,attribute
       integer, intent(in) :: ierr
     end subroutine nferr_0_or_die
  end interface
  include 'netcdf.inc'
  integer ivar,ierr,varid
  integer, intent(in) :: cdfid
  character(len=*), intent(in) :: filename
  integer :: ndims,dimids(3)

!                 DDMASS_SMOKE:FieldType = 104 ;
!                 DDMASS_SMOKE:MemoryOrder = "XY " ;
!                 DDMASS_SMOKE:description = "smoke dry deposition, accumulated" ;
!                 DDMASS_SMOKE:units = "ug/m2" ;
!                 DDMASS_SMOKE:stagger = "" ;
!                 DDMASS_SMOKE:coordinates = "XLONG XLAT XTIME" ;


  do ivar=1,num2dnew
     if(wrfinput(ivar)<1) cycle ! This var should not be in wrfinput

     ierr=nf_inq_varid(cdfid,trim(var2dnew(ivar)),varid_wrfinput(ivar))
     if(ierr/=0) then
        print 77,trim(var2dnew(ivar))
        ! Need to define the variable instead.
        ndims=3

        ierr=nf_redef(cdfid)
        call nferr_0_or_die(ierr,'nf_redef',filename)

        ierr=nf_inq_dimid(cdfid,'west_east',dimids(1))
        call nferr_0_or_die(ierr,'nf_inq_dimid',filename,'west_east')

        ierr=nf_inq_dimid(cdfid,'south_north',dimids(2))
        call nferr_0_or_die(ierr,'nf_inq_dimid',filename,'south_north')

        ierr=nf_inq_dimid(cdfid,'Time',dimids(3))
        call nferr_0_or_die(ierr,'nf_inq_dimid',filename,'Time')

        ierr=nf_def_var(cdfid,trim(var2dnew(ivar)),NF_FLOAT,ndims,dimids,varid)
        call nferr_0_or_die(ierr,'nf_def_var',filename,trim(var2dnew(ivar)))
        varid_wrfinput(ivar)=varid

        ! Attributes

        ierr=nf_put_att_int(cdfid,varid,'FieldType',NF_INT,1,104)
        call nferr_0_or_die(ierr,'nf_put_att_int',filename,trim(var2dnew(ivar)),'FieldType')

        ierr=nf_put_att_text(cdfid,varid,'MemoryOrder',3,'XY ')
        call nferr_0_or_die(ierr,'nf_put_att_text',filename,trim(var2dnew(ivar)),'MemoryOrder')

        ierr=nf_put_att_text(cdfid,varid,'description',len_trim(desc2dnew(ivar)),trim(desc2dnew(ivar)))
        call nferr_0_or_die(ierr,'nf_put_att_text',filename,trim(var2dnew(ivar)),'description')

        ierr=nf_put_att_text(cdfid,varid,'units',len_trim(unit2dnew(ivar)),trim(unit2dnew(ivar)))
        call nferr_0_or_die(ierr,'nf_put_att_text',filename,trim(var2dnew(ivar)),'units')

        ierr=nf_put_att_text(cdfid,varid,'stagger',0,'')
        call nferr_0_or_die(ierr,'nf_put_att_text',filename,trim(var2dnew(ivar)),'stagger')
        
        ierr=nf_put_att_text(cdfid,varid,'coordinates',len('XLONG XLAT XTIME'),'XLONG XLAT XTIME')
        call nferr_0_or_die(ierr,'nf_put_att_text',filename,trim(var2dnew(ivar)),'coordinates')

        ierr=nf_enddef(cdfid)
        call nferr_0_or_die(ierr,'nf_enddef',filename)
     endif
77   format('Var ',A,': have to add this variable to wrfinput')
88   format('Var ',A,': varid=',I0)
     print 88,trim(var2dnew(ivar)),varid_wrfinput(ivar)
  end do
end subroutine initialize_wrfinput_2d
!------------------------------------------------------------------------------
subroutine initializ_3d( cdfid,ncod)

  use species

  implicit none

  include 'netcdf.inc'

  integer , intent(in)                             :: cdfid, ncod         ! Unit numbers of netcdf input and output files
  integer                                          :: rcode,rcodeo,L1C,L2C,id_var,i,ii,ivtype,ndims,natts,attlen,iatt
  integer                                          :: dimids(10),dims(4)
  character (len=80)                               :: varnam, att_name, value_chr
  !
  ! Get input pressure as template, save real attributes for files to write out
  ndims=4
  dimids(1)=3
  dimids(2)=4
  dimids(3)=5
  dimids(4)=1
  natts=6
  do ii=1,num3dnew
     ! Define new 3-D matrices, get ready to fill attributes for each matrix 
     varnam=var3dnew(ii)
     write(*,*)'Before nf_def_var call, varnam,ii= ',varnam,ii
     call flush(6)
     rcodeo = nf_def_var(ncod,varnam,NF_FLOAT,ndims,dimids,idvaro3dn(ii))
     if ( rcodeo == 0 ) then
        write(*,*)'varnam,ii= ',varnam,ii
        call flush(6)
        write(*,*)'New 3-D matrix,i,ncod,id_varo=',varnam,ii,ncod,idvaro3dn(ii)
        call flush(6)
     else
        write(*,*)' Cant define new variable =',varnam,' ,Stopping'
        stop'777'
     endif
     !     rcodeo = nf_def_var_deflate(ncod,idvaro3dn(ii),0,1,4)
     !     if(rcodeo .ne. nf_noerr)then
     !     write(*,*)' Error on compression try, var =',varnam,' ,Stopping'
     !     stop'777'
     !     endif
     att_name='FieldType';attlen=1
     rcodeo = NF_PUT_ATT_INT(ncod, idvaro3dn(ii),att_name,NF_INT,attlen, 104 )
     att_name='MemoryOrder';attlen=3
     rcodeo = NF_PUT_ATT_TEXT(ncod, idvaro3dn(ii),att_name,attlen, 'XYZ' )
     att_name='description'
     value_chr=desc3dnew(ii)   ! Variable description = attribute(3)
     attlen=index(value_chr,'  ')-1
     rcodeo = NF_PUT_ATT_TEXT(ncod, idvaro3dn(ii),att_name,attlen, value_chr )
     att_name='units'
     value_chr=unit3dnew(ii)   ! Variable description = attribute(3)
     attlen=index(value_chr,'  ')-1
     rcodeo = NF_PUT_ATT_TEXT(ncod, idvaro3dn(ii),att_name,attlen, value_chr )
     att_name='stagger';attlen=0
     rcodeo = NF_PUT_ATT_TEXT(ncod, idvaro3dn(ii),att_name,attlen, '' )
     !     att_name='coordinates';value_chr='XLONG XLAT'
     att_name='coordinates';value_chr='longitude latitude'
     attlen=index(value_chr,'  ')-1
     rcodeo = NF_PUT_ATT_TEXT(ncod, idvaro3dn(ii),att_name,attlen, value_chr )
     att_name='grid_mapping';value_chr='latitude_longitude'
     attlen=index(value_chr,'  ')-1
     rcodeo = NF_PUT_ATT_TEXT(ncod, idvaro3dn(ii),att_name,attlen, value_chr )
  enddo  ! Endof i=1,num3dnew loop
  !       write(*,'(A,A," : ",A)') ' Time attribs. in output netcdf ',att_name(1:40),value_chr(1:attlen)

end subroutine initializ_3d
!-------------------------------------------------------------------------------------------
subroutine initializ_2d( cdfid,ncod)

  use species

  implicit none

  include 'netcdf.inc'

  integer , intent(in)                             :: cdfid, ncod         ! Unit numbers of netcdf input and output files
  integer                                          :: rcode,rcodeo,L1C,L2C,id_var,i,ii,ivtype,ndims,natts,attlen,iatt
  integer                                          :: dimids(10),dims(4)
  character (len=80)                               :: varnam, att_name, value_chr
  !
  ndims=3
  dimids(1)=3
  dimids(2)=4
  dimids(3)=1
  natts=6
  do ii=1,num2dnew
     ! Define new 2-D matrices, get ready to fill attributes for each matrix 
     varnam=var2dnew(ii)
     rcodeo = nf_def_var(ncod,varnam,NF_FLOAT,ndims,dimids,idvaro2dn(ii))
     write(*,*)'New 2-D matrix,i,id_varo=',ii,idvaro2dn(ii),varnam
     call flush(6)
     att_name='FieldType';attlen=1
     rcodeo = NF_PUT_ATT_INT(ncod, idvaro2dn(ii),att_name,NF_INT,attlen, 104 )
     att_name='MemoryOrder';attlen=3
     rcodeo = NF_PUT_ATT_TEXT(ncod, idvaro2dn(ii),att_name,attlen, 'XY ' )
     att_name='description'
     value_chr=desc2dnew(ii)   ! Variable description = attribute(3)
     attlen=index(value_chr,'  ')-1
     rcodeo = NF_PUT_ATT_TEXT(ncod, idvaro2dn(ii),att_name,attlen, value_chr )
     att_name='units'
     value_chr=unit2dnew(ii)   ! Variable description = attribute(3)
     attlen=index(value_chr,'  ')-1
     rcodeo = NF_PUT_ATT_TEXT(ncod, idvaro2dn(ii),att_name,attlen, value_chr )
     att_name='stagger';attlen=0
     rcodeo = NF_PUT_ATT_TEXT(ncod, idvaro2dn(ii),att_name,attlen, '' )
     !     att_name='coordinates';value_chr='XLONG XLAT'
     att_name='coordinates';value_chr='longitude latitude'
     attlen=index(value_chr,'  ')-1
     rcodeo = NF_PUT_ATT_TEXT(ncod, idvaro2dn(ii),att_name,attlen, value_chr )
     att_name='grid_mapping';value_chr='latitude_longitude'
     attlen=index(value_chr,'  ')-1
     rcodeo = NF_PUT_ATT_TEXT(ncod, idvaro2dn(ii),att_name,attlen, value_chr )
  enddo  ! Endof i=1,num2dnew loop
  !       write(*,'(A,A," : ",A)') ' Time attribs. in output netcdf ',att_name(1:40),value_chr(1:attlen)

end subroutine initializ_2d
!------------------------------------------------------------------------------
subroutine ncf_readwrite( cdfid,ncod,intime,iflag)

  use species, only: num_rw, var_rw

  implicit none

  include 'netcdf.inc'

  integer , intent(in)                             :: cdfid,ncod,iflag,intime  ! Unit numbers of netcdf input and output files
  integer , save                                   :: nvars
  integer                                          :: rcode,rcodeo,L1C,L2C,id_var,i,j,ii,ivtype,ndims,natts,attlen,iatt
  integer                                          :: dimids(10),dims(4),istart(4),iend(4)
  character (len=80)                               :: varnam, att_name, value_chr, name_sv, unit_sv
  integer , save                                   :: idvarorw(num_rw),idvarirw(num_rw),ndimkeep(num_rw),dimkeep(4,num_rw)
  integer , save                                   :: idx_lat,idx_lon
  real*8, save, allocatable, dimension(:)          :: r1d_lt,r1d_ln
  real, save, allocatable, dimension(:)          :: r1s_lt,r1s_ln ! dsingle precision lat/lon for output netcdf
  real, save, allocatable, dimension(:,:)          :: r2d_ht,r2d_ln,r2d_lt
  real, allocatable, dimension(:,:)                :: r2d
  real, allocatable, dimension(:,:,:)              :: r3d
  !
  istart=1
  if(iflag.eq.0)then
     ! 7/18/17 Force 1-D matrices latitude and longitude into new file
     ! Latitude
     rcode = nf_inq_varid ( cdfid, "latitude", id_var )
     if ( rcode == 0 ) then
        rcode = nf_inq_var( cdfid, id_var , varnam, ivtype, ndims, dimids, natts )
        write(*,*)'Latitude,id_var,varnam,ivtype,ndims=',id_var,varnam,ivtype,ndims
        call flush(6)
        do j=1,ndims
           rcode = nf_inq_dimlen( cdfid, dimids(j), dims(j) )
           write(*,*)'Latitude,j,dimids,dims=',j,dimids(j),dims(j)
           call flush(6)
        enddo
        if (allocated(r1d_lt)) deallocate(r1d_lt)
        allocate ( r1d_lt(dims(1)))
        iend(1)=dims(1)
        idx_lat=dims(1)
        call ncvgt( cdfid,id_var,istart,iend,r1d_lt,rcode)
        write(*,*)'r1d_lt(1),dims(1),r1d_lt(dims(1))=',r1d_lt(1),dims(1),r1d_lt(idx_lat)
        call flush(6)
     endif
     dimids(1)=4
     rcodeo = nf_def_var(ncod,'latitude',NF_FLOAT,1,dimids,i)
     write(*,*)'Add latitude to file,ncod,id_varo=',ncod,i
     call flush(6)
     do iatt = 1,natts
        rcode = nf_inq_attname(cdfid,id_var,iatt,att_name)
        rcodeo=nf_copy_att(cdfid,id_var,att_name,ncod,i)
     enddo
     ! Longitude
     rcode = nf_inq_varid ( cdfid, "longitude", id_var )
     if ( rcode == 0 ) then
        rcode = nf_inq_var( cdfid, id_var , varnam, ivtype, ndims, dimids, natts )
        write(*,*)'Longitude,id_var,varnam,ivtype,ndims=',id_var,varnam,ivtype,ndims
        call flush(6)
        do j=1,ndims
           rcode = nf_inq_dimlen( cdfid, dimids(j), dims(j) )
           write(*,*)'Longitude,j,dimids,dims=',j,dimids(j),dims(j)
           call flush(6)
        enddo
        if (allocated(r1d_ln)) deallocate(r1d_ln)
        allocate ( r1d_ln(dims(1)))
        iend(1)=dims(1)
        idx_lon=dims(1)
        call ncvgt( cdfid,id_var,istart,iend,r1d_ln,rcode)
        write(*,*)'r1d_ln(1),dims(1),r1d_ln(dims(1))=',r1d_ln(1),dims(1),r1d_ln(idx_lon)
        call flush(6)
     endif
     dimids(1)=3
     rcodeo = nf_def_var(ncod,'longitude',NF_FLOAT,1,dimids,i)
     write(*,*)'Add longitude to file,ncod,id_varo=',ncod,i
     call flush(6)
     do iatt = 1,natts
        rcode = nf_inq_attname(cdfid,id_var,iatt,att_name)
        rcodeo=nf_copy_att(cdfid,id_var,att_name,ncod,i)
     enddo
     ! End lat/lon define
     rcode = nf_inq_nvars ( cdfid, nvars )
     write(*,*)'Initialize readwrite, nvars= ',nvars
     call flush(6)
     idvarirw=0
     do ii=1,nvars
        id_var=ii
        rcode = nf_inq_var( cdfid, id_var , varnam, ivtype, ndims, dimids, natts )
        L1C = max(1,index(varnam,' ')-1)
        do i=1,num_rw
           L2C = max(1,index(var_rw(i),' ')-1)
           if(varnam(1:L1C).EQ.var_rw(i)(1:L2C))then
              idvarirw(i)=ii
              ndimkeep(i)=ndims
              ! Define new 2 or 3-D matrix, get ready to fill attributes for each matrix 
              write(*,*)'read-write Initialize ',varnam(1:L1C),' ndims=',ndims
              call flush(6)
              do j=1,ndims
                 rcode = nf_inq_dimlen( cdfid, dimids(j), dims(j) )
                 dimkeep(j,i)=dims(j)
              enddo
              ! Pull off units attribute and UKMO standard name for attributes to write to new file
              do iatt = 1,natts
                 rcode = nf_inq_attname(cdfid,id_var,iatt,att_name)
                 L2C = max(1,index(att_name,' ')-1)
                 if(att_name(1:L2C).eq.'standard_name')then
                    rcode = NF_GET_ATT_TEXT(cdfid, id_var, att_name, value_chr )
                    name_sv=value_chr
                 endif
                 if(att_name(1:L2C).eq.'units')then
                    rcode = NF_GET_ATT_TEXT(cdfid, id_var, att_name, value_chr )
                    unit_sv=value_chr
                 endif
              enddo
              !
              if(ndims.eq.3)then
                 ndims=3
                 dimids(1)=3
                 dimids(2)=4
                 dimids(3)=1
              elseif(ndims.eq.4)then
                 ndims=4
                 dimids(1)=3
                 dimids(2)=4
                 dimids(3)=5
                 dimids(4)=1
              else
                 write(*,*)'Dimen of readwrite ',var_rw(i),' not 3or 4'
                 write(*,*)'Dimension = ',ndimkeep(i),',Stopping'
                 call flush(6)
                 stop'777'
              endif
              rcodeo = nf_def_var(ncod,varnam,NF_FLOAT,ndims,dimids,idvarorw(i))
              write(*,*)'New matrix,ii,ncod,id_varo=',ii,ncod,idvarorw(i),varnam
              call flush(6)
              natts=6
              att_name='FieldType';attlen=1
              rcodeo = NF_PUT_ATT_INT(ncod, idvarorw(i),att_name,NF_INT,attlen, 104 )
              att_name='MemoryOrder';attlen=3
              if(ndims.eq.3)then
                 rcodeo = NF_PUT_ATT_TEXT(ncod, idvarorw(i),att_name,attlen, 'XY ' )
              elseif(ndims.eq.4)then
                 rcodeo = NF_PUT_ATT_TEXT(ncod, idvarorw(i),att_name,attlen, 'XYZ' )
              endif
              att_name='description'
              value_chr=name_sv
              attlen=index(value_chr,'  ')-1
              rcodeo = NF_PUT_ATT_TEXT(ncod, idvarorw(i),att_name,attlen, value_chr )
              att_name='units'
              value_chr=unit_sv
              attlen=index(value_chr,'  ')-1
              rcodeo = NF_PUT_ATT_TEXT(ncod, idvarorw(i),att_name,attlen, value_chr )
              att_name='stagger';attlen=0
              rcodeo = NF_PUT_ATT_TEXT(ncod, idvarorw(i),att_name,attlen, '' )
              !     att_name='coordinates';value_chr='XLONG XLAT'
              att_name='coordinates';value_chr='longitude latitude'
              attlen=index(value_chr,'  ')-1
              rcodeo = NF_PUT_ATT_TEXT(ncod, idvarorw(i),att_name,attlen, value_chr )
              att_name='grid_mapping';value_chr='latitude_longitude'
              attlen=index(value_chr,'  ')-1
              rcodeo = NF_PUT_ATT_TEXT(ncod, idvarorw(i),att_name,attlen, value_chr )
              !        endif
           endif ! END OF if(varnam(1:L1C).EQ.var_rw(1:L2C)then
        enddo ! END OF do i=1,num_rw
     enddo ! END OF do ii=1,nvars
     ! Check to make sure all variables you want to directly transfer are in the input file
     do i=1,num_rw
        if(idvarirw(i).eq.0)then
           write(*,*)' Missing variable in input file=',var_rw(i),' Stopping'
           stop'777'
        endif
     enddo
  elseif(intime.eq.0)then ! conditional iflag>0 below, transfer time indep. vars (lat,lon) in data mode
     rcodeo = nf_inq_varid ( ncod, "latitude", id_var )
     write(*,*)'Lat. id_var,idx_lat to write-file=',id_var,idx_lat
     call flush(6)
     if (allocated(r1s_lt)) deallocate(r1s_lt)
     allocate ( r1s_lt(idx_lat))
     do i=1,idx_lat
        r1s_lt(i)=r1d_lt(i)
     enddo
     write(*,*)'r1s_lt(1),dims(1),r1s_lt(dims(1))=',r1s_lt(1),idx_lat,r1s_lt(idx_lat)
     if ( rcodeo == 0 ) then
        rcodeo = NF_PUT_VAR_REAL( ncod,id_var,r1s_lt)
        write(*,*)'On Lat write,rcodeo=',rcodeo
        call flush(6)
     else
        write(*,*)'No latitude in new file, stopping'
        call flush(6)
        STOP'777'
     endif
     rcodeo = nf_inq_varid ( ncod, "longitude", id_var )
     write(*,*)'Lon. rcodeo,id_var,idx_lon to write-file=',rcodeo,id_var,idx_lon
     call flush(6)
     if (allocated(r1s_ln)) deallocate(r1s_ln)
     allocate ( r1s_ln(idx_lon))
     do i=1,idx_lon
        r1s_ln(i)=r1d_ln(i)
     enddo
     if ( rcodeo == 0 ) then
        write(*,*)'r1s_ln(1),dims(1),r1s_ln(dims(1))=',r1s_ln(1),idx_lon,r1s_ln(idx_lon)
        rcodeo = NF_PUT_VAR_REAL( ncod,id_var,r1s_ln)
        deallocate(r1d_ln,r1s_ln)
        write(*,*)'On Lon write,rcodeo=',rcodeo
        call flush(6)
     else
        write(*,*)'No longitude in new file, stopping'
        call flush(6)
        STOP'777'
     endif
  else ! conditional iflag>0 below, if(iflag.eq.0)then
     do i=1,num_rw
        write(*,*)'retrieving ',var_rw(i),' i,idvarirw,itimes=',i,idvarirw(i),intime
        call flush(6)
        !        L2C = max(1,index(var_rw(i),' ')-1)
        iend(1)=dimkeep(1,i)
        iend(2)=dimkeep(2,i)
        write(*,*)'ndimkeep,dimkeep(1)(2)=',ndimkeep(i),dimkeep(1,i),dimkeep(2,i)
        call flush(6)
        if(ndimkeep(i).eq.3)then
           istart(3)=intime
           iend(3)=1
           iend(1)=dimkeep(1,i)
           iend(2)=dimkeep(2,i)
           if (allocated(r2d)) deallocate(r2d)
           allocate ( r2d(dimkeep(1,i),dimkeep(2,i)))
           call ncvgt( cdfid,idvarirw(i),istart,iend,r2d,rcode)
           rcodeo = NF_PUT_VARA_REAL( ncod,idvarorw(i),istart,iend,r2d)
        elseif(ndimkeep(i).eq.4)then
           istart(3)=1
           iend(3)=dimkeep(3,i)
           istart(4)=intime
           iend(4)=1
           if (allocated(r3d)) deallocate(r3d)
           allocate ( r3d(dimkeep(1,i),dimkeep(2,i),dimkeep(3,i)))
           call ncvgt( cdfid,idvarirw(i),istart,iend,r3d,rcode)
           rcodeo = NF_PUT_VARA_REAL( ncod,idvarorw(i),istart,iend,r3d)
        else
           write(*,*)'Dimen of readwrite ',var_rw(i),' not 3or 4'
           write(*,*)'Dimension = ',ndimkeep(i),',Stopping'
           call flush(6)
           stop'777'
        endif
     enddo
  endif ! END OF if(iflag.eq.0)then
  write(*,*)'Done with wrf_readwrite,intime,iflag=',intime,iflag
  call flush(6)

end subroutine ncf_readwrite
