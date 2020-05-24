!
!   lee_wrfchemius.f90
!   
!
!   Created by Agustin Garcia on 28/04/18.
!   Copyright 2018 Universidad Nacional Autonoma de Mexico. All rights reserved.
!
!   Lee archivos netcdf de wrfinput
!     26 abril para MOZART
!
subroutine lee_wrf(hh,country)
use netcdf
use var_suma
implicit none
  integer, intent ( in) :: hh
  integer i,j,l,it,ikk
  integer ncid,lat_varid,lon_varid,btDimID
  integer land_catID,dimlon,dimlat,dimZ !dimensions ID
  integer latVarId,lonVarId,zdimId  ! dimensions values
  integer nTimes,idcTime
  integer :: nDims2, nVars, nGlobalAtts, unlimDimID
  integer,dimension(nDims) ::dim
  character (len=3 ), intent ( in) ::country
  character (len=20 ) :: name,filename
  character (len = *), parameter :: LAT_NAME = "XLAT"
  character (len = *), parameter :: LON_NAME = "XLONG"
  character (len = *), parameter :: REC_NAME = "Times"
  write(6,"(A25)"),"    "
  if (country.eq."USA") then
    filename="wrfchemi_00z_d01_us"
    if(hh.eq.12)filename="wrfchemi_12z_d01_us"
  else
    filename="wrfchemi_00z_d01_mx"
    if(hh.eq.12)filename="wrfchemi_12z_d01_mx"
  end if
  write(6,"(A,x,A)"),'****    Start reading',filename
  call check(nf90_open(filename, NF90_NOWRITE, ncid))
  call check(nf90_inquire(ncid, nDims2, nVars, nGlobalAtts, unlimdimid))
  call check(nf90_inq_dimid(ncid, "south_north", lat_varid))
  call check(nf90_inq_dimid(ncid, "west_east", lon_varid))
  call check(nf90_inq_dimid(ncid, "emissions_zdim_stag", zdimId))
  call check(nf90_inquire_dimension(ncid, unlimdimid,name,nTimes))
!   Retrive initial Time
  call check( nf90_inq_varid(ncid, REC_NAME, idcTime) )
  call check(nf90_get_var(ncid, idcTime, Times,start = (/ 1, 1 /)))
  current_date(1:19)=Times(1,1)
  print *,current_date!,lat_varid,lon_varid
  !print *,  "Times in UnlimitID",nTimes


  if (nf90_inq_dimid(ncid, "bottom_top",btDimID).eq.nf90_noerr)then
  !print *,'   Dimension bottom_top'
  else
  print *,"   NO dimension bottom_top"
  end if

  ! Dimensiones
  call check(nf90_inquire_dimension(ncid, lon_varid,name,dimlon))
  !print *,dimlon,name
  call check(nf90_inquire_dimension(ncid, lat_varid,name,dimlat))
  !print *,dimlat,name
  call check(nf90_inquire_dimension(ncid, zdimId,name,dimZ))

  if(.not.ALLOCATED(XLON) ) allocate (XLON(dimlon ,dimlat,1))
  if(.not.ALLOCATED(XLAT) ) allocate (XLAT(dimlon ,dimlat,1))
  if(country.eq."USA" ) then
   if(.not.ALLOCATED(ename)) allocate(ename(nVars)) !Emissions Names
  else
  if(.not.ALLOCATED(ENMX)) allocate(ENMX(nVars)) !Emissions Names
  end if
  if(.not.ALLOCATED(id_var) ) allocate(id_var(nVars))  !Emissions ID
  if(.not.ALLOCATED(vus)  ) allocate(vus(dimlon,dimlat,dimZ,nTimes))

  if(nf90_inq_varid(ncid, "XLAT_M", latVarId).eq. nf90_noerr) then
  print *,"XLAT_M"
  else
  call check(nf90_inq_varid(ncid, "XLAT", latVarId))
  !print *,"XLAT"
  end if
  if(nf90_inq_varid(ncid, "XLONG_M", lonVarId).eq. nf90_noerr) then
  print *,"XLONG_M"
  else
  call check(nf90_inq_varid(ncid, "XLONG", lonVarId))
  !print *,"XLONG"
  end if
  call check(nf90_get_var(ncid, latVarId,xlat,start=(/1,1,1/),count=(/dimlon,dimlat,1/)))
  call check(nf90_get_var(ncid, lonVarId,xlon,start=(/1,1,1/),count=(/dimlon,dimlat,1/)))
  print *,'  Reading Global Attribiutes'
  call check( nf90_get_att(ncid, nf90_global, "DX", dx))
  call check( nf90_get_att(ncid, nf90_global, "DY", dy))
  call check( nf90_get_att(ncid, nf90_global, "CEN_LAT",cenlat))
  call check( nf90_get_att(ncid, nf90_global, "CEN_LON",cenlon))
  call check( nf90_get_att(ncid, nf90_global, "TRUELAT1",trulat1))
  call check( nf90_get_att(ncid, nf90_global, "TRUELAT2",trulat2))
  call check( nf90_get_att(ncid, nf90_global, "MOAD_CEN_LAT",moadcenlat))
  call check( nf90_get_att(ncid, nf90_global, "STAND_LON",stdlon))
  call check( nf90_get_att(ncid, nf90_global, "POLE_LAT",pollat))
  call check( nf90_get_att(ncid, nf90_global, "POLE_LON",pollon))
  call check( nf90_get_att(ncid, nf90_global, "GMT",gmt))
  call check( nf90_get_att(ncid, nf90_global, "JULYR",julyr))
  call check( nf90_get_att(ncid, nf90_global, "JULDAY",julday))
  call check( nf90_get_att(ncid, nf90_global, "MAP_PROJ",mapproj))
  if(country.eq."USA" )call check( nf90_get_att(ncid, nf90_global, "MAP_PROJ_CHAR",map_proj_char))
  call check( nf90_get_att(ncid, nf90_global, "MMINLU",mminlu))
  call check( nf90_get_att(ncid, nf90_global, "ISWATER",iswater))
  call check( nf90_get_att(ncid, nf90_global, "ISLAKE",islake))
  call check( nf90_get_att(ncid, nf90_global, "ISICE",isice))
  call check( nf90_get_att(ncid, nf90_global, "ISURBAN",isurban))
  call check( nf90_get_att(ncid, nf90_global, "ISOILWATER",isoilwater))
  call check( nf90_get_att(ncid, nf90_global, "GRID_ID",grid_id))
  if(country.eq."USA" )call check( nf90_get_att(ncid, nf90_global, "NUM_LAND_CAT",num_land_cat))
  call check( nf90_get_att(ncid, nf90_global, "START_DATE",current_date))
  call check( nf90_get_att(ncid, nf90_global, "MECHANISM",mecha))
  !print *,XLAT(1,1,1),XLAT(1,2,1),XLAT(1,3,1)
  !print *,XLON(1,1,1),XLON(2,1,1),XLON(3,1,1)
  print *,"  Finish global",nVars,country
  j=0
  do i=1,nVars
   call check(nf90_inquire_variable(ncid, i, name))
    if(name(2:2).eq."_") then
       j=j+1
    if(country.eq."USA" ) then
      ename(j) = trim(to_upper(name))
    !print *,j,i,ename(j)
    else
      ENMX(j)= trim(to_upper(name))
     ! print *,j,i,ENMX(j)
    end if
      id_var(j) = i
    end if
  end do
  nradm=j  ! Number of species in wrfchemin
  print *,"Emissions vars= ",nradm
  if(.not.ALLOCATED(EMI_USA)  ) allocate(EMI_USA(dimlon,dimlat,dimZ,nTimes,nradm))
  if(.not.ALLOCATED(EMI_MEX).and.country.eq."MEX") allocate(EMI_MEX(dimlon,dimlat,dimZ,nTimes,nradm))
print *,'-----  Reading Emissions Variables   -----'
!print *,dimlon,dimlat,dimZ,nTimes,nradm
  do ikk=1,nradm
    call check(nf90_get_var(ncid, id_var(ikk),vus,start = (/1,1,1,1/)))
    do i=1, dimlon
      do j=1, dimlat
        do l=1,dimZ
          do it=1,nTimes !times in file
          if(country.eq."USA" ) then
            EMI_USA(i,j,l,it,ikk)=vus(i,j,l,it)
          else
            EMI_MEX(i,j,l,it,ikk)=vus(i,j,l,it)
          end if
          end do
        end do
      end do
    end do
  end do
  call check( nf90_close(ncid) )
  deallocate(vus,id_var)
  write(6,"(A,x,A,x,A)"),'****    Done reading',filename, mecha

end subroutine lee_wrf


