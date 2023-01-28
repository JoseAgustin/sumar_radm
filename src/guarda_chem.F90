!
!   guarda_chem.f90
!
!
!   Created by Agustin Garcia on 28/04/18.
!   Copyright 2018 Universidad Nacional Autonoma de Mexico. All rights reserved.
!
!****************************************************************************
!
!  PROGRAM: sumafiles
!
!  PURPOSE:  Guarda la suma de archivos netcdf de USA y MEX en uno solo
!             wrfchemi_00z_d01_mx  and wrfchemi_12z_d01_mx
!             wrfchemi_00z_d01_us  and wrfchemi_12z_d01_us
!   version 0.1  29 abril 2018
!
!***************************************************************************

subroutine guarda_emis
use netcdf
use var_suma
implicit none

  integer :: i,j,l
  integer :: ius,imx,ied,ikk !species indexes US Mex radm2
  integer :: it,iit,eit,PERIODO,iu,id
  integer :: ncid,itnc
  integer :: id_varlong,id_varlat
  integer,dimension(nDims) ::dim,id_dim
  integer :: dimids2(2),dimids3(3),dimids4(4)
  real,dimension(0:23)::ft_edgar=(/ 1.22954028, 1.015457123,&
     0.794663897,0.627231697,&
     0.526900188, 0.466509566, 0.459799497, 0.433278747, 0.459799497, 0.520190119,&
     0.60710149, 0.747693414, 0.928545752, 1.109078564, 1.269800695, 1.370132204,&
     1.430203299, 1.490274394, 1.510404601, 1.51711467, 1.490274394, 1.410073092,&
     1.309741582, 1.276191237 /)
  real,ALLOCATABLE :: ea(:,:,:,:)

  character (len=22) :: FILE_NAME
  character(8)  :: date,cday
  character(10) :: time
  character(19) :: hoy

  print *,"Guarda Archivo"
  allocate(id_var(mozart+1))
  call date_and_time(date,time)
  write(hoy,'(A8,x,A10)')date,time
  print *,hoy
  cday="weekday"
  print *, "    +++++ Guarda Emis ",current_date(12:13)
  if(current_date(12:13).EQ. "00") then
    print *,'PERIODO 1'
    FILE_NAME='wrfchemi_00z_d01'         !******
    TITLE="Emissions for 0 to 11z V4.2 US NEI 2011, INEM 2014, EDGAR 2012"
    PERIODO=1
    iit= 0
    eit=11
    write(current_date(12:13),'(2A)')"00"
  else
    Print *,'PERIODO 2'
    FILE_NAME='wrfchemi_12z_d01'         !******
    TITLE="Emissions for 12 to 23z V4.2 US NEI 2011, INEM 2014, EDGAR 2012"
    PERIODO=2
    iit=0 !12
    eit=11! 23
    write(current_date(12:13),'(2A)')"12"
  end if
  write(FILE_NAME(16:16),'(I1)')grid_id
! Open NETCDF emissions file
  call check( nf90_create(FILE_NAME, nf90_clobber, ncid) )
!     Define dimensiones
  dim(1)=1
  dim(2)=19
  dim(3)=SIZE(EMI_USA,DIM=1)
  dim(4)=SIZE(EMI_USA,DIM=2)
  dim(5)=1!mkx
  dim(6)=SIZE(EMI_USA,DIM=3) ! VERTICAL DATA
  if(.not.ALLOCATED(ea)) allocate (ea(dim(3),dim(4),dim(6),dim(1)))
  call check( nf90_def_dim(ncid,sdim(1), NF90_UNLIMITED, id_dim(1)) )
  do i=2,NDIMS
    call check( nf90_def_dim(ncid, sdim(i), dim(i), id_dim(i)) )
  end do
  dimids2 = (/id_dim(2),id_dim(1)/)
  dimids3 = (/id_dim(3),id_dim(2),id_dim(1) /)
  dimids4 = (/id_dim(3),id_dim(4),id_dim(6),id_dim(1)/)
!Attributos Globales NF90_GLOBAL
    call check( nf90_put_att(ncid, NF90_GLOBAL, "TITLE",TITLE))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "START_DATE",current_date))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "DAY ",cday))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "SIMULATION_START_DATE",current_date))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "WEST-EAST_GRID_DIMENSION",dim(3)))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "SOUTH-NORTH_GRID_DIMENSION",dim(4)))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "BOTTOM-TOP_GRID_DIMENSION",1))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "DX",dx))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "DY",dy))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "CEN_LAT",cenlat))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "CEN_LON",cenlon))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "TRUELAT1",trulat1))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "TRUELAT2",trulat2))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "MOAD_CEN_LAT",moadcenlat))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "STAND_LON",stdlon))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "POLE_LAT",pollat))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "POLE_LON",pollon))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "GMT",gmt))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "JULYR",julyr))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "JULDAY",julday))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "MAP_PROJ",mapproj))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "MAP_PROJ_CHAR",map_proj_char))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "MMINLU",mminlu))
    call check( nf90_put_att(ncid, nf90_global, "ISWATER",iswater))
    call check( nf90_put_att(ncid, nf90_global, "ISLAKE",islake))
    call check( nf90_put_att(ncid, nf90_global, "ISICE",isice))
    call check( nf90_put_att(ncid, nf90_global, "ISURBAN",isurban))
    call check( nf90_put_att(ncid, nf90_global, "NUM_LAND_CAT",num_land_cat))
    call check( nf90_put_att(ncid, nf90_global, "ISOILWATER",isoilwater))
    call check( nf90_put_att(ncid, nf90_global, "GRID_ID",grid_id))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "MECHANISM","RADM2"))
    call check( nf90_put_att(ncid, NF90_GLOBAL, "CREATION_DATE",hoy))
!  Define las variables
    call check( nf90_def_var(ncid, "Times", NF90_CHAR, dimids2,id_var(mozart+1) ) )
!  Attributos para cada variable
    call check( nf90_def_var(ncid, "XLONG", NF90_REAL,(/id_dim(3),id_dim(4),id_dim(1)/),id_varlong ) )
! Assign  attributes
      call check( nf90_put_att(ncid, id_varlong, "FieldType", 104 ) )
      call check( nf90_put_att(ncid, id_varlong, "MemoryOrder", "XYZ") )
      call check( nf90_put_att(ncid, id_varlong, "description", "LONGITUDE, WEST IS NEGATIVE") )
      call check( nf90_put_att(ncid, id_varlong, "units", "degree_east"))
      call check( nf90_put_att(ncid, id_varlong, "axis", "X") )
    call check( nf90_def_var(ncid, "XLAT", NF90_REAL,(/id_dim(3),id_dim(4),id_dim(1)/),id_varlat ) )
! Assign  attributes
      call check( nf90_put_att(ncid, id_varlat, "FieldType", 104 ) )
      call check( nf90_put_att(ncid, id_varlat, "MemoryOrder", "XYZ") )
      call check( nf90_put_att(ncid, id_varlat, "description", "LATITUDE, SOUTH IS NEGATIVE") )
      call check( nf90_put_att(ncid, id_varlat, "units", "degree_north"))
      call check( nf90_put_att(ncid, id_varlat, "axis", "Y") )
    do i=1,mozart
      if(i.lt.34 ) then
        call crea_attr(ncid,4,dimids4,EMOZ(i),cname(i),id_var(i))
      else
        call crea_attr2(ncid,4,dimids4,EMOZ(i),cname(i),id_var(i))
      end if
    end do
!   Terminan definiciones
    call check( nf90_enddef(ncid) )
!    Inicia loop de tiempo
    tiempo: do it=iit,eit
      itnc=it
     if(PERIODO.eq.2) itnc=it+12
    write(6,'(A,x,I3)')'TIEMPO:',itnc
      gases: do ikk=1,mozart
        ea=0.0
        if(ikk.eq.1) then
        write(current_date(12:13),'(I2.2)')itnc
        write(current_date(1:4),'(I4)') julyr
        Times(1,1)=current_date(1:19)
        call check( nf90_put_var(ncid, id_var(mozart+1),Times,start=(/1,it+1/)) )
        call check( nf90_put_var(ncid, id_varlong,xlon,start=(/1,1,it+1/)) )
        call check( nf90_put_var(ncid, id_varlat,xlat,start=(/1,1,it+1/)) )
        end if   ! for kk == 1
        mex:do imx=1,SIZE(EMI_MEX,DIM=5)
          if(ENMX(imx).eq.EMOZ(ikk)) then
!$omp parallel do private(j,l)
          lon: do i=1, dim(3)
            lat:do j=1, dim(4)
              lev:do l=1,dim(6)
                    ea(i,j,l,1)=EMI_MEX(i,j,l,it+1,imx)
              end do lev
            end do lat
          end do lon
!$omp end parallel do
          end if
        end do mex
!
        us: do ius=1,SIZE(EMI_USA,DIM=5)
          if(ename(ius).eq.EMOZ(ikk)) then
!$omp parallel do private(j,l)
            lonu: do i=1, dim(3)
              latu:do j=1, dim(4)
                levu:do l=1,dim(6)
                 if(ea(i,j,l,1).eq.0 .and.ea(i,j,2,1).eq.0) &
                  ea(i,j,l,1)=EMI_USA(i,j,l,it+1,ius)
                end do levu
              end do latu
            end do lonu
!$omp end parallel do
            exit
          end if
        end do us
        edg: do ied=1,SIZE(EMI_EDG,DIM=5)
        if(EMCA(ied).eq.EMOZ(ikk)) then
!$omp parallel do private(j,l)
          lone: do i=1, dim(3)
            late:do j=1, dim(4)
              leve:do l=1,SIZE(EMI_EDG,DIM=3)
                if(ea(i,j,l,1).eq.0 .and. ea(i,j,2,1).eq.0) &
                ea(i,j,l,1)=EMI_EDG(i,j,l,1,ied)*ft_edgar(itnc)
              end do leve
            end do late
          end do lone
!$omp end parallel do
          exit
        end if
        end do edg
      !if(periodo.eq.1) then
        call check( nf90_put_var(ncid, id_var(ikk),ea,start=(/1,1,1,it+1/)) )
      !else
      !  call check( nf90_put_var(ncid, id_var(ikk),ea,start=(/1,1,1,it+1/)) )        !******
      !endif
      end do gases
    end do tiempo
! Close NETCDF file
    call check( nf90_close(ncid) )
    deallocate(ea,EMI_USA,EMI_MEX,ENMX,ename)
    if(periodo.eq.2)then
       if allocated(EMI_EDG) deallocate(EMI_EDG,EMCA)
       deallocate(xlon,xlat)
    end if
contains

!  CCCC RRRR  EEEEE  AAA      AAA  TTTTT TTTTT RRRR
! CC    R  RR E     A   A    A   A   T     T   R  RR
! C     RRRR  EEEE  AAAAA    AAAAA   T     T   RRRR
! CC    R  R  E     A   A    A   A   T     T   R  R
!  CCCC R   R EEEEE A   A____A   A   T     T   R   R
subroutine crea_attr(ncid,idm,dimids,svar,cnamei,id_var)
implicit none
integer , INTENT(IN) ::ncid,idm
integer, INTENT(out) :: id_var
integer, INTENT(IN),dimension(idm):: dimids
character(len=*), INTENT(IN)::svar,cnamei
character(len=50) :: cvar
cvar=trim(cnamei)//" emission rate"

call check( nf90_def_var(ncid, svar, NF90_REAL, dimids,id_var ) )
! Assign  attributes
call check( nf90_put_att(ncid, id_var, "FieldType", 104 ) )
call check( nf90_put_att(ncid, id_var, "MemoryOrder", "XYZ") )
call check( nf90_put_att(ncid, id_var, "description", Cvar) )
call check( nf90_put_att(ncid, id_var, "units", "mol km^-2 hr^-1"))
call check( nf90_put_att(ncid, id_var, "stagger", "Z") )
call check( nf90_put_att(ncid, id_var, "coordinates", "XLONG XLAT") )
! print *,"Entro a Attributos de variable",dimids,id,jd
return
end subroutine crea_attr
!  CCCC RRRR  EEEEE  AAA      AAA  TTTTT TTTTT RRRR   222
! CC    R  RR E     A   A    A   A   T     T   R  RR 2   2
! C     RRRR  EEEE  AAAAA    AAAAA   T     T   RRRR     2
! CC    R  R  E     A   A    A   A   T     T   R  R   2
!  CCCC R   R EEEEE A   A____A   A   T     T   R   R 22222
subroutine crea_attr2(ncid,idm,dimids,svar,cnamei,id_var)
implicit none
integer, INTENT(IN) ::ncid,idm
integer, INTENT(out) :: id_var
integer,INTENT(IN) ,dimension(idm):: dimids
character(len=*),INTENT(IN) ::svar,cnamei
character(len=50) :: cvar
cvar=trim(cnamei)//" emission rate"
call check( nf90_def_var(ncid, svar, NF90_REAL, dimids,id_var ) )
! Assign  attributes
call check( nf90_put_att(ncid, id_var, "FieldType", 104 ) )
call check( nf90_put_att(ncid, id_var, "MemoryOrder", "XYZ") )
call check( nf90_put_att(ncid, id_var, "description",cvar) )
call check( nf90_put_att(ncid, id_var, "units", "ug m-2 s-1"))
call check( nf90_put_att(ncid, id_var, "stagger", "Z") )
call check( nf90_put_att(ncid, id_var, "coordinates", "XLONG XLAT") )
! print *,"Entro a Attributos de variable",dimids,id,jd
return
end subroutine crea_attr2
end subroutine  guarda_emis
