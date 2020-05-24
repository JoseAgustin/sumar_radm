!****************************************************************************
!
!    MODULE  var_suma.mod
!
!  PURPOSE:  Provides the common variables for sumafiles code
!
!   version 0.1  28 abril 2018
!
!***************************************************************************
module var_suma
!integer hh            ! Identifies the start time
integer :: zlev       ! Layer of emission (1 to 8) 8 lower 1 upper
integer :: radm,nradm   ! number of Mechanism classes
integer :: grid_id
integer,parameter::mozart=39 ! number of emision species in Mozart
integer,parameter::nh=24 ! number of hours per day
integer,parameter::nDims=6 ! number of dimensions in wrfchemi file
integer:: julyr,julday,mapproj,iswater,islake,isice,isurban,isoilwater
integer:: land_cat_stag
integer,allocatable::id_var(:)

real :: cenlat,cenlon, dx,dy
real :: trulat1, trulat2,moadcenlat,stdlon,pollat,pollon
real :: gmt,num_land_cat

real,allocatable ::xlon(:,:,:),xlat(:,:,:)! by nx,ny,nh emissions
real,allocatable ::vus(:,:,:,:) ! by nx,ny,nh,tim emissions array
real,allocatable:: EMI_USA(:,:,:,:,:) ! Emissions nx,ny,nh,tim,nradm
real,allocatable:: EMI_MEX(:,:,:,:,:) ! Emissions nx,ny,nh,tim,nradm

character(len=19)::mminlu,map_proj_char
character (len=38) :: Title
character (len=19) :: current_date,mecha
character (len=19),dimension(1,1)::Times ! Date character
character (len=11),allocatable :: ENAME(:),ENMX(:) !US and Mex emissions Names
character (len=11),dimension(mozart) :: EMOZ=(/ & !RADM Emissions Names
        'E_CO   ','E_NH3  ','E_NO   ', &
        'E_NO2  ','E_SO2  ','E_ALD  ','E_CH4  ','E_CSL  ','E_ETH  ','E_GLY  ', &
        'E_HC3  ','E_HC5  ','E_HC8  ','E_HCHO ','E_ISO  ','E_KET  ','E_MACR ', &
        'E_MGLY ','E_MVK  ','E_OL2  ','E_OLI  ','E_OLT  ','E_ORA1 ','E_ORA2 ', &
        'E_TOL  ','E_XYL  ','E_CO2  ','E_PM_10','E_PM25 ','E_SO4I ','E_NO3I','E_PM25I',&
        'E_ORGI ','E_ECI  ','E_SO4J ','E_NO3J ','E_PM25J','E_ORGJ ','E_ECJ  '/)
character(len=19),dimension(mozart):: cname=(/& !RADM2 Emissions description
        'Carbon Monoxide ','NH3             ','NO              ', &
        'NO2  ','SO2  ','ALDEHYDES  ','METHANE','CRESOL','Ethane','Glyoxal', &
        'HC3  ','HC5  ','HC8  ','HCHO ','ISOPRENE','Acetone','Acrolein', &
        'MGLY ','Methyl Vinil Ketone  ','Alkenes','alkenes   ','Terminal Alkynes',&
        'Formic Acid','Acetic Acid ','TOLUENE  ','XYLENE  ','Carbon Dioxide',&
        'PM_10','PM_25 ','Sulfates','Nitrates ','PM25I',&
        'Organic ','Elemental Carbon','SulfatesJ','NitratesJ','PM25J',&
        'Organic','Elemental Carbon'/)
character (len=19),dimension(NDIMS) ::sdim=(/"Time               ",&
& "DateStrLen         ","west_east          ","south_north        ",&
&"bottom_top         ","emissions_zdim_stag"/)

common /domain/ zlev,cenlat,cenlon,dx,dy,trulat1,&
               trulat2,moadcenlat,stdlon,pollat,pollon,Title
common /wrfchem/ julyr,julday,mapproj,iswater,islake,isice,isurban,isoilwater,&
                 gmt,num_land_cat,land_cat_stag,mminlu,map_proj_char,&
                 current_date,mecha,EMOZ,cname
contains
!
!  CCCC  H   H  EEEEE   CCCC  K   K
! CC     H   H  E      CC     K K
! C      HHHHH  EEE   C       KK
! CC     H   H  E      CC     K K
!  CCCC  H   H  EEEEE   CCCC  K   K
subroutine check(status)
USE netcdf
  integer, intent ( in) :: status
  if(status /= nf90_noerr) then
    print *, trim(nf90_strerror(status))
    stop 2
  end if
end subroutine check

Pure Function to_upper(str) Result (string)

!   ==============================
!   Changes a string to upper case
!   ==============================

Implicit None
Character(*), Intent(In) :: str
Character(LEN(str))      :: string

Integer :: ic, i

Character(26), Parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
Character(26), Parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

!   Capitalize each letter if it is lowecase
string = str
do i = 1, LEN_TRIM(str)
ic = INDEX(low, str(i:i))
if (ic > 0) string(i:i) = cap(ic:ic)
end do

End Function to_upper

end module var_suma
