!
!    suma_files.f90
!
!  FUNCTIONS:
!
!    File_reading  - Reads Emission inventory from US and Mexico
!       check      - in case of error prints error messages
!
!  Created by Agustin Garcia on 28/04/2018.
!
!****************************************************************************
!
!  PROGRAM: sumafiles
!
!  PURPOSE:  Reads emissions inventory from
!             wrfchemi_00z_d01_mx  and wrfchemi_12z_d01_mx
!             wrfchemi_00z_d01_us  and wrfchemi_12z_d01_us
!   version 0.1  28 abril 2018
!
!***************************************************************************

program sumafiles

  call  lee_wrf (00,"EDG")
  call  lee_wrf (00,"USA")
  call  lee_wrf (00,"MEX")
  call guarda_emis
  call  lee_wrf (12,"USA")
  call  lee_wrf (12,"MEX")
  call guarda_emis

end program sumafiles
