!     ######spl
      MODULE MODI_MICRO_CLOUD_DROPLET
!     ########################
!
INTERFACE
!
      SUBROUTINE MICRO_CLOUD_DROPLET(D, CST, ICEP, ICED, &
                                     PPRESSURE, PDZZ,    &
                                     PSEA, PTOWN,        &
                                     PCLDROP)

!
USE MODD_DIMPHYEX, ONLY: DIMPHYEX_T
USE MODD_CST, ONLY: CST_T
USE MODD_RAIN_ICE_PARAM_n, ONLY: RAIN_ICE_PARAM_T
USE MODD_RAIN_ICE_DESCR_n, ONLY: RAIN_ICE_DESCR_T

! MICRO_CLOUD_DROPLET
!
!
TYPE(DIMPHYEX_T),       INTENT(IN) :: D
TYPE(CST_T),            INTENT(IN) :: CST
TYPE(RAIN_ICE_PARAM_t), INTENT(IN) :: ICEP
TYPE(RAIN_ICE_DESCR_t), INTENT(IN) :: ICED

!
! *** Variables on model levels
REAL, DIMENSION(D%NIT,D%NKT),   INTENT(IN) :: PPRESSURE    ! (Pa)
REAL, DIMENSION(D%NIT,D%NKT),   INTENT(IN) :: PDZZ         ! ilayer thickness
! *** Single-level variables 
REAL, DIMENSION(D%NIT),         INTENT(IN) :: PSEA         ! 1=sea
REAL, DIMENSION(D%NIT),         INTENT(IN) :: PTOWN       ! 1=town
! OUTPUT ARGUMENT
! 3D Cloud Droplet number concentration
REAL, DIMENSION(D%NIT,D%NKT),   INTENT(OUT) :: PCLDROP

END SUBROUTINE MICRO_CLOUD_DROPLET
!
END INTERFACE
!
END MODULE MODI_MICRO_CLOUD_DROPLET
