SUBROUTINE MICRO_CLOUD_DROPLET &
     & (D, CST, ICEP, ICED, &
     &  PPRESSURE, PDZZ, &
     &  PSEA, PTOWN, &
     &  PCLDROP)

      USE MODD_DIMPHYEX,       ONLY: DIMPHYEX_T
      USE MODD_CST,            ONLY: CST_T
      USE MODD_PARAM_ICE_N,    ONLY: PARAM_ICE_t
      USE MODD_RAIN_ICE_PARAM_N, ONLY: RAIN_ICE_PARAM_T
      USE MODD_RAIN_ICE_DESCR_N, ONLY: RAIN_ICE_DESCR_T

!     ######################################################################
!
! MICRO_CLOUD_DROPLET
!
! PURPOSE
! -------
!   Calculate effective radius of liquid clouds
!
! AUTHOR
! ------
!   D. Martin 
!
!   Original: 2015-09-24 (CLOUD_DROPLET)
!
! MODIFICATIONS
! -------------
!
! -------------------------------------------------------------------
!*       0.    DECLARATIONS
!              ------------
!

USE YOMHOOK  , ONLY : LHOOK, DR_HOOK, JPHOOK

! -------------------------------------------------------------------

IMPLICIT NONE

! INPUT ARGUMENTS

! *** Array dimensions and ranges
TYPE(DIMPHYEX_T),       INTENT(IN) :: D
TYPE(CST_T),            INTENT(IN) :: CST
TYPE(RAIN_ICE_PARAM_t), INTENT(IN) :: ICEP
TYPE(RAIN_ICE_DESCR_t), INTENT(IN) :: ICED

! *** Variables on model levels
REAL,   INTENT(IN) :: PPRESSURE(D%NIJT,D%NKT)    ! (Pa)
REAL,   INTENT(IN) :: PDZZ(D%NIJT,D%NKT)         ! 

! *** Single-level variables 
REAL,   INTENT(IN) :: PSEA(D%NIJT)              ! 1=sea, 0=sea
REAL,   INTENT(IN) :: PTOWN(D%NIJT)             ! 1=town

! OUTPUT ARGUMENT
! 3D Cloud Droplet number concentration
REAL,  INTENT(OUT) :: PCLDROP(D%NIJT,D%NKT)

! PARAMETERS

! LOCAL VARIABLES
REAL, DIMENSION(D%NIJT)       :: ZCONC_TMP
REAL, DIMENSION(D%NIJT,D%NKT) :: ZCLDROP, ZZZZ, ZZZT

INTEGER :: JIJ, JK

REAL(KIND=JPHOOK) :: ZHOOK_HANDLE

! -------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('MICRO_CLOUD_DROPLET',0,ZHOOK_HANDLE)

! -------------------------------------------------------------------

!-----------------------------------
! Cloud droplet number concentration
!-----------------------------------
IF(ICEP%XFRMIN(26)>0.001)THEN 
  ! Use alternative concentration given by (XFRMIN(26)
  ZZZZ(:,D%NKTE)   = PDZZ(:,D%NKTE)*0.5
  ZZZT(:,D%NKTE)   = PDZZ(:,D%NKTE)
  DO JK=D%NKTE-1,D%NKTB,-1
    DO JIJ = D%NIJB, D%NIJE
      ZZZT(JIJ,JK) = ZZZT(JIJ,JK+1) + PDZZ(JIJ,JK)
      ZZZZ(JIJ,JK) = ZZZT(JIJ,JK) - 0.5*PDZZ(JIJ,JK)
    ENDDO
  ENDDO

  ZCONC_TMP(:) = ICEP%XFRMIN(26)
  DO JK=D%NKTB,D%NKTE
    DO JIJ = D%NIJB,D%NIJE
       ZCLDROP(JIJ,JK)= ZCONC_TMP(JIJ)*PPRESSURE(JIJ,JK)/CST%XP00 ! Let it be diluted with decreasing pressure
    ENDDO
  ENDDO
  IF(ICEP%XFRMIN(22)>0.001)THEN
    DO JK=D%NKTB,D%NKTE
      DO JIJ = D%NIJB, D%NIJE
        ZCONC_TMP(JIJ)  = MAX(0.0, MIN(1.0,(ZZZZ(JIJ,JK)- ZZZZ(JIJ,D%NKTE))/ &
             & (MAX(0.01,ICEP%XFRMIN(29)-ZZZZ(JIJ,D%NKTE)))))
        ZCLDROP(JIJ,JK) =  ZCLDROP(JIJ,JK)*(ICEP%XFRMIN(22)*(PSEA(JIJ)*ICEP%XFRMIN(30)+ &
             & (1.-PSEA(JIJ)))*(1.-ZCONC_TMP(JIJ)) + ZCONC_TMP(JIJ))
      END DO
    END DO
  ENDIF
ELSE
  ! Constant values over town, sea and land
   DO JIJ = D%NIJB,D%NIJE
     ZCONC_TMP(JIJ) = PSEA(JIJ)*ICED%XCONC_SEA+(1.-PSEA(JIJ))*ICED%XCONC_LAND
   ENDDO
   DO JK=D%NKTB,D%NKTE
     DO JIJ = D%NIJB, D%NIJE
       ZCLDROP(JIJ,JK)= (1.-PTOWN(JIJ))*ZCONC_TMP(JIJ)+PTOWN(JIJ)*ICED%XCONC_URBAN
     ENDDO
   ENDDO
ENDIF

PCLDROP(:,:) = ZCLDROP(:,:)


! -------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('MICRO_CLOUD_DROPLET',1,ZHOOK_HANDLE)
  
END SUBROUTINE MICRO_CLOUD_DROPLET
