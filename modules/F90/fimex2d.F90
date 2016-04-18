!> @file
!! Fimex F90 interface for 2-dimensional fields

!> @brief Fimex Fortran90 interface for 2-dimensional fields
!! @author Trygve Aspelien, Heiko Klein
!!
!! The Fimex F90 interface for 2-dimensional fields contains support routines
!! for typical handling of 2-dimensional meteorological fields.
!!
!! The fimex.f90 interface is currently not precompiled with building fimex. Please
!! copy the fimex.f90 file to your f90-project and compile it from there, and link with ''-lfimex2d''.
!!
!! @see https://github.com/metno/fimex/blob/master/modules/F90/fimex2d.F90

MODULE FIMEX2D
  INTEGER,PARAMETER              :: RKIND=8
  INTEGER,PARAMETER              :: IKIND=4
  REAL(KIND=RKIND),PARAMETER     :: XUNDEF=1e20

  INTERFACE
    !> Read all dimension values from a dimension
    !! @param FIO the fimex-object
    !! @param DIMSIZE the size of the dimension to read
    !! @param DIMNAME the name of the dimension to read
    !! @param CUNIT optional unit (e.g. reading time)
    !!
    SUBROUTINE FI_READ_DIMENSION(FIO,DIMSIZE,DIMNAME,DIMVALUES,CUNIT)
      USE FIMEX     ,ONLY : FimexIO
      IMPORT                                                 :: RKIND,IKIND
      IMPLICIT NONE
      TYPE(FimexIO),                 INTENT(INOUT)           :: FIO
      INTEGER,                       INTENT(IN)              :: DIMSIZE
      CHARACTER(LEN=*),              INTENT(IN)              :: DIMNAME
      REAL(KIND=RKIND),DIMENSION(:), INTENT(OUT)             :: DIMVALUES
      CHARACTER(LEN=*),              INTENT(IN),OPTIONAL     :: CUNIT
    END SUBROUTINE

    !> Get dimensions of a variable
    !! @param FIO the fimex-object
    !! @param VARNAME the varible to check dimensions for
    !! @param NX optional The x dimension
    !! @param NY optional The y dimension
    !! @param NZ optional The z dimension
    !! @param NREL optional The realization dimension (ensemble size)
    !! @param NTIMES optional The time dimension
    !! @param X_GEO optional If x is geographical coordinate
    !! @param Y_GEO optional If y is geographical coordinate
    !! @param DIMNAME_X optional Name of x dimension
    !! @param DIMNAME_Y optional Name of y dimension
    !! @param DIMNAME_Z optional Name of z dimension
    !! @param DIMNAME_T optional Name of t dimension
    !! @param DIMNAME_R optional Name of realization dimension
    !! @param BASETIME optional Epoch time in seconds for first step in file
    !! @param VERBOSITY optional Set the verbosity level to print
    !!
    SUBROUTINE FI_GET_DIMENSIONS(FIO,VARNAME,NX,NY,NZ,NREL,NTIMES,X_GEO,Y_GEO,&
                               DIMNAME_X,DIMNAME_Y,DIMNAME_Z,DIMNAME_T,DIMNAME_R,BASETIME,VERBOSITY)
      USE FIMEX     ,ONLY : FimexIO,AXIS_GeoX,AXIS_Lon,AXIS_GeoY,AXIS_Lat,AXIS_GeoZ,AXIS_Time,&
                                AXIS_Pressure,AXIS_Height,AXIS_realization
      IMPORT                                             :: RKIND,IKIND
      IMPLICIT NONE
      TYPE(FimexIO),             INTENT(INOUT)           :: FIO
      CHARACTER(LEN=*),          INTENT(IN)              :: VARNAME
      INTEGER(KIND=IKIND),       INTENT(OUT),  OPTIONAL  :: NX
      INTEGER(KIND=IKIND),       INTENT(OUT),  OPTIONAL  :: NY
      INTEGER(KIND=IKIND),       INTENT(OUT),  OPTIONAL  :: NZ
      INTEGER(KIND=IKIND),       INTENT(OUT),  OPTIONAL  :: NREL
      INTEGER(KIND=IKIND),       INTENT(OUT),  OPTIONAL  :: NTIMES
      LOGICAL,                   INTENT(OUT),  OPTIONAL  :: X_GEO
      LOGICAL,                   INTENT(OUT),  OPTIONAL  :: Y_GEO
      CHARACTER(LEN=*),          INTENT(OUT),  OPTIONAL  :: DIMNAME_X
      CHARACTER(LEN=*),          INTENT(OUT),  OPTIONAL  :: DIMNAME_Y
      CHARACTER(LEN=*),          INTENT(OUT),  OPTIONAL  :: DIMNAME_Z
      CHARACTER(LEN=*),          INTENT(OUT),  OPTIONAL  :: DIMNAME_T
      CHARACTER(LEN=*),          INTENT(OUT),  OPTIONAL  :: DIMNAME_R
      REAL(KIND=RKIND),          INTENT(OUT),  OPTIONAL  :: BASETIME
      INTEGER(KIND=IKIND),       INTENT(IN),   OPTIONAL  :: VERBOSITY
    END SUBROUTINE

    !> Read a 2-dimensional x,y field for any other given dimension(s)
    !! @param FIO the fimex-object
    !! @param VARNAME The variable to be read
    !! @param NX The x dimension size
    !! @param NY The y dimension size
    !! @param FIELD The field to be read in
    !! @param STEP optional step for time dimension
    !! @param REL optional realization for realization dimension (EPS)
    !! @param LEVEL optional vertical level
    !! @param TIME optional Epoch time in seconds for time step
    !! @param BASETIME optional Epoch time in seconds for first step in file
    !! @param CUNIT optional unit Udunit2 compatible unit
    !! @param LCONT_ON_ERROR optional Option to tolerate errors
    !! @param VERBOSITY optional Set the verbosity level to print
    !!
    SUBROUTINE FI_READ_FIELD(FIO,VARNAME,NX,NY,FIELD,STEP,REL,LEVEL,TIME,BASETIME,CUNIT,LCONT_ON_ERROR,VERBOSITY)
      USE FIMEX     ,ONLY : FimexIO
      IMPORT                                                   :: RKIND,IKIND
      IMPLICIT NONE
      TYPE(FimexIO),                     INTENT(INOUT)           :: FIO
      CHARACTER(LEN=*),                  INTENT(IN)              :: VARNAME
      INTEGER(KIND=IKIND),               INTENT(IN)              :: NX
      INTEGER(KIND=IKIND),               INTENT(IN)              :: NY
      REAL(KIND=RKIND),DIMENSION(NX,NY), INTENT(OUT)             :: FIELD
      INTEGER(KIND=IKIND),               INTENT(IN),   OPTIONAL  :: STEP
      INTEGER(KIND=IKIND),               INTENT(IN),   OPTIONAL  :: REL
      INTEGER(KIND=IKIND),               INTENT(IN),   OPTIONAL  :: LEVEL
      REAL(KIND=RKIND),                  INTENT(OUT),  OPTIONAL  :: TIME
      REAL(KIND=RKIND),                  INTENT(OUT),  OPTIONAL  :: BASETIME
      CHARACTER(LEN=*),                  INTENT(IN),   OPTIONAL  :: CUNIT
      LOGICAL,                           INTENT(INOUT),OPTIONAL  :: LCONT_ON_ERROR
      INTEGER(KIND=IKIND),               INTENT(IN),   OPTIONAL  :: VERBOSITY
    END SUBROUTINE

    !> Write a 2-dimensional x,y field for any other given dimension(s). The file must be pre-generated with the proper dimensions
    !! @param FIO the fimex-object
    !! @param VARNAME Variable name
    !! @param NX The x dimension
    !! @param NY The y dimension
    !! @param OUTFIELD The 2-dimensional output field
    !! @param STEP optional Step for time dimension
    !! @param REL optional Realization for realization dimension (EPS)
    !! @param LEVEL optional Vertical level
    !! @param TIME optional Epoch time in seconds
    !! @param CUNIT optional unit Udunit2 compatible unit
    !! @param VERBOSITY optional Set the verbosity level to print
    !!
    SUBROUTINE FI_WRITE_FIELD(FIO,VARNAME,NX,NY,OUTFIELD,STEP,REL,LEVEL,CUNIT,VERBOSITY)
      USE FIMEX, ONLY                                             : FimexIO
      IMPORT                                                     :: RKIND,IKIND
      IMPLICIT NONE
      TYPE(FimexIO),                     INTENT(INOUT)           :: FIO
      CHARACTER(len=*),                  INTENT(IN)              :: VARNAME
      INTEGER(KIND=IKIND),               INTENT(IN)              :: NX
      INTEGER(KIND=IKIND),               INTENT(IN)              :: NY
      REAL(KIND=RKIND),DIMENSION(NX*NY), INTENT(IN)              :: OUTFIELD
      INTEGER(KIND=IKIND),               INTENT(IN),   OPTIONAL  :: STEP
      INTEGER(KIND=IKIND),               INTENT(IN),   OPTIONAL  :: REL
      INTEGER(KIND=IKIND),               INTENT(IN),   OPTIONAL  :: LEVEL
      CHARACTER(LEN=*),                  INTENT(IN),   OPTIONAL  :: CUNIT
      INTEGER(KIND=IKIND),               INTENT(IN),   OPTIONAL  :: VERBOSITY
    END SUBROUTINE

    !> Convert an epoch time to a DTG (YYYYMMDDHH)
    !! @param EPOCH time in seconds since January 1st 1970
    !!
    FUNCTION EPOCH2DTG(EPOCH)
      IMPORT                       :: RKIND,IKIND
      IMPLICIT NONE
      INTEGER(KIND=IKIND)          :: EPOCH2DTG
      REAL(KIND=RKIND), INTENT(IN) :: EPOCH
    END FUNCTION

    !> Print an error string and abort
    !! @param ERROR_STRING The string to be printed
    !!
    SUBROUTINE FI_ERROR(ERROR_STRING)
      IMPLICIT NONE
      CHARACTER(*),INTENT(IN) :: ERROR_STRING
    END SUBROUTINE
  END INTERFACE
END MODULE

FUNCTION EPOCH2DTG(EPOCH)
  USE FIMEX2D    ,ONLY : RKIND,IKIND
#ifdef __INTELCOMP
  USE IFPORT
#endif
  IMPLICIT NONE
  INTEGER(KIND=IKIND)              :: EPOCH2DTG
  REAL(KIND=RKIND), INTENT(IN)     :: EPOCH
  CHARACTER(LEN=10)                :: STRING
  INTEGER(KIND=IKIND),DIMENSION(9) :: GMT

  CALL GMTIME(INT(EPOCH),GMT)
  WRITE(STRING(1:4),'(I4.4)') GMT(6)+1900
  WRITE(STRING(5:6),'(I2.2)') GMT(5)+1
  WRITE(STRING(7:8),'(I2.2)') GMT(4)
  WRITE(STRING(9:10),'(I2.2)') (GMT(3)*3600+GMT(2)*60+GMT(1))/3600
  READ(STRING(1:10),'(I10.10)') EPOCH2DTG
END FUNCTION

SUBROUTINE FI_ERROR(ERROR_STRING)
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: ERROR_STRING

  WRITE(*,*) '******** ERROR ***********'
  WRITE(*,*) ERROR_STRING
  WRITE(*,*) '**************************'

  CALL ABORT()
END SUBROUTINE FI_ERROR


SUBROUTINE FI_READ_DIMENSION(FIO,DIMSIZE,DIMNAME,DIMVALUES,CUNIT)
  USE FIMEX     ,ONLY : FimexIO
  USE FIMEX2D    ,ONLY : RKIND,IKIND
  TYPE(FimexIO),                 INTENT(INOUT)           :: FIO
  INTEGER,                       INTENT(IN)              :: DIMSIZE
  CHARACTER(LEN=*),              INTENT(IN)              :: DIMNAME
  REAL(KIND=RKIND),DIMENSION(:), INTENT(OUT)             :: DIMVALUES
  CHARACTER(LEN=*),              INTENT(IN),OPTIONAL     :: CUNIT
  REAL(KIND=RKIND),DIMENSION(:),ALLOCATABLE              :: ZDIMVALUES

   ! Create slice builder for time variable
   NDIMS=FIO%GET_DIMENSIONS(TRIM(DIMNAME))
   IF ( NDIMS <= 0 ) CALL FI_ERROR("Can't get dimensions for variable "//TRIM(DIMNAME))

   IF ( .NOT.ALLOCATED(ZDIMVALUES)) ALLOCATE(ZDIMVALUES(DIMSIZE))
   IF(PRESENT(CUNIT)) THEN
     IERR=FIO%READ(TRIM(DIMNAME),ZDIMVALUES,CUNIT)
   ELSE
     IERR=FIO%READ(TRIM(DIMNAME),ZDIMVALUES)
   ENDIF
   DIMVALUES=ZDIMVALUES

END SUBROUTINE FI_READ_DIMENSION

SUBROUTINE FI_GET_DIMENSIONS(FIO,VARNAME,NX,NY,NZ,NREL,NTIMES,&
                             X_GEO,Y_GEO,&
                             DIMNAME_X,DIMNAME_Y,DIMNAME_Z,DIMNAME_T,DIMNAME_R,&
                             BASETIME,VERBOSITY)
  USE FIMEX     ,ONLY : FimexIO,AXIS_GeoX,AXIS_Lon,&
                                AXIS_GeoY,AXIS_Lat,&
                                AXIS_GeoZ,AXIS_Pressure,AXIS_Height,&
                                AXIS_Time,&
                                AXIS_realization
  USE FIMEX2D    ,ONLY : RKIND,IKIND
  IMPLICIT NONE
  TYPE(FimexIO),             INTENT(INOUT)           :: FIO
  CHARACTER(LEN=*),          INTENT(IN)              :: VARNAME
  INTEGER(KIND=IKIND),       INTENT(OUT),  OPTIONAL  :: NX
  INTEGER(KIND=IKIND),       INTENT(OUT),  OPTIONAL  :: NY
  INTEGER(KIND=IKIND),       INTENT(OUT),  OPTIONAL  :: NZ
  INTEGER(KIND=IKIND),       INTENT(OUT),  OPTIONAL  :: NREL
  INTEGER(KIND=IKIND),       INTENT(OUT),  OPTIONAL  :: NTIMES
  LOGICAL,                   INTENT(OUT),  OPTIONAL  :: X_GEO
  LOGICAL,                   INTENT(OUT),  OPTIONAL  :: Y_GEO
  CHARACTER(LEN=*),          INTENT(OUT),  OPTIONAL  :: DIMNAME_T
  CHARACTER(LEN=*),          INTENT(OUT),  OPTIONAL  :: DIMNAME_X
  CHARACTER(LEN=*),          INTENT(OUT),  OPTIONAL  :: DIMNAME_Y
  CHARACTER(LEN=*),          INTENT(OUT),  OPTIONAL  :: DIMNAME_Z
  CHARACTER(LEN=*),          INTENT(OUT),  OPTIONAL  :: DIMNAME_R
  REAL(KIND=RKIND),          INTENT(OUT),  OPTIONAL  :: BASETIME
  INTEGER(KIND=IKIND),       INTENT(IN),   OPTIONAL  :: VERBOSITY
  CHARACTER(LEN=1024)                                :: DIMNAME_BT
  INTEGER(KIND=IKIND), ALLOCATABLE, DIMENSION(:)     :: START,VSIZE,ATYPES
  INTEGER(KIND=IKIND)                                :: I,NDIMS,IERR,VERB
  REAL(KIND=RKIND),DIMENSION(:),ALLOCATABLE,TARGET   :: ZBASETIME

  IF (PRESENT(DIMNAME_X)) DIMNAME_X="NA"
  IF (PRESENT(DIMNAME_Y)) DIMNAME_Y="NA"
  IF (PRESENT(DIMNAME_Z)) DIMNAME_Z="NA"
  IF (PRESENT(DIMNAME_T)) DIMNAME_T="NA"
  IF (PRESENT(DIMNAME_R)) DIMNAME_R="NA"
  IF (PRESENT(BASETIME)) DIMNAME_BT="NA"
  VERB=0
  IF ( PRESENT(VERBOSITY)) VERB=VERBOSITY

  IF ( VERB > 0 ) WRITE(*,*) 'Getting dimensions for '//TRIM(VARNAME)
  NDIMS=FIO%GET_DIMENSIONS(TRIM(VARNAME))
  IF ( NDIMS <= 0 ) THEN
    CALL FI_ERROR("Can't make slicebuilder when getting dimensions for variable "//TRIM(VARNAME))
  ENDIF
  ALLOCATE(START(NDIMS))
  ALLOCATE(VSIZE(NDIMS))
  IERR = FIO%GET_DIMENSION_START_SIZE(START, VSIZE)
  IF ( IERR /= 0 ) CALL FI_ERROR("Can't get start and size for variable "//TRIM(VARNAME))
  ALLOCATE(ATYPES(NDIMS))
  IERR = FIO%GET_AXISTYPES(ATYPES)
  IF ( IERR /= 0 ) CALL FI_ERROR("Can't get axistypes for variable "//TRIM(VARNAME))

  DO I = 1, NDIMS
    IF ( VERB > 2 ) write(*,*) trim(FIO%GET_DIMNAME(I)),ATYPES(I)
    SELECT CASE (ATYPES(I))
      CASE(AXIS_GeoX, AXIS_Lon)
        IF (PRESENT(NX)) NX = VSIZE(I)
        IF (PRESENT(DIMNAME_X)) DIMNAME_X=FIO%GET_DIMNAME(I)
        SELECT CASE (ATYPES(I))
          CASE(AXIS_Lon)
            IF (PRESENT(X_GEO)) X_GEO=.FALSE.
          CASE DEFAULT
            IF (PRESENT(DIMNAME_X)) DIMNAME_X=FIO%GET_VAR_LONGITUDE(VARNAME)
            IF (PRESENT(X_GEO)) X_GEO=.TRUE.
        END SELECT
      CASE(AXIS_GeoY, AXIS_Lat)
        IF (PRESENT(NY)) NY = VSIZE(I)
        IF (PRESENT(DIMNAME_Y)) DIMNAME_Y=FIO%GET_DIMNAME(I)
        SELECT CASE (ATYPES(I))
          CASE(AXIS_Lat)
            IF (PRESENT(Y_GEO)) Y_GEO=.FALSE.
          CASE DEFAULT
            IF (PRESENT(DIMNAME_Y)) DIMNAME_Y=FIO%GET_VAR_LATITUDE(VARNAME)
            IF (PRESENT(Y_GEO)) Y_GEO=.TRUE.
        END SELECT
      CASE(AXIS_GeoZ,AXIS_Pressure,AXIS_Height)
        IF (PRESENT(NZ)) NZ=VSIZE(I)
        IF (PRESENT(DIMNAME_Z)) DIMNAME_Z=FIO%GET_DIMNAME(I)
      CASE(AXIS_Realization)
        IF (PRESENT(NREL)) NREL=VSIZE(I)
        IF (PRESENT(DIMNAME_R)) DIMNAME_R=FIO%GET_DIMNAME(I)
      CASE(AXIS_Time)
        IF (PRESENT(NTIMES)) NTIMES=VSIZE(I)
        IF (PRESENT(DIMNAME_T)) DIMNAME_T=FIO%GET_DIMNAME(I)
        IF (PRESENT(BASETIME)) DIMNAME_BT=FIO%GET_DIMNAME(I)
      CASE DEFAULT
        WRITE(*,*) "WARNING: Dimension "//TRIM(FIO%GET_DIMNAME(I))//" is not recognised! Dimension size: ",VSIZE(I)
    END SELECT
  END DO
  DEALLOCATE(START)
  DEALLOCATE(VSIZE)
  DEALLOCATE(ATYPES)

  ! Find basetime in file
  IF (PRESENT(BASETIME)) THEN
    IF (TRIM(DIMNAME_BT) /= "NA") THEN
      NDIMS=FIO%GET_DIMENSIONS(TRIM(DIMNAME_BT))
      IF ( NDIMS <= 0 ) CALL FI_ERROR("Can't get dimensions for variable "//TRIM(DIMNAME_BT))
      IERR=FIO%REDUCE_DIMENSION(DIMNAME_BT,0,1)
      IF ( IERR /= 0 ) CALL FI_ERROR("Cant reduce dimension "//TRIM(DIMNAME_BT)//" for basetime for variable "//TRIM(VARNAME))
      IF ( .NOT.ALLOCATED(ZBASETIME)) ALLOCATE(ZBASETIME(1))
      IERR=FIO%READ(TRIM(DIMNAME_BT),ZBASETIME,"seconds since 1970-01-01 00:00:00 +00:00")
      IF ( IERR /= 0 ) CALL FI_ERROR("Can't read basetime for variable "//TRIM(VARNAME))
      BASETIME=ZBASETIME(1)
    ENDIF
  ENDIF

END SUBROUTINE FI_GET_DIMENSIONS

SUBROUTINE FI_READ_FIELD(FIO,VARNAME,NX,NY,FIELD,STEP,REL,LEVEL,TIME,BASETIME,CUNIT,LCONT_ON_ERROR,VERBOSITY)
  USE FIMEX     ,ONLY : FimexIO
  USE FIMEX2D    ,ONLY : FI_GET_DIMENSIONS,EPOCH2DTG,XUNDEF,RKIND,IKIND
  IMPLICIT NONE
  TYPE(FimexIO),                     INTENT(INOUT)           :: FIO
  CHARACTER(LEN=*),                  INTENT(IN)              :: VARNAME
  INTEGER(KIND=IKIND),               INTENT(IN)              :: NX
  INTEGER(KIND=IKIND),               INTENT(IN)              :: NY
  REAL(KIND=RKIND),DIMENSION(NX,NY), INTENT(OUT)             :: FIELD
  INTEGER(KIND=IKIND),               INTENT(IN),   OPTIONAL  :: STEP
  INTEGER(KIND=IKIND),               INTENT(IN),   OPTIONAL  :: REL
  INTEGER(KIND=IKIND),               INTENT(IN),   OPTIONAL  :: LEVEL
  REAL(KIND=RKIND),                  INTENT(OUT),  OPTIONAL  :: TIME
  REAL(KIND=RKIND),                  INTENT(OUT),  OPTIONAL  :: BASETIME
  CHARACTER(LEN=*),                  INTENT(IN),   OPTIONAL  :: CUNIT
  LOGICAL,                           INTENT(INOUT),OPTIONAL  :: LCONT_ON_ERROR
  INTEGER(KIND=IKIND),               INTENT(IN),   OPTIONAL  :: VERBOSITY
  INTEGER(KIND=IKIND)                                        :: NX_VAR,NY_VAR,NZ,NREL,NTIMES
  CHARACTER(LEN=1024)                                        :: DIMNAME_T,DIMNAME_X,DIMNAME_Y,DIMNAME_Z,DIMNAME_R
  REAL(KIND=RKIND),DIMENSION(:),ALLOCATABLE,TARGET           :: ZTIME
  REAL(KIND=8),DIMENSION(:),ALLOCATABLE,TARGET               :: ZFIELD
  REAL(KIND=8),DIMENSION(:,:),POINTER                        :: ZFIELD2D
  REAL(KIND=RKIND)                                           :: BTIME
  INTEGER(KIND=IKIND)                                        :: NDIMS,IERR,I,VERB
  LOGICAL                                                    :: X_GEO,Y_GEO
  LOGICAL                                                    :: LCONTINUE_ON_ERROR

  VERB=0
  IF (PRESENT(VERBOSITY)) VERB=VERBOSITY
  LCONTINUE_ON_ERROR=.FALSE.
  IF(PRESENT(LCONT_ON_ERROR)) LCONTINUE_ON_ERROR=.FALSE.

  IF ( VERB > 0 ) WRITE(*,*) "READING "//TRIM(varName)
  CALL FI_GET_DIMENSIONS(FIO,VARNAME,NX=NX_VAR,NY=NY_VAR,NZ=NZ,NREL=NREL,NTIMES=NTIMES,&
                         X_GEO=X_GEO,Y_GEO=Y_GEO,DIMNAME_X=DIMNAME_X,DIMNAME_Y=DIMNAME_Y,&
                         DIMNAME_Z=DIMNAME_Z,DIMNAME_T=DIMNAME_T,DIMNAME_R=DIMNAME_R,&
                         BASETIME=BTIME,VERBOSITY=VERB)

  ! Create slice builder and reduce to wanted dimensions
  NDIMS=FIO%GET_DIMENSIONS(TRIM(VARNAME))
  IF ( NDIMS <= 0 ) CALL FI_ERROR("Can't make slicebuilder for reading of variable"//TRIM(VARNAME))
  IF ( TRIM(DIMNAME_Z) /= "NA" ) THEN
    IF ( PRESENT(level)) THEN
      IERR=FIO%REDUCE_DIMENSION(TRIM(DIMNAME_Z), LEVEL, 1)
      IF ( IERR /= 0 ) CALL FI_ERROR("Can't reduce dimension "//TRIM(FIO%GET_DIMNAME(I))//" for variable "//TRIM(VARNAME))
    ELSE
      IERR=FIO%REDUCE_DIMENSION(TRIM(DIMNAME_Z), 0, 1)
      IF ( IERR /= 0 ) CALL FI_ERROR("Can't reduce dimension "//TRIM(FIO%GET_DIMNAME(I))//" for variable "//TRIM(VARNAME))
    ENDIF
  ELSE
    IF (PRESENT(level)) CALL FI_ERROR("You try to read a level but the variable does not have a vertical axis!")
  ENDIF
  ! Realization (EPS)
  IF ( TRIM(DIMNAME_R) /= "NA" ) THEN
    IF (PRESENT(REL)) THEN
      IERR=FIO%REDUCE_DIMENSION(TRIM(DIMNAME_R),REL, 1)
      IF ( IERR /= 0 ) CALL FI_ERROR("Can't reduce dimension "//TRIM(FIO%GET_DIMNAME(I))//" for variable "//TRIM(VARNAME))
    ELSE
      IERR=FIO%REDUCE_DIMENSION(TRIM(DIMNAME_R), 0, 1)
      IF ( IERR /= 0 ) CALL FI_ERROR("Can't reduce dimension "//TRIM(FIO%GET_DIMNAME(I))//" for variable "//TRIM(VARNAME))
    ENDIF
  ENDIF
  IF ( TRIM(DIMNAME_T) /= "NA" ) THEN
    IF (PRESENT(STEP)) THEN
      IERR=FIO%REDUCE_DIMENSION(TRIM(DIMNAME_T),STEP,1)
      IF ( IERR /= 0 ) CALL FI_ERROR("Can't reduce dimension "//TRIM(FIO%GET_DIMNAME(I))//" for variable "//TRIM(VARNAME))
    ELSE
      IERR=FIO%REDUCE_DIMENSION(TRIM(DIMNAME_T),0,1)
      IF ( IERR /= 0 ) CALL FI_ERROR("Can't reduce dimension "//TRIM(FIO%GET_DIMNAME(I))//" for variable "//TRIM(VARNAME))
    ENDIF
  ENDIF

  IF (( NX /= NX_VAR ) .OR. ( NY /= NY_VAR )) CALL FI_ERROR("Mismatch in dimensions for variable "//TRIM(VARNAME))
  IF (.NOT.ALLOCATED(ZFIELD)) ALLOCATE(ZFIELD(NX*NY))
  ALLOCATE(ZFIELD2D(NX,NY))
  FIELD=0.
  IF ( PRESENT (CUNIT)) THEN
    IERR=FIO%READ(VARNAME,ZFIELD,CUNIT)
    IF ( IERR /= 0 ) CALL FI_ERROR("Can't read variable "//TRIM(VARNAME))
  ELSE
    IERR=FIO%READ(VARNAME,ZFIELD)
    IF ( IERR /= 0 ) CALL FI_ERROR("Can't read variable "//TRIM(VARNAME))
  ENDIF
  ZFIELD2D(1:NX,1:NY) => ZFIELD
  FIELD(1:NX,1:NY)=REAL(ZFIELD2D(1:NX,1:NY),KIND=RKIND)
  NULLIFY(ZFIELD2D)

  ! Find time in file
  IF ( TRIM(DIMNAME_T) /= "NA" ) THEN
    IF (PRESENT(BASETIME)) BASETIME=BTIME
    IF (PRESENT(TIME)) THEN
      IF ( TRIM(DIMNAME_T) == "NA" ) THEN
        WRITE(*,*) "Dimension name for time dimension not found"
        TIME=-1
      ELSE
        ! Create slice builder for time variable
        NDIMS=FIO%GET_DIMENSIONS(DIMNAME_T)
        IF ( NDIMS <= 0 ) CALL FI_ERROR("Can't get dimensions for variable "//TRIM(DIMNAME_T))
        IERR=FIO%REDUCE_DIMENSION(DIMNAME_T,STEP,1)
        IF ( IERR /= 0 ) CALL FI_ERROR("Can't reduce dimension "//TRIM(DIMNAME_T)//" for time for variable "//TRIM(VARNAME))
        IF ( .NOT.ALLOCATED(ZTIME)) ALLOCATE(ZTIME(1))
        IERR=FIO%READ(TRIM(DIMNAME_T),ZTIME,"seconds since 1970-01-01 00:00:00 +00:00")
        IF ( IERR /= 0 ) CALL FI_ERROR("Can't read time for variable "//TRIM(VARNAME))
        TIME=ZTIME(1)
      ENDIF
    ENDIF
  ELSE
    ! If the field does not have a time dimension
    ! we set the return value to undefined
    IF (PRESENT(BASETIME)) BASETIME=-1
    IF (PRESENT(TIME)) TIME=-1
  ENDIF
  IF ( VERB > 1 ) THEN
    IF ( TIME > 0 ) THEN
       WRITE(*,*) TRIM(VARNAME),EPOCH2DTG(TIME),MINVAL(FIELD),MAXVAL(FIELD),SUM(REAL(FIELD))/REAL(NX*NY)
    ELSE
       WRITE(*,*) TRIM(VARNAME),MINVAL(FIELD),MAXVAL(FIELD),SUM(REAL(FIELD))/REAL(NX*NY)
    ENDIF
  ENDIF
END SUBROUTINE FI_READ_FIELD

SUBROUTINE FI_WRITE_FIELD(FIO,VARNAME,NX,NY,OUTFIELD,STEP,REL,LEVEL,CUNIT,VERBOSITY)
  USE FIMEX, ONLY                                            : FimexIO
  USE FIMEX2D, ONLY                                           : FI_GET_DIMENSIONS,RKIND,IKIND
  IMPLICIT NONE
  TYPE(FimexIO),                    INTENT(INOUT)           :: FIO
  CHARACTER(len=*),                 INTENT(IN)              :: VARNAME
  INTEGER(KIND=IKIND),              INTENT(IN)              :: NX
  INTEGER(KIND=IKIND),              INTENT(IN)              :: NY
  REAL(KIND=RKIND),DIMENSION(NX,NY),INTENT(IN)              :: OUTFIELD
  INTEGER(KIND=IKIND),              INTENT(IN),   OPTIONAL  :: STEP
  INTEGER(KIND=IKIND),              INTENT(IN),   OPTIONAL  :: REL
  INTEGER(KIND=IKIND),              INTENT(IN),   OPTIONAL  :: LEVEL
  CHARACTER(LEN=*),                 INTENT(IN),   OPTIONAL  :: CUNIT
  INTEGER(KIND=IKIND),              INTENT(IN),   OPTIONAL  :: VERBOSITY
  REAL(KIND=RKIND),DIMENSION(NX*NY)                         :: OUTFIELD1D
  INTEGER(KIND=IKIND)                                       :: NDIMS,IERR,I,II,J,VERB
  REAL(KIND=8),DIMENSION(:),ALLOCATABLE,TARGET              :: ZFIELD
  INTEGER(KIND=IKIND)                                       :: NX2,NY2,NZ,NREL,NTIMES
  CHARACTER(LEN=1024)                                       :: DIMNAME_T,DIMNAME_X,DIMNAME_Y,DIMNAME_Z,DIMNAME_R

  VERB=0
  IF (PRESENT(VERBOSITY)) VERB=VERBOSITY

  IF ( VERB > 0 ) WRITE(*,*) "WRITING: ",TRIM(VARNAME)
  CALL FI_GET_DIMENSIONS(FIO,VARNAME,NX=NX2,NY=NY2,NZ=NZ,NREL=NREL,NTIMES=NTIMES,&
                         DIMNAME_X=DIMNAME_X,DIMNAME_Y=DIMNAME_Y,&
                         DIMNAME_Z=DIMNAME_Z,DIMNAME_T=DIMNAME_T,DIMNAME_R=DIMNAME_R,&
                         VERBOSITY=VERB)

  ! Create slice builder and reduce to wanted dimensions
  NDIMS=FIO%GET_DIMENSIONS(TRIM(VARNAME))
  IF ( NDIMS <= 0 ) CALL FI_ERROR("Can't make slicebuilder for writing of variable "//TRIM(VARNAME))
  IF ( TRIM(DIMNAME_Z) /= "NA" ) THEN
    IF ( PRESENT(level)) THEN
      IERR=FIO%REDUCE_DIMENSION(TRIM(DIMNAME_Z), LEVEL, 1)
      IF ( IERR /= 0 ) CALL FI_ERROR("Can't reduce dimension "//TRIM(FIO%GET_DIMNAME(I))//" for variable "//TRIM(VARNAME))
    ELSE
      IERR=FIO%REDUCE_DIMENSION(TRIM(DIMNAME_Z), 0, 1)
      IF ( IERR /= 0 ) CALL FI_ERROR("Can't reduce dimension "//TRIM(FIO%GET_DIMNAME(I))//" for variable "//TRIM(VARNAME))
    ENDIF
  ELSE
    IF (PRESENT(level)) CALL FI_ERROR("You try to read a level but the variable does not have a vertical axis!")
  ENDIF
  ! Realization (EPS)
  IF ( TRIM(DIMNAME_R) /= "NA" ) THEN
    IF (PRESENT(REL)) THEN
      IERR=FIO%REDUCE_DIMENSION(TRIM(DIMNAME_R),REL, 1)
      IF ( IERR /= 0 ) CALL FI_ERROR("Can't reduce dimension "//TRIM(FIO%GET_DIMNAME(I))//" for variable "//TRIM(VARNAME))
    ELSE
      IERR=FIO%REDUCE_DIMENSION(TRIM(DIMNAME_R), 0, 1)
      IF ( IERR /= 0 ) CALL FI_ERROR("Can't reduce dimension "//TRIM(FIO%GET_DIMNAME(I))//" for variable "//TRIM(VARNAME))
    ENDIF
  ENDIF
  IF ( TRIM(DIMNAME_T) /= "NA" ) THEN
    IF (PRESENT(STEP)) THEN
      IF ( STEP >= NTIMES ) THEN
        CALL FI_ERROR("You try to write a step which is larger than the maximum steps for the slicebuilder")
      ENDIF
      IERR=FIO%REDUCE_DIMENSION(TRIM(DIMNAME_T),STEP,1)
      IF ( IERR /= 0 ) CALL FI_ERROR("Can't reduce dimension "//TRIM(FIO%GET_DIMNAME(I))//" for variable "//TRIM(VARNAME))
    ELSE
      IERR=FIO%REDUCE_DIMENSION(TRIM(DIMNAME_T),0,1)
      IF ( IERR /= 0 ) CALL FI_ERROR("Can't reduce dimension "//TRIM(FIO%GET_DIMNAME(I))//" for variable "//TRIM(VARNAME))
    ENDIF
  ENDIF

  ! Modify 2D field to 1D
  II=1
  DO J=1,NY
    DO I=1,NX
      OUTFIELD1D(II)=OUTFIELD(I,J)
      II=II+1
    ENDDO
  ENDDO
  ALLOCATE(ZFIELD(NX*NY))
  ZFIELD=OUTFIELD1D
  IF ( PRESENT (CUNIT)) THEN
    IERR=FIO%WRITE(VARNAME,ZFIELD,CUNIT)
    IF ( IERR /= 0 ) CALL FI_ERROR("Can't write variable "//TRIM(VARNAME))
  ELSE
    IERR=FIO%WRITE(VARNAME,ZFIELD)
    IF ( IERR /= 0 ) CALL FI_ERROR("Can't write variable "//TRIM(VARNAME))
  ENDIF
  IF ( VERB > 1 ) WRITE(*,*) TRIM(VARNAME),MINVAL(ZFIELD),MAXVAL(ZFIELD),SUM(REAL(ZFIELD))/REAL(SIZE(ZFIELD))
  DEALLOCATE(ZFIELD)
END SUBROUTINE

