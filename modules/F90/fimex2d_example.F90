!> @example fimex2d_example.F90
!! Example on using the high-level 2d fimex-fortran interface
PROGRAM FIMEX2D_EXAMPLE
  !
  !
  USE FIMEX, ONLY  : FIMEXIO
  USE FIMEX2D,ONLY : EPOCH2DTG,FI_ERROR,FI_GET_DIMENSIONS,FI_READ_DIMENSION,FI_READ_FIELD,&
                     FI_WRITE_FIELD,IKIND,RKIND
  IMPLICIT NONE
  CHARACTER(LEN=1024)                            :: ARG,INFILE,OUTFILE,FILEFORMAT,CONFIGFILE
  CHARACTER(LEN=1024)                            :: CTEMP2M="air_temperature_2m"
  CHARACTER(LEN=1024)                            :: DIMNAME_T
  CHARACTER(LEN=10)                              :: CDTG
  INTEGER                                        :: I,T
  INTEGER(KIND=IKIND)                            :: NX,NY,NZ
  INTEGER(KIND=IKIND)                            :: NXO,NYO
  LOGICAL                                        :: FOUND12
  TYPE(FIMEXIO)                                  :: FIO_IN,FIO_OUT
  REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE        :: T2M
  INTEGER,DIMENSION(:),ALLOCATABLE               :: TIMES
  REAL(KIND=8),DIMENSION(:),ALLOCATABLE          :: ZTIMES
  REAL(KIND=8)                                   :: BASETIME,ZTIME
  INTEGER                                        :: NTIMES,IERR

  ! Set arguments
  IF ((COMMAND_ARGUMENT_COUNT() /= 3 ) .AND. (COMMAND_ARGUMENT_COUNT() /= 4 ) ) THEN
    WRITE(*,*) "Usage: ./fimex2d_example infile outfile infile-fileformat [configfile]"
    CALL ABORT
  ENDIF

  CONFIGFILE=""
  DO I = 1, COMMAND_ARGUMENT_COUNT()
    CALL GET_COMMAND_ARGUMENT(I, ARG)
    SELECT CASE (I)
      CASE(1)
         INFILE=ARG
      CASE(2)
         OUTFILE=ARG
      CASE(3)
         FILEFORMAT=ARG
         SELECT CASE (FILEFORMAT)
            CASE("felt","FELT","FLT","flt")
               FILEFORMAT="felt"
            CASE("GRIB","grib","grb","GRB","GRIB2","grib2","grb2","GRB2")
               FILEFORMAT="grib"
            CASE("netcdf","NETCDF","nc","NC")
               FILEFORMAT="netcdf"
            CASE("ncml","NCML")
               FILEFORMAT="ncml"
             CASE DEFAULT
              WRITE(*,*) "Invalid file format: "//FILEFORMAT
              CALL ABORT
         END SELECT
      CASE(4)
         IF (COMMAND_ARGUMENT_COUNT() == 4 ) THEN
           CONFIGFILE=ARG
         ENDIF
    END SELECT
  END DO
  IF (( TRIM(FILEFORMAT) == "felt" ) .OR. ( TRIM(FILEFORMAT) == "grib" )) THEN
    IF ( CONFIGFILE == "" ) THEN
      WRITE(*,*) 'No config file provided, but is mandatory for '//FILEFORMAT
      CALL ABORT
    ENDIF
  ENDIF

  IERR=FIO_IN%OPEN(TRIM(INFILE),CONFIGFILE,FILEFORMAT)
  IF ( ierr /= 0 ) CALL FI_ERROR("Error opening "//TRIM(INFILE))
  IERR=FIO_OUT%OPEN(TRIM(OUTFILE),"","netcdf"//"+rw")
  IF ( ierr /= 0 ) CALL FI_ERROR("Error opening "//TRIM(OUTFILE))

  WRITE(*,*) "List variables in input file "//TRIM(INFILE)
  DO I = 1,int(FIO_IN%VARIABLES_SIZE())
    WRITE(*,*) "Found:  ",I, " ", TRIM(FIO_IN%GET_VARNAME(I))
  END DO

  ! Get dimensions from temperature variable
  CALL FI_GET_DIMENSIONS(FIO_IN,TRIM(CTEMP2M),NX=NX,NY=NY,NZ=NZ,NTIMES=NTIMES,DIMNAME_T=DIMNAME_T,BASETIME=BASETIME)

  ! Read times in file
  ALLOCATE(TIMES(0:NTIMES-1))
  ALLOCATE(ZTIMES(0:NTIMES-1))
  WRITE(*,*) 'Listing times in file:'
  CALL FI_READ_DIMENSION(FIO_IN,NTIMES,TRIM(DIMNAME_T),ZTIMES,"seconds since 1970-01-01 00:00:00 +00:00")
  DO I=0,NTIMES-1
    TIMES(I)=EPOCH2DTG(ZTIMES(I))
    WRITE(*,*) I,TIMES(I)
  ENDDO
  DEALLOCATE(ZTIMES)

  ALLOCATE(T2M(NX,NY))
  ! Loop forecast and set variables
  FOUND12=.FALSE.
  TIME_LOOP: DO T=0,NTIMES-1

    WRITE(CDTG,'(I10.10)') TIMES(T)
    IF ( CDTG(9:10)  == "12" ) THEN
      IF (.NOT. FOUND12 ) THEN
        CALL FI_READ_FIELD(FIO_IN,TRIM(CTEMP2M),NX,NY,T2M,T,CUNIT="celsius",TIME=ZTIME,VERBOSITY=2)
        IF ( TIMES(T) /= EPOCH2DTG(ZTIME)) CALL FI_ERROR("Mismatch in time for T2M")
        FOUND12=.TRUE.
        EXIT TIME_LOOP
      ENDIF
    ENDIF
  ENDDO TIME_LOOP

  IF (FOUND12) THEN
    CALL FI_GET_DIMENSIONS(FIO_OUT,TRIM(CTEMP2M),NX=NXO,NY=NYO)
    WRITE (*,*)  NX, NXO, NY, NYO, NZ
    CALL FI_WRITE_FIELD(FIO_OUT,TRIM(CTEMP2M),NX=NX,NY=NY,OUTFIELD=T2M,VERBOSITY=2)
  ELSE
    CALL FI_ERROR("Variable for 12 UTC was not found")
  ENDIF

  IERR=FIO_IN%CLOSE()
  IERR=FIO_OUT%CLOSE()

  ! Deallocation
  DEALLOCATE(TIMES)
  DEALLOCATE(T2M)

END PROGRAM FIMEX2D_EXAMPLE
