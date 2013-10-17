PROGRAM fortran_test
  USE Fimex, ONLY                   : FimexIO, set_filetype, AXIS_GeoX, AXIS_GeoY, AXIS_Lon, AXIS_Lat,INTERPOL_BILINEAR
  IMPLICIT NONE
  TYPE(FimexIO)                   :: fio, finter
  INTEGER                         :: ierr,i
  CHARACTER(LEN=80)               :: input_file
  CHARACTER(LEN=80)               :: config_file
  CHARACTER(LEN=80)               :: varName
  REAL(KIND=8),DIMENSION(:),ALLOCATABLE,TARGET :: field
  REAL(KIND=8),DIMENSION(:,:,:,:),POINTER :: field4d
  REAL(KIND=8),DIMENSION(:,:,:),POINTER :: field3d
  INTEGER                         :: dataRead
  INTEGER                         :: nx,ny
  INTEGER                         :: ndims
  INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:) :: start, vsize, atypes
  CHARACTER(LEN=10)               :: cunit,cfiletype
  CHARACTER(LEN=1024)             :: dimname
  INTEGER,EXTERNAL                :: iargc


  IF (( iargc() /= 4 ) .AND. ( iargc() /= 5 )) THEN
    WRITE(*,*) "Usage: ./fortran_test input-file file-type variable unit [config]"
    WRITE(*,*) "Example ./fortran_test /opdata/arome_norway25/AROME_Norway25_00.nc netcdf surface_air_pressure hPa"
    WRITE(*,*) "        ./fortran_test /opdata/arome_norway25/preprod/AROME_Norway25_00.dat felt surface_air_pressure "//&
                        "hPa ~/metop/arome_norway25/etc/AromeFeltReaderConfig.xml"
  ELSE
    CALL getarg(1,input_file)
    CALL getarg(2,cfiletype)
    CALL getarg(3,varName)
    CALL getarg(4,cunit)
    IF ( iargc() == 5 ) THEN
      CALL getarg(5,config_file)
    ENDIF

    ! Open file
    ierr=fio%open(input_file,config_file,set_filetype(cfiletype))
    IF ( ierr /= 0 ) CALL error("Can't make io-object with file:"//trim(input_file)//" config: "//config_file)
    WRITE(0,*) "open_file: success"

    ! Get dimensions
    ndims=fio%get_dimensions(varName)
    IF ( ndims <= 0 ) CALL error("Can't make slicebuilder")
    WRITE(0,*) "get_dimensions: ", ndims

    ALLOCATE(start(ndims))
    ALLOCATE(vsize(ndims))
    ierr = fio%get_dimension_start_size(start, vsize)
    DO i = 1, ndims
      WRITE(*,*) "dimname ", i, ": ", trim(fio%get_dimname(i)), vsize(i)
    END DO
    ALLOCATE(atypes(ndims))
    ierr = fio%get_axistypes(atypes)

    DO i = 1, ndims
      !WRITE (*,*) i, " axistype: ", atypes(i)
      !WRITE (*,*) AXIS_GeoX, AXIS_GeoY, AXIS_Lon, AXIS_Lat
      SELECT CASE (atypes(i))
        CASE(AXIS_GeoX, AXIS_Lon)
          nx = vsize(i)
        CASE(AXIS_GeoY, AXIS_Lat)
          ny = vsize(i)
        CASE DEFAULT
         WRITE(*,*) "reducind dimension ", i, " ",TRIM(fio%get_dimname(i))
         ierr=fio%reduce_dimension(fio%get_dimname(i), 0, 1)
      END SELECT
    END DO
    WRITE(*,*) "end reduce"
    ALLOCATE(field(nx*ny))
    ierr=fio%read_data(varName,cunit,field)
    field4d(1:nx,1:ny,1:1,1:1) => field
    IF ( ierr /= 0 ) THEN
      CALL error("Can't read field")
    ELSE
      DO i = 1, 10
        WRITE(*,*) field4d(i,1,1,1)
      END DO
    ENDIF
    DEALLOCATE(field)

    ALLOCATE(field(vsize(1)*vsize(2)))
    field3d(1:vsize(1),1:vsize(2),1:1) => field
    ierr=fio%read_data(varName,cunit,field)
    IF ( ierr /= 0 ) THEN
      CALL error("Can't read field")
    ELSE
      DO i = 1, 10
        WRITE(*,*) field3d(i,1,1)
      END DO
    ENDIF
    DEALLOCATE(field)
    DEALLOCATE(start)
    DEALLOCATE(vsize)
    DEALLOCATE(atypes)

    ! interpolate to 1x1 lat lon around oslo
    WRITE(*,*) "method ", INTERPOL_BILINEAR
    ierr = finter%interpolate(fio, INTERPOL_BILINEAR, "+proj=latlon +datum=WGS84", "8,9,...,12", "58,59,...,62", .true.)
    IF ( ierr /= 0 ) THEN
       CALL error("Can't interpolate file")
    END IF
    ! Get dimensions
    ndims=finter%get_dimensions(varName)
    IF ( ndims <= 0 ) CALL error("Can't make slicebuilder for interpol")
    WRITE(0,*) "inter-get_dimensions: ", ndims

    ALLOCATE(start(ndims))
    ALLOCATE(vsize(ndims))
    ierr = finter%get_dimension_start_size(start, vsize)
    ALLOCATE(atypes(ndims))
    ierr = finter%get_axistypes(atypes)

    DO i = 1, ndims
      !WRITE (*,*) i, " axistype: ", atypes(i)
      !WRITE (*,*) AXIS_GeoX, AXIS_GeoY, AXIS_Lon, AXIS_Lat
      SELECT CASE (atypes(i))
        CASE(AXIS_GeoX, AXIS_Lon)
          nx = vsize(i)
        CASE(AXIS_GeoY, AXIS_Lat)
          ny = vsize(i)
        CASE DEFAULT
         WRITE(*,*) "reducind dimension ", i, " ",TRIM(fio%get_dimname(i))
         ierr=finter%reduce_dimension(fio%get_dimname(i), 0, 1)
      END SELECT
    END DO
    WRITE(*,*) "end reduce"
    ALLOCATE(field(nx*ny))
    ierr=finter%read_data(varName,cunit,field)
    field4d(1:nx,1:ny,1:1,1:1) => field
    IF ( ierr /= 0 ) THEN
      CALL error("Can't read field")
    ELSE
        WRITE(*,*) field4d
    ENDIF
    DEALLOCATE(field)

    ierr=finter%close()
    ! Close file (free memory)
    ierr=fio%close()
    IF ( ierr /= 0 ) CALL error("Can't free memory")

  END IF
END PROGRAM fortran_test

SUBROUTINE error(error_string)
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: error_string

  WRITE(*,*) '******** ERROR ***********'
  WRITE(*,*) error_string
  WRITE(*,*) '**************************'

  CALL ABORT()
END SUBROUTINE error
