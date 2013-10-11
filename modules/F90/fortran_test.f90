PROGRAM fortran_test
  USE mifi, ONLY                   : mifi_open_file,mifi_get_dimensions,&
                                     mifi_read_data, mifi_get_dimname, &
                                     mifi_close_file,set_filetype
  IMPLICIT NONE
  INTEGER                         :: ierr,i
  CHARACTER(LEN=80)               :: input_file
  CHARACTER(LEN=80)               :: config_file
  CHARACTER(LEN=80)               :: varName
  REAL(KIND=8),DIMENSION(:,:,:,:),ALLOCATABLE :: field
  INTEGER                         :: dataRead
  INTEGER                         :: nx,ny
  INTEGER                         :: ndims
  INTEGER, ALLOCATABLE, DIMENSION(:) :: start, vsize
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
    ierr=mifi_open_file(input_file,config_file,set_filetype(cfiletype))
    IF ( ierr /= 0 ) CALL error("Can't make io-object with file:"//trim(input_file)//" config: "//config_file)
    WRITE(0,*) "mifi_open_file: success"

    ! Get dimensions
    ndims=mifi_get_dimensions(varName, start, vsize)
    IF ( ndims <= 0 ) CALL error("Can't make slicebuilder")
    WRITE(0,*) "mifi_get_dimensions: ", ndims

    DO i = 1, ndims
      WRITE(*,*) "dimname ", i, ": ", trim(mifi_get_dimname(i)), vsize(i)
    END DO



    ALLOCATE(field(vsize(1),vsize(2),1,1))
    DO i = 3, ndims
      ! reduce the slice
      vsize(i) = 1
    END DO
    ierr=mifi_read_data(varName,cunit,field,start,vsize)
    IF ( ierr /= 0 ) THEN
      CALL error("Can't read field")
    ELSE
      DO i = 1, 10
        WRITE(*,*) field(i,1,1,1)
      END DO
    ENDIF
    DEALLOCATE(field)

    ! Close file (free memory)
    ierr=mifi_close_file()
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
