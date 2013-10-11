PROGRAM fortran_test
  USE mifi, ONLY                   : mifi_open_file,mifi_get_dimensions,&
                                     mifi_set_dimensions,mifi_read_field,&
                                     mifi_close_file
  IMPLICIT NONE
  INTEGER                         :: ierr
  CHARACTER(LEN=80)               :: input_file
  CHARACTER(LEN=80)               :: config_file
  INTEGER                         :: filetype
  CHARACTER(LEN=80)               :: varName
  REAL(KIND=8),DIMENSION(:,:,:,:,:,:),ALLOCATABLE :: field
  INTEGER                         :: dataRead
  INTEGER                         :: nx,ny
  INTEGER                         :: ndims
  INTEGER, ALLOCATABLE, DIMENSION(:) :: start, vsize
  CHARACTER(LEN=10)               :: cunit

  input_file="/opdata/arome_norway25/AROME_Norway25_00.nc"
  filetype=1
  config_file=""
  !/metop/arome_norway25/etc/AromeGribReaderConfig.xml"
  varName="surface_air_pressure"
  cunit="hPa"

  ! Open file
  ierr=mifi_open_file(input_file,config_file,filetype)
  IF ( ierr /= 0 ) CALL error("Can't make io-object with file:"//trim(input_file)//" config: "//config_file)
  write(0,*) "mifi_open_file: success"

  ! Get dimensions
  ndims=mifi_get_dimensions(varName, start, vsize)
  IF ( ndims <= 0 ) CALL error("Can't make slicebuilder")
  write(0,*) "mifi_get_dimensions: ", ndims

  ALLOCATE(field(1,1,1,1,1,1))

  ! Set dimensions
  ierr=mifi_set_dimensions()
  IF ( ierr /= 0 ) CALL error("Can't set dimensions")

  ! Read variable
  ierr=mifi_read_field(cunit,field,dataRead)
  IF ( ierr /= 0 ) CALL error("Can't read field")
  DEALLOCATE(field)

  ! Close file (free memory)
  ierr=mifi_close_file()
  IF ( ierr /= 0 ) CALL error("Can't free memory")

END PROGRAM fortran_test

SUBROUTINE error(error_string)
  IMPLICIT NONE
  CHARACTER(*),INTENT(IN) :: error_string

  WRITE(*,*) '******** ERROR ***********'
  WRITE(*,*) error_string
  WRITE(*,*) '**************************'

  CALL ABORT()
END SUBROUTINE error
