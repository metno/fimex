PROGRAM fortran_test
  USE mifi, ONLY                   : mifi_open_file,mifi_get_dimensions,&
                                     mifi_read_data, mifi_get_dimname, &
                                     mifi_close_file
  IMPLICIT NONE
  INTEGER                         :: ierr,i
  CHARACTER(LEN=80)               :: input_file
  CHARACTER(LEN=80)               :: config_file
  INTEGER                         :: filetype
  CHARACTER(LEN=80)               :: varName
  REAL(KIND=8),DIMENSION(:,:,:,:),ALLOCATABLE :: field
  INTEGER                         :: dataRead
  INTEGER                         :: nx,ny
  INTEGER                         :: ndims
  INTEGER, ALLOCATABLE, DIMENSION(:) :: start, vsize
  CHARACTER(LEN=10)               :: cunit
  CHARACTER(LEN=1024)             :: dimname

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

  do i = 1, ndims
    write(*,*) "dimname ", i, ": ", trim(mifi_get_dimname(i)), vsize(i)
  end do



  ALLOCATE(field(vsize(1),vsize(2),1,1))
  do i = 3, ndims
    ! reduce the slice
    vsize(i) = 1
  end do
  ierr=mifi_read_data(varName,cunit,field,start,vsize)
  IF ( ierr /= 0 ) THEN
    CALL error("Can't read field")
  ELSE
    do i = 1, 10
      write(*,*) field(i,1,1,1)
    end do
  ENDIF
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
