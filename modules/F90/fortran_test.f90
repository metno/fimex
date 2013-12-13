PROGRAM fortran_test
  USE Fimex, ONLY                   : FimexIO, set_filetype, AXIS_GeoX, AXIS_GeoY, AXIS_Lon, AXIS_Lat,INTERPOL_BILINEAR,&
                                      FILETYPE_RW
  IMPLICIT NONE
  TYPE(FimexIO)                   :: fio, finter, frw
  INTEGER                         :: ierr,i
  CHARACTER(LEN=80)               :: input_file
  CHARACTER(LEN=80)               :: config_file
  CHARACTER(LEN=80)               :: varName
  REAL(KIND=8),DIMENSION(:),ALLOCATABLE,TARGET :: field
  REAL(KIND=8),DIMENSION(:,:,:,:),POINTER :: field4d
  REAL(KIND=8),DIMENSION(:,:,:),POINTER :: field3d
  REAL(KIND=8), DIMENSION(:),TARGET,ALLOCATABLE :: lonvals
  REAL(KIND=8), DIMENSION(:),TARGET,ALLOCATABLE :: latvals
  INTEGER                         :: dataRead
  INTEGER                         :: nx,ny
  INTEGER                         :: ndims
  INTEGER(KIND=4), ALLOCATABLE, DIMENSION(:) :: start, vsize, atypes
  INTEGER(KIND=8)          :: vars
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
    ELSE
      config_file = ""
    ENDIF

    ! Open file
    ierr=fio%open(input_file,config_file,set_filetype(cfiletype))
    IF ( ierr /= 0 ) CALL error("Can't make io-object with file:"//trim(input_file)//" config: "//config_file)
    WRITE(0,*) "open_file: success"

    write(*,*) "unlimited dimension = ", TRIM(fio%file_ulim_dimname())

    write(*,*) "dimensions"
    DO i = 1, fio%dimensions_size()
      write(*,*) i, " ", TRIM(fio%file_dimname(i))
    END DO

    write(*,*) "variables"
    DO i = 1, fio%variables_size()
      write(*,*) i, " ", TRIM(fio%get_varname(i))," (", TRIM(fio%get_var_longitude(fio%get_varname(i))), ",",&
                                                        TRIM(fio%get_var_latitude(fio%get_varname(i))), ")"
    END DO

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
    ierr=fio%read(varName,field,cunit)
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
    ierr=fio%read(varName,field,cunit)
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
         WRITE(*,*) "reducind dimension ", i, " ",TRIM(finter%get_dimname(i))
         ierr=finter%reduce_dimension(finter%get_dimname(i), 0, 1)
      END SELECT
    END DO
    WRITE(*,*) "end reduce"
    ALLOCATE(field(nx*ny))
    ierr=finter%read(varName,field,cunit)
    field4d(1:nx,1:ny,1:1,1:1) => field
    IF ( ierr /= 0 ) THEN
      CALL error("Can't read field")
    ELSE
        WRITE(*,*) field4d
    ENDIF

    DEALLOCATE(field)
    ndims=finter%get_dimensions('x')
    ALLOCATE(field(5))
    ierr=finter%read('x',field)
    IF ( ierr /= 0 ) THEN
      CALL error("Can't read field x/latitude")
    ELSE
        WRITE(*,*) field
    ENDIF

    DEALLOCATE(field)
    DEALLOCATE(start)
    DEALLOCATE(vsize)
    DEALLOCATE(atypes)
    ierr=finter%close()

    ! interpolate to lat lon values
    ALLOCATE(lonvals(2))
    ALLOCATE(latvals(2))
    lonvals(1) = 10
    latvals(1) = 60
    lonvals(2) = 10
    latvals(2) = 55
    WRITE(*,*) "method ", INTERPOL_BILINEAR
    ierr = finter%interpolate_lonlat(fio, INTERPOL_BILINEAR, lonvals, latvals)
    IF ( ierr /= 0 ) THEN
       CALL error("Can't interpolate file to lonlat")
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
         WRITE(*,*) "reducind dimension ", i, " ",TRIM(finter%get_dimname(i))
         ierr=finter%reduce_dimension(finter%get_dimname(i), 0, 1)
      END SELECT
    END DO
    WRITE(*,*) "end reduce"
    ALLOCATE(field(nx*ny))
    ierr=finter%read(varName,field,cunit)
    field4d(1:nx,1:ny,1:1,1:1) => field
    IF ( ierr /= 0 ) THEN
      CALL error("Can't read field")
    ELSE
      WRITE(*,*) field4d
    ENDIF

    ndims=finter%get_dimensions(finter%get_var_longitude(varName))
    IF ( ndims == 1 ) then
      WRITE(*,*) TRIM(varName), " has longitude as dimension"
    ELSE
      WRITE(*,*) TRIM(varName), " has longitude as coordinates"
      ierr=finter%read(finter%get_var_longitude(varName),field)
      field4d(1:nx,1:ny,1:1,1:1) => field
      IF ( ierr /= 0 ) THEN
        CALL error("Can't read field longitude")
      ELSE
        WRITE(*,*) field4d
      ENDIF
    ENDIF
    ! write the data to testOut.nc
    ! Open file
    ierr=frw%open("testOut.nc","",set_filetype("netcdf",FILETYPE_RW))
    IF ( ierr /= 0 ) CALL error("Can't make rw-object with file: testOut.nc")
    ndims=frw%get_dimensions("pressure")
    IF ( ndims <= 0 ) CALL error("Can't make slicebuilder for pressure in testOut.nc")
    WRITE(0,*) "frw-get_dimensions: ", ndims
    !
    ! resize the slicebuilder as above, not really needed here since output-size known
    dimname = "time"
    ierr=frw%reduce_dimension(dimname, 0, 1)
    ! write the 1-d field at time 0
    write(*,*) "writing data in cunit to t=0: ", cunit
    ierr=frw%write("pressure", field, cunit)
    IF ( ierr /= 0 ) THEN
      CALL error("Can't write field at t=0")
    ENDIF

    ! write the 1-d field at time-position 1 with 10hPa more
    ierr=frw%reduce_dimension(dimname, 1, 1)
    field = field + 10
    write(*,*) "writing data in cunit to t=1: ", cunit
    ierr=frw%write("pressure", field, cunit)
    IF ( ierr /= 0 ) THEN
      CALL error("Can't write field at t=1")
    ENDIF

    ! write lat/lon axes
    ndims=frw%get_dimensions("lon")
    ! both lonvals are 10, so I can make a 1x2 matrix, but need to reallocate
    ! lonvals
    DEALLOCATE(lonvals)
    ALLOCATE(lonvals(1))
    lonvals(1) = 10
    ierr=frw%write("lon", lonvals, "degrees_east")
    IF ( ierr /= 0 )  CALL error("Can't write lon")
    ndims=frw%get_dimensions("lat")
    ierr=frw%write("lat", latvals, "degrees_north")
    IF ( ierr /= 0 )  CALL error("Can't write lat")

    ierr = frw%close()


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
