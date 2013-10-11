MODULE MIFI
  USE iso_c_binding, ONLY : C_PTR
  IMPLICIT NONE
  
  INTEGER,PARAMETER,PRIVATE   :: mifi_filetype_felt=0
  INTEGER,PARAMETER,PRIVATE   :: mifi_filetype_netcdf=1
  INTEGER,PARAMETER,PRIVATE   :: mifi_filetype_ncml=2
  INTEGER,PARAMETER,PRIVATE   :: mifi_filetype_grib=3
  INTEGER,PARAMETER,PRIVATE   :: mifi_filetype_wdb=4
  INTEGER,PARAMETER,PRIVATE   :: mifi_filetype_metgm=5
  INTEGER,PARAMETER,PRIVATE   :: mifi_filetype_rw=1024
  TYPE(C_PTR),PRIVATE,SAVE    :: io
  TYPE(C_PTR),PRIVATE,SAVE    :: sb

  INTERFACE
    FUNCTION c_mifi_new_io_reader(filetype,infile,config) BIND(C,NAME="mifi_new_io_reader")
      USE iso_c_binding, ONLY: C_INT,C_PTR,C_CHAR
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR),INTENT(IN)       :: infile(*)
      CHARACTER(KIND=C_CHAR),INTENT(IN)       :: config(*)
      INTEGER(KIND=C_INT),   INTENT(IN),VALUE :: filetype
      TYPE(C_PTR)                             :: c_mifi_new_io_reader
    END FUNCTION c_mifi_new_io_reader

    FUNCTION c_mifi_new_slicebuilder(io,varName) BIND(C,NAME="mifi_new_slicebuilder")
      USE iso_c_binding, ONLY: C_PTR,C_CHAR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE                 :: io
      CHARACTER(KIND=C_CHAR),INTENT(IN)  :: varName(*)
      TYPE(C_PTR)                        :: c_mifi_new_slicebuilder
    END FUNCTION c_mifi_new_slicebuilder

    FUNCTION c_mifi_slicebuilder_ndims(sb) BIND(C,NAME="mifi_slicebuilder_ndims")
      USE iso_c_binding, ONLY: C_PTR,C_INT
      IMPLICIT NONE
      TYPE(C_PTR), INTENT(IN), VALUE                 :: sb
      INTEGER(KIND=C_INT)                            :: c_mifi_slicebuilder_ndims
    END FUNCTION c_mifi_slicebuilder_ndims

    FUNCTION c_mifi_slicebuilder_get_start_size(sb, start, sbsize) BIND(C,NAME="mifi_slicebuilder_get_start_size")
      USE iso_c_binding, ONLY: C_PTR,C_INT
      IMPLICIT NONE
      TYPE(C_PTR), INTENT(IN), VALUE                 :: sb
      TYPE(C_PTR), VALUE                             :: start
      TYPE(C_PTR), VALUE                             :: sbsize
      INTEGER(KIND=C_INT)                            :: c_mifi_slicebuilder_get_start_size
    END FUNCTION c_mifi_slicebuilder_get_start_size

    FUNCTION c_mifi_slicebuilder_set_dim_start_size(sb, dimName, start, sbsize)&
          BIND(C,NAME="mifi_slicebuilder_set_dim_start_size")
      USE iso_c_binding, ONLY: C_PTR,C_INT,C_CHAR
      IMPLICIT NONE
      TYPE(C_PTR), INTENT(IN), VALUE                 :: sb
      CHARACTER(KIND=C_CHAR), INTENT(IN)             :: dimName(*)
      INTEGER(KIND=C_INT), VALUE                     :: start
      INTEGER(KIND=C_INT), VALUE                     :: sbsize
      INTEGER(KIND=C_INT)                            :: c_mifi_slicebuilder_set_dim_start_size
    END FUNCTION c_mifi_slicebuilder_set_dim_start_size

    FUNCTION c_mifi_slicebuilder_dimname(sb, pos) BIND(C,NAME="mifi_slicebuilder_dimname")
      USE iso_c_binding, ONLY: C_PTR, C_INT
      IMPLICIT NONE
      TYPE(C_PTR), VALUE                   :: sb
      INTEGER(KIND=C_INT), VALUE           :: pos
      TYPE(C_PTR)                          :: c_mifi_slicebuilder_dimname
    END FUNCTION c_mifi_slicebuilder_dimname

    FUNCTION c_mifi_fill_scaled_double_dataslice(io, varName, sb, units, data, dsize) &
          BIND(C,NAME="mifi_fill_scaled_double_dataslice")
      USE iso_c_binding, ONLY: C_PTR,C_CHAR,C_LONG_LONG, C_INT
      IMPLICIT NONE
      TYPE(C_PTR), VALUE                 :: io
      CHARACTER(KIND=C_CHAR),INTENT(IN)  :: varName(*)
      TYPE(C_PTR), VALUE                 :: sb
      CHARACTER(KIND=C_CHAR),INTENT(IN)  :: units(*)
      TYPE(C_PTR), VALUE                 :: data
      INTEGER(KIND=C_LONG_LONG),INTENT(OUT) :: dsize
      INTEGER(KIND=C_INT)                :: c_mifi_fill_scaled_double_dataslice
    END FUNCTION c_mifi_fill_scaled_double_dataslice

    FUNCTION c_mifi_read_field(io,cunit,fieldptr,dataRead) BIND(C,NAME="mifi_get_double_dataslice")
      USE iso_c_binding, ONLY: C_INT,C_PTR,C_CHAR,C_DOUBLE
      IMPLICIT NONE
      TYPE(C_PTR),INTENT(IN),VALUE                       :: io
      CHARACTER(KIND=C_CHAR),                INTENT(IN)  :: cunit(*)
      TYPE(C_PTR),INTENT(IN),VALUE                       :: fieldptr
      INTEGER(KIND=C_INT),                   INTENT(OUT) :: dataRead
      INTEGER(KIND=C_INT)                                :: c_mifi_read_field
    END FUNCTION c_mifi_read_field

    SUBROUTINE c_mifi_free_slicebuilder(sb) BIND(C,NAME="mifi_free_slicebuilder")
      USE iso_c_binding,     ONLY: C_PTR
      IMPLICIT NONE
      TYPE(C_PTR),INTENT(IN),VALUE    :: sb
    END SUBROUTINE c_mifi_free_slicebuilder

    SUBROUTINE c_mifi_free_cdm_reader(io) BIND(C,NAME="mifi_free_cdm_reader")
      USE iso_c_binding,     ONLY: C_PTR
      IMPLICIT NONE
      TYPE(C_PTR),INTENT(IN),VALUE    :: io
    END SUBROUTINE c_mifi_free_cdm_reader
  END INTERFACE

  INTERFACE mifi_read_data
    MODULE PROCEDURE mifi_read_data_4d
  END INTERFACE mifi_read_data

  CONTAINS

  FUNCTION set_filetype(cfiletype)
    IMPLICIT NONE
    CHARACTER(LEN=10), INTENT(IN) :: cfiletype
    INTEGER                       :: set_filetype
    SELECT CASE (TRIM(cfiletype))
      CASE ("felt","FELT")
         set_filetype=mifi_filetype_felt
      CASE ("netcdf","NETCDF","nc","NC")
         set_filetype=mifi_filetype_netcdf
      CASE ("grib","GRIB","grb","GRB")
         set_filetype=mifi_filetype_grib
      CASE ("ncml","NCML")
         set_filetype=mifi_filetype_ncml
      CASE ("wdb","WDB")
         set_filetype=mifi_filetype_wdb
      CASE ("metgm","METGM")
         set_filetype=mifi_filetype_metgm
      CASE ("rw","RW")
         set_filetype=mifi_filetype_rw
      CASE DEFAULT
        WRITE(*,*) "Filetype not defined: "//TRIM(cfiletype)
        CALL abort()
    END SELECT
  END FUNCTION set_filetype

  FUNCTION mifi_open_file(infile,config,filetype,varName)
    USE iso_c_binding,                ONLY: C_NULL_CHAR,C_ASSOCIATED
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)          :: infile
    CHARACTER(LEN=*),INTENT(IN)          :: config
    INTEGER,         INTENT(IN)          :: filetype
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: varName
    INTEGER, DIMENSION(:), ALLOCATABLE :: start
    INTEGER, DIMENSION(:), ALLOCATABLE :: vsize
    INTEGER                              :: mifi_open_file
    INTEGER                              :: ierr

    IF ( C_ASSOCIATED(io) ) CALL c_mifi_free_cdm_reader(io)
    io=c_mifi_new_io_reader(filetype, TRIM(infile)//C_NULL_CHAR,TRIM(config)//C_NULL_CHAR)
    IF ( C_ASSOCIATED(io) ) THEN
      mifi_open_file=0
      IF ( PRESENT(varName)) THEN
        mifi_open_file=mifi_get_dimensions(varName, start, vsize)
      ENDIF
    ELSE
      mifi_open_file=-1
    ENDIF
  END FUNCTION mifi_open_file

  FUNCTION mifi_get_dimensions(varName, start, vsize)
    USE iso_c_binding,    ONLY: C_NULL_CHAR,C_INT,C_ASSOCIATED,C_LOC
    IMPLICIT NONE
    INTEGER                  :: mifi_get_dimensions
    INTEGER(KIND=C_INT), DIMENSION(:), ALLOCATABLE, TARGET, INTENT(OUT) :: start
    INTEGER(KIND=C_INT), DIMENSION(:), ALLOCATABLE, TARGET, INTENT(OUT) :: vsize
    INTEGER :: i, ierr

    CHARACTER(LEN=*)         :: varName

    IF ( C_ASSOCIATED(io) ) THEN
      IF (C_ASSOCIATED(sb)) CALL c_mifi_free_slicebuilder(sb)
      sb=c_mifi_new_slicebuilder(io,TRIM(varName)//C_NULL_CHAR)
      IF ( C_ASSOCIATED(sb) ) THEN
        mifi_get_dimensions=c_mifi_slicebuilder_ndims(sb)
        ALLOCATE(start(mifi_get_dimensions))
        ALLOCATE(vsize(mifi_get_dimensions))
        ierr = c_mifi_slicebuilder_get_start_size(sb, C_LOC(start), C_LOC(vsize))
      ELSE
        mifi_get_dimensions=-1
      ENDIF
    ELSE
      mifi_get_dimensions=-99
    ENDIF
  END FUNCTION mifi_get_dimensions

  FUNCTION mifi_get_dimname(pos)
    USE iso_c_binding,    ONLY: C_NULL_CHAR,C_CHAR,C_INT,C_PTR,C_ASSOCIATED,C_F_POINTER
    IMPLICIT NONE
    INTEGER, INTENT(IN)           :: pos
    CHARACTER(LEN=1024)           :: mifi_get_dimname
    INTEGER(KIND=C_INT)           :: c_pos
    TYPE(C_PTR)                   :: str_ptr
    INTEGER                       :: slen
    CHARACTER(LEN=1024), POINTER  :: tmp_name

    c_pos = pos - 1
    slen = 0
    IF ( C_ASSOCIATED(sb) ) THEN
      str_ptr = c_mifi_slicebuilder_dimname(sb, c_pos)
      call C_F_POINTER(str_ptr,tmp_name)
      slen = INDEX(tmp_name,C_NULL_CHAR) - 1
      mifi_get_dimname = tmp_name(1:slen)
    ELSE
      mifi_get_dimname = ""
    ENDIF
  END FUNCTION mifi_get_dimname

  FUNCTION mifi_read_data_4d(varname, cunit, field, start, fsize)
    USE iso_c_binding,    ONLY: C_PTR,C_NULL_CHAR,C_DOUBLE,C_ASSOCIATED,C_INT,C_LONG_LONG,C_LOC
    IMPLICIT NONE
    CHARACTER(LEN=*)                                        :: varname
    CHARACTER(LEN=*)                                        :: cunit
    REAL(KIND=C_DOUBLE),DIMENSION(:,:,:,:), INTENT(INOUT), ALLOCATABLE, TARGET :: field
    INTEGER(KIND=C_INT), DIMENSION(:),INTENT(IN)            :: start
    INTEGER(KIND=C_INT), DIMENSION(:),INTENT(IN)            :: fsize
    INTEGER                                                 :: mifi_read_data_4d

    CHARACTER(LEN=1024)                                     :: dimName
    INTEGER(KIND=C_LONG_LONG)                               :: expSize, outSize
    INTEGER :: i

    expSize = 1
    DO i = 1, size(fsize)
      expSize = expSize * fsize(i)
    END DO
    IF (expSize /= size(field, KIND=C_LONG_LONG)) THEN
      mifi_read_data_4d = -1
      WRITE(*,*) "mifi_read_data, allocated field-size != expected size: ", size(field), expSize
      RETURN
    END IF
    IF (C_ASSOCIATED(sb) .AND. C_ASSOCIATED(io)) THEN
      DO i = 1, size(start)
        dimName = mifi_get_dimname(i)
        WRITE(*,*) trim(dimName), start(i), fsize(i)
        mifi_read_data_4d = c_mifi_slicebuilder_set_dim_start_size(sb, TRIM(dimName)//C_NULL_CHAR,start(i), fsize(i))
        IF (mifi_read_data_4d /= 0) THEN
          WRITE(*,*) "error setting slicebuilder ", mifi_read_data_4d
          RETURN
        END IF
      END DO
      mifi_read_data_4d = c_mifi_fill_scaled_double_dataslice(io, trim(varName)//C_NULL_CHAR, sb, &
                                                 trim(cunit)//C_NULL_CHAR, C_LOC(field), outSize)
      IF (mifi_read_data_4d /= 0) THEN
        WRITE(*,*) "error filling scaled_double_dataslice ", mifi_read_data_4d
        RETURN
      END IF
      IF (outSize /= expSize) THEN
        WRITE(*,*) "unexpected output size ", outSize, ", expected ",expSize
        mifi_read_data_4d = -2
        RETURN
      END IF
    ELSE
      mifi_read_data_4d = -99
      WRITE(*,*) "mifi_read_data, io or sb not initialized"
    END IF
    RETURN
  END FUNCTION mifi_read_data_4d

  FUNCTION mifi_close_file()
    USE iso_c_binding,   ONLY: C_ASSOCIATED
    IMPLICIT NONE
    INTEGER                 :: mifi_close_file

    IF ( C_ASSOCIATED(sb) ) CALL c_mifi_free_slicebuilder(sb)
    IF ( C_ASSOCIATED(io) ) CALL c_mifi_free_cdm_reader(io)
    mifi_close_file=0
  END FUNCTION mifi_close_file

END MODULE
