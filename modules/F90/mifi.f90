MODULE MIFI
  USE iso_c_binding, ONLY : C_PTR
  IMPLICIT NONE
  TYPE(C_PTR),PRIVATE,SAVE    :: io
  TYPE(C_PTR),PRIVATE,SAVE    :: sb

  INTERFACE
    FUNCTION mifi_new_io_reader(filetype,infile,config) BIND(C,NAME="mifi_new_io_reader")
      USE iso_c_binding, ONLY: C_INT,C_PTR,C_CHAR
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR),INTENT(IN)       :: infile(*)
      CHARACTER(KIND=C_CHAR),INTENT(IN)       :: config(*)
      INTEGER(KIND=C_INT),   INTENT(IN),VALUE :: filetype
      TYPE(C_PTR)                             :: mifi_new_io_reader
    END FUNCTION mifi_new_io_reader

    FUNCTION mifi_new_slicebuilder(io,varName) BIND(C,NAME="mifi_new_slicebuilder")
      USE iso_c_binding, ONLY: C_PTR,C_CHAR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE                 :: io
      CHARACTER(KIND=C_CHAR),INTENT(IN)  :: varName(*)
      TYPE(C_PTR)                        :: mifi_new_slicebuilder
    END FUNCTION

    FUNCTION mifi_slicebuilder_ndims(sb) BIND(C,NAME="mifi_slicebuilder_ndims")
      USE iso_c_binding, ONLY: C_PTR,C_INT
      IMPLICIT NONE
      TYPE(C_PTR), INTENT(IN), VALUE                 :: sb
      INTEGER(KIND=C_INT)                            :: mifi_slicebuilder_ndims
    END FUNCTION


    FUNCTION c_mifi_set_dimensions(sb) BIND(C,NAME="set_dimensions")
      USE iso_c_binding, ONLY: C_INT,C_PTR,C_CHAR
      IMPLICIT NONE
      TYPE(C_PTR),INTENT(IN),VALUE       :: sb
      INTEGER(KIND=C_INT)                :: c_mifi_set_dimensions
    END FUNCTION

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
    END SUBROUTINE

    SUBROUTINE c_mifi_free_cdm_reader(io) BIND(C,NAME="mifi_free_cdm_reader")
      USE iso_c_binding,     ONLY: C_PTR
      IMPLICIT NONE
      TYPE(C_PTR),INTENT(IN),VALUE    :: io
    END SUBROUTINE
  END INTERFACE
  INTERFACE mifi_read_field
    MODULE PROCEDURE mifi_read_field_6d
    !MODULE PROCEDURE mifi_read_field_5d
    !MODULE PROCEDURE mifi_read_field_4d
    !MODULE PROCEDURE mifi_read_field_3d
    !MODULE PROCEDURE mifi_read_field_2d
    !MODULE PROCEDURE mifi_read_field_1d
    !MODULE PROCEDURE mifi_read_field_0d
  END INTERFACE
  CONTAINS

  FUNCTION mifi_open_file(infile,config,filetype,varName)
    USE iso_c_binding,                ONLY: C_NULL_CHAR,C_ASSOCIATED
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)          :: infile
    CHARACTER(LEN=*),INTENT(IN)          :: config
    INTEGER,         INTENT(IN)          :: filetype
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: varName
    INTEGER                              :: mifi_open_file

    io=mifi_new_io_reader(filetype, TRIM(infile)//C_NULL_CHAR,TRIM(config)//C_NULL_CHAR)
    IF ( C_ASSOCIATED(io) ) THEN
      mifi_open_file=0
      IF ( PRESENT(varName)) THEN
        mifi_open_file=mifi_get_dimensions(varName)
      ENDIF
    ELSE
      mifi_open_file=-1
    ENDIF
  END FUNCTION mifi_open_file

  FUNCTION mifi_get_dimensions(varName)
    USE iso_c_binding,    ONLY: C_NULL_CHAR,C_ASSOCIATED
    IMPLICIT NONE
    INTEGER                  :: mifi_get_dimensions
    CHARACTER(LEN=*)         :: varName

    IF ( C_ASSOCIATED(io) ) THEN
      sb=mifi_new_slicebuilder(io,TRIM(varName)//C_NULL_CHAR)
      IF ( C_ASSOCIATED(sb) ) THEN
        mifi_get_dimensions=mifi_slicebuilder_ndims(sb)
        write (0,*) mifi_get_dimensions
      ELSE
        mifi_get_dimensions=-1
      ENDIF
    ELSE
      mifi_get_dimensions=-99
    ENDIF
  END FUNCTION mifi_get_dimensions

  FUNCTION mifi_set_dimensions()
    USE iso_c_binding,    ONLY: C_PTR
    IMPLICIT NONE
    INTEGER                 :: mifi_set_dimensions

    mifi_set_dimensions=c_mifi_set_dimensions(sb)
  END FUNCTION mifi_set_dimensions

  FUNCTION mifi_read_field_6d(cunit,field,dataRead)
    USE iso_c_binding,    ONLY: C_PTR,C_DOUBLE,C_ASSOCIATED,c_f_pointer
    IMPLICIT NONE
    CHARACTER(LEN=*)                                        :: cunit
    REAL(KIND=C_DOUBLE),DIMENSION(:,:,:,:,:,:), INTENT(OUT) :: field
    INTEGER                                                 :: dataRead
    INTEGER                                                 :: mifi_read_field_6d
    TYPE(C_PTR)                                             :: dataPointer
    INTEGER                                                 :: ierr
    INTEGER(KIND=8),                            POINTER     :: fdataPointer


    mifi_read_field_6d=0
    ! Read the data and get the pointer
    ierr=c_mifi_read_field(io,cunit,dataPointer,dataRead)
    IF ( ierr /= 0 ) THEN
      IF ( C_ASSOCIATED(dataPointer) ) THEN

        ! Convert pointer to field
        CALL c_f_pointer(dataPointer,fdataPointer)
        write(*,*) "Could set field"
      ELSE
        mifi_read_field_6d=-1
      ENDIF
    ELSE
      mifi_read_field_6d=ierr
    ENDIF
  END FUNCTION mifi_read_field_6d

  FUNCTION mifi_close_file()
    USE iso_c_binding,   ONLY: C_ASSOCIATED
    IMPLICIT NONE
    INTEGER                 :: mifi_close_file

    IF ( C_ASSOCIATED(sb) ) CALL c_mifi_free_slicebuilder(sb)
    IF ( C_ASSOCIATED(io) ) CALL c_mifi_free_cdm_reader(io)
    mifi_close_file=0
  END FUNCTION mifi_close_file

END MODULE
