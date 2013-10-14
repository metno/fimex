!> @file
!! Fimex F90 interface

!> @brief Fimex Fortran90 interface
!! @author Trygve Aspelien, Heiko Klein
!!
!! The Fimex F90 interface is devided into two parts. Functions starting with c_mifi
!! are wrapper functions against the Fimex C-interface c_fimex.h . Functions without
!! prefix define a high level F90 interface, which should generally be used
!!
!! @warning The fimex.f90 interface is currently not precompiled with building fimex. Please
!! copy the fimex.f90 file to your f90-project and compile it from there, and link with ''-lfimex''.
!!
!! @warning The module stores internally two refernces to file and data-handles. It is
!!          therefore not save to use in parallel.
!!
!! @see https://svn.met.no/viewvc/fimex/trunk/modules/F90/fimex.f90?view=co
MODULE Fimex
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
    !> F90-wrapper for mifi_new_io_reader()
    FUNCTION c_mifi_new_io_reader(filetype,infile,config) BIND(C,NAME="mifi_new_io_reader")
      USE iso_c_binding, ONLY: C_INT,C_PTR,C_CHAR
      IMPLICIT NONE
      CHARACTER(KIND=C_CHAR),INTENT(IN)       :: infile(*)
      CHARACTER(KIND=C_CHAR),INTENT(IN)       :: config(*)
      INTEGER(KIND=C_INT),   INTENT(IN),VALUE :: filetype
      TYPE(C_PTR)                             :: c_mifi_new_io_reader
    END FUNCTION c_mifi_new_io_reader

    !> F90-wrapper for mifi_new_slicebuilder()
    FUNCTION c_mifi_new_slicebuilder(io,varName) BIND(C,NAME="mifi_new_slicebuilder")
      USE iso_c_binding, ONLY: C_PTR,C_CHAR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE                 :: io
      CHARACTER(KIND=C_CHAR),INTENT(IN)  :: varName(*)
      TYPE(C_PTR)                        :: c_mifi_new_slicebuilder
    END FUNCTION c_mifi_new_slicebuilder

    !> F90-wrapper for mifi_slicebuilder_ndims()
    FUNCTION c_mifi_slicebuilder_ndims(sb) BIND(C,NAME="mifi_slicebuilder_ndims")
      USE iso_c_binding, ONLY: C_PTR,C_INT
      IMPLICIT NONE
      TYPE(C_PTR), INTENT(IN), VALUE                 :: sb
      INTEGER(KIND=C_INT)                            :: c_mifi_slicebuilder_ndims
    END FUNCTION c_mifi_slicebuilder_ndims

    !> F90-wrapper for mifi_slicebuilder_get_start_size()
    FUNCTION c_mifi_slicebuilder_get_start_size(sb, start, sbsize) BIND(C,NAME="mifi_slicebuilder_get_start_size")
      USE iso_c_binding, ONLY: C_PTR,C_INT
      IMPLICIT NONE
      TYPE(C_PTR), INTENT(IN), VALUE                 :: sb
      TYPE(C_PTR), VALUE                             :: start
      TYPE(C_PTR), VALUE                             :: sbsize
      INTEGER(KIND=C_INT)                            :: c_mifi_slicebuilder_get_start_size
    END FUNCTION c_mifi_slicebuilder_get_start_size

    !> F90-wrapper for mifi_slicebuilder_set_dim_start_size()
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

    !> F90-wrapper for mifi_slicebuilder_dimname()
    FUNCTION c_mifi_slicebuilder_dimname(sb, pos) BIND(C,NAME="mifi_slicebuilder_dimname")
      USE iso_c_binding, ONLY: C_PTR, C_INT
      IMPLICIT NONE
      TYPE(C_PTR), VALUE                   :: sb
      INTEGER(KIND=C_INT), VALUE           :: pos
      TYPE(C_PTR)                          :: c_mifi_slicebuilder_dimname
    END FUNCTION c_mifi_slicebuilder_dimname

    !> F90-wrapper for mifi_fill_scaled_double_dataslice()
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

    !> F90-wrapper for mifi_read_field()
    FUNCTION c_mifi_read_field(io,cunit,fieldptr,dataRead) BIND(C,NAME="mifi_get_double_dataslice")
      USE iso_c_binding, ONLY: C_INT,C_PTR,C_CHAR,C_DOUBLE
      IMPLICIT NONE
      TYPE(C_PTR),INTENT(IN),VALUE                       :: io
      CHARACTER(KIND=C_CHAR),                INTENT(IN)  :: cunit(*)
      TYPE(C_PTR),INTENT(IN),VALUE                       :: fieldptr
      INTEGER(KIND=C_INT),                   INTENT(OUT) :: dataRead
      INTEGER(KIND=C_INT)                                :: c_mifi_read_field
    END FUNCTION c_mifi_read_field

    !> F90-wrapper for mifi_free_slicebuilder()
    SUBROUTINE c_mifi_free_slicebuilder(sb) BIND(C,NAME="mifi_free_slicebuilder")
      USE iso_c_binding,     ONLY: C_PTR
      IMPLICIT NONE
      TYPE(C_PTR),INTENT(IN),VALUE    :: sb
    END SUBROUTINE c_mifi_free_slicebuilder

    !> F90-wrapper for mifi_free_cdm_reader()
    SUBROUTINE c_mifi_free_cdm_reader(io) BIND(C,NAME="mifi_free_cdm_reader")
      USE iso_c_binding,     ONLY: C_PTR
      IMPLICIT NONE
      TYPE(C_PTR),INTENT(IN),VALUE    :: io
    END SUBROUTINE c_mifi_free_cdm_reader
  END INTERFACE

  !> Interface to read data with different dimension-sizes.
  !! This will use internally the read_data_4d and similar functions
  INTERFACE read_data
    MODULE PROCEDURE read_data
  END INTERFACE read_data

  CONTAINS

  !> translate the filetype from string to internal number
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

  !> open a new file
  !! @param infile filename (or URL) of input
  !! @param config configuration-file, use "" if not applicable
  !! @param filetype see set_filetype()
  !! @param varName optional varname, if used, will call get_dimensions() and return the number of dimensions
  !! @return negative value on error, >= 0 on success, positive number indicate dimensions of varName
  FUNCTION open_file(infile,config,filetype,varName)
    USE iso_c_binding,                ONLY: C_NULL_CHAR,C_ASSOCIATED
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(IN)          :: infile
    CHARACTER(LEN=*),INTENT(IN)          :: config
    INTEGER,         INTENT(IN)          :: filetype
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: varName
    INTEGER                              :: open_file
    INTEGER                              :: ierr

    IF ( C_ASSOCIATED(io) ) CALL c_mifi_free_cdm_reader(io)
    io=c_mifi_new_io_reader(filetype, TRIM(infile)//C_NULL_CHAR,TRIM(config)//C_NULL_CHAR)
    IF ( C_ASSOCIATED(io) ) THEN
      open_file=0
      IF ( PRESENT(varName)) THEN
        open_file=get_dimensions(varName)
      ENDIF
    ELSE
      open_file=-1
    ENDIF
  END FUNCTION open_file

  !> get the number of dimensions of a variable
  !! Read the number of dimensions of a variable. This function
  !! will internally initialize a slicebuilder, too, so all data
  !! and dimension-fetching will be against this variable.
  !! @param varName variable name
  !! @return number of dimensions, negative on error
  FUNCTION get_dimensions(varName)
    USE iso_c_binding,    ONLY: C_NULL_CHAR,C_INT,C_ASSOCIATED,C_LOC
    IMPLICIT NONE
    INTEGER                  :: get_dimensions
    INTEGER :: i, ierr

    CHARACTER(LEN=*)         :: varName

    IF ( C_ASSOCIATED(io) ) THEN
      IF (C_ASSOCIATED(sb)) CALL c_mifi_free_slicebuilder(sb)
      sb=c_mifi_new_slicebuilder(io,TRIM(varName)//C_NULL_CHAR)
      IF ( C_ASSOCIATED(sb) ) THEN
        get_dimensions=c_mifi_slicebuilder_ndims(sb)
      ELSE
        get_dimensions=-1
      ENDIF
    ELSE
      get_dimensions=-99
    ENDIF
  END FUNCTION get_dimensions

  !> get the dimension-name at a position
  !! Get the dimension at a position between 1 and get_dimensions() The position is directly related
  !! to the position in the return arrays of get_dimension_start_size()
  !! @param pos Position in the dimensions-array, 1 <= pos <= get_dimensions()
  !! @return string, or "" on error
  FUNCTION get_dimname(pos)
    USE iso_c_binding,    ONLY: C_NULL_CHAR,C_CHAR,C_INT,C_PTR,C_ASSOCIATED,C_F_POINTER
    IMPLICIT NONE
    INTEGER, INTENT(IN)           :: pos
    CHARACTER(LEN=1024)           :: get_dimname
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
      get_dimname = tmp_name(1:slen)
    ELSE
      get_dimname = ""
    ENDIF
  END FUNCTION get_dimname

  !> get the current start-position and current size for the current variable dimensions
  !! Get the start-position and size for each dimension of the variable set with the last
  !! get_dimensions() call. Initially, start is 0 for each dimension and size is the full
  !! dimension size. This can be changed with reduce_dimension().
  !! @param start pre-allocated array of size get_dimensions(), returns usually 0,0,0,...
  !! @param vsize pre-allocated array of size get_dimensions(), returns the sizes of the dimension
  !! @return 0 on success, negative on error
  FUNCTION get_dimension_start_size(start, vsize)
    USE iso_c_binding,    ONLY: C_INT, C_ASSOCIATED, C_LOC
    INTEGER :: get_dimension_start_size
    INTEGER(KIND=C_INT), DIMENSION(:), ALLOCATABLE, TARGET, INTENT(INOUT) :: start
    INTEGER(KIND=C_INT), DIMENSION(:), ALLOCATABLE, TARGET, INTENT(INOUT) :: vsize

    IF ( C_ASSOCIATED(sb) ) THEN
       get_dimension_start_size = c_mifi_slicebuilder_get_start_size(sb, C_LOC(start), C_LOC(vsize))
    ELSE
       get_dimension_start_size = -1
    END IF
  END FUNCTION get_dimension_start_size

  !> reduce the dimension by setting a start and size
  !! @param dimName dimension name, e.g. retrieved by get_dimname()
  !! @param start start-position in the dimension to retrieve the data, first position is 0!
  !! @param dsize size of the dimension. This may not be too large, e.g. start+size must be < total dimension size
  !! @return 0 on success, otherwise error
  FUNCTION reduce_dimension(dimName, start, dsize)
    USE iso_c_binding,    ONLY: C_ASSOCIATED,C_INT,C_NULL_CHAR
    IMPLICIT NONE
    INTEGER :: reduce_dimension
    CHARACTER(LEN=1024), INTENT(IN)                         :: dimName
    INTEGER(KIND=C_INT), INTENT(IN)                         :: start
    INTEGER(KIND=C_INT), INTENT(IN)                         :: dsize
    INTEGER :: i
    IF ( C_ASSOCIATED(sb) ) THEN
      reduce_dimension = c_mifi_slicebuilder_set_dim_start_size(sb, &
              TRIM(dimName)//C_NULL_CHAR,start, dsize)
    ELSE
      reduce_dimension = -99
    ENDIF
  END FUNCTION reduce_dimension

  !> Read data to a 1-d field.
  !! To work with n-d arrays, use the fortran rank remapping feature
  !! of fortran2003:
  !! @code
  !! REAL(KIND=8),DIMENSION(:),ALLOCATABLE,TARGET :: target
  !! REAL(KIND=8),DIMENSION(:,:,:,:),ALLOCATABLE,POINTER :: field4d
  !!
  !! ALLOCATE(target(PRODUCT(vsize))
  !! ierr=read_data_1d(varName,cunit,target)
  !! ! remap to 4-d
  !! field4d(1:vsize(1),1:vsize(2),1:vsize(3),1:vsize(4)) => target
  !! @endcode
  !! @param varName the variable name to read, must be similar or equal to the one set in get_dimensions()
  !! @param cunit the unit to read the variable in. Use "" to ignore units.
  !! @param field the preallocated multi-dimensional field
  !! @return 0 on success
  FUNCTION read_data(varname, cunit, field)
    USE iso_c_binding,    ONLY: C_PTR,C_NULL_CHAR,C_DOUBLE,C_ASSOCIATED,C_INT,C_LONG_LONG,C_LOC
    IMPLICIT NONE
    CHARACTER(LEN=*)                                        :: varname
    CHARACTER(LEN=*)                                        :: cunit
    REAL(KIND=C_DOUBLE),DIMENSION(:), INTENT(INOUT), ALLOCATABLE, TARGET :: field
    INTEGER                                                 :: read_data

    INTEGER(KIND=C_INT), DIMENSION(:), ALLOCATABLE, TARGET  :: start
    INTEGER(KIND=C_INT), DIMENSION(:), ALLOCATABLE, TARGET  :: vsize
    CHARACTER(LEN=1024)                                     :: dimName
    INTEGER(KIND=C_LONG_LONG)                               :: expSize, outSize
    INTEGER :: i,ierr, ndims

    IF (C_ASSOCIATED(sb) .AND. C_ASSOCIATED(io)) THEN
      ndims = c_mifi_slicebuilder_ndims(sb)
      ALLOCATE(start(ndims))
      ALLOCATE(vsize(ndims))
      ierr = get_dimension_start_size(start, vsize)
      expSize = PRODUCT(vsize)
      IF (expSize /= size(field, KIND=C_LONG_LONG)) THEN
        read_data = -1
        WRITE(*,*) "read_data, allocated field-size != expected size: ", size(field), "!=", expSize
        RETURN
      END IF

      read_data = c_mifi_fill_scaled_double_dataslice(io, trim(varName)//C_NULL_CHAR, sb, &
                                                 trim(cunit)//C_NULL_CHAR, C_LOC(field), outSize)
      IF (read_data /= 0) THEN
        WRITE(*,*) "error filling scaled_double_dataslice ", read_data
        RETURN
      END IF
      IF (outSize /= expSize) THEN
        WRITE(*,*) "unexpected output size ", outSize, ", expected ",expSize
        read_data = -2
        RETURN
      END IF
    ELSE
      read_data = -99
      WRITE(*,*) "read_data, io or sb not initialized"
    END IF
    RETURN
  END FUNCTION read_data

  FUNCTION close_file()
    USE iso_c_binding,   ONLY: C_ASSOCIATED
    IMPLICIT NONE
    INTEGER                 :: close_file

    IF ( C_ASSOCIATED(sb) ) CALL c_mifi_free_slicebuilder(sb)
    IF ( C_ASSOCIATED(io) ) CALL c_mifi_free_cdm_reader(io)
    close_file=0
  END FUNCTION close_file

END MODULE Fimex
