!> @file
!! Fimex F90 interface

!> @brief Fimex Fortran90 interface
!! @author Trygve Aspelien, Heiko Klein
!!
!! The Fimex F90 interface is devided into two parts. Functions starting with c_mifi
!! are wrapper functions against the Fimex C-interface c_fimex.h . Functions without
!! prefix define a high level F90 interface, which should generally be used
!!
!! Besides the documented functions, the following constants can be used:
!!     AXIS_Undefined = 0, AXIS_GeoX, AXIS_GeoY, AXIS_GeoZ, AXIS_Time, AXIS_Lon, AXIS_Lat,
!!     AXIS_Pressure, AXIS_Height, AXIS_ReferenceTime
!!     INTERPOL_NEAREST_NEIGHBOR = 0, INTERPOL_BILINEAR, INTERPOL_BICUBIC, INTERPOL_COORD_NN,
!!      INTERPOL_COORD_NN_KD, INTERPOL_FORWARD_SUM, INTERPOL_FORWARD_MEAN, INTERPOL_FORWARD_MEDIAN,
!!      INTERPOL_FORWARD_MAX, INTERPOL_FORWARD_MIN
!! FILETYPE_UNKNOWN=-1, FILETYPE_FELT, FILETYPE_NETCDF,&
!!      FILETYPE_NCML, FILETYPE_GRIB, FILETYPE_WDB, FILETYPE_METGM,&
!!      FILETYPE_RW=1024
!!
!! The fimex.f90 interface is currently not precompiled with building fimex. Please
!! copy the fimex.f90 file to your f90-project and compile it from there, and link with ''-lfimex''.
!!
!! @see https://svn.met.no/viewvc/fimex/trunk/modules/F90/fimex.f90?view=co
MODULE Fimex
  USE iso_c_binding, ONLY : C_PTR
  IMPLICIT NONE

  !> Axis-definitions
  !! These are the same definitions as in CoordinateAxis::AxisType
  ENUM, BIND(C)
    ENUMERATOR :: AXIS_Undefined = 0, AXIS_GeoX, AXIS_GeoY, AXIS_GeoZ, AXIS_Time, AXIS_Lon, AXIS_Lat,&
     AXIS_Pressure, AXIS_Height, AXIS_ReferenceTime
  END ENUM
  !> @enum Interpolation methods
  !! These are the same definintions as #mifi_interpol_method in mifi_constants.h
  ENUM, BIND(C)
    ENUMERATOR :: INTERPOL_NEAREST_NEIGHBOR = 0, INTERPOL_BILINEAR, INTERPOL_BICUBIC, INTERPOL_COORD_NN,&
      INTERPOL_COORD_NN_KD, INTERPOL_FORWARD_SUM, INTERPOL_FORWARD_MEAN, INTERPOL_FORWARD_MEDIAN,&
      INTERPOL_FORWARD_MAX, INTERPOL_FORWARD_MIN
  END ENUM
  !> @enum Filetypes
  !! These are the same definintions as #mifi_filetype in CDMconstants.h
  ENUM, BIND(C)
    ENUMERATOR ::     FILETYPE_UNKNOWN=-1, FILETYPE_FELT, FILETYPE_NETCDF,&
      FILETYPE_NCML, FILETYPE_GRIB, FILETYPE_WDB, FILETYPE_METGM,&
      FILETYPE_RW=1024
  END ENUM

  !> Class to store file-handles for the high-level API.
  !! @warning The class FimexIO stores internally two refernces to file and data-handles.
  !!    It should therefore not be accessed from two parallel threads
  TYPE, PUBLIC :: FimexIO
    TYPE(C_PTR),PRIVATE    :: io
    TYPE(C_PTR),PRIVATE    :: sb
  CONTAINS
    procedure :: open => open_file
    procedure :: interpolate => new_interpolator
    procedure :: interpolate_lonlat => new_lonlat_interpolator
    procedure :: close => close_file
    procedure :: get_dimensions
    procedure :: get_dimname
    procedure :: get_dimension_start_size
    procedure :: get_axistypes
    procedure :: reduce_dimension
    procedure :: read_data
    procedure :: write_data
  END TYPE
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

    !> F90-wrapper for mifi_new_cdminterpolator()
    FUNCTION c_mifi_new_cdminterpolator(io, method, proj_input, out_x_axis, out_y_axis, out_x_axis_unit, out_y_axis_unit)&
           BIND(C,NAME="mifi_new_cdminterpolator")
      USE iso_c_binding, ONLY: C_INT,C_PTR,C_CHAR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE                      :: io
      INTEGER(KIND=C_INT), VALUE              :: method
      CHARACTER(KIND=C_CHAR),INTENT(IN)       :: proj_input(*)
      CHARACTER(KIND=C_CHAR),INTENT(IN)       :: out_x_axis(*)
      CHARACTER(KIND=C_CHAR),INTENT(IN)       :: out_y_axis(*)
      CHARACTER(KIND=C_CHAR),INTENT(IN)       :: out_x_axis_unit(*)
      CHARACTER(KIND=C_CHAR),INTENT(IN)       :: out_y_axis_unit(*)
      TYPE(C_PTR)                             :: c_mifi_new_cdminterpolator
    END FUNCTION

    !> F90-wrapper for mifi_new_cdminterpolator()
    FUNCTION c_mifi_new_lonlat_interpolator(io, method, n, lonvals, latvals)&
           BIND(C,NAME="mifi_new_lonlat_interpolator")
      USE iso_c_binding, ONLY: C_INT,C_PTR
      IMPLICIT NONE
      TYPE(C_PTR), VALUE                      :: io
      INTEGER(KIND=C_INT), VALUE              :: method
      INTEGER(KIND=C_INT), VALUE              :: n
      TYPE(C_PTR),VALUE                       :: lonvals
      TYPE(C_PTR),VALUE                       :: latvals
      TYPE(C_PTR)                             :: c_mifi_new_lonlat_interpolator
    END FUNCTION


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

    !> F90-wrapper for mifi_slicebuilder_get_axistype()
    FUNCTION c_mifi_slicebuilder_get_axistype(sb, atypes) BIND(C,NAME="mifi_slicebuilder_get_axistype")
      USE iso_c_binding, ONLY: C_PTR,C_INT
      IMPLICIT NONE
      TYPE(C_PTR), INTENT(IN), VALUE                 :: sb
      TYPE(C_PTR), VALUE                             :: atypes
      INTEGER(KIND=C_INT)                            :: c_mifi_slicebuilder_get_axistype
    END FUNCTION c_mifi_slicebuilder_get_axistype


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

    !> F90-wrapper for mifi_write_scaled_double_dataslice()
    FUNCTION c_mifi_write_scaled_double_dataslice(io, varName, sb, units, data, dsize) &
          BIND(C,NAME="mifi_write_scaled_double_dataslice")
      USE iso_c_binding, ONLY: C_PTR,C_CHAR,C_LONG_LONG, C_INT
      IMPLICIT NONE
      TYPE(C_PTR), VALUE                 :: io
      CHARACTER(KIND=C_CHAR),INTENT(IN)  :: varName(*)
      TYPE(C_PTR), VALUE                 :: sb
      CHARACTER(KIND=C_CHAR),INTENT(IN)  :: units(*)
      TYPE(C_PTR), VALUE                 :: data
      INTEGER(KIND=C_LONG_LONG),VALUE    :: dsize
      INTEGER(KIND=C_INT)                :: c_mifi_write_scaled_double_dataslice
    END FUNCTION c_mifi_write_scaled_double_dataslice

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

    !> F90-wrapper for mifi_get_filetype()
    FUNCTION c_mifi_get_filetype(typename) BIND(C,NAME="mifi_get_filetype")
      USE iso_c_binding, ONLY: C_INT,C_CHAR
      CHARACTER(KIND=C_CHAR), INTENT(IN)  :: typename(*)
      INTEGER(KIND=C_INT)                 :: c_mifi_get_filetype
    END FUNCTION c_mifi_get_filetype
  END INTERFACE

  CONTAINS

  !> translate the filetype from string to internal number
  !! @param filetype_name filetype as "fimex", "netcdf", "grib", ...
  !! @return integer filetype
  FUNCTION set_filetype(filetype_name)
    USE iso_c_binding, ONLY: C_NULL_CHAR
    IMPLICIT NONE
    CHARACTER(LEN=10), INTENT(IN) :: filetype_name
    INTEGER                       :: set_filetype
    set_filetype = c_mifi_get_filetype(TRIM(filetype_name)//C_NULL_CHAR);
    IF (set_filetype < 0) THEN
      WRITE(*,*) "Filetype not defined: "//TRIM(filetype_name)
      CALL abort()
    ENDIF
  END FUNCTION set_filetype

  !> Open a new data-soure.
  !! @param this the new FimexIO object.
  !! @param infile filename (or URL) of input
  !! @param config configuration-file, use "" if not applicable
  !! @param filetype see set_filetype(), to open a file read-writable use IOR(FILETYPE_NETCDF,FILETYPE_RW)
  !! @param varName optional varname, if used, will call get_dimensions() and return the number of dimensions
  !! @return negative value on error, >= 0 on success, positive number indicate dimensions of varName
  FUNCTION open_file(this, infile,config,filetype,varName)
    USE iso_c_binding,                ONLY: C_NULL_CHAR,C_ASSOCIATED
    IMPLICIT NONE
    CLASS(FimexIO), INTENT(OUT)           :: this
    CHARACTER(LEN=*),INTENT(IN)          :: infile
    CHARACTER(LEN=*),INTENT(IN)          :: config
    INTEGER,         INTENT(IN)          :: filetype
    CHARACTER(LEN=*),INTENT(IN),OPTIONAL :: varName
    INTEGER                              :: open_file

    INTEGER                              :: ierr
    TYPE(C_PTR)                          :: io

    IF ( C_ASSOCIATED(this%io) ) CALL c_mifi_free_cdm_reader(this%io)
    io=c_mifi_new_io_reader(filetype, TRIM(infile)//C_NULL_CHAR,TRIM(config)//C_NULL_CHAR)
    IF ( C_ASSOCIATED(io) ) THEN
      open_file=0
      this%io = io
      IF ( PRESENT(varName)) THEN
        open_file=this%get_dimensions(varName)
      ENDIF
    ELSE
      open_file=-1
    ENDIF
  END FUNCTION open_file

  !> Create a new interpolated data-soure from an existing FimexIO.
  !! @param this the new FimexIO object. It must be closed with close_file.
  !! @param fio the input data-source
  !! @param method one of INTERPOL_bilinear, INTERPOL_.... methods
  !! @param proj_input a proj4 text string
  !! @param out_x_axis a description string of the x-axis, e.g. "10,20,...,1000", see MetNoFimex::SpatialAxisSpec
  !! @param out_y_axis a description string of the y-axis, e.g. "10,20,...,1000", see MetNoFimex::SpatialAxisSpec
  !! @param out_is_degree output-axis are in degree (true) or meter (false)
  !! @return negative value on error, >= 0 on success
  FUNCTION new_interpolator(this, fio, method, proj_input, out_x_axis, out_y_axis, out_is_degree)
    USE iso_c_binding,                ONLY: C_CHAR,C_NULL_CHAR,C_ASSOCIATED,C_INT
    IMPLICIT NONE
    CLASS(FimexIO), INTENT(OUT)          :: this
    CLASS(FimexIO), INTENT(IN)           :: fio
    INTEGER, INTENT(IN)                  :: method
    CHARACTER(LEN=*),INTENT(IN)          :: proj_input
    CHARACTER(LEN=*),INTENT(IN)          :: out_x_axis
    CHARACTER(LEN=*),INTENT(IN)          :: out_y_axis
    LOGICAL, INTENT(IN)                  :: out_is_degree
    INTEGER                              :: new_interpolator

    TYPE(C_PTR)                          :: interpol
    CHARACTER(LEN=10)                    :: deg_or_m

    IF ( .not. C_ASSOCIATED(fio%io) ) THEN
      new_interpolator = -99
      RETURN
    ENDIF
    IF (out_is_degree) THEN
      deg_or_m = "degree"
    ELSE
      deg_or_m = "m"
    ENDIF
    interpol = c_mifi_new_cdminterpolator(fio%io, method, TRIM(proj_input)//C_NULL_CHAR,&
                                          TRIM(out_x_axis)//C_NULL_CHAR, TRIM(out_y_axis)//C_NULL_CHAR,&
                                          TRIM(deg_or_m)//C_NULL_CHAR, TRIM(deg_or_m)//C_NULL_CHAR);
    IF (.not. C_ASSOCIATED(interpol) ) THEN
      new_interpolator = -1
    ELSE
      this%io = interpol
      new_interpolator = 0
    ENDIF
  END FUNCTION


  !> Create a new interpolated data-soure from an existing FimexIO.
  !! @param this the new FimexIO object. It must be closed with close_file.
  !! @param fio the input data-source
  !! @param method one of INTERPOL_bilinear, INTERPOL_.... methods
  !! @param lonvals longitude positions
  !! @param latvals longitude positions
  !! @return negative value on error, >= 0 on success
  FUNCTION new_lonlat_interpolator(this, fio, method, lonvals, latvals)
    USE iso_c_binding,                ONLY: C_ASSOCIATED,C_INT,C_DOUBLE,C_LOC
    IMPLICIT NONE
    CLASS(FimexIO), INTENT(OUT)          :: this
    CLASS(FimexIO), INTENT(IN)           :: fio
    INTEGER, INTENT(IN)                  :: method
    REAL(KIND=C_DOUBLE), DIMENSION(:), TARGET, INTENT(IN), ALLOCATABLE :: lonvals
    REAL(KIND=C_DOUBLE), DIMENSION(:), TARGET, INTENT(IN), ALLOCATABLE :: latvals
    INTEGER                              :: new_lonlat_interpolator

    TYPE(C_PTR)                          :: interpol
    INTEGER(KIND=C_INT)                  :: n

    IF ( .not. C_ASSOCIATED(fio%io) ) THEN
      new_lonlat_interpolator = -99
      RETURN
    ENDIF
    IF ( SIZE(latvals) /= SIZE(lonvals)) THEN
      new_lonlat_interpolator = -2
      RETURN
    ENDIF
    n = SIZE(latvals)
    interpol = c_mifi_new_lonlat_interpolator(fio%io, method, n, C_LOC(lonvals), C_LOC(latvals))
    IF (.not. C_ASSOCIATED(interpol) ) THEN
      new_lonlat_interpolator = -1
    ELSE
      this%io = interpol
      new_lonlat_interpolator = 0
    ENDIF
  END FUNCTION

  !> Get the number of dimensions of a variable.
  !! This function
  !! will internally initialize a slicebuilder, too, so all data
  !! and dimension-fetching will be against this variable.
  !! @param varName variable name
  !! @return number of dimensions, negative on error
  FUNCTION get_dimensions(this, varName)
    USE iso_c_binding,    ONLY: C_NULL_CHAR,C_INT,C_ASSOCIATED,C_LOC
    IMPLICIT NONE
    INTEGER                  :: get_dimensions
    INTEGER :: i, ierr

    CLASS(FimexIO), INTENT(INOUT)       :: this
    CHARACTER(LEN=*), INTENT(IN)     :: varName

    IF ( C_ASSOCIATED(this%io) ) THEN
      IF (C_ASSOCIATED(this%sb)) CALL c_mifi_free_slicebuilder(this%sb)
      this%sb=c_mifi_new_slicebuilder(this%io,TRIM(varName)//C_NULL_CHAR)
      IF ( C_ASSOCIATED(this%sb) ) THEN
        get_dimensions=c_mifi_slicebuilder_ndims(this%sb)
      ELSE
        get_dimensions=-1
      ENDIF
    ELSE
      get_dimensions=-99
    ENDIF
  END FUNCTION get_dimensions

  !> Get the dimension-name at a position
  !! Get the dimension at a position between 1 and get_dimensions() The position is directly related
  !! to the position in the return arrays of get_dimension_start_size()
  !! @param pos Position in the dimensions-array, 1 <= pos <= get_dimensions()
  !! @return string, or "" on error
  FUNCTION get_dimname(this, pos)
    USE iso_c_binding,    ONLY: C_NULL_CHAR,C_CHAR,C_INT,C_PTR,C_ASSOCIATED,C_F_POINTER
    IMPLICIT NONE
    CLASS(FimexIO), INTENT(IN)    :: this
    INTEGER, INTENT(IN)           :: pos
    CHARACTER(LEN=1024)           :: get_dimname
    INTEGER(KIND=C_INT)           :: c_pos
    TYPE(C_PTR)                   :: str_ptr
    INTEGER                       :: slen
    CHARACTER(LEN=1024), POINTER  :: tmp_name

    c_pos = pos - 1
    slen = 0
    IF ( C_ASSOCIATED(this%sb) .AND. pos <= c_mifi_slicebuilder_ndims(this%sb) ) THEN
      str_ptr = c_mifi_slicebuilder_dimname(this%sb, c_pos)
      call C_F_POINTER(str_ptr,tmp_name)
      slen = INDEX(tmp_name,C_NULL_CHAR) - 1
      get_dimname = tmp_name(1:slen)
    ELSE
      get_dimname = ""
    ENDIF
  END FUNCTION get_dimname

  !> Get the current start-position and current size for the current variable dimensions.
  !! Get the start-position and size for each dimension of the variable set with the last
  !! get_dimensions() call. Initially, start is 0 for each dimension and size is the full
  !! dimension size. This can be changed with reduce_dimension().
  !! @param start pre-allocated array of size get_dimensions(), returns usually 0,0,0,...
  !! @param vsize pre-allocated array of size get_dimensions(), returns the sizes of the dimension
  !! @return 0 on success, negative on error
  FUNCTION get_dimension_start_size(this, start, vsize)
    USE iso_c_binding,    ONLY: C_INT, C_ASSOCIATED, C_LOC
    INTEGER :: get_dimension_start_size
    CLASS(FimexIO), INTENT(IN)    :: this
    INTEGER(KIND=C_INT), DIMENSION(:), ALLOCATABLE, TARGET, INTENT(INOUT) :: start
    INTEGER(KIND=C_INT), DIMENSION(:), ALLOCATABLE, TARGET, INTENT(INOUT) :: vsize

    IF ( C_ASSOCIATED(this%sb) ) THEN
       get_dimension_start_size = c_mifi_slicebuilder_get_start_size(this%sb, C_LOC(start), C_LOC(vsize))
    ELSE
       get_dimension_start_size = -1
    END IF
  END FUNCTION get_dimension_start_size

  !> Get the types of the axes.
  !! The types are the same as in MetNoFimex::CoordinateAxis::AxisType. Compare against
  !! AXIS_Time, AXIS_GeoX, ... enumerated constants.
  !! @param atypes pre-allocated integer array of size ndims
  !! @return 0 on success, negative on error
  FUNCTION get_axistypes(this, atypes)
    USE iso_c_binding,    ONLY: C_INT, C_ASSOCIATED, C_LOC
    INTEGER :: get_axistypes
    CLASS(FimexIO), INTENT(IN)    :: this
    INTEGER(KIND=C_INT), DIMENSION(:), ALLOCATABLE, TARGET, INTENT(INOUT) :: atypes

    IF ( C_ASSOCIATED(this%sb) ) THEN
       get_axistypes = c_mifi_slicebuilder_get_axistype(this%sb, C_LOC(atypes))
    ELSE
       get_axistypes = -1
    END IF
  END FUNCTION get_axistypes


  !> Reduce the dimension by setting a start and size.
  !! @param dimName dimension name, e.g. retrieved by get_dimname()
  !! @param start start-position in the dimension to retrieve the data, first position is 0!
  !! @param dsize size of the dimension. This may not be too large, e.g. start+size must be < total dimension size
  !! @return 0 on success, otherwise error
  FUNCTION reduce_dimension(this, dimName, start, dsize)
    USE iso_c_binding,    ONLY: C_ASSOCIATED,C_INT,C_NULL_CHAR
    IMPLICIT NONE
    INTEGER :: reduce_dimension
    CLASS(FimexIO), INTENT(IN)                              :: this
    CHARACTER(LEN=1024), INTENT(IN)                         :: dimName
    INTEGER(KIND=C_INT), INTENT(IN)                         :: start
    INTEGER(KIND=C_INT), INTENT(IN)                         :: dsize
    INTEGER :: i
    IF ( C_ASSOCIATED(this%sb) ) THEN
      reduce_dimension = c_mifi_slicebuilder_set_dim_start_size(this%sb, &
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
  !! ierr=read_data(varName,cunit,target)
  !! ! remap to 4-d
  !! field4d(1:vsize(1),1:vsize(2),1:vsize(3),1:vsize(4)) => target
  !! @endcode
  !! @param varName the variable name to read, must be similar or equal to the one set in get_dimensions()
  !! @param cunit the unit to read the variable in. Use "" to ignore units.
  !! @param field the preallocated multi-dimensional field
  !! @return 0 on success
  FUNCTION read_data(this, varname, cunit, field)
    USE iso_c_binding,    ONLY: C_PTR,C_NULL_CHAR,C_DOUBLE,C_ASSOCIATED,C_INT,C_LONG_LONG,C_LOC
    IMPLICIT NONE
    CLASS(FimexIO), INTENT(IN)                              :: this
    CHARACTER(LEN=*), INTENT(IN)                            :: varname
    CHARACTER(LEN=*), INTENT(IN)                            :: cunit
    REAL(KIND=C_DOUBLE),DIMENSION(:), INTENT(INOUT), ALLOCATABLE, TARGET :: field
    INTEGER                                                 :: read_data

    INTEGER(KIND=C_INT), DIMENSION(:), ALLOCATABLE, TARGET  :: start
    INTEGER(KIND=C_INT), DIMENSION(:), ALLOCATABLE, TARGET  :: vsize
    CHARACTER(LEN=1024)                                     :: dimName
    INTEGER(KIND=C_LONG_LONG)                               :: expSize, outSize
    INTEGER :: i,ierr, ndims

    IF (C_ASSOCIATED(this%sb) .AND. C_ASSOCIATED(this%io)) THEN
      ndims = c_mifi_slicebuilder_ndims(this%sb)
      ALLOCATE(start(ndims))
      ALLOCATE(vsize(ndims))
      ierr = this%get_dimension_start_size(start, vsize)
      expSize = PRODUCT(vsize)
      IF (expSize /= size(field, KIND=C_LONG_LONG)) THEN
        read_data = -1
        WRITE(*,*) "read_data, allocated field-size != expected size: ", size(field), "!=", expSize
        RETURN
      END IF

      read_data = c_mifi_fill_scaled_double_dataslice(this%io, trim(varName)//C_NULL_CHAR, this%sb, &
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

  !> Write data to a file
  !! @param varName the variable name to write, must be similar or equal to the one set in get_dimensions()
  !! @param cunit the unit the variable is currently in. Use "" to ignore units.
  !! @param field the data, which must be 1d-allocatable
  !! @return 0 on success
  FUNCTION write_data(this, varname, cunit, field)
    USE iso_c_binding,    ONLY: C_PTR,C_NULL_CHAR,C_DOUBLE,C_ASSOCIATED,C_INT,C_LONG_LONG,C_LOC
    IMPLICIT NONE
    CLASS(FimexIO), INTENT(IN)                              :: this
    CHARACTER(LEN=*), INTENT(IN)                            :: varname
    CHARACTER(LEN=*), INTENT(IN)                            :: cunit
    REAL(KIND=C_DOUBLE),DIMENSION(:), INTENT(INOUT), ALLOCATABLE, TARGET :: field
    INTEGER                                                 :: write_data

    INTEGER(KIND=C_INT), DIMENSION(:), ALLOCATABLE, TARGET  :: start
    INTEGER(KIND=C_INT), DIMENSION(:), ALLOCATABLE, TARGET  :: vsize
    CHARACTER(LEN=1024)                                     :: dimName
    INTEGER(KIND=C_LONG_LONG)                               :: expSize, outSize
    INTEGER :: i,ierr, ndims

    IF (C_ASSOCIATED(this%sb) .AND. C_ASSOCIATED(this%io)) THEN
      ndims = c_mifi_slicebuilder_ndims(this%sb)
      ALLOCATE(start(ndims))
      ALLOCATE(vsize(ndims))
      ierr = this%get_dimension_start_size(start, vsize)
      expSize = PRODUCT(vsize)
      !write(*,*) "output-size: ", expSize
      IF (expSize /= size(field, KIND=C_LONG_LONG)) THEN
        write_data = -1
        WRITE(*,*) "write_data, allocated field-size != expected size: ", size(field), "!=", expSize
        RETURN
      END IF

      write_data = c_mifi_write_scaled_double_dataslice(this%io, trim(varName)//C_NULL_CHAR, this%sb, &
                                                 trim(cunit)//C_NULL_CHAR, C_LOC(field), expSize)
      IF (write_data /= 0) THEN
        WRITE(*,*) "error filling scaled_double_dataslice ", write_data
        RETURN
      END IF
    ELSE
      write_data = -99
      WRITE(*,*) "write_data, io or sb not initialized"
    END IF
    RETURN
  END FUNCTION write_data


  !> Cleanup internally kept handles and close the file.
  FUNCTION close_file(this)
    USE iso_c_binding,   ONLY: C_ASSOCIATED
    IMPLICIT NONE
    CLASS(FimexIO), INTENT(IN)    :: this
    INTEGER                       :: close_file

    IF ( C_ASSOCIATED(this%sb) ) CALL c_mifi_free_slicebuilder(this%sb)
    IF ( C_ASSOCIATED(this%io) ) CALL c_mifi_free_cdm_reader(this%io)
    close_file=0
  END FUNCTION close_file

END MODULE Fimex
