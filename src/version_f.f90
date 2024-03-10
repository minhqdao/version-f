!> The main and only module of `version-f` containing all the types and
!> procedures that are necessary to create, parse, compare, convert and
!> manipulate version numbers.
module version_f
  implicit none
  private

  public :: version_t, string_t, error_t, version_range_t, &
            comparator_set_t, comparator_t, operator_index, try_satisfy

  type :: string_t
    character(:), allocatable :: str
  contains
    generic :: num => string_t_2i
    procedure, private :: string_t_2i
    generic :: is_numeric => string_t_is_numeric
    procedure, private :: string_t_is_numeric
  end type

  interface string_t
    module procedure :: create_string_t
  end interface

  !> Contains all version information.
  type :: version_t
    !> The major version number. Incremented when breaking changes are made.
    integer :: major
    !> The minor version number. It is incremented when new functionality is
    !> added in a backwards-compatible manner.
    integer :: minor
    !> The patch version number. Incremented for backwards-compatible bug fixes.
    integer :: patch
    !> Pre-release version identifiers that are used for comparisons.
    type(string_t), allocatable :: prerelease(:)
    !> Build metadata that does not contribute to sorting.
    type(string_t), allocatable :: build(:)

  contains

    generic :: parse => try_parse
    procedure, private :: try_parse
  end type

  interface version_t
    module procedure parse
  end interface

  type :: error_t
    character(:), allocatable :: msg
  end type

  interface error_t
    module procedure :: create_error_t
  end interface

  type :: comparator_t
    character(:), allocatable :: op
    type(version_t) :: version
  contains
    procedure, private :: parse_comp_and_crop_str
  end type

  interface comparator_t
    module procedure :: create_comp
  end interface

  type :: comparator_set_t
    type(comparator_t), allocatable :: comps(:)
  contains
    generic :: parse => parse_comp_set
    procedure, private :: parse_comp_set
  end type

  interface comparator_set_t
    module procedure :: create_comp_set
  end interface

  type :: version_range_t
    type(comparator_set_t), allocatable :: comp_sets(:)
  contains
    procedure, private :: parse_version_range
  end type

contains

  !> Parse a string into a version including prerelease and build data.
  !>
  !> Wrapper function for `try_parse`.
  !>
  !> Can be invoked by calling the default constructor.
  !>
  !> In strict mode, all major, minor and patch versions must be provided. Implicit
  !> zeros are forbidden in strict mode.
  function parse(str, strict_mode) result(version)
    character(*), intent(in) :: str
    logical, optional, intent(in) :: strict_mode
    type(version_t) :: version

    type(error_t), allocatable :: error

    call version%parse(str, error, strict_mode)
    if (allocated(error)) error stop error%msg
  end

  !> Attempt to parse a string into a version including prerelease and build
  !> data. In strict mode, all major, minor and patch versions must be provided.
  !> Implicit zeros are forbidden in strict mode.
  subroutine try_parse(this, string, error, strict_mode)
    class(version_t), intent(out) :: this
    character(*), intent(in) :: string
    type(error_t), allocatable, intent(out) :: error
    logical, optional, intent(in) :: strict_mode

    integer :: i, j
    character(:), allocatable :: str

    str = trim(adjustl(string))

    i = index(str, '-')
    j = index(str, '+')

    if (i == 0 .and. j == 0) then
      call build_mmp(this, str, error, strict_mode); return
    else if (i /= 0 .and. j == 0) then
      call build_mmp(this, str(1:i - 1), error, strict_mode)
      if (allocated(error)) return
      call build_identifiers(this%prerelease, str(i + 1:len_trim(str)), error); return
    else if ((i == 0 .and. j /= 0) .or. ((i /= 0 .and. j /= 0) .and. (i > j))) then
      call build_mmp(this, str(1:j - 1), error, strict_mode)
      if (allocated(error)) return
      call build_identifiers(this%build, str(j + 1:len_trim(str)), error); return
    else if (i /= 0 .and. j /= 0) then
      call build_mmp(this, str(1:i - 1), error, strict_mode)
      if (allocated(error)) return
      call build_identifiers(this%prerelease, str(i + 1:j - 1), error)
      if (allocated(error)) return
      call build_identifiers(this%build, str(j + 1:len_trim(str)), error); return
    end if
  end

  !> Build the `major.minor.patch` part of the version. In strict mode, all
  !> major, minor and patch versions must be provided. Implicit zeros are
  !> forbidden in strict mode.
  subroutine build_mmp(this, str, error, strict_mode)
    type(version_t), intent(out) :: this
    character(*), intent(in) :: str
    type(error_t), allocatable, intent(out) :: error
    logical, optional, intent(in) :: strict_mode

    integer :: i, j, l
    logical :: is_strict_mode

    if (present(strict_mode)) then
      is_strict_mode = strict_mode
    else
      is_strict_mode = .false.
    end if

    this%major = 0
    this%minor = 0
    this%patch = 0

    i = index(str, '.')
    l = len_trim(str)

    if (l == 0) then
      error = error_t('Version must not be empty.'); return
    end if

    if (i == 0) then
      if (is_strict_mode) then
        error = error_t('Strict mode: No minor and patch versions provided.'); return
      end if
      call s2int(str, this%major, error)
      if (allocated(error)) return
    else
      if (is_strict_mode .and. i == 1) then
        error = error_t('Strict mode: Major version has to be number.'); return
      end if
      call s2int(str(1:i - 1), this%major, error)
      if (allocated(error)) return
      j = index(str(i + 1:l), '.')
      if (j == 0) then
        if (is_strict_mode) then
          error = error_t('Strict mode: No patch version provided.'); return
        end if
        call s2int(str(i + 1:l), this%minor, error)
        if (allocated(error)) return
      else
        if (is_strict_mode .and. j == 1) then
          error = error_t('Strict mode: Minor version has to be number.'); return
        end if
        call s2int(str(i + 1:i + j - 1), this%minor, error)
        if (allocated(error)) return
        if (is_strict_mode .and. len(str) == i + j) then
          error = error_t('Strict mode: Patch version has to be number.'); return
        end if
        call s2int(str(i + j + 1:l), this%patch, error)
        if (allocated(error)) return
      end if
    end if
  end

  !> Convert a string to an integer.
  pure subroutine s2int(str, num, error)
    character(*), intent(in) :: str
    integer, intent(out) :: num
    type(error_t), allocatable, intent(out) :: error

    integer :: i
    character :: c

    num = 0
    do i = 1, len(str)
      c = str(i:i)
      if (c >= '0' .and. c <= '9') then
        num = num*10 + index('0123456789', c) - 1
      else
        error = error_t("Contains non-digit: '"//str//"'."); return
      end if
    end do
  end

  !> Wrapper function for `s2int`.
  elemental integer function s2i(str)
    character(*), intent(in) :: str

    type(error_t), allocatable :: e

    call s2int(str, s2i, e)
    if (allocated(e)) error stop e%msg
  end

  !> Convert a `string_t` to an integer.
  elemental integer function string_t_2i(this)
    class(string_t), intent(in) :: this

    type(error_t), allocatable :: e

    call s2int(this%str, string_t_2i, e)
    if (allocated(e)) error stop e%msg
  end

  !> Convert an integer to a string.
  pure function int2s(num) result(str)
    integer, intent(in) :: num
    character(:), allocatable :: str

    integer :: digits, tmp

    tmp = num
    digits = 0

    do
      digits = digits + 1
      tmp = tmp/10
      if (tmp == 0) exit
    end do

    allocate (character(digits) :: str)
    write (str, '(I0)') num
  end

  !> Check for valid prerelease or build data and build identfiers from
  !> the string.
  pure subroutine build_identifiers(ids, str, error)
    type(string_t), allocatable, intent(out) :: ids(:)
    character(*), intent(in) :: str
    type(error_t), allocatable, intent(out) :: error

    character(*), parameter :: valid_chars = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWYYZ-.'
    character(:), allocatable :: string
    integer :: i

    if (len_trim(str) == 0) then
      error = error_t('Identifier must not be empty.'); return
    end if

    do i = 1, len(str)
      if (index(valid_chars, str(i:i)) == 0) then
        error = error_t("Invalid character in '"//str//"'."); return
      end if
    end do

    ! Last character must not be a dot.
    if (str(len(str):len(str)) == '.') then
      error = error_t('Identifier must not end with a dot.'); return
    end if

    string = str
    allocate (ids(0))

    do
      i = index(string, '.')

      ! No dots (left), record last identifier and return.
      if (i == 0) then
        call validate_identifier(string, error)
        if (allocated(error)) return
        ids = [ids, string_t(string)]; return
      end if

      ! Validate and record identifier, then shorten string.
      call validate_identifier(string(1:i - 1), error)
      if (allocated(error)) return
      ids = [ids, string_t(string(1:i - 1))]
      string = string(i + 1:len(string))
    end do
  end

  !> Validate an identifier.
  pure subroutine validate_identifier(str, error)
    character(*), intent(in) :: str
    type(error_t), allocatable, intent(out) :: error

    ! Empty identifiers are not allowed.
    if (len_trim(str) == 0) then
      error = error_t('Identifier must not be empty.'); return
    end if

    ! Identifiers must not start with `.`.
    if (str(1:1) == '.') then
      error = error_t("Identifiers must not start with '.'"); return
    end if

    ! Numerical identifiers must not start with 0.
    if (is_numerical(str) .and. str(1:1) == '0') then
      error = error_t("Numerical identifiers must not start with '0'."); return
    end if
  end

  !> Check if the string is purely numerical.
  elemental function is_numerical(str)
    character(*), intent(in) :: str
    logical :: is_numerical

    is_numerical = verify(str, '0123456789') == 0
  end

  !> Check if string_t is purely numeric.
  elemental function string_t_is_numeric(this)
    class(string_t), intent(in) :: this
    logical :: string_t_is_numeric

    string_t_is_numeric = verify(this%str, '0123456789') == 0
  end

  !> Helper function to generate a new `string_t` instance.
  elemental function create_string_t(inp_str) result(string)

    !> Input string.
    character(*), intent(in) :: inp_str

    !> The string instance.
    type(string_t) :: string

    string%str = inp_str
  end

  !> Helper function to generate a new `error_t` instance.
  elemental function create_error_t(msg) result(err)

    !> Error message.
    character(*), intent(in) :: msg

    !> The error instance.
    type(error_t) :: err

    err%msg = msg
  end

  !> Determine whether the version meets the comparison expressed in `str`.
  !>
  !> Valid operators are `>`, `>=`, `<`, `<=`, `=` and `!=`.
  !>
  !> Example:
  !>
  !> program main
  !>   use version_f
  !>   implicit none
  !>
  !>   type(version_t) :: version
  !>   character(*), parameter :: requirement = '>=1.2.3'
  !>   logical :: is_satisfied
  !>   type(error_t), allocatable :: error
  !>
  !>   version = version_t(1, 2, 3)
  !>   call version%try_satisfy(requirement, is_satisfied, error)
  !>   if (allocated(error)) return
  !>
  !>   if (is_satisfied) then
  !>     print *, "Version '", version%to_string(), "' meets the requirement '", requirement, "'."
  !>   else
  !>     print *, "Version '", version%to_string(), "' does not meet the requirement '", requirement, "'."
  !>   end if
  !> end
  subroutine try_satisfy(string)
    character(*), intent(in) :: string
    type(version_range_t) :: version_range

    call version_range%parse_version_range(string)

    if (version_range%comp_sets(1)%comps(1)%op /= '>') then
      print *, 'Operator not >: ', version_range%comp_sets(1)%comps(1)%op; stop 1
    end if
  end

  subroutine parse_version_range(this, string)

    !> Sets of comparators to be determined. They are separated by `||` if there
    !> are multiple sets.
    class(version_range_t), intent(out) :: this

    !> Input string to be evaluated.
    character(*), intent(in) :: string
    
    type(comparator_set_t) :: comp_set
    type(error_t), allocatable :: error

    allocate (this%comp_sets(0))

    call comp_set%parse_comp_set(string, error)
    if (allocated(error)) return

    this%comp_sets = [this%comp_sets, comp_set]
  end

  !> Parse a set of comparators that are separated by ` ` from a string. An
  !> example of a set of two comparators is `>=1.2.3 <2.0.0`.
  subroutine parse_comp_set(this, string, error)

    !> Set of comparators to be determined. They are separated by ` ` if there
    !> are multiple comparators.
    class(comparator_set_t), intent(out) :: this

    !> Input string to be evaluated.
    character(*), intent(in) :: string

    !> Error handling.
    type(error_t), allocatable, intent(out) :: error

    character(:), allocatable :: str
    type(comparator_t) :: comp

    str = trim(adjustl(string))

    if (len(str) == 0) then
      error = error_t('Comparator set cannot be empty.'); return
    end if

    allocate (this%comps(0))

    do
      if (len(str) == 0) then
        call comp%parse_comp_and_crop_str('', str, error)
      else if (str(1:1) == '>') then
        if (len(str) == 1) then
          call comp%parse_comp_and_crop_str('>', str, error)
        else if (str(2:2) == '=') then
          call comp%parse_comp_and_crop_str('>=', str, error)
        else
          call comp%parse_comp_and_crop_str('>', str, error)
        end if
      else if (str(1:1) == '<') then
        if (len(str) == 1) then
          call comp%parse_comp_and_crop_str('<', str, error)
        else if (str(2:2) == '=') then
          call comp%parse_comp_and_crop_str('<=', str, error)
        else
          call comp%parse_comp_and_crop_str('<', str, error)
        end if
      else if (str(1:1) == '=') then
        call comp%parse_comp_and_crop_str('=', str, error)
      else if (len(str) == 1) then
        call comp%parse_comp_and_crop_str('', str, error)
      else if (str(1:2) == '!=') then
        call comp%parse_comp_and_crop_str('!=', str, error)
      else
        call comp%parse_comp_and_crop_str('', str, error)
      end if

      if (allocated(error)) return
      this%comps = [this%comps, comp]
      if (str == '') return
      str = trim(adjustl(str))
    end do
  end

  !> Create a comparator from a string. A comparator consists of an operator and
  !> a version. An example of a comparator is `>=1.2.3`.
  subroutine parse_comp_and_crop_str(comp, op, str, error)

    !> Comparator to be determined.
    class(comparator_t), intent(out) :: comp

    !> The operator of the comparator.
    character(*), intent(in) :: op

    !> Input string to be evaluated.
    character(*), intent(inout) :: str

    !> Error handling.
    type(error_t), allocatable, intent(out) :: error

    integer :: i

    comp%op = op
    str = trim(adjustl(str(len(op) + 1:)))

    i = operator_index(str)
    if (i == 0) then
      call comp%version%parse(str, error)
      str = ''
    else
      call comp%version%parse(str(:i - 1), error)
      str = str(i:)
    end if
    if (allocated(error)) return
  end

  !> Index of the first operator (`>`, `<`, `!`, `=` or ` `) within a string.
  elemental integer function operator_index(str)

    !> Input string to be evaluated.
    character(*), intent(in) :: str

    integer :: i
    character :: char

    do i = 1, len(str)
      char = str(i:i)
      if (char == '>' .or. char == '<' .or. char == '!' .or. char == '=' .or. char == ' ') then
        operator_index = i; return
      end if
    end do

    operator_index = 0
  end

  !> Create instance of `comparator_t` using an operator (`op`) and a version.
  elemental function create_comp(op, version) result(comparator)

    !> The operator of the comparator.
    character(*), intent(in) :: op

    !> The version of the comparator.
    type(version_t), intent(in) :: version

    !> Instance of `comparator_t` created from `op` and `version`.
    type(comparator_t) :: comparator

    comparator%op = op
    comparator%version = version
  end

  !> Create instance of `comparator_set_t` using an array of comparators.
  pure function create_comp_set(comps) result(comp_set)

    !> Array of comparators to create the set from.
    type(comparator_t), intent(in) :: comps(:)

    !> Instance of `comparator_set_t` created from `comps`.
    type(comparator_set_t) :: comp_set

    comp_set%comps = comps
  end
end
