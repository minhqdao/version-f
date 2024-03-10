module version_f
  implicit none
  private

  public :: try_satisfy

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

  type :: version_t
    integer :: major
    integer :: minor
    integer :: patch
    type(string_t), allocatable :: prerelease(:)
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
  end type

  type :: comparator_set_t
    type(comparator_t), allocatable :: comps(:)
  contains
    procedure, private :: parse_comp_set
  end type

  type :: version_range_t
    type(comparator_set_t), allocatable :: comp_sets(:)
  contains
    procedure, private :: parse_version_range
  end type

contains

  function parse(str, strict_mode) result(version)
    character(*), intent(in) :: str
    logical, optional, intent(in) :: strict_mode
    type(version_t) :: version

    type(error_t), allocatable :: error

    call version%parse(str, error, strict_mode)
    if (allocated(error)) error stop error%msg
  end

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

  elemental integer function s2i(str)
    character(*), intent(in) :: str

    type(error_t), allocatable :: e

    call s2int(str, s2i, e)
    if (allocated(e)) error stop e%msg
  end

  elemental integer function string_t_2i(this)
    class(string_t), intent(in) :: this

    type(error_t), allocatable :: e

    call s2int(this%str, string_t_2i, e)
    if (allocated(e)) error stop e%msg
  end

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

  elemental function is_numerical(str)
    character(*), intent(in) :: str
    logical :: is_numerical

    is_numerical = verify(str, '0123456789') == 0
  end

  elemental function string_t_is_numeric(this)
    class(string_t), intent(in) :: this
    logical :: string_t_is_numeric

    string_t_is_numeric = verify(this%str, '0123456789') == 0
  end

  elemental function create_string_t(inp_str) result(string)
    character(*), intent(in) :: inp_str

    type(string_t) :: string

    string%str = inp_str
  end

  elemental function create_error_t(msg) result(err)
    character(*), intent(in) :: msg

    type(error_t) :: err

    err%msg = msg
  end

  subroutine try_satisfy()
    type(version_range_t) :: version_range

    call version_range%parse_version_range()

    if (version_range%comp_sets(1)%comps(1)%op /= '>') then
      print *, 'Operator not >: ', version_range%comp_sets(1)%comps(1)%op; stop 1
    end if
  end

  subroutine parse_version_range(this)
    class(version_range_t), intent(out) :: this

    type(comparator_set_t) :: comp_set

    allocate (this%comp_sets(0))

    call comp_set%parse_comp_set()

    this%comp_sets = [this%comp_sets, comp_set]
  end

  subroutine parse_comp_set(this)
    class(comparator_set_t), intent(out) :: this

    type(comparator_t) :: comp

    allocate (this%comps(0))

    comp%op = '>'
    comp%version%major = 1
    comp%version%minor = 0
    comp%version%patch = 1
    this%comps = [this%comps, comp]

    comp%op = '<'
    comp%version%major = 2
    comp%version%minor = 1
    comp%version%patch = 0
    this%comps = [this%comps, comp]
  end
end
