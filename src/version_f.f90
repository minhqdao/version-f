!> The main and only module of `version-f` containing all the types and
!> procedures that are necessary to create, parse, compare, convert and
!> manipulate version numbers.
module version_f
  implicit none
  private

  public :: version_t, string_t, error_t, is_version, version_range_t, &
            comparator_set_t, comparator_t

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
    private
    !> The major version number. Incremented when breaking changes are made.
    integer :: major_ = 0
    !> The minor version number. It is incremented when new functionality is
    !> added in a backwards-compatible manner.
    integer :: minor_ = 0
    !> The patch version number. Incremented for backwards-compatible bug fixes.
    integer :: patch_ = 0
    !> Pre-release version identifiers that are used for comparisons.
    type(string_t), allocatable :: prerelease_(:)
    !> Build metadata that does not contribute to sorting.
    type(string_t), allocatable :: build_(:)

  contains

    procedure :: to_string, major, minor, patch, prerelease, build, &
    & increment_major, increment_minor, increment_patch, &
    & increment_prerelease, increment_build, try_increment_major, &
    & try_increment_minor, try_increment_patch, try_increment_prerelease, &
    & try_increment_build, is_exactly, satisfies, try_satisfy, &
    & satisfies_comp_set, satisfies_comp, is_stable

    generic :: create => try_create
    procedure, private :: try_create

    generic :: parse => try_parse
    procedure, private :: try_parse

    generic :: operator(==) => equals
    procedure, private :: equals

    generic :: operator(/=) => not_equals
    procedure, private :: not_equals

    generic :: operator(>) => greater_than
    procedure, private :: greater_than

    generic :: operator(<) => less_than
    procedure, private :: less_than

    generic :: operator(>=) => greater_equals
    procedure, private :: greater_equals

    generic :: operator(<=) => less_equals
    procedure, private :: less_equals
  end type

  interface version_t
    module procedure create, parse
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
    generic :: extend_with => extend_comps
    procedure, private :: extend_comps
  end type

  interface comparator_set_t
    module procedure :: create_comp_set
  end interface

  type :: version_range_t
    type(comparator_set_t), allocatable :: comp_sets(:)
  contains
    generic :: parse => parse_version_range
    procedure, private :: parse_version_range
    generic :: extend_with => extend_comp_sets
    procedure, private :: extend_comp_sets
  end type

contains

  !> Wrapper function for `try_create`.
  !>
  !> Can be invoked by calling the default constructor.
  !>
  !> In strict mode, all major, minor and patch versions must be provided.
  function create(major, minor, patch, prerelease, build, strict_mode) result(this)
    integer, intent(in) :: major
    integer, optional, intent(in) :: minor
    integer, optional, intent(in) :: patch
    character(*), optional, intent(in) :: prerelease
    character(*), optional, intent(in) :: build
    logical, optional, intent(in) :: strict_mode
    type(version_t) :: this

    type(error_t), allocatable :: error

    call try_create(this, major, minor, patch, prerelease, build, error, strict_mode)
    if (allocated(error)) error stop error%msg
  end

  !> Create a version from individual major, minor, patch, prerelease and build
  !> arguments.
  !>
  !> Version numbers must be positive integers.
  !>
  !> Prelease and build versions are entered through a series of dot-separated
  !> identifiers. The identifiers must be composed of ASCII letters, digits or
  !> hyphens. They must not be empty and must not begin or end with
  !> with a dot. Multi-digit numerical prerelease identifiers must not start
  !> with a zero. Build identifiers may contain leading zeroes.
  !>
  !> Valid examples:
  !>
  !> ```fortran
  !> type(version_t) :: v
  !> type(error_t), allocatable :: err
  !>
  !> call v%create(0, 1, 0, error=err) ! 0.1.0
  !> call v%create(1, error=err) ! 1.0.0
  !> call v%create(1, 2, 3, 'alpha.1', 'build.1', err) ! 1.2.3-alpha.1+build.1
  !> ```
  !>
  !> Invalid examples:
  !>
  !> ```fortran
  !> type(version_t) :: v
  !> type(error_t), allocatable :: err
  !>
  !> call v%create(0, -1, 0, error=err) ! allocated(err) == .true.
  !> call v%create(1, build='0.0', error=err) ! allocated(err) == .true.
  !> call v%create(1, prerelease='.hi.', error=err) ! allocated(err) == .true.
  !> ```
  !>
  !> The default way is to create a version using the constructor.
  !>
  !> Use this procedure if you want to handle errors yourself.
  !>
  !> In strict mode, all major, minor and patch versions must be provided.
  subroutine try_create(this, major, minor, patch, prerelease, build, error, strict_mode)
    class(version_t), intent(out) :: this
    integer, intent(in) :: major
    integer, optional, intent(in) :: minor
    integer, optional, intent(in) :: patch
    character(*), optional, intent(in) :: prerelease
    character(*), optional, intent(in) :: build
    type(error_t), allocatable, intent(out) :: error
    logical, optional, intent(in) :: strict_mode

    logical :: is_strict_mode

    if (present(strict_mode)) then
      is_strict_mode = strict_mode
    else
      is_strict_mode = .false.
    end if

    if (major < 0) then
      error = error_t('Version numbers must not be negative.'); return
    end if
    this%major_ = major

    if (present(minor)) then
      if (minor < 0) then
        error = error_t('Version numbers must not be negative.'); return
      end if
      this%minor_ = minor
    else
      if (is_strict_mode) then
        error = error_t('Strict mode: Minor version must be provided.'); return
      end if
      this%minor_ = 0
    end if

    if (present(patch)) then
      if (patch < 0) then
        error = error_t('Version numbers must not be negative.'); return
      end if
      this%patch_ = patch
    else
      if (is_strict_mode) then
        error = error_t('Strict mode: Patch version must be provided.'); return
      end if
      this%patch_ = 0
    end if

    if (present(prerelease)) then
      call build_identifiers(this%prerelease_, prerelease, .true., error)
      if (allocated(error)) return
    end if

    if (present(build)) then
      call build_identifiers(this%build_, build, .false., error)
      if (allocated(error)) return
    end if
  end

  !> Returns a string representation of the version including prerelease and
  !> build data. Pre-computes the total length to avoid O(n^2) concatenation.
  pure function to_string(this) result(str)
    class(version_t), intent(in) :: this
    character(:), allocatable :: str

    character(:), allocatable :: s_major, s_minor, s_patch
    integer :: n, pos, i

    s_major = int2s(this%major_)
    s_minor = int2s(this%minor_)
    s_patch = int2s(this%patch_)

    n = len_trim(s_major) + 1 + len_trim(s_minor) + 1 + len_trim(s_patch)

    if (allocated(this%prerelease_)) then
      n = n + 1
      do i = 1, size(this%prerelease_)
        n = n + len(this%prerelease_(i)%str)
        if (i < size(this%prerelease_)) n = n + 1
      end do
    end if

    if (allocated(this%build_)) then
      n = n + 1
      do i = 1, size(this%build_)
        n = n + len(this%build_(i)%str)
        if (i < size(this%build_)) n = n + 1
      end do
    end if

    allocate (character(n) :: str)

    pos = 1
    str(pos:pos + len_trim(s_major) - 1) = trim(s_major)
    pos = pos + len_trim(s_major)
    str(pos:pos) = '.'
    pos = pos + 1
    str(pos:pos + len_trim(s_minor) - 1) = trim(s_minor)
    pos = pos + len_trim(s_minor)
    str(pos:pos) = '.'
    pos = pos + 1
    str(pos:pos + len_trim(s_patch) - 1) = trim(s_patch)
    pos = pos + len_trim(s_patch)

    if (allocated(this%prerelease_)) then
      str(pos:pos) = '-'
      pos = pos + 1
      do i = 1, size(this%prerelease_)
        str(pos:pos + len(this%prerelease_(i)%str) - 1) = this%prerelease_(i)%str
        pos = pos + len(this%prerelease_(i)%str)
        if (i < size(this%prerelease_)) then
          str(pos:pos) = '.'
          pos = pos + 1
        end if
      end do
    end if

    if (allocated(this%build_)) then
      str(pos:pos) = '+'
      pos = pos + 1
      do i = 1, size(this%build_)
        str(pos:pos + len(this%build_(i)%str) - 1) = this%build_(i)%str
        pos = pos + len(this%build_(i)%str)
        if (i < size(this%build_)) then
          str(pos:pos) = '.'
          pos = pos + 1
        end if
      end do
    end if
  end

  !> Return the major version number.
  elemental integer function major(this)
    class(version_t), intent(in) :: this

    major = this%major_
  end

  !> Return the minor version number.
  elemental integer function minor(this)
    class(version_t), intent(in) :: this

    minor = this%minor_
  end

  !> Return the patch version number.
  elemental integer function patch(this)
    class(version_t), intent(in) :: this

    patch = this%patch_
  end

  !> Return a copy of the prerelease identifiers.
  pure function prerelease(this) result(identifiers)
    class(version_t), intent(in) :: this
    type(string_t), allocatable :: identifiers(:)

    if (allocated(this%prerelease_)) then
      identifiers = this%prerelease_
    else
      allocate (identifiers(0))
    end if
  end

  !> Return a copy of the build identifiers.
  pure function build(this) result(identifiers)
    class(version_t), intent(in) :: this
    type(string_t), allocatable :: identifiers(:)

    if (allocated(this%build_)) then
      identifiers = this%build_
    else
      allocate (identifiers(0))
    end if
  end

  !> Increments the major version number and resets the minor and patch number
  !> as well as the prerelease and build data. Reports an error and leaves the
  !> version unchanged if the major number cannot be incremented without
  !> overflowing.
  elemental subroutine increment_major(this)
    class(version_t), intent(inout) :: this

    type(error_t), allocatable :: error

    call this%try_increment_major(error)
    if (allocated(error)) error stop error%msg
  end

  !> Attempt to increment the major version, reporting overflow through `error`.
  pure subroutine try_increment_major(this, error)
    class(version_t), intent(inout) :: this
    type(error_t), allocatable, intent(out) :: error

    if (this%major_ == huge(this%major_)) then
      error = error_t('Major version cannot be incremented without overflowing.'); return
    end if
    this%major_ = this%major_ + 1
    this%minor_ = 0
    this%patch_ = 0
    if (allocated(this%prerelease_)) deallocate (this%prerelease_)
    if (allocated(this%build_)) deallocate (this%build_)
  end

  !> Increments the minor version number and resets patch, prerelease and build.
  !> Reports an error and leaves the version unchanged if the minor number would
  !> overflow.
  elemental subroutine increment_minor(this)
    class(version_t), intent(inout) :: this

    type(error_t), allocatable :: error

    call this%try_increment_minor(error)
    if (allocated(error)) error stop error%msg
  end

  !> Attempt to increment the minor version, reporting overflow through `error`.
  pure subroutine try_increment_minor(this, error)
    class(version_t), intent(inout) :: this
    type(error_t), allocatable, intent(out) :: error

    if (this%minor_ == huge(this%minor_)) then
      error = error_t('Minor version cannot be incremented without overflowing.'); return
    end if
    this%minor_ = this%minor_ + 1
    this%patch_ = 0
    if (allocated(this%prerelease_)) deallocate (this%prerelease_)
    if (allocated(this%build_)) deallocate (this%build_)
  end

  !> Increments the patch version number and resets prerelease and build. Reports
  !> an error and leaves the version unchanged if the patch number would overflow.
  elemental subroutine increment_patch(this)
    class(version_t), intent(inout) :: this

    type(error_t), allocatable :: error

    call this%try_increment_patch(error)
    if (allocated(error)) error stop error%msg
  end

  !> Attempt to increment the patch version, reporting overflow through `error`.
  pure subroutine try_increment_patch(this, error)
    class(version_t), intent(inout) :: this
    type(error_t), allocatable, intent(out) :: error

    if (this%patch_ == huge(this%patch_)) then
      error = error_t('Patch version cannot be incremented without overflowing.'); return
    end if
    this%patch_ = this%patch_ + 1
    if (allocated(this%prerelease_)) deallocate (this%prerelease_)
    if (allocated(this%build_)) deallocate (this%build_)
  end

  !> Increment prerelease and reset build data. Reports an error and leaves the
  !> version unchanged if the final numeric prerelease identifier would overflow.
  elemental subroutine increment_prerelease(this)
    class(version_t), intent(inout) :: this

    type(error_t), allocatable :: error

    call this%try_increment_prerelease(error)
    if (allocated(error)) error stop error%msg
  end

  !> Attempt to increment prerelease data, reporting overflow through `error`.
  pure subroutine try_increment_prerelease(this, error)
    class(version_t), intent(inout) :: this
    type(error_t), allocatable, intent(out) :: error

    logical :: incremented

    call increment_identifier(this%prerelease_, incremented)
    if (.not. incremented) then
      error = error_t('Prerelease identifier cannot be incremented without overflowing.'); return
    end if
    if (allocated(this%build_)) deallocate (this%build_)
  end

  !> Increment build metadata. Reports an error and leaves the version unchanged
  !> if the final numeric build identifier would overflow.
  elemental subroutine increment_build(this)
    class(version_t), intent(inout) :: this

    type(error_t), allocatable :: error

    call this%try_increment_build(error)
    if (allocated(error)) error stop error%msg
  end

  !> Attempt to increment build metadata, reporting overflow through `error`.
  pure subroutine try_increment_build(this, error)
    class(version_t), intent(inout) :: this
    type(error_t), allocatable, intent(out) :: error

    logical :: incremented

    call increment_identifier(this%build_, incremented)
    if (.not. incremented) then
      error = error_t('Build identifier cannot be incremented without overflowing.')
    end if
  end

  !> Increment prerelease or build identifiers. If the last identifier is
  !> numeric, increment it by 1. Otherwise add a new identifier with the value
  !> 1.
  pure subroutine increment_identifier(ids, incremented)
    type(string_t), allocatable, intent(inout) :: ids(:)
    logical, intent(out) :: incremented

    type(string_t), allocatable :: tmp(:)
    integer :: n, val
    type(error_t), allocatable :: e

    incremented = .true.

    if (allocated(ids)) then
      n = size(ids)
      if (ids(n)%is_numeric()) then
        call s2int(ids(n)%str, val, e)
        if (allocated(e)) then
          incremented = .false.; return
        else if (val == huge(val)) then
          incremented = .false.; return
        else
          allocate (tmp(n))
          tmp(1:n - 1) = ids(1:n - 1)
          tmp(n)%str = trim(int2s(val + 1))
        end if
      else
        allocate (tmp(n + 1))
        tmp(1:n) = ids(1:n)
        tmp(n + 1)%str = '1'
      end if
      ids = tmp
    else
      allocate (ids(1))
      ids(1)%str = '1'
    end if
  end

  !> Parse a string into a version including prerelease and build data.
  !>
  !> Wrapper function for `try_parse`.
  !>
  !> Can be invoked by calling the default constructor.
  !>
  !> In strict mode, all major, minor and patch versions must be provided.
  !> Implicit zeros and leading zeroes are forbidden in strict mode.
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
  !> Implicit zeros and leading zeroes are forbidden in strict mode.
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
      call build_identifiers(this%prerelease_, str(i + 1:len_trim(str)), .true., error); return
    else if ((i == 0 .and. j /= 0) .or. ((i /= 0 .and. j /= 0) .and. (i > j))) then
      call build_mmp(this, str(1:j - 1), error, strict_mode)
      if (allocated(error)) return
      call build_identifiers(this%build_, str(j + 1:len_trim(str)), .false., error); return
    else if (i /= 0 .and. j /= 0) then
      call build_mmp(this, str(1:i - 1), error, strict_mode)
      if (allocated(error)) return
      call build_identifiers(this%prerelease_, str(i + 1:j - 1), .true., error)
      if (allocated(error)) return
      call build_identifiers(this%build_, str(j + 1:len_trim(str)), .false., error); return
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

    this%major_ = 0
    this%minor_ = 0
    this%patch_ = 0

    i = index(str, '.')
    l = len_trim(str)

    if (l == 0) then
      error = error_t('Version must not be empty.'); return
    end if

    if (i == 0) then
      if (is_strict_mode) then
        error = error_t('Strict mode: No minor and patch versions provided.'); return
      end if
      call s2int(str, this%major_, error)
      if (allocated(error)) return
    else
      if (is_strict_mode .and. i == 1) then
        error = error_t('Strict mode: Major version must be a number.'); return
      end if
      if (is_strict_mode) then
        call validate_core_number(str(1:i - 1), error)
        if (allocated(error)) return
      end if
      call s2int(str(1:i - 1), this%major_, error)
      if (allocated(error)) return
      j = index(str(i + 1:l), '.')
      if (j == 0) then
        if (is_strict_mode) then
          error = error_t('Strict mode: No patch version provided.'); return
        end if
        call s2int(str(i + 1:l), this%minor_, error)
        if (allocated(error)) return
      else
        if (is_strict_mode .and. j == 1) then
          error = error_t('Strict mode: Minor version must be a number.'); return
        end if
        if (is_strict_mode) then
          call validate_core_number(str(i + 1:i + j - 1), error)
          if (allocated(error)) return
        end if
        call s2int(str(i + 1:i + j - 1), this%minor_, error)
        if (allocated(error)) return
        if (is_strict_mode .and. len(str) == i + j) then
          error = error_t('Strict mode: Patch version must be a number.'); return
        end if
        if (is_strict_mode) then
          call validate_core_number(str(i + j + 1:l), error)
          if (allocated(error)) return
        end if
        call s2int(str(i + j + 1:l), this%patch_, error)
        if (allocated(error)) return
      end if
    end if
  end

  !> Reject leading zeroes in a major, minor or patch number.
  pure subroutine validate_core_number(str, error)
    character(*), intent(in) :: str
    type(error_t), allocatable, intent(out) :: error

    if (len(str) > 1) then
      if (str(1:1) == '0') then
        error = error_t('Strict mode: Version numbers must not contain leading zeroes.')
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
        block
          integer :: digit
          digit = index('0123456789', c) - 1
          if (num > (huge(num) - digit)/10) then
            error = error_t("Integer overflow in: '"//str//"'."); return
          end if
          num = num*10 + digit
        end block
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

    tmp = abs(num)
    digits = 0

    do
      digits = digits + 1
      tmp = tmp/10
      if (tmp == 0) exit
    end do

    if (num < 0) digits = digits + 1

    allocate (character(digits) :: str)
    write (str, '(I0)') num
  end

  !> Validate prerelease or build identifier string without allocating. Uses an
  !> ASCII lookup table for O(1) character validation instead of O(m) scans.
  pure subroutine validate_identifiers(str, is_prerelease, error)
    character(*), intent(in) :: str
    logical, intent(in) :: is_prerelease
    type(error_t), allocatable, intent(out) :: error

    integer :: i, c, start, length
    logical :: valid(0:127)

    if (len_trim(str) == 0) then
      error = error_t('Identifier must not be empty.'); return
    end if

    ! Build lookup table for valid identifier characters.
    valid = .false.
    valid(ichar('0'):ichar('9')) = .true.
    valid(ichar('a'):ichar('z')) = .true.
    valid(ichar('A'):ichar('Z')) = .true.
    valid(ichar('-')) = .true.
    valid(ichar('.')) = .true.

    do i = 1, len(str)
      c = ichar(str(i:i))
      if (c < 0 .or. c > 127) then
        error = error_t("Invalid character in '"//str//"'."); return
      end if
      if (.not. valid(c)) then
        error = error_t("Invalid character in '"//str//"'."); return
      end if
    end do

    ! Last character must not be a dot.
    if (str(len(str):len(str)) == '.') then
      error = error_t('Identifier must not end with a dot.'); return
    end if

    ! Validate each identifier.
    start = 1
    do
      length = index(str(start:), '.')
      if (length == 0) then
        call validate_identifier(str(start:), is_prerelease, error)
        return
      else
        call validate_identifier(str(start:start + length - 2), is_prerelease, error)
        if (allocated(error)) return
        start = start + length
      end if
    end do
  end

  !> Check for valid prerelease or build data and build identifiers from
  !> the string.
  pure subroutine build_identifiers(ids, str, is_prerelease, error)
    type(string_t), allocatable, intent(out) :: ids(:)
    character(*), intent(in) :: str
    logical, intent(in) :: is_prerelease
    type(error_t), allocatable, intent(out) :: error

    integer :: i, n, start, length

    call validate_identifiers(str, is_prerelease, error)
    if (allocated(error)) return

    ! Count identifiers.
    n = 1
    do i = 1, len_trim(str)
      if (str(i:i) == '.') n = n + 1
    end do

    allocate (ids(n))

    start = 1
    do i = 1, n
      length = index(str(start:), '.')
      if (length == 0) then
        ids(i)%str = str(start:)
      else
        ids(i)%str = str(start:start + length - 2)
        start = start + length
      end if
    end do
  end

  !> Validate an identifier.
  pure subroutine validate_identifier(str, is_prerelease, error)
    character(*), intent(in) :: str
    logical, intent(in) :: is_prerelease
    type(error_t), allocatable, intent(out) :: error

    ! Empty identifiers are not allowed.
    if (len_trim(str) == 0) then
      error = error_t('Identifier must not be empty.'); return
    end if

    ! Identifiers must not start with `.`.
    if (str(1:1) == '.') then
      error = error_t("Identifiers must not start with '.'"); return
    end if

    ! Multi-digit numerical prerelease identifiers must not start with zero.
    if (is_prerelease .and. len(str) > 1) then
      if (is_numerical(str) .and. str(1:1) == '0') then
        error = error_t("Numerical prerelease identifiers must not contain leading zeroes."); return
      end if
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

  !> Check two versions for equality.
  elemental logical function equals(lhs, rhs)
    class(version_t), intent(in) :: lhs
    class(version_t), intent(in) :: rhs

    integer :: i

    equals = lhs%major_ == rhs%major_ &
    &  .and. lhs%minor_ == rhs%minor_ &
    &  .and. lhs%patch_ == rhs%patch_

    if (.not. equals) return

    if (allocated(lhs%prerelease_) .and. allocated(rhs%prerelease_)) then
      if (size(lhs%prerelease_) /= size(rhs%prerelease_)) then
        equals = .false.; return
      end if
      do i = 1, size(lhs%prerelease_)
        if (lhs%prerelease_(i)%str /= rhs%prerelease_(i)%str) then
          equals = .false.; return
        end if
      end do
    else if (allocated(lhs%prerelease_) .or. allocated(rhs%prerelease_)) then
      equals = .false.
    end if
  end

  !> Check two versions for inequality.
  elemental logical function not_equals(lhs, rhs)
    class(version_t), intent(in) :: lhs
    class(version_t), intent(in) :: rhs

    not_equals = .not. lhs == rhs
  end

  !> Check if the first version is greater than the second.
  elemental logical function greater_than(lhs, rhs)
    class(version_t), intent(in) :: lhs
    class(version_t), intent(in) :: rhs

    greater_than = lhs%major_ > rhs%major_ &
    & .or. (lhs%major_ == rhs%major_ &
    & .and. lhs%minor_ > rhs%minor_) &
    & .or. (lhs%major_ == rhs%major_ &
    & .and. lhs%minor_ == rhs%minor_ &
    & .and. lhs%patch_ > rhs%patch_)

    if (greater_than) return

    if (lhs%major_ == rhs%major_ .and. lhs%minor_ == rhs%minor_ .and. lhs%patch_ == rhs%patch_) then
      if (allocated(lhs%prerelease_) .and. .not. allocated(rhs%prerelease_)) then
        greater_than = .false.
      else if (.not. allocated(lhs%prerelease_) .and. allocated(rhs%prerelease_)) then
        greater_than = .true.
      else if (allocated(lhs%prerelease_) .and. allocated(rhs%prerelease_)) then
        greater_than = is_greater(lhs%prerelease_, rhs%prerelease_)
      end if
    end if
  end

  !> Check if the first version is smaller than the second.
  elemental logical function less_than(lhs, rhs)
    class(version_t), intent(in) :: lhs
    class(version_t), intent(in) :: rhs

    less_than = .not. lhs > rhs .and. .not. lhs == rhs
  end

  !> Check if the first version is greater than or equal to the second.
  elemental logical function greater_equals(lhs, rhs)
    class(version_t), intent(in) :: lhs
    class(version_t), intent(in) :: rhs

    greater_equals = lhs > rhs .or. lhs == rhs
  end

  !> Check if the first version is smaller than or equal to the second.
  elemental logical function less_equals(lhs, rhs)
    class(version_t), intent(in) :: lhs
    class(version_t), intent(in) :: rhs

    less_equals = .not. lhs > rhs
  end

  !> Check if the first prerelease (`lhs`) is greater than the second (`rhs`).
  pure logical function is_greater(lhs, rhs)
    type(string_t), intent(in) :: lhs(:)
    type(string_t), intent(in) :: rhs(:)

    integer :: i, j
    integer :: lhs_num, rhs_num
    type(error_t), allocatable :: e

    do i = 1, min(size(lhs), size(rhs))
      if (lhs(i)%str == rhs(i)%str) cycle
      if (lhs(i)%is_numeric() .and. rhs(i)%is_numeric()) then
        call s2int(lhs(i)%str, lhs_num, e)
        if (allocated(e)) then
          is_greater = lhs(i)%str > rhs(i)%str; return
        end if
        call s2int(rhs(i)%str, rhs_num, e)
        if (allocated(e)) then
          is_greater = lhs(i)%str > rhs(i)%str; return
        end if
        is_greater = lhs_num > rhs_num; return
      else if (lhs(i)%is_numeric()) then
        is_greater = .false.; return
      else if (rhs(i)%is_numeric()) then
        is_greater = .true.; return
      end if

      do j = 1, min(len(lhs(i)%str), len(rhs(i)%str))
        if (lhs(i)%str(j:j) == rhs(i)%str(j:j)) cycle
        is_greater = lhs(i)%str(j:j) > rhs(i)%str(j:j); return
      end do

      if (len(lhs(i)%str) /= len(rhs(i)%str)) then
        is_greater = len(lhs(i)%str) > len(rhs(i)%str); return
      end if
    end do

    is_greater = size(lhs) > size(rhs)
  end

  !> True if both versions are exactly the same including the build metadata.
  !> This procedure has been added for convenience. It is not part of the
  !> Semantic Versioning 2.0.0 specification.
  elemental logical function is_exactly(this, other)
    class(version_t), intent(in) :: this
    type(version_t), intent(in) :: other

    integer :: i

    is_exactly = this == other
    if (.not. is_exactly) return

    if (allocated(this%build_) .and. allocated(other%build_)) then
      if (size(this%build_) /= size(other%build_)) then
        is_exactly = .false.; return
      end if

      do i = 1, size(this%build_)
        if (this%build_(i)%str /= other%build_(i)%str) then
          is_exactly = .false.; return
        end if
      end do
    else if (allocated(this%build_) .or. allocated(other%build_)) then
      is_exactly = .false.; return
    end if
  end

  !> Validate a version string without allocating a `version_t`. Returns the
  !> first error encountered, or an unallocated error on success.
  subroutine validate_version_string(str, error, strict_mode)
    character(*), intent(in) :: str
    type(error_t), allocatable, intent(out) :: error
    logical, optional, intent(in) :: strict_mode

    type(version_t) :: version
    integer :: i, j
    character(:), allocatable :: trimmed

    trimmed = trim(adjustl(str))

    i = index(trimmed, '-')
    j = index(trimmed, '+')

    if (i == 0 .and. j == 0) then
      call build_mmp(version, trimmed, error, strict_mode)
    else if (i /= 0 .and. j == 0) then
      call build_mmp(version, trimmed(1:i - 1), error, strict_mode)
      if (allocated(error)) return
      call validate_identifiers(trimmed(i + 1:len_trim(trimmed)), .true., error)
    else if ((i == 0 .and. j /= 0) .or. &
            & ((i /= 0 .and. j /= 0) .and. (i > j))) then
      call build_mmp(version, trimmed(1:j - 1), error, strict_mode)
      if (allocated(error)) return
      call validate_identifiers(trimmed(j + 1:len_trim(trimmed)), .false., error)
    else if (i /= 0 .and. j /= 0) then
      call build_mmp(version, trimmed(1:i - 1), error, strict_mode)
      if (allocated(error)) return
      call validate_identifiers(trimmed(i + 1:j - 1), .true., error)
      if (allocated(error)) return
      call validate_identifiers(trimmed(j + 1:len_trim(trimmed)), .false., error)
    end if
  end

  !> True if the string can be parsed as a valid `version_t`. Use `parse` if you
  !> wish to receive detailed error messages. In strict mode, all major, minor
  !> and patch versions must be provided. Implicit zeros are forbidden in strict
  !> mode.
  logical function is_version(str, strict_mode)

    !> Input string.
    character(*), intent(in) :: str

    !> If true, all major, minor and patch versions must be provided. Implicit
    !> zeros are forbidden in strict mode.
    logical, optional, intent(in) :: strict_mode

    type(error_t), allocatable :: error

    call validate_version_string(str, error, strict_mode)
    is_version = .not. allocated(error)
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
  subroutine try_satisfy(this, string, is_satisfied, error)

    !> Version to be evaluated.
    class(version_t), intent(in) :: this

    !> Input string to be evaluated.
    character(*), intent(in) :: string

    !> Whether the version meets the comparison expressed in `str`.
    logical, intent(out) :: is_satisfied

    !> Error handling.
    type(error_t), allocatable, intent(out) :: error

    character(:), allocatable :: str
    type(version_range_t) :: version_range
    integer :: i

    str = trim(adjustl(string))

    if (len(str) == 0) then
      error = error_t('Do not compare empty expressions.'); return
    end if

    call version_range%parse(str, error)
    if (allocated(error)) return

    do i = 1, size(version_range%comp_sets)
      call this%satisfies_comp_set(version_range%comp_sets(i), is_satisfied, error)
      if (is_satisfied .or. allocated(error)) return
    end do
  end

  !> Convenience function to determine whether the version meets the comparison.
  !>
  !> Wrapper function for `try_satisfy`, which returns `.false.` if the
  !> comparison fails.
  logical function satisfies(this, str)

    !> Instance of `version_t` to be evaluated.
    class(version_t), intent(in) :: this

    !> Input string to be evaluated.
    character(*), intent(in) :: str

    type(error_t), allocatable :: error

    call this%try_satisfy(str, satisfies, error)
    if (allocated(error)) satisfies = .false.
  end

  !> Create sets of comparators that are separated by `||`. An example of a
  !> version range is `4.2.3 || 5.0.0 - 7.2.3`.
  subroutine parse_version_range(this, string, error)

    !> Sets of comparators to be determined. They are separated by `||` if there
    !> are multiple sets.
    class(version_range_t), intent(out) :: this

    !> Input string to be evaluated.
    character(*), intent(in) :: string

    !> Error handling.
    type(error_t), allocatable, intent(out) :: error

    integer :: i_sep, n_sets, idx
    character(:), allocatable :: str
    type(comparator_set_t) :: comp_set

    str = string

    ! Pre-count sets separated by ||.
    n_sets = 1
    i_sep = index(str, '||')
    do while (i_sep /= 0)
      n_sets = n_sets + 1
      str = str(i_sep + 2:)
      i_sep = index(str, '||')
    end do

    allocate (this%comp_sets(n_sets))

    ! Parse each set and assign directly.
    str = string
    idx = 0

    i_sep = index(str, '||')
    do while (i_sep /= 0)
      idx = idx + 1
      call comp_set%parse_comp_set(str(1:i_sep - 1), error)
      if (allocated(error)) return
      this%comp_sets(idx) = comp_set
      str = str(i_sep + 2:)
      i_sep = index(str, '||')
    end do

    idx = idx + 1
    call comp_set%parse_comp_set(str, error)
    if (allocated(error)) return
    this%comp_sets(idx) = comp_set
  end

  !> Extend array of comparator sets within version range with another comparator.
  subroutine extend_comp_sets(range, comp_set)
    class(version_range_t), intent(inout) :: range
    type(comparator_set_t), intent(in) :: comp_set

    type(comparator_set_t), allocatable :: tmp(:)

    allocate (tmp(size(range%comp_sets) + 1))
    tmp(1:size(range%comp_sets)) = range%comp_sets
    tmp(size(tmp)) = comp_set
    call move_alloc(tmp, range%comp_sets)
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
    integer :: n_comps, idx, i, l

    str = trim(adjustl(string))

    if (len(str) == 0) then
      error = error_t('Comparator set cannot be empty.'); return
    end if

    ! Pre-count comparators by scanning for operator/version boundaries.
    n_comps = 0
    i = 1
    l = len(str)
    do while (i <= l)
      ! Skip whitespace between comparators.
      do while (i <= l)
        if (str(i:i) /= ' ') exit
        i = i + 1
      end do
      if (i > l) exit
      n_comps = n_comps + 1
      ! Skip past the operator (if any).
      if (str(i:i) == '>' .or. str(i:i) == '<' .or. &
          & str(i:i) == '!' .or. str(i:i) == '=') then
        i = i + 1
        if (i <= l) then
          if (str(i:i) == '=') i = i + 1
        end if
      end if
      ! Skip whitespace after operator (before version).
      do while (i <= l)
        if (str(i:i) /= ' ') exit
        i = i + 1
      end do
      ! Skip past the version part until next operator or end.
      do while (i <= l)
        if (str(i:i) == ' ' .or. str(i:i) == '>' .or. &
            & str(i:i) == '<' .or. str(i:i) == '!' .or. &
            & str(i:i) == '=') exit
        i = i + 1
      end do
    end do

    allocate (this%comps(n_comps))

    ! Parse each comparator and assign directly.
    idx = 0
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
      idx = idx + 1
      this%comps(idx) = comp
      if (str == '') return
      str = trim(adjustl(str))
    end do
  end

  !> Extend array of comparators within comparator set with another comparator.
  subroutine extend_comps(set, comp)
    class(comparator_set_t), intent(inout) :: set
    type(comparator_t), intent(in) :: comp

    type(comparator_t), allocatable :: tmp(:)

    allocate (tmp(size(set%comps) + 1))
    tmp(1:size(set%comps)) = set%comps
    tmp(size(tmp)) = comp
    call move_alloc(tmp, set%comps)
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

  !> Attempt to evaluate a comparator set. A comparator set consists of multiple
  !> comparators that are separated by ` `. An example of a comparator set is
  !> `>=1.2.3 <2.0.0`. A comparator set is satisfied if all of its comparators
  !> are satisfied.
  pure subroutine satisfies_comp_set(version, comp_set, is_satisfied, error)

    !> Instance of `version_t` to be evaluated.
    class(version_t), intent(in) :: version

    !> Set of comparators to be evaluated.
    type(comparator_set_t), intent(in) :: comp_set

    !> Whether the comparator set is satisfied.
    logical, intent(out) :: is_satisfied

    !> Error handling.
    type(error_t), allocatable, intent(out) :: error

    integer :: i

    if (size(comp_set%comps) == 0) then
      error = error_t('Comparator set cannot be empty.'); return
    end if

    do i = 1, size(comp_set%comps)
      call version%satisfies_comp(comp_set%comps(i), is_satisfied, error)
      if (.not. is_satisfied .or. allocated(error)) return
    end do
  end

  !> Attempt to evaluate a comparator which consists of a comparison operator
  !> and a version string.
  pure subroutine satisfies_comp(this, comparator, is_satisfied, error)

    !> Instance of `version_t` to be evaluated.
    class(version_t), intent(in) :: this

    !> Comparator to be evaluated.
    type(comparator_t), intent(in) :: comparator

    !> Whether the version meets the comparison expressed in `comparator`.
    logical, intent(out) :: is_satisfied

    !> Error handling.
    type(error_t), allocatable, intent(out) :: error

    if (comparator%op == '>') then
      is_satisfied = this > comparator%version
    else if (comparator%op == '>=') then
      is_satisfied = this >= comparator%version
    else if (comparator%op == '<') then
      is_satisfied = this < comparator%version
    else if (comparator%op == '<=') then
      is_satisfied = this <= comparator%version
    else if (comparator%op == '!=') then
      is_satisfied = this /= comparator%version
    else if (comparator%op == '=' .or. comparator%op == '') then
      is_satisfied = this == comparator%version
    else
      is_satisfied = .false.
      error = error_t("Invalid operator: '"//comparator%op//"'.")
    end if
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

    allocate (comp_set%comps(size(comps)))
    comp_set%comps = comps
  end

  !> Returns true if the version is stable. A version is stable if its major
  !> version is greater than zero and the version is not a prerelease.
  elemental logical function is_stable(version)

    !> Instance of `version_t` to be evaluated.
    class(version_t), intent(in) :: version

    is_stable = version%major_ > 0 .and. .not. allocated(version%prerelease_)
  end
end
