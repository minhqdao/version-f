!> The main and only module of `version-f` containing all the types and
!> procedures that are necessary to create, parse, compare, convert and
!> manipulate version numbers.
module version_f
  implicit none
  private

  public :: version_t, string_t, error_t, is_version

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

    procedure :: to_string, increment_major, increment_minor, increment_patch, &
    & increment_prerelease, increment_build, is_exactly

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

  type :: string_t
    character(len=:), allocatable :: str
  contains
    generic :: num => string_t_2i
    procedure, private :: string_t_2i
    generic :: is_numeric => string_t_is_numeric
    procedure, private :: string_t_is_numeric
  end type

  type :: error_t
    character(len=:), allocatable :: msg
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
    character(len=*), optional, intent(in) :: prerelease
    character(len=*), optional, intent(in) :: build
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
  !> with a dot. Numerical identifiers must not start with a zero.
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
    character(len=*), optional, intent(in) :: prerelease
    character(len=*), optional, intent(in) :: build
    type(error_t), allocatable, intent(out) :: error
    logical, optional, intent(in) :: strict_mode

    logical :: is_strict_mode = .false.

    if (present(strict_mode)) is_strict_mode = strict_mode

    if (major < 0) then
      error = error_t('Version numbers must not be negative.'); return
    end if
    this%major = major

    if (present(minor)) then
      if (minor < 0) then
        error = error_t('Version numbers must not be negative.'); return
      end if
      this%minor = minor
    else
      if (is_strict_mode) then
        error = error_t('Strict mode: Minor version must be provided.'); return
      end if
      this%minor = 0
    end if

    if (present(patch)) then
      if (patch < 0) then
        error = error_t('Version numbers must not be negative.'); return
      end if
      this%patch = patch
    else
      if (is_strict_mode) then
        error = error_t('Strict mode: Patch version must be provided.'); return
      end if
      this%patch = 0
    end if

    if (present(prerelease)) then
      call build_identifiers(this%prerelease, prerelease, error)
      if (allocated(error)) return
    end if

    if (present(build)) then
      call build_identifiers(this%build, build, error)
      if (allocated(error)) return
    end if
  end

  !> Returns a string representation of the version including prerelease and
  !> build data.
  pure function to_string(this) result(str)
    class(version_t), intent(in) :: this
    character(len=:), allocatable :: str

    integer :: i

    str = trim(int2s(this%major))//'.' &
    &   //trim(int2s(this%minor))//'.' &
    &   //trim(int2s(this%patch))

    if (allocated(this%prerelease)) then
      str = str//'-'
      do i = 1, size(this%prerelease)
        str = str//this%prerelease(i)%str
        if (i < size(this%prerelease)) str = str//'.'
      end do
    end if

    if (allocated(this%build)) then
      str = str//'+'
      do i = 1, size(this%build)
        str = str//this%build(i)%str
        if (i < size(this%build)) str = str//'.'
      end do
    end if
  end

  !> Increments the major version number and resets the minor and patch number
  !> as well as the prerelease and build data.
  pure subroutine increment_major(this)
    class(version_t), intent(inout) :: this

    this%major = this%major + 1
    this%minor = 0
    this%patch = 0

    if (allocated(this%prerelease)) deallocate (this%prerelease)
    if (allocated(this%build)) deallocate (this%build)
  end

  !> Increments the minor version number and resets patch, prerelease and build.
  pure subroutine increment_minor(this)
    class(version_t), intent(inout) :: this

    this%minor = this%minor + 1
    this%patch = 0

    if (allocated(this%prerelease)) deallocate (this%prerelease)
    if (allocated(this%build)) deallocate (this%build)
  end

  !> Increments the patch version number and resets prerelease and build.
  pure subroutine increment_patch(this)
    class(version_t), intent(inout) :: this

    this%patch = this%patch + 1

    if (allocated(this%prerelease)) deallocate (this%prerelease)
    if (allocated(this%build)) deallocate (this%build)
  end

  !> Increment prerelease and reset build data.
  pure subroutine increment_prerelease(this)
    class(version_t), intent(inout) :: this

    call increment_identifier(this%prerelease)
    if (allocated(this%build)) deallocate (this%build)
  end

  !> Increment build metadata.
  pure subroutine increment_build(this)
    class(version_t), intent(inout) :: this

    call increment_identifier(this%build)
  end

  !> Increment prerelease or build identifiers. If the last identifier is
  !> numeric, increment it by 1. Otherwise add a new identifier with the value
  !> 1.
  pure subroutine increment_identifier(ids)
    type(string_t), allocatable, intent(inout) :: ids(:)

    if (allocated(ids)) then
      if (ids(size(ids))%is_numeric()) then
        ids = [ids(1:size(ids) - 1), &
        & string_t(trim(int2s(ids(size(ids))%num() + 1)))]
      else
        ids = [ids, string_t('1')]
      end if
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
  !> In strict mode, all major, minor and patch versions must be provided. Implicit
  !> zeros are forbidden in strict mode.
  function parse(str, strict_mode) result(version)
    character(len=*), intent(in) :: str
    logical, optional, intent(in) :: strict_mode
    type(version_t) :: version

    type(error_t), allocatable :: error

    call version%parse(str, error, strict_mode)
    if (allocated(error)) error stop error%msg
  end

  !> Attempt to parse a string into a version including prerelease and build
  !> data. In strict mode, all major, minor and patch versions must be provided.
  !> Implicit zeros are forbidden in strict mode.
  subroutine try_parse(this, str, error, strict_mode)
    class(version_t), intent(out) :: this
    character(len=*), intent(in) :: str
    type(error_t), allocatable, intent(out) :: error
    logical, optional, intent(in) :: strict_mode

    integer :: i, j

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
    character(len=*), intent(in) :: str
    type(error_t), allocatable, intent(out) :: error
    logical, optional, intent(in) :: strict_mode

    integer :: i, j, l
    logical :: is_strict_mode = .false.

    if (present(strict_mode)) is_strict_mode = strict_mode

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
    character(len=*), intent(in) :: str
    integer, intent(out) :: num
    type(error_t), allocatable, intent(out) :: error

    integer :: i
    character(len=1) :: c

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
  pure integer function s2i(str)
    character(len=*), intent(in) :: str

    type(error_t), allocatable :: e

    call s2int(str, s2i, e)
    if (allocated(e)) error stop e%msg
  end

  !> Convert a `string_t` to an integer.
  pure integer function string_t_2i(this)
    class(string_t), intent(in) :: this

    type(error_t), allocatable :: e

    call s2int(this%str, string_t_2i, e)
    if (allocated(e)) error stop e%msg
  end

  !> Convert an integer to a string.
  pure function int2s(num) result(str)
    integer, intent(in) :: num
    character(len=:), allocatable :: str

    if (num == 0) then
      str = '0'
    else
      allocate (character(int(log10(real(num))) + 1) :: str)
      write (str, '(I0)') num
    end if
  end

  !> Check for valid prerelease or build data and build identfiers from
  !> the string.
  pure subroutine build_identifiers(ids, str, error)
    type(string_t), allocatable, intent(out) :: ids(:)
    character(len=*), intent(in) :: str
    type(error_t), allocatable, intent(out) :: error

    character(len=*), parameter :: valid_chars = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWYYZ-.'
    character(len=:), allocatable :: string
    integer :: i

    if (len_trim(str) == 0) then
      error = error_t('Entered data cannot be empty.'); return
    end if

    do i = 1, len(str)
      if (index(valid_chars, str(i:i)) == 0) then
        error = error_t("Data contains invalid character: '"//str//"'."); return
      end if
    end do

    string = str
    allocate (ids(0))

    ! Last character must not be a dot.
    if (string(len(string):len(string)) == '.') then
      error = error_t('Data must not start or end with a dot.'); return
    end if

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
    character(len=*), intent(in) :: str
    type(error_t), allocatable, intent(out) :: error

    ! Identifiers must not start with `.`.
    if (str(1:1) == '.') then
      error = error_t("Identifiers must not start with '.'"); return
    end if

    ! Numerical identifiers must not start with 0.
    if (is_numeric(str) .and. str(1:1) == '0') then
      error = error_t("Numerical identifiers must not start with '0'."); return
    end if
  end

  !> Check if a string is purely numeric.
  pure function is_numeric(str)
    character(len=*), intent(in) :: str
    logical :: is_numeric

    is_numeric = verify(str, '0123456789') == 0
  end

  !> Check if string_t is purely numeric.
  pure function string_t_is_numeric(this)
    class(string_t), intent(in) :: this
    logical :: string_t_is_numeric

    string_t_is_numeric = verify(this%str, '0123456789') == 0
  end

  !> Check two versions for equality.
  pure logical function equals(lhs, rhs)
    class(version_t), intent(in) :: lhs
    class(version_t), intent(in) :: rhs

    integer :: i

    equals = lhs%major == rhs%major &
    &  .and. lhs%minor == rhs%minor &
    &  .and. lhs%patch == rhs%patch

    if (.not. equals) return

    if (allocated(lhs%prerelease) .and. allocated(rhs%prerelease)) then
      if (size(lhs%prerelease) /= size(rhs%prerelease)) then
        equals = .false.; return
      end if
      do i = 1, size(lhs%prerelease)
        if (lhs%prerelease(i)%str /= rhs%prerelease(i)%str) then
          equals = .false.; return
        end if
      end do
    else if (allocated(lhs%prerelease) .or. allocated(rhs%prerelease)) then
      equals = .false.
    end if
  end

  !> Check two versions for inequality.
  pure logical function not_equals(lhs, rhs)
    class(version_t), intent(in) :: lhs
    class(version_t), intent(in) :: rhs

    not_equals = .not. lhs == rhs
  end

  !> Check if the first version is greater than the second.
  pure logical function greater_than(lhs, rhs)
    class(version_t), intent(in) :: lhs
    class(version_t), intent(in) :: rhs

    greater_than = lhs%major > rhs%major &
    & .or. (lhs%major == rhs%major &
    & .and. lhs%minor > rhs%minor) &
    & .or. (lhs%major == rhs%major &
    & .and. lhs%minor == rhs%minor &
    & .and. lhs%patch > rhs%patch)

    if (greater_than) return

    if (lhs%major == rhs%major .and. lhs%minor == rhs%minor .and. lhs%patch == rhs%patch) then
      if (allocated(lhs%prerelease) .and. .not. allocated(rhs%prerelease)) then
        greater_than = .false.
      else if (.not. allocated(lhs%prerelease) .and. allocated(rhs%prerelease)) then
        greater_than = .true.
      else if (allocated(lhs%prerelease) .and. allocated(rhs%prerelease)) then
        greater_than = is_greater(lhs%prerelease, rhs%prerelease)
      end if
    end if
  end

  !> Check if the first version is smaller than the second.
  pure logical function less_than(lhs, rhs)
    class(version_t), intent(in) :: lhs
    class(version_t), intent(in) :: rhs

    less_than = .not. lhs > rhs .and. .not. lhs == rhs
  end

  !> Check if the first version is greater than or equal to the second.
  pure logical function greater_equals(lhs, rhs)
    class(version_t), intent(in) :: lhs
    class(version_t), intent(in) :: rhs

    greater_equals = lhs > rhs .or. lhs == rhs
  end

  !> Check if the first version is smaller than or equal to the second.
  pure logical function less_equals(lhs, rhs)
    class(version_t), intent(in) :: lhs
    class(version_t), intent(in) :: rhs

    less_equals = .not. lhs > rhs
  end

  !> Check if the first prerelease (`lhs`) is greater than the second (`rhs`).
  pure logical function is_greater(lhs, rhs)
    type(string_t), intent(in) :: lhs(:)
    type(string_t), intent(in) :: rhs(:)

    integer :: i, j

    do i = 1, min(size(lhs), size(rhs))
      if (lhs(i)%str == rhs(i)%str) cycle
      if (is_numeric(lhs(i)%str) .and. is_numeric(rhs(i)%str)) then
        is_greater = s2i(lhs(i)%str) > s2i(rhs(i)%str); return
      else if (is_numeric(lhs(i)%str)) then
        is_greater = .false.; return
      else if (is_numeric(rhs(i)%str)) then
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
  !> This procedure has been added for conveniece. It is not part of the
  !> Semantic Versioning 2.0.0 specification.
  pure logical function is_exactly(self, other)
    class(version_t), intent(in) :: self
    type(version_t), intent(in) :: other

    integer :: i

    is_exactly = self == other; 
    if (.not. is_exactly) return

    if (allocated(self%build) .and. allocated(other%build)) then
      if (size(self%build) /= size(other%build)) then
        is_exactly = .false.; return
      end if

      do i = 1, size(self%build)
        if (self%build(i)%str /= other%build(i)%str) then
          is_exactly = .false.; return
        end if
      end do
    else if (allocated(self%build) .or. allocated(other%build)) then
      is_exactly = .false.; return
    end if
  end

  !> True if the string can be parsed as a valid `version_t`. Use `parse` if you
  !> wish to receive detailed error messages. In strict mode, all major, minor
  !> and patch versions must be provided. Implicit zeros are forbidden in strict
  !> mode.
  logical function is_version(str, strict_mode)
    character(len=*), intent(in) :: str
    logical, optional, intent(in) :: strict_mode

    type(version_t) :: version
    type(error_t), allocatable :: error

    call version%parse(str, error, strict_mode)
    is_version = .not. allocated(error)
  end

end
