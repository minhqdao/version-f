module version_f
  implicit none
  private

  public :: try_satisfy

  type :: string_t
    character(:), allocatable :: str
  end type

  type :: version_t
    integer :: major
    integer :: minor
    integer :: patch
    type(string_t), allocatable :: prerelease(:)
    type(string_t), allocatable :: build(:)
  end type

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
    print *, 'before array extension: ', this%comps(1)%op ! Pay attention to this
    this%comps = [this%comps, comp]
    print *, 'after array extension: ', this%comps(1)%op ! And this
  end
end
