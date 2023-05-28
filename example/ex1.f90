module version_f__boxes
  implicit none
  private

  public :: box, load_boxes

  type box
    character(len=:), allocatable :: name
    character(len=:), allocatable :: version
  end type

contains
  subroutine load_boxes(boxes)
    type(box), allocatable, intent(out) :: boxes(:)

    boxes = [ &
    & box('nice box', '1.0'), &
    & box('prototype box', '0.0.1'), &
    & box('nicest box', '2.1'), &
    & box('nearly nicest box', '2.1.0-alpha+1') &
    & ]
  end
end

!> A program that loads boxes and finds the box with the highest version number.
program ex1
  use version_f__boxes, only: box, load_boxes
  use version_f, only: version_t
  implicit none

  type(box), allocatable :: boxes(:)
  type(version_t), allocatable :: versions(:)
  type(box) :: latest_box
  type(version_t) :: highest_version
  integer :: i

  call load_boxes(boxes)

  ! Create and register versions
  allocate (versions(size(boxes)))
  do i = 1, size(boxes)
    versions(i) = version_t(boxes(i)%version)
  end do

  ! Find the highest version number
  highest_version = versions(1)
  latest_box = boxes(1)
  do i = 1, size(versions)
    if (versions(i) > highest_version) then
      highest_version = versions(i)
      latest_box = boxes(i)
    end if
  end do

  print *, "The latest box is '", latest_box%name, "'."
  print *, "It has the version '", highest_version%to_string(), "'."
end
