!> This program loads a list of versions and checks whether they satisfy given version ranges.
program ex2
  use version_f

  implicit none

  type(version_t), allocatable :: all_versions(:)
  type(version_t), allocatable :: smaller_versions(:)
  type(version_t), allocatable :: versions_between(:)
  type(version_t), allocatable :: greater_versions(:)
  integer :: i

  call load_versions(all_versions)

  allocate (smaller_versions(0))
  allocate (versions_between(0))
  allocate (greater_versions(0))

  do i = 1, size(all_versions)
    if (all_versions(i)%satisfies('<1.0.0')) then
      smaller_versions = [smaller_versions, all_versions(i)]
    else if (all_versions(i)%satisfies('>=1.0.0 <2.0.0')) then
      versions_between = [versions_between, all_versions(i)]
    else if (all_versions(i)%satisfies('>=2.0.0')) then
      greater_versions = [greater_versions, all_versions(i)]
    end if
  end do

  print *, 'Versions less than 1.0.0:'
  do i = 1, size(smaller_versions)
    print *, smaller_versions(i)%to_string()
  end do

  print *, ''
  print *, 'Versions greater or equal 1.0.0 and less than 2.0.0:'
  do i = 1, size(versions_between)
    print *, versions_between(i)%to_string()
  end do

  print *, ''
  print *, 'Versions greater or equal 2.0.0:'
  do i = 1, size(greater_versions)
    print *, greater_versions(i)%to_string()
  end do

contains

  subroutine load_versions(versions)
    type(version_t), allocatable, intent(out) :: versions(:)

    versions = [ &
      & version_t(0, 2, 5), &
      & version_t(0, 2, 5, 'pre'), &
      & version_t(0, 99, 999), &
      & version_t(1, 2), &
      & version_t(1, 2, 4), &
      & version_t(1, 2, 4, 'alpha'), &
      & version_t(1, 2, 4, 'alpha', '1'), &
      & version_t(2, prerelease='pre'), &
      & version_t(2, build='one'), &
      & version_t(2, 3, 5), &
      & version_t(2, 5) &
    & ]
  end

end
