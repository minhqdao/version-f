program test
  use version_f

  implicit none

  type(version_t) :: v1
  logical :: is_satisfied
  type(error_t), allocatable :: e

   v1 = version_t(0, 1, 0)

  call v1%try_satisfy('  > 1.0.1 <  2.1.0 ', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-36 should not satisfy.')
  if (allocated(e)) call fail('satisfy-36 should not fail.')

  call v1%try_satisfy('>0.0.1 <=0.1.0', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-37 should satisfy.')
  if (allocated(e)) call fail('satisfy-37 should not fail.')

  print *, achar(10)//achar(27)//'[92m All tests passed.'//achar(27)

contains

  subroutine fail(msg)
    character(*), intent(in) :: msg
    print *, achar(27)//'[31m'//'Test failed: '//msg//achar(27)//'[0m'
    stop 1
  end
end program
