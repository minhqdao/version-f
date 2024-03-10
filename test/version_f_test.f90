program test
  use version_f
  implicit none

  call try_satisfy('> 1.0.1 <  2.1.0')

  print *, achar(10)//achar(27)//'[92m All tests passed.'//achar(27)

contains

  subroutine fail(msg)
    character(*), intent(in) :: msg
    print *, achar(27)//'[31m'//'Test failed: '//msg//achar(27)//'[0m'
    stop 1
  end
end program
