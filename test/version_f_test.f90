program test
  use version_f

  implicit none

  type(version_t) :: v1, v2
  logical :: is_satisfied
  type(comparator_t), allocatable :: comps(:)
  type(comparator_set_t) :: comp_set
  type(version_range_t) :: range
  type(error_t), allocatable :: e

!##################################try_satisfy#################################!

  v1 = version_t(0, 1, 0)

  call v1%try_satisfy('', is_satisfied, e)
  if (.not. allocated(e)) call fail('satisfy-1 should fail.')

  call v1%try_satisfy(' ', is_satisfied, e)
  if (.not. allocated(e)) call fail('satisfy-2 should fail.')

  call v1%try_satisfy('0.1.0', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-3 should satisfy.')
  if (allocated(e)) call fail('satisfy-3 should not fail.')

  v1 = version_t(0, 1, 0, 'abc')
  call v1%try_satisfy('0.1.0', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-4 should not satisfy.')
  if (allocated(e)) call fail('satisfy-4 should not fail.')

  v1 = version_t(0, 1, 0)
  call v1%try_satisfy('0.1.0-abc', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-5 should not satisfy.')
  if (allocated(e)) call fail('satisfy-5 should not fail.')

  v1 = version_t(0, 1, 0, 'abc')
  call v1%try_satisfy('0.1.0-cde', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-6 should not satisfy.')
  if (allocated(e)) call fail('satisfy-6 should not fail.')

  v1 = version_t(0, 1, 0, 'abc', 'cde')
  call v1%try_satisfy('0.1.0-abc', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-7 should satisfy.')
  if (allocated(e)) call fail('satisfy-7 should not fail.')

  v1 = version_t(0, 1, 0, 'abc', 'cde')
  call v1%try_satisfy('0.1.0-abc+xyz', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-8 should satisfy.')
  if (allocated(e)) call fail('satisfy-8 should not fail.')

  v1 = version_t(0, 1, 0)
  call v1%try_satisfy('  0.1.0  ', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-9 should satisfy.')
  if (allocated(e)) call fail('satisfy-9 should not fail.')

  call v1%try_satisfy('  0.1.0+abc  ', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-10 should satisfy.')
  if (allocated(e)) call fail('satisfy-10 should not fail.')

  call v1%try_satisfy('0.2.0', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-11 should not satisfy.')
  if (allocated(e)) call fail('satisfy-11 should not fail.')

  call v1%try_satisfy('0.2.0', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-12 should not satisfy.')
  if (allocated(e)) call fail('satisfy-12 should not fail.')

  call v1%try_satisfy('=0.1.0', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-13 should satisfy.')
  if (allocated(e)) call fail('satisfy-13 should not fail.')

  call v1%try_satisfy('=   0.1.0', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-14 should satisfy.')
  if (allocated(e)) call fail('satisfy-14 should not fail.')

  call v1%try_satisfy('= 0.2.0', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-15 should not satisfy.')
  if (allocated(e)) call fail('satisfy-15 should not fail.')

  call v1%try_satisfy('!=0.2.0', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-16 should satisfy.')
  if (allocated(e)) call fail('satisfy-16 should not fail.')

  call v1%try_satisfy('!=0.1.0', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-17 should not satisfy.')
  if (allocated(e)) call fail('satisfy-17 should not fail.')

  call v1%try_satisfy('!= 0.1.0', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-18 should not satisfy.')
  if (allocated(e)) call fail('satisfy-18 should not fail.')

  call v1%try_satisfy('0.1.0abcd', is_satisfied, e)
  if (.not. allocated(e)) call fail('satisfy-19 should fail.')

  call v1%try_satisfy('=0.1.0abcd', is_satisfied, e)
  if (.not. allocated(e)) call fail('satisfy-20 should fail.')

  call v1%try_satisfy('>0.1.0', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-21 should not satisfy.')
  if (allocated(e)) call fail('satisfy-21 should not fail.')

  call v1%try_satisfy('>0.1.0-1', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-22 should satisfy.')
  if (allocated(e)) call fail('satisfy-22 should not fail.')

  call v1%try_satisfy('> 0.0.9', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-23 should satisfy.')
  if (allocated(e)) call fail('satisfy-23 should not fail.')

  call v1%try_satisfy('>=0.1.0', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-24 should satisfy.')
  if (allocated(e)) call fail('satisfy-24 should not fail.')

  call v1%try_satisfy('>=   0.1.0-678', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-25 should satisfy.')
  if (allocated(e)) call fail('satisfy-25 should not fail.')

  call v1%try_satisfy('>=0.1.0+123', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-26 should satisfy.')
  if (allocated(e)) call fail('satisfy-26 should not fail.')

  call v1%try_satisfy('<0.1.0', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-27 should not satisfy.')
  if (allocated(e)) call fail('satisfy-27 should not fail.')

  call v1%try_satisfy('< 0.1.0-1', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-28 should not satisfy.')
  if (allocated(e)) call fail('satisfy-28 should not fail.')

  call v1%try_satisfy('< 0.0.9', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-29 should not satisfy.')
  if (allocated(e)) call fail('satisfy-29 should not fail.')

  call v1%try_satisfy('<=0.1.0', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-30 should satisfy.')
  if (allocated(e)) call fail('satisfy-30 should not fail.')

  call v1%try_satisfy('<=   0.1.0-678', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-31 should not satisfy.')
  if (allocated(e)) call fail('satisfy-31 should not fail.')

  call v1%try_satisfy('<=0.1.0+123', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-32 should satisfy.')
  if (allocated(e)) call fail('satisfy-32 should not fail.')

  call v1%try_satisfy(' abc ', is_satisfied, e)
  if (.not. allocated(e)) call fail('satisfy-33 should fail.')

  call v1%try_satisfy('0.0.1 1.0.0', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-34 should not satisfy.')
  if (allocated(e)) call fail('satisfy-34 should not fail.')

  call v1%try_satisfy('0.0.1 0.1.0', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-35 should not satisfy.')
  if (allocated(e)) call fail('satisfy-35 should not fail.')

  call v1%try_satisfy('  > 1.0.1 <  2.1.0 ', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-36 should not satisfy.')
  if (allocated(e)) call fail('satisfy-36 should not fail.')

  call v1%try_satisfy('>0.0.1 <=0.1.0', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-37 should satisfy.')
  if (allocated(e)) call fail('satisfy-37 should not fail.')

  call v1%try_satisfy('>0.0.1 <0.1.0', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-38 should not satisfy.')
  if (allocated(e)) call fail('satisfy-38 should not fail.')

  call v1%try_satisfy('<0.1.0 || 0.0.1', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-39 should not satisfy.')
  if (allocated(e)) call fail('satisfy-39 should not fail.')

  call v1%try_satisfy('<0.1.0 || 0.1.0', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-40 should satisfy.')
  if (allocated(e)) call fail('satisfy-40 should not fail.')

  call v1%try_satisfy('<0.1.0 || >0.1.0 || != 0.1.0', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-41 should not satisfy.')
  if (allocated(e)) call fail('satisfy-41 should not fail.')

  call v1%try_satisfy('<0.1.0 || >0.1.0 || =0.1.0', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-42 should satisfy.')
  if (allocated(e)) call fail('satisfy-42 should not fail.')

  call v1%try_satisfy('<0.1.0 0.1.0 >2.0.0 || !=0.1.0 <0.2.1', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-43 should not satisfy.')
  if (allocated(e)) call fail('satisfy-43 should not fail.')

  call v1%try_satisfy('>=0.1.0 <2.0.0 ||  >0.2.0', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-44 should satisfy.')
  if (allocated(e)) call fail('satisfy-44 should not fail.')

  call v1%try_satisfy('2.1.0 2.0.0 0.1.0 || >=0.2.0', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-45 should not satisfy.')
  if (allocated(e)) call fail('satisfy-45 should not fail.')

  call v1%try_satisfy('>0.1.0 <2.0.0 0.1.0 ||  <=0.2.0   0.1.0', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-46 should satisfy.')
  if (allocated(e)) call fail('satisfy-46 should not fail.')

  call v1%try_satisfy('>0.1.0 <2.0.0 0.1.0 ||  <=0.2.0   0.0.9', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-47 should not satisfy.')
  if (allocated(e)) call fail('satisfy-47 should not fail.')

  call v1%try_satisfy(' bx ||  <=0.2.0   0.0.9', is_satisfied, e)
  if (.not. allocated(e)) call fail('satisfy-48 should fail.')

  call v1%try_satisfy(' 0.1.0 || ahc', is_satisfied, e)
  if (.not. allocated(e)) call fail('satisfy-49 should fail.')

  call v1%try_satisfy(' || 0.1.0', is_satisfied, e)
  if (.not. allocated(e)) call fail('satisfy-50 should fail.')

  call v1%try_satisfy('0.1.0 || ', is_satisfied, e)
  if (.not. allocated(e)) call fail('satisfy-51 should fail.')

  call v1%try_satisfy('>=0.1.0<2.0.0 0.1.0||<=0.2.0 0.0.9', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-52 should satisfy.')
  if (allocated(e)) call fail('satisfy-52 should not fail.')

  call v1%try_satisfy('>0.1.0-abc', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-53 should satisfy.')
  if (allocated(e)) call fail('satisfy-53 should not fail.')

  call v1%try_satisfy('>0.0.1-abc', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-54 should satisfy.')
  if (allocated(e)) call fail('satisfy-54 should not fail.')

  call v1%try_satisfy('>0.1.0+abc', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-55 should not satisfy.')
  if (allocated(e)) call fail('satisfy-55 should not fail.')

  call v1%try_satisfy('>=0.1.0+abc', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-56 should satisfy.')
  if (allocated(e)) call fail('satisfy-56 should not fail.')

  call v1%try_satisfy('0.1.0-abc', is_satisfied, e)
  if (is_satisfied) call fail('satisfy-57 should not satisfy.')
  if (allocated(e)) call fail('satisfy-57 should not fail.')

  call v1%try_satisfy('0.1.0+abc', is_satisfied, e)
  if (.not. is_satisfied) call fail('satisfy-58 should satisfy.')
  if (allocated(e)) call fail('satisfy-58 should not fail.')
