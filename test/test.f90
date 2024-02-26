program test
  use version_f

  implicit none

  type(version_t) :: v1, v2
  logical :: is_satisfied
  type(comparator_t), allocatable :: comps(:)
  type(comparator_set_t) :: comp_set
  type(version_range_t) :: range
  type(error_t), allocatable :: e

!################################### Create ###################################!

  v1 = version_t(0, 0, 0)
  if (v1%to_string() /= '0.0.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3)
  if (v1%to_string() /= '1.2.3') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(9999999, 0, 21)
  if (v1%to_string() /= '9999999.0.21') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(0)
  if (v1%to_string() /= '0.0.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(10)
  if (v1%to_string() /= '10.0.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(2, 25)
  if (v1%to_string() /= '2.25.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(00002, 25, 0000090)
  if (v1%to_string() /= '2.25.90') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%create(-1, error=e)
  if (.not. allocated(e)) call fail('A negative number should fail.')

  call v1%create(1, -3, error=e)
  if (.not. allocated(e)) call fail('A negative number should fail.')

  call v1%create(1, 5, -3, error=e)
  if (.not. allocated(e)) call fail('A negative number should fail.')

  call v1%create(1, 5, 3, '', error=e)
  if (.not. allocated(e)) call fail('An empty string should fail.')

  call v1%create(1, 5, 3, '1234/', error=e)
  if (.not. allocated(e)) call fail('Invalid character.')

  call v1%create(1, 5, 3, 'abc', '', e)
  if (.not. allocated(e)) call fail('An empty string should fail.')

  call v1%create(1, 5, 3, 'abc', 'abc&def', e)
  if (.not. allocated(e)) call fail('Invalid character.')

  v1 = version_t(1, 5, 3, 'abc', '789')
  if (v1%to_string() /= '1.5.3-abc+789') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, prerelease='abc.def---', build='---789.abc')
  if (v1%to_string() /= '1.0.0-abc.def---+---789.abc') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%create(1, prerelease='abc....ded', build='---789.abc', error=e)
  if (.not. allocated(e)) call fail('Invalid prerelease missed.')

  call v1%create(1, prerelease='abc.ded', build='--..-789.abc', error=e)
  if (.not. allocated(e)) call fail('Invalid build missed.')

  call v1%create(1, prerelease='0abc.ded', build='---789.ab missedc', error=e)
  if (.not. allocated(e)) call fail('Invalid prerelease missed.')

  call v1%create(1, prerelease='abc.ded', build='05567.abc', error=e)
  if (.not. allocated(e)) call fail('Invalid build missed.')

  v1 = version_t(0, 1, 0, prerelease='abc.def---', build='0---789.abc')
  if (v1%to_string() /= '0.1.0-abc.def---+0---789.abc') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%create(1, prerelease='abc.ded', build='.', error=e)
  if (.not. allocated(e)) call fail('Invalid build missed.')

  call v1%create(1, prerelease='d.', build='9', error=e)
  if (.not. allocated(e)) call fail('Invalid prerelease missed.')

  v1 = version_t(1, 3, prerelease='a.b.c.d.e', build='---789.abc')
  if (v1%to_string() /= '1.3.0-a.b.c.d.e+---789.abc') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 3, prerelease='a000', build='---789.abc')
  if (v1%to_string() /= '1.3.0-a000+---789.abc') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 3, prerelease='0abc', build='000-')
  if (v1%to_string() /= '1.3.0-0abc+000-') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%create(1, prerelease='d', build='9.0', error=e)
  if (.not. allocated(e)) call fail('Invalid build missed.')

!################################# Increment ##################################!

  v1 = version_t(2, 25, 0, 'ab0c', '123')
  call v1%increment_major()
  if (v1%to_string() /= '3.0.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(2, 25, 46, 'abc', '12tg3')
  call v1%increment_minor()
  if (v1%to_string() /= '2.26.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(2, 25, 46, 'abc.789', '---123')
  call v1%increment_patch()
  if (v1%to_string() /= '2.25.47') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3)
  call v1%increment_prerelease()
  if (v1%to_string() /= '1.2.3-1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3, build='123')
  call v1%increment_prerelease()
  if (v1%to_string() /= '1.2.3-1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3, '80')
  call v1%increment_prerelease()
  if (v1%to_string() /= '1.2.3-81') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3, '80', '123')
  call v1%increment_prerelease()
  if (v1%to_string() /= '1.2.3-81') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3, 'abc.789')
  call v1%increment_prerelease()
  if (v1%to_string() /= '1.2.3-abc.790') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3, '123.789')
  call v1%increment_prerelease()
  if (v1%to_string() /= '1.2.3-123.790') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3, 'abc')
  call v1%increment_prerelease()
  if (v1%to_string() /= '1.2.3-abc.1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3, 'a23c')
  call v1%increment_prerelease()
  if (v1%to_string() /= '1.2.3-a23c.1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3, build='1')
  call v1%increment_build()
  if (v1%to_string() /= '1.2.3+2') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3)
  call v1%increment_build()
  if (v1%to_string() /= '1.2.3+1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3, 'abc')
  call v1%increment_build()
  if (v1%to_string() /= '1.2.3-abc+1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3, 'abc', '123')
  call v1%increment_build()
  if (v1%to_string() /= '1.2.3-abc+124') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3, '123', '123')
  call v1%increment_build()
  if (v1%to_string() /= '1.2.3-123+124') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3, '123', 'abc')
  call v1%increment_build()
  if (v1%to_string() /= '1.2.3-123+abc.1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3, '123', '78H')
  call v1%increment_build()
  if (v1%to_string() /= '1.2.3-123+78H.1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3, '123', '78-')
  call v1%increment_build()
  if (v1%to_string() /= '1.2.3-123+78-.1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

!################################### Parse ####################################!

  call v1%parse('0', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '0.0.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('.', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '0.0.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('0.1', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '0.1.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('..988', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '0.0.988') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1..988', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '1.0.988') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('.1.', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '0.1.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('..', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '0.0.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('', e)
  if (.not. allocated(e)) call fail('An empty string should fail.')

  call v1%parse('-1', e)
  if (.not. allocated(e)) call fail('A negative number should fail.')

  call v1%parse('.-1.', e)
  if (.not. allocated(e)) call fail('A negative number should fail.')

  call v1%parse('a', e)
  if (.not. allocated(e)) call fail('Invalid character should fail.')

  call v1%parse('..a', e)
  if (.not. allocated(e)) call fail('Invalid character should fail.')

  call v1%parse('0.1.0.2', e)
  if (.not. allocated(e)) call fail('Too many dots.')

  call v1%parse('1-1', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '1.0.0-1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('8.1-1', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '8.1.0-1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1-1.1', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '1.0.0-1.1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('8.1-1.9-9--.2', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '8.1.0-1.9-9--.2') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1+1', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '1.0.0+1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1+1.1-0P.2', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '1.0.0+1.1-0P.2') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1+f-1', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '1.0.0+f-1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1-23+1-1', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '1.0.0-23+1-1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1.0.1-43.fs23+1-1', e)
  if (allocated(e)) then
    call fail(e%msg)
  else if (v1%to_string() /= '1.0.1-43.fs23+1-1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1-0', e)
  if (.not. allocated(e)) call fail('Leading zero identifier not allowed.')

  call v1%parse('1-hff.08', e)
  if (.not. allocated(e)) call fail('Leading zero identifier not allowed.')

  call v1%parse('1-hff.87+08', e)
  if (.not. allocated(e)) call fail('Leading zero identifier not allowed.')

  call v1%parse('1-hff.87+fejf.08', e)
  if (.not. allocated(e)) call fail('Leading zero identifier not allowed.')

  call v1%parse('1-..', e)
  if (.not. allocated(e)) call fail('No consecutive dots.')

  call v1%parse('1-irhife..oihie', e)
  if (.not. allocated(e)) call fail('No consecutive dots.')

  call v1%parse('1-irh+ife..oihie.', e)
  if (.not. allocated(e)) call fail('Trailing dot.')

  call v1%parse('1-irh+.ife..oihie', e)
  if (.not. allocated(e)) call fail('Leading dot.')

!################################## Compare ###################################!

  v1 = version_t(1, 2, 3)
  v2 = version_t(1, 2, 3)
  if (.not. v1 == v2) call fail('Equality failed.')
  if (v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(1, 2, 3)
  v2 = version_t('1.2.3')
  if (.not. v1 == v2) call fail('Equality failed.')
  if (v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(90, 3, 0)
  v2 = version_t(90, 3)
  if (.not. v1 == v2) call fail('Equality failed.')
  if (v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(1, 5, 3)
  v2 = version_t(90)
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(1, 5, 1)
  v2 = version_t(1, 0, 1)
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t('..999')
  v2 = version_t('.1.')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(1)
  v2 = version_t(0, 0, 1)
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  call v1%create(1, build='abc', error=e)
  call v2%create(1, build='abc', error=e)
  if (.not. v1 == v2) call fail('Equality failed.')
  if (v1 /= v2) call fail('InequalityInequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  call v1%create(1, build='abc', error=e)
  call v2%create(1, build='123', error=e)
  if (.not. v1 == v2) call fail('Equality failed.')
  if (v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(1, prerelease='abc')
  v2 = version_t(0, 1, prerelease='abc')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'abc0123---')
  v2 = version_t(0, 3, 0, 'abc0123---')
  if (.not. v1 == v2) call fail('Equality failed.')
  if (v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'abc.123.---')
  v2 = version_t(0, 3, 0, 'abc.123.---')
  if (.not. v1 == v2) call fail('Equality failed.')
  if (v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'abc.123.---')
  v2 = version_t(0, 3, 0, 'abc.123.--')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'abc.123.--')
  v2 = version_t(0, 3, 0, 'abc.123.---')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'abc.123')
  v2 = version_t(0, 3, 0, 'abc')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'abc')
  v2 = version_t(0, 3, 0, 'abc.123')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 4, 0, '2')
  v2 = version_t(0, 4, 0, '3')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 4, 0, '3')
  v2 = version_t(0, 4, 0, '2')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 4, 0, '911')
  v2 = version_t(0, 4, 0, '199')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 4, 0, '911')
  v2 = version_t(0, 4, 0, '1991')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, '123')
  v2 = version_t(0, 3, 0, '---')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, '---')
  v2 = version_t(0, 3, 0, '123')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'abc')
  v2 = version_t(0, 3, 0, '---')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, '---')
  v2 = version_t(0, 3, 0, 'abc')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'abc')
  v2 = version_t(0, 3, 0, '1')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, '1')
  v2 = version_t(0, 3, 0, 'abc')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, '123.789')
  v2 = version_t(0, 3, 0, '789.123')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'beta')
  v2 = version_t(0, 3, 0, 'alpha')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'beta')
  v2 = version_t(0, 3, 0, 'alpha.1')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'beta')
  v2 = version_t(0, 3, 0, 'beta.1')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'alpha.999')
  v2 = version_t(0, 3, 0, 'beta.100')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'alpha.999')
  v2 = version_t(0, 3, 0, 'alphaa.1')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'alphaa.1')
  v2 = version_t(0, 3, 0, 'alpha.999')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'alpha.9')
  v2 = version_t(0, 3, 0, 'alpha.10')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'alpha10')
  v2 = version_t(0, 3, 0, 'alpha9')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (.not. v1 < v2) call fail('Less than failed.')
  if (v1 > v2) call fail('Greater than failed.')
  if (.not. v1 <= v2) call fail('Less than or equal failed.')
  if (v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'alpha9')
  v2 = version_t(0, 3, 0, 'alpha10')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(0, 3, 0, 'abc-b')
  v2 = version_t(0, 3, 0, 'abc-a')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  v1 = version_t(90, 3, 0, '---')
  v2 = version_t(90, 3, prerelease='--')
  if (v1 == v2) call fail('Equality failed.')
  if (.not. v1 /= v2) call fail('Inequality failed.')
  if (v1 < v2) call fail('Less than failed.')
  if (.not. v1 > v2) call fail('Greater than failed.')
  if (v1 <= v2) call fail('Less than or equal failed.')
  if (.not. v1 >= v2) call fail('Greater than or equal failed.')

  !############################### is_version #################################!

  if (is_version('')) call fail("'' isn't a version.")
  if (is_version(' ')) call fail("' ' isn't a version.")
  if (is_version('a')) call fail("'a' isn't version.")
  if (is_version('1a')) call fail("'1a' isn't a version.")
  if (is_version('1.0.0a')) call fail("'1.0.0a' isn't a version.")
  if (is_version('1.0.0.a')) call fail("'1.0.0.a' isn't a version.")
  if (is_version('1.0.0.a-a')) call fail("'1.0.0.a-a' isn't a version.")
  if (is_version('1.0.0-(')) call fail("'1.0.0-(' isn't a version.")
  if (is_version('1.0.0-')) call fail("'1.0.0-' isn't a version.")
  if (is_version('1.0.0-+')) call fail("'1.0.0-+' isn't a version.")
  if (is_version('1.0.0-0')) call fail("'1.0.0-' isn't a version.")
  if (is_version('1.0.0-ab..cd')) call fail("'1.0.0-ab..cd' isn't a version.")
  if (is_version('...')) call fail("'...' isn't a version.")
  if (is_version('-')) call fail("'-' isn't a version.")
  if (is_version('+')) call fail("'+' isn't a version.")
  if (.not. is_version('0.0.0')) call fail("'0.0.0' is a version.")
  if (.not. is_version('0.0.99999')) call fail("'0.0.99999' a version.")
  if (.not. is_version('7')) call fail("'7' is a version.")
  if (.not. is_version('7.49')) call fail("'7.49' is a version.")
  if (.not. is_version('7.49-a')) call fail("'7.49-a' is a version.")
  if (.not. is_version('7.49-a')) call fail("'7.49-a' is a version.")
  if (.not. is_version('7.49-a+12.a')) call fail("'7.49-a+12.a' is a version.")

  !############################### is_exactly #################################!

  v1 = version_t(0, 1, 0)
  if (.not. v1%is_exactly(v1)) call fail('0.1.0 is exactly 0.1.0.')
  v1 = version_t(0, 1, 0, 'a', '123')
  if (.not. v1%is_exactly(v1)) call fail('0.1.0-a+123 is exactly 0.1.0-a+123.')
  v1 = version_t(0, 1, 0)
  v2 = version_t(0, 1, 0)
  if (.not. v1%is_exactly(v2)) call fail('0.1.0 is exactly 0.1.0.')
  if (.not. v2%is_exactly(v1)) call fail('0.1.0 is exactly 0.1.0.')
  v1 = version_t(0, 0, 1)
  v2 = version_t(0, 1, 0)
  if (v1%is_exactly(v2)) call fail('0.0.1 is not exactly 0.1.0.')
  if (v2%is_exactly(v1)) call fail('0.0.1 is not exactly 0.1.0.')
  v1 = version_t(0, 1, 0, '123')
  v2 = version_t(0, 1, 0, '123')
  if (.not. v1%is_exactly(v2)) call fail('0.1.0-123 is exactly 0.1.0-123.')
  if (.not. v2%is_exactly(v1)) call fail('0.1.0-123 is exactly 0.1.0-123.')
  v1 = version_t(0, 1, 0, '123')
  v2 = version_t(0, 1, 0, 'abc')
  if (v1%is_exactly(v2)) call fail('0.1.0-123 is not exactly 0.1.0-abc.')
  if (v2%is_exactly(v1)) call fail('0.1.0-123 is not exactly 0.1.0-abc.')
  v1 = version_t(0, 1, 0, 'a', '1')
  v2 = version_t(0, 1, 0, 'a', '1')
  if (.not. v1%is_exactly(v2)) call fail('0.1.0-a+1 is exactly 0.1.0-a+1.')
  if (.not. v2%is_exactly(v1)) call fail('0.1.0-a+1 is exactly 0.1.0-a+1.')
  v1 = version_t(0, 1, 0, 'a', '1')
  v2 = version_t(0, 1, 0, 'a', '2')
  if (v1%is_exactly(v2)) call fail('0.1.0-a+1 is not exactly 0.1.0-a+2.')
  if (v2%is_exactly(v1)) call fail('0.1.0-a+1 is not exactly 0.1.0-a+2.')
  v1 = version_t(0, 1, 0, 'a')
  v2 = version_t(0, 1, 0, 'a', '2')
  if (v1%is_exactly(v2)) call fail('0.1.0-a is not exactly 0.1.0-a+2.')
  if (v2%is_exactly(v1)) call fail('0.1.0-a is not exactly 0.1.0-a+2.')
  v1 = version_t(0, 1, 0, 'a', '1')
  v2 = version_t(0, 1, 0, 'a')
  if (v1%is_exactly(v2)) call fail('0.1.0-a+1 is not exactly 0.1.0-a.')
  if (v2%is_exactly(v1)) call fail('0.1.0-a+1 is not exactly 0.1.0-a.')
  v1 = version_t(0, 1, 0, 'a', '1.1')
  v2 = version_t(0, 1, 0, 'a', '1')
  if (v1%is_exactly(v2)) call fail('0.1.0-a+1.1 is not exactly 0.1.0-1.')
  if (v2%is_exactly(v1)) call fail('0.1.0-a+1.1 is not exactly 0.1.0-1.')
  v1 = version_t(0, 1, 0, 'a', '1')
  v2 = version_t(0, 1, 0, 'a', '1.1')
  if (v1%is_exactly(v2)) call fail('0.1.0-a+1 is not exactly 0.1.0-1.1.')
  if (v2%is_exactly(v1)) call fail('0.1.0-a+1 is not exactly 0.1.0-1.1.')
  v1 = version_t(0, 1, 0, 'a', '1.123')
  v2 = version_t(0, 1, 0, 'a', '1.1')
  if (v1%is_exactly(v2)) call fail('0.1.0-a+1.123 is not exactly 0.1.0-1.1.')
  if (v2%is_exactly(v1)) call fail('0.1.0-a+1.123 is not exactly 0.1.0-1.1.')
  v1 = version_t(0, 1, 0, 'a', '1.1')
  v2 = version_t(0, 1, 0, 'a', '1.123')
  if (v1%is_exactly(v2)) call fail('0.1.0-a+1.1 is not exactly 0.1.0-1.123.')
  if (v2%is_exactly(v1)) call fail('0.1.0-a+1.1 is not exactly 0.1.0-1.123.')

  !############################### strict_mode ################################!

  v1 = version_t(1, 0, 0, strict_mode=.true.)
  if (v1%to_string() /= '1.0.0') then
    call fail("Strict mode: Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(0, 0, 0, strict_mode=.true.)
  if (v1%to_string() /= '0.0.0') then
    call fail("Strict mode: Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(0, 1, 0, 'abc', 'def', strict_mode=.true.)
  if (v1%to_string() /= '0.1.0-abc+def') then
    call fail("Strict mode: Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(3, strict_mode=.false.)
  if (v1%to_string() /= '3.0.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(3, patch=2, build='cde', strict_mode=.false.)
  if (v1%to_string() /= '3.0.2+cde') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%create(1, error=e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: No minor and patch.')

  call v1%create(1, 2, error=e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: Patch not provided.')

  call v1%create(1, patch=2, error=e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: Minor not provided.')

  call v1%create(1, 2, 3, error=e, strict_mode=.true.)
  if (allocated(e)) call fail('Strict mode: Everything is provided.')

  call v1%create(1, 2, 3, '1', '2', error=e, strict_mode=.true.)
  if (allocated(e)) call fail('Strict mode: Everything is provided.')

  call v1%create(1, 2, prerelease='1', build='2', error=e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: No patch.')

  call v1%create(1, patch=2, prerelease='1', build='2', error=e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: No minor.')

  call v1%create(1, 2, prerelease='1', build='2', error=e, strict_mode=.false.)
  if (allocated(e)) call fail('No strict mode: Missing patch.')

  call v1%create(1, error=e, strict_mode=.false.)
  if (allocated(e)) call fail('No strict mode: Missing minor and patch.')

  call v1%parse('1.2.3', error=e, strict_mode=.true.)
  if (allocated(e)) call fail('Strict mode: All is provided.')

  call v1%parse('1.2', error=e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: No patch.')

  call v1%parse('1', error=e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: No minor, no patch.')

  call v1%parse('1-1+1', error=e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: No minor, no patch.')

  call v1%parse('0', e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: No minor, no patch.')

  call v1%parse('.', e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: No patch.')

  call v1%parse('0.1', e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: No patch.')

  call v1%parse('.2.988', e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: No implicit major.')

  call v1%parse('1..988', e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: No implicit minor.')

  call v1%parse('100.1.', e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: No implicit patch.')

  call v1%parse('..', e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: No implicit major, minor and patch.')

  call v1%parse('.', e, strict_mode=.false.)
  if (allocated(e)) call fail('No strict mode: Has implicit major, minor and patch.')

  call v1%parse('1.2', e, strict_mode=.false.)
  if (allocated(e)) call fail('No strict mode: Has implicit major, minor and patch.')

  if (is_version('1', strict_mode=.true.)) call fail("Strict mode: Missing minor and patch.")
  if (is_version('1+123', strict_mode=.true.)) call fail("Strict mode: Missing minor and patch.")
  if (is_version('1.0+123', strict_mode=.true.)) call fail("Strict mode: Missing patch.")
  if (is_version('1.0', strict_mode=.true.)) call fail("Strict mode: Missing patch.")
  if (.not. is_version('1.0.0+123', strict_mode=.true.)) call fail("Strict mode: Is valid version.")
  if (.not. is_version('1.0.0+123', strict_mode=.false.)) call fail("No strict mode: Is valid version.")
  if (.not. is_version('11.0', strict_mode=.false.)) call fail("No strict mode: Is valid version.")

!################################operator_index################################!

  if (operator_index('') /= 0) call fail('op-index-1 failed.')
  if (operator_index(' ') /= 1) call fail('op-index-2 failed.')
  if (operator_index('      ') /= 1) call fail('op-index-3 failed.')
  if (operator_index('<') /= 1) call fail('op-index-4 failed.')
  if (operator_index('>') /= 1) call fail('op-index-5 failed.')
  if (operator_index('!') /= 1) call fail('op-index-6 failed.')
  if (operator_index('=') /= 1) call fail('op-index-7 failed.')
  if (operator_index('asdfj76r58>') /= 11) call fail('op-index-8 failed.')
  if (operator_index('asdfj76r58 ') /= 11) call fail('op-index-8 failed.')
  if (operator_index('asdfj76r58=z73z242   ') /= 11) call fail('op-index-8 failed.')
  if (operator_index('asdfj76r58 z73z242   ') /= 11) call fail('op-index-9 failed.')
  if (operator_index('2.3.4 > 9.2.4') /= 6) call fail('op-index-10 failed.')
  if (operator_index('2.3.4 > 9.2.4 <38 >>= 8') /= 6) call fail('op-index-11 failed.')

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
  if (allocated(e)) print *, e%msg
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

!###################################satisfies##################################!

  v1 = version_t(0, 1, 0)
  if (v1%satisfies('  ')) call fail('satisfies-1 should fail.')
  if (v1%satisfies('abc')) call fail('satisfies-2 should fail.')
  if (.not. v1%satisfies('0.1.0')) call fail('satisfies-3 should not fail.')
  if (.not. v1%satisfies('>=0.1.0')) call fail('satisfies-4 should not fail.')
  if (v1%satisfies('>0.1.0')) call fail('satisfies-5 should fail.')
  if (v1%satisfies('>=0.1.0-....')) call fail('satisfies-6 should fail.')
  if (.not. v1%satisfies('>0.0.99 <1.0.0')) call fail('satisfies-7 should not fail.')
  if (.not. v1%satisfies('>0.0.99 <0.1.0 || 0.1.0')) call fail('satisfies-8 should not fail.')
  if (v1%satisfies('>0.0.99 <0.1.0 || >0.1.0')) call fail('satisfies-9 should fail.')
  if (.not. v1%satisfies('<0.0.1 || >0.1.0-123')) call fail('satisfies-10 should not fail.')

!################################satisfies_comp################################!

  v1 = version_t(0, 1, 0)
  call v1%satisfies_comp(comparator_t('abc', version_t(1)), is_satisfied, e)
  if (.not. allocated(e)) call fail('try_satisfy-comp-1 should fail.')

  call v1%satisfies_comp(comparator_t('=', version_t(1)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-2 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-2 should not be true.')
  call v1%satisfies_comp(comparator_t('', version_t(1)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-3 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-3 should not be true.')
  call v1%satisfies_comp(comparator_t('!=', version_t(1)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-4 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-4 should be true.')
  call v1%satisfies_comp(comparator_t('>', version_t(1)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-5 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-5 should not be true.')
  call v1%satisfies_comp(comparator_t('>=', version_t(1)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-6 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-6 should not be true.')
  call v1%satisfies_comp(comparator_t('<', version_t(1)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-7 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-7 should be true.')
  call v1%satisfies_comp(comparator_t('<=', version_t(1)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-8 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-8 should be true.')

  call v1%satisfies_comp(comparator_t('=', version_t(0, 0, 9)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-9 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-9 should not be true.')
  call v1%satisfies_comp(comparator_t('', version_t(0, 0, 9)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-10 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-10 should not be true.')
  call v1%satisfies_comp(comparator_t('!=', version_t(0, 0, 9)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-11 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-11 should be true.')
  call v1%satisfies_comp(comparator_t('>', version_t(0, 0, 9)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-12 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-12 should be true.')
  call v1%satisfies_comp(comparator_t('>=', version_t(0, 0, 9)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-13 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-13 should be true.')
  call v1%satisfies_comp(comparator_t('<', version_t(0, 0, 9)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-14 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-14 should not be true.')
  call v1%satisfies_comp(comparator_t('<=', version_t(0, 0, 9)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-15 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-15 should not be true.')

  call v1%satisfies_comp(comparator_t('=', version_t(0, 1, 0)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-16 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-16 should be true.')
  call v1%satisfies_comp(comparator_t('', version_t(0, 1, 0)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-17 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-17 should be true.')
  call v1%satisfies_comp(comparator_t('!=', version_t(0, 1, 0)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-18 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-18 should not be true.')
  call v1%satisfies_comp(comparator_t('>', version_t(0, 1, 0)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-19 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-19 should not be true.')
  call v1%satisfies_comp(comparator_t('>=', version_t(0, 1, 0)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-20 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-20 should be true.')
  call v1%satisfies_comp(comparator_t('<', version_t(0, 1, 0)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-21 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-21 should not be true.')
  call v1%satisfies_comp(comparator_t('<=', version_t(0, 1, 0)), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-22 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-22 should be true.')

  call v1%satisfies_comp(comparator_t('=', version_t(0, 1, 0, 'abc')), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-23 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-23 should not be true.')
  call v1%satisfies_comp(comparator_t('', version_t(0, 1, 0, 'abc')), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-24 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-24 should not be true.')
  call v1%satisfies_comp(comparator_t('!=', version_t(0, 1, 0, 'abc')), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-25 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-25 should be true.')
  call v1%satisfies_comp(comparator_t('>', version_t(0, 1, 0, 'abc')), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-26 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-26 should be true.')
  call v1%satisfies_comp(comparator_t('>=', version_t(0, 1, 0, 'abc')), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-27 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-27 should be true.')
  call v1%satisfies_comp(comparator_t('<', version_t(0, 1, 0, 'abc')), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-28 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-28 should not be true.')
  call v1%satisfies_comp(comparator_t('<=', version_t(0, 1, 0, 'abc')), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-29 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-29 should not be true.')

  call v1%satisfies_comp(comparator_t('=', version_t(0, 1, 0, build='1')), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-30 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-30 should be true.')
  call v1%satisfies_comp(comparator_t('', version_t(0, 1, 0, build='1')), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-31 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-31 should be true.')
  call v1%satisfies_comp(comparator_t('!=', version_t(0, 1, 0, build='1')), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-32 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-32 should not be true.')
  call v1%satisfies_comp(comparator_t('>', version_t(0, 1, 0, build='1')), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-33 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-33 should not be true.')
  call v1%satisfies_comp(comparator_t('>=', version_t(0, 1, 0, build='1')), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-34 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-34 should be true.')
  call v1%satisfies_comp(comparator_t('<', version_t(0, 1, 0, build='1')), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-29 should not fail.')
  if (is_satisfied) call fail('try_satisfy-comp-29 should not be true.')
  call v1%satisfies_comp(comparator_t('<=', version_t(0, 1, 0, build='1')), is_satisfied, e)
  if (allocated(e)) call fail('try_satisfy-comp-30 should not fail.')
  if (.not. is_satisfied) call fail('try_satisfy-comp-30 should be true.')

!##############################satisfies_comp_set##############################!

  v1 = version_t(0, 1, 0)

  if (allocated(comps)) deallocate (comps)
  allocate (comps(0))
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (.not. allocated(e)) call fail('try_satisfy-comp-set-1 should fail.')

  comps = [comparator_t('=', version_t(0, 1, 0))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (.not. is_satisfied) call fail('try_satisfy-comp-set-2 should satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-2 should not fail.')
  comps = [comparator_t('', version_t(0, 1, 0))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (.not. is_satisfied) call fail('try_satisfy-comp-set-2 should satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-2 should not fail.')
  comps = [comparator_t('!=', version_t(0, 1, 0))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (is_satisfied) call fail('try_satisfy-comp-set-3 should not satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-3 should not fail.')
  comps = [comparator_t('>', version_t(0, 1, 0))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (is_satisfied) call fail('try_satisfy-comp-set-4 should not satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-4 should not fail.')
  comps = [comparator_t('>=', version_t(0, 1, 0))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (.not. is_satisfied) call fail('try_satisfy-comp-set-5 should satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-5 should not fail.')
  comps = [comparator_t('<', version_t(0, 1, 0))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (is_satisfied) call fail('try_satisfy-comp-set-6 should not satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-6 should not fail.')
  comps = [comparator_t('<=', version_t(0, 1, 0))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (.not. is_satisfied) call fail('try_satisfy-comp-set-7 should satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-7 should not fail.')

  comps = [comparator_t('=', version_t(1))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (is_satisfied) call fail('try_satisfy-comp-set-8 should not satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-8 should not fail.')
  comps = [comparator_t('', version_t(1))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (is_satisfied) call fail('try_satisfy-comp-set-8 should not satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-8 should not fail.')
  comps = [comparator_t('!=', version_t(1))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (.not. is_satisfied) call fail('try_satisfy-comp-set-9 should satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-9 should not fail.')
  comps = [comparator_t('>', version_t(1))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (is_satisfied) call fail('try_satisfy-comp-set-10 should not satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-10 should not fail.')
  comps = [comparator_t('>=', version_t(1))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (is_satisfied) call fail('try_satisfy-comp-set-11 should not satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-11 should not fail.')
  comps = [comparator_t('<', version_t(1))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (.not. is_satisfied) call fail('try_satisfy-comp-set-12 should satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-12 should not fail.')
  comps = [comparator_t('<=', version_t(1))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (.not. is_satisfied) call fail('try_satisfy-comp-set-13 should satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-13 should not fail.')

  comps = [comparator_t('!=', version_t(1)), comparator_t('>', version_t(0, 9))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (is_satisfied) call fail('try_satisfy-comp-set-14 should not satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-14 should not fail.')

  comps = [comparator_t('=', version_t(1)), comparator_t('<', version_t(0, 9))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (is_satisfied) call fail('try_satisfy-comp-set-15 should not satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-15 should not fail.')

  comps = [comparator_t('', version_t(1)), comparator_t('>', version_t(0, 9))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (is_satisfied) call fail('try_satisfy-comp-set-16 should not satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-16 should not fail.')

  comps = [comparator_t('!=', version_t(1)), comparator_t('<', version_t(0, 9))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (.not. is_satisfied) call fail('try_satisfy-comp-set-17 should satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-17 should not fail.')

  comps = [comparator_t('', version_t(1)), comparator_t('', version_t(2))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (is_satisfied) call fail('try_satisfy-comp-set-18 should not satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-18 should not fail.')

  comps = [comparator_t('', version_t(0, 1)), comparator_t('', version_t(1))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (is_satisfied) call fail('try_satisfy-comp-set-19 should not satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-19 should not fail.')

  comps = [comparator_t('', version_t(1)), comparator_t('', version_t(0, 1))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (is_satisfied) call fail('try_satisfy-comp-set-20 should not satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-20 should not fail.')

  comps = [comparator_t('!=', version_t(1)), comparator_t('', version_t(0, 1))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (.not. is_satisfied) call fail('try_satisfy-comp-set-21 should satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-21 should not fail.')

  comps = [comparator_t('<', version_t(0, 1)), comparator_t('', version_t(0, 1)), comparator_t('>', version_t(2))]
  call v1%satisfies_comp_set(comparator_set_t(comps), is_satisfied, e)
  if (is_satisfied) call fail('try_satisfy-comp-set-22 should not satisfy.')
  if (allocated(e)) call fail('try_satisfy-comp-set-22 should not fail.')

!################################parse_comp_set################################!

  call comp_set%parse('abc', e)
  if (.not. allocated(e)) call fail('parse-comp-set-1 should fail.')

  call comp_set%parse('>0.0.1', e)
  if (comp_set%comps(1)%op /= '>') call fail("parse-comp-set-2: Wrong operator parsed.")
  if (comp_set%comps(1)%version /= version_t(0, 0, 1)) call fail('parse-comp-set-2: Version does not match.')
  if (allocated(e)) call fail('parse-comp-set-2 should not fail.')

  call comp_set%parse('> 0.0.1', e)
  if (comp_set%comps(1)%op /= '>') call fail("parse-comp-set-3: Wrong operator parsed.")
  if (comp_set%comps(1)%version /= version_t(0, 0, 1)) call fail('parse-comp-set-3: Version does not match.')
  if (allocated(e)) call fail('parse-comp-set-3 should not fail.')

  call comp_set%parse('1', e)
  if (comp_set%comps(1)%op /= '') call fail("parse-comp-set-4: Wrong operator parsed.")
  if (comp_set%comps(1)%version /= version_t(1)) call fail('parse-comp-set-4: Version does not match.')
  if (allocated(e)) call fail('parse-comp-set-4 should not fail.')

  call comp_set%parse('1.0.0 1.0.1', e)
  if (comp_set%comps(1)%op /= '') call fail("parse-comp-set-5: Wrong operator parsed.")
  if (comp_set%comps(1)%version /= version_t(1)) call fail('parse-comp-set-5: Version does not match.')
  if (comp_set%comps(2)%op /= '') call fail("parse-comp-set-5: Wrong operator parsed.")
  if (comp_set%comps(2)%version /= version_t(1, 0, 1)) call fail('parse-comp-set-5: Version does not match.')
  if (allocated(e)) call fail('parse-comp-set-5 should not fail.')

  call comp_set%parse('=1.0.0 !=1.0.1', e)
  if (comp_set%comps(1)%op /= '=') call fail("parse-comp-set-6: Wrong operator parsed.")
  if (comp_set%comps(1)%version /= version_t(1)) call fail('parse-comp-set-6: Version does not match.')
  if (comp_set%comps(2)%op /= '!=') call fail("parse-comp-set-6: Wrong operator parsed.")
  if (comp_set%comps(2)%version /= version_t(1, 0, 1)) call fail('parse-comp-set-6: Version does not match.')
  if (allocated(e)) call fail('parse-comp-set-6 should not fail.')

  call comp_set%parse('<  1.0.0 > 1.0.1 =2', e)
  if (size(comp_set%comps) /= 3) call fail("parse-comp-set-7: Wrong number of comparators.")
  if (comp_set%comps(1)%op /= '<') call fail("parse-comp-set-7: Wrong operator parsed.")
  if (comp_set%comps(1)%version /= version_t(1)) call fail('parse-comp-set-7: Version does not match.')
  if (comp_set%comps(2)%op /= '>') call fail("parse-comp-set-7: Wrong operator parsed.")
  if (comp_set%comps(2)%version /= version_t(1, 0, 1)) call fail('parse-comp-set-7: Version does not match.')
  if (comp_set%comps(3)%op /= '=') call fail("parse-comp-set-7: Wrong operator parsed.")
  if (comp_set%comps(3)%version /= version_t(2)) call fail('parse-comp-set-7: Version does not match.')
  if (allocated(e)) call fail('parse-comp-set-7 should not fail.')

!##############################parse_version_range#############################!

  call range%parse('', e)
  if (.not. allocated(e)) call fail('parse-version-range-1 should fail.')

  call range%parse('abc', e)
  if (.not. allocated(e)) call fail('parse-version-range-2 should fail.')

  call range%parse('abc||abc', e)
  if (.not. allocated(e)) call fail('parse-version-range-3 should fail.')

  call range%parse('abc||', e)
  if (.not. allocated(e)) call fail('parse-version-range-4 should fail.')

  call range%parse('||abc', e)
  if (.not. allocated(e)) call fail('parse-version-range-5 should fail.')

  call range%parse('||', e)
  if (.not. allocated(e)) call fail('parse-version-range-6 should fail.')

  call range%parse('0.2.0', e)
  if (size(range%comp_sets) /= 1) call fail('parse-version-range-7 has wrong number of sets.')
  if (range%comp_sets(1)%comps(1)%op /= '') call fail('parse-version-range-7: Wrong operator.')
  if (range%comp_sets(1)%comps(1)%version /= version_t(0, 2)) call fail('parse-version-range-7: Wrong version.')
  if (allocated(e)) call fail('parse-version-range-7 should not fail.')

  call range%parse('0.2.0 || 0.3.0', e)
  if (size(range%comp_sets) /= 2) call fail('parse-version-range-8 has wrong number of sets.')
  if (range%comp_sets(1)%comps(1)%op /= '') call fail('parse-version-range-8: Wrong operator.')
  if (range%comp_sets(1)%comps(1)%version /= version_t(0, 2)) call fail('parse-version-range-8: Wrong version.')
  if (range%comp_sets(2)%comps(1)%op /= '') call fail('parse-version-range-8: Wrong operator.')
  if (range%comp_sets(2)%comps(1)%version /= version_t(0, 3)) call fail('parse-version-range-8: Wrong version.')
  if (allocated(e)) call fail('parse-version-range-8 should not fail.')

  call range%parse('0.2.0 || <0.3.0 || >= 0.4.0', e)
  if (size(range%comp_sets) /= 3) call fail('parse-version-range-9 has wrong number of sets.')
  if (range%comp_sets(1)%comps(1)%op /= '') call fail('parse-version-range-9 has parsed the wrong operator.')
  if (range%comp_sets(1)%comps(1)%version /= version_t(0, 2)) call fail('parse-version-range-9 has parsed the wrong version.')
  if (range%comp_sets(2)%comps(1)%op /= '<') call fail('parse-version-range-9 has parsed the wrong operator.')
  if (range%comp_sets(2)%comps(1)%version /= version_t(0, 3)) call fail('parse-version-range-9 has parsed the wrong version.')
  if (range%comp_sets(3)%comps(1)%op /= '>=') call fail('parse-version-range-9 has parsed the wrong operator.')
  if (range%comp_sets(3)%comps(1)%version /= version_t(0, 4)) call fail('parse-version-range-9 has parsed the wrong version.')
  if (allocated(e)) call fail('parse-version-range-9 should not fail.')

  call range%parse('0.2.0 <0.3.0 || >= 0.4.0 !=0.4.1 =0.5.0 || 0.6.0', e)
  if (size(range%comp_sets) /= 3) call fail('parse-version-range-10 has wrong number of sets.')
  if (range%comp_sets(1)%comps(1)%op /= '') call fail('parse-version-range-10: Wrong operator.')
  if (range%comp_sets(1)%comps(1)%version /= version_t(0, 2)) call fail('parse-version-range-10: Wrong version.')
  if (range%comp_sets(1)%comps(2)%op /= '<') call fail('parse-version-range-10: Wrong operator.')
  if (range%comp_sets(1)%comps(2)%version /= version_t(0, 3)) call fail('parse-version-range-10: Wrong version.')
  if (range%comp_sets(2)%comps(1)%op /= '>=') call fail('parse-version-range-10: Wrong operator.')
  if (range%comp_sets(2)%comps(1)%version /= version_t(0, 4)) call fail('parse-version-range-10: Wrong version.')
  if (range%comp_sets(2)%comps(2)%op /= '!=') call fail('parse-version-range-10: Wrong operator.')
  if (range%comp_sets(2)%comps(2)%version /= version_t(0, 4, 1)) call fail('parse-version-range-10: Wrong version.')
  if (range%comp_sets(2)%comps(3)%op /= '=') call fail('parse-version-range-10: Wrong operator.')
  if (range%comp_sets(2)%comps(3)%version /= version_t(0, 5)) call fail('parse-version-range-10: Wrong version.')
  if (range%comp_sets(3)%comps(1)%op /= '') call fail('parse-version-range-10: Wrong operator.')
  if (range%comp_sets(3)%comps(1)%version /= version_t(0, 6)) call fail('parse-version-range-10: Wrong version.')
  if (allocated(e)) call fail('parse-version-range-10 should not fail.')

  call range%parse('0.2.0 0.2.1 0.3.0', e)
  if (size(range%comp_sets) /= 1) call fail('parse-version-range-11 has wrong number of sets.')
  if (range%comp_sets(1)%comps(1)%op /= '') call fail('parse-version-range-11: Wrong operator.')
  if (range%comp_sets(1)%comps(1)%version /= version_t(0, 2)) call fail('parse-version-range-11: Wrong version.')
  if (range%comp_sets(1)%comps(2)%op /= '') call fail('parse-version-range-11: Wrong operator.')
  if (range%comp_sets(1)%comps(2)%version /= version_t(0, 2, 1)) call fail('parse-version-range-11: Wrong version.')
  if (range%comp_sets(1)%comps(3)%op /= '') call fail('parse-version-range-11: Wrong operator.')
  if (range%comp_sets(1)%comps(3)%version /= version_t(0, 3)) call fail('parse-version-range-11: Wrong version.')
  if (allocated(e)) call fail('parse-version-range-11 should not fail.')

!###################################is_stable##################################!

  v1 = version_t(0, 9, 99)
  if (v1%is_stable()) call fail('is_stable-1 should not be stable')
  v1 = version_t(1, 0, 0)
  if (.not. v1%is_stable()) call fail('is_stable-2 should be stable')
  v1 = version_t(1, 0, 0, 'alpha')
  if (v1%is_stable()) call fail('is_stable-3 should not be stable')
  v1 = version_t(0, 0, 1, 'alpha')
  if (v1%is_stable()) call fail('is_stable-4 should not be stable')
  v1 = version_t(0, 0, 1, build='alpha')
  if (v1%is_stable()) call fail('is_stable-5 should not be stable')
  v1 = version_t(1, 0, 1, build='alpha')
  if (.not. v1%is_stable()) call fail('is_stable-6 should be stable')

!#################################final_message################################!

  print *, achar(10)//achar(27)//'[92m All tests passed.'//achar(27)

contains

  subroutine fail(msg)
    character(*), intent(in) :: msg
    print *, achar(27)//'[31m'//'Test failed: '//msg//achar(27)//'[0m'
    stop 1
  end
end
