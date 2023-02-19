program check
  use version_f, only: version_t, error_t
  implicit none

  type(version_t) :: v1, v2
  type(error_t), allocatable :: e

!################################### Create ###################################!

  v1 = version_t(0, 0, 0)
  if (v1%to_string() /= '0.0.') then
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

  call v1%parse('1-irh+.ife..oihie.', e)
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

contains

  subroutine fail(msg)
    character(len=*), intent(in) :: msg
    print *, achar(27)//'[31m'//'Test failed: '//msg//achar(27)//'[0m'
    stop 1
  end
end
