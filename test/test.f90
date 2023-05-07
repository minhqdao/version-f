program test
  use version_f, only: version_t, is_version, error_t
  implicit none

  type(version_t) :: v1, v2
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

contains

  subroutine fail(msg)
    character(*), intent(in) :: msg
    print *, achar(27)//'[31m'//'Test failed: '//msg//achar(27)//'[0m'
    stop 1
  end
end
