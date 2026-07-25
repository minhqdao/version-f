program test
  use, intrinsic :: iso_fortran_env, only: int64
  use version_f, only: error_t, is_version, version_range_t, version_t

  implicit none(type, external)

  type(version_t) :: v1, v2
  logical :: is_satisfied
  type(version_range_t) :: range
  type(error_t), allocatable :: e
  character(:), allocatable :: long_input, fuzz_input, canonical
  character(32) :: huge_str
  integer(int64) :: rng_state = 20260726_int64
  integer :: i

!################################### Create ###################################!

  if (v1%to_string() /= '0.0.0') call fail('Default version should be initialized to 0.0.0')
  if (v1%major() /= 0) call fail('Default major version should be zero')
  if (v1%minor() /= 0) call fail('Default minor version should be zero')
  if (v1%patch() /= 0) call fail('Default patch version should be zero')
  if (v1%prerelease() /= '') call fail('Default version should not have prerelease identifiers')
  if (v1%build() /= '') call fail('Default version should not have build identifiers')

  v1 = version_t(0, 0, 0)
  if (v1%to_string() /= '0.0.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 2, 3)
  if (v1%major() /= 1) call fail('Major accessor returned the wrong value')
  if (v1%minor() /= 2) call fail('Minor accessor returned the wrong value')
  if (v1%patch() /= 3) call fail('Patch accessor returned the wrong value')
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
  if (v1%prerelease() /= 'abc') call fail('Prerelease accessor returned wrong data')
  if (v1%build() /= '789') call fail('Build accessor returned wrong data')
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
  if (allocated(e)) call fail('Numeric build identifier with leading zeroes should be valid.')
  if (v1%to_string() /= '1.0.0-abc.ded+05567.abc') call fail('Build identifier with leading zeroes changed.')

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

  v1 = version_t(1, 0, 0, 'RC-X', 'build-X')
  if (v1%to_string() /= '1.0.0-RC-X+build-X') then
    call fail("Uppercase X failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 0, 0, 'X', 'X')
  if (v1%to_string() /= '1.0.0-X+X') then
    call fail("Single uppercase X failed for '"//v1%to_string()//"'")
  end if

  v1 = version_t(1, 0, 0, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ')
  if (v1%to_string() /= '1.0.0-ABCDEFGHIJKLMNOPQRSTUVWXYZ') then
    call fail("Full uppercase alphabet failed for '"//v1%to_string()//"'")
  end if

  call v1%create(1, prerelease='d', build='9.0', error=e)
  if (allocated(e)) call fail('Zero build identifier should be valid.')

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

  v1 = version_t(1, 2, 3, 'alpha', 'build')
  call v1%try_increment_patch(e)
  if (allocated(e)) call fail('try_increment_patch should not report an error')
  if (v1%to_string() /= '1.2.4') call fail('try_increment_patch failed')

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
    call fail(e%message())
  else if (v1%to_string() /= '0.0.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('.', e)
  if (allocated(e)) then
    call fail(e%message())
  else if (v1%to_string() /= '0.0.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('0.1', e)
  if (allocated(e)) then
    call fail(e%message())
  else if (v1%to_string() /= '0.1.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('..988', e)
  if (allocated(e)) then
    call fail(e%message())
  else if (v1%to_string() /= '0.0.988') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1..988', e)
  if (allocated(e)) then
    call fail(e%message())
  else if (v1%to_string() /= '1.0.988') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('.1.', e)
  if (allocated(e)) then
    call fail(e%message())
  else if (v1%to_string() /= '0.1.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('..', e)
  if (allocated(e)) then
    call fail(e%message())
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
    call fail(e%message())
  else if (v1%to_string() /= '1.0.0-1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('8.1-1', e)
  if (allocated(e)) then
    call fail(e%message())
  else if (v1%to_string() /= '8.1.0-1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1-1.1', e)
  if (allocated(e)) then
    call fail(e%message())
  else if (v1%to_string() /= '1.0.0-1.1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('8.1-1.9-9--.2', e)
  if (allocated(e)) then
    call fail(e%message())
  else if (v1%to_string() /= '8.1.0-1.9-9--.2') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1+1', e)
  if (allocated(e)) then
    call fail(e%message())
  else if (v1%to_string() /= '1.0.0+1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1+1.1-0P.2', e)
  if (allocated(e)) then
    call fail(e%message())
  else if (v1%to_string() /= '1.0.0+1.1-0P.2') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1+f-1', e)
  if (allocated(e)) then
    call fail(e%message())
  else if (v1%to_string() /= '1.0.0+f-1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1-23+1-1', e)
  if (allocated(e)) then
    call fail(e%message())
  else if (v1%to_string() /= '1.0.0-23+1-1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1.0.1-43.fs23+1-1', e)
  if (allocated(e)) then
    call fail(e%message())
  else if (v1%to_string() /= '1.0.1-43.fs23+1-1') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('1-0', e)
  if (allocated(e)) call fail('A single zero prerelease identifier should be valid.')

  call v1%parse('1-hff.08', e)
  if (.not. allocated(e)) call fail('Leading zero identifier not allowed.')

  call v1%parse('1-hff.87+08', e)
  if (allocated(e)) call fail('Build identifier with leading zeroes should be valid.')
  if (v1%to_string() /= '1.0.0-hff.87+08') call fail('Build identifier with leading zeroes changed.')

  call v1%parse('1-hff.87+fejf.08', e)
  if (allocated(e)) call fail('Build identifier with leading zeroes should be valid.')
  if (v1%to_string() /= '1.0.0-hff.87+fejf.08') call fail('Build identifier with leading zeroes changed.')

  call v1%parse('1-..', e)
  if (.not. allocated(e)) call fail('No consecutive dots.')

  call v1%parse('1-irhife..oihie', e)
  if (.not. allocated(e)) call fail('No consecutive dots.')

  call v1%parse('1-irh+ife..oihie.', e)
  if (.not. allocated(e)) call fail('Trailing dot.')

  call v1%parse('1-irh+.ife..oihie', e)
  if (.not. allocated(e)) call fail('Leading dot.')

  call v1%parse('1.0.0-RC-X+build-X', e)
  if (allocated(e)) then
    call fail(e%message())
  else if (v1%to_string() /= '1.0.0-RC-X+build-X') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

!################################## Compare ###################################!

  call v1%parse('99999999999.0.0', e)
  if (.not. allocated(e)) call fail('Overflow in major version not caught.')

  call v1%parse('0.99999999999.0', e)
  if (.not. allocated(e)) call fail('Overflow in minor version not caught.')

  call v1%parse('0.0.99999999999', e)
  if (.not. allocated(e)) call fail('Overflow in patch version not caught.')

  v1 = version_t(2147483647, 0, 0)
  if (v1%to_string() /= '2147483647.0.0') then
    call fail("Parsing failed for '"//v1%to_string()//"'")
  end if

  call v1%parse('2147483648.0.0', e)
  if (.not. allocated(e)) call fail('Overflow at huge(0)+1 not caught.')

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
  if (v1 /= v2) call fail('Inequality failed.')
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

  v1 = version_t(1, 0, 0, '99999999999')
  v2 = version_t(1, 0, 0, '99999999998')
  if (.not. v1 > v2) call fail('Overflow in numeric prerelease comparison failed.')

  v1 = version_t(1, 0, 0, '99999999999')
  v2 = version_t(1, 0, 0, '99999999999')
  if (.not. v1 == v2) call fail('Overflow in numeric prerelease equality failed.')

  v1 = version_t(1, 0, 0, '99999999998')
  v2 = version_t(1, 0, 0, '99999999999')
  if (.not. v1 < v2) call fail('Overflow in numeric prerelease less-than failed.')

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
  if (.not. is_version('1.0.0-0')) call fail("'1.0.0-0' is a version.")
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

  if (.not. is_version('1.0.0-RC-X+build-X')) call fail("'1.0.0-RC-X+build-X' is a version.")
  if (.not. is_version('1.0.0-X')) call fail("'1.0.0-X' is a version.")

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

  if (is_version('1', strict_mode=.true.)) call fail('Strict mode: Missing minor and patch.')
  if (is_version('1+123', strict_mode=.true.)) call fail('Strict mode: Missing minor and patch.')
  if (is_version('1.0+123', strict_mode=.true.)) call fail('Strict mode: Missing patch.')
  if (is_version('1.0', strict_mode=.true.)) call fail('Strict mode: Missing patch.')
  if (.not. is_version('1.0.0+123', strict_mode=.true.)) call fail('Strict mode: Is valid version.')
  if (.not. is_version('1.0.0+123', strict_mode=.false.)) call fail('No strict mode: Is valid version.')
  if (.not. is_version('11.0', strict_mode=.false.)) call fail('No strict mode: Is valid version.')

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

!############################ parsed version ranges ############################!

  call range%parse('>=1.0.0 <2.0.0 || =3.0.0', e)
  if (allocated(e)) call fail(e%message())

  call range%try_satisfy(version_t(1, 5, 0), is_satisfied, e)
  if (allocated(e)) call fail(e%message())
  if (.not. is_satisfied) call fail('Parsed range should satisfy 1.5.0')

  if (.not. range%satisfies(version_t(3, 0, 0))) call fail('Parsed range should satisfy 3.0.0')
  if (range%satisfies(version_t(2, 0, 0))) call fail('Parsed range should not satisfy 2.0.0')

  call range%parse('', e)
  if (.not. allocated(e)) call fail('Empty parsed range should report an error')

  block
    type(version_range_t) :: unparsed_range
    call unparsed_range%try_satisfy(version_t(1), is_satisfied, e)
    if (.not. allocated(e)) call fail('Unparsed range should report an error')
    if (unparsed_range%satisfies(version_t(1))) call fail('Unparsed range should not satisfy a version')
  end block

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

!############################ increment_identifier overflow ####################!

  v1 = version_t(huge(0), 2, 3, 'alpha', 'build')
  v2 = v1
  call v1%try_increment_major(e)
  if (.not. allocated(e)) call fail('increment_major overflow should report an error')
  if (.not. v1%is_exactly(v2)) call fail('increment_major overflow should leave the version unchanged')

  v1 = version_t(1, huge(0), 3, 'alpha', 'build')
  v2 = v1
  call v1%try_increment_minor(e)
  if (.not. allocated(e)) call fail('increment_minor overflow should report an error')
  if (.not. v1%is_exactly(v2)) call fail('increment_minor overflow should leave the version unchanged')

  v1 = version_t(1, 2, huge(0), 'alpha', 'build')
  v2 = v1
  call v1%try_increment_patch(e)
  if (.not. allocated(e)) call fail('increment_patch overflow should report an error')
  if (.not. v1%is_exactly(v2)) call fail('increment_patch overflow should leave the version unchanged')

  write (huge_str, '(I0)') huge(0)

  v1 = version_t(0, 0, 0, trim(huge_str), 'build')
  v2 = v1
  call v1%try_increment_prerelease(e)
  if (.not. allocated(e)) call fail('increment_prerelease overflow should report an error')
  if (.not. v1%is_exactly(v2)) call fail('increment_prerelease overflow should leave the version unchanged')

  v1 = version_t(0, 0, 0, build=trim(huge_str))
  v2 = v1
  call v1%try_increment_build(e)
  if (.not. allocated(e)) call fail('increment_build overflow should report an error')
  if (.not. v1%is_exactly(v2)) call fail('increment_build overflow should leave the version unchanged')

  v1 = version_t(0, 0, 0, '99999999999')
  call v1%try_increment_prerelease(e)
  if (.not. allocated(e)) call fail('Out-of-range prerelease increment should report an error')
  if (v1%to_string() /= '0.0.0-99999999999') call fail('increment_prerelease overflow should preserve ids')

  v1 = version_t(0, 0, 0, build='99999999999')
  call v1%try_increment_build(e)
  if (.not. allocated(e)) call fail('Out-of-range build increment should report an error')
  if (v1%to_string() /= '0.0.0+99999999999') call fail('increment_build overflow should keep unchanged')

!###################### prerelease vs no-prerelease (same m.m.p) #############!

  v1 = version_t(1, 0, 0)
  v2 = version_t(1, 0, 0, 'alpha')
  if (v1 == v2) call fail('1.0.0 should not equal 1.0.0-alpha')
  if (.not. v1 /= v2) call fail('Inequality failed for 1.0.0 vs 1.0.0-alpha')
  if (v1 < v2) call fail('1.0.0 should not be less than 1.0.0-alpha')
  if (.not. v1 > v2) call fail('1.0.0 should be greater than 1.0.0-alpha')
  if (v1 <= v2) call fail('Less than or equal failed for 1.0.0 vs 1.0.0-alpha')
  if (.not. v1 >= v2) call fail('Greater than or equal failed for 1.0.0 vs 1.0.0-alpha')

  v1 = version_t(1, 0, 0, 'alpha')
  v2 = version_t(1, 0, 0)
  if (v1 == v2) call fail('1.0.0-alpha should not equal 1.0.0')
  if (.not. v1 /= v2) call fail('Inequality failed for 1.0.0-alpha vs 1.0.0')
  if (.not. v1 < v2) call fail('1.0.0-alpha should be less than 1.0.0')
  if (v1 > v2) call fail('1.0.0-alpha should not be greater than 1.0.0')
  if (.not. v1 <= v2) call fail('Less than or equal failed for 1.0.0-alpha vs 1.0.0')
  if (v1 >= v2) call fail('Greater than or equal failed for 1.0.0-alpha vs 1.0.0')

  v1 = version_t(1, 0, 0, build='abc')
  v2 = version_t(1, 0, 0, 'alpha')
  if (v1 == v2) call fail('1.0.0+abc should not equal 1.0.0-alpha')
  if (.not. v1 /= v2) call fail('Inequality failed for 1.0.0+abc vs 1.0.0-alpha')
  if (v1 < v2) call fail('1.0.0+abc should not be less than 1.0.0-alpha')
  if (.not. v1 > v2) call fail('1.0.0+abc should be greater than 1.0.0-alpha')
  if (.not. v1 >= v2) call fail('Greater than or equal failed for 1.0.0+abc vs 1.0.0-alpha')
  if (v1 <= v2) call fail('Less than or equal failed for 1.0.0+abc vs 1.0.0-alpha')

  v1 = version_t(1, 0, 0, 'alpha')
  v2 = version_t(1, 0, 0, build='abc')
  if (v1 == v2) call fail('1.0.0-alpha should not equal 1.0.0+abc')
  if (.not. v1 /= v2) call fail('Inequality failed for 1.0.0-alpha vs 1.0.0+abc')
  if (.not. v1 < v2) call fail('1.0.0-alpha should be less than 1.0.0+abc')
  if (v1 > v2) call fail('1.0.0-alpha should not be greater than 1.0.0+abc')
  if (.not. v1 <= v2) call fail('Less than or equal failed for 1.0.0-alpha vs 1.0.0+abc')
  if (v1 >= v2) call fail('Greater than or equal failed for 1.0.0-alpha vs 1.0.0+abc')

!###################### to_string edge cases ##################################!

  v1 = version_t(0, 0, 0, 'alpha')
  if (v1%to_string() /= '0.0.0-alpha') call fail('to_string only prerelease')

  v1 = version_t(0, 0, 0, build='build')
  if (v1%to_string() /= '0.0.0+build') call fail('to_string only build')

  v1 = version_t(0, 0, 0, 'a', 'b')
  if (v1%to_string() /= '0.0.0-a+b') call fail('to_string prerelease + build')

!###################### parse with whitespace ################################!

  call v1%parse('  1.0.0  ', e)
  if (allocated(e)) call fail(e%message())
  if (v1%to_string() /= '1.0.0') call fail('parse should trim whitespace')

  call v1%parse('  1.0.0-alpha  ', e)
  if (allocated(e)) call fail(e%message())
  if (v1%to_string() /= '1.0.0-alpha') call fail('parse should trim whitespace with prerelease')

  call v1%parse('  1.0.0-alpha+build  ', e)
  if (allocated(e)) call fail(e%message())
  if (v1%to_string() /= '1.0.0-alpha+build') call fail('parse should trim whitespace with prerelease and build')

!###################### parse strict_mode edge cases ##########################!

  call v1%parse('0.0.0-alpha', e, strict_mode=.true.)
  if (allocated(e)) call fail(e%message())

  call v1%parse('0.0.0-alpha+build', e, strict_mode=.true.)
  if (allocated(e)) call fail(e%message())

  call v1%parse('01.2.3', e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: Leading zero in major version.')

  call v1%parse('1.02.3', e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: Leading zero in minor version.')

  call v1%parse('1.2.03', e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Strict mode: Leading zero in patch version.')

  if (is_version('01.2.3', strict_mode=.true.)) call fail('Strict mode: Leading zero in major version accepted.')
  if (is_version('1.02.3', strict_mode=.true.)) call fail('Strict mode: Leading zero in minor version accepted.')
  if (is_version('1.2.03', strict_mode=.true.)) call fail('Strict mode: Leading zero in patch version accepted.')

  call v1%parse('01.02.03', e, strict_mode=.false.)
  if (allocated(e)) call fail('No strict mode: Leading zeroes should be accepted.')
  if (v1%to_string() /= '1.2.3') call fail('No strict mode: Leading zeroes should be normalized.')

!###################### is_greater overflow fallback ##########################!

  v1 = version_t(0, 0, 0, '99999999999')
  v2 = version_t(0, 0, 0, '99999999998')
  if (.not. v1 > v2) call fail('greater than overflow fallback failed')

  v1 = version_t(0, 0, 0, repeat('9', 100))
  v2 = version_t(0, 0, 0, '1'//repeat('0', 100))
  if (.not. v1 < v2) call fail('Large numeric prerelease identifiers should compare by digit count')

!###################### increment_prerelease with build-only ##################!

  v1 = version_t(1, 0, 0, build='build')
  call v1%increment_prerelease()
  if (v1%to_string() /= '1.0.0-1') call fail('increment_prerelease on build-only should add prerelease')

!############################ adversarial parsing #############################!

  call v1%parse('1.0.0'//achar(9), e)
  if (.not. allocated(e)) call fail('Trailing tab should not be accepted')

  call v1%parse('1.0.0'//achar(10), e)
  if (.not. allocated(e)) call fail('Trailing newline should not be accepted')

  call v1%parse('1.'//achar(9)//'0.0', e)
  if (.not. allocated(e)) call fail('Tab inside a version should not be accepted')

  call v1%parse('1.0.0-alpha'//achar(128), e)
  if (.not. allocated(e)) call fail('Non-ASCII identifier byte should not be accepted')

  long_input = '1.0.0-'//repeat('a', 10000)
  call v1%parse(long_input, e, strict_mode=.true.)
  if (allocated(e)) call fail('Very long valid identifier should be accepted')
  if (v1%to_string() /= long_input) call fail('Very long identifier did not round-trip')

  long_input = repeat('9', 10000)//'.0.0'
  call v1%parse(long_input, e, strict_mode=.true.)
  if (.not. allocated(e)) call fail('Huge major version should report an error')

  call v1%parse('1.0.0-alpha..beta', e)
  if (.not. allocated(e)) call fail('Repeated prerelease separator should not be accepted')

  call v1%parse('1.0.0+build..meta', e)
  if (.not. allocated(e)) call fail('Repeated build separator should not be accepted')

  call v1%parse('1.0.0++build', e)
  if (.not. allocated(e)) call fail('Repeated build marker should not be accepted')

  call v1%try_satisfy('>', is_satisfied, e)
  if (.not. allocated(e)) call fail('Lone greater-than operator should report an error')

  call v1%try_satisfy('<=', is_satisfied, e)
  if (.not. allocated(e)) call fail('Lone less-equals operator should report an error')

  call v1%try_satisfy('!=', is_satisfied, e)
  if (.not. allocated(e)) call fail('Lone not-equals operator should report an error')

  call v1%try_satisfy('!', is_satisfied, e)
  if (.not. allocated(e)) call fail('Lone exclamation mark should report an error')

  call v1%try_satisfy('1.0.0 ||| 2.0.0', is_satisfied, e)
  if (.not. allocated(e)) call fail('Triple OR separator should report an error')

  call v1%try_satisfy('1.0.0 || || 2.0.0', is_satisfied, e)
  if (.not. allocated(e)) call fail('Empty OR branch should report an error')

  call v1%try_satisfy('1.0.0 | 2.0.0', is_satisfied, e)
  if (.not. allocated(e)) call fail('Single OR separator should report an error')

  v1 = version_t(1, 5, 0)
  call v1%try_satisfy('  >=  1.0.0  <  2.0.0  ||  =  3.0.0  ', is_satisfied, e)
  if (allocated(e)) call fail('Whitespace at range token boundaries should be accepted')
  if (.not. is_satisfied) call fail('Whitespace-separated comparator range should be satisfied')

  call v1%try_satisfy('>=1.0.0||<2.0.0', is_satisfied, e)
  if (allocated(e)) call fail('Range separators should not require surrounding whitespace')
  if (.not. is_satisfied) call fail('Compact OR range should be satisfied')

  call v1%try_satisfy('>=1.0.0'//achar(9)//'<2.0.0', is_satisfied, e)
  if (.not. allocated(e)) call fail('Tab-separated comparators should report an error')

  call v1%try_satisfy('>=1.0.0'//achar(10)//'<2.0.0', is_satisfied, e)
  if (.not. allocated(e)) call fail('Newline-separated comparators should report an error')

!######################## property and fuzz testing ###########################!

  long_input = repeat('>=1.0.0 ', 2000)
  call range%parse(long_input, e)
  if (allocated(e)) call fail('Large comparator range should parse')

  long_input = '1.0.0-'//repeat('a.', 1999)//'a'
  call v1%parse(long_input, e, strict_mode=.true.)
  if (allocated(e)) call fail('Version with many identifiers should parse')
  if (v1%to_string() /= long_input) call fail('Version with many identifiers should round-trip')

  do i = 1, 2000
    v1 = random_version()
    call v2%parse(v1%to_string(), e, strict_mode=.true.)
    if (allocated(e)) call fail('parse(to_string(version)) should succeed')
    if (.not. v1%is_exactly(v2)) call fail('parse(to_string(version)) should round-trip exactly')

    v1 = random_version()
    v2 = random_version()
    if ((v1 < v2) .and. (v2 < v1)) call fail('Version ordering should be antisymmetric')
    if ((v1 > v2) .and. (v2 > v1)) call fail('Version ordering should be antisymmetric')
    if ((v1 == v2) .neqv. (.not. (v1 < v2) .and. .not. (v1 > v2))) then
      call fail('Equality should be consistent with ordering')
    end if

    block
      type(version_t) :: v3
      v3 = random_version()
      if ((v1 < v2) .and. (v2 < v3) .and. .not. (v1 < v3)) call fail('Less-than should be transitive')
      if ((v1 > v2) .and. (v2 > v3) .and. .not. (v1 > v3)) call fail('Greater-than should be transitive')
    end block

    v1 = version_t(random_int(20), random_int(20), random_int(20), 'alpha.1', 'build-a')
    v2 = version_t(v1%major(), v1%minor(), v1%patch(), v1%prerelease(), 'build-b.001')
    if (v1 /= v2 .or. v1 < v2 .or. v1 > v2) call fail('Build metadata should not affect precedence')
  end do

  do i = 1, 5000
    fuzz_input = random_text(64)

    call v1%parse(fuzz_input, e)
    if (.not. allocated(e)) then
      canonical = v1%to_string()
      if (.not. is_version(canonical, strict_mode=.true.)) call fail('Parsed version should have canonical output')
    end if

    call v1%parse(fuzz_input, e, strict_mode=.true.)
    if (.not. allocated(e)) then
      canonical = v1%to_string()
      if (.not. is_version(canonical, strict_mode=.true.)) call fail('Strict parse should emit valid SemVer')
      call v2%parse(canonical, e, strict_mode=.true.)
      if (allocated(e)) call fail('Canonical strict output should parse')
      if (.not. v1%is_exactly(v2)) call fail('Canonical strict output should round-trip')
    end if

    if (is_version(fuzz_input)) then
      call v1%parse(fuzz_input, e)
      if (allocated(e)) call fail('is_version and parse should agree')
    end if
    if (is_version(fuzz_input, strict_mode=.true.)) then
      call v1%parse(fuzz_input, e, strict_mode=.true.)
      if (allocated(e)) call fail('Strict is_version and parse should agree')
    end if

    call range%parse(fuzz_input, e)
    if (.not. allocated(e)) then
      v1 = random_version()
      call range%try_satisfy(v1, is_satisfied, e)
      if (allocated(e)) call fail('Successfully parsed range should be safe to evaluate')
    end if
  end do

!#################################final_message################################!

  print *, achar(10)//achar(27)//'[92m All tests passed.'//achar(27)

contains

  integer function random_int(limit)
    integer, intent(in) :: limit

    rng_state = modulo(rng_state*1103515245_int64 + 12345_int64, 2147483648_int64)
    random_int = int(modulo(rng_state, int(limit, int64)))
  end

  function random_version() result(version)
    type(version_t) :: version

    integer :: major, minor, patch

    major = random_int(20)
    minor = random_int(20)
    patch = random_int(20)

    select case (random_int(6))
    case (0)
      version = version_t(major, minor, patch)
    case (1)
      version = version_t(major, minor, patch, 'alpha')
    case (2)
      version = version_t(major, minor, patch, 'alpha.1')
    case (3)
      version = version_t(major, minor, patch, '1.2', 'build.001')
    case (4)
      version = version_t(major, minor, patch, build='sha-1.000')
    case default
      version = version_t(major, minor, patch, 'rc.9', 'linux-x86.42')
    end select
  end

  function random_text(max_length) result(str)
    integer, intent(in) :: max_length
    character(:), allocatable :: str

    character(*), parameter :: parser_chars = &
                               '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.-+<>=!| '
    integer :: j, length

    length = random_int(max_length + 1)
    allocate (character(length) :: str)
    do j = 1, length
      if (random_int(4) == 0) then
        str(j:j) = achar(random_int(256))
      else
        block
          integer :: index
          index = random_int(len(parser_chars)) + 1
          str(j:j) = parser_chars(index:index)
        end block
      end if
    end do
  end

  subroutine fail(msg)
    character(*), intent(in) :: msg
    print *, achar(27)//'[31m'//'Test failed: '//msg//achar(27)//'[0m'
    stop 1
  end
end
