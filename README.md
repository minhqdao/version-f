# version-f

This package provides a Fortran implementation of [Semantic Versioning 2.0.0](https://semver.org). It aims to be a user-friendly tool for handling versions in your Fortran projects.

It follows the `major`.`minor`.`patch` pattern and allows the inclusion of `prerelease` and `build` metadata.

## Installation

### fpm

If you are using [fpm](https://fpm.fortran-lang.org/en/index.html), you can
simply add this package as a dependency to your `fpm.toml` file:

```toml
[dependencies]

[dependencies.version-f]
git = "https://github.com/minhqdao/version-f.git"
tag = "v0.1.0"
```

Then import the `version_f` module into your Fortran code:

```fortran
use version_f, only: version_t, error_t
```

## Create versions

```fortran
type(version_t) :: version
type(error_t), allocatable :: error

! Default
version = version_t(0, 1, 0)

! From string
version = version_t('0.1.0')

! Default with external error handling
call version%create(0, 1, 0, error=error)

! From string with external error handling
call version%parse('0.1.0', error)

! Create version with prerelease and build metadata.
call version%create(0, 1, 0, 'alpha', '1' error)

! Parse string with prerelease and build metadata.
call version%parse('0.1.0-alpha+1', error)
```

## Compare versions
```fortran
type(version_t) :: v1, v2

v1 = version_t(0, 1, 0)
v2 = version_t(1, 0, 0)

if (v1 < v2) then ! true
if (v1 <= v2) then ! true
if (v1 > v2) then ! false
if (v1 <= v2) then ! false
if (v1 == v2) then ! false
if (v1 /= v2) then ! true
```

## Increment versions

```fortran
type(version_t) :: version

version = version_t(0, 5, 3)

call version%increment_patch() ! 0.5.4
call version%increment_minor() ! 0.6.0
call version%increment_major() ! 1.0.0
```

## Include prerelase

`prerelease` metadata can be included and will be appended after the `patch` via a `-` sign. The identifiers must comprise only ASCII alphanumerics and hyphens `[0-9A-Za-z-]` and are separated by dots. Numerical identifiers must not start with a `0` digit. A version containing `prerelease` data has lower precedence than the equivalent version without. `prerelease` information is cleared each time the version is incremented.

```fortran
type(version_t) :: v1, v2

v1 = version_t(0, 5, 3, 'beta.1')
v1%to_string() ! '0.5.3-beta.1'

v2 = version_t(0, 5, 3)
v2%to_string() ! '0.5.3'

v1 < v2 ! true
v1 == v2 ! false

v1%increment_patch() ! 0.5.4
```

## Include build

`build` metadata can be included and will be appended after the `patch` or the `prerelease` via a `+` sign. The identifiers must comprise only ASCII alphanumerics and hyphens `[0-9A-Za-z-]` and are separated by dots. Numerical identifiers must not start with a `0` digit. The `build` data is not used for comparison and it is cleared each time the version is incremented.

```fortran
type(version_t) :: v1, v2

v1 = version_t(0, 5, 3, build='1')
v1%to_string() ! '0.5.3+1'

v2 = version_t(0, 5, 3, build='abc.1-13')
v2%to_string() ! '0.5.3+abc.1-13'

v1 == v2 ! true

v1%increment_patch() ! 0.5.4

v1 = version_t(0, 5, 3, 'alpha.1' '1')
v1%to_string() ! '0.5.3-alpha.1+1'
```

## More examples

```fortran
type(version_t) :: version
character(len=:), allocatable :: string

version = version_t(0) ! 0.0.0
version = version_t(1) ! 1.0.0
version = version_t(3, 2) ! 3.2.0
version = version_t('4.1') ! 4.1.0
version = version_t('.5.') ! 0.5.0
version = version_t('..1') ! 0.0.1
string = version%to_string() ! '4.1.0'
```
There is also a full example in the [example]() folder. Run with:

```bash
fpm run --example
```

## Tests

Run tests with:

```bash
fpm test
```

## Contribute

Feel free to [create an issue]() in case you found a bug, have any questions or
want to propose further improvements. Please stick to the existing coding style
when you open a pull request.

## License

This is free software. You can use, redistribute and/or modify it under the terms of the [Apache License, Version 2.0]().