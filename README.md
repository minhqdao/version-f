# version-f

This package provides a complete Fortran implementation of
[Semantic Versioning 2.0.0](https://semver.org). It aims to be a user-friendly
tool for handling versions in your Fortran projects.

It follows the `major`.`minor`.`patch` pattern and allows the inclusion of
`prerelease` labels and `build` metadata. Versions can be created or parsed from
strings, compared, incremented and converted back to strings.

## Installation

### fpm

If you are using [fpm](https://fpm.fortran-lang.org/en/index.html), you can
simply add this package as a dependency to your `fpm.toml` file:

```toml
[dependencies]

[dependencies.version-f]
git = "https://github.com/minhqdao/version-f.git"
tag = "v0.3.0"
```

Then import the `version_f` module into your Fortran code:

```fortran
use version_f, only: version_t, error_t
```

Run `fpm build` to download the dependency.

## Create versions

Create versions using one of the following commands:

```fortran
type(version_t) :: version
type(error_t), allocatable :: error

! The default way using individual arguments
version = version_t(0, 1, 0)

! Parse from string
version = version_t('0.1.0')

! From arguments with external error handling
call version%create(0, 1, 0, error=error)

! From string with external error handling
call version%parse('0.1.0', error)

! From arguments with prerelease labels and build metadata
call version%create(0, 1, 0, 'alpha', '1', error)

! From string with prerelease labels and build metadata
call version%parse('0.1.0-alpha+1', error)
```

## Compare versions

Versions can be compared using the standard Fortran operators. Be aware that a version containing `prerelease` labels has lower precedence than the equivalent version without. `build` information isn't used for comparison. However, you can use the [is_exactly()](#is_exactly) function to include it.

```fortran
type(version_t) :: v1, v2, v3, v4

v1 = version_t(0, 1, 0)
v2 = version_t(1, 0, 0)
v3 = version_t(1, 0, 0, 'alpha')
v4 = version_t(1, 0, 0, build='1')

if (v1 < v2) then ! true
if (v1 <= v2) then ! true
if (v1 > v2) then ! false
if (v1 <= v2) then ! false
if (v1 == v2) then ! false
if (v1 /= v2) then ! true

! With prerelease labels
if (v2 == v3) then ! false
if (v2 > v3) then ! true

! With build metadata
if (v2 == v4) then ! true
```

## Increment versions

`Prerelease` and `build` data are cleared each time a major, minor or patch number is incremented. `prerelease` and `build` values are incremented by adding `1` to the their last identifier if it is numeric. If the last identifier isn't a number or no identifiers exist, a new identifier is added with the value of `1`. Existing `build` information is cleared each time a `prerelease` is incremented.

```fortran
type(version_t) :: version

version = version_t(0, 5, 3, 'beta.1', '1')

call version%increment_build() ! 0.5.3-beta.1+2
call version%increment_prerelease() ! 0.5.3-beta.2
call version%increment_patch() ! 0.5.4
call version%increment_minor() ! 0.6.0
call version%increment_major() ! 1.0.0
```

## Convert to string

Versions are converted to strings using the `to_string()` method.

```fortran
type(version_t) :: version

version = version_t(0, 5, 3, 'beta.1', '1-100')

print *, version%to_string() ! '0.5.3-beta.1+1-100'
```

## prerelase labels

`prerelease` labels can be included and will be appended after the `patch` via a `-` sign. The identifiers must comprise only ASCII alphanumerics and hyphens `[0-9A-Za-z-]` and are separated by dots. Numerical identifiers must not start with a `0` digit. A version containing `prerelease` data has lower precedence than the equivalent version without. `prerelease` information is cleared each time the version is incremented. A `prerelease` can be [incremented](#increment-versions).

```fortran
type(version_t) :: v1, v2

v1 = version_t(0, 5, 3, 'beta.1')
print *, v1%to_string() ! '0.5.3-beta.1'

v2 = version_t(0, 5, 3)
print *, v2%to_string() ! '0.5.3'

print *, v1 < v2 ! true
print *, v1 == v2 ! false

call v1%increment_patch() ! 0.5.4
call v1%increment_prerelease() ! 0.5.4-1
call v1%increment_prerelease() ! 0.5.4-2
```

## build metadata

`build` metadata can be included and will be appended after the `patch` or the `prerelease` via a `+` sign. The identifiers must comprise only ASCII alphanumerics and hyphens `[0-9A-Za-z-]` and are separated by dots. Numerical identifiers must not start with a `0` digit. `build` data is not used for comparison and it is cleared each time a `major`, `minor`, `patch` or `prerelease` version is incremented. A `build` value can be [incremented](#increment-versions).

```fortran
type(version_t) :: v1, v2

v1 = version_t(0, 5, 3, build='1')
print *, v1%to_string() ! '0.5.3+1'

v2 = version_t(0, 5, 3, build='abc.1-13')
print *, v2%to_string() ! '0.5.3+abc.1-13'

print *, v1 == v2 ! true

call v1%increment_patch() ! 0.5.4
call v1%increment_build() ! 0.5.4+1

v1 = version_t(0, 5, 3, 'alpha.1' '1')
print *, v1%to_string() ! '0.5.3-alpha.1+1'
print *, v1%increment_build() ! '0.5.3-alpha.1+2'
```

## is_version()

The `is_version()` function can be used to conveniently check if the string is
a valid version. Use `parse` to receive detailed error messages.

```fortran
print *, is_version('0.1.0-alpha.1') ! true
print *, is_version('abc') ! false
```

## is_exactly()

The `is_exactly()` function has been added for convencience and isn't part of the original Semantic Versioning 2.0.0 specification. It is `true` if both versions are equal including the `build` metadata.

```fortran
v1 = version_t(0, 1, 0, 'a', '1')
v2 = version_t(0, 1, 0, 'a', '1')
print *, v1%is_exactly(v2) ! true

v1 = version_t(0, 1, 0, 'a', '1')
v2 = version_t(0, 1, 0, 'a', '2')
print *, v1%is_exactly(v2) ! false
print *, v1 == v2 ! true
```

## More examples

```fortran
type(version_t) :: version

version = version_t(0) ! 0.0.0
version = version_t(1) ! 1.0.0
version = version_t(3, 2) ! 3.2.0
version = version_t('4.1') ! 4.1.0
version = version_t('.5.') ! 0.5.0
version = version_t('..1') ! 0.0.1
```
There is also a full example in the [example](https://github.com/minhqdao/version-f/tree/main/example) folder. Run it with:

```bash
fpm run --example
```

## Tests

Run tests with:

```bash
fpm test
```

## Formatting

The CI will fail if the code is not formatted correctly. Please configure your
editor to use [fprettify](https://pypi.org/project/fprettify/) and use an
indentation width of 2 or run `fprettify -i 2 -r .` before committing. Thank you. 🙏🏽

## Contribute

Feel free to [create an issue](https://github.com/minhqdao/version-f/issues) in case you found a bug, have any questions or
want to propose further improvements. Please stick to the existing coding style
when you open a pull request.

## License

You can use, redistribute and/or modify it under the terms of the [Apache License, Version 2.0](https://github.com/minhqdao/version-f/blob/main/LICENSE).