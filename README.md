# version-f

[![License](https://img.shields.io/github/license/minhqdao/version-f?label=License&color=teal)](https://opensource.org/licenses/Apache-2.0)
[![Release](https://img.shields.io/github/release/minhqdao/version-f?label=Release)](https://github.com/minhqdao/version-f/releases)
[![CI](https://github.com/minhqdao/version-f/actions/workflows/ci.yml/badge.svg)](https://github.com/minhqdao/version-f/actions/workflows/ci.yml)


This package implements the version syntax and precedence rules from
[Semantic Versioning 2.0.0](https://semver.org). It aims to be a user-friendly
tool for handling versions in your Fortran projects.

It follows the `major`.`minor`.`patch` pattern and allows the inclusion of
`prerelease` labels and `build` metadata. Versions can be created or parsed from
strings, compared, incremented and printed as strings.

## Installation

### fpm

If you are using [fpm](https://fpm.fortran-lang.org/en/index.html), you can
simply add this package as a dependency to your `fpm.toml` file:

```toml
[dependencies]

[dependencies.version-f]
git = "https://github.com/minhqdao/version-f.git"
tag = "v0.4.0"
```

Then import the `version_f` module into your Fortran code:

```fortran
use version_f, only: version_t, error_t
```

Run `fpm build` to download the dependency.

The components of `version_t` are private so that a version cannot be placed in
an invalid state through direct assignment. Use `major()`, `minor()`, `patch()`,
`prerelease()` and `build()` to inspect them. A default
constructed `version_t` is initialized to `0.0.0`. Identifier accessors return
dot-separated strings, or an empty string when no identifiers are present.

### make

To build the library using the provided `Makefile`, navigate to the project's root directory and execute the following command if you are on a Unix-like system:

```bash
make
```

This will generate the static library `libversion-f.a` in the root directory. You can compile it alongside your project or link it using the `-L` and `-l` flags.

A dynamic library will also be created for use in your projects (suffix `.so` on Linux systems and `.dylib` on macOS).

If you wish to use a compiler other than `gfortran`, simply specify it by running:

```bash
make FC=<compiler>
```

## Create versions

Create versions using one of the following commands:

```fortran
type(version_t) :: version
type(error_t), allocatable :: error

! The default way using individual arguments
version = version_t(0, 1, 0)

! Parse from string
version = version_t('0.1.0')

! From arguments with error handling
call version%create(0, 1, 0, error=error)

! From string with error handling
call version%parse('0.1.0', error)

! From arguments with prerelease labels and build metadata
call version%create(0, 1, 0, 'alpha', '1', error)

! From string with prerelease labels and build metadata
call version%parse('0.1.0-alpha+1', error)
```

## Prerelease labels

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

## Build metadata

`build` metadata can be included and will be appended after the `patch` or the `prerelease` via a `+` sign. The identifiers must comprise only ASCII alphanumerics and hyphens `[0-9A-Za-z-]` and are separated by dots. Build identifiers may contain leading zeroes. `build` data is not used for comparison and it is cleared every time a `major`, `minor`, `patch` or `prerelease` version is incremented. A `build` value can be [incremented](#increment-versions).

```fortran
type(version_t) :: v1, v2

v1 = version_t(0, 5, 3, build='1')
print *, v1%to_string() ! '0.5.3+1'

v2 = version_t(0, 5, 3, build='abc.1-13')
print *, v2%to_string() ! '0.5.3+abc.1-13'

print *, v1 == v2 ! true

call v1%increment_patch() ! 0.5.4
call v1%increment_build() ! 0.5.4+1

v1 = version_t(0, 5, 3, 'alpha.1', '1')
print *, v1%to_string() ! '0.5.3-alpha.1+1'
call v1%increment_build()
print *, v1%to_string() ! '0.5.3-alpha.1+2'
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

## Version ranges

Use the `satisfies`/`try_satisfy` procedures to verify whether a version meets a range. A range encompasses one or more comparator sets, with multiple sets separated by || (logical OR). Each comparator set combines one or more comparators using a space-separated arrangement (logical AND). A comparator consists of an operator and a version. The available operators include the following:

- `=`: Equal to
- `!=`: Not equal to
- `>`: Greater than
- `>=`: Greater than or equal to
- `<`: Less than
- `<=`: Less than or equal to

Ranges use `version-f` syntax, not npm, Cargo or Python syntax. Partial versions
are filled with zero and compared exactly, so `1.2` means `1.2.0`. Prereleases
are not excluded automatically. Wildcards, caret, tilde and hyphen ranges are
not supported.

```fortran
program main
  use version_f

  type(version_t) :: version
  logical :: is_satisfied
  type(error_t), allocatable :: error

  version = version_t(0, 1, 0)

  print *, version%satisfies('0.1.0') ! true
  print *, version%satisfies('=0.1.0') ! true
  print *, version%satisfies('!=0.1.0') ! false
  print *, version%satisfies('>0.1.0') ! false
  print *, version%satisfies('>=0.1.0') ! true
  print *, version%satisfies('<0.1.0') ! false
  print *, version%satisfies('<=0.1.0') ! true
  print *, version%satisfies('>=0.1.0 <0.2.0') ! true
  print *, version%satisfies('>0.1.0 <0.2.0') ! false
  print *, version%satisfies('>0.1.0 <0.2.0 || 0.1.0') ! true
  print *, version%satisfies('0.0.8 || 0.0.9 || >0.1.0 <0.2.0') ! false

  call version%try_satisfy('<=0.1.0', is_satisfied, error)
  if (allocated(error)) call exit(1)
  print *, is_satisfied ! true
end
```

Parse a `version_range_t` once to reuse it:

```fortran
type(version_range_t) :: range

call range%parse('>=1.0.0 <2.0.0', error)
print *, range%satisfies(version_t(1, 5, 0)) ! true
```

## Strict mode

In `strict_mode` (optional parameter in `create`, `parse` and `is_version`), all `major`, `minor` and `patch` numbers must be provided. Implicit zeros and leading zeroes are forbidden. Without strict mode, missing numbers become zero. Input is trimmed in both modes.

```fortran
type(version_t) :: version
type(error_t), allocatable :: error

call version%create(1, error=error, strict_mode=.true.)
print *, allocated(error) ! true

call version%parse('1.2', error=error, strict_mode=.true.)
print *, allocated(error) ! true

print *, is_version('0.1.0-alpha.1', strict_mode=.true.) ! true
print *, is_version('0.1.-alpha.1', strict_mode=.true.) ! false
print *, is_version('0.1.-alpha.1') ! true
print *, is_version('0.1-alpha.1', strict_mode=.true.) ! false
print *, is_version('0.1-alpha.1') ! true
```

## Increment versions

`Prerelease` and `build` data are cleared every time a major, minor or patch number is incremented. `prerelease` and `build` values are incremented by adding `1` to the their last identifier if it is numeric. If the last identifier isn't a number or no identifiers exist, a new identifier is added with the value of `1`. Existing `build` information is cleared each time a `prerelease` is incremented.

```fortran
type(version_t) :: version

version = version_t(0, 5, 3, 'beta.1', '1')

call version%increment_build() ! 0.5.3-beta.1+2
call version%increment_prerelease() ! 0.5.3-beta.2
call version%increment_patch() ! 0.5.4
call version%increment_minor() ! 0.6.0
call version%increment_major() ! 1.0.0
```

The `increment_*` procedures stop with an error if the value cannot be
incremented without integer overflow. Use the corresponding `try_increment_*`
procedure to handle the error instead:

```fortran
type(error_t), allocatable :: error

call version%try_increment_major(error)
if (allocated(error)) print *, error%message()
```

## is_version()

The `is_version()` function can be used to conveniently check if the string is
a valid version. Use `parse` to receive detailed error messages.

```fortran
print *, is_version('0.1.0-alpha.1') ! true
print *, is_version('abc') ! false
```

## is_stable()

The `is_stable()` function returns `true` if the `major` version is strictly positive and the version does not have a `prerelease` label.

```fortran
v1 = version_t(0, 9, 10)
print *, v1%is_stable() ! false
v1 = version_t(1, 0, 0)
print *, v1%is_stable() ! true
v1 = version_t(1, 0, 0, 'alpha')
print *, v1%is_stable() ! false
```

## is_exactly()

The `is_exactly()` function has been added for convenience and isn't part of the original Semantic Versioning 2.0.0 specification. It is `true` if both versions are equal _including_ the `build` metadata.

```fortran
v1 = version_t(0, 1, 0, 'a', '1')
v2 = version_t(0, 1, 0, 'a', '1')
print *, v1%is_exactly(v2) ! true

v1 = version_t(0, 1, 0, 'a', '1')
v2 = version_t(0, 1, 0, 'a', '2')
print *, v1%is_exactly(v2) ! false
print *, v1 == v2 ! true
```

## to_string()

Versions are converted to strings using the `to_string()` method.

```fortran
type(version_t) :: version

version = version_t(0, 5, 3, 'beta.1', '1-100')

print *, version%to_string() ! '0.5.3-beta.1+1-100'
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
There are also full examples in the [example](https://github.com/minhqdao/version-f/tree/main/example) folder. Run them with:

```bash
fpm run --example <example_program>
```

## Tests

The CI enforces compiler warnings, so make sure to test locally with the same
flags to avoid surprises. The recommended flags per compiler are:

| Compiler | Flags |
| --- | --- |
| gfortran | `-Wall -Wextra -fcheck=all -fbacktrace` |
| ifx / ifort | `-warn:all` |
| nvfortran | `-Minform=inform` |

With fpm:

```bash
fpm test --flag '-Wall -Wextra -fcheck=all -fbacktrace'
```

With make:

```bash
make test FFLAGS="-O2 -Wall -Wextra -fcheck=all -fbacktrace"
```

## Formatting

The CI will fail if the code is not formatted correctly. Please configure your
editor to use [fprettify](https://pypi.org/project/fprettify/) and use an
indentation width of 2 or run `fprettify -i 2 -r .` before committing.

## Contribute

Feel free to [create an issue](https://github.com/minhqdao/version-f/issues) in case you found a bug, have any questions or
want to propose further improvements. Please stick to the existing coding style
when you open a pull request.

## API stability

Before version 1.0, public APIs may change in minor releases. Breaking changes
will be noted in the release notes. Deprecated APIs will be kept for one minor
release when practical. After version 1.0, breaking changes require a new major
version.

## License

You can use, redistribute and/or modify it under the terms of the [Apache License, Version 2.0](https://github.com/minhqdao/version-f/blob/main/LICENSE).
