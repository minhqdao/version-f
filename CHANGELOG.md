# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project follows [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.5.0] - 2026-07-26

### Added

- CMake support for standalone, installed-package and `FetchContent` use.
- Static and shared library builds through the Makefile.
- Property-based, fuzz and adversarial parser tests.
- Read-only accessors and recoverable increment procedures for versions.
- Fortitude linting in the editor configuration and CI.
- Automated tagged releases with source archives and SHA-256 checksums.

### Changed

- Version and range parsing now use linear-time allocation and validation.
- **Breaking:** Version fields and parser implementation types are no longer
  public. Use the version accessors and range query procedures instead.
- Version and range syntax compatibility is documented explicitly.
- CI covers additional compilers, platforms and downstream CMake consumers.

### Fixed

- Build metadata accepts leading zeroes as required by Semantic Versioning.
- Strict parsing rejects leading zeroes in major, minor and patch numbers.
- Version increments report overflow instead of silently succeeding.
- Default and failed operations cannot leave versions or range results
  uninitialized.
- Range parsing handles bounds, malformed separators and very large inputs
  safely.

## [0.4.0] - 2024-01-07

### Added

- Version range parsing and satisfaction for comparison operators and logical
  OR sets.
- Strict parsing and validation through `strict_mode` and `is_version`.
- Exact comparison including build metadata through `is_exactly`.
- Stable-release detection through `is_stable`.
- A second example program covering version ranges.
- Windows and additional compiler coverage in CI.

### Changed

- Enabled fpm module naming and expanded the package documentation and tests.

### Fixed

- Rejected empty prerelease and build identifiers.
- Improved parsing, comparison and identifier handling across supported
  compilers.

## [0.3.0] - 2023-03-02

### Added

- Build metadata increments through `increment_build`.
- Tests and documentation for incrementing build identifiers.

## [0.2.0] - 2023-03-02

### Added

- Prerelease increments through `increment_prerelease`.
- Additional parsing, identifier and increment tests.

### Changed

- Improved identifier handling and prerelease increments.

## [0.1.0] - 2023-02-19

### Added

- Initial fpm package.
- Version creation and parsing with prerelease and build identifiers.
- Semantic Versioning precedence comparisons.
- Major, minor and patch increments.
- Basic tests, example program and CI.

[Unreleased]: https://github.com/minhqdao/version-f/compare/v0.5.0...HEAD
[0.5.0]: https://github.com/minhqdao/version-f/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/minhqdao/version-f/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/minhqdao/version-f/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/minhqdao/version-f/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/minhqdao/version-f/releases/tag/v0.1.0
