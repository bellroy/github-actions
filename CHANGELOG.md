# CHANGELOG

## Version 0.3.0.0

- Changed `runsOn` from `Maybe Text` to `Maybe RunsOn` to support GitHub Actions
  runner label lists (e.g. `runs-on: [self-hosted, linux]`), while preserving
  the original YAML representation during round-trip serialization

## Version 0.2.0 (2025-07-21)

- Changed `cancelInProgress` from `Maybe Bool` to `Maybe Text` to support GitHub Actions expressions

## Version 0.1.0 (2025-07-01)

Released upon an unsuspecting world

## Version 0.1.1 (2025-07-21)

- Added missing module exports
- Support multiple undocumented input types that work in YAML but were not supported
