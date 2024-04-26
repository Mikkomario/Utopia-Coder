# Utopia Coder - list of changes

## v1.1 (in development)
Adding refactoring support
### Breaking changes
- Added `.backup: Backup` to **ProjectSetup**'s required properties
### New features
- **Files** now generate backups when merging with existing source files
- Added **InputFiles** object that offers a function for identifying the latest versioned file in some directory
- Added support for type declarations
- Added preliminary support for bulk class renaming
### Other changes
- Generated sources are now written to a sub-folder called `src` by default

## v1.0.1 - 22.01.2024
Supports **Flow v2.3**
### Other changes
- Scala version updated to 2.13.12

## v1.0 - 27.09.2023
Initial release
