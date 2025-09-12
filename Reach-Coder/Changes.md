# Reach Coder - List of changes

## v1.0.4 (in development)
### Bugfixes
- Fixed bugged context property parsing
  - Previously the parsing would not recognize the static and variable variants
### New features
- Added context type: `"any"`
### Other changes
- The "Contextual" keyword is omitted from component factory names in situations where no non-contextual version exists
- Output is now compatible with **Reach v1.6**

## v1.0.3 - 23.1.2025
This update adds support for the new context classes introduced in **Firmament v1.4**. 
Basic project commands are also added, and some bugfixes included.
### Bugfixes
- Fixed some smaller container code-generation bugs
### New features
- Added `list` and `show <project>` commands / modes
### Other changes
- Supports the new context classes introduced in **Firmament** v1.4

## v1.0.2 - 28.7.2024
A minor update following mostly general **Utopia Coder** changes.
### New features
- Backups are now taken when files are merged together
### Other changes
- Generated scala files are now placed in a subdirectory called `src`

## v1.0.1 - 22.01.2024
Supports **Reach v1.2**
### Bugfixes
- Contextual component factories now correctly define `.self`

## v1.0 - 27.09.2023
Initial release
