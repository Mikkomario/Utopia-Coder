# Utopia Coder - list of changes

## v1.1.1 (in development)
### Bugfixes
- Fixed a bug where project sources would not get applied automatically
### New features
- New refactoring tools:
  - Added **PackageTarget** for locating packages within project sources
  - Added **ReplaceCode**, which can be used for modifying specific types of instances throughout the project
- Added support for 2 new application logics:
  - **ListProjectAppLogic**, which lists projects that have been registered
  - **ShowProjectAppLogic**, which shows project paths
    - This logic may also be used to easily register new projects
- Added support for function identifiers
### New methods
- **File**
  - Add `.mapInstanceDeclarations(...)`
- **InstanceDeclaration**
  - Added `.declarationType: InstanceDeclarationType`, plus `.isClass`, `.isTrait` and `.isObject`
  - Added a number of functions for adding and removing methods, properties and nested instances
- **ScalaType**
  - Added `.withoutGenericTypeParameters`
### Other changes
- Command argument hints are no longer displayed for commands with no arguments
- **Applogic**`.argumentSchema` is now of type **Seq** instead of **Vector**
- **InstanceDeclaration**`.makeCopy(...)` now has default parameter values
- **Parameters** now extends **Iterable**

## v1.1 - 28.7.2024
Adding refactoring support
### Breaking changes
- Modified **CodeAppLogic**'s `run(...)` function's parameters
- Added `.backup: Backup` to **ProjectSetup**'s required properties
- Modified **ProjectPaths** to use 0-n source directories, instead of 1 source + 0-1 alternative sources
- Renamed most properties in **ProjectPaths**
- **ProjectPaths** now tracks project root directory
  - NB: Existing projects may contain wrong information 
    and may need to be edited manually in order for some (new) features to work properly
### New features
- Generation and extension of generic traits is now possible (see README for details)
  - NB: At this time, this feature is yet to be tested properly
- **Files** now generate backups when merging with existing source files
- Added **InputFiles** object that offers a function for identifying the latest versioned file in some directory
- Added support for type declarations
- The generation process now produces a .ndjson file, which contains descriptive information of all the 
  classes / tables and their properties / columns
- Added preliminary support for bulk class renaming
- Added **LazyProjectPaths** class
### New methods
- **NamingRules** (object)
  - Added new model-parsing support, allowing custom defaults
- **Package**
  - Added `/(Package)`
### Other changes
- Generated sources are now written to a sub-folder called `src` by default
- The `merge` parameter now has an alias `src`
- Instead of requesting for the primary and alternative sources separately, 
  the user is now requested to provide all sources at once, separating them with `&`

## v1.0.1 - 22.01.2024
Supports **Flow v2.3**
### Other changes
- Scala version updated to 2.13.12

## v1.0 - 27.09.2023
Initial release
