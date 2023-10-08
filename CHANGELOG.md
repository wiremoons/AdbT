## v0.1.5
- Add `adbt.gpr` build option to define processor used via env variable `PROCESSORS`.
- Add additional 'release' build info to include using Alire.
- Add new `Misc_Utils` module with function `Convert_Epoch_To_Local_Time`. See: `misc_util.adb` and `misc_util.ads`.
- Add new function `Convert_Epoch_String` to convert database epoch change timestamps to strings.
- Update record outputs for search to include record changes timestamp value.

## v0.1.4
- Add hostname the application is being executed on to `-v/--version` output.
- Re-order the output of `-v/--version` output to improve clarity and align with other apps.
- Update copyright notice year range in `-v/--version` output.
- Add a 'CHANGELOG.md' file to the project.

## v0.1.3
- Add build instructions for 'alr'. Change default build to 'release'.
- Add Alire directory 'config/' to .gitignore to exclude.
- Add GNATStudio 'src/.clang-format' added file to '.gitignore'.

## prior versions.
- See output of `git log` for changes made.
