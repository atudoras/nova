# CRAN Submission Comments — NOVA 0.1.1

## Test environments

- macOS 15.3 (Darwin 25.3.0), R 4.x, x86_64
- Tested via `devtools::check(args = "--as-cran")`

## R CMD check results

```
0 errors | 0 warnings | 1 note
```

### NOTE

```
checking for hidden files and directories ... NOTE
Found: .git
```

This NOTE is a known devtools artefact caused by running the check
against a live working tree. It does not appear when submitting the
package tarball to CRAN.

## Downstream dependencies

None — this is a new package with no reverse dependencies.

## Notes for CRAN reviewers

- This is a first submission.
- The package provides MEA (Multi-Electrode Array) neuronal data analysis
  tools for neuroscience research.
- All functions use message() for progress output (suppressible with
  suppressMessages()).
- No internet access is required at runtime.
- No persistent state is written outside of user-specified output directories.
