# NOVA CRAN Readiness Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix all R CMD check --as-cran blockers so NOVA can be submitted to CRAN without ERROR, WARNING, or NOTE.

**Architecture:** Work entirely in `R/plots.R`, `R/data_handling.R`, `DESCRIPTION`, `.Rbuildignore`, and `R/zzz.R`. Each task is independent and commits immediately. No new public API surface — all changes are internal polish. Option B chosen for verbose output: `cat()` → `message()` throughout.

**Tech Stack:** R, devtools, roxygen2, ggplot2 (>= 3.4), rlang `.data` pronoun

**Worktree:** `/Users/alextudoras/My Documents (change name)/Project_NOVA/NOVA copy 2/.claude/worktrees/nova-features`
**Branch:** `claude/nova-features`

---

## Task 1: Fix Unicode characters in plots.R  *(fixes 4 test failures — ERROR)*

**Files:**
- Modify: `R/plots.R` — lines 866 and 1699

**Context:** R CMD check in ASCII locale fails when source files contain non-ASCII characters
(`\u2192`, `\u2014`, `\u00d7`). Two locations in `plots.R` use these.

**Step 1: Verify the two lines**

```r
# In R/plots.R, confirm these exact strings exist:
# Line 866:
tp_subtitle <- paste0("Timepoints: ", paste(tp_ordered, collapse = " \u2192 "))
# Line 1699:
main = "MEA Heatmap \u2014 Treatment \u00d7 Genotype (Z-score)"
```

**Step 2: Replace line 866**

Find:
```r
tp_subtitle <- paste0("Timepoints: ", paste(tp_ordered, collapse = " \u2192 "))
```
Replace with:
```r
tp_subtitle <- paste0("Timepoints: ", paste(tp_ordered, collapse = " -> "))
```

**Step 3: Replace line 1699**

Find:
```r
main = "MEA Heatmap \u2014 Treatment \u00d7 Genotype (Z-score)"
```
Replace with:
```r
main = "MEA Heatmap -- Treatment x Genotype (Z-score)"
```

**Step 4: Check no other Unicode escapes remain**

Run from the worktree root:
```bash
grep -n "\\\\u[0-9a-fA-F]\{4\}" R/plots.R R/data_handling.R
```
Expected: no output (zero matches).

**Step 5: Run the tests**

```r
devtools::test()
```
Expected: All tests pass. Previously-failing subtitle/heatmap title tests now pass.

**Step 6: Commit**

```bash
git add R/plots.R
git commit -m "fix: replace non-ASCII Unicode escapes with ASCII equivalents

Fixes 4 R CMD check test failures in ASCII locale.
\u2192 -> '->', \u2014 -> '--', \u00d7 -> 'x'"
```

---

## Task 2: Add missing @param docs to create_mea_heatmaps_enhanced  *(fixes WARNING)*

**Files:**
- Modify: `R/plots.R` — roxygen block for `create_mea_heatmaps_enhanced`

**Context:** R CMD check warns about undocumented parameters. Four params are in the function
signature but have no `@param` entry: `use_raw`, `filter_timepoints`, `filter_treatments`,
`filter_genotypes`.

**Step 1: Find the roxygen block**

Search for the start of the block:
```bash
grep -n "@title.*heatmap\|#' create_mea_heatmaps_enhanced\|#' @export" R/plots.R | head -20
```
The roxygen block is at approximately lines 1440–1524. It ends just before `create_mea_heatmaps_enhanced <- function(`.

**Step 2: Find the last @param in the block**

Look for the last `#' @param` line in the block. It is likely `@param split_by` (added in a previous session). The new params go right after it, before `@param output_dir` or `@return`.

**Step 3: Add the four @param entries**

Find the line that reads:
```r
#' @param split_by Character string specifying how to split heatmaps
```
(or whatever the exact last @param line is — adjust the find string accordingly)

After that line, insert:
```r
#' @param use_raw Logical. If \code{TRUE}, plot raw electrode values instead of
#'   normalised values. Default \code{FALSE}.
#' @param filter_timepoints Character vector of timepoint names to include.
#'   \code{NULL} (default) includes all timepoints.
#' @param filter_treatments Character vector of treatment names to include.
#'   \code{NULL} (default) includes all treatments.
#' @param filter_genotypes Character vector of genotype names to include.
#'   \code{NULL} (default) includes all genotypes.
```

**Step 4: Regenerate NAMESPACE**

```r
devtools::document()
```
Expected: no warnings about undocumented arguments.

**Step 5: Run R CMD check and confirm WARNING is gone**

```r
devtools::check(args = "--no-tests")
```
Expected: 0 WARNINGs related to `create_mea_heatmaps_enhanced`.

**Step 6: Commit**

```bash
git add R/plots.R NAMESPACE man/
git commit -m "docs: add missing @param entries for create_mea_heatmaps_enhanced

Fixes R CMD check WARNING: undocumented arguments 'use_raw',
'filter_timepoints', 'filter_treatments', 'filter_genotypes'."
```

---

## Task 3: Fix DESCRIPTION, .Rbuildignore, and delete stray Rplots.pdf  *(fixes NOTEs)*

**Files:**
- Modify: `DESCRIPTION`
- Modify: `.Rbuildignore`
- Delete: `Rplots.pdf` (root) and `tests/testthat/Rplots.pdf`

**Context:** R CMD check emits NOTEs for:
- `LazyData: true` with no `data/` directory
- Non-standard top-level files (`Rplots.pdf`)
- Hidden directories (`.claude/`) and non-standard dirs (`docs/`, `Example/`) in build

**Step 1: Edit DESCRIPTION**

Find:
```
LazyData: true
```
Replace with:
```
Language: en-US
```
(Remove LazyData entirely; add Language in its place.)

**Step 2: Edit .Rbuildignore**

Current contents:
```
^.*\.Rproj$
^\.Rproj\.user$
^Example(/|$)
^Example/.*\.RData$
^CRAN-SUBMISSION$
^cran-comments\.md$
```

Replace entire file with:
```
^.*\.Rproj$
^\.Rproj\.user$
^Example(/|$)
^Example/.*\.RData$
^CRAN-SUBMISSION$
^cran-comments\.md$
^\.claude(/|$)
^docs(/|$)
^scripts(/|$)
^Rplots\.pdf$
^tests/testthat/Rplots\.pdf$
^tmp(/|$)
^\.github(/|$)
```

**Step 3: Delete Rplots.pdf files**

```bash
rm -f "Rplots.pdf" "tests/testthat/Rplots.pdf"
```

Verify they're gone:
```bash
find . -name "Rplots.pdf"
```
Expected: no output.

**Step 4: Confirm DESCRIPTION looks correct**

```bash
grep -E "LazyData|Language" DESCRIPTION
```
Expected: `Language: en-US` (no `LazyData` line).

**Step 5: Run check**

```r
devtools::check(args = "--no-tests")
```
Expected: NOTEs about LazyData, Rplots.pdf, and hidden dirs should be gone.

**Step 6: Commit**

```bash
git add DESCRIPTION .Rbuildignore
git commit -m "chore: remove LazyData, expand .Rbuildignore, add Language field

- Remove LazyData: true (no data/ directory exists)
- Add Language: en-US to DESCRIPTION
- Expand .Rbuildignore to exclude .claude/, docs/, scripts/, Rplots.pdf"
```

---

## Task 4: Add LICENSE file  *(CRAN policy P5)*

**Files:**
- Create: `LICENSE` (plain text GPL-3 notice)
- Create: `LICENSE.md` (full GPL-3 text, linked from DESCRIPTION)
- Modify: `DESCRIPTION` — change `License: GPL-3` to `License: GPL-3 | file LICENSE`

**Context:** DESCRIPTION declares `License: GPL-3` but no LICENSE file exists. CRAN requires
the license file to be present.

**Step 1: Create LICENSE (short notice)**

Create `LICENSE` with:
```
YEAR: 2024
COPYRIGHT HOLDER: Alex Tudoras
```

**Step 2: Update DESCRIPTION license field**

Find:
```
License: GPL-3
```
Replace with:
```
License: GPL (>= 3)
```
(Standard CRAN form — no `| file LICENSE` needed for GPL-3 since it's a standard SPDX license.)

**Step 3: Verify no NOTE about license**

```r
devtools::check(args = "--no-tests")
```
Expected: no NOTE or WARNING about missing license file.

**Step 4: Commit**

```bash
git add LICENSE DESCRIPTION
git commit -m "chore: add LICENSE file and fix license field format for CRAN"
```

---

## Task 5: Convert cat() → message() in data_handling.R  *(CRAN policy P1, part 1)*

**Files:**
- Modify: `R/data_handling.R` — all `cat(` calls (~51 total)

**Context:** CRAN policy forbids functions that write to stdout via `cat()` unless the user
explicitly requests it. `message()` is the standard mechanism — it writes to stderr and
users can suppress it with `suppressMessages()`. Option B chosen: simple global replacement.

**Key transformation rules:**
- `cat("text\n")` → `message("text")`  (strip trailing `\n` — message() adds newline)
- `cat("text:", value, "\n")` → `message("text: ", value)` (comma-separated args work in message)
- `if (verbose) cat(...)` → `if (verbose) message(...)` (keep the verbose guard as-is)
- `cat(paste0(...), "\n")` → `message(paste0(...))` (strip outer `\n`)

**Step 1: Count cat() calls before**

```bash
grep -c "cat(" R/data_handling.R
```
Note the number.

**Step 2: Do a careful manual replacement**

Because the trailing `\n` must be stripped, use a sed command that handles the most common patterns:

```bash
# In the worktree root, create a backup first
cp R/data_handling.R R/data_handling.R.bak

# Replace: cat("...\n") → message("...")
# This sed removes trailing \n before the closing quote+paren
sed -i '' 's/cat(\(.*\)\\n")/message(\1")/g' R/data_handling.R
```

Then manually review the file to catch any cases the sed didn't handle:
```bash
grep -n "cat(" R/data_handling.R
```
For any remaining `cat(` lines, convert by hand using the Edit tool:
- Remove trailing `\n` from the string
- Change `cat(` to `message(`

**Step 3: Remove the backup**

```bash
rm R/data_handling.R.bak
```

**Step 4: Run tests**

```r
devtools::test()
```
Expected: all tests pass.

**Step 5: Commit**

```bash
git add R/data_handling.R
git commit -m "refactor: replace cat() with message() in data_handling.R

CRAN policy requires using message() for informational output so
users can suppress it with suppressMessages(). Converts ~51 cat()
calls to message(). verbose guard remains unchanged."
```

---

## Task 6: Convert cat() → message() in plots.R  *(CRAN policy P1, part 2)*

**Files:**
- Modify: `R/plots.R` — all `cat(` calls (~141 total)

**Context:** Same as Task 5 but for the much larger plots.R file. Same transformation rules.

**Step 1: Count cat() calls before**

```bash
grep -c "cat(" R/plots.R
```
Note the number.

**Step 2: Apply sed transformation**

```bash
cp R/plots.R R/plots.R.bak

sed -i '' 's/cat(\(.*\)\\n")/message(\1")/g' R/plots.R
```

**Step 3: Check for any remaining cat() calls**

```bash
grep -n "^[[:space:]]*cat(" R/plots.R
grep -n "if.*verbose.*cat(" R/plots.R
```
For any remaining ones, convert manually: change `cat(` → `message(`, remove trailing `\n`.

**Step 4: Remove backup**

```bash
rm R/plots.R.bak
```

**Step 5: Run tests**

```r
devtools::test()
```
Expected: all tests pass.

**Step 6: Commit**

```bash
git add R/plots.R
git commit -m "refactor: replace cat() with message() in plots.R

CRAN policy: ~141 cat() calls converted to message(). verbose
guards unchanged. Users can now suppress with suppressMessages()."
```

---

## Task 7: Remove unguarded print() on ggplot/pheatmap objects  *(CRAN policy P2)*

**Files:**
- Modify: `R/plots.R` — lines 337, 377, 404, 432, 465, 1431

**Context:** `print()` calls on plot objects cause unwanted side effects (plots rendered to
screen) during non-interactive use, e.g., in test pipelines or batch scripts. These lines
are inside functions that already save outputs with `ggsave()` / `png()`; the print() is
redundant.

**Step 1: Locate each print() call**

```bash
grep -n "print(p[0-9]\|print(plot_list" R/plots.R
```
Expected output (approximately):
```
337:    print(p1)
377:    print(p2)
404:    print(p3)
432:    print(p4)
465:    print(p5)
1431:   print(plot_list[[plot_name]])
```

**Step 2: Delete or comment each line**

For each line, simply delete the `print(...)` statement entirely.
The functions already save to file — the print() adds nothing.

Example: line 337 — remove the line `    print(p1)`.

Do this for all 6 occurrences.

**Step 3: Run tests**

```r
devtools::test()
```
Expected: all tests pass (no test should depend on side-effect printing).

**Step 4: Commit**

```bash
git add R/plots.R
git commit -m "refactor: remove redundant print() calls on ggplot/pheatmap objects

CRAN policy: functions should not produce output as a side effect.
Plots are already saved to file; print() calls were redundant."
```

---

## Task 8: Replace aes_string() with .data[[]] pronoun  *(CRAN policy P3)*

**Files:**
- Modify: `R/plots.R` — lines 306, 317, 357, 389, 416, 445

**Context:** `aes_string()` was deprecated in ggplot2 3.0.0 and will eventually be removed.
CRAN packages should use the `.data` pronoun from rlang instead:
`aes_string(x = "PC1", color = color_var)` → `aes(x = .data[["PC1"]], color = .data[[color_var]])`

The variables in scope are: `pc1_col`, `pc2_col`, `color_variable`, `shape_variable`,
`secondary_shape_variable` — all of which are character strings (column names).

**Step 1: Find all aes_string calls**

```bash
grep -n "aes_string" R/plots.R
```
Expected: lines 306, 317, 357, 389, 416, 445.

**Step 2: Replace each occurrence**

Pattern to replace each `aes_string(x = VAR, y = VAR2, ...)` call.

**Line 306** (base_aes):
```r
# OLD:
base_aes <- aes_string(x = pc1_col, y = pc2_col)
# NEW:
base_aes <- aes(x = .data[[pc1_col]], y = .data[[pc2_col]])
```

**Line 317** (p1):
```r
# OLD:
p1 <- ggplot(plot_data, aes_string(x = pc1_col, y = pc2_col, color = color_variable, shape = shape_variable)) +
# NEW:
p1 <- ggplot(plot_data, aes(x = .data[[pc1_col]], y = .data[[pc2_col]], color = .data[[color_variable]], shape = .data[[shape_variable]])) +
```

**Line 357** (p2):
```r
# OLD:
p2 <- ggplot(plot_data, aes_string(x = pc1_col, y = pc2_col, color = color_variable, shape = secondary_shape_variable)) +
# NEW:
p2 <- ggplot(plot_data, aes(x = .data[[pc1_col]], y = .data[[pc2_col]], color = .data[[color_variable]], shape = .data[[secondary_shape_variable]])) +
```

**Line 389** (p3):
```r
# OLD:
p3 <- ggplot(plot_data, aes_string(x = pc1_col, y = pc2_col, color = color_variable)) +
# NEW:
p3 <- ggplot(plot_data, aes(x = .data[[pc1_col]], y = .data[[pc2_col]], color = .data[[color_variable]])) +
```

**Line 416** (p4):
```r
# OLD:
p4 <- ggplot(plot_data, aes_string(x = pc1_col, y = pc2_col, color = color_variable)) +
# NEW:
p4 <- ggplot(plot_data, aes(x = .data[[pc1_col]], y = .data[[pc2_col]], color = .data[[color_variable]])) +
```

**Line 445** (p5):
```r
# OLD:
p5 <- ggplot(plot_data, aes_string(x = pc1_col, y = pc2_col, color = color_variable)) +
# NEW:
p5 <- ggplot(plot_data, aes(x = .data[[pc1_col]], y = .data[[pc2_col]], color = .data[[color_variable]])) +
```

**Step 3: Verify no aes_string() remains**

```bash
grep -n "aes_string" R/plots.R
```
Expected: no output.

**Step 4: Run tests**

```r
devtools::test()
```
Expected: all tests pass.

**Step 5: Commit**

```bash
git add R/plots.R
git commit -m "refactor: replace deprecated aes_string() with .data[[]] pronoun

aes_string() was deprecated in ggplot2 3.0.0. Replaces 6 occurrences
with aes(x = .data[[col]], ...) pattern using rlang .data pronoun."
```

---

## Task 9: Fix size= → linewidth= in geom_tile  *(CRAN policy P4)*

**Files:**
- Modify: `R/plots.R` — line 2607

**Context:** In ggplot2 >= 3.4.0, `size` aesthetic for non-point geoms (lines, tiles, etc.)
was renamed to `linewidth`. Using `size` produces a deprecation warning in R CMD check.

**Step 1: Find the line**

```bash
grep -n "geom_tile.*size\|geom_segment.*size\|geom_errorbar.*size" R/plots.R
```
Expected: one hit around line 2607.

**Step 2: Replace size= with linewidth=**

Find:
```r
geom_tile(color = "white", size = 0.5) +
```
Replace with:
```r
geom_tile(color = "white", linewidth = 0.5) +
```

**Step 3: Check for any other geom size= usages**

```bash
grep -n "geom_segment\|geom_errorbar\|geom_tile\|geom_path\|geom_line\|geom_ribbon" R/plots.R | grep "size ="
```
Fix any additional hits with the same pattern.

**Step 4: Run tests**

```r
devtools::test()
```
Expected: all tests pass, no ggplot2 size deprecation warnings.

**Step 5: Commit**

```bash
git add R/plots.R
git commit -m "fix: replace deprecated size= with linewidth= in geom_tile

ggplot2 >= 3.4 deprecates size for non-point geoms in favour of
linewidth. Fixes R CMD check deprecation warning."
```

---

## Task 10: Add missing globalVariables entries  *(fixes NOTE)*

**Files:**
- Modify: `R/zzz.R` — the `globalVariables()` call

**Context:** R CMD check emits NOTEs for variables used in dplyr/ggplot2 pipes that R's
static analyser can't resolve. These must be declared in `globalVariables()` to silence the
NOTE. Run `devtools::check()` first to see the exact list of missing entries, then add them.

**Step 1: Run check to find undefined globals**

```r
devtools::check(args = c("--no-tests"))
```
Look in the output for lines like:
```
NOTE: no visible binding for global variable 'foo'
```

Collect all variable names from those NOTE lines.

**Step 2: Add missing variables to zzz.R**

Open `R/zzz.R`. Find the `utils::globalVariables(c(...))` block (around line 20–34).
Add any missing names to the character vector.

Common candidates based on code review (add any that appear in the NOTE output):
```r
"Metric", "metric_value", "group_label", "Treatment", "Genotype",
"combo_label", "Well", "row_id", "Normalized_Value", "Mean_Value",
"SEM_Value", "SD_Value", "CI95_Value", "n_wells"
```

**Step 3: Regenerate NAMESPACE**

```r
devtools::document()
```

**Step 4: Run check again**

```r
devtools::check(args = c("--no-tests"))
```
Expected: no `no visible binding for global variable` NOTEs remain.

**Step 5: Commit**

```bash
git add R/zzz.R NAMESPACE
git commit -m "fix: add missing globalVariables() entries to silence R CMD check NOTEs

Adds all column-name variables referenced in dplyr/ggplot2 NSE
calls that R's static analyser cannot resolve automatically."
```

---

## Task 11: Final check — run full R CMD check --as-cran  *(verification)*

**Context:** After all fixes, run the full check suite to confirm zero ERRORs, zero WARNINGs,
and minimal/zero NOTEs.

**Step 1: Run full check**

```r
devtools::check(args = c("--as-cran"))
```

**Step 2: Triage any remaining output**

- **ERROR**: Must fix before proceeding.
- **WARNING**: Must fix before CRAN submission.
- **NOTE** about `New submission`: OK — expected for first CRAN submission.
- **NOTE** about `checking installed package size`: OK if < 5 MB.
- Any other NOTEs: assess and fix.

**Step 3: Push to PR**

```bash
git push origin claude/nova-features
```

**Step 4: Commit if any last-minute fixes were needed**

```bash
git add -u
git commit -m "fix: final R CMD check --as-cran cleanup"
git push origin claude/nova-features
```

---

## Summary of all changes

| Task | Files | CRAN issue fixed |
|------|-------|-----------------|
| 1 | `R/plots.R` | ERROR: 4 test failures (Unicode) |
| 2 | `R/plots.R` | WARNING: undocumented params |
| 3 | `DESCRIPTION`, `.Rbuildignore`, delete `Rplots.pdf` | NOTEs: LazyData, non-standard files |
| 4 | `LICENSE`, `DESCRIPTION` | Policy: missing license file |
| 5 | `R/data_handling.R` | Policy: cat() → message() |
| 6 | `R/plots.R` | Policy: cat() → message() |
| 7 | `R/plots.R` | Policy: print() side-effects |
| 8 | `R/plots.R` | Policy: aes_string() deprecated |
| 9 | `R/plots.R` | Policy: size= deprecated |
| 10 | `R/zzz.R` | NOTE: undefined globals |
| 11 | — | Verification: full --as-cran check |
