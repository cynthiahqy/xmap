# PR merge checklist

Copy the relevant sections into the PR description before merging. `xmap` has one CI
workflow (`pkgdown`, rebuilds the site including every vignette) -- everything else
below is a manual check to run locally, since there's no `R-CMD-check` workflow.

## Every PR

- [ ] `devtools::test()`: 0 failures
- [ ] `devtools::check()`: 0 errors, 0 warnings (NOTEs are fine if limited to local
      artifact noise -- `.claude/worktrees`, `.history`, `.vscode`, `README_files` --
      not anything introduced by this PR)
- [ ] `NEWS.md` bullet added, referencing the issue/PR number it resolves (standing
      rule: never merge without one)
- [ ] No dev version bump in this PR -- bumps happen as their own standalone commit
      on `main` after merging (often bundling several just-merged PRs), not inside a
      feature PR. See e.g. `7e032cf`, `0badf7d`
- [ ] pkgdown CI check passes (`gh pr checks <n>`)
- [ ] Branch is up to date with `main` (rebase or merge locally, resolve conflicts
      before pushing -- don't rely on GitHub's merge UI to catch conflicts)

## New or changed vignette

- [ ] Rendered via `rmarkdown::render()` or `devtools::build_vignettes()` -- **not**
      only checked under `devtools::load_all()`. `load_all()` sources
      `tests/testthat/helper-*.R`, which can make a test-only fixture (e.g.
      `simple_links`) look like it exists when it isn't actually part of the
      package -- a real build will fail with `object '...' not found`. This exact
      bug shipped in an early draft of `applying-crossmaps.Rmd` (#46) and only
      surfaced when `devtools::check()` was run for real
- [ ] Any new vignette-only dependency (plotting packages, etc.) added to
      `Suggests` in `DESCRIPTION`
- [ ] If CRAN status of a new dependency matters: confirm it's actually on CRAN and
      still maintained before adding it, not just that `library()` works locally
      (`ggbump` was archived off CRAN and had to be swapped for `ggforce`, #51)
- [ ] No leftover `TODO`/placeholder HTML comments (`grep -n "TODO" vignettes/*.Rmd`)
- [ ] No commented-out prose blocks left in the source (`grep -n "<!--" vignettes/*.Rmd`)
      -- either restore or delete, don't ship a silent draft note
- [ ] Re-read the intro/summary paragraph after adding or removing a section --
      it's easy to leave a promise ("this vignette covers X") that no longer
      matches what the vignette actually demonstrates
- [ ] `fig-alt` text added for any new figure

## New or changed package data

- [ ] Regenerated via its `data-raw/*.R` script, not hand-edited
- [ ] Documented in `R/data.R` (roxygen `@format`, source/provenance noted)
- [ ] Provenance/source confirmed and stated explicitly, not assumed (matches
      established practice, e.g. `9407834`, `f11c940`)
- [ ] Confirmed accessible as `pkg::name` after a real `library(xmap)`, not just
      under `load_all()` (same class of bug as the vignette `load_all()` check above)

## New or changed exported function

- [ ] `devtools::document()` run; `NAMESPACE`/`man/*.Rd` up to date and committed
- [ ] Roxygen docs complete: `@param`, `@return`, `@examples`
- [ ] Tests cover the specific failure mode being guarded against, not just a
      happy-path smoke test -- e.g. #49's tests specifically cover a zero weight
      whose `.from` sum is *still* 1 (so it can't be caught by the existing sum
      check), not just an obviously-invalid case
- [ ] Any check/logic shared across more than one call site is deduplicated behind
      a single internal helper, not reimplemented at each site (the `vhas_*()` /
      `check_valid_xmap_df()` / `check_conformable_xmap_data()` pattern -- #19, #45)
- [ ] If the function is exported and user-facing: added to `_pkgdown.yml`'s
      reference index, or given `@keywords internal` to deliberately exclude it --
      an exported function in neither state fails the pkgdown CI build (#47)
