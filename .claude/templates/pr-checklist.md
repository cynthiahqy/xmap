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
- [ ] Dev version bumped as part of this PR (`DESCRIPTION`'s `Version`, plus a
      `NEWS.md` "Dev version bumped to `x.y.z.9NNN`" bullet) -- do this inside the
      PR, not as a follow-up commit on `main` afterwards. `7e032cf`/`0badf7d` look
      like an established "bump separately after merging" convention but weren't --
      they were catch-up commits patching over PRs (including this checklist's own
      originating PRs) that omitted the bump. Don't repeat that omission
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
- [ ] Spell-checked with `aspell` and style-checked with `proselint` (both use
      repo-local config, see below) -- run on the specific vignette touched, not
      necessarily the whole package unless doing a broader pass
      ```sh
      aspell list --lang=en_GB --mode=markdown \
        --personal="$(pwd)/.aspell.en_GB.pws" < vignettes/<file>.Rmd
      proselint check --config .proselintrc.json vignettes/<file>.Rmd
      ```
      Both catch real things, not just noise -- `aspell` caught "Explictly" for
      "Explicitly" in a section heading (#46), and a first pass across every
      vignette surfaced "arithemtic"/"calcuted" (`xmap.Rmd`) and
      "classifcations" (`examine-compose-crossmaps.Rmd`), all genuine typos.
      `aspell` output for a word that's real but domain-specific (package/function
      names, "crossmap", "recoding", the standard `%\Vignette*` YAML fields) means
      add it to `.aspell.en_GB.pws`, not that the check is broken -- but don't add
      an entry without checking it's actually not a typo first. `.proselintrc.json`
      already disables `typography.symbols.curly_quotes` (fires on every straight
      quote inside backtick-quoted code, which is correct as written) and
      `lexical_illusions` (false "repeated word" on the standard `vignette:` /
      `\VignetteIndexEntry` YAML block present in every `.Rmd` here) -- if a new
      check starts firing pure structural noise the same way, disable it in that
      config (it's plain JSON, no comments -- explain the reason in the commit
      message instead) rather than ignoring the tool's output going forward

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
