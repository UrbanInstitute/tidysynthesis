---
name: CRAN Release
about: Describe this issue template's purpose here.
title: ''
labels: ''
assignees: ''

---

This checklist was built using the [R package book](https://r-pkgs.org/release.html), [this issue from parsnip](https://github.com/tidymodels/parsnip/issues/1282), and Davis Vaughn's [extrachecks](https://github.com/DavisVaughan/extrachecks).

Prepare for release

- [ ] Update the package version to #.#.#.
- [ ] Proofread Title: and Description: 
- [ ] Check that all exported functions have @returns and @examples.
- [ ] Check that Authors@R: includes a copyright holder (role ‘cph’).
- [ ] Double check to see if any third-party code is included. 
- [ ] Add aspirational installation instructions to the README. 
- [ ] Run `devtools::check()`. Eliminate all errors, warnings, and notes.
- [ ] `urlchecker::url_check()`
- [ ] `devtools::build_readme()` (we need to manually render for now since we use Quarto instead of R Markdown)
- [ ] Add GitHub actions to check the package on macOS, Windows, and Linux. Eliminate all errors, warnings, and notes.
- [ ] Go through [Davis Vaughn's extrachecks](https://github.com/DavisVaughan/extrachecks)
- [ ] Polish NEWS.md

Submit to CRAN

- [ ] Create communications for submitting a package. Start with `usethis::use_cran_comments()`.
- [ ] Submit to CRAN using the web application
- [ ] Approve email

Wait for CRAN

- [ ] Accepted 🎉
- [ ] Create a GitHub release to align with the CRAN release (`usethis::use_github_release()`).
- [ ] Outline the next release (`usethis::use_dev_version(push = TRUE)`).


We want version to release on CRAN
