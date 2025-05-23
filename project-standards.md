# Project Standards

## Overview

This document explains the project and programming conventions used for `library(tidysynthesis)` and the generation of synthetic data. The document is a work-in-progress and should be updated as conventions are created or changed. 

## Contents

* [Principles](#principles)
  - [tidyverse](#tidyverse)
  - [tidymodels](#tidymodels)
  - [tidysynthesis](#tidysynthesis)
* [Project Organization](#project-organization)
  - [Directory Structure](#directory-structure)
  - [Documentation](#documentation)
* [Workflow](#workflow)
  - [Development](#development-workflow)
  - [Synthesis](#synthesis-workflow)
* [Styles](#style)
  - [Code Style](code-style)
  - [Naming Conventions](#naming-conventions)
  - [roxygen2](#roxygen2)
  - [Assertions and In-line Errors](#assertions-and-in-line-errors)
  - [Tests](#tests)

## Principles

This project is heavily inspired by `library(tidyverse)` and `library(tidymodels)`. 

### tidyverse

This project aims to follow the four guiding principles outlined in the [tidytools manifesto](https://tidyverse.tidyverse.org/articles/manifesto.html):

1. Reuse existing data structures
2. Compose simple functions with the pipe
3. Embrace functional programming
4. Design for humans

Building smaller packages that handle discrete tasks instead of large packages that do everything is clearly a tidytools principle that is not listed. Our eventual goal is to reflect this design. 

### tidymodels

`library(tidymodels)` weds the unified modeling interface of `library(caret)` with tidy principles. [Conventions for R Modeling Packages](https://tidymodels.github.io/model-implementation-principles/index.html) is a draft outline of principles to `library(tidymodels)`. Here are a few important principles:

* All results should be reproducible from run-to-run
* Retain the minimally sufficient objects in the model object.
* Every class should have a `print` method that gives a concise description of the object.

### tidysynthesis

`tidysynthesis` aims to leverage the power of `library(tidymodels)` for data synthesis. The package should be extensible and transparent, and it should be easy for the community to review and contribute to the code on GitHub.  

* Use `.data` and `.env` any time there is ambiguity about data masking ([source](https://tinyheero.github.io/2020/03/01/use-data-env-pronouns-tidyverse.html)).

## Project Organization

### Directories

Directories add structure to a project and make it possible to turn `tidysynthesis` into `library(tidysynthesis)`

* `R/` contains R functions as `.R` scripts
* `man/` contains `.Rd` documentation files. No manual editing should happen in this directory.
* `tests/` contains unit tests for functions

### Documentation

There are several important places where documentation is captured:

* The README contains information specific to the code base
* `roxygen2` skeletons contain information specific to functions
* Some `.R` scripts contain in-line comments clarifying code

Out-of-date and incorrect documentation can be more damaging than no documentation at all. It is important that documentation is updated when changes are made. Check all of the above places after making changes to code.

## Workflow

### Development

1.  Open a [GitHub issue](https://github.com/UI-Research/tidysynthesis/issues)
2.  Checkout a new branch named `iss###` that corresponds to the related issue
3.  Update the code
4.  Build necessary tests for new code and updating existing tests for code changes
5.  Run `devtools::document()` to update package documentation and the package NAMESPACE
6.  Build and install the package with Ctrl-Shift-b
7.  Run R CMD check with Ctrl-Shift-E
8.  Push the code and put in a Pull Request to the `version#.#.#` branch. Request a code reviewer for any changes to code. 
9.  Delete the remote branch (and possibly the local branch) when all changes are merged into the `main` branch.
10. In the GitHub issue, reference the pull request that resolves the issue and close the issue with comment. 
11. From time-to-time, new releases will be moved from `version#.#.#` to `main`. The `main` branch should be stable at all times and updated according to a release schedule. 

**Note:** do not use `devtools::load_all(.)`.

**Note:** use `git merge main`, not `git rebase main` if your Pull Request falls behind the `main` branch of the repository. This preserves the commit history.

### Releases

* Major changes should be tracked in `NEWS.md`. `library(parnsip)` is a good [example](https://github.com/tidymodels/parsnip/blob/master/NEWS.md). 
* Changes on the development branch should be tracked at the top under the header `tidysynthesis (development version)`.
* Version numbers will update as the code is merged to `main`. Accordingly, changes should be moved to the corresponding version number. 

We use [semantic versioning](https://semver.org/).

### Synthesis

## Styles

### Code Style

The project follows [the tidyverse style guide](https://style.tidyverse.org/index.html). 

One major exception is that all functions should include `return()` at the end of the function. 

Package NAMESPACEs should be directly referenced with `::` in all production code including R Markdown reports. 

Argument names should be explicitly included in all function calls from `library(tidysynthesis)`. Arguments other than `data` or `x` should be explicitly included for most other function calls. 

The [tidyverse style guide](https://style.tidyverse.org/index.html) is light on details about vertical spacing. Vertical spacing should be liberally used. For example:

```
if (x > 3) {

  "apple"

} else {

  "orange"

}
```

This project takes a functional programming approach. Functions should be heavily used. Each function should get its own `.R` script in the `R/` directory. 

Functions should be [referentially transparent](https://b-rodrigues.github.io/fput/fprog.html#fprog_intro). Values and data should always be explicitly passed to the function through function arguments so that a function always returns the same output for a given set of arguments regardless of the environment. 

Hard coding of values should be avoided in functions. When possible, values should be parameterized. 

The project uses `.Rproj` to manage directory paths. `setwd()` and absolute file paths should never be used.

### Naming Conventions

### roxygen2

Every function should include a [roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) header. 

* The first line of the documentation should be a concise description of the function without a full stop
* Every argument of the function should be documented with `@param`. Text should be in sentence case and end in a full stop. 

### Assertions and In-line Errors

Assertions, things expected to always be true about the code, should be tested in-line. [healthinequality-code](https://github.com/michaelstepner/healthinequality-code/blob/master/code/readme.md#assert-what-youre-expecting-to-be-true) offers some good background. 

Functions should contain logical tests that catch glaring errors when functions are called. Consider the following example from `visit_sequence()`:

```
  valid_types <- c("default", "correlation", "proportion", "weighted total",
                   "weighted absolute total")
  
  if (!type %in% valid_types) {
    stop(
      'type' argument must be one of: ",
      paste0(valid_types, collapse = ", ")
    )
  }
```

### Tests

> Whenever you are tempted to type something into a print statement or a debugger expression, write it as a test instead. — Martin Fowler

Every function should include a corresponding test file in `tests/testthat/`.

Use `usethis::use_testthat()` to create a new test file for `library(tidysynthesis)`. Test files have three layers:

1. **expectations** describe the expected result of a computation
2. **tests** are collections of expectations related to the same functionality
3. **files** are groups of related tests

Consider the following example from [Advanced R](https://r-pkgs.org/tests.html):

```
context("String length")
library(stringr)

test_that("str_length is number of characters", {
  expect_equal(str_length("a"), 1)
  expect_equal(str_length("ab"), 2)
  expect_equal(str_length("abc"), 3)
})

test_that("str_length of factor is length of level", {
  expect_equal(str_length(factor("a")), 1)
  expect_equal(str_length(factor("ab")), 2)
  expect_equal(str_length(factor("abc")), 3)
})

test_that("str_length of missing is missing", {
  expect_equal(str_length(NA), NA_integer_)
  expect_equal(str_length(c(NA, 1)), c(NA, 1))
  expect_equal(str_length("NA"), 2)
})
```

Our workflow:

1. Every function should have tests. Write tests *before* writing a new function. 
2. Develop code. Add tests as functionality changes.
3. Always run the tests after building the package with `devtools::test()`

A few suggestions:

* Always write a test when you discover a bug
* Test each behavior once and only once--if possible
* Test simple code. Spend even more time testing complex/fragile code

Tests will focus on if correct values are returned by a function, if the return values are of the right class, and if error messages are thrown when necessary. The test workflow will also catch warnings and errors from all code called in the code base. 

Here are common `expect_*()` functions:

* `expect_equal()`
* `expect_identical()`
* `expect_match()`
* `expect_output()`
* `expect_warning()`
* `expect_error()`
* `expect_is()`
* `expect_true()`
* `expect_false()`

**Note:** do not use `devtools::load_all(.)` in test files.

Assertions should be used to catch user errors or unexpected results. Tests should be used to catch design errors and errors in the code base. 
