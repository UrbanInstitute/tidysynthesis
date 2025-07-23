# tidysynthesis 0.1.1

* Add rounding to `noise()` (#6) and fix `obs_per_ntile` in `noise()` (#3).
* Add default noise mechanism in `synth_spec()` for when exactly one type of noise mechanism is specified (#2).
* Update printing for `synthesize()` (#4) and constraints (#10).
* Add a gini method to `add_sequence_factor()` and resolve minor bugs (#15, #16).
* Update documentation for built-in data sets (#7).

# tidysynthesis 0.1.0

* Add `constraints()` support for generic conditions and categorical constraints.
* Update `noise()` to accept generic functions and parameters.
* Add new `add_noise_*()` functions for use with new generic `noise()`. 
* Audit and update all user-facing error messages for more informative errors.
* Refactor `synthesize()` internals for more modular development and validation.
* New `replicates()` logic for replicating start methods and conditional syntheses independently. 

# tidysynthesis 0.0.7 

* New `tidymodels` inspired API.
* Add `visit_sequence` API for mixed-type data-dependent sequencing.
* Add `start_method` for randomized `start_data` transformations.
* Add `tuners` and user-specified hyperparameter tuning. 
* Update `constraints()` to work with `NA` values.
* Remove dependence on `furrr` and stratified synthesis.

# tidysynthesis 0.0.6

* Fix a bug caused by `col_schema` in `visit_sequence` not following the sort order of the `visit_sequence`.

# tidysynthesis 0.0.5

* Add ability to synthesize numeric and categorical variables with `NA`.
* Add ability to extract information from synthesizer models using [`extract_*()` functions.](https://workflows.tidymodels.org/reference/extract-workflow.html)
* Refactor `schema()` object and reverse the order of `schema()` and `visit_sequence()` in a typical synthesis. 
* Improve `ldiversity` calculations for tree-based models.
* Fix issue with empty factor levels caused by new version of `library(recipes)`. 

# tidysynthesis 0.0.4

* Refactor `construct_recipes()` to handle much larger data sets. The `.` notation in the formula caused a C Stack error for  with large visit sequences. 
* Add variable-by-variable hyperparameter tuning. 
* Add ability to store model metrics from each predictive model.  
* Add new schema object to support mixed-data types and data validation. 
* Add categorical sampler for random forests and add samplers for logistic regression and Poisson regression. 
* Improve validators for S3 objects. 
* Add `library(ranger)`.
* Add support for batch synthesis. 
* Update installation instruction. 

# tidysynthesis 0.0.3

* Fix bug in `sample_rpart()` for categorical synthesis.
* Fix bug in progress bar caused by `library(furrr)`. The package now uses `library(progressr)` for tracking progress.

# tidysynthesis 0.0.2

* Add default objects in `presynth()` for `noise`, `constraints`, and `replicates` so `noise()`, `constraints()`, and `replicates()` don't always need to be called.
* Add code to handle empty factor levels, which break many models.
* Add tests in `roadmap()` for ordinal variables. 
* Make the `"identity"` method use the same structure as models from `library(parsnip)`.
* Add tests and fix all issues with s3 print methods.
* Rewrite joins with `multiple = "all"` to minimize R warnings. 

# tidysynthesis 0.0.1

* Create first numbered version of `library(tidysynthesis)`!
* Rewrite `assign_constraints()` and `synthesize_j()` to improve performance by as much as 10x. 
* Factors are now synthesized as factors instead of strings.
* Add "identity" methods for variables without variation. This previously caused decision trees to error out. 
* Add more tests for the `ldiversity` tibble.
* Add defaults for `noise`, `constraints`, and `replicates` in `presynth()`. 


