#' Identifies configured component parameters, associates each with its display
#' category, and collects synthesis settings for use by console or HTML
#' renderers.
#'
#' @param x A `synth_spec` object.
#' @return A list with:
#'   \itemize{
#'     \item{title}{A character title for the display.}
#'     \item{components}{A data frame containing the configured component
#'     parameter names and their categories.}
#'     \item{settings}{A named logical vector containing the
#'     `invert_transformations` and `enforce_na` settings.}
#'   }
#' @noRd
.synth_spec_display_data <- function(x) {
  
  metadata <- .synth_spec_component_metadata()
  is_configured <- !vapply(
    x[metadata$name],
    is.null,
    logical(1)
  )
  
  configured <- metadata[is_configured, , drop = FALSE]
  
  settings <- c(
    invert_transformations = x[["invert_transformations"]],
    enforce_na = x[["enforce_na"]]
  )
  
  return(list(
    title = "Synthesis specification",
    components = configured,
    settings = settings
  )) 
}


#' Define display categories for synth_spec component parameters
#'
#' Provides the ordered mapping from `synth_spec` component parameter names to
#' their user-facing display categories.
#'
#' @return A data frame with character columns:
#'   \itemize{
#'     \item{name}{The component parameter name in a `synth_spec` object.}
#'     \item{category}{The display category for that component.}
#'   }
#' @noRd
.synth_spec_component_metadata <- function() {
  
  return(data.frame(
    name = c(
      "default_regression_model",
      "default_classification_model",
      "custom_models",
      "default_regression_steps",
      "default_classification_steps",
      "custom_steps",
      "default_regression_sampler",
      "default_classification_sampler",
      "custom_samplers",
      "default_regression_noise",
      "default_classification_noise",
      "custom_noise",
      "default_regression_tuner",
      "default_classification_tuner",
      "custom_tuners",
      "default_extractor",
      "custom_extractors"
    ),
    category = c(
      "Models",
      "Models",
      "Models",
      "Recipes",
      "Recipes",
      "Recipes",
      "Samplers",
      "Samplers",
      "Samplers",
      "Noise",
      "Noise",
      "Noise",
      "Tuners",
      "Tuners",
      "Tuners",
      "Extractors",
      "Extractors"
    ),
    stringsAsFactors = FALSE
  ))
  
}


#' Build an HTML representation of a `synth_spec` object
#'
#' @param x A `synth_spec` object.
#' @return An `htmltools` tag object: an outer box containing a title block
#'   and a body block with one stacked group block per configured category.
#' @noRd
.synth_spec_html <- function(x) {
  
  display <- .synth_spec_display_data(x)
  
  return(
    htmltools::tags$div(
      class = "tidysynthesis-synth-spec",
      htmltools::tags$div(
        class = "tidysynthesis-synth-spec-title",
          display$title
        ),
      htmltools::tags$div(
        class = "collapse show tidysynthesis-synth-spec-body",
        .synth_spec_html_groups(display$components)
      )
    )
  )
}


#' Build stacked HTML component group blocks from display metadata
#'
#' @param components A data frame with `name` and `category` columns, as
#'   returned by `.synth_spec_display_data()`. Categories are stacked in the
#'   order they first appear.
#' @return An `htmltools` tag object, or `NULL` if there are no components.
#' @noRd
.synth_spec_html_groups <- function(components) {
  
  if (nrow(components) == 0) {
    
    return(NULL)
    
  }
  
  categories <- unique(components$category)
  
  return(
    htmltools::tagList(
      lapply(categories, function(category) {
        
        items <- components$name[components$category == category]
        
        return(.synth_spec_html_group(category = category, items = items))
        
      })
    )
  )
  
}


#' Build a single stacked HTML component group block
#'
#' @param category A category label, such as `"Models"`.
#' @param items A character vector of configured component names in
#'   `category`; only components that were actually specified appear here.
#' @return An `htmltools` tag object: a native `<details>` element with a
#'   category summary followed by one collapsible item per component name.
#' @noRd
.synth_spec_html_group <- function(category, items) {
  
  return(
    htmltools::tags$details(
      class = "tidysynthesis-synth-spec-group",
      open = NA,
      htmltools::tags$summary(
        class = "tidysynthesis-synth-spec-group-title",
        category
      ),
      htmltools::tags$div(
        class = "tidysynthesis-synth-spec-group-body",
        lapply(items, .synth_spec_html_item)
      )
    )
  )
  
}


#' Build a single collapsible HTML component item
#'
#' @param item A component name, such as `"default_regression_model"`.
#' @return An `htmltools` tag object: a native `<details>` element with the
#'   component name as both summary and expanded content.
#' @noRd
.synth_spec_html_item <- function(item) {
  
  return(
    htmltools::tags$details(
      class = "tidysynthesis-synth-spec-item-wrapper",
      htmltools::tags$summary(
        class = "tidysynthesis-synth-spec-item-toggle",
        item
      ),
      htmltools::tags$div(
        class = "tidysynthesis-synth-spec-item",
        item
      )
    )
  )
  
}