#' HTML Builders ----------------------------------------------------------

#' Identifies configured component parameters, associates each with its display
#' category, and collects synthesis settings for use by console or HTML
#' renderers.
#'
#' @param x A `synth_spec` object.
#' @return A list with:
#'   \itemize{
#'     \item{title}{A character title for the display.}
#'     \item{components_reg}{A data frame containing the configured regression
#'     component parameter names and their categories.}
#'     \item{components_class}{A data frame containing the configured
#'      classification component parameter names and their categories.}
#'     \item{components_custom}{A data frame containing the configured custom
#'     component parameter names and their categories.}
#'     \item{components_extractors}{A data frame containing the configured
#'     extractor component parameter names and their categories.}
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
    title = "synth_spec()",
    components_reg = configured[
      stringr::str_starts(configured$name, "default_regression"), ,
      drop = FALSE
    ],
    components_class = configured[
      stringr::str_starts(configured$name, "default_classification"), ,
      drop = FALSE
    ],
    components_custom = configured[
      stringr::str_starts(configured$name, "custom") &
        configured$name != "custom_extractors", ,
      drop = FALSE
    ],
    components_extractors = configured[
      configured$name %in% c("default_extractor", "custom_extractors"), ,
      drop = FALSE
    ],
    settings = settings
  ))
}


#' Define display metadata for synth_spec component parameters
#'
#' Provides the ordered mapping from `synth_spec` component parameter names to
#' their user-facing display category, component kind, and whether it is custom.
#'
#' @return A data frame with character columns:
#'   \itemize{
#'     \item{name}{The component parameter name in a `synth_spec` object.}
#'     \item{category}{The display category for that component.}
#'     \item{kind}{The kind of component (e.g., "model", "steps", "sampler",
#'     "noise", "tuner", "extractor").}
#'     \item{custom}{A logical value indicating whether the component is
#'      custom as defined by user.}
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
      "Model(s)",
      "Model(s)",
      "Model(s)",
      "Recipe(s)",
      "Recipe(s)",
      "Recipe(s)",
      "Sampler(s)",
      "Sampler(s)",
      "Sampler(s)",
      "Noise",
      "Noise",
      "Noise",
      "Tuner(s)",
      "Tuner(s)",
      "Tuner(s)",
      "Extractor(s)",
      "Extractor(s)"
    ),
    kind = c(
      "model",
      "model",
      "model",
      "steps",
      "steps",
      "steps",
      "sampler",
      "sampler",
      "sampler",
      "noise",
      "noise",
      "noise",
      "tuner",
      "tuner",
      "tuner",
      "extractor",
      "extractor"
    ),
    custom = c(
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      FALSE,
      TRUE
    ),
    stringsAsFactors = FALSE
  ))
}


#' HTML dependency for the `synth_spec` display.
#'
#' @return An `htmltools::htmlDependency` object.
#' @noRd
.synth_spec_html_dependency <- function() {
  htmltools::htmlDependency(
    name = "tidysynthesis-synth-spec",
    version = as.character(utils::packageVersion("tidysynthesis")),
    src = c(file = system.file("www", package = "tidysynthesis")),
    stylesheet = "style.css"
  )
}


#' Build an HTML representation of a `synth_spec` object
#'
#' @param x A `synth_spec` object.
#' @return An `htmltools` tag object with `tidysynthesis` CSS dependency
#' attached via `attachDependencies`. The tag has a title block, a "columns" row
#' with stacked group blocks for regression/classification/custom parameter
#' categories, and a secondary row with an extractors group block
#' and a settings block.
#' @noRd
.synth_spec_html <- function(x) {
  display <- .synth_spec_display_data(x)

  html <- (htmltools::tags$div(
    class = "tidysynthesis-synth-spec",
    htmltools::tags$div(
      class = "tidysynthesis-synth-spec-title",
      display$title
    ),
    htmltools::tags$div(
      class = "tidysynthesis-synth-spec-columns",
      # display regression related parameters in a collapsible
      if (nrow(display$components_reg) > 0) {
        .synth_spec_html_param_group(
          title = "Parameters: Regression",
          components = display$components_reg,
          x = x
        )
      },
      # display classification related parameters in a collapsible
      if (nrow(display$components_class) > 0) {
        .synth_spec_html_param_group(
          title = "Parameters: Classification",
          components = display$components_class,
          x = x
        )
      },
      # display custom parameters in a collapsible
      if (nrow(display$components_custom) > 0) {
        .synth_spec_html_param_group(
          title = "Parameters: Custom",
          components = display$components_custom,
          x = x
        )
      }
    ),
    htmltools::tags$div(
      class = "tidysynthesis-synth-spec-secondary-row",
      htmltools::tags$div(
        # display extractor related parameters in a collapsible
        class = "tidysynthesis-synth-spec-extractors",
        .synth_spec_html_groups(
          components = display$components_extractors,
          x = x
        )
      ),
      # display settings related parameters in a collapsible
      htmltools::tags$div(
        class = "tidysynthesis-synth-spec-settings-row",
        .synth_spec_html_settings(display$settings)
      )
    )
  )
  )

  htmltools::attachDependencies(
    html,
    .synth_spec_html_dependency()
  )
}


#' Build an HTML representation of a single parameter group within a
#' `synth_spec` object.
#'
#' @param title The title of the parameter group.
#' @param components A data frame with `name` and `category` columns, as
#'   returned by `.synth_spec_display_data()`.
#' @param x A `synth_spec` object.
#' @return An `htmltools` tag object: a native `<details>` element with a
#'   group summary followed by one collapsible item per component name.
#' @noRd
.synth_spec_html_param_group <- function(title, components, x) {
  htmltools::tags$details(
    class = "tidysynthesis-synth-spec-body",
    htmltools::tags$summary(
      class = "tidysynthesis-synth-spec-parameter-title",
      title
    ),
    htmltools::tags$div(
      class = "tidysynthesis-synth-spec-parameter-content",
      .synth_spec_html_groups(
        components = components,
        x = x
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
.synth_spec_html_groups <- function(components, x) {
  if (nrow(components) == 0) {
    return(NULL)
  }

  categories <- unique(components$category)

  return(
    htmltools::tagList(
      lapply(categories, function(category) {
        items <- components$name[components$category == category]

        displays <- unlist(
          lapply(items, .synth_spec_component_displays, x = x),
          recursive = FALSE
        )

        return(.synth_spec_html_group(category = category, displays = displays))
      })
    )
  )
}


#' Build a single stacked HTML component group block
#'
#' @param category A category label, such as `"Models"`.
#' @param displays A list of display metadata for the components in
#'   `category`; only components that were actually specified appear here.
#' @return An `htmltools` tag object: a native `<details>` element with a
#'   category summary followed by one collapsible item per distinct parameter.
#'   For `"Extractors"` category, `default_extractor` and `custom_extractors`
#'   share one category but are two distinct parameters, so group summary uses
#'   category label and item summary uses parameter name. For other categories,
#'   there is only one parameter associated, so group summary uses parameter 
#'   name.
#' @noRd
.synth_spec_html_group <- function(category, displays) {
  # Determine if category is Extractor and set group and parameter
  # headings accordingly
  is_extractor <- stringr::str_starts(category, "Extractor")
  group_heading <- if (is_extractor) category else displays[[1]][["parameter"]]

  # Extract unique parameter names from displays and group displays by parameter
  param_names <- vapply(displays, `[[`, character(1), "parameter")
  groups <- split(displays, factor(param_names, levels = unique(param_names)))

  # Build HTML items for each parameter within a group
  items <- Map(function(p, group) {
    # Handle heading for extractor
    param_heading <- if (is_extractor) p else category
    .synth_spec_html_item(param_heading, group)
  }, names(groups), groups)

  return(
    htmltools::tags$details(
      class = "tidysynthesis-synth-spec-group",
      htmltools::tags$summary(
        class = "tidysynthesis-synth-spec-group-title",
        group_heading
      ),
      htmltools::tags$div(
        class = "tidysynthesis-synth-spec-group-body",
        items
      )
    )
  )
}


#' Build a single collapsible HTML component item
#'
#' @param param_heading A category name, such as "Model(s)" or
#'  a component name, such as `"default_extractor"`.
#' @param displays A list of display metadata for the component specified by
#' `param_heading`.
#' @return An `htmltools` tag object: a native `<details>` element with
#'   `param_heading` as the summary, and each display's `label` and `details`
#'   rendered as expanded content.
#' @noRd
.synth_spec_html_item <- function(param_heading, displays) {
  return(
    htmltools::tags$details(
      class = "tidysynthesis-synth-spec-item-wrapper",
      htmltools::tags$summary(
        class = "tidysynthesis-synth-spec-item-toggle",
        param_heading
      ),
      lapply(displays, function(display) {
        htmltools::tags$div(
          class = "tidysynthesis-synth-spec-item",
          htmltools::tags$div(display[["label"]]),
          lapply(names(display[["details"]]), function(name) {
            htmltools::tags$div(paste0(
              name,
              ": ",
              .synth_spec_value_text(display[["details"]][[name]])
            ))
          })
        )
      })
    )
  )
}


#' Build the HTML settings block for a `synth_spec` object
#'
#' @param settings A named logical vector, as returned by
#'   `.synth_spec_display_data()`.
#' @return An `htmltools` tag object, or `NULL` if there are no settings.
#' @noRd
.synth_spec_html_settings <- function(settings) {
  displays <- .synth_spec_settings_displays(settings)

  if (length(displays) == 0) {
    return(NULL)
  }

  return(
    htmltools::tags$details(
      class = "tidysynthesis-synth-spec-group",
      htmltools::tags$summary(
        class = "tidysynthesis-synth-spec-group-title",
        "Settings"
      ),
      htmltools::tags$div(
        class = "tidysynthesis-synth-spec-group-body",
        lapply(displays, function(display) {
          .synth_spec_html_item(display[["parameter"]], list(display))
        })
      )
    )
  )
}


#' Metadata Extractors ---------------------------------------------------------

#' Build display metadata for the settings of a `synth_spec` object
#'
#' @param settings A named logical vector, as returned by
#'   `.synth_spec_display_data()`.
#' @return A list of display metadata for each setting.
#' @noRd
.synth_spec_settings_displays <- function(settings) {
  if (length(settings) == 0) {
    return(list())
  }

  return(
    lapply(
      names(settings),
      function(name) {
        return(list(
          parameter = name,
          label = .synth_spec_value_text(settings[[name]]),
          details = list()
        ))
      }
    )
  )
}


#' Build display metadata for each `synth_spec` component
#'
#' @param component_name The name of the component for which to build
#'  display metadata.
#' @param x A `synth_spec` object.
#' @return A list of display metadata for the specified component.
#' @noRd
.synth_spec_component_displays <- function(component_name, x) {
  metadata <- .synth_spec_component_metadata()
  entry <- metadata[metadata$name == component_name, , drop = FALSE]

  if (nrow(entry) != 1) {
    return(list())
  }

  value <- x[[component_name]]

  # If component is custom, call the custom display builder
  if (entry$custom) {
    return(
      .synth_spec_custom_displays(
        entries = value,
        parameter = component_name,
        kind = entry$kind
      )
    )
  }

  # For non-custom components, build display metadata based on the kind
  return(list(
    .synth_spec_kind_display(
      value = value,
      parameter = component_name,
      kind = entry$kind
    )
  ))
}


#' Build display metadata for a `synth_spec` component based on its kind.
#'
#' @param value The value of the component.
#' @param parameter The name of the component.
#' @param kind The kind of the component (e.g., "model", "steps", "sampler").
#' @return A list containing the display metadata for the component.
#' @noRd
.synth_spec_kind_display <- function(value, parameter, kind) {
  # Call the appropriate display builder based on the kind of component
  display <- switch(kind,
    "model" = .synth_spec_model_display(value = value),
    "steps" = .synth_spec_steps_display(value = value),
    "sampler" = .synth_spec_sampler_display(value = value),
    "noise" = .synth_spec_noise_display(value = value),
    "tuner" = .synth_spec_tuner_display(value = value),
    "extractor" = .synth_spec_extractor_display(value = value),
    list(
      label = .synth_spec_value_text(value),
      details = list()
    )
  )
  display[["parameter"]] <- parameter

  return(display[c("parameter", "label", "details")])
}


#' Build display metadata for custom `synth_spec` components.
#'
#' @param entries A list of custom component entries.
#' @param parameter The name of the custom component.
#' @param kind The kind of the custom component.
#' @return A list of display metadata for each custom component entry.
#' @noRd
.synth_spec_custom_displays <- function(entries, parameter, kind) {
  if (length(entries) == 0) {
    return(list())
  }

  displays <- Map(
    function(entry) {
      # Build the display metadata for the custom component entry
      # based on its kind
      display <- .synth_spec_kind_display(
        value = entry[[kind]],
        kind = kind,
        parameter = parameter
      )

      # Add variables to custom component detais
      display[["details"]] <- c(
        list(variables = paste(entry[["vars"]], collapse = ", ")),
        display[["details"]]
      )

      return(display)
    },
    entries
  )

  return(unname(displays))
}


#' Build display metadata for the model component of a `synth_spec` object.
#'
#' @param value The model component of the `synth_spec` object.
#' @return A list containing the display metadata for the model component.
#' @noRd
.synth_spec_model_display <- function(value) {
  if (!.is_model(value)) {
    return(list(
      label = .synth_spec_value_text(value),
      details = list()
    ))
  }

  # Get model class so function name can be formatted for display
  model_class <- utils::head(class(value), 1)

  return(list(
    label = .synth_spec_format_func_name(model_class),
    details = list(
      engine = value[["engine"]],
      mode = value[["mode"]]
    )
  ))
}


#' Build display metadata for the recipe component of a `synth_spec` object
#'
#' @param value The recipe steps component of the `synth_spec` object.
#' @return A list containing display metadata for the recipe steps component.
#' @noRd
.synth_spec_steps_display <- function(value) {
  # Find all function names present in the body of recipe function
  # and extract step functions
  function_names <- all.names(body(value), functions = TRUE)
  step_names <- grep("^step_", function_names, value = TRUE)

  if (length(step_names) == 0) {
    return(list(
      label = "user-defined recipe",
      details = list()
    ))
  }

  step_summary <- paste0(
    unlist(
      lapply(unique(step_names), .synth_spec_format_func_name)
    ),
    collapse = " + "
  )

  return(list(
    label = step_summary,
    details = list()
  ))
}


#' Build display metadata for the tuner component of a `synth_spec` object.
#'
#' @param value The tuner component of the `synth_spec` object.
#' @return A list containing the display metadata for the tuner component.
#' @noRd
.synth_spec_tuner_display <- function(value) {
  return(list(
    label = "tuner",
    details = list(
      v = value[["v"]],
      grid = value[["grid"]],
      metrics = value[["metrics"]]
    )
  ))
}


#' Build display metadata for the sampler component of a `synth_spec` object.
#'
#' @param value The sampler component of the `synth_spec` object.
#' @return A list containing the display metadata for the sampler component.
#' @noRd
.synth_spec_sampler_display <- function(value) {
  # Get namespaced function name for the sampler, if available
  namespace_func_name <- .synth_spec_namespace_function(value)

  if (is.na(namespace_func_name)) {
    return(list(
      label = "user-defined sampler",
      details = list(
        func_body = deparse(body(value)),
        arguments = names(formals(value))
      )
    ))
  } else {
    return(list(
      label = namespace_func_name,
      details = list(
        arguments = names(formals(value))
      )
    ))
  }
}


#' Build display metadata for the noise component of a `synth_spec` object.
#'
#' @param value The noise component of the `synth_spec` object.
#' @return A list containing the display metadata for the noise component.
#' @noRd
.synth_spec_noise_display <- function(value) {
  noise_function <- value[["noise_func"]]

  if (is.null(noise_function)) {
    return(list(label = "none", details = list()))
  }

  # Get namespaced function name for the noise function, if available
  namespace_func_name <- .synth_spec_namespace_function(noise_function)

  if (is.na(namespace_func_name)) {
    return(list(
      label = "user-defined noise",
      details = list(
        func_body = deparse(body(noise_function)),
        arguments = names(formals(noise_function))
      )
    ))
  }

  return(list(
    label = namespace_func_name,
    details = list(arguments = names(formals(noise_function)))
  ))
}


#' Build display metadata for the extractor component of a `synth_spec` object.
#'
#' @param value The extractor component of the `synth_spec` object.
#' @return A list containing the display metadata for the extractor component.
#' @noRd
.synth_spec_extractor_display <- function(value) {
  # Get namespaced function name for the extractor function, if available
  namespace_func_name <- .synth_spec_namespace_function(value)

  if (is.na(namespace_func_name)) {
    return(list(
      label = "user-defined extractor",
      details = list(
        func_body = deparse(body(value)),
        arguments = names(formals(value))
      )
    ))
  } else {
    return(list(
      label = namespace_func_name,
      details = list(
        arguments = names(formals(value))
      )
    ))
  }
}


#' Helpers ----------------------------------------------------------

#' Look up a function's exported name from its own namespace
#'
#' @param value A function.
#' @return The name of a matching binding in `value`'s namespace, or `NA` if
#'   `value` isn't a namespaced function or no match is found.
#' @noRd
.synth_spec_namespace_function <- function(value) {
  if (!is.function(value)) {
    return(NA_character_)
  }

  env <- environment(value)

  # Package namespaces expose a searchable, stable set of bindings
  if (is.null(env) || !isNamespace(env)) {
    return(NA_character_)
  }

  # Get all candidate function names in the namespace
  candidates <- ls(envir = env, all.names = TRUE)

  for (name in candidates) {
    candidate <- get(name, envir = env)

    # Check if candidate function is identical to input function
    if (is.function(candidate) && .identical_funcs(value, candidate)) {
      return(.synth_spec_format_func_name(name, env))
    }
  }

  return(NA_character_)
}


#' Format a function name with its namespace for display purposes.
#'
#' @param name The name of the function.
#' @param env The environment of the function, typically its namespace.
#' @return A formatted string representing the function with its namespace.
#' @noRd
.synth_spec_format_func_name <- function(name, env = NULL) {
  if (is.null(env)) {
    # Try to find the package using the search path
    pkgs <- utils::find(name)

    if (length(pkgs) == 0) {
      return(name)
    }
    pkg_name <- sub("package:", "", pkgs[1])
  } else {
    # Get the package name from its namespace
    pkg_name <- getNamespaceName(env)
  }

  if (is.null(pkg_name) || pkg_name == "") {
    return(name)
  } else {
    return(paste0("function: ", pkg_name, "::", name, "()"))
  }
}


#' Build display text for a value.
#'
#' @param value The value to build display text for.
#' @return A character string representing the value for display purposes.
#' @noRd
.synth_spec_value_text <- function(value) {
  if (length(value) == 0) {
    return("none")
  }

  if (inherits(value, "metric_set")) {
    return(paste(names(attr(value, "metrics")), collapse = ", "))
  }

  if (is.function(value)) {
    return("function")
  }

  if (is.data.frame(value)) {
    return(paste0(nrow(value), " x ", ncol(value), " data frame"))
  }

  if (is.list(value)) {
    return(.synth_spec_arguments_text(value))
  }

  if (is.character(value)) {
    return(paste(value, collapse = ", "))
  }

  return(paste(format(value), collapse = ", "))
}


#' Build display text for a list of arguments.
#'
#' @param value A list of arguments.
#' @return A character string representing the arguments for display purposes.
#' @noRd
.synth_spec_arguments_text <- function(value) {
  if (length(value) == 0) {
    return("none")
  }

  argument_names <- names(value)

  if (is.null(argument_names)) {
    # Format values for unamed list
    return(
      paste(
        vapply(value, .synth_spec_value_text, character(1)),
        collapse = ", "
      )
    )
  }

  paste(
    # Format values for named list
    vapply(
      seq_along(value),
      function(i) {
        paste0(
          argument_names[[i]],
          " = ",
          .synth_spec_value_text(value[[i]])
        )
      },
      character(1)
    ),
    collapse = ", "
  )
}
