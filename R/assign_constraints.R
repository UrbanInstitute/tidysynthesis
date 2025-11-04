#' Assign constraints to a synthetic data frame for numeric variables
#'
#' @param synth_data A synthetic data set
#' @param constraints A list of constraints created by the `constraints` 
#' constructor.
#'
#' @return A synthetic data frame with a `min` and `max` column added.
#'
#' @importFrom rlang .data
assign_constraints_num <- function(synth_data, constraints) {

  num_constraints <- nrow(constraints)

  # Create a new min col and max col for each constraint
  synth_data <- synth_data |>
    dplyr::mutate(
      .assigned_min = -Inf,
      .assigned_max = Inf
    )


  for (cond in 1:num_constraints) {

    synth_data <- synth_data |>
      dplyr::mutate(
        .assigned_min = dplyr::case_when(
          is.na(eval(parse(text = constraints[cond, ]$conditions))) 
            ~ .data$.assigned_min,
          eval(parse(text = constraints[cond, ]$conditions)) &
            constraints[cond, ]$min > .data$.assigned_min 
            ~ constraints[cond, ]$min,
          TRUE ~ .data$.assigned_min
        ),

        .assigned_max = dplyr::case_when(
          is.na(eval(parse(text = constraints[cond, ]$conditions))) 
            ~ .data$.assigned_max,
          eval(parse(text = constraints[cond, ]$conditions)) &
            constraints[cond, ]$max < .data$.assigned_max 
            ~ constraints[cond, ]$max,
          TRUE ~ .data$.assigned_max
        )
      )

  }

  return(synth_data)

}


#' 
#' Assign constraints to a synthetic data frame for categorical variables.
#' 
#' This function serves two functions. First, it maps each row in the synthetic
#' data to an integer `.condition_id` that determines how to enforce categorical
#' constraints. Second, it creates a new dataframe, `condition_defs`, that stores
#' information about how to implement categorical constraints for a specific
#' `.condition_id` value. 
#'
#' @param synth_data A synthetic data set
#' @param constraints A list of constraints created by the `constraints` 
#' constructor.
#'
#' @return A list of two data.frames: `synth_data` and `condition_defs`
#'
#' @importFrom rlang .data
#' 
assign_constraints_cat <- function(synth_data, constraints) {
  
  cat_conditions <- list()
  n_constraints <- nrow(constraints)
  col_names <- paste0("cc", 1:n_constraints)
  
  # first, evaluate all conditions on synthetic data to map rows to condition
  # combination identifiers
  for (ix in seq_len(n_constraints)) {
    
    # note: according to stackoverflow / advanced R, there's not an elegant solution
    # to dynamically creating multiple columns with dynamic names using across()
    # while simultaneously using dynamic evaluation of constraint conditions.
    new_col <- dplyr::mutate(
      synth_data, 
      .condition_res = dplyr::case_when(
        is.na(eval(parse(text = constraints[ix, ]$conditions))) ~ FALSE,
        TRUE ~ eval(parse(text = constraints[ix, ]$conditions))
      ),
      .keep = "none"
    )
    
    cat_conditions[col_names[ix]] <- new_col
    
  }
  
  # next, assign identifiers to these conditions...
  conditions <- dplyr::bind_cols(cat_conditions) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(col_names))) |> 
    dplyr::mutate(.condition_id = dplyr::cur_group_id()) |> 
    dplyr::ungroup()
  
  # ...and select only the unique combinations (to avoid evaluating every
  # potential constraint combination)
  condition_defs <- conditions |> 
    dplyr::distinct() 
  
  # based on these identifiers, add dummy variables to make it possible to 
  # link constraints to the condition combinations where they appear
  constraint_types <- dplyr::bind_cols(
    constraints, 
    data.frame(diag(n_constraints)) |> 
      stats::setNames(col_names) |> 
      dplyr::mutate(dplyr::across(dplyr::everything(), as.logical))
  )
  
  # expand the condition definitions using the constraint specifications
  condition_defs_expanded <- dplyr::bind_rows(
    purrr::map(
      # for each constraint...
      .x = seq_len(n_constraints), 
      .f = \(x) {
        # join the condition defs to their specification using the dummy variable
        dplyr::left_join(
          condition_defs, 
          dplyr::select(
            constraint_types[x, ],
            dplyr::all_of(
              c("var", "allowed", "forbidden", "conditions", paste0("cc", x))
            )
          ),
          by=paste0("cc", x)
        )
      }
    )
  )
  
  # create condition definitions by...
  condition_defs_final <- condition_defs_expanded |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(".condition_id")))) |>
    dplyr::summarise(
      # collecting all allowed and forbidden values by case...
      allowed_vals = list(.data[["allowed"]][!is.na(.data[["allowed"]])]),
      allowed_mode = any(!is.na(.data[["allowed"]])),
      forbidden_vals = list(.data[["forbidden"]][!is.na(.data[["forbidden"]])]),
      forbidden_mode = any(!is.na(.data[["forbidden"]]))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # define the condition mode where allowed precedes forbidden precedes none
      .condition_mode = dplyr::case_when(
        allowed_mode ~ "inclusion",
        forbidden_mode ~ "exclusion", 
        TRUE ~ "none"
      ),
      # extract relevant comparison values based on condition mode
      .condition_vals = dplyr::case_when(
        .condition_mode == "inclusion" ~ allowed_vals,
        .condition_mode == "exclusion" ~ forbidden_vals,
        TRUE ~ NA
      )
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(".condition_id", ".condition_mode", ".condition_vals")
      )
    )
  
  # return values
  return(
    list(
      "synth_data" = dplyr::mutate(
        synth_data, .condition_id = conditions[[".condition_id"]]
      ),
      "condition_defs" = condition_defs_final
    )
  )
  
}

