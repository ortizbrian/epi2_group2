comparative_stats_table <- function(
    t_df, dichot_var, cont_mean_cols, cont_median_cols, cat_cols, num_digits = 1, 
    docx_path = NA, var_order = NA){
  
  # Makes a summary statistics table. To get more comments later
  
  true_df <- t_df |>
    filter(!! rlang::sym(dichot_var))
  
  true_stats <- cont_mean_cols |>
    rlang::set_names(cont_mean_cols) |>
    purrr::map(
      ~ true_df |>
        meantables::mean_table(!! rlang::sym(.x)) |> 
        meantables::mean_format("mean (lcl - ucl)", digits = num_digits) |>
        dplyr::rename_at('response_var', ~'var') |>
        dplyr::select(var, n, formatted_stats) |>
        dplyr::mutate(
          n_missing = sum(is.na(true_df[[var]])),
          cat = 'Mean (95% CI)',
          formatted_stats = paste0(
            formatted_stats, "; ",
            "SD:", round(sd(na.omit(true_df[[var]])), num_digits), "; Range: ", 
            round(na.omit(min(true_df[[var]])), num_digits), ' - ', 
            round(na.omit(max(true_df[[var]])), num_digits)
          )
        ) |>
        dplyr::relocate(var, cat, n, n_missing, formatted_stats)
    ) |>
    append(
      cont_median_cols |>
        rlang::set_names(cont_median_cols) |>
        purrr::map( 
          ~ true_df |>  
            dplyr::summarise(
              var    = .x,
              n      = sum(!is.na(!! rlang::sym(.x))),
              n_miss = sum(is.na(!! rlang::sym(.x))),
              median = stats::median(!! rlang::sym(.x), na.rm = TRUE),
              lcl    = sort(na.omit(!! rlang::sym(.x)))[
                stats::qbinom(.025, 
                              length(na.omit(!! rlang::sym(.x))),
                              0.5)
              ],
              ucl    = sort(na.omit(!! rlang::sym(.x)))[
                stats::qbinom(.975, 
                              length(na.omit(!! rlang::sym(.x))), 
                              0.5)
              ]
            ) |> 
            meantables::mean_format(
              "median (lcl - ucl)", 
              digits = num_digits
            ) |> 
            dplyr::select(var, n, formatted_stats) |>
            dplyr::mutate(
              n_missing = sum(is.na(true_df[[var]])),
              cat = 'Median (95% CI)',
              formatted_stats = paste0(
                formatted_stats, "; ",
                "IQR: (", 
                round(summary(na.omit(true_df[[var]]))[2], num_digits), " - ", 
                round(summary(na.omit(true_df[[var]]))[5], num_digits), 
                "); Range: ", 
                round(min(na.omit(true_df[[var]])), num_digits), ' - ', 
                round(max(na.omit(true_df[[var]])), num_digits)
              )
            ) |>
            dplyr::relocate(var, cat, n, n_missing, formatted_stats)
        )
    ) |>
    append(cat_cols |>
             rlang::set_names(cat_cols) |> 
             purrr::map(
               ~ true_df |> 
                 dplyr::filter(!is.na(!! rlang::sym(.x))) |> 
                 freqtables::freq_table(!! rlang::sym(.x)) |> 
                 freqtables::freq_format(
                   recipe = "percent (lcl - ucl)", 
                   digits = num_digits
                 ) |> 
                 dplyr::select(var, cat, n, formatted_stats) |>
                 dplyr::mutate(
                   n_missing = sum(is.na(true_df[[.x]]))
                 ) |>
                 dplyr::relocate(var, cat, n, n_missing, formatted_stats)
             ) 
    )
  
  true_table <- purrr::map_dfr(
    .x = setdiff(colnames(true_df), 'id'),
    .f = ~ dplyr::bind_rows(true_stats[[.x]])
  ) |>
    select(-n_missing) |>
    rename_at(
      c('var', 'cat', 'n', 'formatted_stats'), 
      ~c(
        'Variable', 'Category', 'N - TRUE', 'Statistic (95% CI) - TRUE'
      )) |>
    ungroup()
  
  false_df <- t_df |>
    filter(!(!! rlang::sym(dichot_var)))
  
  false_stats <- cont_mean_cols |>
    rlang::set_names(cont_mean_cols) |>
    purrr::map(
      ~ false_df |>
        meantables::mean_table(!! rlang::sym(.x)) |> 
        meantables::mean_format("mean (lcl - ucl)", digits = num_digits) |>
        dplyr::rename_at('response_var', ~'var') |>
        dplyr::select(var, n, formatted_stats) |>
        dplyr::mutate(
          n_missing = sum(is.na(false_df[[var]])),
          cat = 'Mean (95% CI)',
          formatted_stats = paste0(
            formatted_stats, "; ",
            "SD:", round(sd(na.omit(false_df[[var]])), num_digits), "; Range: ", 
            round(na.omit(min(false_df[[var]])), num_digits), ' - ', 
            round(na.omit(max(false_df[[var]])), num_digits)
          )
        ) |>
        dplyr::relocate(var, cat, n, n_missing, formatted_stats)
    ) |>
    append(
      cont_median_cols |>
        rlang::set_names(cont_median_cols) |>
        purrr::map( 
          ~ false_df |>  
            dplyr::summarise(
              var    = .x,
              n      = sum(!is.na(!! rlang::sym(.x))),
              n_miss = sum(is.na(!! rlang::sym(.x))),
              median = stats::median(!! rlang::sym(.x), na.rm = TRUE),
              lcl    = sort(na.omit(!! rlang::sym(.x)))[
                stats::qbinom(.025, 
                              length(na.omit(!! rlang::sym(.x))),
                              0.5)
              ],
              ucl    = sort(na.omit(!! rlang::sym(.x)))[
                stats::qbinom(.975, 
                              length(na.omit(!! rlang::sym(.x))), 
                              0.5)
              ]
            ) |> 
            meantables::mean_format(
              "median (lcl - ucl)", 
              digits = num_digits
            ) |> 
            dplyr::select(var, n, formatted_stats) |>
            dplyr::mutate(
              n_missing = sum(is.na(false_df[[var]])),
              cat = 'Median (95% CI)',
              formatted_stats = paste0(
                formatted_stats, "; ",
                "IQR: (", 
                round(summary(na.omit(false_df[[var]]))[2], num_digits), " - ", 
                round(summary(na.omit(false_df[[var]]))[5], num_digits), 
                "); Range: ", 
                round(min(na.omit(false_df[[var]])), num_digits), ' - ', 
                round(max(na.omit(false_df[[var]])), num_digits)
              )
            ) |>
            dplyr::relocate(var, cat, n, n_missing, formatted_stats)
        )
    ) |>
    append(cat_cols |>
             rlang::set_names(cat_cols) |> 
             purrr::map(
               ~ false_df |> 
                 dplyr::filter(!is.na(!! rlang::sym(.x))) |> 
                 freqtables::freq_table(!! rlang::sym(.x)) |> 
                 freqtables::freq_format(
                   recipe = "percent (lcl - ucl)", 
                   digits = num_digits
                 ) |> 
                 dplyr::select(var, cat, n, formatted_stats) |>
                 dplyr::mutate(
                   n_missing = sum(is.na(false_df[[.x]]))
                 ) |>
                 dplyr::relocate(var, cat, n, n_missing, formatted_stats)
             ) 
    )
  
  false_table <- purrr::map_dfr(
    .x = setdiff(colnames(false_df), 'id'),
    .f = ~ dplyr::bind_rows(false_stats[[.x]])
  ) |> 
    select(-n_missing) |>
    rename_at(
      c('var', 'cat', 'n', 'formatted_stats'), 
      ~c(
        'Variable', 'Category', 'N - FALSE', 'Statistic (95% CI) - FALSE'
      )) |>
    ungroup()
  
  stats_table <- full_join(
    true_table, false_table, by=c('Variable', 'Category')
  ) |>
    mutate('Test-Statistic (p-value)' = NA)
  
  if (sum(!is.na(var_order)) == length(var_order)){
    stats_table <- stats_table |>
      arrange(factor(Variable, levels = var_order))
  }
  
  ft_table <- flextable::flextable(stats_table)
  flextable::save_as_docx(ft_table, path = docx_path)  
  
  stats_table <- stats_table |>
    DT::datatable(
      colnames = colnames(stats_table),
      options = list(
        pageLength = 20,
        columnDefs = list(
          # Hide row numbers column from view
          list(targets = 0, visible = FALSE)
        )
      )
    )
  
  stats_table
}