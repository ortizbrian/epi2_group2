summary_stats_table <- function(
    t_df, cont_mean_cols, cont_median_cols, cat_cols, num_digits = 1, 
    shade_colors = c('white', '#ededed'), docx_path = NA){

# Makes a summary statistics table. To get more comments later
  
  stats_list <- cont_mean_cols |>
    rlang::set_names(cont_mean_cols) |>
    purrr::map(
      ~ t_df |>
        meantables::mean_table(!! rlang::sym(.x)) |> 
        meantables::mean_format("mean (lcl - ucl)", digits = num_digits) |>
        dplyr::rename_at('response_var', ~'var') |>
        dplyr::select(var, n, formatted_stats) |>
        dplyr::mutate(
          cat = NA,
          statistic = 'Mean (95% CI)'
        ) |>
        dplyr::relocate(var, cat, statistic)
    ) |>
    append(cont_median_cols |>
             rlang::set_names(cont_median_cols) |>
             purrr::map( 
               ~ t_df |>  
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
                   cat = NA,
                   statistic = 'Median (95% CI)'
                 ) |>
                 dplyr::relocate(var, cat, statistic)
             )
    ) |>
    append(cat_cols |>
       rlang::set_names(cat_cols) |> 
       purrr::map(
         ~ t_df |> 
           dplyr::filter(!is.na(!! rlang::sym(.x))) |> 
           freqtables::freq_table(!! rlang::sym(.x)) |> 
           freqtables::freq_format(
             recipe = "percent (lcl - ucl)", 
             digits = num_digits
             ) |> 
           dplyr::select(var, cat, n, formatted_stats) |>
           dplyr::mutate(statistic = 'Frequency % (95% CI)') |>
           dplyr::relocate(var, cat, statistic)
       )
    )
  
  stats_table <- purrr::map_dfr(
    .x = setdiff(colnames(t_df), 'id'),
    .f = ~ dplyr::bind_rows(stats_list[[.x]])
    ) |>
    group_by(var) |> 
    mutate(shade = cur_group_id() %% 2 == 0) |> 
    select(shade, everything()) |> 
    rename_at(
      c('var', 'cat', 'statistic', 'n', 'formatted_stats'), 
      ~c(
        'Variable', 'Category', 'Statistic Type', 'N',
        'Statistic (95% CI)'
        )) |>
    ungroup()
  
  ft_table <- flextable::flextable(stats_table |> 
                                     select(-shade))
  flextable::save_as_docx(ft_table, path = docx_path)  
    
  stats_table <- stats_table |>
      DT::datatable(
      colnames = c(
        "Shade", "Variable", "Category", "Statistic Type", "N", 
        "Statistic (95% CI)"
      ),
      options = list(
        pageLength = 20,
        columnDefs = list(
          # Center n and formatted stats
          list(className = 'dt-center', targets = 4:5),
          # Hide row numbers column from view
          list(targets = 0, visible = FALSE),
          # Hide "shade" column from view
          list(targets = 1, visible = FALSE)
        )
      )
    )|> 
    DT::formatStyle(
      "shade",
      target = "row",
      backgroundColor = DT::styleEqual(c(0, 1), shade_colors)
    )
  
  stats_table
}