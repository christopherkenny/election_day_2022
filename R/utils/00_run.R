run_17 <- function() {
  source(here('R/01_counties.R'))
  source(here('R/02_ny_17.R'))
  
  list(
    county = bind_rows(
        rc_17, wc_17, pc_17, dc_17
      ),
    total = bind_rows(
      rc_17, wc_17, pc_17, dc_17
    ) |> 
      group_by(candidate) |> 
      summarize(votes = sum(votes))
  )

}

tab_17 <- function(l) {
  x <- l[['county']] |> 
    mutate(
      candidate = case_when(
        str_detect(candidate, 'Sean') ~ 'SPM_dem',
        str_detect(candidate, 'Mike') ~ 'ML_rep',
        TRUE ~ 'write_in'
      )
    ) |> 
    pivot_wider(id_cols = county, names_from = candidate, values_from = votes) |> 
    replace_na(list(write_in = 0)) |> 
    mutate(dvs = SPM_dem /( SPM_dem + ML_rep)) 
  
  xsum <- tibble(county = 'TOTAL', SPM_dem = sum(x$SPM_dem), ML_rep = sum(x$ML_rep),
                 write_in = sum(x$write_in), 
                 dvs = SPM_dem / (SPM_dem + ML_rep))
  
  bind_rows(x, xsum) |> 
    gt() |> 
    cols_hide(write_in) |> 
    data_color(columns = dvs, colors = scales::col_numeric(palette = as.character(ggredist$partisan), domain = c(0, 1))) |> 
    cols_label(
      county = 'County',
      SPM_dem = 'Sean Patrick Maloney',
      ML_rep = 'Mike Lawler',
      dvs = 'Dem. Share'
    ) |> 
    fmt_number(columns = c(SPM_dem, ML_rep), decimals = 0) |> 
    fmt_percent(dvs, decimals = 1) 
}

run_ssd_38 <- function() {
  source(here('R/01_counties.R'))
  source(here('R/03_ny_ssd_38.R'))
  
  rc_38
}
