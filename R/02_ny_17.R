dc_17 <- dutchess[103:117] |>
  matrix(data = _, ncol = 3, byrow = TRUE) |>
  as_tibble(.name_repair = 'unique') |>
  `colnames<-`(c('candidate', 'votes', 'percent')) |>
  mutate(
    candidate = case_when(
      str_detect(candidate, 'Mike Lawler') ~ 'Mike Lawler',
      str_detect(candidate, 'Sean') ~ 'Sean Patrick Maloney',
      TRUE ~ 'Write-In'
    ),
    votes = str_remove_all(votes, pattern = ','),
    percent = str_remove_all(percent, pattern = '%')
  ) |>
  group_by(candidate) |>
  summarize(across(.fns = \(x) sum(as.numeric(x)))) |>
  mutate(
    county = 'Dutchess',
    .before = everything()
  )

rc_17 <- rockland[133:135] |>
  str_remove_all(pattern = fixed('. .')) |>
  str_remove_all(pattern = fixed(' . ')) |>
  str_remove_all(pattern = ',') |>
  str_squish() |>
  tibble(
    raw = _,
    n_words = str_count(raw, ' '),
    candidate = str_to_title(word(raw, 1, n_words - 1)),
    votes = as.numeric(word(raw, n_words, end = n_words)),
    percent = as.numeric(word(raw, n_words + 1, end = n_words + 1)),
  ) |>
  select(candidate, votes, percent) |>
  mutate(
    county = 'Rockland',
    .before = everything()
  )

wc_17 <- westchester[410:454] |>
  Filter(function(s) {
    !s %in% c('&nbsp ', '')
  }, x = _) |>
  vctrs::vec_slice(i = c(14:16, 25:27)) |>
  str_to_title() |>
  str_remove_all(pattern = ',') |>
  str_remove_all(pattern = '&Nbsp Totals') |>
  str_squish() |>
  str_remove_all(pattern = '%') |>
  matrix(data = _, ncol = 3, byrow = TRUE) |>
  as_tibble(.name_repair = 'unique') |>
  `colnames<-`(c('candidate', 'votes', 'percent')) |>
  mutate(
    votes = as.numeric(votes),
    percent = as.numeric(percent)
  ) |>
  mutate(
    county = 'Westchester',
    .before = everything()
  )

pc_17 <- putnam[196:210] |>
  str_squish() |>
  str_remove_all(pattern = ',') |>
  str_remove_all(pattern = '%') |>
  matrix(data = _, ncol = 3, byrow = TRUE) |>
  as_tibble(.name_repair = 'unique') |>
  `colnames<-`(c('candidate', 'votes', 'percent')) |>
  mutate(
    candidate = case_when(
      str_detect(candidate, 'Mike Lawler') ~ 'Mike Lawler',
      str_detect(candidate, 'Sean') ~ 'Sean Patrick Maloney',
      TRUE ~ 'Write-In'
    )
  ) |>
  group_by(candidate) |>
  summarize(across(.fns = \(x) sum(as.numeric(x)))) |>
  mutate(
    county = 'Putnam',
    .before = everything()
  )

bind_rows(
  rc_17, wc_17, pc_17, dc_17
) |> 
  group_by(candidate) |> 
  summarize(votes = sum(votes))

# bind_rows(
#   rc, wc, pc, dc
# )
