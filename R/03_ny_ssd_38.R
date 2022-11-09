rc_38 <- rockland[149:151] |>
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
