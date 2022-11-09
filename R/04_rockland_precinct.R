offices <- c(
  'Governor and Lieutenant Governor', 'Comptroller',
  'Attorney General',
  'United States Senator',
  'Justices of the Supreme Court 9th Judicial District',
  'Representative in Congress 17th Congressional District',
  'State Senator 38th Senatorial District',
  'State Senator 40th Senatorial District',
  'Member of Assembly 96th Assembly District',
  'Member of Assembly 97th Assembly District',
  'Member of Assembly 98th Assembly District',
  'Member of Assembly 99th Assembly District',
  'Town Council Stony Point',
  'Town Justice Ramapo',
  'Supt. of Highways Stony Point',
  'Village Trustee Village of Nyack',
  'Village Trustee Village of Piermont',
  'Village Trustee Village of Suffern',
  'Village Trustee Village of Haverstraw',
  'Village Trustee Village of Sloatsburg',
  'Village Justice Village of Suffern',
  'Proposal one, a proposition',
  'Proposal two Stony Point'
)

rcp <- st_read('data/Cty_ED_Mar_5_2021.shp')

curl_download('https://rocklandgov.com/static_pages/elect_results/EL30.pdf', 'data/rockland_precinct.pdf')
rockland_prec <- pdf_text('data/rockland_precinct.pdf') |>
  str_split('\n') 
lens <- lengths(rockland_prec)

tb <- tibble(
  text = rockland_prec |>
    unlist() |> 
    str_squish(),
  page = sapply(seq_along(lens), \(i) rep(i, lens[i])) |> unlist()
) |> 
  mutate(
    office = ifelse(text %in% offices, text, NA_character_),
    ed = ifelse(str_sub(text, 1, 1) %in% 0:9, text, NA_character_)
  ) |> 
  fill(ed, office) |> 
  mutate(
    ED_KEY = word(ed, 2, -1), 
    ED_KEY = str_remove(ED_KEY, '\\s*\\([^\\)]+\\)'), 
    ED_KEY = case_when(
      str_detect(ED_KEY, 'Clarkstown') ~ paste0('C', str_pad(word(ED_KEY, 2, 2), 2, pad = '0')),
      str_detect(ED_KEY, 'Haverstraw') ~ paste0('H', str_pad(word(ED_KEY, 2, 2), 2, pad = '0')),
      str_detect(ED_KEY, 'Ramapo') ~ paste0('R', str_pad(word(ED_KEY, 2, 2), 2, pad = '0')),
      str_detect(ED_KEY, 'Orangetown') ~ paste0('O', str_pad(word(ED_KEY, 2, 2), 2, pad = '0')),
      str_detect(ED_KEY, 'Stony Point') ~ paste0('S', str_pad(word(ED_KEY, 3, 3), 2, pad = '0')),
      TRUE ~ NA_character_
    ), # No data for O31 (small vote count...) and precinct R109 doesn't exist
    has_votes = str_detect(text, fixed(' . ')),
    text = str_squish(text),
    text = ifelse(has_votes & !is.na(office), str_remove_all(text, pattern = fixed('. .')), text),
    text = ifelse(has_votes & !is.na(office), str_remove_all(text, pattern = fixed(' . ')), text),
    text = ifelse(has_votes & !is.na(office), str_remove_all(text, pattern = ','), text),
    text = str_squish(text),
    has_candidate = has_votes & !is.na(office) &
      !str_detect(text, 'WRITE-IN') & !str_detect(text, 'Total') &
      !str_detect(text, 'Over Votes') & !str_detect(text, 'Under Votes'),
    n_words = str_count(text, ' '),
    candidate = ifelse(has_candidate, word(text, 1, n_words - 1), NA_character_), #str_to_title?
    votes = ifelse(has_candidate, as.numeric(word(text, n_words, end = n_words)), NA_real_),
    percent = ifelse(has_candidate, as.numeric(word(text, n_words + 1, end = n_words + 1)), NA_real_),
    candidate = str_remove(candidate, fixed(' (DEM)')),
    candidate = str_remove(candidate, fixed(' (REP)')),
    candidate = str_remove(candidate, fixed(' (CON)')),
    candidate = str_remove(candidate, fixed(' (WOR)'))
  )

ny17_prec <- tb |> 
  filter(office == 'Representative in Congress 17th Congressional District', has_candidate) |> 
  select(ED_KEY, candidate, votes) |> 
  group_by(ED_KEY, candidate) |> 
  summarize(votes = sum(votes, na.rm = TRUE), .groups = 'drop') |> 
  pivot_wider(id_cols = ED_KEY, names_from = candidate, values_from = votes) 

ny17_prec <- left_join(rcp, ny17_prec, by = 'ED_KEY')


ny17_prec |> 
  ggplot() + 
  geom_sf(aes(fill = `Sean Patrick Maloney` / (`Sean Patrick Maloney` + `Mike Lawler`)), color = NA) + 
  scale_fill_party_c(name = 'NY17 Vote share') + 
  theme_map() + 
  labs(caption = '@chris_t_kenny\nPreliminary Results as of 2022-11-09, 9:34AM')

ggsave('figs/rockland_ny17.png', width = 9, height = 9)


ssd38_prec <- tb |> 
  filter(office == 'State Senator 38th Senatorial District', has_candidate) |> 
  select(ED_KEY, candidate, votes) |> 
  group_by(ED_KEY, candidate) |> 
  summarize(votes = sum(votes, na.rm = TRUE), .groups = 'drop') |> 
  pivot_wider(id_cols = ED_KEY, names_from = candidate, values_from = votes) 

ssd38_prec <- left_join(rcp, ssd38_prec, by = 'ED_KEY') |> 
  filter(!is.na(`Bill Weber`))

ssd38_prec |> 
  ggplot() + 
  geom_sf(aes(fill = `Elijah Reichlin-` / (`Elijah Reichlin-` + `Bill Weber`)), color = NA) + 
  scale_fill_party_c(name = 'NY SSD 38 Vote share') + 
  theme_map() + 
  labs(caption = '@chris_t_kenny\nPreliminary Results as of 2022-11-09, 10:09AM')
