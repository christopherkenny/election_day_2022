dutchess <- read_html('https://elections.dutchessny.gov/2022/11/08/general-election-2022-unofficial-results/') |>
  html_nodes('tr td') |>
  html_text()

curl_download('https://rocklandgov.com/static_pages/elect_results/EL45.pdf', 'data/rockland.pdf')
rockland <- pdf_text('data/rockland.pdf') |>
  pluck(2) |>
  str_split('\n') |>
  pluck(1) |>
  str_squish()

westchester <- read_html('https://www.westchestergov.com/boe99/linkcounty.aspx') |>
  html_nodes('tr td') |>
  html_text()


putnam <- read_html('https://putnamboe.com/live-election-results/') |>
  html_nodes('tr td') |>
  html_text()