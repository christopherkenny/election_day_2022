dutchess <- read_html('https://elections.dutchessny.gov/2022/11/08/general-election-2022-unofficial-results/') |>
  html_nodes('tr td') |>
  html_text()

curl_download('https://rocklandgov.com/static_pages/elect_results/EL45.pdf', 'data/rockland.pdf')
rockland <- pdf_text('data/rockland.pdf') |>
  str_split('\n') |>
  unlist() |> 
  str_squish()

westchester <- read_html('https://www.westchestergov.com/boe99/linkcounty.aspx') |>
  html_nodes('tr td') |>
  html_text()


putnam <- read_html('https://putnamboe.com/live-election-results/') |>
  html_nodes('tr td') |>
  html_text()

nassau <- read_html('https://app.nassaucountyny.gov/BOE/results/election2.html') |> 
  html_text() |> 
  str_split('\r\n') |> 
  unlist() |> 
  str_squish()

suffolk <- read_html('https://www.elections.ny.gov/Suffolk/ElectionNightReporting.html#at/67713ca6-e0a3-48c3-9917-16485efdf454/ar/1/ct/1001') |> 
  html_elements('tr td')
