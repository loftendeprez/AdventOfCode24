
## Part 1
# Get data
library(httr)

cookies = c(
  `_ga_MHSNPJKWC7` = "GS1.2.1733242729.1.1.1733244862.0.0.0",
  `_ga` = "GA1.2.451528456.1733242729",
  `_gid` = "GA1.2.46912567.1733242729",
  session = "53616c7465645f5f7b5013075a9b2ef2fde2ae48dcedc527f95940fd7e54ce3c57c1424c5244d743de0a65099c3139c2cf640df250fd740360f70a29d5e59720"
)

headers = c(
  Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  `Sec-Fetch-Site` = "same-origin",
  `Sec-Fetch-Dest` = "document",
  `Accept-Language` = "en-US,en;q=0.9",
  `Sec-Fetch-Mode` = "navigate",
  `User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/18.1.1 Safari/605.1.15",
  `Accept-Encoding` = "gzip, deflate, br",
  Referer = "https://adventofcode.com/2024/day/2",
  Priority = "u=0, i"
)

res <- httr::GET(url = "https://adventofcode.com/2024/day/2/input", httr::add_headers(.headers=headers), httr::set_cookies(.cookies = cookies))

con <- httr::content(res, "text")

lines <- strsplit(con, "\n")[[1]]

line_is_safe <- function(line) {
  numbers <- strsplit(line, split = " ")[[1]] |>
    as.numeric()
  
  is_asc <- all(diff(numbers) > 0)
  
  is_desc <- all(diff(numbers) < 0)
  
  is_sub_three <- all(diff(numbers) < 4 & diff(numbers) > -4)
  
  if ((is_asc | is_desc) & is_sub_three) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
  
}


safe_count <- lines |>
  purrr::map_lgl(line_is_safe) |>
  sum()

print(safe_count)

## Part 2 - problem dampener

line_is_safe_with_dampener <-  function(line) {
  numbers <- strsplit(line, split = " ")[[1]] |>
    as.numeric()
  
  is_safe <- FALSE
  for (i in seq_along(numbers)) {
    temp_numbers <- numbers[-i]
    
    is_asc <- all(diff(temp_numbers) > 0)
    
    is_desc <- all(diff(temp_numbers) < 0)
    
    is_sub_three <- all(diff(temp_numbers) < 4 & diff(temp_numbers) > -4)
    
    if ((is_asc | is_desc) & is_sub_three) {
      is_safe <- TRUE
      break
    } 
  }
  
  return(is_safe)
  
}

safe_count_with_dampener <- lines |>
  purrr::map_lgl(line_is_safe_with_dampener) |>
  sum()

print(safe_count_with_dampener)


