
## Part 1
# Get data
library(httr)

res <- httr::GET(url = "https://adventofcode.com/2024/day/2/input", httr::add_headers(.headers=headers), httr::set_cookies(.cookies = cookies))

con <- httr::content(res, "text")

lines <- strsplit(con, "\n")[[1]]

numbers <- purrr::map(lines, ~ strsplit(.x, split = " ")[[1]] |> as.numeric())
  
is_safe <- function(numbers, use_dampener = F) {
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


