
## Part 1
library(httr)

res <- httr::GET(url = "https://adventofcode.com/2024/day/3/input", httr::add_headers(.headers=headers), httr::set_cookies(.cookies = cookies))

con <- httr::content(res, "text")

# ID valid instructions and extract
valid_instructions <- stringr::str_extract_all(con,  "mul\\(\\d+,\\d+\\)")[[1]]

# Calculate product
result <- tidyr::tibble(
  "left_num" = gsub("mul\\(", "", gsub(",.*", "", valid_instructions)),
  "right_num" = gsub("\\)", "", gsub(".*,", "", valid_instructions))
) |> 
  dplyr::summarise(sum(as.numeric(left_num)*as.numeric(right_num))) |>
  dplyr::pull()

print(result)

## Part 2

# Tokenize the input
tokens <- stringr::str_extract_all(con,  "mul\\(\\d+,\\d+\\)|don't\\(\\)|do\\(\\)")[[1]]

enabled_state <- purrr::accumulate(
  tokens,
  ~ if (.y == "don't()") FALSE else if (.y == "do()") TRUE else .x,
  .init = TRUE
)[-1] # remove the initial state

results <- purrr::map2_dbl(tokens, enabled_state, ~ {
  if (.y && stringr::str_detect(.x, "\\d+")[[1]]) {
    nums <- as.numeric(stringr::str_extract_all(.x, "\\d+")[[1]])
    return(nums[1] * nums[2])
  } else {
    return(0)
  }
})

print(sum(results))

