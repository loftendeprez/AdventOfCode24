

## Part 1

# (Add headers and cookie)

res <- httr::GET(url = "https://adventofcode.com/2024/day/1/input", httr::add_headers(.headers=headers), httr::set_cookies(.cookies = cookies))

con <- httr::content(res, "text")

vals <- strsplit(con, "   |\n")

df <- data.frame(matrix(strsplit(con, "   |\n")[[1]], ncol = 2, byrow = T)) |>
  dplyr::rename(
    "first" = 1,
    "second" = 2
  )

first_ordered <- df |>
  dplyr::select(first) |>
  dplyr::arrange(first) |>
  dplyr::mutate(first = as.numeric(first))

second_ordered <- df |>
  dplyr::select(second) |>
  dplyr::arrange(second) |>
  dplyr::mutate(second = as.numeric(second))

df_ordered <- data.frame(
  "first" = first_ordered,
  "second" = second_ordered
)

difference <- df_ordered |>
  dplyr::mutate(difference =  abs(second - first)) |>
  dplyr::summarise(sum(difference)) |>
  dplyr::pull()

print(difference)

## Part 2

second_counts <- df_ordered |>
  dplyr::group_by(second) |>
  dplyr::summarise(second_count = dplyr::n())

similarity_score_sum <- df_ordered |>
  dplyr::select(first) |>
  dplyr::left_join(second_counts, by = c("first" = "second")) |>
  dplyr::mutate(
    similarity_score = first * second_count
  ) |>
  dplyr::summarise(sum(similarity_score, na.rm = T)) |>
  dplyr::pull()

print(similarity_score_sum)

