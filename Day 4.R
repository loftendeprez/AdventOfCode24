
## Part 1
con <- clipr::read_clip()

calc_xmax_count <- function(input_grid) {
  
  characters <- purrr::map(input_grid, ~ strsplit(.x, "")[[1]])
  grid <- do.call(rbind, characters)
  
  nrow <- nrow(grid)
  ncol <- ncol(grid)
  
  # Function to extract all diagonals
  extract_diagonals <- function(mat, direction = "left") {
    shifts <- seq(-(nrow - 1), ncol - 1)
    purrr::map(shifts, ~ {
      if (direction == "left") {
        # Extract top-left to bottom-right diagonals
        purrr::map_chr(seq_len(nrow), ~ {
          i <- .x
          j <- i + .y
          if (j > 0 && j <= ncol) mat[i, j] else NA
        }, .y = .x) |> na.omit() |> paste0(collapse = "")
      } else if (direction == "right") {
        # Extract top-right to bottom-left diagonals
        purrr::map_chr(seq_len(nrow), ~ {
          i <- .x
          j <- ncol - i + 1 + .y
          if (j > 0 && j <= ncol) mat[i, j] else NA
        }, .y = .x) |> na.omit() |> paste0(collapse = "")
      } else {
        stop("Unknown direction")
      }
    }) |> purrr::discard(~ nchar(.x) < 4) # Keep only strings of sufficient length
  }
  
  # Function to count matches in a list of strings
  count_matches <- function(strings, pattern) {
    purrr::map_dbl(strings, ~ stringr::str_count(.x, pattern)) |> sum()
  }
  
  # Horizontal and vertical strings
  horiz_strings <- apply(grid, 1, paste0, collapse = "")
  vert_strings <- apply(grid, 2, paste0, collapse = "")
  
  # Diagonal strings (both directions)
  left_diag_strings <- extract_diagonals(grid, direction = "left")
  right_diag_strings <- extract_diagonals(grid, direction = "right")
  
  
  patterns <- c("XMAS", "SAMX")
  total_count <- 0
  for (pattern in patterns) {
    total_count <- total_count +
      count_matches(horiz_strings, pattern) +
      count_matches(vert_strings, pattern) +
      count_matches(left_diag_strings, pattern) +
      count_matches(right_diag_strings, pattern)
  }
  
  return(total_count)
}

print(calc_xmax_count(con))

## Part 2 X-MAS

calc_x_max_count <- function(input_grid) {
  
  characters <- purrr::map(input_grid, ~ strsplit(.x, "")[[1]])
  grid <- do.call(rbind, characters)
  
  nrow <- nrow(grid)
  ncol <- ncol(grid)
  
  xmax_count <- 0
  
  # Go through all subgrids
  for (i in 2:(nrow-1)) {
    for (j in 2:(ncol-1)) {
      if (grid[i, j] == "A" && (
        (grid[i-1,j-1] == "M" && grid[i+1,j+1] == "S" && grid[i-1,j+1] == "M" && grid[i+1,j-1] == "S") || # forward diagonal
        (grid[i-1,j-1] == "S" && grid[i+1,j+1] == "M" && grid[i-1,j+1] == "S" && grid[i+1,j-1] == "M") || # backward diagonal
        (grid[i-1,j-1] == "M" && grid[i+1,j+1] == "S" && grid[i-1,j+1] == "S" && grid[i+1,j-1] == "M") || # mixed 1
        (grid[i-1,j-1] == "S" && grid[i+1,j+1] == "M" && grid[i-1,j+1] == "M" && grid[i+1,j-1] == "S") # mixed 2
      )
      ) {
        xmax_count <- xmax_count + 1
      }
    }
  }

  return(xmax_count)
}

print(calc_x_max_count(con))



