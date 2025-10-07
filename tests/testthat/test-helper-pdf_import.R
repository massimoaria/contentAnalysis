# Helper functions for PDF import tests

#' Create mock PDF data for testing
#' @noRd
create_mock_pdf_data <- function(n_rows = 100, n_columns = 2, page_width = 600) {
  if (n_columns == 1) {
    data.frame(
      x = runif(n_rows, 50, page_width - 50),
      y = sort(runif(n_rows, 50, 700)),
      width = runif(n_rows, 20, 100),
      height = rep(12, n_rows),
      text = replicate(n_rows, paste(sample(letters, 5, replace = TRUE), collapse = "")),
      stringsAsFactors = FALSE
    )
  } else {
    col_width <- page_width / n_columns
    column_data_list <- lapply(1:n_columns, function(col) {
      x_start <- (col - 1) * col_width + 50
      x_end <- col * col_width - 50
      rows_per_col <- n_rows %/% n_columns
      data.frame(
        x = runif(rows_per_col, x_start, x_end),
        y = sort(runif(rows_per_col, 50, 700)),
        width = runif(rows_per_col, 20, 100),
        height = rep(12, rows_per_col),
        text = replicate(rows_per_col, paste(sample(letters, 5, replace = TRUE), collapse = "")),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, column_data_list)
  }
}

#' Check if running in CI environment
#' @noRd
is_ci <- function() {
  isTRUE(as.logical(Sys.getenv("CI")))
}

#' Skip test if not on CI
#' @noRd
skip_if_not_ci <- function() {
  if (!is_ci()) {
    testthat::skip("Only run on CI")
  }
}
