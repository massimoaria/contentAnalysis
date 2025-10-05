#' Extract text from multi-column PDF with structure preservation
#'
#' @description
#' Extracts text from PDF files handling multi-column layouts, with options
#' for structure preservation and automatic column detection.
#'
#' @param file Character string. Path to the PDF file.
#' @param n_columns Integer or NULL. Number of columns to detect. If NULL,
#'   attempts automatic detection. Default is NULL.
#' @param column_threshold Numeric or NULL. X-coordinate threshold for column
#'   separation. If NULL and n_columns is NULL, calculated automatically.
#' @param preserve_structure Logical. If TRUE, preserves paragraph breaks and
#'   section structure. If FALSE, returns continuous text. Default is TRUE.
#'
#' @return Character string with extracted text.
#'
#' @details
#' This function uses `pdftools::pdf_data()` for precise text extraction with
#' spatial coordinates. It handles:
#' \itemize{
#'   \item Multi-column layouts (2+ columns)
#'   \item Section detection and paragraph preservation
#'   \item Hyphenation removal
#'   \item Title and heading identification
#' }
#'
#' If `pdf_data()` fails, falls back to `pdftools::pdf_text()`.
#'
#' @examples
#' \dontrun{
#' # Extract from 2-column paper
#' text <- pdf2txt_multicolumn_safe("paper.pdf", n_columns = 2)
#'
#' # Automatic column detection
#' text <- pdf2txt_multicolumn_safe("paper.pdf")
#'
#' # Single column, no structure
#' text <- pdf2txt_multicolumn_safe("paper.pdf", n_columns = 1,
#'                                   preserve_structure = FALSE)
#' }
#'
#' @export
#' @importFrom pdftools pdf_data pdf_length pdf_text poppler_config
#' @importFrom stats kmeans
pdf2txt_multicolumn_safe <- function(file,
                                     n_columns = NULL,
                                     column_threshold = NULL,
                                     preserve_structure = TRUE) {

  has_poppler_config <- exists("poppler_config", where = asNamespace("pdftools"), mode = "function")

  if (has_poppler_config) {
    if (!pdftools::poppler_config()$has_pdf_data) {
      message("Pdf import feature requires a recent version of libpoppler. Please install it.")
      return(NA)
    }
  }

  tryCatch({
    data_list <- pdftools::pdf_data(file)
    all_text <- c()

    for (page_num in seq_along(data_list)) {
      page_data <- data_list[[page_num]]
      if (nrow(page_data) == 0) next

      if (!is.null(n_columns)) {
        if (n_columns == 1) {
          page_data <- page_data[order(page_data$y, page_data$x), ]
          page_text <- reconstruct_text_structured(page_data, preserve_structure)
        } else if (n_columns >= 2) {
          x_positions <- page_data$x

          tryCatch({
            clusters <- kmeans(x_positions, centers = n_columns, nstart = 20)
            cluster_centers <- sort(clusters$centers[, 1])

            thresholds <- numeric(n_columns - 1)
            for (i in 1:(n_columns - 1)) {
              thresholds[i] <- mean(c(cluster_centers[i], cluster_centers[i + 1]))
            }

            columns <- list()
            page_data$column <- cut(page_data$x,
                                    breaks = c(-Inf, thresholds, Inf),
                                    labels = FALSE)

            for (col in 1:n_columns) {
              col_data <- page_data[page_data$column == col, ]
              if (nrow(col_data) > 0) {
                col_data <- col_data[order(col_data$y, col_data$x), ]
                columns[[col]] <- reconstruct_text_structured(col_data, preserve_structure)
              } else {
                columns[[col]] <- ""
              }
            }

            if (preserve_structure) {
              page_text <- paste(columns, collapse = "\n\n")
            } else {
              page_text <- paste(columns, collapse = " ")
            }

          }, error = function(e) {
            message("K-means clustering failed for ", n_columns, " columns: ", e$message)
            page_width <- max(page_data$x) - min(page_data$x)
            column_width <- page_width / n_columns
            thresholds <- min(page_data$x) + column_width * (1:(n_columns - 1))

            columns <- list()
            page_data$column <- cut(page_data$x,
                                    breaks = c(-Inf, thresholds, Inf),
                                    labels = FALSE)

            for (col in 1:n_columns) {
              col_data <- page_data[page_data$column == col, ]
              if (nrow(col_data) > 0) {
                col_data <- col_data[order(col_data$y, col_data$x), ]
                columns[[col]] <- reconstruct_text_structured(col_data, preserve_structure)
              }
            }

            page_text <- paste(columns, collapse = ifelse(preserve_structure, "\n\n", " "))
          })
        }
      } else {
        if (is.null(column_threshold)) {
          x_positions <- page_data$x
          if (length(unique(x_positions)) > 20) {
            tryCatch({
              clusters <- kmeans(x_positions, centers = 2, nstart = 10)
              cluster_centers <- sort(clusters$centers[, 1])
              column_threshold <- mean(cluster_centers)
            }, error = function(e) {
              column_threshold <- (max(page_data$x) + min(page_data$x)) / 2
            })
          } else {
            column_threshold <- (max(page_data$x) + min(page_data$x)) / 2
          }
        }

        left_column <- page_data[page_data$x < column_threshold, ]
        right_column <- page_data[page_data$x >= column_threshold, ]

        if (nrow(left_column) < 5 || nrow(right_column) < 5) {
          page_data <- page_data[order(page_data$y, page_data$x), ]
          page_text <- reconstruct_text_structured(page_data, preserve_structure)
        } else {
          left_column <- left_column[order(left_column$y, left_column$x), ]
          right_column <- right_column[order(right_column$y, right_column$x), ]

          left_text <- reconstruct_text_structured(left_column, preserve_structure)
          right_text <- reconstruct_text_structured(right_column, preserve_structure)

          if (preserve_structure) {
            page_text <- paste(left_text, right_text, sep = "\n\n")
          } else {
            page_text <- paste(left_text, right_text, sep = " ")
          }
        }
      }

      all_text <- c(all_text, page_text)
    }

    if (preserve_structure) {
      txt <- paste(all_text, collapse = "\n\n")
      txt <- gsub("([0-9]+(?:\\.[0-9]+)*\\.\\s+[A-Z][A-Za-z\\s]{3,50})", "\n\n\\1", txt)
      txt <- gsub("\\s+([A-Z][A-Z\\s]{10,60})\\s+", "\n\n\\1\n\n", txt)
      txt <- gsub("([.!?])\\s+([A-Z][a-z])", "\\1\n\n\\2", txt)
      txt <- gsub("\\n{3,}", "\n\n", txt)
    } else {
      txt <- paste(all_text, collapse = " ")
    }

    txt <- gsub("-\\s*\n", "", txt)
    txt <- gsub("-\\s+", "", txt)

    if (preserve_structure) {
      txt <- gsub("[ \t]+", " ", txt)
      txt <- gsub("\\n ", "\n", txt)
    } else {
      txt <- gsub("\\s+", " ", txt)
    }

    txt <- trimws(txt)
    return(txt)

  }, error = function(e) {
    message("pdf_data failed, falling back to pdf_text method: ", e$message)

    pages <- pdftools::pdf_length(file)
    txt <- pdftools::pdf_text(file)

    if (preserve_structure) {
      txt <- gsub("([0-9]+(?:\\.[0-9]+)*\\.\\s+[A-Z][A-Za-z\\s]{3,50})", "\n\n\\1\n\n", txt, perl = TRUE)
      txt <- gsub("\\n\\s*\\n", "\n\n", txt)
      txt <- gsub("([.!?])\\s*\n\\s*([A-Z])", "\\1\n\n\\2", txt, perl = TRUE)
      txt <- gsub("(?<![.!?\\n])\\n(?![A-Z0-9\\n])", " ", txt, perl = TRUE)
      txt <- paste(txt, collapse = "\n\n")
    } else {
      txt <- gsub("(?<![\\s\\.])\\n(?!\\s)", " ", txt, perl = TRUE)
      txt <- gsub("\n  ", "\n\n", txt)
      txt <- paste(txt, collapse = " ")
    }

    txt <- gsub("-\\s", "", txt)
    return(txt)
  })
}

#' Reconstruct text from PDF data with structure
#'
#' @param column_data Data frame from pdftools::pdf_data() for a column/page
#' @param preserve_structure Logical. Preserve paragraph breaks and structure
#'
#' @return Character string with reconstructed text
#'
#' @keywords internal
#' @noRd
reconstruct_text_structured <- function(column_data, preserve_structure = TRUE) {
  if (nrow(column_data) == 0) return("")

  tolerance <- 4
  column_data$line <- round(column_data$y / tolerance) * tolerance

  available_cols <- names(column_data)

  if ("height" %in% available_cols) {
    column_data$font_size <- column_data$height
  } else {
    column_data$font_size <- 12
  }

  lines <- split(column_data, column_data$line)

  line_results <- lapply(lines, function(line) {
    line <- line[order(line$x), ]

    line_text <- paste(line$text, collapse = " ")
    line_text <- trimws(line_text)

    avg_font_size <- mean(line$font_size, na.rm = TRUE)
    is_short <- nchar(line_text) < 80
    is_caps <- grepl("^[A-Z\\s\\d\\.\\-]+$", line_text)
    starts_with_number <- grepl("^\\d+\\.", line_text)
    starts_with_section <- grepl("^\\d+\\.\\d+", line_text)

    is_reference_start <- grepl("^[A-Z][a-z]+(?:['-][A-Z][a-z]+)?,\\s+[A-Z]\\.", line_text, perl = TRUE)

    is_title <- (is_short && (is_caps || starts_with_number || starts_with_section))

    return(list(
      text = line_text,
      y = min(line$y),
      is_title = is_title,
      font_size = avg_font_size,
      starts_with_number = starts_with_number,
      is_reference_start = is_reference_start
    ))
  })

  line_results <- line_results[sapply(line_results, function(x) nchar(x$text) > 0)]
  line_results <- line_results[order(sapply(line_results, function(x) x$y))]

  if (!preserve_structure) {
    result <- paste(sapply(line_results, function(x) x$text), collapse = " ")
  } else {
    result_parts <- c()

    for (i in seq_along(line_results)) {
      current_line <- line_results[[i]]
      line_text <- current_line$text

      if (nchar(line_text) == 0) next

      if (i == 1) {
        result_parts <- c(result_parts, line_text)
      } else {
        prev_line <- line_results[[i-1]]

        if (current_line$is_reference_start) {
          result_parts <- c(result_parts, "\n\n", line_text)
        }
        else if (current_line$is_title) {
          result_parts <- c(result_parts, "\n\n", line_text)
        } else if (prev_line$is_title) {
          result_parts <- c(result_parts, "\n\n", line_text)
        } else if (grepl("[.!?]\\s*$", prev_line$text) &&
                   grepl("^[A-Z]", line_text) &&
                   !grepl("^[A-Z][a-z]+\\s+[a-z]", line_text)) {
          result_parts <- c(result_parts, "\n\n", line_text)
        } else {
          result_parts <- c(result_parts, " ", line_text)
        }
      }
    }

    result <- paste(result_parts, collapse = "")
    result <- gsub("\\s+", " ", result)
    result <- gsub("\\n\\s+", "\n", result)
    result <- gsub("\\n{3,}", "\n\n", result)
  }

  result <- trimws(result)
  return(result)
}
