#' Extract DOI from PDF Metadata
#'
#' This function extracts the Digital Object Identifier (DOI) from a PDF file's
#' metadata using pdftools::pdf_info(). It searches through various metadata
#' fields and applies pattern matching to identify valid DOIs.
#'
#' @param pdf_path Character. Path to the PDF file.
#' @param return_all Logical. If TRUE, returns all DOIs found; if FALSE (default),
#'   returns only the first DOI found.
#'
#' @return A character string containing the DOI, or NA_character_ if no DOI is found.
#'   If return_all = TRUE, returns a character vector of all DOIs found.
#'
#' @details
#' The function searches for DOIs in the following metadata fields:
#' \itemize{
#'   \item doi/DOI field
#'   \item Subject
#'   \item Keywords
#'   \item Title
#'   \item Creator
#'   \item Producer
#' }
#'
#' Common DOI prefixes (doi:, DOI:, https://doi.org/, https://dx.doi.org/) are
#' automatically removed. The function uses regex pattern matching to validate
#' DOI format according to the standard 10.xxxx/yyyy structure.
#'
#' @examples
#' \dontrun{
#' # Extract DOI from a single PDF
#' doi <- extract_doi_from_pdf("path/to/paper.pdf")
#'
#' # Extract all DOIs if multiple are present
#' dois <- extract_doi_from_pdf("path/to/paper.pdf", return_all = TRUE)
#' }
#'
#' @seealso \code{\link[pdftools]{pdf_info}}
#'
#' @importFrom pdftools pdf_info
#'
#' @export
extract_doi_from_pdf <- function(pdf_path, return_all = FALSE) {

  # Check if file exists
  if (!file.exists(pdf_path)) {
    stop("PDF file does not exist: ", pdf_path)
  }

  # Load PDF information
  tryCatch({
    info <- pdftools::pdf_info(pdf_path)
  }, error = function(e) {
    stop("Unable to read PDF file: ", e$message)
  })

  # Regex pattern for DOI (standard format)
  doi_pattern <- "10\\.\\d{4,9}/[-._;()/:A-Za-z0-9]+"

  # Metadata fields to check
  metadata_fields <- c(
    info$keys$doi,
    info$keys$DOI,
    info$keys$Subject,
    info$keys$Keywords,
    info$keys$Title,
    info$keys$Creator,
    info$keys$Producer
  )

  # Storage for all DOIs found
  all_dois <- character(0)

  # Search for DOI in all fields
  for (field in metadata_fields) {
    if (!is.null(field) && nchar(field) > 0) {
      # Remove common prefixes
      field_clean <- gsub("^(doi:|DOI:|https?://doi.org/|https?://dx.doi.org/)",
                          "", field, ignore.case = TRUE)

      # Extract DOI using regex
      doi_matches <- regmatches(field_clean,
                                gregexpr(doi_pattern, field_clean, perl = TRUE))[[1]]

      if (length(doi_matches) > 0) {
        all_dois <- c(all_dois, trimws(doi_matches))

        # If return_all is FALSE, return first match immediately
        if (!return_all) {
          return(trimws(doi_matches[1]))
        }
      }
    }
  }

  # Return results based on return_all parameter
  if (return_all) {
    if (length(all_dois) > 0) {
      return(unique(all_dois))  # Remove duplicates
    } else {
      return(NA_character_)
    }
  }

  # If no DOI found, return NA
  return(NA_character_)
}
