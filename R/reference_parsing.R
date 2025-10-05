#' Normalize references text formatting
#'
#' @description
#' Removes unnecessary line breaks within reference entries while preserving
#' separation between different references.
#'
#' @param references_text Character string. Raw references section text.
#'
#' @return Character string with normalized formatting.
#'
#' @keywords internal
#' @noRd
#' @importFrom stringr str_replace_all
normalize_references_text <- function(references_text) {

  if (is.null(references_text) || length(references_text) == 0 ||
      all(is.na(references_text)) || all(references_text == "")) {
    return("")
  }

  normalized <- references_text

  # Remove \n\n between authors of same entry
  normalized <- stringr::str_replace_all(
    normalized,
    "(?<!\\.)\\s*,\\s*\\n\\n(?=[A-Z][a-z]+,\\s*[A-Z])",
    ", "
  )

  # Remove \n\n after initials, before surname
  normalized <- stringr::str_replace_all(
    normalized,
    "(?<=[A-Z]\\.)\\s*,\\s*\\n\\n(?=[A-Z][a-z]+,)",
    ", "
  )

  # Remove \n\n after & before surname
  normalized <- stringr::str_replace_all(
    normalized,
    "(?<=&)\\s*\\n\\n(?=[A-Z][a-z]+,)",
    " "
  )

  # Remove \n\n between initials
  normalized <- stringr::str_replace_all(
    normalized,
    "(?<=[A-Z]\\.)\\s*\\n\\n(?=[A-Z]\\.)",
    " "
  )

  return(normalized)
}

#' Parse references section from text
#'
#' @description
#' Parses a references section into individual entries with extracted metadata
#' including authors, year, and title information.
#'
#' @param references_text Character string. Text of references section.
#'
#' @return Tibble with columns:
#'   \itemize{
#'     \item ref_id: Unique reference identifier
#'     \item ref_full_text: Complete reference text
#'     \item ref_authors: Author string
#'     \item ref_year: Publication year
#'     \item ref_first_author: First author surname
#'     \item ref_first_author_normalized: Lowercase first author
#'     \item ref_second_author: Second author surname (if present)
#'     \item ref_second_author_normalized: Lowercase second author
#'     \item n_authors: Number of authors (99 = et al.)
#'   }
#'
#' @examples
#' \dontrun{
#' refs_text <- doc$References
#' parsed_refs <- parse_references_section(refs_text)
#' }
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom stringr str_split str_trim str_extract str_count str_detect str_replace_all str_to_lower
#' @importFrom dplyr mutate select
parse_references_section <- function(references_text) {

  if (is.null(references_text) || length(references_text) == 0 ||
      all(is.na(references_text)) || all(references_text == "")) {
    return(tibble::tibble(
      ref_id = character(),
      ref_full_text = character(),
      ref_authors = character(),
      ref_year = character(),
      ref_first_author = character(),
      ref_first_author_normalized = character(),
      ref_second_author = character(),
      ref_second_author_normalized = character(),
      n_authors = integer()
    ))
  }

  normalized_text <- normalize_references_text(references_text)

  individual_refs <- stringr::str_split(normalized_text, "\\n\\n+")[[1]]
  individual_refs <- individual_refs[stringr::str_trim(individual_refs) != ""]

  if (length(individual_refs) == 0) {
    return(tibble::tibble(
      ref_id = character(),
      ref_full_text = character(),
      ref_authors = character(),
      ref_year = character(),
      ref_first_author = character(),
      ref_first_author_normalized = character(),
      ref_second_author = character(),
      ref_second_author_normalized = character(),
      n_authors = integer()
    ))
  }

  parsed_refs <- tibble::tibble(
    ref_id = paste0("REF_", seq_along(individual_refs)),
    ref_full_text = individual_refs %>%
      stringr::str_replace_all("\\s+", " ") %>%
      stringr::str_trim()
  ) %>%
    dplyr::mutate(
      ref_year = stringr::str_extract(ref_full_text, "\\(\\d{4}[a-z]?\\)"),
      ref_year = stringr::str_replace_all(ref_year, "[()]", ""),

      authors_section = stringr::str_extract(ref_full_text, "^[^(]*?(?=\\(\\d{4})"),
      authors_section = ifelse(
        is.na(authors_section),
        stringr::str_extract(ref_full_text, "^[^.]{0,200}"),
        authors_section
      ),
      authors_section = stringr::str_trim(authors_section),

      n_authors = stringr::str_count(authors_section, ",\\s*[A-Z]") + 1,
      n_authors = ifelse(stringr::str_detect(authors_section, "et\\s+al"), 99, n_authors),

      ref_first_author = stringr::str_extract(authors_section, "^[^,]+"),
      ref_first_author = stringr::str_trim(ref_first_author),
      ref_first_author_normalized = stringr::str_extract(ref_first_author, "[A-Za-z'-]+"),
      ref_first_author_normalized = stringr::str_to_lower(ref_first_author_normalized),

      ref_second_author = stringr::str_extract(
        authors_section,
        "(?<=,\\s)[A-Z][a-z]+(?:-[A-Z][a-z]+)?"
      ),
      ref_second_author_normalized = stringr::str_to_lower(ref_second_author),

      ref_authors = authors_section
    ) %>%
    dplyr::select(-authors_section)

  return(parsed_refs)
}
