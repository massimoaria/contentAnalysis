#' Match citations to references
#'
#' @description
#' Matches in-text citations to entries in the reference list using author-year
#' matching with multiple disambiguation strategies.
#'
#' @param citations_df Data frame with citation information, must include:
#'   citation_id, citation_text, citation_text_clean, citation_type
#' @param references_df Data frame with parsed references from parse_references_section()
#'
#' @return Tibble with matched citations including columns:
#'   \itemize{
#'     \item citation_id: Citation identifier
#'     \item citation_text: Original citation text
#'     \item citation_text_clean: Cleaned citation text
#'     \item citation_type: Type of citation
#'     \item cite_author: Extracted first author from citation
#'     \item cite_second_author: Second author (if present)
#'     \item cite_year: Extracted year
#'     \item cite_has_etal: Logical, contains "et al."
#'     \item matched_ref_id: ID of matched reference
#'     \item ref_full_text: Full text of matched reference
#'     \item ref_authors: Authors from reference
#'     \item ref_year: Year from reference
#'     \item match_confidence: Quality of match (high, medium, low, no_match)
#'   }
#'
#' @details
#' Matching algorithm:
#' \enumerate{
#'   \item Filter by exact year match
#'   \item Match first author (exact, then fuzzy)
#'   \item Disambiguate using second author or et al. heuristics
#' }
#'
#' Match confidence levels include:
#' high (exact first author + year),
#' high_second_author (disambiguated with second author),
#' medium_multiple_matches, medium_fuzzy, medium_etal_heuristic (various medium confidence scenarios),
#' no_match_year, no_match_author, no_match_missing_info (no suitable reference found).
#'
#' @examples
#' \dontrun{
#' matched <- match_citations_to_references(citations_df, references_df)
#' table(matched$match_confidence)
#' }
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select filter bind_rows rowwise ungroup arrange desc
#' @importFrom stringr str_extract str_replace_all str_detect str_count str_to_lower str_replace str_trim
match_citations_to_references <- function(citations_df, references_df) {

  if (nrow(citations_df) == 0 || nrow(references_df) == 0) {
    return(tibble::tibble(
      citation_id = character(),
      citation_text = character(),
      citation_text_clean = character(),
      citation_type = character(),
      matched_ref_id = character(),
      ref_full_text = character(),
      match_confidence = character(),
      ref_authors = character(),
      ref_year = character()
    ))
  }

  extract_citation_info <- function(citation_text) {
    clean_cite <- stringr::str_replace_all(citation_text, "^\\(|\\)$", "")

    year <- stringr::str_extract(clean_cite, "\\d{4}[a-z]?")
    has_etal <- stringr::str_detect(clean_cite, "et\\s+al\\.")
    has_and <- stringr::str_detect(clean_cite, "\\s+and\\s+|\\s+&\\s+")

    author <- stringr::str_extract(clean_cite, "^[A-Za-z'-]+")
    author_normalized <- stringr::str_to_lower(author)

    second_author <- NA_character_
    if (has_and && !has_etal) {
      second_author <- stringr::str_extract(clean_cite, "(?:and|&)\\s+([A-Za-z'-]+)")
      second_author <- stringr::str_replace(second_author, "^(?:and|&)\\s+", "")
    }
    second_author_normalized <- stringr::str_to_lower(second_author)

    n_cite_authors <- if (has_etal) {
      99
    } else if (has_and) {
      stringr::str_count(clean_cite, ",") + 2
    } else {
      1
    }

    list(
      author = author,
      author_normalized = author_normalized,
      second_author = second_author,
      second_author_normalized = second_author_normalized,
      year = year,
      has_etal = has_etal,
      has_and = has_and,
      n_authors = n_cite_authors
    )
  }

  citations_info <- citations_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      cite_info = list(extract_citation_info(citation_text_clean)),
      cite_author = cite_info$author,
      cite_author_normalized = cite_info$author_normalized,
      cite_second_author = cite_info$second_author,
      cite_second_author_normalized = cite_info$second_author_normalized,
      cite_year = cite_info$year,
      cite_has_etal = cite_info$has_etal,
      cite_has_and = cite_info$has_and,
      cite_n_authors = cite_info$n_authors
    ) %>%
    dplyr::select(-cite_info) %>%
    dplyr::ungroup()

  matched_citations <- tibble::tibble()

  for (i in 1:nrow(citations_info)) {
    cite <- citations_info[i, ]

    if (is.na(cite$cite_author_normalized) || is.na(cite$cite_year)) {
      matched_row <- cite %>%
        dplyr::mutate(
          matched_ref_id = NA_character_,
          ref_full_text = NA_character_,
          match_confidence = "no_match_missing_info",
          ref_authors = NA_character_,
          ref_year = NA_character_
        )
      matched_citations <- dplyr::bind_rows(matched_citations, matched_row)
      next
    }

    year_matches <- references_df %>%
      dplyr::filter(!is.na(ref_year), ref_year == cite$cite_year)

    if (nrow(year_matches) == 0) {
      matched_row <- cite %>%
        dplyr::mutate(
          matched_ref_id = NA_character_,
          ref_full_text = NA_character_,
          match_confidence = "no_match_year",
          ref_authors = NA_character_,
          ref_year = NA_character_
        )
      matched_citations <- dplyr::bind_rows(matched_citations, matched_row)
      next
    }

    author_matches <- year_matches %>%
      dplyr::filter(
        !is.na(ref_first_author_normalized),
        ref_first_author_normalized == cite$cite_author_normalized
      )

    if (nrow(author_matches) == 0) {
      fuzzy_matches <- year_matches %>%
        dplyr::filter(
          !is.na(ref_first_author_normalized),
          stringr::str_detect(ref_first_author_normalized, cite$cite_author_normalized) |
            stringr::str_detect(cite$cite_author_normalized, ref_first_author_normalized)
        )

      if (nrow(fuzzy_matches) > 0) {
        author_matches <- fuzzy_matches
      } else {
        matched_row <- cite %>%
          dplyr::mutate(
            matched_ref_id = NA_character_,
            ref_full_text = NA_character_,
            match_confidence = "no_match_author",
            ref_authors = NA_character_,
            ref_year = NA_character_
          )
        matched_citations <- dplyr::bind_rows(matched_citations, matched_row)
        next
      }
    }

    final_match <- NULL
    confidence <- "high"

    if (nrow(author_matches) == 1) {
      final_match <- author_matches[1, ]

      if (cite$cite_has_etal && final_match$n_authors < 3) {
        confidence <- "medium_etal_inconsistent"
      }

    } else {
      if (!is.na(cite$cite_second_author_normalized)) {
        second_match <- author_matches %>%
          dplyr::filter(
            !is.na(ref_second_author_normalized),
            ref_second_author_normalized == cite$cite_second_author_normalized
          )

        if (nrow(second_match) == 1) {
          final_match <- second_match[1, ]
          confidence <- "high_second_author"
        } else if (nrow(second_match) > 1) {
          final_match <- second_match[1, ]
          confidence <- "medium_multiple_with_second"
        }
      }

      if (is.null(final_match) && cite$cite_has_etal) {
        etal_candidates <- author_matches %>%
          dplyr::filter(n_authors >= 3) %>%
          dplyr::arrange(dplyr::desc(n_authors))

        if (nrow(etal_candidates) > 0) {
          final_match <- etal_candidates[1, ]
          confidence <- "medium_etal_heuristic"
        }
      }

      if (is.null(final_match)) {
        final_match <- author_matches[1, ]
        confidence <- "medium_multiple_matches"
      }
    }

    matched_row <- cite %>%
      dplyr::mutate(
        matched_ref_id = final_match$ref_id,
        ref_full_text = final_match$ref_full_text,
        match_confidence = confidence,
        ref_authors = final_match$ref_authors,
        ref_year = final_match$ref_year
      )

    matched_citations <- dplyr::bind_rows(matched_citations, matched_row)
  }

  result <- matched_citations %>%
    dplyr::select(
      citation_id,
      citation_text,
      citation_text_clean,
      citation_type,
      cite_author,
      cite_second_author,
      cite_year,
      cite_has_etal,
      matched_ref_id,
      ref_full_text,
      ref_authors,
      ref_year,
      match_confidence
    )

  return(result)
}
