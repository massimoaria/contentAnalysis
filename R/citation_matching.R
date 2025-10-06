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

  # === FUNZIONE PER ESTRARRE COGNOME ===
  # CrossRef restituisce autori nel formato "U Bititci" (iniziale + cognome)
  # Questa funzione estrae solo il cognome
  extract_surname <- function(author_string) {
    if (is.na(author_string)) return(NA_character_)

    # Rimuovi spazi extra
    cleaned <- stringr::str_trim(author_string)

    # Estrai l'ultima parola (il cognome)
    # Pattern modificato per includere caratteri Unicode (ö, é, etc.)
    # \\p{L} = qualsiasi lettera Unicode
    surname <- stringr::str_extract(cleaned, "[\\p{L}'-]+$")

    return(stringr::str_to_lower(surname))
  }

  # === PREPARAZIONE RIFERIMENTI ===
  # Normalizza il secondo autore nei riferimenti
  if ("ref_second_author" %in% colnames(references_df)) {
    references_df <- references_df %>%
      dplyr::mutate(
        ref_second_author_normalized = stringr::str_to_lower(ref_second_author)
      )
  } else {
    references_df <- references_df %>%
      dplyr::mutate(
        ref_second_author = NA_character_,
        ref_second_author_normalized = NA_character_
      )
  }

  # Per riferimenti da CrossRef, estrai il cognome dal campo ref_authors
  # e marca come non affidabili le info su autori multipli
  if ("ref_source" %in% colnames(references_df)) {
    references_df <- references_df %>%
      dplyr::mutate(
        # Per CrossRef, estrai cognome da ref_authors (es. "H Abdi" → "abdi")
        ref_first_author_surname = ifelse(
          ref_source == "crossref",
          sapply(ref_authors, extract_surname),  # Usa ref_authors invece di ref_first_author
          ref_first_author_normalized
        ),
        # Per CrossRef, n_authors non è affidabile (sempre 1)
        n_authors = ifelse(ref_source == "crossref", NA_integer_, n_authors),
        # Per CrossRef, non abbiamo il secondo autore
        ref_second_author = ifelse(ref_source == "crossref", NA_character_, ref_second_author),
        ref_second_author_normalized = ifelse(ref_source == "crossref", NA_character_,
                                              ref_second_author_normalized)
      )
  } else {
    # Se non c'è ref_source, usa la normalizzazione standard
    references_df <- references_df %>%
      dplyr::mutate(
        ref_first_author_surname = ref_first_author_normalized
      )
  }

  # === SEPARAZIONE CITAZIONI NUMERATE E NARRATIVE ===
  numbered_citations <- citations_df %>%
    dplyr::filter(stringr::str_detect(citation_type, "^numbered_"))

  narrative_citations <- citations_df %>%
    dplyr::filter(!stringr::str_detect(citation_type, "^numbered_"))

  # === MATCHING PER CITAZIONI NUMERATE ===
  matched_numbered <- tibble::tibble()

  if (nrow(numbered_citations) > 0) {
    for (i in 1:nrow(numbered_citations)) {
      cite <- numbered_citations[i, ]

      # Estrai i numeri dalla citazione
      numbers <- stringr::str_extract_all(cite$citation_text_clean, "\\d+")[[1]]

      if (length(numbers) == 0) {
        matched_row <- cite %>%
          dplyr::mutate(
            matched_ref_id = NA_character_,
            ref_full_text = NA_character_,
            match_confidence = "no_match_missing_number",
            ref_authors = NA_character_,
            ref_year = NA_character_,
            cite_author = NA_character_,
            cite_second_author = NA_character_,
            cite_year = NA_character_,
            cite_has_etal = FALSE
          )
        matched_numbered <- dplyr::bind_rows(matched_numbered, matched_row)
        next
      }

      # Per citazioni multiple, crea una riga per ogni numero
      for (num in numbers) {
        ref_id_to_match <- paste0("REF_", num)

        matched_ref <- references_df %>%
          dplyr::filter(ref_id == ref_id_to_match)

        if (nrow(matched_ref) == 1) {
          matched_row <- cite %>%
            dplyr::mutate(
              matched_ref_id = matched_ref$ref_id,
              ref_full_text = matched_ref$ref_full_text,
              match_confidence = "high_numbered",
              ref_authors = matched_ref$ref_authors,
              ref_year = as.character(matched_ref$ref_year),
              cite_author = NA_character_,
              cite_second_author = NA_character_,
              cite_year = as.character(matched_ref$ref_year),
              cite_has_etal = FALSE
            )
        } else {
          matched_row <- cite %>%
            dplyr::mutate(
              matched_ref_id = NA_character_,
              ref_full_text = NA_character_,
              match_confidence = "no_match_numbered",
              ref_authors = NA_character_,
              ref_year = NA_character_,
              cite_author = NA_character_,
              cite_second_author = NA_character_,
              cite_year = NA_character_,
              cite_has_etal = FALSE
            )
        }

        matched_numbered <- dplyr::bind_rows(matched_numbered, matched_row)
      }
    }
  }

  # === MATCHING PER CITAZIONI NARRATIVE ===
  matched_narrative <- tibble::tibble()

  if (nrow(narrative_citations) > 0) {
    # Funzione per estrarre informazioni dalla citazione
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

    # Estrai informazioni da tutte le citazioni
    citations_info <- narrative_citations %>%
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

    # Match ogni citazione
    for (i in 1:nrow(citations_info)) {
      cite <- citations_info[i, ]

      # Verifica informazioni minime
      if (is.na(cite$cite_author_normalized) || is.na(cite$cite_year)) {
        matched_row <- cite %>%
          dplyr::mutate(
            matched_ref_id = NA_character_,
            ref_full_text = NA_character_,
            match_confidence = "no_match_missing_info",
            ref_authors = NA_character_,
            ref_year = NA_character_
          )
        matched_narrative <- dplyr::bind_rows(matched_narrative, matched_row)
        next
      }

      # 1. Filtra per anno
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
        matched_narrative <- dplyr::bind_rows(matched_narrative, matched_row)
        next
      }

      # 2. Match primo autore (esatto) usando ref_first_author_surname
      # Questo funziona sia per CrossRef (solo cognome) che per riferimenti parsati
      author_matches <- year_matches %>%
        dplyr::filter(
          !is.na(ref_first_author_surname),
          ref_first_author_surname == cite$cite_author_normalized
        )

      # 3. Se non trova match esatto, prova fuzzy matching
      if (nrow(author_matches) == 0) {
        fuzzy_matches <- year_matches %>%
          dplyr::filter(
            !is.na(ref_first_author_surname),
            stringr::str_detect(ref_first_author_surname, cite$cite_author_normalized) |
              stringr::str_detect(cite$cite_author_normalized, ref_first_author_surname)
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
          matched_narrative <- dplyr::bind_rows(matched_narrative, matched_row)
          next
        }
      }

      # 4. Disambiguazione
      final_match <- NULL
      confidence <- "high"

      if (nrow(author_matches) == 1) {
        # Match unico: verifica solo consistenza et al. se abbiamo l'info
        final_match <- author_matches[1, ]

        if (cite$cite_has_etal && !is.na(final_match$n_authors) && final_match$n_authors < 3) {
          confidence <- "medium_etal_inconsistent"
        }

      } else {
        # Match multipli: disambigua

        # A. Prova con secondo autore (solo se disponibile nel riferimento)
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

        # B. Euristica et al. (solo se n_authors è disponibile)
        if (is.null(final_match) && cite$cite_has_etal) {
          etal_candidates <- author_matches %>%
            dplyr::filter(!is.na(n_authors), n_authors >= 3) %>%
            dplyr::arrange(dplyr::desc(n_authors))

          if (nrow(etal_candidates) > 0) {
            final_match <- etal_candidates[1, ]
            confidence <- "medium_etal_heuristic"
          } else {
            # Per CrossRef, accetta il primo match anche senza info su n_authors
            final_match <- author_matches[1, ]
            confidence <- "medium_crossref_etal"
          }
        }

        # C. Fallback: prendi il primo
        if (is.null(final_match)) {
          final_match <- author_matches[1, ]
          confidence <- "medium_multiple_matches"
        }
      }

      # Crea riga con match
      matched_row <- cite %>%
        dplyr::mutate(
          matched_ref_id = final_match$ref_id,
          ref_full_text = final_match$ref_full_text,
          match_confidence = confidence,
          ref_authors = final_match$ref_authors,
          ref_year = final_match$ref_year
        )

      matched_narrative <- dplyr::bind_rows(matched_narrative, matched_row)
    }
  }

  # === COMBINA RISULTATI ===
  matched_citations <- dplyr::bind_rows(matched_numbered, matched_narrative)

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
