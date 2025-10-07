#' Map citations to document segments or sections
#'
#' @param citations_df Data frame with citations and their positions
#' @param text Text object (string or list with sections)
#' @param use_sections Logical or "auto". Use sections if available
#' @param n_segments Integer. Number of segments if not using sections
#'
#' @return Citations data frame with segment/section information
#'
#' @keywords internal
#'
#' @export
#' @importFrom dplyr mutate
map_citations_to_segments <- function(citations_df,
                                      text,
                                      use_sections = "auto",
                                      n_segments = 10) {

  sections_available <- FALSE
  section_names <- character(0)

  if (is.list(text)) {
    section_names <- setdiff(names(text), c("Full_text", "References"))
    sections_available <- length(section_names) > 0
  }

  use_sections_final <- FALSE
  if (use_sections == "auto") {
    use_sections_final <- sections_available
  } else if (is.logical(use_sections)) {
    if (use_sections && !sections_available) {
      warning("Sections requested but not available. Using segments instead.")
      use_sections_final <- FALSE
    } else {
      use_sections_final <- use_sections
    }
  }

  if (use_sections_final) {
    section_positions <- list()
    cumulative_pos <- 1

    for (sect_name in section_names) {
      sect_text <- text[[sect_name]]
      sect_length <- nchar(sect_text)

      section_positions[[sect_name]] <- list(
        start = cumulative_pos,
        end = cumulative_pos + sect_length - 1,
        name = sect_name
      )

      cumulative_pos <- cumulative_pos + sect_length + 1
    }

    citations_df$segment <- NA_character_
    for (i in 1:nrow(citations_df)) {
      cite_pos <- citations_df$start_pos[i]

      for (sect_name in section_names) {
        sect_info <- section_positions[[sect_name]]
        if (cite_pos >= sect_info$start && cite_pos <= sect_info$end) {
          citations_df$segment[i] <- sect_name
          break
        }
      }
    }

    citations_df$segment <- ifelse(is.na(citations_df$segment), "Unknown", citations_df$segment)
    citations_df$segment_type <- "section"

  } else {
    min_pos <- min(citations_df$start_pos, na.rm = TRUE)
    max_pos <- max(citations_df$start_pos, na.rm = TRUE)

    position_range <- max_pos - min_pos
    segment_size <- position_range / n_segments

    citations_df$segment <- paste0(
      "Segment ",
      pmin(n_segments, ceiling((citations_df$start_pos - min_pos) / segment_size))
    )

    citations_df$segment <- ifelse(
      citations_df$start_pos == min_pos,
      "Segment 1",
      citations_df$segment
    )

    citations_df$segment_type <- "equal_length"
  }

  return(citations_df)
}

#' Enhanced scientific content analysis with citation extraction
#'
#' @description
#' Comprehensive analysis of scientific documents including citation extraction,
#' reference matching, text analysis, and bibliometric indicators.
#'
#' @param text Character string or named list. Document text or text with sections.
#' @param doi Character string or NULL. DOI for CrossRef reference retrieval.
#' @param mailto Character string or NULL. Email for CrossRef API.
#' @param window_size Integer. Words before/after citations for context (default: 10).
#' @param min_word_length Integer. Minimum word length for analysis (default: 3).
#' @param remove_stopwords Logical. Remove stopwords (default: TRUE).
#' @param language Character. Language for stopwords (default: "en").
#' @param custom_stopwords Character vector. Additional stopwords.
#' @param ngram_range Integer vector. N-gram range, e.g. c(1,3) (default: c(1,3)).
#' @param parse_multiple_citations Logical. Parse complex citations (default: TRUE).
#' @param use_sections_for_citations Logical or "auto". Use sections for mapping (default: "auto").
#' @param n_segments_citations Integer. Segments if not using sections (default: 10).
#'
#' @return List with class "enhanced_scientific_content_analysis" containing:
#'   \itemize{
#'     \item text_analytics: Basic statistics and word frequencies
#'     \item citations: All extracted citations with metadata
#'     \item citation_contexts: Citations with surrounding text
#'     \item citation_metrics: Citation type distribution, density, etc.
#'     \item citation_references_mapping: Matched citations to references
#'     \item parsed_references: Structured reference list
#'     \item word_frequencies: Word frequency table
#'     \item ngrams: N-gram frequency tables
#'     \item network_data: Citation co-occurrence data
#'     \item summary: Overall analysis summary
#'   }
#'
#' @details
#' This function performs:
#' \itemize{
#'   \item Citation extraction (numbered, author-year, narrative, parenthetical)
#'   \item Reference parsing (from text or CrossRef API)
#'   \item Citation-reference matching
#'   \item Text analysis (word frequencies, n-grams)
#'   \item Citation context extraction
#'   \item Bibliometric indicators
#' }
#'
#' @examples
#' \dontrun{
#' doc <- pdf2txt_auto("paper.pdf")
#' analysis <- analyze_scientific_content(
#'   doc,
#'   doi = "10.xxxx/xxxxx",
#'   mailto = "your@email.com"
#' )
#'
#' summary(analysis)
#' head(analysis$citations)
#' table(analysis$citation_metrics$type_distribution)
#' }
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select filter bind_rows arrange desc left_join count rowwise ungroup slice_head rename
#' @importFrom stringr str_replace_all str_trim str_locate_all str_sub str_detect str_extract str_split str_extract_all str_length str_count str_remove str_squish str_to_title str_to_lower
#' @importFrom tidytext unnest_tokens
analyze_scientific_content <- function(text,
                                                doi = NULL,
                                                mailto = NULL,
                                                window_size = 10,
                                                min_word_length = 3,
                                                remove_stopwords = TRUE,
                                                language = "en",
                                                custom_stopwords = NULL,
                                                ngram_range = c(1, 3),
                                                parse_multiple_citations = TRUE,
                                                use_sections_for_citations = "auto",
                                                n_segments_citations = 10) {

  results <- list()

  references_section <- NULL

  if (is.list(text)) {
    if ("Full_text" %in% names(text)) {
      clean_text <- text$Full_text
      sections_to_use <- NULL
    } else {
      sections_to_use <- setdiff(names(text), "References")
      clean_text <- paste(text[sections_to_use], collapse = " ")
    }

    if ("References" %in% names(text)) {
      references_section <- text$References
    }
  } else {
    clean_text <- text
    sections_to_use <- NULL  # <-- AGGIUNGERE QUESTA RIGA
  }

  clean_text <- clean_text %>%
    stringr::str_replace_all("\\n+", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()

  text_stats <- data.frame(
    total_characters = nchar(clean_text),
    total_words = length(stringr::str_split(clean_text, "\\s+")[[1]]),
    total_sentences = length(stringr::str_split(clean_text, "[.!?]+")[[1]]),
    avg_words_per_sentence = length(stringr::str_split(clean_text, "\\s+")[[1]]) /
      length(stringr::str_split(clean_text, "[.!?]+")[[1]])
  )

  # Citation patterns
  citation_patterns <- list(
    complex_multiple_citations = "\\((?:see\\s+)?(?:e\\.g\\.\\s+)?[A-Z][^)]*(?:\\d{4}[a-z]?[;,][^)]*){2,}\\d{4}[a-z]?[^)]*\\)",
    narrative_four_authors_and = "[A-Z][A-Za-z'-]+,\\s*[A-Z][A-Za-z'-]+,\\s*[A-Z][A-Za-z'-]+,\\s*(?:and|&)\\s*[A-Z][A-Za-z'-]+\\s*\\(\\d{4}[a-z]?\\)",
    narrative_three_authors_and = "[A-Z][A-Za-z'-]+,\\s*[A-Z][A-Za-z'-]+,\\s*(?:and|&)\\s*[A-Z][A-Za-z'-]+\\s*\\(\\d{4}[a-z]?\\)",
    narrative_two_authors_and = "[A-Z][A-Za-z'-]+\\s+(?:and|&)\\s+[A-Z][A-Za-z'-]+\\s*\\(\\d{4}[a-z]?\\)",
    narrative_etal = "[A-Z][A-Za-z'-]+(?:\\s*,\\s*[A-Z][A-Za-z'-]+)*(?:\\s*,?\\s*(?:and|&)?\\s*et\\s+al\\.)?\\s*\\(\\d{4}[a-z]?\\)",
    narrative_multiple_authors = "[A-Z][A-Za-z'-]+(?:\\s*,\\s*[A-Z][A-Za-z'-]+)+(?:\\s*,\\s*&\\s*[A-Z][A-Za-z'-]+)?\\s*\\(\\d{4}[a-z]?\\)",
    narrative_single = "[A-Z][A-Za-z'-]+\\s*\\(\\d{4}[a-z]?\\)",
    multiple_citations_semicolon = "\\([A-Z][A-Za-z'-]+[^)]*\\d{4}[a-z]?(?:\\s*;\\s*[A-Z][^)]*\\d{4}[a-z]?)+[^)]*\\)",
    see_citations = "\\((?:see|e\\.g\\.|cf\\.|compare)\\s+[A-Z][A-Za-z'-]+[^)]+\\d{4}[a-z]?\\)",
    author_year_etal = "\\([A-Z][A-Za-z'-]+\\s+et\\s+al\\.,\\s*\\d{4}[a-z]?\\)",
    author_year_and = "\\([A-Z][A-Za-z'-]+(?:,\\s*[A-Z][A-Za-z'-]+)*,?\\s+(?:and|&)\\s+[A-Z][A-Za-z'-]+,\\s*\\d{4}[a-z]?\\)",
    author_year_ampersand = "\\([A-Z][A-Za-z'-]+[^)]*&[^)]*\\d{4}[a-z]?\\)",
    author_year_basic = "\\([A-Z][A-Za-z'-]+(?:\\s+[A-Z][A-Za-z'-]+)*,\\s*\\d{4}[a-z]?\\)",
    numbered_simple = "\\[\\d+\\]",
    numbered_multiple = "\\[\\d+(?:[-,;\\s]+\\d+)*\\]",
    superscript = "[\u00b9\u00b2\u00b3\u2074\u2075\u2076\u2077\u2078\u2079\u2070]+",
    doi_pattern = "https?://doi\\.org/[\\w\\./\\-]+"
  )

  # Inizializza con struttura completa
  all_citations <- tibble::tibble(
    citation_type = character(0),
    citation_text = character(0),
    start_pos = integer(0),
    end_pos = integer(0),
    citation_id = character(0)
  )

  for (pattern_name in names(citation_patterns)) {
    pattern <- citation_patterns[[pattern_name]]
    matches <- stringr::str_locate_all(clean_text, pattern)[[1]]

    if (nrow(matches) > 0) {
      citations_temp <- tibble::tibble(
        citation_type = pattern_name,
        citation_text = stringr::str_sub(clean_text, matches[,1], matches[,2]),
        start_pos = matches[,1],
        end_pos = matches[,2],
        citation_id = paste0(pattern_name, "_", 1:nrow(matches))
      )

      all_citations <- dplyr::bind_rows(all_citations, citations_temp)
    }
  }

  # Remove duplicates
  all_citations <- all_citations %>%
    dplyr::arrange(start_pos, dplyr::desc(nchar(citation_text)))

  if (nrow(all_citations) > 1) {
    to_remove <- c()

    for (i in 1:(nrow(all_citations) - 1)) {
      if (i %in% to_remove) next

      for (j in (i + 1):nrow(all_citations)) {
        if (j %in% to_remove) next

        cite_i_start <- all_citations$start_pos[i]
        cite_i_end <- all_citations$end_pos[i]
        cite_j_start <- all_citations$start_pos[j]
        cite_j_end <- all_citations$end_pos[j]

        if (cite_j_start >= cite_i_start && cite_j_end <= cite_i_end) {
          to_remove <- c(to_remove, j)
        }
        else if (cite_i_start >= cite_j_start && cite_i_end <= cite_j_end) {
          to_remove <- c(to_remove, i)
          break
        }
        else if (cite_j_start < cite_i_end && cite_j_start > cite_i_start) {
          len_i <- cite_i_end - cite_i_start
          len_j <- cite_j_end - cite_j_start
          if (len_j < len_i) {
            to_remove <- c(to_remove, j)
          } else {
            to_remove <- c(to_remove, i)
            break
          }
        }
      }
    }

    if (length(to_remove) > 0) {
      all_citations <- all_citations[-unique(to_remove), ]
    }
  }

  all_citations <- all_citations %>%  dplyr::arrange(start_pos)

  # Parse complex citations
  parsed_citations <- tibble::tibble()

  if (parse_multiple_citations && nrow(all_citations) > 0) {
    for (i in 1:nrow(all_citations)) {
      citation <- all_citations[i,]

      if (citation$citation_type == "complex_multiple_citations") {
        inner_text <- stringr::str_replace(citation$citation_text, "^\\((?:see\\s+)?(?:e\\.g\\.\\s+)?", "")
        inner_text <- stringr::str_replace(inner_text, "\\)$", "")
        individual_citations <- stringr::str_split(inner_text, "\\s*;\\s*")[[1]]

        for (j in seq_along(individual_citations)) {
          indiv_cite <- stringr::str_trim(individual_citations[j])
          if (stringr::str_detect(indiv_cite, "\\d{4}")) {
            parsed_row <- tibble::tibble(
              citation_type = "parsed_from_multiple",
              citation_text = paste0("(", indiv_cite, ")"),
              start_pos = citation$start_pos,
              end_pos = citation$end_pos,
              citation_id = paste0("parsed_multiple_", i, "_", j),
              original_complex_citation = citation$citation_text
            )
            parsed_citations <- dplyr::bind_rows(parsed_citations, parsed_row)
          }
        }
      } else {
        citation$original_complex_citation <- NA
        parsed_citations <- dplyr::bind_rows(parsed_citations, citation)
      }
    }
    all_citations <- parsed_citations
  }

  # Process narrative citations
  if (nrow(all_citations) > 0) {
    narrative_citations <- all_citations %>%
      dplyr::filter(stringr::str_detect(citation_type, "narrative")) %>%
      dplyr::mutate(
        author_part = stringr::str_extract(citation_text, "^[^(]+"),
        year_part = stringr::str_extract(citation_text, "\\(\\d{4}[a-z]?\\)"),
        standardized_citation = paste0("(", stringr::str_trim(author_part), ", ",
                                       stringr::str_replace_all(year_part, "[()]", ""), ")")
      )

    if (nrow(narrative_citations) > 0) {
      all_citations <- all_citations %>%
        dplyr::left_join(
          narrative_citations %>%
            dplyr::select(citation_id, author_part, year_part, standardized_citation),
          by = "citation_id"
        ) %>%
        dplyr::mutate(
          citation_text_clean = ifelse(
            !is.na(standardized_citation),
            standardized_citation,
            citation_text
          )
        )
    } else {
      all_citations$citation_text_clean <- all_citations$citation_text
      all_citations$author_part <- NA
      all_citations$year_part <- NA
      all_citations$standardized_citation <- NA
    }
  }

  # Word frequency analysis
  tokens <- tibble::tibble(text = clean_text) %>%
    tidytext::unnest_tokens(word, text) %>%
    dplyr::filter(stringr::str_length(word) >= min_word_length) %>%
    dplyr::filter(!stringr::str_detect(word, "^\\d+$"))

  if (remove_stopwords) {
    data(stop_words, package = "tidytext", envir = environment())
    tokens <- tokens %>%
      dplyr::anti_join(stop_words, by = "word")

    if (!is.null(custom_stopwords)) {
      custom_stops <- tibble::tibble(word = custom_stopwords)
      tokens <- tokens %>%
        dplyr::anti_join(custom_stops, by = "word")
    }
  }

  word_frequencies <- tokens %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::mutate(
      frequency = n / sum(n),
      rank = row_number()
    )

  # N-gram analysis
  ngrams_results <- list()

  clean_text_filtered <- clean_text %>%
    stringr::str_replace_all("\\n+", " ") %>%
    stringr::str_replace_all("\\d+", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()

  if (remove_stopwords){
    word_tokens <- tibble::tibble(text = clean_text_filtered) %>%
      tidytext::unnest_tokens(word, text, token = "words") %>%
      dplyr::anti_join(stop_words, by = c("word"))
    clean_text_filtered <- word_tokens %>%
      dplyr::pull(word) %>%
      paste(collapse = " ")
  }

  for (n in ngram_range[1]:ngram_range[2]) {
    if (n == 1) {
      ngrams_results[[paste0(n, "gram")]] <- word_frequencies %>%
        dplyr::slice_head(n = 15)
    } else {
      ngram_tokens <- tibble::tibble(text = clean_text_filtered) %>%
        tidytext::unnest_tokens(ngram, text, token = "ngrams", n = n) %>%
        dplyr::filter(!is.na(ngram)) %>%
        dplyr::count(ngram, sort = TRUE) %>%
        dplyr::slice_head(n = 15) %>%
        dplyr::mutate(frequency = n / sum(n))

      ngrams_results[[paste0(n, "gram")]] <- ngram_tokens
    }
  }

  # Map citations to segments/sections
  if (nrow(all_citations) > 0) {
    all_citations <- map_citations_to_segments(
      citations_df = all_citations,
      text = if(is.list(text)) text else list(Full_text = text),
      use_sections = use_sections_for_citations,
      n_segments = n_segments_citations
    )

    all_citations <- all_citations %>%
      dplyr::rename(section = segment)
  } else {
    all_citations$section <- "Full_text"
    all_citations$segment_type <- "unknown"
  }

  # Citation metrics
  citation_metrics <- list()

  if (nrow(all_citations) > 0) {
    citation_metrics$type_distribution <- all_citations %>%
      dplyr::count(citation_type, sort = TRUE) %>%
      dplyr::mutate(percentage = round(n / sum(n) * 100, 2))

    if (!is.null(sections_to_use) && length(sections_to_use) > 0) {
      citation_metrics$section_distribution <- all_citations %>%
        dplyr::mutate(section = factor(section, levels = sections_to_use)) %>%
        dplyr::count(section, sort = FALSE, .drop = FALSE) %>%
        dplyr::mutate(percentage = round(n / sum(n) * 100, 2))
    } else {
      citation_metrics$section_distribution <- all_citations %>%
        dplyr::count(section, sort = TRUE) %>%
        dplyr::mutate(percentage = round(n / sum(n) * 100, 2))
    }

    citation_metrics$narrative_ratio <- all_citations %>%
      dplyr::summarise(
        total_citations = dplyr::n(),
        narrative_citations = sum(stringr::str_detect(citation_type, "narrative")),
        parenthetical_citations = sum(!stringr::str_detect(citation_type, "narrative")),
        narrative_percentage = round(narrative_citations / total_citations * 100, 2)
      )

    if (parse_multiple_citations) {
      citation_metrics$complex_citations <- all_citations %>%
        dplyr::filter(citation_type == "parsed_from_multiple") %>%
        dplyr::count(original_complex_citation, sort = TRUE) %>%
        dplyr::rename(individual_citations_extracted = n)
    }

    citation_metrics$density <- list(
      citations_per_1000_words = round((nrow(all_citations) / text_stats$total_words) * 1000, 2),
      avg_words_between_citations = if(nrow(all_citations) > 1) {
        round(text_stats$total_words / nrow(all_citations), 2)
      } else {
        text_stats$total_words
      }
    )
  }

  # Citation co-occurrence
  citation_cooccurrence <- NULL

  if (nrow(all_citations) > 1) {
    citation_pairs <- tibble::tibble()

    for (i in 1:(nrow(all_citations)-1)) {
      for (j in (i+1):nrow(all_citations)) {
        distance <- all_citations$start_pos[j] - all_citations$end_pos[i]
        if (distance <= 1000) {
          pair <- tibble::tibble(
            citation1 = all_citations$citation_text_clean[i],
            citation2 = all_citations$citation_text_clean[j],
            distance = distance,
            type1 = all_citations$citation_type[i],
            type2 = all_citations$citation_type[j]
          )
          citation_pairs <- dplyr::bind_rows(citation_pairs, pair)
        }
      }
    }

    citation_cooccurrence <- citation_pairs
  }

  # Citation contexts
  citation_contexts <- tibble::tibble()

  if (nrow(all_citations) > 0) {
    for (i in 1:nrow(all_citations)) {
      citation <- all_citations[i,]
      citation_start <- citation$start_pos
      citation_end <- citation$end_pos

      text_before_citation <- substr(clean_text, 1, citation_start - 1)
      text_after_citation <- substr(clean_text, citation_end + 1, nchar(clean_text))

      words_before_df <- tibble::tibble(text = text_before_citation) %>%
        tidytext::unnest_tokens(word, text, token = "words", to_lower = FALSE)

      n_words_before <- nrow(words_before_df)
      if (n_words_before > 0) {
        start_idx <- max(1, n_words_before - window_size + 1)
        words_before <- words_before_df %>%
          dplyr::slice(start_idx:n_words_before) %>%
          dplyr::pull(word) %>%
          paste(collapse = " ")
      } else {
        words_before <- ""
      }

      next_citations <- all_citations %>%
        dplyr::filter(start_pos > citation_end) %>%
        dplyr::arrange(start_pos)

      if (nrow(next_citations) > 0) {
        next_citation_start <- next_citations$start_pos[1]
        max_end_pos <- next_citation_start - citation_end - 1

        if (max_end_pos > 0) {
          text_after_limited <- substr(text_after_citation, 1, max_end_pos)
        } else {
          text_after_limited <- ""
        }
      } else {
        text_after_limited <- text_after_citation
      }

      words_after_df <- tibble::tibble(text = text_after_limited) %>%
        tidytext::unnest_tokens(word, text, token = "words", to_lower = FALSE)

      n_words_after <- nrow(words_after_df)
      if (n_words_after > 0) {
        end_idx <- min(window_size, n_words_after)
        words_after <- words_after_df %>%
          dplyr::slice(1:end_idx) %>%
          dplyr::pull(word) %>%
          paste(collapse = " ")
      } else {
        words_after <- ""
      }

      full_context <- paste(words_before, citation$citation_text, words_after) %>%
        stringr::str_trim()

      context_word_count <- n_words_before + n_words_after +
        length(strsplit(citation$citation_text, "\\s+")[[1]])

      context_row <- tibble::tibble(
        citation_id = citation$citation_id,
        citation_text = citation$citation_text,
        citation_text_clean = citation$citation_text_clean,
        citation_type = citation$citation_type,
        section = citation$section,
        words_before = words_before,
        words_after = words_after,
        full_context = full_context,
        context_word_count = context_word_count,
        citation_position_in_text = citation$start_pos,
        is_narrative = stringr::str_detect(citation$citation_type, "narrative"),
        is_parsed_multiple = citation$citation_type == "parsed_from_multiple"
      )

      if ("original_complex_citation" %in% names(citation) &&
          !is.na(citation$original_complex_citation)) {
        context_row$original_complex_citation <- citation$original_complex_citation
      }

      citation_contexts <- dplyr::bind_rows(citation_contexts, context_row)
    }
  }

  # Citation-reference mapping
  citation_references_mapping <- NULL
  parsed_references <- NULL
  if (is.null(mailto)) {
    mailto <- Sys.getenv("CROSSREF_MAILTO")
    if (mailto == "") {
      mailto <- "your@email.com"
    }
  }

  if (!is.null(doi)) {
    tryCatch({
      message("Attempting to retrieve references from CrossRef...")
      refs_crossref <- get_crossref_references(doi, mailto)

      if (!is.null(refs_crossref) && nrow(refs_crossref) > 0) {
        parsed_references <- refs_crossref %>%
          dplyr::mutate(
            ref_id = paste0("REF_", row_number()),
            ref_full_text = paste(
              ifelse(!is.na(author), author, ""),
              ifelse(!is.na(year), paste0("(", year, ")"), ""),
              ifelse(!is.na(article_title), article_title, ""),
              ifelse(!is.na(journal), journal, ""),
              sep = " "
            ) %>%  stringr::str_trim() %>%  stringr::str_squish(),
            ref_first_author = stringr::str_extract(author, "^[A-Za-z'-]+") %>%  stringr::str_to_title(),
            ref_first_author_normalized = stringr::str_to_lower(ref_first_author),
            ref_year = as.character(year),
            ref_authors = author,
            n_authors = 1
          ) %>%
          dplyr::select(ref_id, ref_full_text, ref_authors, ref_year,
                        ref_first_author, ref_first_author_normalized, n_authors) %>%
          mutate(ref_source = "crossref")

        message(paste("Successfully retrieved", nrow(parsed_references), "references from CrossRef"))
      }
    }, error = function(e) {
      warning(paste("Failed to retrieve from CrossRef:", e$message))
    })
  }

  if (is.null(parsed_references) && !is.null(references_section) && references_section != "") {
    message("Parsing references from text...")
    parsed_references <- parse_references_section(references_section) %>%
      mutate(ref_source = "parsed")
  }

  if (!is.null(parsed_references) && nrow(parsed_references) > 0 &&
      nrow(all_citations) > 0) {

    citation_references_mapping <- match_citations_to_references(
      citations_df = all_citations %>%
        dplyr::select(citation_id, citation_text, citation_text_clean, citation_type),
      references_df = parsed_references
    )
  }

  # Add ref_full_text to citation_contexts
  if (!is.null(citation_references_mapping) && nrow(citation_contexts) > 0) {
    citation_contexts <- citation_contexts %>%
      dplyr::left_join(
        citation_references_mapping %>%
          dplyr::select(citation_id, matched_ref_id, ref_full_text, match_confidence),
        by = "citation_id"
      )
  }

  # Compile results
  results$text_analytics <- list(
    basic_stats = text_stats,
    total_citations_found = nrow(all_citations),
    citation_types_found = unique(all_citations$citation_type),
    most_frequent_words = word_frequencies %>%  dplyr::slice_head(n = 20)
  )

  results$citations <- all_citations
  results$citation_contexts <- citation_contexts
  results$citation_metrics <- citation_metrics
  results$citation_references_mapping <- citation_references_mapping
  results$parsed_references <- parsed_references
  results$word_frequencies <- word_frequencies
  results$ngrams <- ngrams_results
  results$network_data <- citation_cooccurrence

  # Handle section colors safely - check if citation_contexts has data and section column
  if (nrow(citation_contexts) > 0 && "section" %in% names(citation_contexts)) {
    section_values <- unique(citation_contexts$section)
    section_colors <- colorlist()[1:length(section_values)]
    names(section_colors) <- section_values
  } else {
    # Fallback when there are no citations or sections
    section_colors <- c("Full_text" = colorlist()[1])
  }
  results$section_colors <- section_colors

  results$summary <- list(
    total_words_analyzed = nrow(tokens),
    unique_words = nrow(word_frequencies),
    citations_extracted = nrow(all_citations),
    narrative_citations = sum(stringr::str_detect(all_citations$citation_type, "narrative"), na.rm = TRUE),
    parenthetical_citations = sum(!stringr::str_detect(all_citations$citation_type, "narrative"), na.rm = TRUE),
    complex_citations_parsed = sum(all_citations$citation_type == "parsed_from_multiple", na.rm = TRUE),
    lexical_diversity = nrow(word_frequencies) / nrow(tokens),
    average_citation_context_length = if(nrow(citation_contexts) > 0) mean(citation_contexts$context_word_count) else 0,
    citation_density_per_1000_words = if(nrow(all_citations) > 0) round((nrow(all_citations) / text_stats$total_words) * 1000, 2) else 0,
    references_parsed = if (!is.null(parsed_references)) nrow(parsed_references) else 0,
    citations_matched_to_refs = if (!is.null(citation_references_mapping)) {
      sum(citation_references_mapping$match_confidence %in% c("high", "high_second_author", "medium_multiple_matches", "medium_fuzzy"), na.rm = TRUE)
    } else {
      0
    },
    match_quality = if (!is.null(citation_references_mapping)) {
      citation_references_mapping %>%
        dplyr::count(match_confidence) %>%
        dplyr::mutate(percentage = round(n / sum(n) * 100, 1))
    } else {
      NULL
    }
  )

  class(results) <- c("enhanced_scientific_content_analysis", "list")
  return(results)
}

#' Print diagnostic information about citation-reference matching
#'
#' @param results Output from analyze_scientific_content_enhanced()
#'
#' @return Invisibly returns match summary
#'
#' @export
print_matching_diagnostics <- function(results) {

  cat("\n=== CITATION-REFERENCE MATCHING DIAGNOSTICS ===\n\n")

  if (is.null(results$citation_references_mapping)) {
    cat("No citation-reference mapping performed (missing References section)\n")
    return(invisible(NULL))
  }

  mapping <- results$citation_references_mapping

  cat("Total citations:", nrow(mapping), "\n")
  cat("Total references parsed:", nrow(results$parsed_references), "\n\n")

  cat("Match quality distribution:\n")
  match_summary <- mapping %>%
    dplyr::count(match_confidence) %>%
    dplyr::arrange(dplyr::desc(n))
  print(match_summary)

  cat("\n")
  cat("Match rate:",
      round(sum(!is.na(mapping$matched_ref_id)) / nrow(mapping) * 100, 1),
      "%\n")

  cat("\nHigh confidence matches:",
      sum(mapping$match_confidence %in% c("high", "high_second_author"), na.rm = TRUE), "\n")

  cat("\nCitations without matches:\n")
  unmatched <- mapping %>%
    dplyr::filter(is.na(matched_ref_id)) %>%
    dplyr::select(citation_text_clean, cite_author, cite_year, match_confidence)

  if (nrow(unmatched) > 0) {
    print(head(unmatched, 10))
    if (nrow(unmatched) > 10) {
      cat("... and", nrow(unmatched) - 10, "more\n")
    }
  } else {
    cat("All citations matched!\n")
  }

  invisible(match_summary)
}

# color palette
colorlist <- function() {
  c(
    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
    "#B3B3B3", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#B15928", "#8DD3C7", "#BEBADA",
    "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#D9D9D9", "#BC80BD", "#CCEBC5"
  )
}
