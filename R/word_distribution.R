#' Calculate word distribution across text segments or sections
#'
#' @description
#' Calculates the frequency of selected words/n-grams across document sections
#' or equal-length segments.
#'
#' @param text Character string or named list. Document text or text with sections.
#' @param selected_words Character vector. Words/n-grams to track.
#' @param use_sections Logical or "auto". Use document sections if available (default: "auto").
#' @param n_segments Integer. Number of segments if not using sections (default: 10).
#' @param remove_stopwords Logical. Remove stopwords before analysis (default: TRUE).
#' @param language Character. Language for stopwords (default: "en").
#'
#' @return Tibble with columns:
#'   \itemize{
#'     \item segment_id: Segment identifier
#'     \item segment_name: Section name or segment number
#'     \item segment_type: "section" or "equal_length"
#'     \item word: Word/n-gram
#'     \item count: Absolute frequency
#'     \item total_words: Total words in segment
#'     \item relative_frequency: Proportion of total words
#'     \item percentage: Percentage representation
#'   }
#'   Attributes include metadata about segmentation used.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Automatically detects if sections are available
#'   \item Removes stopwords before creating n-grams (if requested)
#'   \item Supports unigrams, bigrams, trigrams, etc.
#'   \item Calculates both absolute and relative frequencies
#' }
#'
#' @examples
#' \dontrun{
#' doc <- pdf2txt_auto("paper.pdf")
#'
#' # Track specific words across sections
#' words_to_track <- c("machine learning", "neural network", "accuracy")
#' dist <- calculate_word_distribution(doc, words_to_track)
#'
#' # Use equal-length segments instead
#' dist <- calculate_word_distribution(doc, words_to_track,
#'                                     use_sections = FALSE,
#'                                     n_segments = 20)
#' }
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr mutate filter bind_rows left_join count arrange
#' @importFrom tidytext unnest_tokens
#' @importFrom stringr str_length str_trim str_replace_all str_trunc str_to_lower
calculate_word_distribution <- function(text,
                                        selected_words,
                                        use_sections = "auto",
                                        n_segments = 10,
                                        remove_stopwords = TRUE,
                                        language = "en") {

  selected_words <- stringr::str_to_lower(selected_words)

  max_ngram <- max(sapply(selected_words, function(w) {
    length(strsplit(w, "\\s+")[[1]])
  }))

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
      warning("Sections requested but not available. Using equal-length segments instead.")
      use_sections_final <- FALSE
    } else {
      use_sections_final <- use_sections
    }
  } else {
    stop("use_sections must be TRUE, FALSE, or 'auto'")
  }

  segment_texts <- list()
  segment_info <- tibble::tibble()

  if (use_sections_final) {
    for (i in seq_along(section_names)) {
      section_name <- section_names[i]
      section_text <- text[[section_name]]

      if (!is.null(section_text) && nchar(section_text) > 0) {
        segment_texts[[i]] <- section_text
        segment_info <- dplyr::bind_rows(
          segment_info,
          tibble::tibble(
            segment_id = i,
            segment_name = section_name,
            segment_type = "section"
          )
        )
      }
    }
  } else {
    if (is.list(text)) {
      full_text <- text$Full_text
    } else {
      full_text <- text
    }

    full_text <- full_text %>%
      stringr::str_replace_all("\\n+", " ") %>%
      stringr::str_replace_all("\\s+", " ") %>%
      stringr::str_trim()

    total_chars <- nchar(full_text)
    chars_per_segment <- ceiling(total_chars / n_segments)

    for (i in 1:n_segments) {
      start_pos <- (i - 1) * chars_per_segment + 1
      end_pos <- min(i * chars_per_segment, total_chars)
      segment_texts[[i]] <- substr(full_text, start_pos, end_pos)
      segment_info <- dplyr::bind_rows(
        segment_info,
        tibble::tibble(
          segment_id = i,
          segment_name = paste0("Segment ", i),
          segment_type = "equal_length"
        )
      )
    }
  }

  if (remove_stopwords) {
    data(stop_words, package = "tidytext", envir = environment())
  }

  all_ngrams <- tibble::tibble()

  for (i in seq_along(segment_texts)) {
    segment_text <- segment_texts[[i]]

    tokens <- tibble::tibble(text = segment_text) %>%
      tidytext::unnest_tokens(word, text, token = "words")

    if (remove_stopwords) {
      tokens <- tokens %>%
        dplyr::anti_join(stop_words, by = "word")
    }

    clean_text <- paste(tokens$word, collapse = " ")

    for (n in 1:max_ngram) {
      ngrams <- tibble::tibble(text = clean_text) %>%
        tidytext::unnest_tokens(word, text, token = "ngrams", n = n) %>%
        dplyr::filter(!is.na(word)) %>%
        dplyr::mutate(
          segment_id = segment_info$segment_id[i],
          segment_name = segment_info$segment_name[i],
          segment_type = segment_info$segment_type[i],
          ngram_size = n
        )

      all_ngrams <- dplyr::bind_rows(all_ngrams, ngrams)
    }
  }

  word_counts <- all_ngrams %>%
    dplyr::filter(word %in% selected_words) %>%
    dplyr::count(segment_id, segment_name, segment_type, word, name = "count")

  total_per_segment <- all_ngrams %>%
    dplyr::filter(ngram_size == 1) %>%
    dplyr::count(segment_id, segment_name, segment_type, name = "total_words")

  result <- word_counts %>%
    dplyr::left_join(total_per_segment, by = c("segment_id", "segment_name", "segment_type")) %>%
    dplyr::mutate(
      relative_frequency = count / total_words,
      percentage = relative_frequency * 100
    ) %>%
    dplyr::arrange(segment_id, word)

  attr(result, "use_sections") <- use_sections_final
  attr(result, "sections_available") <- sections_available
  attr(result, "n_segments") <- length(unique(result$segment_id))
  attr(result, "selected_words") <- selected_words
  attr(result, "segment_type") <- if (use_sections_final) "section" else "equal_length"

  return(result)
}

#' Create interactive word distribution plot
#'
#' @description
#' Creates an interactive plotly visualization of word frequencies across
#' document segments or sections.
#'
#' @param word_distribution_data Tibble from calculate_word_distribution()
#' @param plot_type Character. "line" or "area" (default: "line").
#' @param smooth Logical. Apply smoothing to lines (default: FALSE).
#' @param show_points Logical. Show data points on lines (default: TRUE).
#' @param colors Character vector. Custom colors for words (optional).
#'
#' @return A plotly object with interactive visualization.
#'
#' @details
#' The plot shows:
#' \itemize{
#'   \item X-axis: Document sections or segments
#'   \item Y-axis: Relative frequency (percentage)
#'   \item Each word as a separate line/area
#'   \item Hover information with exact values
#' }
#'
#' @examples
#' \dontrun{
#' dist <- calculate_word_distribution(doc, c("method", "result", "conclusion"))
#' plot_word_distribution(dist, plot_type = "line", show_points = TRUE)
#'
#' # Area plot with custom colors
#' plot_word_distribution(dist, plot_type = "area",
#'                       colors = c("#FF6B6B", "#4ECDC4", "#45B7D1"))
#' }
#'
#' @export
#' @importFrom dplyr filter arrange mutate left_join select distinct
#' @importFrom tidyr replace_na
#' @importFrom stringr str_trunc
plot_word_distribution <- function(word_distribution_data,
                                   plot_type = "line",
                                   smooth = FALSE,
                                   show_points = TRUE,
                                   colors = NULL) {

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for this function. Install it with: install.packages('plotly')")
  }

  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("Package 'RColorBrewer' is required for this function. Install it with: install.packages('RColorBrewer')")
  }

  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Package 'scales' is required for this function. Install it with: install.packages('scales')")
  }

  use_sections <- attr(word_distribution_data, "use_sections")
  segment_type <- attr(word_distribution_data, "segment_type")
  selected_words <- attr(word_distribution_data, "selected_words")

  all_segments <- unique(word_distribution_data$segment_id)
  all_words <- unique(word_distribution_data$word)

  complete_data <- expand.grid(
    segment_id = all_segments,
    word = all_words,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(
      word_distribution_data %>%
        dplyr::select(segment_id, segment_name, word, relative_frequency, count),
      by = c("segment_id", "word")
    ) %>%
    dplyr::mutate(
      relative_frequency = tidyr::replace_na(relative_frequency, 0),
      count = tidyr::replace_na(count, 0)
    )

  segment_labels <- word_distribution_data %>%
    dplyr::distinct(segment_id, segment_name) %>%
    dplyr::arrange(segment_id)

  complete_data <- complete_data %>%
    dplyr::left_join(segment_labels, by = "segment_id") %>%
    dplyr::mutate(
      segment_name = dplyr::coalesce(segment_name.x, segment_name.y)
    ) %>%
    dplyr::select(-segment_name.x, -segment_name.y)

  n_words <- length(all_words)
  if (is.null(colors)) {
    if (n_words <= 8) {
      colors <- RColorBrewer::brewer.pal(max(3, n_words), "Set2")[1:n_words]
    } else {
      colors <- scales::hue_pal()(n_words)
    }
  }

  color_mapping <- setNames(colors, all_words)

  fig <- plotly::plot_ly()

  for (i in seq_along(all_words)) {
    word_i <- all_words[i]
    word_data <- complete_data %>%
      dplyr::filter(word == word_i) %>%
      dplyr::arrange(segment_id)

    hover_text <- paste0(
      "<b>Word:</b> ", word_i, "<br>",
      "<b>", if (use_sections) "Section" else "Segment", ":</b> ",
      word_data$segment_name, "<br>",
      "<b>Frequency:</b> ", round(word_data$relative_frequency * 100, 3), "%<br>",
      "<b>Count:</b> ", word_data$count
    )

    if (plot_type == "area") {
      fig <- fig %>%
        plotly::add_trace(
          data = word_data,
          x = ~segment_id,
          y = ~relative_frequency,
          type = 'scatter',
          mode = if (show_points) 'lines+markers' else 'lines',
          fill = 'tozeroy',
          fillcolor = paste0(color_mapping[word_i], "4D"),
          name = word_i,
          line = list(color = color_mapping[word_i], width = 2),
          marker = if (show_points) list(size = 8, color = color_mapping[word_i]) else NULL,
          text = hover_text,
          hovertemplate = "%{text}<extra></extra>"
        )
    } else {
      fig <- fig %>%
        plotly::add_trace(
          data = word_data,
          x = ~segment_id,
          y = ~relative_frequency,
          type = 'scatter',
          mode = if (show_points) 'lines+markers' else 'lines',
          name = word_i,
          line = list(
            color = color_mapping[word_i],
            width = 2.5,
            shape = if (smooth) 'spline' else 'linear'
          ),
          marker = if (show_points) list(
            size = 8,
            color = color_mapping[word_i],
            line = list(color = 'white', width = 1)
          ) else NULL,
          text = hover_text,
          hovertemplate = "%{text}<extra></extra>"
        )
    }
  }

  x_labels <- if (use_sections) {
    stringr::str_trunc(segment_labels$segment_name, 20)
  } else {
    as.character(segment_labels$segment_id)
  }

  fig <- fig %>%
    plotly::layout(
      title = list(
        text = paste0(
          "<b>Word Distribution Across Document</b><br>",
          "<sub>",
          if (use_sections) "By Document Section" else paste0("By Segment (", length(all_segments), " equal parts)"),
          " | Words: ", paste(selected_words, collapse = ", "),
          "</sub>"
        ),
        font = list(size = 16, color = "#2E86AB")
      ),
      xaxis = list(
        title = list(
          text = if (use_sections) "Document Section" else "Document Segments (chronological)",
          font = list(size = 12, family = "Arial, sans-serif", color = "#333")
        ),
        tickmode = "array",
        tickvals = segment_labels$segment_id,
        ticktext = x_labels,
        tickangle = if (use_sections) -45 else 0,
        gridcolor = "#e0e0e0",
        gridwidth = 1,
        showline = TRUE,
        linecolor = "#cccccc"
      ),
      yaxis = list(
        title = list(
          text = "Relative Frequency",
          font = list(size = 12, family = "Arial, sans-serif", color = "#333")
        ),
        tickformat = ".2%",
        gridcolor = "#e0e0e0",
        gridwidth = 1,
        showline = TRUE,
        linecolor = "#cccccc"
      ),
      hovermode = "closest",
      legend = list(
        title = list(text = "<b>Words</b>"),
        orientation = "v",
        x = 1.02,
        y = 1,
        xanchor = "left",
        bgcolor = "rgba(255, 255, 255, 0.9)",
        bordercolor = "#cccccc",
        borderwidth = 1,
        font = list(size = 11)
      ),
      plot_bgcolor = "#fafafa",
      paper_bgcolor = "white",
      margin = list(l = 60, r = 120, t = 80, b = 80)
    ) %>%
    plotly::config(
      displayModeBar = TRUE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d"),
      toImageButtonOptions = list(
        format = "png",
        filename = "word_distribution",
        height = 600,
        width = 1000,
        scale = 2
      )
    )

  return(fig)
}
