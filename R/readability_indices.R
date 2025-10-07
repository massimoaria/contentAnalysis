#' Calculate readability indices for text
#'
#' @description
#' Calculates multiple readability indices including Flesch-Kincaid Grade Level,
#' Flesch Reading Ease, Automated Readability Index (ARI), and Gunning Fog Index.
#'
#' @param text Character vector containing the text to analyze
#' @param detailed Logical, if TRUE returns detailed statistics along with indices
#'
#' @return A tibble with the following columns:
#'   \itemize{
#'     \item flesch_kincaid_grade: US grade level required to understand the text
#'     \item flesch_reading_ease: Score from 0-100 (higher = easier to read)
#'     \item automated_readability_index: ARI grade level
#'     \item gunning_fog_index: Grade level based on sentence length and complex words
#'   }
#'
#'   If detailed = TRUE, also includes:
#'   \itemize{
#'     \item n_sentences: Number of sentences
#'     \item n_words: Number of words
#'     \item n_syllables: Total syllables
#'     \item n_characters: Total characters
#'     \item n_complex_words: Words with 3+ syllables
#'     \item avg_sentence_length: Average words per sentence
#'     \item avg_syllables_per_word: Average syllables per word
#'     \item pct_complex_words: Percentage of complex words
#'   }
#'
#' @details
#' **Formulas:**
#'
#' *Flesch-Kincaid Grade Level:*
#' \deqn{0.39 \times \frac{words}{sentences} + 11.8 \times \frac{syllables}{words} - 15.59}
#'
#' *Flesch Reading Ease:*
#' \deqn{206.835 - 1.015 \times \frac{words}{sentences} - 84.6 \times \frac{syllables}{words}}
#'
#' *Automated Readability Index (ARI):*
#' \deqn{4.71 \times \frac{characters}{words} + 0.5 \times \frac{words}{sentences} - 21.43}
#'
#' *Gunning Fog Index:*
#' \deqn{0.4 \times (\frac{words}{sentences} + 100 \times \frac{complex\_words}{words})}
#'
#' where complex words are those with 3 or more syllables.
#'
#' @examples
#' \dontrun{
#' # Simple text
#' text <- "The cat sat on the mat. It was a sunny day."
#' readability <- calculate_readability_indices(text)
#'
#' # With detailed statistics
#' text2 <- "Reading is fun. Books open new worlds. They teach us many things."
#' readability_detailed <- calculate_readability_indices(text2, detailed = TRUE)
#' }
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom stringr str_split str_count str_to_lower str_trim str_replace_all
calculate_readability_indices <- function(text, detailed = FALSE) {

  # Validate input
  if (is.na(text) || nchar(text) == 0) {
    return(create_empty_readability_tibble(detailed))
  }

  # Clean text
  clean_text <- stringr::str_trim(text)

  # Count sentences
  # Split on period, exclamation, question mark followed by space or end
  sentences <- stringr::str_split(clean_text, "[.!?]+\\s+|[.!?]+$")[[1]]
  sentences <- sentences[nchar(stringr::str_trim(sentences)) > 0]
  n_sentences <- length(sentences)

  if (n_sentences == 0) {
    return(create_empty_readability_tibble(detailed))
  }

  # Extract words (alphanumeric sequences)
  words <- stringr::str_extract_all(clean_text, "\\b[a-zA-Z]+\\b")[[1]]
  words <- stringr::str_to_lower(words)
  n_words <- length(words)

  if (n_words == 0) {
    return(create_empty_readability_tibble(detailed))
  }

  # Count characters (letters only, no spaces or punctuation)
  n_characters <- sum(nchar(words))

  # Count syllables for each word
  syllable_counts <- sapply(words, count_syllables)
  n_syllables <- sum(syllable_counts)

  # Count complex words (3+ syllables)
  complex_words <- sum(syllable_counts >= 3)

  # Calculate averages
  avg_sentence_length <- n_words / n_sentences
  avg_syllables_per_word <- n_syllables / n_words
  avg_characters_per_word <- n_characters / n_words
  pct_complex_words <- (complex_words / n_words) * 100

  # === FLESCH-KINCAID GRADE LEVEL ===
  flesch_kincaid_grade <- 0.39 * avg_sentence_length +
    11.8 * avg_syllables_per_word -
    15.59

  # === FLESCH READING EASE ===
  flesch_reading_ease <- 206.835 -
    1.015 * avg_sentence_length -
    84.6 * avg_syllables_per_word

  # === AUTOMATED READABILITY INDEX (ARI) ===
  ari <- 4.71 * avg_characters_per_word +
    0.5 * avg_sentence_length -
    21.43

  # === GUNNING FOG INDEX ===
  gunning_fog <- 0.4 * (avg_sentence_length + pct_complex_words)

  # Create result tibble
  if (detailed) {
    result <- tibble::tibble(
      flesch_kincaid_grade = round(flesch_kincaid_grade, 2),
      flesch_reading_ease = round(flesch_reading_ease, 2),
      automated_readability_index = round(ari, 2),
      gunning_fog_index = round(gunning_fog, 2),
      n_sentences = n_sentences,
      n_words = n_words,
      n_syllables = n_syllables,
      n_characters = n_characters,
      n_complex_words = complex_words,
      avg_sentence_length = round(avg_sentence_length, 2),
      avg_syllables_per_word = round(avg_syllables_per_word, 2),
      pct_complex_words = round(pct_complex_words, 2)
    )
  } else {
    result <- tibble::tibble(
      flesch_kincaid_grade = round(flesch_kincaid_grade, 2),
      flesch_reading_ease = round(flesch_reading_ease, 2),
      automated_readability_index = round(ari, 2),
      gunning_fog_index = round(gunning_fog, 2)
    )
  }

  return(result)
}


#' Count syllables in a word
#'
#' @description
#' Estimates the number of syllables in an English word using improved heuristic rules.
#'
#' @param word Character string containing a single word
#'
#' @return Integer number of syllables
#'
#' @details
#' Uses a rule-based approach with multiple adjustments:
#' \itemize{
#'   \item Counts vowel groups (consecutive vowels = 1 syllable)
#'   \item Adjusts for silent 'e' at end of words
#'   \item Handles common suffixes and prefixes
#'   \item Special cases for diphthongs and common patterns
#'   \item Ensures minimum of 1 syllable
#' }
#'
#' @keywords internal
count_syllables <- function(word) {
  if (is.na(word) || nchar(word) == 0) return(0)

  word <- stringr::str_to_lower(word)

  # Remove non-alphabetic characters
  word <- stringr::str_replace_all(word, "[^a-z]", "")

  if (nchar(word) == 0) return(0)

  # Single letter words
  if (nchar(word) == 1) return(1)

  # Count vowel groups (consecutive vowels count as one syllable)
  vowel_groups <- stringr::str_count(word, "[aeiouy]+")

  # Subtract 1 for silent 'e' at the end
  # But not if the word ends in 'le', 'les', or if it's the only vowel group
  if (stringr::str_detect(word, "e$") && vowel_groups > 1) {
    # Keep the 'e' if word ends in 'le' (like 'able', 'table')
    if (!stringr::str_detect(word, "[^aeiouy]le$")) {
      vowel_groups <- vowel_groups - 1
    }
  }

  # Subtract 1 for 'es' or 'ed' endings (but not for -ted, -ded, -ies, -ied, -ues, -ued)
  if (stringr::str_detect(word, "ed$") &&
      !stringr::str_detect(word, "[td]ed$|ied$") &&
      vowel_groups > 1) {
    vowel_groups <- vowel_groups - 1
  }

  if (stringr::str_detect(word, "es$") &&
      !stringr::str_detect(word, "[aeiou]les$|ies$|ues$") &&
      vowel_groups > 1) {
    vowel_groups <- vowel_groups - 1
  }

  # Subtract for 'ing' ending if it follows a vowel (like 'being')
  # but the vowel group was already counted
  # This helps with words like "doing" (2 syllables not 3)
  if (stringr::str_detect(word, "[aeiou]ing$") && vowel_groups > 1) {
    # Already counted correctly, no adjustment needed
  }

  # Add 1 for 'ion', 'ious', 'ial' endings which often add a syllable
  if (stringr::str_detect(word, "i[aou][ns]$")) {
    # Check if the 'i' is already counted separately
    # In words like "action" (ac-tion), the 'io' should be 2 syllables
    if (!stringr::str_detect(word, "[aeiou]i[aou][ns]$")) {
      vowel_groups <- vowel_groups + 1
    }
  }

  # Common exceptions and adjustments
  # Words ending in 'ism', 'ist' typically keep their syllable count

  # Ensure at least 1 syllable
  if (vowel_groups < 1) vowel_groups <- 1

  return(vowel_groups)
}


#' Create empty readability tibble
#'
#' @description
#' Creates an empty tibble with NA values for readability indices.
#'
#' @param detailed Logical, if TRUE includes detailed statistics columns
#'
#' @return Tibble with NA values
#'
#' @keywords internal
create_empty_readability_tibble <- function(detailed = FALSE) {
  if (detailed) {
    tibble::tibble(
      flesch_kincaid_grade = NA_real_,
      flesch_reading_ease = NA_real_,
      automated_readability_index = NA_real_,
      gunning_fog_index = NA_real_,
      n_sentences = NA_integer_,
      n_words = NA_integer_,
      n_syllables = NA_integer_,
      n_characters = NA_integer_,
      n_complex_words = NA_integer_,
      avg_sentence_length = NA_real_,
      avg_syllables_per_word = NA_real_,
      pct_complex_words = NA_real_
    )
  } else {
    tibble::tibble(
      flesch_kincaid_grade = NA_real_,
      flesch_reading_ease = NA_real_,
      automated_readability_index = NA_real_,
      gunning_fog_index = NA_real_
    )
  }
}


#' Calculate readability indices for multiple texts
#'
#' @description
#' Vectorized version that calculates readability indices for multiple texts.
#'
#' @param texts Character vector containing texts to analyze
#' @param detailed Logical, if TRUE returns detailed statistics along with indices
#' @param text_id Optional character vector with identifiers for each text
#'
#' @return A tibble with one row per text, including text_id if provided
#'
#' @examples
#' \dontrun{
#' texts <- c("First text here.", "Second text is longer and more complex.")
#' ids <- c("doc1", "doc2")
#' readability_multiple(texts, text_id = ids, detailed = TRUE)
#' }
#'
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map_dfr
readability_multiple <- function(texts, detailed = FALSE, text_id = NULL) {

  # Validate text_id length if provided
  if (!is.null(text_id) && length(text_id) != length(texts)) {
    stop("text_id must have the same length as texts")
  }

  # Calculate readability for each text
  results <- purrr::map_dfr(
    seq_along(texts),
    function(i) {
      result <- calculate_readability_indices(texts[i], detailed = detailed)

      # Add text_id if provided
      if (!is.null(text_id)) {
        result <- result %>%
          dplyr::mutate(text_id = text_id[i], .before = 1)
      }

      return(result)
    }
  )

  return(results)
}
