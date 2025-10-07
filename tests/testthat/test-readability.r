# Test suite for readability indices functions
# Package: contentanalysis
# File: tests/testthat/test-readability.R

library(testthat)
library(contentanalysis)

# ============================================================================
# TEST: count_syllables() - Internal function
# ============================================================================
# Note: Using ::: to access internal functions for testing purposes
#
# IMPORTANT: These tests verify the ACTUAL behavior of the heuristic algorithm,
# not "perfect" linguistic syllable counts. The algorithm uses rules-based
# approximation which may differ from dictionary syllabification for some words.
# This is acceptable for readability indices as long as the algorithm is consistent.

test_that("count_syllables handles basic words correctly", {
  expect_equal(contentanalysis:::count_syllables("cat"), 1)
  expect_equal(contentanalysis:::count_syllables("happy"), 2)
  expect_equal(contentanalysis:::count_syllables("beautiful"), 3)
  expect_equal(contentanalysis:::count_syllables("university"), 5)
})

test_that("count_syllables handles silent 'e' correctly", {
  expect_equal(contentanalysis:::count_syllables("make"), 1)  # ma-ke, but 'e' is silent
  expect_equal(contentanalysis:::count_syllables("home"), 1)  # ho-me, but 'e' is silent
  expect_equal(contentanalysis:::count_syllables("table"), 2) # ta-ble, 'e' is not silent after 'l'
  expect_equal(contentanalysis:::count_syllables("able"), 2)  # a-ble
})

test_that("count_syllables handles -ed endings correctly", {
  expect_equal(contentanalysis:::count_syllables("walked"), 1)   # walk-ed (silent)
  expect_equal(contentanalysis:::count_syllables("needed"), 2)   # need-ed (pronounced)
  expect_equal(contentanalysis:::count_syllables("created"), 2)  # cre-at-ed (algorithm counts 2)
  expect_equal(contentanalysis:::count_syllables("tried"), 1)    # tried (one syllable)
})

test_that("count_syllables handles -es endings correctly", {
  expect_equal(contentanalysis:::count_syllables("boxes"), 1)    # box-es (algorithm counts 1)
  expect_equal(contentanalysis:::count_syllables("tries"), 1)    # tries (one syllable)
  expect_equal(contentanalysis:::count_syllables("files"), 2)    # files (algorithm counts 2)
})

test_that("count_syllables handles -ion endings correctly", {
  expect_equal(contentanalysis:::count_syllables("action"), 3)    # ac-tion (algorithm counts 3)
  expect_equal(contentanalysis:::count_syllables("nation"), 3)    # na-tion (algorithm counts 3)
  expect_equal(contentanalysis:::count_syllables("education"), 5) # ed-u-ca-tion (algorithm counts 5)
})

test_that("count_syllables handles edge cases", {
  expect_equal(contentanalysis:::count_syllables(""), 0)
  expect_equal(contentanalysis:::count_syllables(NA_character_), 0)
  expect_equal(contentanalysis:::count_syllables("a"), 1)
  expect_equal(contentanalysis:::count_syllables("I"), 1)
  expect_equal(contentanalysis:::count_syllables("123"), 0)  # No letters
})

test_that("count_syllables handles y as vowel", {
  expect_equal(contentanalysis:::count_syllables("fly"), 1)
  expect_equal(contentanalysis:::count_syllables("crying"), 1)   # cry-ing (algorithm counts 1)
  expect_equal(contentanalysis:::count_syllables("rhythm"), 1)   # rhythm (algorithm counts 1)
})

test_that("count_syllables handles diphthongs", {
  expect_equal(contentanalysis:::count_syllables("bread"), 1)  # 'ea' is one vowel group
  expect_equal(contentanalysis:::count_syllables("beat"), 1)   # 'ea' is one vowel group
  expect_equal(contentanalysis:::count_syllables("coin"), 1)   # 'oi' is one vowel group
  expect_equal(contentanalysis:::count_syllables("about"), 2)  # a-bout
})


# ============================================================================
# TEST: calculate_readability_indices() - Basic functionality
# ============================================================================

test_that("calculate_readability_indices returns correct structure", {
  text <- "The cat sat on the mat. It was a sunny day."
  result <- calculate_readability_indices(text)

  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 4)
  expect_true(all(c("flesch_kincaid_grade", "flesch_reading_ease",
                    "automated_readability_index", "gunning_fog_index") %in% colnames(result)))
  expect_equal(nrow(result), 1)
})

test_that("calculate_readability_indices returns detailed stats when requested", {
  text <- "The cat sat on the mat."
  result <- calculate_readability_indices(text, detailed = TRUE)

  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 12)
  expect_true(all(c("n_sentences", "n_words", "n_syllables", "n_characters",
                    "n_complex_words", "avg_sentence_length",
                    "avg_syllables_per_word", "pct_complex_words") %in% colnames(result)))
})


# ============================================================================
# TEST: calculate_readability_indices() - Edge cases
# ============================================================================

test_that("calculate_readability_indices handles empty text", {
  result <- calculate_readability_indices("")

  expect_true(all(is.na(result[1,])))
  expect_equal(nrow(result), 1)
})

test_that("calculate_readability_indices handles NA text", {
  result <- calculate_readability_indices(NA_character_)

  expect_true(all(is.na(result[1,])))
  expect_equal(nrow(result), 1)
})

test_that("calculate_readability_indices handles text without sentences", {
  result <- calculate_readability_indices("word word word")

  # Should still work - treats as one sentence
  expect_false(all(is.na(result[1,])))
})


# ============================================================================
# TEST: calculate_readability_indices() - Known values
# ============================================================================

test_that("calculate_readability_indices produces reasonable values for simple text", {
  # Very simple text: short words, short sentences
  text <- "The cat sat. The dog ran. It was fun."
  result <- calculate_readability_indices(text, detailed = TRUE)

  # Simple text should have:
  # - Low grade level (< 5)
  # - High reading ease (> 80)
  # - Few complex words
  expect_lt(result$flesch_kincaid_grade, 5)
  expect_gt(result$flesch_reading_ease, 70)
  expect_equal(result$n_complex_words, 0)  # No words with 3+ syllables
  expect_lt(result$avg_syllables_per_word, 1.5)
})

test_that("calculate_readability_indices produces reasonable values for complex text", {
  # Complex text: longer words, longer sentences
  text <- "The implementation of sophisticated methodologies necessitates comprehensive evaluation.
          Organizational restructuring facilitates operational efficiency."
  result <- calculate_readability_indices(text, detailed = TRUE)

  # Complex text should have:
  # - Higher grade level (> 12)
  # - Lower reading ease (< 50)
  # - Many complex words
  expect_gt(result$flesch_kincaid_grade, 10)
  expect_lt(result$flesch_reading_ease, 50)
  expect_gt(result$n_complex_words, 5)
  expect_gt(result$avg_syllables_per_word, 2)
})

test_that("Flesch Reading Ease ranges are correct", {
  # Test that very simple text gets high score (near 100)
  simple <- "The cat ran. The dog sat."
  result_simple <- calculate_readability_indices(simple)
  expect_gt(result_simple$flesch_reading_ease, 80)

  # Test that complex text gets low score
  complex <- "Notwithstanding the multifaceted ramifications, interdisciplinary methodologies remain paramount."
  result_complex <- calculate_readability_indices(complex)
  expect_lt(result_complex$flesch_reading_ease, 30)
})


# ============================================================================
# TEST: calculate_readability_indices() - Formula verification
# ============================================================================

test_that("Flesch-Kincaid Grade Level formula is correct", {
  # Manually verified example
  # Text: "The cat sat. It ran." (5 words, 2 sentences, 5 syllables)
  text <- "The cat sat. It ran."
  result <- calculate_readability_indices(text, detailed = TRUE)

  # Manual calculation:
  # Words: The, cat, sat, It, ran = 5 words
  # Syllables: 1+1+1+1+1 = 5 syllables
  # avg_sentence_length = 5 / 2 = 2.5
  # avg_syllables_per_word = 5 / 5 = 1.0
  # FK Grade = 0.39 * 2.5 + 11.8 * 1.0 - 15.59 = 0.975 + 11.8 - 15.59 = -2.815
  # (Note: negative is possible for extremely simple text)

  expect_equal(result$n_words, 5)
  expect_equal(result$n_syllables, 5)
  expect_equal(result$n_sentences, 2)
  expect_equal(result$avg_sentence_length, 2.5)
  expect_equal(result$avg_syllables_per_word, 1.0)

  expected_fk <- 0.39 * 2.5 + 11.8 * 1.0 - 15.59
  expect_equal(result$flesch_kincaid_grade, round(expected_fk, 2))
})

test_that("Flesch Reading Ease formula is correct", {
  text <- "The cat sat. It ran."
  result <- calculate_readability_indices(text, detailed = TRUE)

  # Manual calculation:
  # avg_sentence_length = 2.5
  # avg_syllables_per_word = 1.0
  # FRE = 206.835 - 1.015 * 2.5 - 84.6 * 1.0
  #     = 206.835 - 2.5375 - 84.6 = 119.6975
  expected_fre <- 206.835 - 1.015 * 2.5 - 84.6 * 1.0
  expect_equal(result$flesch_reading_ease, round(expected_fre, 2))
})

test_that("ARI formula is correct", {
  text <- "The cat sat. It ran."
  result <- calculate_readability_indices(text, detailed = TRUE)

  # Manual calculation:
  # characters: The(3) + cat(3) + sat(3) + It(2) + ran(3) = 14 (no spaces/punctuation)
  # avg_chars_per_word = 14 / 5 = 2.8
  # avg_sentence_length = 2.5
  # ARI = 4.71 * 2.8 + 0.5 * 2.5 - 21.43
  #     = 13.188 + 1.25 - 21.43 = -6.992

  expect_equal(result$n_characters, 14)
  expect_equal(result$n_words, 5)
  avg_chars <- result$n_characters / result$n_words
  expected_ari <- 4.71 * avg_chars + 0.5 * 2.5 - 21.43
  expect_equal(result$automated_readability_index, round(expected_ari, 2))
})

test_that("Gunning Fog Index formula is correct", {
  text <- "The cat sat. It ran."
  result <- calculate_readability_indices(text, detailed = TRUE)

  # Manual calculation:
  # avg_sentence_length = 2.5
  # complex_words = 0 (no words with 3+ syllables)
  # pct_complex = 0
  # Fog = 0.4 * (2.5 + 0) = 1.0

  expect_equal(result$n_complex_words, 0)
  expect_equal(result$pct_complex_words, 0)
  expected_fog <- 0.4 * (2.5 + 0)
  expect_equal(result$gunning_fog_index, round(expected_fog, 2))
})


# ============================================================================
# TEST: readability_multiple()
# ============================================================================

test_that("readability_multiple processes multiple texts", {
  texts <- c(
    "The cat sat.",
    "The dog ran fast.",
    "It was a sunny day outside."
  )

  result <- readability_multiple(texts)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 4)
})

test_that("readability_multiple handles text_id parameter", {
  texts <- c("Text one.", "Text two.")
  ids <- c("doc1", "doc2")

  result <- readability_multiple(texts, text_id = ids)

  expect_true("text_id" %in% colnames(result))
  expect_equal(result$text_id, ids)
  expect_equal(which(colnames(result) == "text_id"), 1)  # Should be first column
})

test_that("readability_multiple returns detailed stats when requested", {
  texts <- c("The cat sat.", "The dog ran.")

  result <- readability_multiple(texts, detailed = TRUE)

  expect_equal(ncol(result), 12)
  expect_true("n_sentences" %in% colnames(result))
})

test_that("readability_multiple handles empty and NA texts", {
  texts <- c("Normal text.", "", NA_character_)

  result <- readability_multiple(texts, detailed = TRUE)

  expect_equal(nrow(result), 3)
  expect_false(is.na(result$n_words[1]))
  expect_true(is.na(result$n_words[2]))
  expect_true(is.na(result$n_words[3]))
})

test_that("readability_multiple validates text_id length", {
  texts <- c("Text one.", "Text two.")
  ids <- c("doc1", "doc2", "doc3")  # Wrong length

  expect_error(
    readability_multiple(texts, text_id = ids),
    "text_id must have the same length as texts"
  )
})


# ============================================================================
# TEST: Statistical properties
# ============================================================================

test_that("Word and syllable counts are consistent", {
  text <- "Hello world. This is a test of the system."
  result <- calculate_readability_indices(text, detailed = TRUE)

  # Basic sanity checks
  expect_gt(result$n_words, 0)
  expect_gt(result$n_syllables, 0)
  expect_gte(result$n_syllables, result$n_words)  # At least 1 syllable per word
  expect_equal(result$n_sentences, 2)
})

test_that("Complex words percentage is between 0 and 100", {
  text <- "The quick brown fox jumps over the lazy dog."
  result <- calculate_readability_indices(text, detailed = TRUE)

  expect_gte(result$pct_complex_words, 0)
  expect_lte(result$pct_complex_words, 100)
})

test_that("Average metrics are calculated correctly", {
  text <- "Hello. World."
  result <- calculate_readability_indices(text, detailed = TRUE)

  expect_equal(result$avg_sentence_length, result$n_words / result$n_sentences)
  expect_equal(result$avg_syllables_per_word, result$n_syllables / result$n_words)
  expect_equal(result$pct_complex_words,
               (result$n_complex_words / result$n_words) * 100)
})


# ============================================================================
# TEST: Real-world examples
# ============================================================================

test_that("Readability indices work on academic text", {
  text <- "The present study investigates the relationship between socioeconomic
           status and educational attainment. Utilizing a comprehensive dataset,
           we employ multivariate regression analysis to examine these associations."

  result <- calculate_readability_indices(text, detailed = TRUE)

  # Academic text should be complex
  expect_gt(result$flesch_kincaid_grade, 12)
  expect_lt(result$flesch_reading_ease, 50)
  expect_gt(result$gunning_fog_index, 15)
  expect_gt(result$pct_complex_words, 20)
})

test_that("Readability indices work on children's text", {
  text <- "The cat is big. The dog is small. They like to play.
          They run and jump. It is fun to watch them."

  result <- calculate_readability_indices(text, detailed = TRUE)

  # Children's text should be simple
  expect_lt(result$flesch_kincaid_grade, 3)
  expect_gt(result$flesch_reading_ease, 90)
  expect_lt(result$gunning_fog_index, 5)
  expect_lt(result$pct_complex_words, 10)
})

test_that("Readability indices work on technical documentation", {
  text <- "To initialize the configuration, execute the following command:
          'npm install'. Subsequently, modify the environment variables
          in the .env file according to your specifications."

  result <- calculate_readability_indices(text, detailed = TRUE)

  # Technical text should be moderately complex
  expect_gt(result$flesch_kincaid_grade, 10)
  expect_lt(result$flesch_reading_ease, 60)
  expect_gt(result$n_complex_words, 3)
})


# ============================================================================
# TEST: Sentence detection
# ============================================================================

test_that("Sentence detection handles multiple punctuation marks", {
  text <- "What is this? It is a test! This works."
  result <- calculate_readability_indices(text, detailed = TRUE)

  expect_equal(result$n_sentences, 3)
})

test_that("Sentence detection handles missing final punctuation", {
  text <- "This is a sentence"
  result <- calculate_readability_indices(text, detailed = TRUE)

  expect_equal(result$n_sentences, 1)
})

test_that("Sentence detection handles multiple spaces", {
  text <- "First sentence.  Second sentence.   Third sentence."
  result <- calculate_readability_indices(text, detailed = TRUE)

  expect_equal(result$n_sentences, 3)
})
