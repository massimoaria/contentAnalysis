# Test suite for word_distribution.R functions

library(testthat)
library(tibble)
#library(dplyr)

# ============================================================================
# calculate_word_distribution() - INPUT VALIDATION
# ============================================================================

test_that("calculate_word_distribution requires text input", {
  expect_error(
    calculate_word_distribution(selected_words = c("test")),
    "argument \"text\" is missing"
  )
})

test_that("calculate_word_distribution requires selected_words", {
  expect_error(
    calculate_word_distribution(text = "Sample text"),
    "argument \"selected_words\" is missing"
  )
})

test_that("calculate_word_distribution validates use_sections parameter", {
  expect_error(
    calculate_word_distribution(
      text = "Sample text",
      selected_words = c("test"),
      use_sections = "invalid"
    ),
    "use_sections must be TRUE, FALSE, or 'auto'"
  )
})

# ============================================================================
# calculate_word_distribution() - STRING INPUT
# ============================================================================

test_that("calculate_word_distribution works with string input", {
  skip_if_not_installed("tidytext")

  text <- "machine learning is important. machine learning helps research."

  result <- calculate_word_distribution(
    text = text,
    selected_words = c("machine", "learning"),
    use_sections = FALSE,
    n_segments = 2
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(
    c("segment_id", "segment_name", "word", "count") %in% names(result)
  ))
})

test_that("calculate_word_distribution counts word frequencies", {
  skip_if_not_installed("tidytext")

  text <- "test word test word test another"

  result <- calculate_word_distribution(
    text = text,
    selected_words = c("test", "word"),
    use_sections = FALSE,
    n_segments = 1,
    remove_stopwords = FALSE
  )

  # test appears 3 times, word appears 2 times
  test_count <- result %>% dplyr::filter(word == "test") %>% dplyr::pull(count)
  word_count <- result %>% dplyr::filter(word == "word") %>% dplyr::pull(count)

  expect_equal(test_count, 3)
  expect_equal(word_count, 2)
})

test_that("calculate_word_distribution calculates relative frequency", {
  skip_if_not_installed("tidytext")

  text <- "test test test other"

  result <- calculate_word_distribution(
    text = text,
    selected_words = c("test"),
    use_sections = FALSE,
    n_segments = 1,
    remove_stopwords = FALSE
  )

  expect_true("relative_frequency" %in% names(result))
  expect_true("percentage" %in% names(result))
  expect_true(all(
    result$relative_frequency >= 0 & result$relative_frequency <= 1
  ))
})

test_that("calculate_word_distribution creates equal-length segments", {
  skip_if_not_installed("tidytext")

  # Long text to divide into segments
  text <- paste(rep("word test sample", 100), collapse = " ")

  result <- calculate_word_distribution(
    text = text,
    selected_words = c("word", "test"),
    use_sections = FALSE,
    n_segments = 5,
    remove_stopwords = FALSE
  )

  # Should have 5 segments
  n_segments <- length(unique(result$segment_id))
  expect_equal(n_segments, 5)
  expect_equal(unique(result$segment_type), "equal_length")
})

# ============================================================================
# calculate_word_distribution() - LIST INPUT WITH SECTIONS
# ============================================================================

test_that("calculate_word_distribution detects sections automatically", {
  skip_if_not_installed("tidytext")

  text_list <- list(
    Introduction = "machine learning introduction text",
    Methods = "machine learning methods description",
    Results = "machine learning results data"
  )

  result <- calculate_word_distribution(
    text = text_list,
    selected_words = c("machine", "learning"),
    use_sections = "auto"
  )

  expect_equal(unique(result$segment_type), "section")
  expect_true(all(
    c("Introduction", "Methods", "Results") %in% result$segment_name
  ))
})

test_that("calculate_word_distribution excludes Full_text and References", {
  skip_if_not_installed("tidytext")

  text_list <- list(
    Full_text = "full text content",
    Introduction = "intro text",
    References = "reference list"
  )

  result <- calculate_word_distribution(
    text = text_list,
    selected_words = c("text"),
    use_sections = "auto"
  )

  # Should only use Introduction, not Full_text or References
  expect_false("Full_text" %in% result$segment_name)
  expect_false("References" %in% result$segment_name)
  expect_true("Introduction" %in% result$segment_name)
})

test_that("calculate_word_distribution uses Full_text when no sections", {
  skip_if_not_installed("tidytext")

  text_list <- list(
    Full_text = "this is the full text content for analysis"
  )

  result <- calculate_word_distribution(
    text = text_list,
    selected_words = c("text", "content"),
    use_sections = "auto",
    n_segments = 2
  )

  # Should fall back to segments
  expect_equal(unique(result$segment_type), "equal_length")
})

test_that("calculate_word_distribution forces sections when use_sections=TRUE", {
  skip_if_not_installed("tidytext")

  text_list <- list(
    Introduction = "intro text here",
    Methods = "methods text here"
  )

  result <- calculate_word_distribution(
    text = text_list,
    selected_words = c("text"),
    use_sections = TRUE
  )

  expect_equal(unique(result$segment_type), "section")
})

test_that("calculate_word_distribution warns when sections requested but unavailable", {
  skip_if_not_installed("tidytext")

  text_list <- list(
    Full_text = "just full text"
  )

  expect_warning(
    result <- calculate_word_distribution(
      text = text_list,
      selected_words = c("text"),
      use_sections = TRUE
    ),
    "Sections requested but not available"
  )
})

# ============================================================================
# calculate_word_distribution() - STOPWORDS
# ============================================================================

test_that("calculate_word_distribution removes stopwords when requested", {
  skip_if_not_installed("tidytext")

  text <- "the machine learning is the important and the useful"

  result_with_stops <- calculate_word_distribution(
    text = text,
    selected_words = c("the", "machine"),
    use_sections = FALSE,
    n_segments = 1,
    remove_stopwords = FALSE
  )

  result_no_stops <- calculate_word_distribution(
    text = text,
    selected_words = c("the", "machine"),
    use_sections = FALSE,
    n_segments = 1,
    remove_stopwords = TRUE
  )

  # "the" should appear with stopwords
  expect_true("the" %in% result_with_stops$word)

  # "the" should NOT appear without stopwords (or have 0 count)
  # Actually, selected_words are searched after stopword removal, so "the" won't be found
  the_count_no_stops <- result_no_stops %>%
    dplyr::filter(word == "the") %>%
    nrow()

  expect_equal(the_count_no_stops, 0)
})

# ============================================================================
# calculate_word_distribution() - N-GRAMS
# ============================================================================

test_that("calculate_word_distribution handles bigrams", {
  skip_if_not_installed("tidytext")

  text <- "machine learning is important machine learning helps research"

  result <- calculate_word_distribution(
    text = text,
    selected_words = c("machine learning"),
    use_sections = FALSE,
    n_segments = 1,
    remove_stopwords = FALSE
  )

  expect_true("machine learning" %in% result$word)
  ml_count <- result %>%
    dplyr::filter(word == "machine learning") %>%
    dplyr::pull(count)
  expect_equal(ml_count, 2)
})

test_that("calculate_word_distribution handles trigrams", {
  skip_if_not_installed("tidytext")

  text <- "neural network model is good neural network model works"

  result <- calculate_word_distribution(
    text = text,
    selected_words = c("neural network model"),
    use_sections = FALSE,
    n_segments = 1,
    remove_stopwords = FALSE
  )

  expect_true("neural network model" %in% result$word)
})

test_that("calculate_word_distribution handles mixed unigrams and bigrams", {
  skip_if_not_installed("tidytext")

  text <- "machine learning is important for data science research"

  result <- calculate_word_distribution(
    text = text,
    selected_words = c("machine", "machine learning", "data science"),
    use_sections = FALSE,
    n_segments = 1,
    remove_stopwords = FALSE
  )

  expect_true(all(
    c("machine", "machine learning", "data science") %in% result$word
  ))
})

# ============================================================================
# calculate_word_distribution() - OUTPUT STRUCTURE
# ============================================================================

test_that("calculate_word_distribution returns correct column structure", {
  skip_if_not_installed("tidytext")

  text <- "test text here"

  result <- calculate_word_distribution(
    text = text,
    selected_words = c("test"),
    use_sections = FALSE,
    n_segments = 1
  )

  expected_cols <- c(
    "segment_id",
    "segment_name",
    "segment_type",
    "word",
    "count",
    "total_words",
    "relative_frequency",
    "percentage"
  )

  expect_true(all(expected_cols %in% names(result)))
})

test_that("calculate_word_distribution sets attributes", {
  skip_if_not_installed("tidytext")

  text <- "test text"

  result <- calculate_word_distribution(
    text = text,
    selected_words = c("test"),
    use_sections = FALSE,
    n_segments = 3
  )

  expect_false(attr(result, "use_sections"))
  expect_false(attr(result, "sections_available"))
  expect_equal(attr(result, "segment_type"), "equal_length")
  expect_true(!is.null(attr(result, "selected_words")))
})

test_that("calculate_word_distribution handles empty results gracefully", {
  skip_if_not_installed("tidytext")

  text <- "this text has no matching words"

  result <- calculate_word_distribution(
    text = text,
    selected_words = c("nonexistent", "missing"),
    use_sections = FALSE,
    n_segments = 1,
    remove_stopwords = FALSE
  )

  # Should return empty or zero counts
  expect_s3_class(result, "tbl_df")
})

# ============================================================================
# calculate_word_distribution() - CASE SENSITIVITY
# ============================================================================

test_that("calculate_word_distribution is case insensitive", {
  skip_if_not_installed("tidytext")

  text <- "Machine Learning MACHINE learning"

  result <- calculate_word_distribution(
    text = text,
    selected_words = c("machine", "learning"),
    use_sections = FALSE,
    n_segments = 1,
    remove_stopwords = FALSE
  )

  # Should find both "Machine" and "MACHINE" as "machine"
  machine_count <- result %>%
    dplyr::filter(word == "machine") %>%
    dplyr::pull(count)
  expect_gte(machine_count, 2)
})

# ============================================================================
# calculate_word_distribution() - INTEGRATION TEST
# ============================================================================

test_that("calculate_word_distribution complete workflow with sections", {
  skip_if_not_installed("tidytext")

  text_list <- list(
    Introduction = "machine learning is a field of artificial intelligence that machine learning uses algorithms",
    Methods = "our machine learning approach uses neural networks and deep learning techniques",
    Results = "the machine learning model achieved high accuracy in prediction tasks",
    Discussion = "machine learning has many applications in data science and research"
  )

  result <- calculate_word_distribution(
    text = text_list,
    selected_words = c("machine learning", "neural networks"),
    use_sections = TRUE,
    remove_stopwords = TRUE
  )

  # Verify structure
  expect_s3_class(result, "tbl_df")
  expect_equal(unique(result$segment_type), "section")

  # Verify sections
  expect_true(all(
    c("Introduction", "Methods", "Results", "Discussion") %in%
      result$segment_name
  ))

  # Verify words tracked
  expect_true("machine learning" %in% result$word)

  # Verify metrics
  expect_true(all(!is.na(result$count)))
  expect_true(all(!is.na(result$relative_frequency)))
  expect_true(all(result$percentage >= 0))
})

# ============================================================================
# plot_word_distribution() - BASIC TESTS
# ============================================================================

test_that("plot_word_distribution requires plotly", {
  # Skip this test if plotly is installed (can't test the error)
  skip_if_not_installed("plotly")

  # This test would only work if plotly is NOT installed
  # Since we skip when it IS installed, we just verify the function exists
  expect_true(exists("plot_word_distribution"))
})

test_that("plot_word_distribution creates plotly object", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("RColorBrewer")
  skip_if_not_installed("scales")
  skip_if_not_installed("tidytext")

  # Create sample data
  text <- "machine learning is important machine learning helps"
  dist_data <- calculate_word_distribution(
    text = text,
    selected_words = c("machine", "learning"),
    use_sections = FALSE,
    n_segments = 2
  )

  result <- plot_word_distribution(dist_data)

  expect_s3_class(result, "plotly")
})

test_that("plot_word_distribution accepts plot_type parameter", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("RColorBrewer")
  skip_if_not_installed("scales")
  skip_if_not_installed("tidytext")

  text <- "test word test word"
  dist_data <- calculate_word_distribution(
    text = text,
    selected_words = c("test", "word"),
    use_sections = FALSE,
    n_segments = 2
  )

  result_line <- plot_word_distribution(dist_data, plot_type = "line")
  result_area <- plot_word_distribution(dist_data, plot_type = "area")

  expect_s3_class(result_line, "plotly")
  expect_s3_class(result_area, "plotly")
})

test_that("plot_word_distribution accepts custom colors", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("RColorBrewer")
  skip_if_not_installed("scales")
  skip_if_not_installed("tidytext")

  text <- "test word sample"
  dist_data <- calculate_word_distribution(
    text = text,
    selected_words = c("test", "word"),
    use_sections = FALSE,
    n_segments = 2
  )

  custom_colors <- c("#FF0000", "#00FF00")

  result <- plot_word_distribution(
    dist_data,
    colors = custom_colors
  )

  expect_s3_class(result, "plotly")
})

test_that("plot_word_distribution handles show_points parameter", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("RColorBrewer")
  skip_if_not_installed("scales")
  skip_if_not_installed("tidytext")

  text <- "test word"
  dist_data <- calculate_word_distribution(
    text = text,
    selected_words = c("test"),
    use_sections = FALSE,
    n_segments = 2
  )

  result_with_points <- plot_word_distribution(dist_data, show_points = TRUE)
  result_no_points <- plot_word_distribution(dist_data, show_points = FALSE)

  expect_s3_class(result_with_points, "plotly")
  expect_s3_class(result_no_points, "plotly")
})

test_that("plot_word_distribution handles smooth parameter", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("RColorBrewer")
  skip_if_not_installed("scales")
  skip_if_not_installed("tidytext")

  text <- "test word test"
  dist_data <- calculate_word_distribution(
    text = text,
    selected_words = c("test"),
    use_sections = FALSE,
    n_segments = 3
  )

  result_smooth <- plot_word_distribution(dist_data, smooth = TRUE)
  result_no_smooth <- plot_word_distribution(dist_data, smooth = FALSE)

  expect_s3_class(result_smooth, "plotly")
  expect_s3_class(result_no_smooth, "plotly")
})

# ============================================================================
# plot_word_distribution() - INTEGRATION
# ============================================================================

test_that("plot_word_distribution complete workflow", {
  skip_if_not_installed("plotly")
  skip_if_not_installed("RColorBrewer")
  skip_if_not_installed("scales")
  skip_if_not_installed("tidytext")

  # Create realistic data
  text_list <- list(
    Introduction = paste(rep("machine learning research", 10), collapse = " "),
    Methods = paste(rep("neural networks deep learning", 10), collapse = " "),
    Results = paste(rep("accuracy performance metrics", 10), collapse = " ")
  )

  dist_data <- calculate_word_distribution(
    text = text_list,
    selected_words = c("machine learning", "neural networks", "accuracy"),
    use_sections = TRUE,
    remove_stopwords = TRUE
  )

  plot <- plot_word_distribution(
    dist_data,
    plot_type = "line",
    show_points = TRUE,
    colors = c("#FF6B6B", "#4ECDC4", "#45B7D1")
  )

  expect_s3_class(plot, "plotly")

  # Verify it's a valid plotly object with data
  expect_true(!is.null(plot$x))
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("calculate_word_distribution handles single segment", {
  skip_if_not_installed("tidytext")

  text <- "short text"

  result <- calculate_word_distribution(
    text = text,
    selected_words = c("text"),
    use_sections = FALSE,
    n_segments = 1
  )

  expect_equal(length(unique(result$segment_id)), 1)
})

test_that("calculate_word_distribution handles many segments", {
  skip_if_not_installed("tidytext")

  text <- paste(rep("word test sample", 200), collapse = " ")

  result <- calculate_word_distribution(
    text = text,
    selected_words = c("word", "test"),
    use_sections = FALSE,
    n_segments = 20
  )

  expect_equal(length(unique(result$segment_id)), 20)
})

test_that("calculate_word_distribution handles very short text", {
  skip_if_not_installed("tidytext")

  text <- "test"

  result <- calculate_word_distribution(
    text = text,
    selected_words = c("test"),
    use_sections = FALSE,
    n_segments = 2
  )

  expect_s3_class(result, "tbl_df")
})
