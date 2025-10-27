# Test suite for reconstruct_text_structured function
# This is an internal function, accessed via :::

library(testthat)

# Helper to create mock column data
create_column_data <- function(
  texts,
  y_coords,
  x_coords = NULL,
  heights = NULL,
  widths = NULL
) {
  n <- length(texts)

  if (is.null(x_coords)) {
    x_coords <- rep(100, n)
  }
  if (is.null(heights)) {
    heights <- rep(12, n)
  }
  if (is.null(widths)) {
    widths <- rep(50, n)
  }

  data.frame(
    x = x_coords,
    y = y_coords,
    width = widths,
    height = heights,
    text = texts,
    stringsAsFactors = FALSE
  )
}

# Test 1: Empty data frame
test_that("reconstruct_text_structured handles empty data frame", {
  skip_on_cran()
  empty_data <- data.frame(
    x = numeric(0),
    y = numeric(0),
    width = numeric(0),
    height = numeric(0),
    text = character(0),
    stringsAsFactors = FALSE
  )

  result <- contentanalysis:::reconstruct_text_structured(empty_data, TRUE)

  expect_equal(result, "")
  expect_type(result, "character")
})

# Test 2: Single line of text
test_that("reconstruct_text_structured handles single line", {
  skip_on_cran()
  single_line <- create_column_data(
    texts = c("Hello", "world", "test"),
    y_coords = c(100, 100, 100),
    x_coords = c(50, 100, 150)
  )

  result <- contentanalysis:::reconstruct_text_structured(single_line, TRUE)

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  expect_match(result, "Hello world test")
})

# Test 3: Multiple lines - preserve structure
test_that("reconstruct_text_structured preserves line structure", {
  skip_on_cran()
  multi_line <- create_column_data(
    texts = c("First", "line", "Second", "line", "Third", "line"),
    y_coords = c(100, 100, 120, 120, 140, 140),
    x_coords = c(50, 100, 50, 100, 50, 100)
  )

  result_structured <- contentanalysis:::reconstruct_text_structured(
    multi_line,
    preserve_structure = TRUE
  )

  expect_type(result_structured, "character")
  expect_true(nchar(result_structured) > 0)
  # Should contain the text
  expect_match(result_structured, "First line")
  expect_match(result_structured, "Second line")
})

# Test 4: Multiple lines - no structure preservation
test_that("reconstruct_text_structured creates continuous text", {
  skip_on_cran()
  multi_line <- create_column_data(
    texts = c("First", "line", "Second", "line"),
    y_coords = c(100, 100, 120, 120),
    x_coords = c(50, 100, 50, 100)
  )

  result_continuous <- contentanalysis:::reconstruct_text_structured(
    multi_line,
    preserve_structure = FALSE
  )

  expect_type(result_continuous, "character")
  expect_true(nchar(result_continuous) > 0)
  # Should be continuous (spaces but fewer newlines)
  expect_match(result_continuous, "First line Second line")
})

# Test 5: Title detection - all caps
test_that("reconstruct_text_structured identifies CAPS titles", {
  skip_on_cran()
  data_with_title <- create_column_data(
    texts = c("INTRODUCTION", "This is regular text.", "More text here."),
    y_coords = c(100, 130, 145)
  )

  result <- contentanalysis:::reconstruct_text_structured(
    data_with_title,
    preserve_structure = TRUE
  )

  expect_type(result, "character")
  expect_match(result, "INTRODUCTION")
  expect_match(result, "This is regular text")
  # Verify content is present
  expect_true(nchar(result) > 20)
})

# Test 6: Title detection - numbered section
test_that("reconstruct_text_structured identifies numbered sections", {
  skip_on_cran()
  data_with_section <- create_column_data(
    texts = c("1. Methods", "This describes the methods.", "More details."),
    y_coords = c(100, 130, 145)
  )

  result <- contentanalysis:::reconstruct_text_structured(
    data_with_section,
    preserve_structure = TRUE
  )

  expect_type(result, "character")
  expect_match(result, "1\\. Methods")
  expect_match(result, "describes the methods")
  expect_true(nchar(result) > 20)
})

# Test 7: Title detection - subsection numbering
test_that("reconstruct_text_structured identifies subsection numbers", {
  skip_on_cran()
  data_with_subsection <- create_column_data(
    texts = c(
      "2.1 Data Collection",
      "We collected data from...",
      "Analysis followed."
    ),
    y_coords = c(100, 130, 145)
  )

  result <- contentanalysis:::reconstruct_text_structured(
    data_with_subsection,
    preserve_structure = TRUE
  )

  expect_type(result, "character")
  expect_match(result, "2\\.1 Data Collection")
})

# Test 8: Reference detection
test_that("reconstruct_text_structured identifies reference starts", {
  skip_on_cran()
  data_with_reference <- create_column_data(
    texts = c(
      "Regular text here.",
      "Smith, J. Research in AI.",
      "More references."
    ),
    y_coords = c(100, 130, 145)
  )

  result <- contentanalysis:::reconstruct_text_structured(
    data_with_reference,
    preserve_structure = TRUE
  )

  expect_type(result, "character")
  expect_match(result, "Smith, J\\.")
  expect_match(result, "Regular text")
  # Verify all content is present
  expect_true(nchar(result) > 30)
})

# Test 9: Sentence end detection for paragraph breaks
test_that("reconstruct_text_structured creates paragraphs at sentence ends", {
  skip_on_cran()
  data_with_sentences <- create_column_data(
    texts = c(
      "First sentence ends here.",
      "New sentence starts.",
      "Continues text."
    ),
    y_coords = c(100, 130, 145)
  )

  result <- contentanalysis:::reconstruct_text_structured(
    data_with_sentences,
    preserve_structure = TRUE
  )

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

# Test 10: No height column (edge case)
test_that("reconstruct_text_structured handles missing height column", {
  skip_on_cran()
  data_no_height <- data.frame(
    x = c(100, 100, 100),
    y = c(100, 120, 140),
    width = c(50, 50, 50),
    text = c("First", "Second", "Third"),
    stringsAsFactors = FALSE
  )

  result <- contentanalysis:::reconstruct_text_structured(
    data_no_height,
    preserve_structure = TRUE
  )

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  expect_match(result, "First")
})

# Test 11: Line tolerance grouping
test_that("reconstruct_text_structured groups lines with similar y coords", {
  skip_on_cran()
  # Y coordinates within tolerance (4 pixels) should be same line
  close_y <- create_column_data(
    texts = c("Word1", "Word2", "Word3"),
    y_coords = c(100, 101, 102), # Within 4 pixel tolerance
    x_coords = c(50, 100, 150)
  )

  result <- contentanalysis:::reconstruct_text_structured(close_y, TRUE)

  expect_type(result, "character")
  # Should be on same line
  expect_match(result, "Word1 Word2 Word3")
})

# Test 12: Line separation with different y coords
test_that("reconstruct_text_structured separates lines with different y coords", {
  skip_on_cran()
  far_y <- create_column_data(
    texts = c("Line1", "Line2", "Line3"),
    y_coords = c(100, 120, 140), # More than 4 pixel difference
    x_coords = c(50, 50, 50)
  )

  result <- contentanalysis:::reconstruct_text_structured(far_y, TRUE)

  expect_type(result, "character")
  expect_match(result, "Line1")
  expect_match(result, "Line2")
  expect_match(result, "Line3")
})

# Test 13: X coordinate sorting within lines
test_that("reconstruct_text_structured sorts words by x coordinate", {
  skip_on_cran()
  unsorted_x <- create_column_data(
    texts = c("Third", "First", "Second"),
    y_coords = c(100, 100, 100),
    x_coords = c(300, 100, 200)
  )

  result <- contentanalysis:::reconstruct_text_structured(unsorted_x, TRUE)

  expect_type(result, "character")
  # Should be sorted by x: First, Second, Third
  expect_match(result, "First Second Third")
})

# Test 14: Font size variation (title detection)
test_that("reconstruct_text_structured uses font size for title detection", {
  skip_on_cran()
  varying_fonts <- create_column_data(
    texts = c("BIG TITLE", "Normal text here", "More normal"),
    y_coords = c(100, 130, 145),
    heights = c(18, 12, 12) # Larger font for title
  )

  result <- contentanalysis:::reconstruct_text_structured(
    varying_fonts,
    preserve_structure = TRUE
  )

  expect_type(result, "character")
  expect_match(result, "BIG TITLE")
})

# Test 15: Very long line (not a title)
test_that("reconstruct_text_structured doesn't treat long lines as titles", {
  skip_on_cran()
  long_line <- create_column_data(
    texts = paste(rep("word", 30), collapse = " "),
    y_coords = 100
  )

  result <- contentanalysis:::reconstruct_text_structured(long_line, TRUE)

  expect_type(result, "character")
  expect_true(nchar(result) > 80)
})

# Test 16: Short non-caps line (not necessarily a title)
test_that("reconstruct_text_structured handles short mixed-case lines", {
  skip_on_cran()
  short_mixed <- create_column_data(
    texts = c("Short line", "Another text", "More text"),
    y_coords = c(100, 120, 140)
  )

  result <- contentanalysis:::reconstruct_text_structured(short_mixed, TRUE)

  expect_type(result, "character")
  expect_match(result, "Short line")
})

# Test 17: Multiple spaces handling
test_that("reconstruct_text_structured normalizes whitespace", {
  skip_on_cran()
  multi_space <- create_column_data(
    texts = c("Word1", "  ", "Word2", "   ", "Word3"),
    y_coords = c(100, 100, 100, 100, 100),
    x_coords = c(50, 100, 150, 200, 250)
  )

  result <- contentanalysis:::reconstruct_text_structured(multi_space, TRUE)

  expect_type(result, "character")
  # Should not have excessive spaces
  expect_false(grepl("  ", result))
})

# Test 18: Empty text elements
test_that("reconstruct_text_structured filters empty text", {
  skip_on_cran()
  with_empty <- create_column_data(
    texts = c("Word1", "", "Word2", "  ", "Word3"),
    y_coords = c(100, 100, 100, 100, 100),
    x_coords = c(50, 100, 150, 200, 250)
  )

  result <- contentanalysis:::reconstruct_text_structured(with_empty, TRUE)

  expect_type(result, "character")
  expect_match(result, "Word1")
  expect_match(result, "Word2")
  expect_match(result, "Word3")
})

# Test 19: Complex reference patterns
test_that("reconstruct_text_structured handles hyphenated author names", {
  skip_on_cran()
  hyphenated_ref <- create_column_data(
    texts = c("Regular text.", "Smith-Jones, A. Paper title.", "More text."),
    y_coords = c(100, 130, 160)
  )

  result <- contentanalysis:::reconstruct_text_structured(
    hyphenated_ref,
    preserve_structure = TRUE
  )

  expect_type(result, "character")
  expect_match(result, "Smith-Jones, A\\.")
})

# Test 20: Paragraph detection with proper capitalization
test_that("reconstruct_text_structured detects paragraph breaks", {
  skip_on_cran()
  paragraphs <- create_column_data(
    texts = c(
      "This is the end of first paragraph.",
      "This starts a new paragraph with capital.",
      "This continues the paragraph."
    ),
    y_coords = c(100, 120, 135)
  )

  result <- contentanalysis:::reconstruct_text_structured(
    paragraphs,
    preserve_structure = TRUE
  )

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  # Should have some structure
  expect_true(grepl("paragraph", result))
})

# Test 21: Preserving title after title
test_that("reconstruct_text_structured handles consecutive titles", {
  skip_on_cran()
  consecutive_titles <- create_column_data(
    texts = c("MAIN TITLE", "SUBTITLE", "Regular text follows"),
    y_coords = c(100, 115, 145)
  )

  result <- contentanalysis:::reconstruct_text_structured(
    consecutive_titles,
    preserve_structure = TRUE
  )

  expect_type(result, "character")
  expect_match(result, "MAIN TITLE")
  expect_match(result, "SUBTITLE")
})

# Test 22: Mixed content with all features
test_that("reconstruct_text_structured handles complex mixed content", {
  skip_on_cran()
  complex_data <- create_column_data(
    texts = c(
      "INTRODUCTION",
      "1. Background",
      "This is regular text that continues for a while.",
      "More text here.",
      "Smith, J. Reference item here.",
      "2. Methods",
      "Description of methods used in study."
    ),
    y_coords = c(100, 130, 160, 175, 210, 240, 270),
    heights = c(16, 14, 12, 12, 12, 14, 12)
  )

  result <- contentanalysis:::reconstruct_text_structured(
    complex_data,
    preserve_structure = TRUE
  )

  expect_type(result, "character")
  expect_match(result, "INTRODUCTION")
  expect_match(result, "1\\. Background")
  expect_match(result, "Smith, J\\.")
  expect_match(result, "2\\. Methods")
  # Verify comprehensive content
  expect_true(nchar(result) > 100)
  # All major pieces should be present
  expect_match(result, "regular text")
  expect_match(result, "methods used")
})

# Test 23: Y-coordinate ordering
test_that("reconstruct_text_structured orders lines by y coordinate", {
  skip_on_cran()
  unordered_y <- create_column_data(
    texts = c("Third", "First", "Second"),
    y_coords = c(300, 100, 200)
  )

  result <- contentanalysis:::reconstruct_text_structured(unordered_y, TRUE)

  expect_type(result, "character")
  # Should be ordered: First, Second, Third
  expect_true(grepl("First.*Second.*Third", result))
})

# Test 24: Continuous text mode with long content
test_that("continuous mode creates single-line output", {
  skip_on_cran()
  long_content <- create_column_data(
    texts = rep("word", 20),
    y_coords = rep(c(100, 120, 140, 160), each = 5)
  )

  result <- contentanalysis:::reconstruct_text_structured(
    long_content,
    preserve_structure = FALSE
  )

  expect_type(result, "character")
  expect_true(nchar(result) > 50)
})

# Test 25: Edge case - all caps but long (not a title)
test_that("long all-caps text is not treated as title", {
  skip_on_cran()
  long_caps <- create_column_data(
    texts = paste(rep("WORD", 50), collapse = " "),
    y_coords = 100
  )

  result <- contentanalysis:::reconstruct_text_structured(long_caps, TRUE)

  expect_type(result, "character")
  # Should still process it
  expect_true(nchar(result) > 100)
})
