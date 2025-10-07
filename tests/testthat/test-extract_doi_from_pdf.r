# Test suite for extract_doi_from_pdf function

library(testthat)

# ============================================================================
# INPUT VALIDATION
# ============================================================================

test_that("extract_doi_from_pdf stops when file does not exist", {
  expect_error(
    extract_doi_from_pdf("nonexistent_file.pdf"),
    "PDF file does not exist"
  )
})

test_that("extract_doi_from_pdf handles unreadable PDF", {
  skip_if_not_installed("pdftools")
  
  # Create a temporary non-PDF file
  temp_file <- tempfile(fileext = ".pdf")
  writeLines("This is not a PDF", temp_file)
  on.exit(unlink(temp_file))
  
  expect_error(
    extract_doi_from_pdf(temp_file),
    "Unable to read PDF file"
  )
})

# ============================================================================
# DOI EXTRACTION FROM DIFFERENT FIELDS
# ============================================================================

test_that("extract_doi_from_pdf extracts DOI from doi field", {
  skip_if_not_installed("pdftools")
  
  # Create a minimal PDF for testing
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  # Mock pdf_info to return DOI in doi field
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "10.1234/test.2020.001",
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_equal(result, "10.1234/test.2020.001")
})

test_that("extract_doi_from_pdf extracts DOI from DOI field (uppercase)", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = NULL,
          DOI = "10.5678/example.2021",
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_equal(result, "10.5678/example.2021")
})

test_that("extract_doi_from_pdf extracts DOI from Subject field", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = NULL,
          DOI = NULL,
          Subject = "Research paper with DOI: 10.9999/subject.test",
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_equal(result, "10.9999/subject.test")
})

test_that("extract_doi_from_pdf extracts DOI from Keywords field", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = NULL,
          DOI = NULL,
          Subject = NULL,
          Keywords = "machine learning, AI, 10.1111/keywords.2022",
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_equal(result, "10.1111/keywords.2022")
})

test_that("extract_doi_from_pdf extracts DOI from Title field", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = NULL,
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = "Paper Title 10.2222/title.field",
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_equal(result, "10.2222/title.field")
})

test_that("extract_doi_from_pdf extracts DOI from Creator field", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = NULL,
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = "LaTeX with DOI 10.3333/creator.test",
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_equal(result, "10.3333/creator.test")
})

test_that("extract_doi_from_pdf extracts DOI from Producer field", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = NULL,
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = "pdfTeX with 10.4444/producer.doi"
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_equal(result, "10.4444/producer.doi")
})

# ============================================================================
# PREFIX REMOVAL
# ============================================================================

test_that("extract_doi_from_pdf removes 'doi:' prefix", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "doi:10.1234/test",
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_equal(result, "10.1234/test")
  expect_false(grepl("^doi:", result))
})

test_that("extract_doi_from_pdf removes 'DOI:' prefix (uppercase)", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "DOI:10.5678/example",
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_equal(result, "10.5678/example")
})

test_that("extract_doi_from_pdf removes 'https://doi.org/' prefix", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "https://doi.org/10.1234/https.test",
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_equal(result, "10.1234/https.test")
  expect_false(grepl("^https://", result))
})

test_that("extract_doi_from_pdf removes 'http://doi.org/' prefix", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "http://doi.org/10.9876/http.test",
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_equal(result, "10.9876/http.test")
})

test_that("extract_doi_from_pdf removes 'https://dx.doi.org/' prefix", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "https://dx.doi.org/10.5555/dx.test",
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_equal(result, "10.5555/dx.test")
})

# ============================================================================
# RETURN_ALL PARAMETER
# ============================================================================

test_that("extract_doi_from_pdf returns first DOI by default", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "10.1111/first",
          DOI = "10.2222/second",
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf, return_all = FALSE)
  
  expect_type(result, "character")
  expect_length(result, 1)
  expect_equal(result, "10.1111/first")
})

test_that("extract_doi_from_pdf returns all DOIs when return_all = TRUE", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "10.1111/first",
          DOI = "10.2222/second",
          Subject = "10.3333/third",
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf, return_all = TRUE)
  
  expect_type(result, "character")
  expect_length(result, 3)
  expect_true("10.1111/first" %in% result)
  expect_true("10.2222/second" %in% result)
  expect_true("10.3333/third" %in% result)
})

test_that("extract_doi_from_pdf removes duplicates when return_all = TRUE", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "10.1234/duplicate",
          DOI = "10.1234/duplicate",
          Subject = "10.1234/duplicate",
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf, return_all = TRUE)
  
  expect_type(result, "character")
  expect_length(result, 1)
  expect_equal(result, "10.1234/duplicate")
})

# ============================================================================
# NO DOI FOUND
# ============================================================================

test_that("extract_doi_from_pdf returns NA when no DOI found", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = NULL,
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = "Paper without DOI",
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_true(is.na(result))
  expect_type(result, "character")
})

test_that("extract_doi_from_pdf returns NA when return_all = TRUE and no DOI", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = NULL,
          DOI = NULL,
          Subject = "No DOI here",
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf, return_all = TRUE)
  
  expect_true(is.na(result))
})

test_that("extract_doi_from_pdf handles empty metadata fields", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "",
          DOI = "",
          Subject = "",
          Keywords = "",
          Title = "",
          Creator = "",
          Producer = ""
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_true(is.na(result))
})

# ============================================================================
# DOI FORMAT VALIDATION
# ============================================================================

test_that("extract_doi_from_pdf validates DOI format", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "10.1234/valid-doi_2020.test",
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_match(result, "^10\\.\\d+/")
})

test_that("extract_doi_from_pdf handles DOI with special characters", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "10.1234/test-doi_with.special:chars(2020)",
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_true(!is.na(result))
  expect_match(result, "^10\\.\\d+/")
})

test_that("extract_doi_from_pdf handles very long DOI", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  long_doi <- "10.1234/very.long.doi.with.many.parts.2020.volume.issue.page"
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = long_doi,
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_equal(result, long_doi)
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("extract_doi_from_pdf trims whitespace", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "  10.1234/whitespace  ",
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf)
  
  expect_equal(result, "10.1234/whitespace")
  expect_false(grepl("^\\s|\\s$", result))
})

test_that("extract_doi_from_pdf handles multiple DOIs in same field", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "First: 10.1111/first, Second: 10.2222/second",
          DOI = NULL,
          Subject = NULL,
          Keywords = NULL,
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result_first <- extract_doi_from_pdf(temp_pdf, return_all = FALSE)
  result_all <- extract_doi_from_pdf(temp_pdf, return_all = TRUE)
  
  expect_equal(result_first, "10.1111/first")
  expect_length(result_all, 2)
  expect_true(all(c("10.1111/first", "10.2222/second") %in% result_all))
})

test_that("extract_doi_from_pdf prioritizes doi field over others", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "10.1111/priority",
          DOI = NULL,
          Subject = "10.2222/subject",
          Keywords = "10.3333/keywords",
          Title = NULL,
          Creator = NULL,
          Producer = NULL
        )
      )
    },
    .package = "pdftools"
  )
  
  result <- extract_doi_from_pdf(temp_pdf, return_all = FALSE)
  
  # Should return first found (from doi field)
  expect_equal(result, "10.1111/priority")
})

# ============================================================================
# INTEGRATION TEST
# ============================================================================

test_that("extract_doi_from_pdf complete workflow", {
  skip_if_not_installed("pdftools")
  
  temp_pdf <- tempfile(fileext = ".pdf")
  pdf(temp_pdf, width = 8.5, height = 11)
  plot(1:10)
  dev.off()
  on.exit(unlink(temp_pdf))
  
  local_mocked_bindings(
    pdf_info = function(pdf) {
      list(
        keys = list(
          doi = "doi:10.1234/main.doi",
          DOI = "DOI:10.1234/main.doi",  # Duplicate
          Subject = "Paper about something with https://doi.org/10.5678/related",
          Keywords = "keyword1, keyword2",
          Title = "Important Paper Title",
          Creator = "LaTeX",
          Producer = "pdfTeX"
        )
      )
    },
    .package = "pdftools"
  )
  
  # Test return_all = FALSE
  result_single <- extract_doi_from_pdf(temp_pdf, return_all = FALSE)
  expect_equal(result_single, "10.1234/main.doi")
  
  # Test return_all = TRUE
  result_all <- extract_doi_from_pdf(temp_pdf, return_all = TRUE)
  expect_type(result_all, "character")
  expect_true(length(result_all) >= 2)
  expect_true("10.1234/main.doi" %in% result_all)
  expect_true("10.5678/related" %in% result_all)
  
  # Verify duplicates are removed
  expect_equal(length(result_all), length(unique(result_all)))
})
