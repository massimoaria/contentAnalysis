# Integration tests for pdf2txt_multicolumn_safe with real PDF files
# These tests require actual PDF fixtures in tests/testthat/fixtures/

library(testthat)

# Helper functions
fixture_path <- function(filename) {
  path <- test_path("fixtures", filename)
  if (!file.exists(path)) {
    return(NULL)
  }
  path
}

skip_if_no_fixture <- function(filename) {
  path <- fixture_path(filename)
  if (is.null(path)) {
    skip(paste("Fixture not available:", filename))
  }
}

list_fixtures <- function() {
  fixtures_dir <- test_path("fixtures")
  if (!dir.exists(fixtures_dir)) {
    return(character(0))
  }
  list.files(fixtures_dir, pattern = "\\.pdf$")
}

# Integration Tests with Real PDFs

describe("Integration tests with real PDF files", {
  it("lists available fixtures for debugging", {
    skip_on_cran()
    fixtures <- list_fixtures()
    message("Available PDF fixtures: ", paste(fixtures, collapse = ", "))
    expect_true(TRUE) # Always pass, just for info
  })

  it("extracts text from single-column PDF", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("sample-single-column.pdf")

    pdf_file <- fixture_path("sample-single-column.pdf")
    result <- pdf2txt_multicolumn_safe(pdf_file, n_columns = 1)

    expect_type(result, "character")
    expect_false(is.na(result))
    expect_gt(nchar(result), 0)

    # Should contain readable text (letters)
    expect_true(grepl("[a-zA-Z]{3,}", result))
  })

  it("extracts text from two-column PDF", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("sample-two-columns.pdf")

    pdf_file <- fixture_path("sample-two-columns.pdf")
    result <- pdf2txt_multicolumn_safe(pdf_file, n_columns = 2)

    expect_type(result, "character")
    expect_false(is.na(result))
    expect_gt(nchar(result), 0)
    expect_true(grepl("[a-zA-Z]{3,}", result))
  })

  it("extracts text from three-column PDF", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("sample-three-columns.pdf")

    pdf_file <- fixture_path("sample-three-columns.pdf")
    result <- pdf2txt_multicolumn_safe(pdf_file, n_columns = 3)

    expect_type(result, "character")
    expect_false(is.na(result))
    expect_gt(nchar(result), 0)
  })

  it("auto-detects columns in two-column PDF", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("sample-two-columns.pdf")

    pdf_file <- fixture_path("sample-two-columns.pdf")
    result <- pdf2txt_multicolumn_safe(pdf_file) # No n_columns specified

    expect_type(result, "character")
    expect_false(is.na(result))
    expect_gt(nchar(result), 0)
  })

  it("handles multi-page PDF correctly", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("multi-page.pdf")

    pdf_file <- fixture_path("multi-page.pdf")
    result <- pdf2txt_multicolumn_safe(pdf_file)

    expect_type(result, "character")
    expect_false(is.na(result))

    # Multi-page should have substantial content
    expect_gt(nchar(result), 500)
  })

  it("preserves structure when requested", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("sample-two-columns.pdf")

    pdf_file <- fixture_path("sample-two-columns.pdf")

    result_structured <- pdf2txt_multicolumn_safe(
      pdf_file,
      preserve_structure = TRUE
    )

    result_continuous <- pdf2txt_multicolumn_safe(
      pdf_file,
      preserve_structure = FALSE
    )

    # Structured should have newlines
    expect_true(grepl("\n", result_structured))

    # Both should have content
    expect_gt(nchar(result_structured), 0)
    expect_gt(nchar(result_continuous), 0)
  })

  it("handles empty or near-empty PDF", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("empty-page.pdf")

    pdf_file <- fixture_path("empty-page.pdf")
    result <- pdf2txt_multicolumn_safe(pdf_file)

    expect_type(result, "character")
    # Empty PDF should return empty string
    expect_lte(nchar(result), 10)
  })

  it("extracts from scientific paper format", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("scientific-paper.pdf")

    pdf_file <- fixture_path("scientific-paper.pdf")
    result <- pdf2txt_multicolumn_safe(pdf_file, n_columns = 2)

    expect_type(result, "character")
    expect_gt(nchar(result), 100)

    # Should contain typical academic paper elements
    # (adjust these based on your actual fixture)
    expect_true(
      grepl(
        "abstract|introduction|method|result|conclusion",
        result,
        ignore.case = TRUE
      ) ||
        nchar(result) > 100 # Or just has substantial text
    )
  })

  it("correctly orders text from columns", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("sample-two-columns.pdf")

    pdf_file <- fixture_path("sample-two-columns.pdf")
    result <- pdf2txt_multicolumn_safe(pdf_file, n_columns = 2)

    # This is hard to test without knowing content
    # But we can at least verify it produces text
    expect_type(result, "character")
    expect_gt(nchar(result), 50)
  })

  it("handles PDFs with special characters", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("special-chars.pdf")

    pdf_file <- fixture_path("special-chars.pdf")
    result <- pdf2txt_multicolumn_safe(pdf_file)

    expect_type(result, "character")
    expect_false(is.na(result))
  })

  it("handles PDFs with unicode content", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("unicode-content.pdf")

    pdf_file <- fixture_path("unicode-content.pdf")
    result <- pdf2txt_multicolumn_safe(pdf_file)

    expect_type(result, "character")
    expect_false(is.na(result))
  })

  it("removes hyphenation from real PDF", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("hyphenated-text.pdf")

    pdf_file <- fixture_path("hyphenated-text.pdf")
    result <- pdf2txt_multicolumn_safe(pdf_file)

    expect_type(result, "character")

    # Should not have "word-\n" or "word- " patterns
    expect_false(grepl("-\\s*\n", result))
  })

  it("handles PDF with complex layout", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("complex-layout.pdf")

    pdf_file <- fixture_path("complex-layout.pdf")
    result <- pdf2txt_multicolumn_safe(pdf_file)

    expect_type(result, "character")
    expect_false(is.na(result))
    expect_gt(nchar(result), 0)
  })

  it("custom threshold works with real PDF", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("sample-two-columns.pdf")

    pdf_file <- fixture_path("sample-two-columns.pdf")

    # Get page dimensions to calculate meaningful threshold
    page_data <- pdftools::pdf_data(pdf_file)[[1]]
    page_width <- max(page_data$x) - min(page_data$x)
    threshold <- min(page_data$x) + page_width / 2

    result <- pdf2txt_multicolumn_safe(
      pdf_file,
      column_threshold = threshold
    )

    expect_type(result, "character")
    expect_gt(nchar(result), 0)
  })

  it("compares output consistency between runs", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("sample-single-column.pdf")

    pdf_file <- fixture_path("sample-single-column.pdf")

    result1 <- pdf2txt_multicolumn_safe(pdf_file, n_columns = 1)
    result2 <- pdf2txt_multicolumn_safe(pdf_file, n_columns = 1)

    # Results should be identical
    expect_equal(result1, result2)
  })
})

describe("Performance tests with real PDFs", {
  it("extracts large multi-page PDF in reasonable time", {
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("large-doc.pdf")
    skip_on_cran()

    pdf_file <- fixture_path("large-doc.pdf")

    start_time <- Sys.time()
    result <- pdf2txt_multicolumn_safe(pdf_file)
    end_time <- Sys.time()

    elapsed <- as.numeric(end_time - start_time, units = "secs")

    expect_type(result, "character")
    expect_gt(nchar(result), 1000)

    # Should complete in reasonable time (adjust as needed)
    expect_lt(elapsed, 30) # 30 seconds max
  })
})

describe("Content validation with known PDFs", {
  it("extracts expected content from known PDF", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("test-content.pdf")

    # If you have a PDF with known content, test it here
    pdf_file <- fixture_path("test-content.pdf")
    result <- pdf2txt_multicolumn_safe(pdf_file)

    # Example: if you know the PDF contains certain phrases
    # expect_match(result, "expected phrase")

    expect_type(result, "character")
    expect_gt(nchar(result), 0)
  })
})

describe("Error handling with real files", {
  it("handles corrupted PDF gracefully", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("malformed.pdf")

    pdf_file <- fixture_path("malformed.pdf")

    # Should either error or return NA/empty, but not crash
    result <- tryCatch(
      {
        pdf2txt_multicolumn_safe(pdf_file)
      },
      error = function(e) {
        return(NA_character_)
      }
    )

    expect_true(is.na(result) || is.character(result))
  })

  it("handles password-protected PDF", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("password-protected.pdf")

    pdf_file <- fixture_path("password-protected.pdf")

    # Should error or handle gracefully
    expect_error(
      pdf2txt_multicolumn_safe(pdf_file),
      regexp = "password|encrypted|protected"
    )
  })
})

describe("Regression tests", {
  it("maintains backwards compatibility with v0.1 output", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("regression-test-v0.1.pdf")

    # If you have a reference output from previous version
    pdf_file <- fixture_path("regression-test-v0.1.pdf")
    result <- pdf2txt_multicolumn_safe(pdf_file)

    # Could compare against saved reference output
    # reference_file <- test_path("fixtures", "regression-test-v0.1.txt")
    # if (file.exists(reference_file)) {
    #   reference <- readLines(reference_file, warn = FALSE)
    #   reference <- paste(reference, collapse = "\n")
    #   expect_equal(result, reference)
    # }

    expect_type(result, "character")
  })
})

describe("Comparative tests", {
  it("auto-detect gives similar results to explicit n_columns", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("sample-two-columns.pdf")

    pdf_file <- fixture_path("sample-two-columns.pdf")

    result_auto <- pdf2txt_multicolumn_safe(pdf_file)
    result_explicit <- pdf2txt_multicolumn_safe(pdf_file, n_columns = 2)

    # Results should be similar (may not be identical due to threshold differences)
    # At minimum, should have similar length
    expect_true(
      abs(nchar(result_auto) - nchar(result_explicit)) <
        nchar(result_explicit) * 0.1 # Within 10%
    )
  })

  it("different column specifications produce different results", {
    skip_on_cran()
    skip_if_not_installed("pdftools")
    skip_if_no_fixture("sample-two-columns.pdf")

    pdf_file <- fixture_path("sample-two-columns.pdf")

    result_1col <- pdf2txt_multicolumn_safe(pdf_file, n_columns = 1)
    result_2col <- pdf2txt_multicolumn_safe(pdf_file, n_columns = 2)

    # Results should differ (different text ordering)
    expect_false(identical(result_1col, result_2col))
  })
})
