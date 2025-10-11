# Test suite for get_crossref_references function

library(testthat)
library(tibble)

# ============================================================================
# INPUT VALIDATION
# ============================================================================

test_that("get_crossref_references stops when DOI is missing", {
  expect_error(
    get_crossref_references(),
    "DOI is required"
  )
})

test_that("get_crossref_references stops when DOI is NULL", {
  expect_error(
    get_crossref_references(doi = NULL),
    "DOI is required"
  )
})

test_that("get_crossref_references stops when DOI is empty string", {
  expect_error(
    get_crossref_references(doi = ""),
    "DOI is required"
  )
})

# ============================================================================
# SUCCESSFUL API CALLS
# ============================================================================

test_that("get_crossref_references constructs correct API URL", {
  skip_if_not_installed("httr2")

  # Mock the httr2 functions
  local_mocked_bindings(
    request = function(url) {
      expect_equal(url, "https://api.crossref.org/works/10.1234/test")
      list(url = url)
    },
    req_user_agent = function(req, agent) req,
    req_perform = function(req) {
      list(
        status_code = 200,
        body = list()
      )
    },
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = list(
            list(
              key = "ref1",
              DOI = "10.5678/cited",
              `article-title` = "Cited Article",
              author = "Smith J",
              year = "2020",
              `journal-title` = "Journal",
              volume = "10",
              `first-page` = "123"
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  result <- get_crossref_references("10.1234/test")

  expect_s3_class(result, "data.frame")
})

test_that("get_crossref_references adds mailto to user agent", {
  skip_if_not_installed("httr2")

  mailto_called <- FALSE

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) {
      expect_equal(agent, "mailto:test@example.com")
      mailto_called <<- TRUE
      req
    },
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = list(
            list(
              key = "ref1",
              author = "Author",
              year = "2020"
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  result <- get_crossref_references("10.1234/test", mailto = "test@example.com")

  expect_true(mailto_called)
})

test_that("get_crossref_references works without mailto", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = list(
            list(key = "ref1", author = "Author", year = "2020")
          )
        )
      )
    },
    .package = "httr2"
  )

  expect_no_error(
    result <- get_crossref_references("10.1234/test", mailto = NULL)
  )
})

test_that("get_crossref_references extracts single reference", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = list(
            list(
              key = "ref1",
              DOI = "10.5678/cited",
              `article-title` = "Test Article",
              author = "Smith J",
              year = "2020",
              `journal-title` = "Test Journal",
              volume = "15",
              `first-page` = "100"
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  result <- get_crossref_references("10.1234/test")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$key[1], "ref1")
  expect_equal(result$doi[1], "10.5678/cited")
  expect_equal(result$article_title[1], "Test Article")
  expect_equal(result$author[1], "Smith J")
  expect_equal(result$year[1], "2020")
  expect_equal(result$journal[1], "Test Journal")
  expect_equal(result$volume[1], "15")
  expect_equal(result$first_page[1], "100")
})

test_that("get_crossref_references extracts multiple references", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = list(
            list(
              key = "ref1",
              DOI = "10.1111/first",
              `article-title` = "First Article",
              author = "Author A",
              year = "2020"
            ),
            list(
              key = "ref2",
              DOI = "10.2222/second",
              `article-title` = "Second Article",
              author = "Author B",
              year = "2021"
            ),
            list(
              key = "ref3",
              DOI = "10.3333/third",
              `article-title` = "Third Article",
              author = "Author C",
              year = "2022"
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  result <- get_crossref_references("10.1234/test")

  expect_equal(nrow(result), 3)
  expect_equal(result$key, c("ref1", "ref2", "ref3"))
  expect_equal(result$year, c("2020", "2021", "2022"))
})

# ============================================================================
# HANDLING MISSING FIELDS
# ============================================================================

test_that("get_crossref_references handles NULL fields as NA", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = list(
            list(
              key = "ref1",
              DOI = NULL,  # Missing DOI
              `article-title` = "Article",
              author = NULL,  # Missing author
              year = "2020",
              `journal-title` = NULL,  # Missing journal
              volume = NULL,
              `first-page` = NULL
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  result <- get_crossref_references("10.1234/test")

  expect_equal(nrow(result), 1)
  expect_true(is.na(result$doi[1]))
  expect_true(is.na(result$author[1]))
  expect_true(is.na(result$journal[1]))
  expect_true(is.na(result$volume[1]))
  expect_true(is.na(result$first_page[1]))
  expect_equal(result$year[1], "2020")
})

test_that("get_crossref_references handles references with partial data", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = list(
            list(
              key = "ref1",
              author = "Complete Author",
              year = "2020",
              `article-title` = "Complete",
              DOI = "10.1111/complete"
            ),
            list(
              key = "ref2",
              author = "Partial Author"
              # Missing year, title, DOI
            ),
            list(
              key = "ref3",
              year = "2022"
              # Missing everything else
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  result <- get_crossref_references("10.1234/test")

  expect_equal(nrow(result), 3)

  # First reference complete
  expect_equal(result$author[1], "Complete Author")
  expect_equal(result$year[1], "2020")

  # Second reference partial
  expect_equal(result$author[2], "Partial Author")
  expect_true(is.na(result$year[2]))

  # Third reference minimal
  expect_true(is.na(result$author[3]))
  expect_equal(result$year[3], "2022")
})

# ============================================================================
# NO REFERENCES FOUND
# ============================================================================

test_that("get_crossref_references returns NULL when no references found", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = NULL  # No references
        )
      )
    },
    .package = "httr2"
  )

  # When references are NULL, .parse_references returns NULL
  # The function completes successfully and returns NULL without error/message
  result <- get_crossref_references("10.1234/test")

  expect_null(result)
})

test_that("get_crossref_references returns NULL when reference list is empty", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = list()  # Empty list
        )
      )
    },
    .package = "httr2"
  )

  # Empty list results in NULL from .parse_references
  # The function completes successfully and returns NULL without error/message
  result <- get_crossref_references("10.1234/test")

  expect_null(result)
})

# ============================================================================
# ERROR HANDLING
# ============================================================================

test_that("get_crossref_references handles API errors", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) {
      stop("HTTP 404: Not Found")
    },
    .package = "httr2"
  )

  # Expect message about error and NULL return
  expect_message(
    result <- get_crossref_references("10.1234/nonexistent"),
    "Error retrieving or parsing data"
  )

  expect_null(result)
})

test_that("get_crossref_references handles network errors", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) {
      stop("Network timeout")
    },
    .package = "httr2"
  )

  # Expect message about error and NULL return
  expect_message(
    result <- get_crossref_references("10.1234/test"),
    "Error retrieving or parsing data"
  )

  expect_null(result)
})

test_that("get_crossref_references handles malformed JSON response", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      stop("Invalid JSON")
    },
    .package = "httr2"
  )

  # Expect message about error and NULL return
  expect_message(
    result <- get_crossref_references("10.1234/test"),
    "Error retrieving or parsing data"
  )

  expect_null(result)
})

# ============================================================================
# OUTPUT STRUCTURE
# ============================================================================

test_that("get_crossref_references returns data.frame with correct columns", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = list(
            list(
              key = "ref1",
              DOI = "10.1234/test",
              `article-title` = "Article",
              author = "Author",
              year = "2020",
              `journal-title` = "Journal",
              volume = "10",
              `first-page` = "1"
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  result <- get_crossref_references("10.1234/test")

  expected_cols <- c("key", "doi", "article_title", "author", "year",
                     "journal", "volume", "first_page")

  expect_true(all(expected_cols %in% names(result)))
})

test_that("get_crossref_references has correct column types", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = list(
            list(
              key = "ref1",
              DOI = "10.1234/test",
              author = "Author",
              year = "2020"
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  result <- get_crossref_references("10.1234/test")

  expect_type(result$key, "character")
  expect_type(result$doi, "character")
  expect_type(result$author, "character")
  expect_type(result$year, "character")
})

# ============================================================================
# REALISTIC EXAMPLES
# ============================================================================

test_that("get_crossref_references handles typical academic references", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = list(
            list(
              key = "bib1",
              DOI = "10.1038/nature12345",
              `article-title` = "A groundbreaking study in Nature",
              author = "Smith JA",
              year = "2020",
              `journal-title` = "Nature",
              volume = "580",
              `first-page` = "123"
            ),
            list(
              key = "bib2",
              DOI = "10.1126/science.abc1234",
              `article-title` = "Important findings in Science",
              author = "Jones BR",
              year = "2021",
              `journal-title` = "Science",
              volume = "371",
              `first-page` = "456"
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  result <- get_crossref_references("10.1016/example.2022")

  expect_equal(nrow(result), 2)
  expect_true(all(!is.na(result$doi)))
  expect_true(all(!is.na(result$article_title)))
  expect_true(all(!is.na(result$year)))
})

test_that("get_crossref_references handles book chapters", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = list(
            list(
              key = "book1",
              DOI = "10.1007/978-3-642-12345-6_7",
              `article-title` = "Chapter Title",
              author = "Author A",
              year = "2019",
              volume = NULL,
              `first-page` = "45"
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  result <- get_crossref_references("10.1234/test")

  expect_equal(nrow(result), 1)
  expect_equal(result$article_title[1], "Chapter Title")
  expect_true(is.na(result$volume[1]))
})

# ============================================================================
# INTEGRATION TEST
# ============================================================================

test_that("get_crossref_references complete workflow", {
  skip_if_not_installed("httr2")

  url_captured <- NULL
  agent_captured <- NULL

  local_mocked_bindings(
    request = function(url) {
      url_captured <<- url
      list(url = url)
    },
    req_user_agent = function(req, agent) {
      agent_captured <<- agent
      req
    },
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = list(
            list(
              key = "ref1",
              DOI = "10.1111/first",
              `article-title` = "Complete Article",
              author = "Smith J",
              year = "2020",
              `journal-title` = "Journal of Testing",
              volume = "15",
              `first-page` = "100"
            ),
            list(
              key = "ref2",
              author = "Jones A",
              year = "2021"
              # Partial data
            ),
            list(
              key = "ref3",
              DOI = "10.3333/third",
              `article-title` = "Another Article",
              author = "Brown K",
              year = "2022"
            )
          )
        )
      )
    },
    .package = "httr2"
  )

  result <- get_crossref_references(
    "10.1016/j.example.2023.01.001",
    mailto = "researcher@university.edu"
  )

  # Verify URL construction
  expect_equal(url_captured, "https://api.crossref.org/works/10.1016/j.example.2023.01.001")

  # Verify mailto was used
  expect_equal(agent_captured, "mailto:researcher@university.edu")

  # Verify results
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)

  # Verify complete reference
  expect_equal(result$key[1], "ref1")
  expect_equal(result$doi[1], "10.1111/first")
  expect_equal(result$author[1], "Smith J")

  # Verify partial reference
  expect_equal(result$author[2], "Jones A")
  expect_true(is.na(result$doi[2]))

  # Verify all have keys
  expect_true(all(!is.na(result$key)))
})

# ============================================================================
# EDGE CASES
# ============================================================================

test_that("get_crossref_references handles DOI with special characters", {
  skip_if_not_installed("httr2")

  local_mocked_bindings(
    request = function(url) {
      # httr2 handles URL encoding internally, just verify the function is called
      expect_true(grepl("10.1234/test", url))
      list(url = url)
    },
    req_user_agent = function(req, agent) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(
        message = list(
          reference = list(
            list(key = "ref1", author = "Author", year = "2020")
          )
        )
      )
    },
    .package = "httr2"
  )

  # The function should handle DOI with special chars - httr2 handles encoding
  result <- get_crossref_references("10.1234/test(special)")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("get_crossref_references handles very long reference list", {
  skip_if_not_installed("httr2")

  # Create 100 references
  refs <- lapply(1:100, function(i) {
    list(
      key = paste0("ref", i),
      DOI = paste0("10.1111/ref", i),
      author = paste0("Author", i),
      year = as.character(2000 + i %% 23)
    )
  })

  local_mocked_bindings(
    request = function(url) list(url = url),
    req_user_agent = function(req, agent) req,
    req_perform = function(req) list(status_code = 200),
    resp_body_json = function(resp) {
      list(message = list(reference = refs))
    },
    .package = "httr2"
  )

  result <- get_crossref_references("10.1234/test")

  expect_equal(nrow(result), 100)
  expect_equal(length(unique(result$key)), 100)
})
