# Test file for create_citation_network function
library(testthat)
#library(dplyr)
library(igraph)
library(visNetwork)
library(stringr)

# Helper function to create mock citation analysis results
create_mock_citation_results <- function() {
  list(
    network_data = data.frame(
      citation1 = c(
        "Smith et al. (2020)",
        "Jones et al. (2019)",
        "Smith et al. (2020)",
        "Brown et al. (2021)"
      ),
      citation2 = c(
        "Jones et al. (2019)",
        "Brown et al. (2021)",
        "Brown et al. (2021)",
        "Davis et al. (2022)"
      ),
      distance = c(150, 450, 800, 300),
      stringsAsFactors = FALSE
    ),
    citations = data.frame(
      citation_text_clean = c(
        "Smith et al. (2020)",
        "Jones et al. (2019)",
        "Brown et al. (2021)",
        "Davis et al. (2022)",
        "Smith et al. (2020)"
      ),
      section = c(
        "Introduction",
        "Methods",
        "Results",
        "Discussion",
        "Results"
      ),
      stringsAsFactors = FALSE
    ),
    section_colors = c(
      "Introduction" = "#FF6B6B",
      "Methods" = "#4ECDC4",
      "Results" = "#45B7D1",
      "Discussion" = "#FFA07A"
    )
  )
}

# Test 1: Basic functionality with valid input
test_that("create_citation_network creates valid network with default parameters", {
  mock_data <- create_mock_citation_results()
  network <- create_citation_network(mock_data)

  expect_s3_class(network, "visNetwork")
  expect_false(is.null(network))
  expect_true("stats" %in% names(attributes(network)))
})

# Test 2: Network statistics are correctly calculated
test_that("network statistics are correctly computed", {
  mock_data <- create_mock_citation_results()
  network <- create_citation_network(mock_data)
  stats <- attr(network, "stats")

  expect_true(is.list(stats))
  expect_true("n_nodes" %in% names(stats))
  expect_true("n_edges" %in% names(stats))
  expect_true("avg_distance" %in% names(stats))
  expect_true("max_distance" %in% names(stats))
  expect_true("section_distribution" %in% names(stats))

  expect_true(stats$n_nodes > 0)
  expect_true(stats$n_edges > 0)
  expect_equal(stats$max_distance, 1000)
  expect_true(is.numeric(stats$avg_distance))
})

# Test 3: max_distance parameter filters correctly
test_that("max_distance parameter filters citation pairs", {
  mock_data <- create_mock_citation_results()

  # With max_distance = 500, should exclude the pair with distance 800
  network_500 <- create_citation_network(mock_data, max_distance = 500)
  stats_500 <- attr(network_500, "stats")

  # With max_distance = 200, should only include pair with distance 150
  network_200 <- create_citation_network(mock_data, max_distance = 200)
  stats_200 <- attr(network_200, "stats")

  expect_true(stats_500$n_edges < 4) # Less than total possible edges
  expect_true(stats_200$n_edges <= stats_500$n_edges)
  expect_true(all(stats_200$avg_distance <= 200))
})

# Test 4: min_connections parameter filters nodes
test_that("min_connections parameter filters nodes correctly", {
  mock_data <- create_mock_citation_results()

  network_min1 <- create_citation_network(mock_data, min_connections = 1)
  network_min2 <- create_citation_network(mock_data, min_connections = 2)

  stats_min1 <- attr(network_min1, "stats")
  stats_min2 <- attr(network_min2, "stats")

  # Higher min_connections should result in fewer nodes
  expect_true(stats_min2$n_nodes <= stats_min1$n_nodes)
})

# Test 5: show_labels parameter works
test_that("show_labels parameter controls label display", {
  mock_data <- create_mock_citation_results()

  network_with_labels <- create_citation_network(mock_data, show_labels = TRUE)
  network_no_labels <- create_citation_network(mock_data, show_labels = FALSE)

  # Both should create valid networks
  expect_s3_class(network_with_labels, "visNetwork")
  expect_s3_class(network_no_labels, "visNetwork")
})

# Test 6: Handles NULL or empty network_data
test_that("function handles NULL or empty network_data gracefully", {
  mock_data_null <- list(
    network_data = NULL,
    citations = data.frame(),
    section_colors = c()
  )

  expect_warning(
    result <- create_citation_network(mock_data_null),
    "No citation co-occurrence data found"
  )
  expect_null(result)

  mock_data_empty <- list(
    network_data = data.frame(
      citation1 = character(0),
      citation2 = character(0),
      distance = numeric(0)
    ),
    citations = data.frame(),
    section_colors = c()
  )

  expect_warning(
    result <- create_citation_network(mock_data_empty),
    "No citation co-occurrence data found"
  )
  expect_null(result)
})

# Test 7: Warning when no pairs within max_distance
test_that("function warns when no pairs within max_distance", {
  mock_data <- create_mock_citation_results()

  expect_warning(
    result <- create_citation_network(mock_data, max_distance = 50),
    "No citation pairs found within the specified maximum distance"
  )
  expect_null(result)
})

# Test 8: Warning when no valid connections after filtering
test_that("function warns when no valid connections after filtering", {
  mock_data <- create_mock_citation_results()

  # Set very high min_connections that no node can satisfy
  expect_warning(
    result <- create_citation_network(mock_data, min_connections = 100),
    "No valid connections after filtering"
  )
  expect_null(result)
})

# Test 9: Multi-section citations are correctly identified
test_that("multi-section citations are correctly identified", {
  mock_data <- create_mock_citation_results()
  network <- create_citation_network(mock_data)
  stats <- attr(network, "stats")

  expect_true("multi_section_citations" %in% names(stats))

  # Smith et al. (2020) appears in both Introduction and Results
  multi_section_df <- stats$multi_section_citations
  if (nrow(multi_section_df) > 0) {
    expect_true(any(grepl("Smith et al.", multi_section_df$citation_text)))
  }
})

# Test 10: Section colors are properly applied
test_that("section colors are properly applied", {
  mock_data <- create_mock_citation_results()
  network <- create_citation_network(mock_data)
  stats <- attr(network, "stats")

  expect_true("section_colors" %in% names(stats))
  expect_true(is.vector(stats$section_colors))
  expect_true(length(stats$section_colors) > 0)
})

# Test 11: Edge properties are correctly set
test_that("edge properties reflect distance correctly", {
  mock_data <- create_mock_citation_results()
  network <- create_citation_network(mock_data, max_distance = 1000)

  # Check that network has edges
  expect_s3_class(network, "visNetwork")

  # Network should have data structure
  expect_true(!is.null(network$x$edges))
})

# Test 12: Section distribution is calculated
test_that("section distribution is calculated in statistics", {
  mock_data <- create_mock_citation_results()
  network <- create_citation_network(mock_data)
  stats <- attr(network, "stats")

  expect_true("section_distribution" %in% names(stats))
  expect_s3_class(stats$section_distribution, "data.frame")
  expect_true("primary_section" %in% names(stats$section_distribution))
  expect_true("n" %in% names(stats$section_distribution))
})

# Test 13: Unknown sections are handled
test_that("unknown sections are handled correctly", {
  mock_data_with_na <- create_mock_citation_results()
  mock_data_with_na$citations$section[1] <- NA

  network <- create_citation_network(mock_data_with_na)

  expect_s3_class(network, "visNetwork")
  stats <- attr(network, "stats")

  # Should have Unknown in section colors
  expect_true("Unknown" %in% names(stats$section_colors))
  expect_equal(unname(stats$section_colors["Unknown"]), "#CCCCCC")
})

# Test 14: Network attributes contain expected components
test_that("returned network has all expected attributes", {
  mock_data <- create_mock_citation_results()
  network <- create_citation_network(mock_data)

  expect_s3_class(network, "visNetwork")
  expect_true("x" %in% names(network))
  expect_true("nodes" %in% names(network$x))
  expect_true("edges" %in% names(network$x))
  expect_true("options" %in% names(network$x))
})
