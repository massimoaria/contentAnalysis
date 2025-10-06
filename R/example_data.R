#' Get path to example paper
#'
#' @description
#' Returns the path to the example paper included in the package source
#' (available on GitHub but not in CRAN builds).
#'
#' @param example Character string. Name of example file (default: "example_paper.pdf")
#'
#' @return Path to example file if it exists, otherwise downloads it from GitHub
#'
#' @examples
#' \dontrun{
#' paper_path <- get_example_paper()
#' doc <- pdf2txt_auto(paper_path, n_columns = 2)
#' }
#'
#' @export
get_example_paper <- function(example = "example_paper.pdf") {

  # Try to find in package installation
  pkg_path <- system.file("examples", example, package = "contentanalysis")

  if (file.exists(pkg_path)) {
    return(pkg_path)
  }

  # If not found (e.g., CRAN version), download from GitHub
  message("Example paper not found in package installation. Downloading from GitHub...")

  if (example == "example_paper.pdf") {
    # GitHub raw URL
    github_url <- "https://raw.githubusercontent.com/massimoaria/contentanalysis/master/inst/examples/example_paper.pdf"
    temp_path <- file.path(tempdir(), example)

    tryCatch({
      download.file(github_url, destfile = temp_path, mode = "wb", quiet = TRUE)
      message("Downloaded to: ", temp_path)
      return(temp_path)
    }, error = function(e) {
      stop("Could not download example paper from GitHub. Error: ", e$message,
           "\nMake sure the file exists at: ", github_url)
    })
  }

  stop("Example file '", example, "' not found")
}
