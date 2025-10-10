#' Retrieve rich metadata from the CrossRef API for a given DOI
#'
#' @description
#' Fetches a comprehensive set of metadata for a given DOI from the CrossRef API.
#' The function parses the JSON response to extract main article details,
#' author information, and the full reference list.
#'
#' @param doi Character string. The DOI (Digital Object Identifier) of the article.
#' @param mailto Character string or NULL. An email address for polite API access,
#'   as recommended by CrossRef. This can lead to better service. Default is NULL.
#' @param output Character string. The desired output content. c("all","metadata","authors","references").
#'
#' @return A list containing three main elements:
#'   \itemize{
#'     \item \strong{main_metadata}: A data frame with a single row containing key
#'       metadata about the article (e.g., DOI, title, journal, year, volume, issue).
#'     \item \strong{authors}: A data frame listing all authors with their given name,
#'       family name, ORCID, and affiliation (if available).
#'     \item \strong{references}: A data frame containing detailed information for each
#'       reference in the article (e.g., key, DOI, title, author, year).
#'   }
#'   Returns NULL if the DOI is not found or an error occurs.
#'
#' @details
#' This function accesses the CrossRef REST API to retrieve structured metadata.
#' It handles nested information like authors and references by parsing them into
#' tidy data frames. Providing a `mailto` address in the user-agent is a best
#' practice for API interaction.
#'
#' @examples
#' \dontrun{
#' metadata <- get_crossref_references(
#'   "10.1007/s11192-016-1948-8",
#'   mailto = "your.email@example.com",
#'   output = 'all'
#' )
#'
#' # View main article metadata
#' print(metadata$main_metadata)
#'
#' # View author information
#' head(metadata$authors)
#'
#' # View reference list
#' head(metadata$references)
#' }
#'
#' @export
#' @importFrom httr2 request req_user_agent req_perform resp_body_json
get_crossref_references <- function(doi, mailto = NULL, output="references") {

  # --- Input Validation ---
  if (missing(doi) || is.null(doi) || doi == "") {
    stop("A valid DOI is required.")
  }

  # --- Helper function to parse authors ---
  .parse_authors <- function(author_list) {
    if (is.null(author_list) || length(author_list) == 0) return(NULL)

    do.call(rbind, lapply(author_list, function(a) {
      data.frame(
        given_name = ifelse(is.null(a$given), NA, a$given),
        family_name = ifelse(is.null(a$family), NA, a$family),
        orcid = ifelse(is.null(a$ORCID), NA, sub("http(s)?://orcid.org/", "", a$ORCID)),
        affiliation = ifelse(length(a$affiliation) == 0 || is.null(a$affiliation[[1]]$name), NA, a$affiliation[[1]]$name),
        stringsAsFactors = FALSE
      )
    }))
  }

  # --- Helper function to parse references ---
  .parse_references <- function(ref_list) {
    if (is.null(ref_list) || length(ref_list) == 0) return(NULL)

    do.call(rbind, lapply(ref_list, function(r) {
      data.frame(
        key = ifelse(is.null(r$key), NA, r$key),
        doi = ifelse(is.null(r$DOI), NA, r$DOI),
        article_title = ifelse(is.null(r$`article-title`), NA, r$`article-title`),
        author = ifelse(is.null(r$author), NA, r$author),
        year = ifelse(is.null(r$year), NA, r$year),
        journal = ifelse(is.null(r$`journal-title`), NA, r$`journal-title`),
        volume = ifelse(is.null(r$volume), NA, r$volume),
        first_page = ifelse(is.null(r$`first-page`), NA, r$`first-page`),
        ref_full = ifelse(is.null(r$`unstructured`), NA, r$`unstructured`),
        stringsAsFactors = FALSE
      )
    }))
  }

  # --- API Request ---
  base_url <- "https://api.crossref.org/works/"
  api_url <- paste0(base_url, doi)

  req <- httr2::request(api_url)

  if (!is.null(mailto)) {
    req <- req %>% httr2::req_user_agent(paste0("mailto:", mailto))
  }

  tryCatch({
    resp <- httr2::req_perform(req)
    content <- httr2::resp_body_json(resp)
    msg <- content$message

    # --- Extract and structure data ---
    main_meta <- data.frame(
      doi = ifelse(is.null(msg$DOI), NA, msg$DOI),
      title = ifelse(is.null(msg$title[[1]]), NA, msg$title[[1]]),
      journal_title = ifelse(is.null(msg$`container-title`[[1]]), NA, msg$`container-title`[[1]]),
      publisher = ifelse(is.null(msg$publisher), NA, msg$publisher),
      publication_year = ifelse(is.null(msg$issued$`date-parts`[[1]][1]), NA, msg$issued$`date-parts`[[1]][1]),
      volume = ifelse(is.null(msg$volume), NA, msg$volume),
      issue = ifelse(is.null(msg$issue), NA, msg$issue),
      pages = ifelse(is.null(msg$page), NA, msg$page),
      issn = ifelse(is.null(msg$ISSN[[1]]), NA, msg$ISSN[[1]]),
      subject = ifelse(length(msg$subject) == 0, NA, paste(unlist(msg$subject), collapse = "; ")),
      type = ifelse(is.null(msg$type), NA, msg$type),
      url = ifelse(is.null(msg$URL), NA, msg$URL),
      stringsAsFactors = FALSE
    )

    authors_df <- .parse_authors(msg$author)
    references_df <- .parse_references(msg$reference)

    # --- Return structured list ---
    results <- list(
      main_metadata = main_meta,
      authors = authors_df,
      references = references_df
    )
    switch(tolower(output),
           "all" = return(results),
           "metadata" = return(results$main_metadata),
           "authors" = return(results$authors),
           "references" = return(results$references))

  }, error = function(e) {
    message("Error retrieving or parsing data from Crossref API for DOI: ", doi)
    message("Original error: ", e$message)
    return(NULL)
  })
}

