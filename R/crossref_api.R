#' Retrieve references from CrossRef API
#'
#' @description
#' Fetches reference list for a given DOI from the CrossRef API.
#'
#' @param doi Character string. DOI of the article.
#' @param mailto Character string or NULL. Email address for polite API access.
#'   Recommended by CrossRef for better service. Default is NULL.
#'
#' @return Data frame with reference information including:
#'   \itemize{
#'     \item key: Reference key/identifier
#'     \item doi: DOI of cited work (if available)
#'     \item article_title: Title of cited article
#'     \item author: Author string
#'     \item year: Publication year
#'     \item journal: Journal name
#'     \item volume: Volume number
#'     \item first_page: First page
#'   }
#'   Returns NULL if no references found.
#'
#' @details
#' Uses CrossRef REST API to retrieve structured reference data. This is more
#' reliable than text parsing when DOI is available. The mailto parameter
#' identifies your application to CrossRef and may result in better service.
#'
#' @examples
#' \dontrun{
#' refs <- get_crossref_references(
#'   "10.1016/j.joi.2017.08.007",
#'   mailto = "your@email.com"
#' )
#' head(refs)
#' }
#'
#' @export
#' @importFrom httr2 request req_user_agent req_perform resp_body_json
get_crossref_references <- function(doi, mailto = NULL) {

  if (missing(doi) || is.null(doi) || doi == "") {
    stop("DOI is required")
  }

  base_url <- "https://api.crossref.org/works/"
  api_url <- paste0(base_url, doi)

  req <- httr2::request(api_url)

  if (!is.null(mailto)) {
    req <- req %>% httr2::req_user_agent(paste0("mailto:", mailto))
  }

  tryCatch({
    resp <- httr2::req_perform(req)

    content <- httr2::resp_body_json(resp)

    if (!is.null(content$message$reference)) {
      refs <- content$message$reference

      refs_df <- do.call(rbind, lapply(refs, function(x) {
        data.frame(
          key = ifelse(is.null(x$key), NA, x$key),
          doi = ifelse(is.null(x$DOI), NA, x$DOI),
          article_title = ifelse(is.null(x$`article-title`), NA, x$`article-title`),
          author = ifelse(is.null(x$author), NA, x$author),
          year = ifelse(is.null(x$year), NA, x$year),
          journal = ifelse(is.null(x$`journal-title`), NA, x$`journal-title`),
          volume = ifelse(is.null(x$volume), NA, x$volume),
          first_page = ifelse(is.null(x$`first-page`), NA, x$`first-page`),
          stringsAsFactors = FALSE
        )
      }))

      return(refs_df)

    } else {
      message("No references found for DOI: ", doi)
      return(NULL)
    }

  }, error = function(e) {
    stop("Error retrieving data from Crossref API: ", e$message)
  })
}
