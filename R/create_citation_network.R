#' Create Citation Co-occurrence Network
#'
#' @description
#' Creates an interactive network visualization of citation co-occurrences within a document.
#' Citations that appear close to each other are connected, with the strength of the connection
#' based on their distance (in characters). Nodes are colored by the document section where
#' citations primarily appear.
#'
#' @param citation_analysis_results A list object returned by citation analysis functions,
#'   containing at least two elements:
#'   \itemize{
#'     \item \code{network_data}: A data frame with columns \code{citation1}, \code{citation2},
#'       and \code{distance} representing pairs of co-occurring citations
#'     \item \code{citations}: A data frame with columns \code{citation_text_clean} and
#'       \code{section} containing citation text and section information
#'     \item \code{section_colors}: A named vector of colors for each section
#'   }
#' @param max_distance Numeric. Maximum distance (in characters) between citations to be
#'   considered connected. Default is 1000.
#' @param min_connections Integer. Minimum number of connections a citation must have to be
#'   included in the network. Default is 1.
#' @param show_labels Logical. Whether to show citation labels on the network nodes.
#'   Default is TRUE.
#'
#' @return A \code{visNetwork} object representing the interactive citation network, or NULL
#'   if no valid network can be created. The returned object has an additional \code{stats}
#'   attribute containing:
#'   \itemize{
#'     \item \code{n_nodes}: Number of nodes in the network
#'     \item \code{n_edges}: Number of edges in the network
#'     \item \code{avg_distance}: Average distance between connected citations
#'     \item \code{max_distance}: Maximum distance parameter used
#'     \item \code{section_distribution}: Distribution of citations across sections
#'     \item \code{multi_section_citations}: Citations appearing in multiple sections
#'     \item \code{section_colors}: Color mapping for sections
#'   }
#'
#' @details
#' The function creates a network where:
#' \itemize{
#'   \item \strong{Nodes} represent unique citations
#'   \item \strong{Node size} is proportional to the number of connections
#'   \item \strong{Node color} indicates the primary section where the citation appears
#'   \item \strong{Node border} is thicker (3px) for citations appearing in multiple sections
#'   \item \strong{Edges} connect citations that co-occur within the specified distance
#'   \item \strong{Edge width} decreases with distance (closer citations = thicker edges)
#'   \item \strong{Edge color} indicates distance: red (≤300 chars), blue (≤600 chars),
#'     gray (>600 chars)
#' }
#'
#' The network uses the Fruchterman-Reingold layout algorithm for optimal node positioning.
#' Interactive features include zooming, panning, node dragging, and highlighting of
#' nearest neighbors on hover.
#'
#' @examples
#' \dontrun{
#' # Assuming you have citation_analysis_results from a previous analysis
#' network <- create_citation_network(
#'   citation_analysis_results,
#'   max_distance = 800,
#'   min_connections = 2,
#'   show_labels = TRUE
#' )
#'
#' # Display the network
#' network
#'
#' # Access network statistics
#' stats <- attr(network, "stats")
#' print(stats$n_nodes)
#' print(stats$section_distribution)
#' }
#'
#' @import dplyr
#' @import visNetwork
#' @import stringr
#' @importFrom grDevices col2rgb
#' @importFrom stats na.omit
#'
#' @export
create_citation_network <- function(citation_analysis_results,
                                    max_distance = 1000,
                                    min_connections = 1,
                                    show_labels = TRUE) {

  network_data <- citation_analysis_results$network_data

  if (is.null(network_data) || nrow(network_data) == 0) {
    warning("No citation co-occurrence data found.")
    return(NULL)
  }

  # Filter by distance
  network_data_filtered <- network_data %>%
    filter(abs(distance) <= max_distance)

  if (nrow(network_data_filtered) == 0) {
    warning("No citation pairs found within the specified maximum distance.")
    return(NULL)
  }

  # Get unique citations
  all_citation_texts <- unique(c(network_data_filtered$citation1, network_data_filtered$citation2))

  # Get section information - aggregate by citation
  citations_with_sections <- citation_analysis_results$citations %>%
    select(citation_text_clean, section) %>%
    group_by(citation_text_clean) %>%
    summarise(
      sections = paste(unique(section), collapse = ", "),
      n_sections = n_distinct(section),
      primary_section = first(section),
      .groups = "drop"
    )

  # Create nodes
  nodes <- data.frame(
    id = 1:length(all_citation_texts),
    citation_text = all_citation_texts,
    label = if (show_labels) str_trunc(all_citation_texts, 25) else "",
    stringsAsFactors = FALSE
  )

  # Add section information to nodes
  nodes <- nodes %>%
    left_join(
      citations_with_sections,
      by = c("citation_text" = "citation_text_clean")
    ) %>%
    mutate(
      sections = replace_na(sections, "Unknown"),
      primary_section = replace_na(primary_section, "Unknown"),
      n_sections = replace_na(n_sections, 1)
    )

  # Calculate connections
  node_connections <- rbind(
    data.frame(citation = network_data_filtered$citation1, stringsAsFactors = FALSE),
    data.frame(citation = network_data_filtered$citation2, stringsAsFactors = FALSE)
  ) %>%
    count(citation, name = "connections")

  nodes$connections <- sapply(nodes$citation_text, function(cite) {
    conn <- node_connections$connections[node_connections$citation == cite]
    if (length(conn) == 0) return(0)
    return(conn[1])
  })

  # Filter by connections
  nodes <- nodes[nodes$connections >= min_connections, ]
  valid_citations <- nodes$citation_text
  network_data_filtered <- network_data_filtered %>%
    filter(citation1 %in% valid_citations & citation2 %in% valid_citations)

  if (nrow(network_data_filtered) == 0) {
    warning("No valid connections after filtering.")
    return(NULL)
  }

  # Set node properties
  nodes$size <- pmax(15, pmin(40, 15 + nodes$connections * 3))/2

  # Assign colors dynamically using section colors
  section_colors <- citation_analysis_results$section_colors

  # Ensure "Unknown" always has a gray color (add if missing, override if present)
  section_colors["Unknown"] <- "#CCCCCC"

  nodes$group <- nodes$primary_section

  # Add transparency to node colors (85% opacity)
  nodes$color <- sapply(section_colors[nodes$primary_section], function(hex_color) {
    # Convert hex to rgba with transparency
    rgb_vals <- col2rgb(hex_color)
    sprintf("rgba(%d, %d, %d, 0.85)", rgb_vals[1], rgb_vals[2], rgb_vals[3])
  })

  # Add special border for multi-section nodes
  nodes$borderWidth <- ifelse(nodes$n_sections > 1, 3, 1)
  nodes$borderWidthSelected <- ifelse(nodes$n_sections > 1, 5, 2)

  # Create title with complete information
  nodes$title <- paste0(
    nodes$citation_text,
    "\n<br><b>Section(s):</b> ", nodes$sections,
    ifelse(nodes$n_sections > 1,
           paste0(" (", nodes$n_sections, " sections)"),
           ""),
    "\n<br><b>Connections:</b> ", nodes$connections
  )

  nodes <- nodes %>%
    mutate(font.size = size,
           font.vadjust = -0.7 * font.size)

  # Create edges
  edges <- data.frame(
    from = sapply(network_data_filtered$citation1, function(cite) {
      nodes$id[nodes$citation_text == cite][1]
    }),
    to = sapply(network_data_filtered$citation2, function(cite) {
      nodes$id[nodes$citation_text == cite][1]
    }),
    distance = abs(network_data_filtered$distance),
    stringsAsFactors = FALSE
  )

  edges <- edges[!is.na(edges$from) & !is.na(edges$to), ]

  # Reduce edge width and add transparency
  edges$width <- pmax(0.5, 3 - (edges$distance / 200))

  # Colors with transparency (30% opacity for greater transparency)
  edges$color <- ifelse(edges$distance <= 300, "rgba(255, 111, 111, 0.3)",
                        ifelse(edges$distance <= 600, "rgba(127, 179, 213, 0.3)",
                               "rgba(204, 204, 204, 0.25)"))

  edges$title <- paste("Distance:", edges$distance, "characters")

  # Create network with Fruchterman-Reingold layout
  network <- visNetwork(nodes, edges, type="full", smooth = TRUE, physics = FALSE) %>%
    visIgraphLayout(layout = "layout_nicely", type = "full")

  # Configure options
  network <- network %>%
    visOptions(highlightNearest = TRUE) %>%
    visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE, zoomSpeed = 0.2) %>%
    visPhysics(enabled = FALSE) %>%
    visNodes(
      borderWidth = 1,
      borderWidthSelected = 2
    )

  # Add stats
  n_nodes <- nrow(nodes)
  n_edges <- nrow(edges)
  avg_distance <- round(mean(edges$distance), 1)

  # Statistics by section and multi-section
  section_stats <- nodes %>%
    count(primary_section) %>%
    arrange(desc(n))

  multi_section_citations <- nodes %>%
    filter(n_sections > 1) %>%
    select(citation_text, sections, n_sections)

  attr(network, "stats") <- list(
    n_nodes = n_nodes,
    n_edges = n_edges,
    avg_distance = avg_distance,
    max_distance = max_distance,
    section_distribution = section_stats,
    multi_section_citations = multi_section_citations,
    section_colors = section_colors
  )

  return(network)
}
