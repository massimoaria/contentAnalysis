
# contentAnalysis

<!-- badges: start -->

<!-- badges: end -->

`contentAnalysis` provides comprehensive tools for extracting and
analyzing scientific content from PDF documents, including citation
extraction, reference matching, text analysis, and bibliometric
indicators.

## Features

- **PDF Import**: Multi-column layout support with structure
  preservation
- **Citation Extraction**: Comprehensive detection of citation formats
  (numbered, author-year, narrative, parenthetical)
- **Reference Parsing**: Extract references from text or CrossRef API
- **Citation-Reference Matching**: Automatic matching with multiple
  disambiguation strategies
- **Text Analysis**: Word frequencies, n-grams, lexical diversity
- **Citation Context**: Extract surrounding text for each citation
- **Bibliometric Indicators**: Citation density, distribution by
  section, co-occurrence networks
- **Word Distribution**: Track word frequencies across document sections

## Installation

You can install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("yourusername/contentAnalysis")
```

## Example

Basic workflow for analyzing a scientific paper:

``` r
library(contentAnalysis)

# Import PDF with automatic section detection
doc <- pdf2txt_auto("path/to/paper.pdf")

# Perform comprehensive content analysis
analysis <- analyze_scientific_content_enhanced(
  text = doc,
  doi = "10.xxxx/xxxxx",  # Optional: for CrossRef reference retrieval
  mailto = "your@email.com"
)

# View summary
analysis$summary

# Examine citations
head(analysis$citations)
table(analysis$citation_metrics$type_distribution)

# Check citation-reference matching quality
print_matching_diagnostics(analysis)

# Track specific words across sections
words <- c("machine learning", "neural network", "accuracy")
dist <- calculate_word_distribution(doc, words)
plot_word_distribution(dist)
```

## Main Functions

- `pdf2txt_auto()`: Import PDF with automatic section detection
- `analyze_scientific_content_enhanced()`: Comprehensive content and
  citation analysis
- `parse_references_section()`: Parse reference list
- `match_citations_to_references()`: Match citations to references
- `calculate_word_distribution()`: Track word frequencies across
  sections
- `plot_word_distribution()`: Interactive visualization of word
  distribution

## Dependencies

Core: pdftools, dplyr, tidyr, stringr, tidytext, tibble, httr2

Suggested: plotly, RColorBrewer, scales (for visualization)

## Citation

If you use this package in your research, please cite:

    Your Name (2025). contentAnalysis: Scientific Content and Citation Analysis from PDF Documents.
    R package version 0.1.0.
