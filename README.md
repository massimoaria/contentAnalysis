
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

Complete workflow analyzing a real scientific paper:

``` r
library(contentAnalysis)
```

### Download example paper (open access)

``` r
paper_url <- "https://raw.githubusercontent.com/massimoaria/contentAnalysis/master/inst/examples/example_paper.pdf"
download.file(paper_url, destfile = "example_paper.pdf", mode = "wb")
```

### Import PDF with automatic section detection

``` r
doc <- pdf2txt_auto("example_paper.pdf", n_columns = 2)
#> Using 17 sections from PDF table of contents
#> Found 16 sections: Preface, Introduction, Related work, Internal processing approaches, Random forest extra information, Visualization toolkits, Post-Hoc approaches, Size reduction, Rule extraction, Local explanation, Comparison study, Experimental design, Analysis, Conclusion, Acknowledgment, References
#> Normalized 77 references with consistent \n\n separators

# Check detected sections
names(doc)
#>  [1] "Full_text"                       "Preface"                        
#>  [3] "Introduction"                    "Related work"                   
#>  [5] "Internal processing approaches"  "Random forest extra information"
#>  [7] "Visualization toolkits"          "Post-Hoc approaches"            
#>  [9] "Size reduction"                  "Rule extraction"                
#> [11] "Local explanation"               "Comparison study"               
#> [13] "Experimental design"             "Analysis"                       
#> [15] "Conclusion"                      "Acknowledgment"                 
#> [17] "References"
```

### Perform comprehensive content analysis with CrossRef

``` r
analysis <- analyze_scientific_content_enhanced(
  text = doc,
  doi = "10.1016/j.mlwa.2021.100094",
  mailto = "your@email.com"
)
#> Attempting to retrieve references from CrossRef...
#> Successfully retrieved 33 references from CrossRef
```

### View summary statistics

``` r

analysis$summary
#> $total_words_analyzed
#> [1] 3894
#> 
#> $unique_words
#> [1] 1429
#> 
#> $citations_extracted
#> [1] 51
#> 
#> $narrative_citations
#> [1] 15
#> 
#> $parenthetical_citations
#> [1] 36
#> 
#> $complex_citations_parsed
#> [1] 12
#> 
#> $lexical_diversity
#> [1] 0.3669748
#> 
#> $average_citation_context_length
#> [1] 3198.725
#> 
#> $citation_density_per_1000_words
#> [1] 6.08
#> 
#> $references_parsed
#> [1] 33
#> 
#> $citations_matched_to_refs
#> [1] 35
#> 
#> $match_quality
#> # A tibble: 5 × 3
#>   match_confidence             n percentage
#>   <chr>                    <int>      <dbl>
#> 1 high                        35       68.6
#> 2 medium_etal_inconsistent     7       13.7
#> 3 no_match_author              6       11.8
#> 4 no_match_missing_info        1        2  
#> 5 no_match_year                2        3.9
```

### Examine citations by type

``` r
analysis$citation_metrics$type_distribution
#> # A tibble: 11 × 3
#>    citation_type                   n percentage
#>    <chr>                       <int>      <dbl>
#>  1 parsed_from_multiple           12      23.5 
#>  2 author_year_basic               9      17.6 
#>  3 author_year_and                 8      15.7 
#>  4 narrative_etal                  7      13.7 
#>  5 author_year_etal                3       5.88
#>  6 narrative_three_authors_and     3       5.88
#>  7 narrative_two_authors_and       3       5.88
#>  8 narrative_four_authors_and      2       3.92
#>  9 see_citations                   2       3.92
#> 10 doi_pattern                     1       1.96
#> 11 numbered_simple                 1       1.96
```

### Check matching quality

``` r
print_matching_diagnostics(analysis)
#> 
#> === CITATION-REFERENCE MATCHING DIAGNOSTICS ===
#> 
#> Total citations: 51 
#> Total references parsed: 33 
#> 
#> Match quality distribution:
#> # A tibble: 5 × 2
#>   match_confidence             n
#>   <chr>                    <int>
#> 1 high                        35
#> 2 medium_etal_inconsistent     7
#> 3 no_match_author              6
#> 4 no_match_year                2
#> 5 no_match_missing_info        1
#> 
#> Match rate: 82.4 %
#> 
#> High confidence matches: 35 
#> 
#> Citations without matches:
#> # A tibble: 9 × 4
#>   citation_text_clean                     cite_author cite_year match_confidence
#>   <chr>                                   <chr>       <chr>     <chr>           
#> 1 https://doi.org/10.1016/j.mlwa.2021.10… https       1016      no_match_year   
#> 2 (see Breiman, 1996)                     see         1996      no_match_author 
#> 3 (Ribeiro, Singh, and Guestrin, 2016)    Ribeiro     2016      no_match_author 
#> 4 [6]                                     <NA>        <NA>      no_match_missin…
#> 5 (Subsequently, Zhou, Zhou, and Hooker,… Subsequent… 2018      no_match_author 
#> 6 (see Guidotti et al., 2018)             see         2018      no_match_author 
#> 7 (Lou, Caruana, and Gehrke, 2012)        Lou         2012      no_match_year   
#> 8 (Recently, Ribeiro et al., 2016)        Recently    2016      no_match_author 
#> 9 (Akosa, 2017)                           Akosa       2017      no_match_author
```

### Analyze citation contexts

``` r
head(analysis$citation_contexts[, c("citation_text_clean", "section", "full_context")])
#> # A tibble: 6 × 3
#>   citation_text_clean                        section      full_context          
#>   <chr>                                      <chr>        <chr>                 
#> 1 (Mitchell, 1997)                           Introduction on their own and make…
#> 2 (Breiman, Friedman, Olshen, & Stone, 1984) Introduction are supervised learni…
#> 3 https://doi.org/10.1016/j.mlwa.2021.100094 Introduction author E mail address…
#> 4 (Breiman, 2001)                            Introduction node of a random subs…
#> 5 (see Breiman, 1996)                        Introduction single training set a…
#> 6 (Hastie, Tibshirani, & Friedman, 2009)     Introduction by calculating predic…
```

### Track methodological terms across sections

``` r
method_terms <- c("machine learning", "regression", "validation", "dataset")
word_dist <- calculate_word_distribution(doc, method_terms)
```

### Create interactive visualization

### Examine most frequent words

``` r
head(analysis$word_frequencies, 10)
#> # A tibble: 10 × 4
#>    word         n frequency  rank
#>    <chr>    <int>     <dbl> <int>
#>  1 model       47   0.0121      1
#>  2 forest      46   0.0118      2
#>  3 accuracy    43   0.0110      3
#>  4 trees       42   0.0108      4
#>  5 random      41   0.0105      5
#>  6 learning    34   0.00873     6
#>  7 data        31   0.00796     7
#>  8 machine     31   0.00796     8
#>  9 set         29   0.00745     9
#> 10 variable    28   0.00719    10
```

### Citation co-occurrence network

``` r
head(analysis$network_data)
#> # A tibble: 6 × 5
#>   citation1                                  citation2      distance type1 type2
#>   <chr>                                      <chr>             <int> <chr> <chr>
#> 1 (Mitchell, 1997)                           (Breiman, Fri…      701 auth… auth…
#> 2 (Mitchell, 1997)                           https://doi.o…      992 auth… doi_…
#> 3 (Breiman, Friedman, Olshen, & Stone, 1984) https://doi.o…      250 auth… doi_…
#> 4 (Breiman, 2001)                            (see Breiman,…      257 auth… see_…
#> 5 (Breiman, 2001)                            (Hastie, Tibs…      617 auth… auth…
#> 6 (Breiman, 2001)                            (Hastie et al…      829 auth… auth…
```

### Working with references

``` r
# View parsed references
head(analysis$parsed_references[, c("ref_first_author", "ref_year", "ref_full_text")])
#>   ref_first_author ref_year
#> 1            Adadi     2018
#> 2             <NA>     <NA>
#> 3           Branco     2016
#> 4          Breiman     1996
#> 5          Breiman     2001
#> 6          Breiman     1984
#>                                                                                                  ref_full_text
#> 1 Adadi (2018) Peeking inside the black-box: A survey on explainable artificial intelligence (XAI) IEEE Access
#> 2                                                                                                             
#> 3                    Branco (2016) A survey of predictive modeling on imbalanced domains ACM Computing Surveys
#> 4                                                           Breiman (1996) Bagging predictors Machine Learning
#> 5                                                               Breiman (2001) Random forests Machine Learning
#> 6               Breiman (1984) Classification and regression trees. Belmont, CA: Wadsworth International Group

# Find citations to specific author
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
analysis$citation_references_mapping %>%
  filter(grepl("Smith", ref_authors, ignore.case = TRUE))
#> # A tibble: 0 × 13
#> # ℹ 13 variables: citation_id <chr>, citation_text <chr>,
#> #   citation_text_clean <chr>, citation_type <chr>, cite_author <chr>,
#> #   cite_second_author <chr>, cite_year <chr>, cite_has_etal <lgl>,
#> #   matched_ref_id <chr>, ref_full_text <chr>, ref_authors <chr>,
#> #   ref_year <chr>, match_confidence <chr>

# Citations by section
analysis$citation_metrics$section_distribution
#> # A tibble: 9 × 3
#>   section                             n percentage
#>   <chr>                           <int>      <dbl>
#> 1 Related work                        9      17.6 
#> 2 Introduction                        7      13.7 
#> 3 Experimental design                 6      11.8 
#> 4 Local explanation                   6      11.8 
#> 5 Random forest extra information     6      11.8 
#> 6 Size reduction                      6      11.8 
#> 7 Visualization toolkits              5       9.8 
#> 8 Analysis                            4       7.84
#> 9 Rule extraction                     2       3.92
```

### Advanced: Word distribution analysis

``` r
# Track disease-related terms
disease_terms <- c("covid", "pandemic", "health", "policy", "vaccination")
dist <- calculate_word_distribution(doc, disease_terms, use_sections = TRUE)

# View frequencies by section
dist %>%
  select(segment_name, word, count, percentage) %>%
  arrange(segment_name, desc(percentage))
#> # A tibble: 1 × 4
#>   segment_name word   count percentage
#>   <chr>        <chr>  <int>      <dbl>
#> 1 Conclusion   health     1      0.328

# Visualize trends
#plot_word_distribution(dist, plot_type = "area", smooth = FALSE)
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

    Massimo Aria (2025). contentAnalysis: Scientific Content and Citation Analysis from PDF Documents.
    R package version 0.1.0.
