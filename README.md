
# contentanalysis

<!-- badges: start -->

<!-- badges: end -->

`contentanalysis` provides comprehensive tools for extracting and
analyzing scientific content from PDF documents, including citation
extraction, reference matching, text analysis, network visualization,
and bibliometric indicators.

## Features

- **PDF Import**: Multi-column layout support with structure
  preservation
- **Citation Extraction**: Comprehensive detection of citation formats
  (numbered, author-year, narrative, parenthetical)
- **Reference Parsing**: Extract references from text or CrossRef API
- **Citation-Reference Matching**: Automatic matching with multiple
  disambiguation strategies
- **Citation Network**: Interactive network visualization of citation
  co-occurrences
- **Text Analysis**: Word frequencies, n-grams, lexical diversity,
  readability metrics
- **Citation Context**: Extract surrounding text for each citation
- **Bibliometric Indicators**: Citation density, distribution by
  section, co-occurrence analysis
- **Word Distribution**: Track word frequencies across document sections

## Installation

You can install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("masismo/contentanalysis")
```

## Example

Complete workflow analyzing a real scientific paper:

``` r
library(contentanalysis)
```

### Download example paper (open access)

``` r
paper_url <- "https://raw.githubusercontent.com/massimoaria/contentanalysis/master/inst/examples/example_paper.pdf"
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
analysis <- analyze_scientific_content(
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
#> [1] 42
#> 
#> $match_quality
#> # A tibble: 5 × 3
#>   match_confidence            n percentage
#>   <chr>                   <int>      <dbl>
#> 1 high                       41       80.4
#> 2 high_numbered               1        2  
#> 3 medium_multiple_matches     1        2  
#> 4 no_match_author             6       11.8
#> 5 no_match_year               2        3.9
```

### Readability indices

``` r
readability <- calculate_readability_indices(doc$Full_text, detailed = TRUE)
readability
#> # A tibble: 1 × 12
#>   flesch_kincaid_grade flesch_reading_ease automated_readability_index
#>                  <dbl>               <dbl>                       <dbl>
#> 1                 12.4                33.9                        11.8
#> # ℹ 9 more variables: gunning_fog_index <dbl>, n_sentences <int>,
#> #   n_words <int>, n_syllables <dbl>, n_characters <int>,
#> #   n_complex_words <int>, avg_sentence_length <dbl>,
#> #   avg_syllables_per_word <dbl>, pct_complex_words <dbl>
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
#>   match_confidence            n
#>   <chr>                   <int>
#> 1 high                       41
#> 2 no_match_author             6
#> 3 no_match_year               2
#> 4 high_numbered               1
#> 5 medium_multiple_matches     1
#> 
#> Match rate: 84.3 %
#> 
#> High confidence matches: 41 
#> 
#> Citations without matches:
#> # A tibble: 8 × 4
#>   citation_text_clean                     cite_author cite_year match_confidence
#>   <chr>                                   <chr>       <chr>     <chr>           
#> 1 https://doi.org/10.1016/j.mlwa.2021.10… https       1016      no_match_year   
#> 2 (see Breiman, 1996)                     see         1996      no_match_author 
#> 3 (Ribeiro, Singh, and Guestrin, 2016)    Ribeiro     2016      no_match_author 
#> 4 (Subsequently, Zhou, Zhou, and Hooker,… Subsequent… 2018      no_match_author 
#> 5 (see Guidotti et al., 2018)             see         2018      no_match_author 
#> 6 (Lou, Caruana, and Gehrke, 2012)        Lou         2012      no_match_year   
#> 7 (Recently, Ribeiro et al., 2016)        Recently    2016      no_match_author 
#> 8 (Akosa, 2017)                           Akosa       2017      no_match_author
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

## Citation Network Visualization

Create interactive network visualizations showing how citations co-occur
within your document:

``` r
# Create citation network
network <- create_citation_network(
  citation_analysis_results = analysis,
  max_distance = 800,          # Max distance between citations (characters)
  min_connections = 2,          # Minimum connections to include a node
  show_labels = TRUE
)

# Display interactive network
network
```

<div class="visNetwork html-widget html-fill-item" id="htmlwidget-3e16391232030925367d" style="width:100%;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-3e16391232030925367d">{"x":{"nodes":{"id":[2,3,4,5,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,29,31,32,34,36,37],"citation_text":["(Breiman, Friedman, Olshen, & Stone, 1984)","(Breiman, 2001)","(see Breiman, 1996)","(Hastie, Tibshirani, & Friedman, 2009)","(Adadi & Berrada, 2018)","(Došilović, Brčić, & Hlupić, 2018)","(Du, Liu, & Hu, 2019)","(Guidotti et al., 2018)","(Haddouchi & Berrado, 2019)","(Breiman, un2001)","(Genuer, Poggi, & Tuleau-Malot, 2010)","(Louppe, Wehenkel, Sutera, & Geurts, 2013)","(Friedman, 2001)","(Liaw, Wiener, et al., 2002)","(Tan, Hooker, & Wells, 2016)","(Chipman, George, and McCulloh, 1998)","(Gibbons et al., 2013)","(Subsequently, Zhou, Zhou, and Hooker, 2018)","(see Guidotti et al., 2018)","(Lou, Caruana, and Gehrke, 2012)","(Meinshausen, 2010)","(Akosa, 2017)","(García, Mollineda, & Sánchez, 2009)","(Liaw et al., 2002)","(Hastie et al., 2009)","(Lipton, 2018)","(Ehrlinger, 2016)","(Zhou et al., 2018)","(Deng, 2019)","(Sokolova, Japkowicz, Szpakowicz, 2006)"],"label":["(Breiman, Friedman, Ol...","(Breiman, 2001)","(see Breiman, 1996)","(Hastie, Tibshirani, &...","(Adadi & Berrada, 2018)","(Došilović, Brčić, & H...","(Du, Liu, & Hu, 2019)","(Guidotti et al., 2018)","(Haddouchi & Berrado, ...","(Breiman, un2001)","(Genuer, Poggi, & Tule...","(Louppe, Wehenkel, Sut...","(Friedman, 2001)","(Liaw, Wiener, et al.,...","(Tan, Hooker, & Wells,...","(Chipman, George, and ...","(Gibbons et al., 2013)","(Subsequently, Zhou, Z...","(see Guidotti et al., ...","(Lou, Caruana, and Geh...","(Meinshausen, 2010)","(Akosa, 2017)","(García, Mollineda, & ...","(Liaw et al., 2002)","(Hastie et al., 2009)","(Lipton, 2018)","(Ehrlinger, 2016)","(Zhou et al., 2018)","(Deng, 2019)","(Sokolova, Japkowicz, ..."],"sections":["Introduction","Introduction","Introduction","Introduction","Related work","Related work","Related work","Related work","Related work, Visualization toolkits, Local explanation","Random forest extra information","Random forest extra information","Random forest extra information","Random forest extra information","Visualization toolkits","Visualization toolkits","Size reduction","Size reduction","Size reduction","Local explanation","Local explanation","Rule extraction, Experimental design, Analysis","Experimental design","Experimental design","Analysis","Introduction","Related work","Visualization toolkits","Size reduction","Rule extraction, Experimental design, Analysis","Experimental design"],"n_sections":[1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,3,1],"primary_section":["Introduction","Introduction","Introduction","Introduction","Related work","Related work","Related work","Related work","Related work","Random forest extra information","Random forest extra information","Random forest extra information","Random forest extra information","Visualization toolkits","Visualization toolkits","Size reduction","Size reduction","Size reduction","Local explanation","Local explanation","Rule extraction","Experimental design","Experimental design","Analysis","Introduction","Related work","Visualization toolkits","Size reduction","Rule extraction","Experimental design"],"connections":[2,2,3,3,5,5,5,5,8,4,4,4,8,2,2,2,5,3,2,2,3,2,2,2,2,5,2,2,3,2],"size":[10.5,10.5,12,12,15,15,15,15,19.5,13.5,13.5,13.5,19.5,10.5,10.5,10.5,15,12,10.5,10.5,12,10.5,10.5,10.5,10.5,15,10.5,10.5,12,10.5],"group":["Introduction","Introduction","Introduction","Introduction","Related work","Related work","Related work","Related work","Related work","Random forest extra information","Random forest extra information","Random forest extra information","Random forest extra information","Visualization toolkits","Visualization toolkits","Size reduction","Size reduction","Size reduction","Local explanation","Local explanation","Rule extraction","Experimental design","Experimental design","Analysis","Introduction","Related work","Visualization toolkits","Size reduction","Rule extraction","Experimental design"],"color":["rgba(228, 26, 28, 0.85)","rgba(228, 26, 28, 0.85)","rgba(228, 26, 28, 0.85)","rgba(228, 26, 28, 0.85)","rgba(55, 126, 184, 0.85)","rgba(55, 126, 184, 0.85)","rgba(55, 126, 184, 0.85)","rgba(55, 126, 184, 0.85)","rgba(55, 126, 184, 0.85)","rgba(77, 175, 74, 0.85)","rgba(77, 175, 74, 0.85)","rgba(77, 175, 74, 0.85)","rgba(77, 175, 74, 0.85)","rgba(152, 78, 163, 0.85)","rgba(152, 78, 163, 0.85)","rgba(255, 127, 0, 0.85)","rgba(255, 127, 0, 0.85)","rgba(255, 127, 0, 0.85)","rgba(247, 129, 191, 0.85)","rgba(247, 129, 191, 0.85)","rgba(166, 86, 40, 0.85)","rgba(153, 153, 153, 0.85)","rgba(153, 153, 153, 0.85)","rgba(102, 194, 165, 0.85)","rgba(228, 26, 28, 0.85)","rgba(55, 126, 184, 0.85)","rgba(152, 78, 163, 0.85)","rgba(255, 127, 0, 0.85)","rgba(166, 86, 40, 0.85)","rgba(153, 153, 153, 0.85)"],"borderWidth":[1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,3,1],"borderWidthSelected":[2,2,2,2,2,2,2,2,5,2,2,2,2,2,2,2,2,2,2,2,5,2,2,2,2,2,2,2,5,2],"title":["(Breiman, Friedman, Olshen, & Stone, 1984)\n<br><b>Section(s):<\/b> Introduction\n<br><b>Connections:<\/b> 2","(Breiman, 2001)\n<br><b>Section(s):<\/b> Introduction\n<br><b>Connections:<\/b> 2","(see Breiman, 1996)\n<br><b>Section(s):<\/b> Introduction\n<br><b>Connections:<\/b> 3","(Hastie, Tibshirani, & Friedman, 2009)\n<br><b>Section(s):<\/b> Introduction\n<br><b>Connections:<\/b> 3","(Adadi & Berrada, 2018)\n<br><b>Section(s):<\/b> Related work\n<br><b>Connections:<\/b> 5","(Došilović, Brčić, & Hlupić, 2018)\n<br><b>Section(s):<\/b> Related work\n<br><b>Connections:<\/b> 5","(Du, Liu, & Hu, 2019)\n<br><b>Section(s):<\/b> Related work\n<br><b>Connections:<\/b> 5","(Guidotti et al., 2018)\n<br><b>Section(s):<\/b> Related work\n<br><b>Connections:<\/b> 5","(Haddouchi & Berrado, 2019)\n<br><b>Section(s):<\/b> Related work, Visualization toolkits, Local explanation (3 sections)\n<br><b>Connections:<\/b> 8","(Breiman, un2001)\n<br><b>Section(s):<\/b> Random forest extra information\n<br><b>Connections:<\/b> 4","(Genuer, Poggi, & Tuleau-Malot, 2010)\n<br><b>Section(s):<\/b> Random forest extra information\n<br><b>Connections:<\/b> 4","(Louppe, Wehenkel, Sutera, & Geurts, 2013)\n<br><b>Section(s):<\/b> Random forest extra information\n<br><b>Connections:<\/b> 4","(Friedman, 2001)\n<br><b>Section(s):<\/b> Random forest extra information\n<br><b>Connections:<\/b> 8","(Liaw, Wiener, et al., 2002)\n<br><b>Section(s):<\/b> Visualization toolkits\n<br><b>Connections:<\/b> 2","(Tan, Hooker, & Wells, 2016)\n<br><b>Section(s):<\/b> Visualization toolkits\n<br><b>Connections:<\/b> 2","(Chipman, George, and McCulloh, 1998)\n<br><b>Section(s):<\/b> Size reduction\n<br><b>Connections:<\/b> 2","(Gibbons et al., 2013)\n<br><b>Section(s):<\/b> Size reduction\n<br><b>Connections:<\/b> 5","(Subsequently, Zhou, Zhou, and Hooker, 2018)\n<br><b>Section(s):<\/b> Size reduction\n<br><b>Connections:<\/b> 3","(see Guidotti et al., 2018)\n<br><b>Section(s):<\/b> Local explanation\n<br><b>Connections:<\/b> 2","(Lou, Caruana, and Gehrke, 2012)\n<br><b>Section(s):<\/b> Local explanation\n<br><b>Connections:<\/b> 2","(Meinshausen, 2010)\n<br><b>Section(s):<\/b> Rule extraction, Experimental design, Analysis (3 sections)\n<br><b>Connections:<\/b> 3","(Akosa, 2017)\n<br><b>Section(s):<\/b> Experimental design\n<br><b>Connections:<\/b> 2","(García, Mollineda, & Sánchez, 2009)\n<br><b>Section(s):<\/b> Experimental design\n<br><b>Connections:<\/b> 2","(Liaw et al., 2002)\n<br><b>Section(s):<\/b> Analysis\n<br><b>Connections:<\/b> 2","(Hastie et al., 2009)\n<br><b>Section(s):<\/b> Introduction\n<br><b>Connections:<\/b> 2","(Lipton, 2018)\n<br><b>Section(s):<\/b> Related work\n<br><b>Connections:<\/b> 5","(Ehrlinger, 2016)\n<br><b>Section(s):<\/b> Visualization toolkits\n<br><b>Connections:<\/b> 2","(Zhou et al., 2018)\n<br><b>Section(s):<\/b> Size reduction\n<br><b>Connections:<\/b> 2","(Deng, 2019)\n<br><b>Section(s):<\/b> Rule extraction, Experimental design, Analysis (3 sections)\n<br><b>Connections:<\/b> 3","(Sokolova, Japkowicz, Szpakowicz, 2006)\n<br><b>Section(s):<\/b> Experimental design\n<br><b>Connections:<\/b> 2"],"font.size":[10.5,10.5,12,12,15,15,15,15,19.5,13.5,13.5,13.5,19.5,10.5,10.5,10.5,15,12,10.5,10.5,12,10.5,10.5,10.5,10.5,15,10.5,10.5,12,10.5],"font.vadjust":[-7.35,-7.35,-8.399999999999999,-8.399999999999999,-10.5,-10.5,-10.5,-10.5,-13.65,-9.449999999999999,-9.449999999999999,-9.449999999999999,-13.65,-7.35,-7.35,-7.35,-10.5,-8.399999999999999,-7.35,-7.35,-8.399999999999999,-7.35,-7.35,-7.35,-7.35,-10.5,-7.35,-7.35,-8.399999999999999,-7.35],"x":[-0.131220841700425,-0.5338869017986387,-0.6079719602481056,-0.7506458832291836,0.6873384019856548,0.5402440983389927,0.5795438112331048,0.531914144333848,0.3529655403235288,-0.72971251342704,-0.9339942442343938,-1,-0.8770700278924546,0.09353504412540459,1,0.9741730154625226,-0.5388760831892117,-0.7162398685030397,0.1763794014886566,0.0009717022734050396,0.4280323155068428,0.1406376554759865,0.0003612498528278163,0.5539048440483061,-0.8373148286520492,0.7184412864928902,0.02498551779213698,-0.5680225853723436,0.5987345596757605,-0.07502230443586932],"y":[0.2566567384806639,-0.7925576837007097,-0.5706141063884114,-0.7139523963437109,-0.3474200140878467,-0.4515867631873537,-0.5969062404453618,-0.3014642452030273,-0.4888654546361746,0.08141609800828609,0.2286193884080132,0.003523028845041631,0.08227178678535729,-0.2775637960027925,0.1697516234158849,0.3755111213595386,0.6361611416231838,0.6914449884723575,-0.8062173171983938,-1,0.6245838136659314,0.975460152413667,0.8024922977831797,0.4607410848073874,-0.5048812590515523,-0.5044453422458056,-0.4669114058199197,0.8456913756759374,0.6822668898731969,1]},"edges":{"from":[3,3,4,4,5,7,7,7,7,7,8,8,8,8,9,9,9,10,10,11,12,12,12,12,13,13,13,14,14,15,11,11,16,18,20,20,21,21,20,11,22,24,25,25,26,27,27,24],"to":[4,5,5,29,29,8,9,10,11,31,9,10,11,31,10,11,31,11,31,31,13,14,15,15,14,15,15,15,15,15,16,32,32,19,21,20,20,34,34,22,23,36,26,37,37,24,36,36],"distance":[257,617,342,554,175,150,150,150,150,150,150,150,150,150,150,150,150,150,150,150,99,99,189,289,99,189,289,189,289,85,84,297,187,702,312,433,79,468,368,128,680,14,87,87,87,144,469,307],"width":[1.715,0.5,1.29,0.5,2.125,2.25,2.25,2.25,2.25,2.25,2.25,2.25,2.25,2.25,2.25,2.25,2.25,2.25,2.25,2.25,2.505,2.505,2.055,1.555,2.505,2.055,1.555,2.055,1.555,2.575,2.58,1.515,2.065,0.5,1.44,0.835,2.605,0.6600000000000001,1.16,2.36,0.5,2.93,2.565,2.565,2.565,2.28,0.6549999999999998,1.465],"color":["rgba(255, 111, 111, 0.3)","rgba(204, 204, 204, 0.25)","rgba(127, 179, 213, 0.3)","rgba(127, 179, 213, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(204, 204, 204, 0.25)","rgba(127, 179, 213, 0.3)","rgba(127, 179, 213, 0.3)","rgba(255, 111, 111, 0.3)","rgba(127, 179, 213, 0.3)","rgba(127, 179, 213, 0.3)","rgba(255, 111, 111, 0.3)","rgba(204, 204, 204, 0.25)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(255, 111, 111, 0.3)","rgba(127, 179, 213, 0.3)","rgba(127, 179, 213, 0.3)"],"title":["Distance: 257 characters","Distance: 617 characters","Distance: 342 characters","Distance: 554 characters","Distance: 175 characters","Distance: 150 characters","Distance: 150 characters","Distance: 150 characters","Distance: 150 characters","Distance: 150 characters","Distance: 150 characters","Distance: 150 characters","Distance: 150 characters","Distance: 150 characters","Distance: 150 characters","Distance: 150 characters","Distance: 150 characters","Distance: 150 characters","Distance: 150 characters","Distance: 150 characters","Distance: 99 characters","Distance: 99 characters","Distance: 189 characters","Distance: 289 characters","Distance: 99 characters","Distance: 189 characters","Distance: 289 characters","Distance: 189 characters","Distance: 289 characters","Distance: 85 characters","Distance: 84 characters","Distance: 297 characters","Distance: 187 characters","Distance: 702 characters","Distance: 312 characters","Distance: 433 characters","Distance: 79 characters","Distance: 468 characters","Distance: 368 characters","Distance: 128 characters","Distance: 680 characters","Distance: 14 characters","Distance: 87 characters","Distance: 87 characters","Distance: 87 characters","Distance: 144 characters","Distance: 469 characters","Distance: 307 characters"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot","physics":false,"borderWidth":1,"borderWidthSelected":2},"manipulation":{"enabled":false},"edges":{"smooth":false},"physics":{"enabled":false},"interaction":{"dragNodes":true,"dragView":true,"zoomView":true,"zoomSpeed":0.2}},"groups":["Introduction","Related work","Random forest extra information","Visualization toolkits","Size reduction","Local explanation","Rule extraction","Experimental design","Analysis"],"width":null,"height":null,"idselection":{"enabled":false,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)","highlight":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","igraphlayout":{"type":"full"},"highlight":{"enabled":true,"hoverNearest":false,"degree":1,"algorithm":"all","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":false,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"},"tooltipStay":300,"tooltipStyle":"position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;-moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);"},"evals":[],"jsHooks":[]}</script>

### Access network statistics

``` r
stats <- attr(network, "stats")

# Network size
cat("Nodes:", stats$n_nodes, "\n")
#> Nodes: 30
cat("Edges:", stats$n_edges, "\n")
#> Edges: 48
cat("Average distance:", stats$avg_distance, "characters\n")
#> Average distance: 228 characters

# Citations by section
print(stats$section_distribution)
#>                   primary_section n
#> 1                    Related work 6
#> 2                    Introduction 5
#> 3 Random forest extra information 4
#> 4                  Size reduction 4
#> 5             Experimental design 3
#> 6          Visualization toolkits 3
#> 7               Local explanation 2
#> 8                 Rule extraction 2
#> 9                        Analysis 1

# Multi-section citations
if (nrow(stats$multi_section_citations) > 0) {
  print(stats$multi_section_citations)
}
#>                 citation_text
#> 1 (Haddouchi & Berrado, 2019)
#> 2         (Meinshausen, 2010)
#> 3                (Deng, 2019)
#>                                                  sections n_sections
#> 1 Related work, Visualization toolkits, Local explanation          3
#> 2          Rule extraction, Experimental design, Analysis          3
#> 3          Rule extraction, Experimental design, Analysis          3
```

### Network Features

The citation network visualization includes:

- **Node size**: Proportional to number of connections
- **Node color**: Indicates the primary section where citations appear
- **Node border**: Thicker border (3px) for citations appearing in
  multiple sections
- **Edge thickness**: Decreases with distance (closer citations =
  thicker edges)
- **Edge color**:
  - Red: Very close citations (≤300 characters)
  - Blue: Moderate distance (≤600 characters)
  - Gray: Distant citations (\>600 characters)
- **Interactive features**: Zoom, pan, drag nodes, highlight neighbors
  on hover

### Customizing the Network

``` r
# Focus on very close citations only
network_close <- create_citation_network(
  analysis,
  max_distance = 300,
  min_connections = 1
)

# Show only highly connected citations
network_hubs <- create_citation_network(
  analysis,
  max_distance = 1000,
  min_connections = 5
)

# Hide labels for cleaner visualization
network_clean <- create_citation_network(
  analysis,
  show_labels = FALSE
)
```

## Text Analysis

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

### Citation co-occurrence data

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

## Working with references

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

## Advanced: Word distribution analysis

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

### PDF Import

- `pdf2txt_auto()`: Import PDF with automatic section detection
- `reconstruct_text_structured()`: Advanced text reconstruction

### Content Analysis

- `analyze_scientific_content()`: Comprehensive content and citation
  analysis
- `parse_references_section()`: Parse reference list
- `match_citations_to_references()`: Match citations to references

### Network Analysis

- `create_citation_network()`: Create interactive citation co-occurrence
  network

### Text Analysis

- `calculate_readability_indices()`: Compute readability scores
- `calculate_word_distribution()`: Track word frequencies across
  sections
- `readability_multiple()`: Batch readability analysis

### Visualization

- `plot_word_distribution()`: Interactive visualization of word
  distribution

### Utilities

- `get_example_paper()`: Download example paper for testing
- `extract_doi_from_pdf()`: Extract DOI from PDF metadata

## Dependencies

**Core**: pdftools, dplyr, tidyr, stringr, tidytext, tibble, httr2,
visNetwork

**Suggested**: plotly, RColorBrewer, scales (for visualization)

## Citation

If you use this package in your research, please cite:

    Massimo Aria (2025). contentanalysis: Scientific Content and Citation Analysis from PDF Documents.
    R package version 0.1.0.
    https://github.com/massimoaria/contentanalysis

## License

GPL (\>= 3)

## Issues and Contributions

Please report issues at:
<https://github.com/massimoaria/contentanalysis/issues>

Contributions are welcome! Please feel free to submit a Pull Request.
