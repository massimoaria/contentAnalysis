# Example Papers

This folder contains example papers for testing and demonstration.

## example_paper.pdf
- Title: Identifying and prioritizing health research funding agencies globally
- DOI: 10.1016/j.healthpol.2022.05.004
- License: Open Access (CC BY)
- Source: https://www.sciencedirect.com/science/article/pii/S266682702200113X

## Usage

From GitHub repository (developers):
```r
paper_path <- system.file('examples', 'example_paper.pdf', package = 'contentAnalysis')
```

From installed package (users):
```r
paper_path <- get_example_paper()
```

## Note
This folder is included in the GitHub repository but excluded from CRAN builds to save space.
The `get_example_paper()` function will automatically download the file from GitHub if not found locally.
