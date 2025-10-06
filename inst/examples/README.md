# Example Papers

This folder contains example papers for testing and demonstration.

## example_paper.pdf
- Title: A comparison among interpretative proposals for Random Forests
- DOI: 10.1016/j.mlwa.2021.100094
- License: Open Access (CC BY)
- Source: https://doi.org/10.1016/j.mlwa.2021.100094

## Usage

From GitHub repository (developers):
```r
paper_path <- system.file('examples', 'example_paper.pdf', package = 'contentanalysis')
```

From installed package (users):
```r
paper_path <- get_example_paper()
```

## Note
This folder is included in the GitHub repository but excluded from CRAN builds to save space.
The `get_example_paper()` function will automatically download the file from GitHub if not found locally.
