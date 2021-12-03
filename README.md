# stcEQDT

This project contains the source code of the R package **stcEQDT**,
which implements functionalities to visualize the questionnaire structure
encoded in an EQDT JSON file.

# Documentation

* [Installing stcEQDT](#installing-stcEQDT)
* [Demo -- json2tree](#demo-stcEQDT-json2tree)

## Installing stcEQDT

Paste the following into R and run it:

```r
# Install the dependencies
install.packages(c("R6", "dplyr", "rjson"), repos="https://artifactory.statcan.ca:8443/artifactory/cran")

# Install from GitLab
install.packages("https://gitlab.statcan.ca/dscd-methods-quality/projects/2021-2022/stcEQDT/-/archive/master/stcEQDT-master.tar.gz", repos = NULL, type = "source")
```

## Demo -- json2tree

Consult the vignette __json2tree-usage__ to see how to use the function
__stcEQDT::json2tree(.)__ to visualize a given input EQDT JSON file
to tree format and save the result to file, by

1.  downloading __vignettes/json2tree-usage.html__ and open the downloaded file in a web browser, or
1.  executing the command __browseVignettes('stcEQDT')__ at the R console (after __stcEQDT__ has been installed).
