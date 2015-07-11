GEOsearch: Extendable Search Engine for Gene Expression Omnibus
====

## Overview
GEOsearch is an extendable search engine for NCBI GEO (Gene Expression Omnibus). Instead of directly searching the term, GEOsearch can find all the gene names contained in the search term and search all the alias of the gene names simultaneously in GEO database. GEOsearch also provides other functions such as summarizing common biology keywords in the search results.

## GEOsearch Online User Interface
GEOsearch user interface can be directly launched online without installing any software package: https://zhiji.shinyapps.io/GEOsearch. Users can also install GEOsearch on their own computers with following procedures.

## GEOsearch Installation

GEOsearch software can be installed via Github. 
Users should have R installed on their computer before installing GSCA. R can be downloaded here: http://www.r-project.org/.
To install the latest version of GSCA package via Github, run following commands in R:
```{r }
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("GEOsearch","zji90")
```
To launch user interface after installation, run following commands in R:
```{r }
library(GEOsearch)
GEOsearchui()
```
For users with R programming experience, command line tools are also available in GEOsearch R package. Please check the manual package included in the package for details.

## Contact the Author
Author: Zhicheng Ji, Hongkai Ji

Report bugs and provide suggestions by sending email to:

Maintainer: Zhicheng Ji (zji4@jhu.edu)

Or open a new issue in this Github page
