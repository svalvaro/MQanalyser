
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MQanalyser

<!-- badges: start -->
<!-- badges: end -->

The goal of MQanalyser is to provide the user an interface to
interactively analyse their Proteomics LC-MS/MS output.

## Installation

You can install it from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("svalvaro/MQanalyser")
```

## Usage

You can access to the web application through:
<https://proteomics.fgu.cas.cz/ProteoAnalyser/>

## Example

To run it locally:

``` r
library(MQanalyser)

appDir <- system.file("shiny_app", package = "MQanalyser")
shiny::runApp(appDir)
```

This is how it should look like when you have uploaded your
proteinGroups.txt and the experiment design. Just press Start Analysis…
and Enjoy!

![MQanalyser interface
example](https://github.com/svalvaro/MQanalyser/blob/master/inst/shiny_app/www/images/shinyApp.png?raw=true)
