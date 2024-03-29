<img src="man/figures/bbplot.png" alt="hexagon software logo, white background with black outline and a black L shaped plot border and the text bbplot in computer code typeface" width="200px" align="right">

# bbplot 0.0.1

[![R Build](https://github.com/dapperstats/bbplot/actions/workflows/r.yml/badge.svg)](https://github.com/dapperstats/bbplot/actions/workflows/r.yml)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/dapperstats/bbplot/master/LICENSE)
[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test coverage](https://img.shields.io/codecov/c/github/dapperstats/bbplot/master.svg)](https://codecov.io/github/dapperstats/bbplot/branch/master)

`bbplot` extends base R plotting for [bbs](https://www.computerhope.com/jargon/b/bb.htm) (an endearing shorthand for "baby" as in "ty bb" for "thank you, baby").

It is a set of functions written to provide additional flexibility and power to R graphics written using the base R plotting functionality provided by the `graphics` and `grDevices` packages loaded by default with R.

The code is designed to specifically mimic the base plotting functions in API (e.g., have the same variable inputs named the same and in the same order) as well as in function-level syntax. 

We follow the *painters model* of R graphics, where output production occurs in steps that are placed with subsequent steps being layered on top of earlier steps. 
We seek to extend the classical painter model to allow for removal of earlier steps, although this is not yet implemented. 

A formal written definition of the code and design aesthetics is forthcoming.

## Status: Experimental, Active Development

The `bbplot` package is currently being actively developed, and is an early stage of package construction from disparate function code.

## Installation

You can install the R package from github:

```r
install.packages("devtools")
devtools::install_github("dapperstats/bbplot")
```

## Author Contributions

**J. L. Simonis** is the main author of `bbplot`.  
**M. Fidino** is a recent addition to the team.

If you are interested in contributing, see the [Contributor Guidelines](https://github.com/dapperstats/bbplot/blob/master/CONTRIBUTING.md).
