
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cleanstart

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

During data cleaning for a survey, analysts typically must create
countless scripts to inspect and clean every variable in each survey
data files. To a large degree, this involves:

- **Creating** an empty cleaning program for a given scope of cleaning
  (per data file, survey module, analyst area of expertise, etc.)
- **Copying** information for each variable from Survey Solutions
  Designer (e.g., enablement conditions, validation conditions, etc from
  Survey Solutions Designer)
- **Pasting** information into the empty Stata file in a consistent,
  readable format
- **Transforming** copied information following known rules/procedures
  (e.g., translate Survey Solutions’ expressions into Stata, write the
  same form of check(s) for variables of a given question type, etc.)
- **Repeating** (ad naseaum) for each desired cleaning program

Convinced that computers are better at copy-paste-transform operations
than humans, `{cleanstart}` provides analysts an interactive graphical
for creating a template cleaning program. From there, the analyst can
dedicate their time, skill, and judgment to a task that computers aren’t
(yet) good at doing: cleaning data.

To get started, the analyst simply:

- Installs the package–a one-time operation
- Downloads a JSON file containing questionnaire metadata–a
  one-time-per-survey operation.
- Opens the app’s graphical interface
- Selects the range of variables to clean (e.g., from `s02q01` to
  `s02q31a`)
- Provides optional information on desired customizations (e.g., replace
  `@rowcode` with `members__id`, identify “other (specify)” variables as
  those ending in `_os`, etc.)
- Provides a file name for the Stata .do file
- Downloads the Stata .do file template

## Installation

Since susometa is not yet available on CRAN, it can be installed from
GitHub as follows:

``` r
if (!require("pak")) install.packages("pak")
pak::pak("lsms-worldbank/cleanstart")
```

## Usage

To open the app’s graphical interface, simply run this command in the R
console:

``` r
run_app()
```
