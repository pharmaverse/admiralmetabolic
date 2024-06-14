# admiralmetabolic <img src="man/figures/logo.png" align="right" width="200" style="margin-left:50px;"/>

Metabolism extension package for ADaM in R Asset Library `{admiral}`

## Purpose

To provide a complementary (to `{admiral}`) toolbox that enables users
to develop specifics for metabolic clinical trials.

## Installation

To install the latest development version of the package directly from
GitHub use the following code:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("pharmaverse/admiraldev") # This is a required dependency of {admiralmetabolic}
remotes::install_github("pharmaverse/admiral") # This is a required dependency of {admiralmetabolic}
remotes::install_github("pharmaverse/admiralmetabolic")
```

### Dependencies

The latest version of the package works with the latest versions of the
packages stated in `DESCRIPTION`.

If a previous version of the package should be used, it is recommended
to use latest version of the dependencies at the point of time when the
previous version of `{admiralmetabolic}` was released.

## Scope

-   Build a toolbox of re-usable functions and utilities to create
    metabolism-specific ADaM datasets in R in a modular manner.
-   All functions are created based upon the ADaM Implementation Guide
    and aim to facilitate the programming of ADaM dataset standards.
-   Initially the package will focus on <insert focus>.

## Expectations

`{admiralmetabolic}` is expected to complement `{admiral}` and provide
functions to help with the creation of analyses required
for metabolic trial ADaMs.

## References and Documentation

-   Please refer to the [References and
    Documentation](https://pharmaverse.github.io/admiral/index.html#references-and-documentation)

## R Versions

Here’s a summary of our strategy for this package related to R versions:

-   R versions for developers and users will follow the same as
    `{admiral}` core package.
-   For development the `main` branch of `{admiral}` core is used as a
    dependency. For releasing a new `{admiralmetabolic}` version it must run
    using the latest released `{admiral}` core version.

## Contact

We use the following for support and communications between user and
developer community:

-   [Slack](https://pharmaverse.slack.com/) - for
    informal discussions, Q&A and building our user community. If you
    don't have access, use this
    [link](https://join.slack.com/t/pharmaverse/shared_invite/zt-yv5atkr4-Np2ytJ6W_QKz_4Olo7Jo9A)
    to join the pharmaverse Slack workspace
-   [GitHub Issues](https://github.com/pharmaverse/admiralmetabolic/issues) -
    for direct feedback, enhancement requests or raising bugs
