# Get Started

## Introduction

As this is a package extension, if you are new to
[admiral](https://pharmaverse.github.io/admiral/) then the best place to
first start reading would be the [Get
Started](https://pharmaverse.github.io/admiral/articles/admiral.html)
page. This extension package follows the same main idea and conventions,
and re-uses many functions from
[admiral](https://pharmaverse.github.io/admiral/), so it is important to
thoroughly understand these to be able to use
[admiralmetabolic](https://pharmaverse.github.io/admiralmetabolic/).

## Derivations

The most important functions in
[admiralmetabolic](https://pharmaverse.github.io/admiralmetabolic/) are
the
[derivations](https://pharmaverse.github.io/admiralmetabolic/reference/index.html).
Again these follow the same conventions as
[admiral](https://pharmaverse.github.io/admiral/) but are focused to
metabolism-specific needs.

## Creating ADaM Datasets

For the metabolic ADaM data structures, an overview of the flow and
example function calls for the most common steps are provided by the
following vignettes:

- [Creating a Control of Eating Questionnaire
  ADaM](https://pharmaverse.github.io/admiralmetabolic/articles/adcoeq.md)
- [Creating a Metabolic ADVS
  ADaM](https://pharmaverse.github.io/admiralmetabolic/articles/advs.md)

[admiralmetabolic](https://pharmaverse.github.io/admiralmetabolic/) also
provides template R scripts as a starting point. They can be created by
calling
[`use_ad_template()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/use_ad_template.html)
from [admiral](https://pharmaverse.github.io/admiral/), e.g.,

``` r
library(admiral)

use_ad_template(
  adam_name = "advs",
  save_path = "./ad_advs.R",
  package = "admiralmetabolic"
)
```

A list of all available templates from
[admiralmetabolic](https://pharmaverse.github.io/admiralmetabolic/) can
be obtained by
[`list_all_templates()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/list_all_templates.html)
from [admiral](https://pharmaverse.github.io/admiral/):

``` r
list_all_templates(package = "admiralmetabolic")
```

## Support

Support is provided via [pharmaverse
Slack](https://pharmaverse.slack.com/). Additionally, please feel free
to raise issues in our [GitHub
repository](https://github.com/pharmaverse/admiralmetabolic/issues).
