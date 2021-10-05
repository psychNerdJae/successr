# successr

## Overview

R package for modeling Successor algorithms in reinforcement learning (i.e., Successor Representation, Successor Features).

Graphs, matrices, and dataframes become situationally useful for different plotting/wrangling operations. This package makes it easy to convert between different representations of networks (and successor values).

As of now, this package is very much still in development mode. Please don't hesitate to contact me or start a new issue if you find errors. This is my first package (!!), so I'm sure that I've bungled some things here and there...

## Installation

```
# Install straight from GitHub
# install.packages("devtools")
devtools::install_github("psychNerdJae/successr")
```

## Notes

Builds on top of functions from `tidyverse`, `igraph`, and `tidygraph`.

The karate club network data is sourced from `igraphdata`.

