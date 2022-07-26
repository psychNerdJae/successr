# Development
library(devtools)
library(testthat)
# library(rlang)

# All of the functions that get added to the namespace
library(dplyr)
library(tidyr)

# Reading in the karate data
load(here::here("data", "karate.rda"))

karate <- karate %>%
  tidygraph::tbl_graph(
    edges = .,
    directed = FALSE
  )

# Load package as-is
load_all()
