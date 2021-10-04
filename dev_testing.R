# These all get imported anyways, so easier to develop with them in namespace
library(dplyr)
library(tidyr)
library(igraph)
library(tidygraph)
library(devtools)

# Reading in the karate data
# karate <- tidygraph::as_tbl_graph(
#   readr::read_csv(here::here("data", "karate_edgelist.csv")),
#   directed = FALSE
# )
karate <- tidygraph::as_tbl_graph(successr::karate, directed = F)

# Test whether the package works
test <- graph_to_adjlist(karate)
