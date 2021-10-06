# successr

## Overview

R package for modeling Successor algorithms in reinforcement learning (i.e., Successor Representation, Successor Features).

Graphs, matrices, and dataframes become situationally useful for different plotting/wrangling operations. This package makes it easy to convert between different representations of networks (and successor values).

As of now, this package is very much still in development mode. Please don't hesitate to contact me or start a new issue if you find errors. This is my first package (!!), so I'm sure that I've bungled some things here and there...

## Further reading

1. [Russek, Momennejad, Botvinick, Gershman, & Daw, 2017](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005768)
2. [Momennejad, 2020](https://www.sciencedirect.com/science/article/pii/S2352154620300371)
3. [Lehnert, Littman, & Frank, 2020](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008317)

## Installation

```
# Install straight from GitHub
# install.packages("devtools")
devtools::install_github("psychNerdJae/successr")
```

## Example

```
library(tidyverse)
library(tidygraph)
library(successr)

karate_network <- karate %>%
  as_tbl_graph(directed = FALSE)

# Converting between different network representations
karate_network %>%
  graph_to_adjlist() %>%
  adjlist_to_matrix() %>%
  matrix_to_adjlist()

# Simulate random walk through network (observations)
karate_walk <- karate_network %>%
  generate_random_walk(n_obs = 5000)

# Learn successor values through observation
karate_network %>%
  initialize_successor() %>%
  learn_from_observations(observations = karate_walk,
                          input_alpha = 0.1,
                          input_gamma = 0.4,
                          bidirectional = TRUE)

# Learn successor values using many parameter values
karate_successor <- karate_network %>%
  initialize_successor() %>%
  learn_from_param_sweep(observations = karate_walk,
                         alphas = c(0.1, 0.3),
                         gammas = seq(0, 0.8, 0.4),
                         bidirectional = TRUE)

# Convert counts to probabilities (or vice-versa)
karate_successor %>%
  counts_to_probability(successor_value) %>%
  probability_to_counts(successor_value)

# Plot results using ggplot
karate_successor %>%
  mutate(across(.cols = from:to, .fns = factor),
         from = fct_rev(from)) %>%
  mutate(alpha = str_c("alpha=", alpha),
         gamma = str_c("gamma=", gamma)) %>%
  ggplot(aes(x = to, y = from, fill = successor_value)) +
  facet_grid(rows = vars(alpha), cols = vars(gamma)) +
  geom_tile() +
  scale_fill_viridis_c() +
  coord_fixed() +
  theme(legend.position = "bottom",
        axis.text = element_blank())

```

