#' @title Zachary's karate club network
#'
#' @description
#' The famous "karate club" network studied by Wayne Zachary. Only contains an
#' unweighted edgelist. Node #1 is "Mr. Hi" and node #34 is "John A".
#'
#' @format A tibble with 78 rows and 2 variables:
#' \describe{
#'   \item{from}{Person #1 in a given dyad}
#'   \item{to}{Person #1 in the same dyad}
#'   ...
#' }
#' @source \url{https://www.rdocumentation.org/packages/igraphdata/versions/1.0.1/topics/karate}
"karate_numeric"
"karate_named"
