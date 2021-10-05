% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_successor.R
\name{update_successor}
\alias{update_successor}
\title{Update successor values}
\usage{
update_successor(
  input_matrix,
  alpha,
  gamma,
  previous_state,
  current_state,
  bidirectional = FALSE
)
}
\arguments{
\item{input_matrix}{A square NxN matrix created by `initialize_successor`.}

\item{alpha}{Scalar corresponding to the learning rate bound in [0, 1].}

\item{gamma}{Scalar corresponding to the lookaround horizon bound in [0, 1).}

\item{previous_state}{Scalar corresponding to the previously-seen state.}

\item{current_state}{Scalar corresponding to the currently-seen state.}

\item{bidirectional}{Logical. Defaults to `FALSE`, which means that only
the `from-to` relationship gets updated. If set to `TRUE`, this function
will also update the `to-from` relationship.}
}
\value{
A matrix with updated successor values, given the observation.
}
\description{
After observing a relation between two nodes (i.e., transitioning between
two states), update the successor values.
}
\examples{
`\%>\%` <- magrittr::`\%>\%`
successr::karate \%>\%
    tidygraph::as_tbl_graph(directed = F) \%>\%
    initialize_successor() \%>\%
    update_successor(0.1, 0.4, 1, 2, TRUE) \%>\%
    matrix_to_adjlist()

}