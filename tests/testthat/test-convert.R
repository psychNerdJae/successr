test_that(
  "`convert_adjlist_to_matrix` fails if dataframe is not given",
  {
    expect_error(
      convert_adjlist_to_matrix(
        matrix(),
        edge
      )
    )
  }
)

test_that(
  "`convert_adjlist_to_matrix` fails if `from` is missing",
  {
    expect_error(
      convert_adjlist_to_matrix(
        for_testing_convert %>% dplyr::select(-from),
        edge
      )
    )
  }
)

test_that(
  "`convert_adjlist_to_matrix` fails if `to` is missing",
  {
    expect_error(
      convert_adjlist_to_matrix(
        for_testing_convert %>% dplyr::select(-to),
        edge
      )
    )
  }
)

test_that(
  "`convert_adjlist_to_matrix` fails if `from` contains non-numbers",
  {
    expect_error(
      convert_adjlist_to_matrix(
        for_testing_convert %>% dplyr::mutate(from = as.character(from)),
        edge
      )
    )
  }
)

test_that(
  "`convert_adjlist_to_matrix` fails if `to` contains non-numbers",
  {
    expect_error(
      convert_adjlist_to_matrix(
        for_testing_convert %>% dplyr::mutate(to = as.character(to)),
        edge
      )
    )
  }
)

test_that(
  "`convert_adjlist_to_matrix` fails if relation value column is misspecified",
  {
    expect_error(
      convert_adjlist_to_matrix(
        for_testing_convert,
        asdf
      )
    )
  }
)

test_that(
  "`convert_adjlist_to_matrix` fails if relation values contain non-numbers",
  {
    expect_error(
      convert_adjlist_to_matrix(
        for_testing_convert %>% dplyr::mutate(edge = as.character(edge)),
        edge
      )
    )
  }
)

test_that(
  "`convert_adjlist_to_matrix` fails if adjlist is the wrong size",
  {
    expect_error(
      convert_adjlist_to_matrix(
        for_testing_convert %>% dplyr::filter(from != to),
        edge
      )
    )
  }
)

test_that(
  "`convert_matrix_to_adjlist` returns the correct adjlist",
  {
    expect_equal(
      object = for_testing_convert %>%
        convert_adjlist_to_matrix(edge) %>%
        convert_matrix_to_adjlist(edge),
      expected = dplyr::tribble(
        ~from, ~to, ~edge,
        1, 1, 0,
        1, 2, 2,
        2, 1, 1,
        2, 2, 3
      )
    )
  }
)

