library(tidyverse)
library(magrittr)
library(logger)
library(rlang)
library(glue)

parse_input <- function(path = "day6_2022/input.txt") {

  readr::read_lines(path) %>%
    stringr::str_split(pattern = ",") %>%
    purrr::pluck(1) %>%
    rlang::as_list() %>%
    purrr::map(as.integer) %>%
    rlang::set_attrs(iteration = 0)

  }

reproduction <- function(fishes, n_max) {

  fishes %>%
    attributes() %>%
    purrr::pluck("iteration") ->
    iteration_n

  logger::log_info("Iteration {iteration_n}")

  fishes %>%
    purrr::keep(~ .x == 0L) %>%
    length() ->
    n_new_fishes

  rep(
    list(8),
    times = n_new_fishes) ->
    smol_fishes

  fishes %>%
    purrr::map(function(fish) {

      if (fish > 0) {

        return(fish - 1L)

      } else if (fish == 0L) {

        return(6)

      }

    }) %>%
    c(smol_fishes) %>%
    rlang::set_attrs(iteration = iteration_n + 1) ->
    new_fishes

  if (iteration_n < n_max) {

    new_fishes %>%
      reproduction(n_max) %>%
      return()

  } else if (iteration_n == n_max) {

    return(fishes)

  }

}

result <- function(fishes) {

  length(fishes)

}

parse_input() %>%
  reproduction(n_max = 80) %>%
  result()

parse_input() %>%
  reproduction(n_max = 256) %>%
  result()

# ---


parse_input2 <- function(path = "day6_2022/input.txt") {

  readr::read_lines(path) %>%
    stringr::str_split(pattern = ",") %>%
    purrr::pluck(1) %>%
    as.integer() %>%
    tibble::tibble(fishes = .) %>%
    dplyr::group_by(.data$fishes) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    rlang::set_attrs(iteration = 0)

}

reproduction2 <- function(fishes, n_max) {

  fishes %>%
    attributes() %>%
    purrr::pluck("iteration") ->
    iteration_n

  fishes %>%
    dplyr::filter(.data$fishes == 0) %$%
    count ->
    smol_fishes

  logger::log_info("Iteration {iteration_n}")

  tibble::tibble(
    old_fishes = 0:8,
    fishes = dplyr::if_else(
      condition = .data$old_fishes - 1 >= 0,
      true = .data$old_fishes - 1,
      false = 6)) %>%
    dplyr::left_join(
      fishes,
      by = c("old_fishes" = "fishes")) %>%
    dplyr::arrange(.data$fishes) %>%
    dplyr::select(- .data$old_fishes) %>%
    dplyr::bind_rows(
      tibble::tibble(
        fishes = 8,
        count = smol_fishes)) %>%
    dplyr::group_by(.data$fishes) %>%
    dplyr::summarise(
      count = sum(.data$count, na.rm = TRUE),
      .groups = "drop") %>%
    rlang::set_attrs(iteration = iteration_n + 1) ->
    new_fishes

  if (iteration_n < n_max) {

    new_fishes %>%
      reproduction2(n_max) %>%
      return()

  } else if (iteration_n == n_max) {

    return(fishes)

  }

}

result2 <- function(fishes) {

  fishes %>%
    dplyr::pull(.data$count) %>%
    purrr::reduce(`+`)

}

parse_input2() %>%
  reproduction2(n_max = 256) %>%
  result2()
