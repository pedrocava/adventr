
library(tidyverse)

parse_input <- function(path = "day13_2021/input.txt") {

  readr::read_delim(
    file = path,
    col_names = FALSE) %>%
    purrr::set_names(c("x", "y")) %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::everything(),
        as.integer)) %>%
    dplyr::arrange(.data$x)

}


fold_x_transparency <- function(
    transparency = parse_input(),
    xfold) {

  transparency %>%
    dplyr::filter(.data$y != .env$xfold) %>%
    dplyr::mutate(
      x = dplyr::case_when(
        .data$x > xfold ~ as.integer(xfold - (.data$x - xfold)),
        .data$x < xfold ~ as.integer(.data$x)
      )) %>%
    dplyr::group_by(.data$x) %>%
    dplyr::summarise(
      y = unique(.data$y),
      .groups = "drop")

}

result <- function(firstfold = fold_x_transparency()) {

  firstfold %>%
    nrow()

}


tibble(x = 14L, y = 2L) %>%
  fold_x_transparency(xfold = 7L)


parse_input() %>%
  fold_x_transparency(xfold = 655) %>%
  result()

