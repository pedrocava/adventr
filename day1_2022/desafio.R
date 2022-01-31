library(tidyverse)
library(magrittr)

input <- function() {

  "day1_2022/input.txt" %>%
    readr::read_lines() %>%
    readr::parse_number()

}


processamento <- function(input) {

  tibble::tibble(
    input = input) %>%
    dplyr::mutate(
      lag = dplyr::lag(.data$input),
      diff = .data$input - .data$lag,
      is_increment = .data$diff > 0)

}

resposta <- function(processing) {

  processing %$%
    sum(is_increment, na.rm = TRUE)

}


input() %>%
  processamento() %>%
  resposta()

