library(tidyverse)
library(magrittr)

result <- function() {

  "day4_2022/input.txt" %>%
    readr::read_lines(n_max = 1) %>%
    stringr::str_split(pattern = ",") %>%
    purrr::pluck(1) %>%
    readr::parse_number()

}

cartoes <- function() {

  "day4_2022/input.txt" %>%
    readr::read_lines(skip = 1) %>%
    tibble::tibble(input = .) %>%
    dplyr::mutate(
      empty_tag = input == "",
      cartao = sort(rep(1:(dplyr::n() / 6), times = 6))) %>%
    dplyr::filter(! input == "") %>%
    dplyr::group_by(cartao) %>%
    dplyr::group_split() %>%
    purrr::map(function(entry) {

      entry %>%
        dplyr::pull(input)

      }) %>%
    purrr::map(paste0, collapse = " ") %>%
    purrr::map(leitora_cartoes)

}

leitora_cartoes <- function(cartao = ? typed::Character()) {

  cartao %>%
    stringr::str_split(pattern = " ") %>%
    purrr::pluck(1) %>%
    purrr::discard(~ .x == "") %>%
    readr::parse_double() %>%
    matrix(nrow = 5, ncol = 5) ->
    cartao_as_matrix

  tidyr::expand_grid(
    row = 1:5,
    column = 1:5) %>%
    dplyr::mutate(
      value = purrr::map2_dbl(
        .x = row,
        .y = column,
        .f = ~ cartao_as_matrix[.x, .y]),
      drawn = FALSE)

}

sorteio <- function(board, drawn_number) {

  board %>%
    dplyr::mutate(
      drawn = dplyr::if_else(
        condition = .data$value == drawn_number,
        true = TRUE,
        false = .data$drawn))

}

score_cartao <- function(board, drawn_numbers) {

  for (i in 1:length(drawn_numbers)) {

    board <- sorteio(board, drawn_numbers[i])

    check <- is_bingo(board)

    if (check) {

      break()

    }

  }

  list(
    board = board,
    n_bingo = i,
    score = board %>%
      dplyr::filter(! drawn) %$%
      sum(value) * drawn_numbers[i])

}

is_bingo <- function(board) {

  board %>%
    dplyr::group_by(column) %>%
    dplyr::summarise(complete = purrr::every(drawn, ~ .x)) ->
    colunas

  board %>%
    dplyr::group_by(row) %>%
    dplyr::summarise(complete = purrr::every(drawn, ~ .x)) ->
    linhas

  dplyr::bind_rows(colunas, linhas) %$%
    purrr::some(complete, ~ .x)

}


"64 98 67 54 75  8 10 31  5 57 89 23 25 34 47 72 74 37 48 94 39 59 15 55 87" %>%
  leitora_cartoes() %>%
  score_cartao(result())


tibble::tibble(
  boards = cartoes(),
  results = purrr::map(
    .x = boards,
    .f = score_cartao,
    drawn_numbers = result())) %>%
  dplyr::mutate(
    n_bingo = purrr::map_int(results, ~ .x$n_bingo),
    score = purrr::map_dbl(results, ~ .x$score))

results %>%
  dplyr::group_by(n_bingo) %>%
  dplyr::summarise(score_max = max(score, na.rm = TRUE))
