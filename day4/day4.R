library(tidyverse)

input <- readr::read_lines("day4/input.txt")

exemplo <- " cid:274 pid:390115952 byr:1934 hgt:161cm iyr:2017 hcl:#b95b0d"

processadora <- function(string) {

  stringr::str_split(string, pattern = " ") %>%
    purrr::pluck(1) %>%
    purrr::discard(~ .x == "") %>%
    purrr::map(
      function(str) {

        stringr::str_split(str, pattern = ":") %>%
          purrr::pluck(1) }) %>%
    purrr::transpose() ->
    conteudo

  chaves <- conteudo %>%
    purrr::pluck(1) %>%
    unlist()

  valores <- conteudo %>%
    purrr::pluck(2) %>%
    purrr::set_names(chaves)

  tibble::as_tibble(valores)

}

processadora(exemplo)

validadora <- function(passaporte) {

  campos_necessarios <- c(
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid")

  purrr::every(
    .x = campos_necessarios,
    .f = ~ .x %in% names(passaporte))

}

exemplo %>%
  processadora() %>%
  validadora()

day4 <- function(input = "day4/input.txt") {

  tibble::tibble(
    input = readr::read_lines(input),
    input_tratado = purrr::accumulate(
      input,
      ~ ifelse(
        .y == "",
        "",
        paste(.x, .y, sep = " "))),
    tag_linha = input_tratado == "",
    lag_tag = dplyr::lead(tag_linha)) %>%
    filter(lag_tag) %>%
    pull(input_tratado) %>%
    purrr::map(processadora) %>%
    purrr::keep(validadora) %>%
    length()

  }

day4()






