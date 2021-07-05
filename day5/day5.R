
exemplo <- "FBFBBFFRLR"

f <- function(range) {

  c(range[1], ((range[2] - range[1]) %/% 2) + range[1])

}


b <- function(range) {

  c(((range[2] - range[1]) %/% 2) + 1 + range[1], range[2])

}


fileira <- function(assento) {

   stringr::str_split(assento, "") %>%
    purrr::pluck(1) %>%
    purrr::discard(~ .x %in% c("R", "L")) ->
    assento_processado

  tibble::tibble(
    entradas = assento_processado,
    codigo = purrr::map_chr(
      entradas,
      ~ ifelse(.x == "B",
      " %>% b() ",
      " %>% f() "))) %>%
    pull(codigo) %>%
    c("c(0, 127)", .) %>%
    purrr::accumulate(paste, sep = "") %>%
    purrr::map(rlang::parse_expr) %>%
    purrr::map(rlang::eval_bare) %>%
    dplyr::last() %>%
    purrr::pluck(1)

}


l <- function(range) {

  c(range[1], ((range[2] - range[1]) %/% 2) + range[1])

}


r <- function(range) {

  c(((range[2] - range[1]) %/% 2) + 1 + range[1], range[2])

}


coluna <- function(assento) {

  stringr::str_split(assento, "") %>%
    purrr::pluck(1) %>%
    purrr::discard(~ .x %in% c("F", "B")) ->
    assento_processado

  tibble::tibble(
    entradas = assento_processado,
    codigo = purrr::map_chr(
      entradas,
      ~ ifelse(.x == "R",
               " %>% r() ",
               " %>% l() "))) %>%
    pull(codigo) %>%
    c("c(0, 7)", .) %>%
    purrr::accumulate(paste, sep = "") %>%
    purrr::map(rlang::parse_expr) %>%
    purrr::map(rlang::eval_bare) %>%
    dplyr::last() %>%
    purrr::pluck(1)

}


id_assento <- function(fileira, coluna) {

  8*fileira + coluna

}


day5 <- function(input = "day5/input.txt") {

    tibble::tibble(
      cartao = readr::read_lines(input),
      fileira = purrr::map_dbl(cartao, fileira),
      coluna = purrr::map_dbl(cartao, coluna),
      id = purrr::map2_dbl(
        .x = fileira,
        .y = coluna,
        .f = id_assento)) %>%
    dplyr::filter(id == max(id))

}

day5()
