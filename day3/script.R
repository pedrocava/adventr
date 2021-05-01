library(tidyverse)
library(stringr)
library(rlang)


options(scipen = 999)

data <- readLines("day3/input.txt")

repeated_data <- map_chr(
    data,
    ~ rep(.x, 50) %>%
      reduce(paste, sep = ""))


tibble(repeated_data) %>%
  mutate(
    index = 1:length(repeated_data),
    posicao = (index - 1) * 3 + 1,
    tem_arvore = map2_lgl(
      .x = repeated_data,
      .y = posicao,
      ~ str_sub(.x, start = .y, end = .y) == "#"
    )
  ) %>%
  pull(tem_arvore) %>%
  sum()
  

