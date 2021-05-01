library(tidyverse)
library(rlang)

options(scipen = 999)

(data <- readLines("day1/input.txt"))


day1 <- function(vec, alvo = 2020) {
  
  vec1 <- vec2 <- vec
  
  expand_grid(vec1, vec2) %>%
    filter(! vec1 == vec2) %>%
    mutate(
      across(everything(), parse_number),
      soma = map2_dbl(vec1, vec2, ~ .x + .y)) %>%
    filter(soma == alvo) %>%
    pull(vec1, vec2) %>%
    unname() %>%
    reduce(`*`)
    
}


day1(data, alvo = 2020)  


extra <- function(vec, alvo = 2020) {
  
  vec1 <- vec2 <- vec3 <- vec
  
  expand_grid(vec1, vec2, vec3) %>%
    filter(! vec1 == vec2, ! vec2 == vec3, ! vec1 == vec3) %>%
    mutate(
      across(everything(), parse_number),
      soma = 
        pmap_dbl(
          list(vec1, vec2, vec3),
          function(vec1, vec2, vec3) {
            
            sum(vec1, vec2, vec3)
            
          }
        )) %>%
    filter(soma == alvo) %>%
    pull(1) %>%
    unique() %>%
    reduce(`*`)
  
}

extra(data, alvo = 2020)  
