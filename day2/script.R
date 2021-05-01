library(tidyverse)
library(stringr)
library(rlang)


options(scipen = 999)

data <- readLines("day2/input.txt")


senha <- c("1-2 n: qnhn", "2-4 p: ptw0efdgp8d")

validador <- function(senhas) {
  
  str_split(senhas, pattern = " ")  %>%
    map_dfr(
      function(.x) {
        
        tibble(
          limites = str_split(pluck(.x, 1), "-"),
          letra = str_sub(pluck(.x, 2), end = 1),
          senha = pluck(.x, 3)) %>%
        mutate(
          min = map_int(limites, ~ as.integer(parse_number(pluck(.x, 1)))),
          max = map_int(limites, ~ as.integer(parse_number(pluck(.x, 2)))),
          tag = pmap_lgl(
            list(min, max, letra, senha),
            function(min, max, letra, senha) {
              
              senha %>%
                str_count(pattern = letra) %>%
                between(min, max)
              
            }
          )
        )
      }
    ) %>%
    pull(tag)
  
}

validador(data) %>%
  keep(~ .x) %>%
  sum()


checker <- function(string, letra, n1, n2) {
  
  primeira_posição <- str_sub(string, n1, n1)
  
  segunda_posição <- str_sub(string, n2, n2)
  
  if(primeira_posição == segunda_posição) {
    
    return(primeira_posição == letra)
    
  } else {
    
    return(FALSE)
    
  }
  
  
}


validador2 <- function(senhas) {
  
  str_split(senhas, pattern = " ")  %>%
    map_dfr(
      function(.x) {
        
        tibble(
          limites = str_split(pluck(.x, 1), "-"),
          letra = str_sub(pluck(.x, 2), end = 1),
          senha = pluck(.x, 3)) %>%
          mutate(
            indice1 = map_int(limites, ~ as.integer(parse_number(pluck(.x, 1)))),
            indice2 = map_int(limites, ~ as.integer(parse_number(pluck(.x, 2)))))
      }
    ) %>%
    mutate(
      tag = map_lgl(
        list(senha, letra, indice1, indice2),
        function(senha, letra, indice1, indice2) {
          
          checker( # gambiarra para ver se era o que eu achava que seria
            string = senha,
            letra = letra,
            n1 = indice1,
            n2 = indice2
          )
        })
    )
}


validador2(data)






# 
# validador2 <- function(senhas) {
#   
#   str_split(senhas, pattern = " ")  %>%
#     map_dfr(
#       function(.x) {
#         
#         tibble(
#           limites = str_split(pluck(.x, 1), "-"),
#           letra = str_sub(pluck(.x, 2), end = 1),
#           senha = pluck(.x, 3)) %>%
#           mutate(
#             min = map_int(limites, ~ as.integer(parse_number(pluck(.x, 1)))),
#             max = map_int(limites, ~ as.integer(parse_number(pluck(.x, 2)))),
#             tag = pmap_lgl(
#               list(min, max, letra, senha),
#               function(min, max, letra, senha) {
#                 
#                 senha %>%
#                   tibble(senha = .) %>%
#                   mutate(
#                     letras = map(
#                       senha,
#                       function(.x) {
#                         
#                         tibble(
#                           letras = unlist(str_split(.x, "")),
#                           indice = 1:length(letras))  
#                       }
#                     ),
#                     numero = map_dbl(
#                       letras,
#                       function(.x) {
#                         
#                         ~ filter(
#                           .x, 
#                           indice == min | indice == max) %>%
#                           pull(letras) %>%
#                           keep(~ .x == letra) %>%
#                           length()
#                         
#                       }
#                       ),
#                     tag = numero == 2) %>%
#                   pull(tag)
#               }
#             )
#           )
#       }
#     ) %>%
#     pull(tag)
#   
# }
