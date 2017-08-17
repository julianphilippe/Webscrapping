rm(list = ls())



library(rvest)
library(dplyr)
library(stringr)
library(jsonlite)
library(tibble)




#------------------------------submarino----------------------------#


buscarProdutosSubmarino <- function(produto){
  
  
  #variável de entrada
  #produto <- 'the witcher 3'
  
  #tratamento da variável
  produto <- str_replace_all(produto, ' ', '%20')
  
  #url do site
  url <- 'https://www.submarino.com.br/busca/?conteudo='
  
  #url do site completa
  url <- paste0(url,produto)
  
  #acessar site
  webpage <- read_html(url)
  
  #selecionar dados desejados CSS
  data_html <- html_nodes(webpage, '.card-product , .value')
  
  #texto para caixa baixa
  data <- str_to_lower(html_text(data_html))
  
  #localizar paradas do JSON com estoque
  parada_json <- as.data.frame(str_locate(data, '\\}\\}\\}')) 
  
  data_json <- cbind(data, parada_json)
  
  data_json$estoque <- (case_when(is.na(data_json$end) == TRUE  ~ "nao",
                                  is.na(data_json$end) == FALSE ~ "sim"))
  
  #selecionar com estoque
  data_estoque_sim <- data_json %>%
    filter(data_json$estoque == 'sim')
  
  
  #cria tibble de saída
  listaProdutos <- tibble()
  
  
  #dados em estoque
  for (i in 1:nrow(data_estoque_sim)){
    
    #carrega dados do produto em JSON
    data_json <- fromJSON(str_sub(as.character(data_estoque_sim$data[i]), 1, data_estoque_sim$end[i]))
    
    #armazena valores em variável 
    x <- tibble(name = data_json$name,
                url = data_json$url,
                image = data_json$image,
                price = data_json$offers$price,
                stock = 'sim')
    
    #carrega dados no tibble de saída
    listaProdutos <- rbind(listaProdutos,x)
  }
  
  return(listaProdutos)
  
}
