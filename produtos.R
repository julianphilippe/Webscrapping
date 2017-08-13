rm(list = ls())



library(rvest)
library(dplyr)
library(stringr)
library(jsonlite)
library(tibble)




#------------------------------submarino----------------------------#


buscarProdutosSubmarino <- function(produto){
  
  
  #variável de entrada
  produto <- 'the last of us'
  
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
  
  #localizar paradas do JSON
  parada_json <- as.data.frame(str_locate(data, '\\}\\}\\}')) 
  
  #excluir NAs
  parada_json <- na.omit(parada_json)
  
  #cria tibble de saída
  listaProdutos <- tibble()
  
  
  for (i in 1:nrow(parada_json)){
    
    #carrega dados do produto em JSON
    data_json <- fromJSON(str_sub(data[i], 1, parada_json$end[i]))
    
    #armazena valores em variável 
    x <- tibble(name = data_json$name,
                url = data_json$url,
                image = data_json$image,
                price = data_json$offers$price)
    
    #carrega dados no tibble de saída
    listaProdutos <- rbind(listaProdutos,x)
    
    
  }
  
  return(listaProdutos)
  
}





teste <- buscarProdutosSubmarino('the last of us')
