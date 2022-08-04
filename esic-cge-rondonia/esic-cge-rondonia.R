library(tidyverse)
library(xml2)
library(httr2)
library(here)  

# Extrai dados da tabela inicial, aquela que tem o botão 'Detalhes"
cge_ro <- "https://esic.cge.ro.gov.br/ConsultaPublica" %>% 
  httr2::request() %>% 
  httr2::req_perform() %>% 
  httr2::resp_body_html() %>% 
  xml_find_all("//*[@id='tabelaPartial']/table/tbody/tr") %>% 
  xml_text() %>% 
  str_split(pattern = '\r\n\\s*') %>% 
  map_df(~tibble(
    protocolo = .x[1],
    categoria = .x[2],
    unidade_gestora = .x[3]
  )) %>% 
  bind_cols(
    # Extrai url do botão "Detalhes"
    cge_ro %>% 
      httr2::resp_body_html() %>% 
      xml_find_all("//*[@id='tabelaPartial']/table/tbody/tr/td/a") %>% 
      xml_attr('href') %>% 
      as_tibble_col(column_name = 'detalhes')
  )

# Acessando o botão "Detalhes" e coletando pedidos
get_dado_pedido <- function(url_detalhes) {
  url_detalhes %>% 
    paste0("https://esic.cge.ro.gov.br/", .) %>% 
    request() %>% 
    req_perform() %>% 
    resp_body_html() %>% 
    xml_find_all('//*[@id="dadoPedido"]/form/div/div/span') %>% 
    xml_text()
}

# Inclui detalhes do pedido na base principal
dado_pedido <- dado_pedido %>% 
  mutate(dado_pedido = map(detalhes, get_dado_pedido))


#dado_pedido %>% saveRDS(here("esic-cge-rondonia/data/dado_pedido.rds"))
dado_pedido <- readRDS(here("esic-cge-rondonia/data/dado_pedido.rds"))

# dados de anexos
get_url_anexo_pedido <- function(url_detalhes) {
  url_detalhes %>% 
    paste0("https://esic.cge.ro.gov.br/", .) %>% 
    request() %>% 
    req_perform() %>% 
    resp_body_html() %>% 
    xml_find_all('//*[@id="dadoPedido"]/form/div/div/table/tbody/tr/td/a') %>% 
    xml_attr('href')
}

# dados de anexos
get_nome_anexo_pedido <- function(url_detalhes) {
  url_detalhes %>% 
    paste0("https://esic.cge.ro.gov.br/", .) %>% 
    request() %>% 
    req_perform() %>% 
    resp_body_html() %>% 
    xml_find_all('//*[@id="dadoPedido"]/form/div/div/table/tbody/tr/td/a') %>%
    xml_text() %>% 
    str_squish()
}

dado_pedido <- dado_pedido %>% 
  mutate(
    url_anexo_pedido = map(detalhes, get_url_anexo_pedido),
    nome_anexo_pedido = map(detalhes, get_nome_anexo_pedido)
  )

dado_pedido %>% saveRDS(here("esic-cge-rondonia/data/dado_pedido.rds"))
dado_pedido <- readRDS(here("esic-cge-rondonia/data/dado_pedido.rds"))

# finaliza preparação da tabela
dado_pedido <- dado_pedido %>% 
  mutate(
    
    dado_pedido = map(dado_pedido, as_tibble_col, column_name = "raw"),
    dado_pedido = map(dado_pedido, mutate, coluna = case_when(
      raw == "Protocolo" ~ raw,
      raw == "Situação" ~ raw,
      raw == "Órgão supervisor" ~ raw,
      raw == "Órgão responsável" ~ raw,
      raw == "Data de abertura" ~ raw,
      raw == "Data da cientificação oficial" ~ raw,
      raw == "Prazo para atendimento" ~ raw,
      raw == "Forma de recebimento da resposta" ~ raw,
      raw == "Classificação do pedido" ~ raw,
      raw == "Aplicação da informação" ~ raw,
      raw == "Categoria" ~ raw,
      raw == "Subcategoria" ~ raw,
      raw == "Descrição da solicitação" ~ raw,                                
      TRUE ~ NA_character_
    )),
    dado_pedido = map(dado_pedido, fill, coluna, .direction = "down"),
    dado_pedido = map(dado_pedido, filter, raw != coluna, raw != "Anexos"),
    dado_pedido = map(dado_pedido, pivot_wider, names_from = coluna, values_from = raw),
    
    nome_anexo_pedido = map(nome_anexo_pedido, as_tibble_col, column_name = "nome_anexo_pedido"),
    nome_anexo_pedido = map(nome_anexo_pedido, mutate, id = str_glue("nome_anexo_{row_number()}")),
    nome_anexo_pedido = map(nome_anexo_pedido, pivot_wider, values_from = "nome_anexo_pedido", names_from = id),
    
    url_anexo_pedido = map(url_anexo_pedido, as_tibble_col, column_name = "url_anexo_pedido"),
    url_anexo_pedido = map(url_anexo_pedido, mutate, id = str_glue("url_anexo_{row_number()}")),
    url_anexo_pedido = map(url_anexo_pedido, pivot_wider, values_from = "url_anexo_pedido", names_from = id),
    
  ) %>% 
  unnest(cols = c(dado_pedido, nome_anexo_pedido, url_anexo_pedido))

dado_pedido
