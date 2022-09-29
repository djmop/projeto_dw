box::use(
  dplyr[across, left_join, select, arrange, mutate, n, relocate],
  lubridate[hour, minute, second, date, is.POSIXct],
  stringr[str_remove_all, str_pad]
)

box::use(G.FUNC = ../../../globals/Z4_global_functions)
box::use(G.LOG  = ../../../globals/Z2_global_logging)


#  DIM: Ocorrências  -----------------------------------------------------------
#' @export
#' 
dim_ocorrencias <- function(dataset_ocor, dim_tipo_acidente,
                            dim_origem_boletim) {
  G.LOG$log_dw(
    proc_msg = 'DIM: ocorrencias',
    
    expr = {
      ## Converte tipos de dados  ----
      fact <- G.FUNC$type_convert(df = dataset_ocor)
      
      ## Ajusta datas  ----
      fact <- split_date_time(fact)
      
      ## Surrogate Keys (FK)  ----
      fact <- left_join(
        x = fact,
        y = select(dim_tipo_acidente, acidente_tipo_nk, acidente_tipo_sk),
        by = 'acidente_tipo_nk'
      )
      
      fact <- left_join(
        x = fact,
        y = select(dim_origem_boletim, boletim_origem, boletim_origem_sk),
        by = 'boletim_origem'
      )
      
      ## Surrogate Key (PK)  ----
      fact <- arrange(.data = fact, boletim_data_sk, boletim_hora)
      fact <- mutate(.data = fact, boletim_sk = 1:n())
      
      ## Remove atributos das dimensões  ----
      fact <- select(
        .data = fact,
        -c(acidente_tipo_nk,
           acidente_tipo_desc,
           boletim_origem,
           tempo_desc,
           pavimento_desc,
           regional_desc,
           ups_desc)
      )
      
      ## Reorganiza estrutura  ----
      fact <- relocate(
        .data = fact,
        boletim_sk,
        boletim_nk,
        inclusao_data_sk,
        inclusao_hora,
        boletim_data_sk,
        boletim_hora,
        hora_informada,
        acidente_tipo_sk,
        ups_valor_sk,
        indicador_fatalidade,
        tempo_sk,
        pavimento_sk,
        regional_sk,
        boletim_origem_sk,
        local_sinalizado,
        velocidade_permitida,
        coordenada_x,
        coordenada_y,
        arquivo_origem
      )
      
      fact
    }
  )
}



#  Separa data e hora ( - )  ---------------------------------------------------
split_date_time <- function(dataset) {
  simplify_name <- '{str_remove_all(.col, "data_|hora_")}'
  
  ## Surrogate Key (PK) ----
  dataset <- mutate(.data = dataset, across(
    .cols  = where(is.POSIXct),
    .fns   = ~ date_to_sk(.),
    .names = paste0(simplify_name, '_data_sk')
  ))
  
  ## Hora ----
  dataset <- mutate(.data = dataset, across(
    .cols  = where(is.POSIXct),
    .fns   = ~ hour_to_str(.),
    .names = paste0(simplify_name, '_hora')
  ))
  
  ## Remove POSIXcts ----
  dataset <- select(.data = dataset, -where(is.POSIXct))
  return(dataset)
}


#  Constrói SK a partir de POSIXct ( - ) ---------------------------------------
date_to_sk <- function(x) {
  res <- as.character( date(x) )
  res <- as.numeric( str_remove_all(res, '-') )
  return(res)
}


#  Constrói Hora a partir de POSIXct ( - ) -------------------------------------
hour_to_str <- function(x) {
  h   <- str_pad(hour(x), 2, pad = '0')
  m   <- str_pad(minute(x), 2, pad = '0')
  s   <- str_pad(second(x), 2, pad = '0')
  res <- paste(h, m, s, sep = ':')
  return(res)
}
