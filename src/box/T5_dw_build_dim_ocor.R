#' Constrói dimensões de ocorrências
#' @export
#' 
build <- function(datasets) {
  box::use(
    dplyr[relocate, distinct, arrange, mutate, filter, select, n, left_join],
    magrittr[`%<>%`, `%>%`], readr[type_convert, col_guess, cols]
  )
  
  occurrences <- datasets[['ocorrencias']]
  occurrences %<>% type_convert(col_types = cols(.default = col_guess()))
  
  
  ##  AJUSTA DATAS
  ##  ------------
  occurrences %<>% split_date_time()
  
  occurrences %<>% relocate(
    boletim_data_sk,
    boletim_hora,
    inclusao_data_sk, 
    inclusao_hora,
    .after = boletim_nk
  )
  
  
  ##=============#
  ##  DIMENSÕES  
  ##=============#
  dimensions <- list()

  
  ##  DIM: TIPO ACIDENTE
  ##  ------------------
  dimensions[['dim.ocor.tipo_acidente']] <- occurrences %>%
    distinct(acidente_tipo_nk, acidente_tipo_desc) %>% 
    arrange(acidente_tipo_nk) %>% 
    mutate(acidente_tipo_sk = 1:n()) %>% 
    relocate(acidente_tipo_sk)
  
  
  ##  DIM: TEMPO
  ##  ----------
  dimensions[['dim.ocor.tempo']] <- occurrences %>%
    distinct(tempo_sk, tempo_desc) %>% 
    arrange(tempo_sk) %>%
    filter(!is.na(tempo_sk))
  
  
  ##  DIM: PAVIMENTO
  ##  --------------
  dimensions[['dim.ocor.pavimento']] <- occurrences %>%
    distinct(pavimento_sk, pavimento_desc) %>% 
    arrange(pavimento_sk) %>%
    filter(!is.na(pavimento_sk))
  
  
  ##  DIM: REGIONAL
  ##  -------------
  dimensions[['dim.ocor.regional']] <- occurrences %>%
    distinct(regional_sk, regional_desc) %>% 
    arrange(regional_sk) %>%
    filter(!is.na(regional_sk))
  
  
  
  ##  DIM: ORIGEM BOLETIM
  ##  -------------------
  dimensions[['dim.ocor.origem_boletim']] <- occurrences %>%
    distinct(boletim_origem) %>% 
    arrange(boletim_origem) %>% 
    mutate(boletim_origem_sk = 1:n()) %>% 
    relocate(boletim_origem_sk)
  
  
  ##  DIM: VALOR UPS
  ##  --------------
  dimensions[['dim.ocor.valor_ups']] <- occurrences %>%
    distinct(ups_valor_sk, ups_desc) %>% 
    arrange(ups_valor_sk) %>%
    filter(!is.na(ups_valor_sk))
  
  
  ##  DIM: OCORRENCIA
  ##  ----------------
  occurrences %<>% select(-c(
    acidente_tipo_desc,
    tempo_desc,
    pavimento_desc,
    regional_desc,
    ups_desc
  ))
  
  # Retornando SKs geradas
  # ----------------------
  occurrences %<>% left_join(
    dimensions[["dim.ocor.tipo_acidente"]] %>%
      select(acidente_tipo_nk, acidente_tipo_sk),
    by = 'acidente_tipo_nk'
  )
  
  occurrences %<>% left_join(
    dimensions[["dim.ocor.origem_boletim"]] %>%
      select(boletim_origem, boletim_origem_sk),
    by = 'boletim_origem'
  )
  
  occurrences %<>% select(-c(
    acidente_tipo_nk,
    boletim_origem
  ))
  
  # Gerando SK para ocorrências
  # ---------------------------
  occurrences %<>%
    arrange(boletim_data_sk, boletim_hora) %>% 
    mutate(boletim_sk = 1:n())
  
  # Realocação
  # ----------
  occurrences %<>% relocate(
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
  
  dimensions[['dim.ocorrencias']] <- occurrences
  
  
  return(dimensions)
}



#' Separa data e hora
#' 
split_date_time <- function(dataset) {
  box::use(
    dplyr[select, mutate, across, everything, distinct, bind_rows, arrange],
    tidyr[pivot_longer], lubridate[date, is.POSIXct, hour, minute, second],
    stringr[str_remove_all, str_pad],
    purrr[map], magrittr[`%<>%`, `%>%`]
  )
  
  ##  DATA SK
  ##  -------
  dataset %<>% mutate(across(
    .cols  = where(is.POSIXct),
    .fns   = ~ as.character(date(.)) %>%
      str_remove_all('-') %>% 
      as.numeric(),
    .names = '{str_remove_all(.col, "data_|hora_")}_data_sk'
  ))
  
  
  ##  HORA
  ##  ----
  dataset %<>% mutate(across(
    .cols  = where(is.POSIXct),
    .fns   = ~ paste(
      str_pad(hour(.), 2, pad = '0'),
      str_pad(minute(.), 2, pad = '0'),
      str_pad(second(.), 2, pad = '0'),
      sep = ':'
    ),
    .names = '{str_remove_all(.col, "data_|hora_")}_hora'
  ))
  
  dataset %<>% select(-where(is.POSIXct))
  
  return(dataset)
}
