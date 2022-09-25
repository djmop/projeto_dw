#' Transformação de valores
#' @param x list. Lista de datasets.
#' @export
#' 
val_transform <- function(datasets) {
  box::use(glue[glue], purrr[map2], cli[cli_process_start, cli_process_done])
  datasets <- map2(
    datasets,
    names(datasets),
    function(.x, .y) {
      cli_process_start(glue('Transformação de valores: `{.y}`'))
      val_transformation <- val_func_mapper(.y)
      cli_process_done()
      return(val_transformation(.x))
    }
  )
  
  ##  REMOVENDO DUPLICATAS
  ##  --------------------
  datasets <- purrr::modify_depth(datasets, 2, dplyr::distinct)
  
  return(datasets)
}

#' Mapeia função de transformação
val_func_mapper <- function(dataset.name) {
  func <- switch(
    dataset.name,
    logradouros         = val_transf_logradouros,
    ocorrencias         = val_transf_ocorrencias,
    pessoas_envolvidas  = val_transf_pessoas,
    veiculos_envolvidos = val_transf_veiculos
  )
  return(func)
}

#' Transformação estrutural individualizada para Logradouros
val_transf_logradouros <- function(dataset) {
  box::use(
    dplyr[mutate, across, case_when, select, relocate],
    stringr[str_detect], stringi[stri_trans_general],
    lubridate[parse_date_time], magrittr[`%<>%`],
    g = ./zzz_globals
  )
  
  dataset.df <- dataset$data
  
  ##  REMOÇÃO DE CAMPOS SEM DADOS
  ##  ---------------------------
  dataset.df %<>% select(-c(
    municipio_sk,
    municipio_nome,
    logr_anterior_nome,
    logr_anterior_tipo,
    imovel_num,
    imovel_prox_num
  ))
  
  ##  AJUSTE DATAS
  ##  ------------
  dataset.df %<>% mutate(data_hora_boletim = parse_date_time(
    data_hora_boletim, '%d/%m/%Y %H:%M', tz = g$constants$TIMEZONE
  ))
  
  ##  PADRONIZAÇÃO
  ##  ------------
  dataset.df %<>% mutate(across(
    .cols = c(
      bairro_tipo_desc,
      logradouro_nome,
      bairro_nome
    ),
    .fns = ~ case_when(
      str_detect(., 'N.+O INFORMADO') ~ 'NAO INFORMADO',
      TRUE ~ .
    )
  ))
  
  ##  Source: https://fazenda.pbh.gov.br/iss/cmc/forms.htm
  dataset.df %<>% mutate(across(
    logradouro_tipo,
    .fns = ~ case_when(
      . == 'ACS' ~ 'ACESSO',
      . == 'ALA' ~ 'ALAMEDA',
      . == 'AVE' ~ 'AVENIDA',
      . == 'BEC' ~ 'BECO',
      . == 'CAM' ~ 'CAMINHO',
      . == 'ELV' ~ 'ELEVADO',
      . == 'ELP' ~ 'ESPACO LIVRE PARA PEDESTRE',
      . == 'EST' ~ 'ESTRADA',
      . == 'MAR' ~ 'MARGINAL',
      . == 'PCA' ~ 'PRACA',
      . == 'QTF' ~ 'QUARTEIRAO FECHADO',
      . == 'RDP' ~ 'RUA DE PEDESTRE',
      . == 'RMA' ~ 'REDE DE MANILHAS',
      . == 'ROD' ~ 'RODOVIA',
      . == 'RTN' ~ 'RETORNO',
      . == 'RUA' ~ 'RUA',
      . == 'TRE' ~ 'TREVO',
      . == 'TRI' ~ 'TRINCHEIRA',
      . == 'TRV' ~ 'TRAVESSA',
      . == 'TUN' ~ 'TUNEL',
      . == 'VDP' ~ 'VIA DE PEDESTRE',
      . == 'VDT' ~ 'VIADUTO',
      . == 'VIA' ~ 'VIA'
    ),
    .names = 'logradouro_tipo_desc'
  ))
  
  dataset.df %<>% relocate(logradouro_tipo_desc, .after = logradouro_tipo)
  
  
  ##  REMOÇÃO DE ACENTOS
  ##  ------------------
  dataset.df %<>% mutate(across(
    .cols = c(
      bairro_tipo_desc,
      logradouro_nome,
      bairro_nome
    ),
    .fns = ~ stri_trans_general(str = ., id = g$constants$STR_TRANSLIT)
  ))
  
  
  ##  SALVANDO ALTERAÇÕES
  ##  -------------------
  dataset$data <- dataset.df
  return(dataset)
}

#' Transformação estrutural individualizada para Ocorrências
val_transf_ocorrencias <- function(dataset) {
  box::use(
    dplyr[mutate, across, case_when, select, na_if],
    lubridate[parse_date_time], magrittr[`%<>%`],
    stringi[stri_trans_general], g = ./zzz_globals
  )
  
  dataset.df <- dataset$data
  
  ##  REMOÇÃO DE CAMPOS SEM DADOS
  ##  ---------------------------
  dataset.df %<>% select(-c(
    data_alteracao_smsa,
    ups_antigo_valor,
    ups_antigo_desc 
  ))
  
  
  ##  AJUSTE DATA
  ##  -----------  
  dataset.df %<>% mutate(across(
    c(data_hora_boletim, data_hora_inclusao),
    ~ parse_date_time(., '%d/%m/%Y %H:%M', tz = g$constants$TIMEZONE)
  ))
  
  
  ##  REMOÇÃO DE ACENTOS
  ##  ------------------
  dataset.df %<>% mutate(across(
    .cols = c(
      hora_informada,
      acidente_tipo_desc,
      tempo_desc,
      pavimento_desc,
      regional_desc,
      boletim_origem,
      local_sinalizado,
      indicador_fatalidade,
      hora_informada,
      ups_desc
    ),
    .fns = ~ stri_trans_general(str = ., id = g$constants$STR_TRANSLIT)
  ))
  
  
  ##  PADRONIZAÇÃO
  ##  ------------
  dataset.df %<>% mutate(across(
    c(boletim_origem),
    ~ case_when(
      . == "CORPO DE BOMBEI" ~ 'CORPO DE BOMBEIROS',
      . == "POLICIA RODOVIA" ~ 'POLICIA RODOVIARIA',
      . == "PMMG"            ~ 'POLICIA MILITAR',
      . == 'NI'              ~ 'NAO INFORMADO',
      TRUE ~ .
    )
  ))
  
  dataset.df %<>% mutate(across(
    c(regional_desc),
    ~ case_when(
      regional_sk == 0 ~ 'NAO INFORMADO',
      . == ""          ~ 'NAO INFORMADO',
      TRUE ~ .
    )
  ))
  
  dataset.df %<>% mutate(across(
    c(coordenada_x, coordenada_y),
    ~case_when(
      . == '0000000000.00' ~ NA_character_,
      TRUE ~ .
    )
  ))
  
  dataset.df$velocidade_permitida %<>% na_if(., 0)
  
  
  ##  SALVANDO ALTERAÇÕES
  ##  -------------------
  dataset$data <- dataset.df
  return(dataset)
}

#' Transformação estrutural individualizada para Pessoas Envolvidas
val_transf_pessoas <- function(dataset) {
  box::use(
    dplyr[mutate, across, case_when, select, na_if, rename, relocate],
    lubridate[parse_date_time, dmy, date, as.duration, interval, dyears, NA_Date_],
    magrittr[`%<>%`, `%>%`], stringi[stri_trans_general], g = ./zzz_globals
  )
  
  dataset.df <- dataset$data
  
  
  ##  REMOÇÃO DE CAMPOS SEM DADOS
  ##  ---------------------------
  dataset.df %<>% select(-c(
    declaracao_obito,
    severidade_antiga_cod,
    usa_capacete,
    x
  ))
  
  ##  REMOÇÃO DE ACENTOS
  ##  ------------------
  dataset.df %<>% mutate(across(
    .cols = c(
      condutor,
      severidade_desc,
      cinto_seguranca,
      embreagues,
      categ_habilitacao_desc,
      especie_veiculo_desc,
      pedestre,
      passageiro
    ),
    .fns = ~ stri_trans_general(str = ., id = g$constants$STR_TRANSLIT)
  ))
  
  ##  PADRONIZAÇÃO
  ##  ------------
  dataset.df %<>% mutate(across(
    .cols = c(
      condutor,
      pedestre,
      passageiro,
      cinto_seguranca,
      embreagues
    ),
    .fns = ~ case_when(
      . == '0' | . == '' | . == 'NAO INFORMADO' | is.na(.) ~ 'NI',
      . == 'N' ~ 'NAO',
      . == 'S' ~ 'SIM',
      TRUE ~ .
    )
  ))
  
  dataset.df %<>% mutate(across(
    categ_habilitacao_cod,
    ~ case_when(
      categ_habilitacao_desc == 'NAO SE APLICA' ~ 'NA',
      . == 'N' | . == '' | is.na(.) ~ 'NI',
      TRUE ~ .
    )
  ))
  
  dataset.df %<>% mutate(across(
    categ_habilitacao_desc,
    ~ case_when(
      . == '' | is.na(.) ~ 'NAO INFORMADO',
      TRUE ~ .
    )
  ))
  
  dataset.df %<>% mutate(across(
    especie_veiculo_desc,
    ~ case_when(
      . == '' | is.na(.) ~ 'NAO INFORMADO',
      TRUE ~ .
    )
  ))
  
  
  ##  SEXO
  ##  ----
  dataset.df %<>% mutate(across(
    sexo_cod,
    ~ case_when(
      !(. %in% c('M', 'F')) ~ '0',
      TRUE ~ .
    )
  ))
  
  
  ##  AJUSTE DATAS
  ##  ------------
  dataset.df %<>% mutate(across(
    data_hora_boletim,
    ~parse_date_time(., '%d/%m/%Y %H:%M', tz = g$constants$TIMEZONE)
  ))
  
  dataset.df$data_nascimento %<>% na_if(., '00/00/0000')
  
  dataset.df %<>% mutate(across(data_nascimento, ~dmy(.)))
  
  
  ##  AJUSTE DE IDADE E DATA DE NASCIMENTO
  ##  ------------------------------------
  dataset.df$idade %<>% as.numeric()
  
  dataset.df %<>% mutate(across(
    idade,
    ~case_when(
      . == 0 & is.na(data_nascimento) ~ NA_real_,
      TRUE ~ .
    )
  ))
  
  dataset.df %<>% mutate(
    idade_new = as.duration(
      interval(data_nascimento, date(data_hora_boletim))
    ) %/% dyears()
  ) %>% relocate(idade_new, .after = idade)
  
  dataset.df %<>% mutate(
    idade_diff = idade_new - idade
  ) %>% relocate(idade_diff, .after = idade_new)
  
  dataset.df %<>% mutate(across(
    data_nascimento,
    ~ case_when(
      is.na(idade_new) | is.na(idade_diff) ~ .,
      idade_new < 0 | idade_new > 125 ~ NA_Date_,
      TRUE ~ .
    )
  ))
  
  dataset.df %<>% mutate(across(
    idade_new,
    ~ case_when(
      is.na(idade_new) | is.na(idade_diff) ~ .,
      idade_new < 0 | idade_new > 125 ~ NA_real_,
      TRUE ~ .
    )
  ))
  
  dataset.df %<>% select(-idade, -idade_diff)
  dataset.df %<>% rename(idade = idade_new)
  
  
  ##  SALVANDO ALTERAÇÕES
  ##  -------------------
  dataset$data <- dataset.df
  return(dataset)
}

#' Transformação estrutural individualizada para Veículos Envolvidos
val_transf_veiculos <- function(dataset) {
  box::use(
    dplyr[mutate, across, case_when], lubridate[parse_date_time],
    magrittr[`%<>%`], stringi[stri_trans_general],
    g = ./zzz_globals
  )
  
  dataset.df <- dataset$data
  
  ##  REMOÇÃO DE ACENTOS
  ##  ------------------
  dataset.df %<>% mutate(across(
    .cols = c(
      categoria_desc,
      especie_desc,
      situacao_desc,
      tipo_socorro_desc
    ),
    .fns = ~ stri_trans_general(str = ., id = g$constants$STR_TRANSLIT)
  ))
  
  
  ##  PADRONIZAÇÃO
  ##  ------------
  dataset.df %<>% mutate(across(
    .cols = situacao_desc,
    .fns  = ~ case_when(
      . == '' ~ 'NAO INFORMADO',
      TRUE ~ .
    )
  ))
  
  dataset.df %<>% mutate(across(
    .cols = tipo_socorro_desc,
    .fns  = ~ case_when(
      . == 'DISPENSOU ATENDIMEN' ~ 'DISPENSOU ATENDIMENTO',
      TRUE ~ .
    )
  ))
  
  ##  AJUSTANDO TIPO DATA
  ##  -------------------
  dataset.df %<>% mutate(across(
    data_hora_boletim,
    ~ parse_date_time(., '%d/%m/%Y %H:%M', tz = g$constants$TIMEZONE)
  ))
  
  ##  SALVANDO ALTERAÇÕES
  ##  -------------------
  dataset$data <- dataset.df
  return(dataset)
}
