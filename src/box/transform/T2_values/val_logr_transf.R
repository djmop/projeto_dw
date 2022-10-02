box::use(
  dplyr[select, mutate, across, case_when, relocate],
  lubridate[parse_date_time],
  stringi[stri_trans_general],
  stringr[str_detect]
)

box::use(G.LOG   = ../../globals/Z2_global_logging)
box::use(G.CONST = ../../globals/Z3_global_constants)



#  Transf. Logradouros ( - ) ===================================================
#' @description Transformação de valores individualizada para
#'   Logradouros.
#' 
val_transf_logradouros <- function(data_set) {
  TIMEZONE     <- G.CONST$constants$TIMEZONE
  STR_TRANSLIT <- G.CONST$constants$STR_TRANSLIT
  
  ## Remoção de campos sem dados -----------------------------------------------
  dt <- select(
    .data = data_set,
    -c(municipio_sk,
       municipio_nome,
       logr_anterior_tipo_nk,
       logr_anterior_nome,
       imovel_num,
       imovel_prox_num)
  )
  
  ## Conversão de datas --------------------------------------------------------
  dt <- mutate(.data = dt, across(
    data_hora_boletim, ~ parse_date_time(., '%d/%m/%Y %H:%M', tz = TIMEZONE)
  ))
  
  ## Padronização de valores ---------------------------------------------------
  dt <- mutate(.data = dt, across(
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
  
  dt <- mutate(.data = dt, across(
    logradouro_tipo_nk,
    .fns = ~ case_when(
      . == 'NI' ~ 'NIF',
      TRUE ~ .
    )
  ))
  
  ## Source: https://fazenda.pbh.gov.br/iss/cmc/forms.htm
  dt <- mutate(.data = dt, across(
    logradouro_tipo_nk,
    .fns = ~ case_when(
      . == 'NIF' ~ 'NAO INFORMADO',
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
  
  dt <- relocate(
    .data = dt, 
    logradouro_tipo_desc, .after = logradouro_tipo_nk
  )
  
  ## Remoção de acentos --------------------------------------------------------
  dt <- mutate(.data = dt, across(
    .cols = c(
      bairro_tipo_desc,
      logradouro_nome,
      bairro_nome
    ),
    .fns = ~ stri_trans_general(str = ., id = STR_TRANSLIT)
  ))
  
  ## Retorno -------------------------------------------------------------------
  return(dt)
}


