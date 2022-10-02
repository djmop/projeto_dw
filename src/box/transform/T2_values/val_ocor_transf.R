box::use(
  dplyr[select, mutate, across, case_when, na_if],
  lubridate[parse_date_time],
  stringi[stri_trans_general]
)

box::use(G.LOG   = ../../globals/Z2_global_logging)
box::use(G.CONST = ../../globals/Z3_global_constants)



#  Transf. Ocorrências ( - ) ===================================================
#' 
#' @description Transformação de valores individualizada para
#'   Ocorrências.
#' 
val_transf_ocorrencias <- function(data_set) {
  TIMEZONE     <- G.CONST$constants$TIMEZONE
  STR_TRANSLIT <- G.CONST$constants$STR_TRANSLIT
  
  ## Remoção de campos sem dados -----------------------------------------------
  dt <- select(.data = data_set, -c(
    data_alteracao_smsa,
    ups_antigo_valor,
    ups_antigo_desc 
  ))
  
  
  ## Conversão de datas --------------------------------------------------------
  dt <- mutate(.data = dt, across(
    .cols = c(
      data_hora_boletim,
      data_hora_inclusao
    ),
    .fns = ~ parse_date_time(., '%d/%m/%Y %H:%M', tz = TIMEZONE)
  ))
  
  
  ## Remoção de acentos --------------------------------------------------------
  dt <- mutate(.data = dt, across(
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
    .fns = ~ stri_trans_general(str = ., id = STR_TRANSLIT)
  ))
  
  
  ## Padronização de valores ---------------------------------------------------
  dt <- mutate(.data = dt, across(
    boletim_origem,
    ~ case_when(
      . == "CORPO DE BOMBEI" ~ 'CORPO DE BOMBEIROS',
      . == "POLICIA RODOVIA" ~ 'POLICIA RODOVIARIA',
      . == "PMMG"            ~ 'POLICIA MILITAR',
      . == 'NI'              ~ 'NAO INFORMADO',
      TRUE ~ .
    )
  ))
  
  dt <- mutate(.data = dt, across(
    c(regional_desc),
    ~ case_when(
      regional_sk == 0 ~ 'NAO INFORMADO',
      . == ""          ~ 'NAO INFORMADO',
      TRUE ~ .
    )
  ))
  
  dt <- mutate(.data = dt, across(
    c(coordenada_x, coordenada_y),
    ~case_when(
      . == '0000000000.00' ~ NA_character_,
      TRUE ~ .
    )
  ))
  
  dt$velocidade_permitida <- na_if(dt$velocidade_permitida, 0)
  
  
  ## Retorno -------------------------------------------------------------------
  return(dt)
}


