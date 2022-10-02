box::use(
  dplyr[mutate, across, case_when],
  lubridate[parse_date_time],
  stringi[stri_trans_general]
)

box::use(G.LOG   = ../../globals/Z2_global_logging)
box::use(G.CONST = ../../globals/Z3_global_constants)



#  Transf. Veículos ( - ) =======================================================
#' 
#' @description Transformação de valores individualizada para
#'   Veículos Envolvidos.
#' 
val_transf_veiculos <- function(data_set) {
  TIMEZONE     <- G.CONST$constants$TIMEZONE
  STR_TRANSLIT <- G.CONST$constants$STR_TRANSLIT
  
  ## Remoção de acentos ----------------------------------------------------------
  dt <- mutate(.data = data_set, across(
    .cols = c(
      categoria_desc,
      especie_desc,
      situacao_desc,
      tipo_socorro_desc
    ),
    .fns = ~ stri_trans_general(str = ., id = STR_TRANSLIT)
  ))
  
  ## Padronização de valores -----------------------------------------------------
  dt <- mutate(.data = dt, across(
    .cols = situacao_desc,
    .fns  = ~ case_when(
      situacao_sk == 3 ~ 'ESTACIONADO',
      . == ''          ~ 'NAO INFORMADO',
      TRUE ~ .
    )
  ))
  dt <- mutate(.data = dt, across(
    .cols = especie_desc,
    .fns  = ~ case_when(
      . == 'BONDE' ~ 'TREM/BONDE',
      . == 'TREM'  ~ 'TREM/BONDE',
      TRUE ~ .
    )
  ))
  
  
  dt <- mutate(.data = dt, across(
    .cols = tipo_socorro_desc,
    .fns  = ~ case_when(
      . == 'DISPENSOU ATENDIMEN' ~ 'DISPENSOU ATENDIMENTO',
      TRUE ~ .
    )
  ))
  
  ## Conversão de datas ----------------------------------------------------------
  dt <- mutate(.data = dt, across(
    data_hora_boletim, ~ parse_date_time(., '%d/%m/%Y %H:%M', tz = TIMEZONE)
  ))
  
  ## Retorno ---------------------------------------------------------------------
  return(dt)
}
