box::use(
  dplyr[select, mutate, across, case_when, na_if, relocate, rename],
  lubridate[parse_date_time, dmy, as.duration, interval, date, dyears, NA_Date_],
  stringi[stri_trans_general]
)

box::use(G.LOG   = ../../globals/Z2_global_logging)
box::use(G.CONST = ../../globals/Z3_global_constants)



#  Transf. Pessoas ( # ) =======================================================
#' 
#' @description Transformação de valores individualizada para
#'   Pessoas Envolvidas.
#' @export
#' 
val_transf_pessoas <- function(data_set) {
  TIMEZONE     <- G.CONST$constants$TIMEZONE
  STR_TRANSLIT <- G.CONST$constants$STR_TRANSLIT
  
  ## Remoção de campos sem dados -----------------------------------------------
  dt <- select(
    .data = data_set,
    -c(declaracao_obito,
       severidade_antiga_nk,
       usa_capacete,
       x)
  )
  
  
  ## Remoção de acentos --------------------------------------------------------
  dt <- mutate(.data = dt, across(
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
    .fns = ~ stri_trans_general(str = ., id = STR_TRANSLIT)
  ))
  
  
  ## Padronização de valores ---------------------------------------------------
  dt <- mutate(.data = dt, across(
    .cols = c(
      condutor,
      pedestre,
      passageiro,
      cinto_seguranca,
      embreagues
    ),
    .fns = ~ case_when(
      . == '0' | . == '' | . == 'NAO INFORMADO' | is.na(.) ~ 'NIF',
      . == 'N' ~ 'NAO',
      . == 'S' ~ 'SIM',
      TRUE ~ .
    )
  ))
  
  dt <- mutate(.data = dt, across(
    .cols = categ_habilitacao_nk,
    .fns  = ~ case_when(
      categ_habilitacao_desc == 'NAO SE APLICA' ~ 'NSA',
      categ_habilitacao_desc == 'INABILITADO'   ~ 'INB',
      . == 'N' | . == '' | is.na(.)             ~ 'NIF',
      TRUE ~ .
    )
  ))
  
  dt <- mutate(.data = dt, across(
    .cols = categ_habilitacao_desc,
    .fns  = ~ case_when(
      . == '' | is.na(.) ~ 'NAO INFORMADO',
      TRUE ~ .
    )
  ))
  
  dt <- mutate(.data = dt, across(
    .cols = especie_veiculo_desc,
    .fns  = ~ case_when(
      . == '' | is.na(.) ~ 'NAO INFORMADO',
      TRUE ~ .
    )
  ))
  
  dt <- mutate(.data = dt, across(
    .cols = sexo_nk,
    .fns  = ~ case_when(
      !(. %in% c('M', 'F')) ~ '0',
      TRUE ~ .
    )
  ))
  
  
  ## Conversão de datas --------------------------------------------------------
  dt <- mutate(.data = dt, across(
    data_hora_boletim, ~ parse_date_time(., '%d/%m/%Y %H:%M', tz = TIMEZONE)
  ))
  
  dt$data_nascimento <- na_if(dt$data_nascimento, '00/00/0000')
  dt <- mutate(.data = dt, across(data_nascimento, ~dmy(.)))
  
  
  ## Ajuste de idade e data de nascimento --------------------------------------
  dt$idade <- as.numeric(dt$idade)
  
  dt <- mutate(.data = dt, across(
    .cols = idade,
    .fns  = ~case_when(
      . == 0 & is.na(data_nascimento) ~ NA_real_,
      TRUE ~ .
    )
  ))
  
  dt <- mutate(dt, idade_new = calc_age(data_nascimento, data_hora_boletim))
  dt <- mutate(dt, idade_diff = idade_new - idade)
  dt <- relocate(dt, idade_new, idade_diff, .after = idade)
  
  dt <- mutate(.data = dt, across(
    .cols = data_nascimento,
    .fns  = ~ case_when(
      is.na(idade_new) | is.na(idade_diff) ~ .,
      idade_new < 0 | idade_new > 125 ~ NA_Date_,
      TRUE ~ .
    )
  ))
  
  dt <- mutate(.data = dt, across(
    .cols = idade_new,
    .fns  = ~ case_when(
      is.na(idade_new) | is.na(idade_diff) ~ .,
      idade_new < 0 | idade_new > 125 ~ NA_real_,
      TRUE ~ .
    )
  ))
  
  dt <- select(.data = dt, -idade, -idade_diff)
  dt <- rename(.data = dt, idade = idade_new)
  
  
  ## Retorno -------------------------------------------------------------------
  return(dt)
}



#  Calculo de idade ( - ) ======================================================
#' 
calc_age <- function(data_nasc, data_bol) {
  x <- interval(data_nasc, date(data_bol))
  x <- as.duration(x)
  age <- x %/% dyears()
  return(age)
}
