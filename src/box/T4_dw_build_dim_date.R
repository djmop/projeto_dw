#' Constrói dimensão data
#' @export
#' 
build <- function(datasets) {
  box::use(
    stringi[stri_trans_general], stringr[str_to_upper, str_remove_all],
    lubridate[year, quarter, month, mday, wday],
    dplyr[mutate, across, relocate],
    magrittr[`%<>%`, `%>%`], g = ./zzz_globals
  )
  
  
  dim.dates <- select_dates(datasets)
  
  
  ##  GERA ATRIBUTOS
  ##  --------------
  loc <- g$constants$LOCALE
  
  dim.dates %<>% mutate(
    ano            = year(data),
    trimestre      = quarter(data),
    mes_num        = month(data),
    mes_nome_abrev = month(data, label = T, locale = loc),
    mes_nome_compl = month(data, label = T, abbr = F, locale = loc),
    dia_mes        = mday(data),
    dia_semana     = wday(data),
    dia_nome_abrev = wday(data, label = T, locale = loc),
    dia_nome_compl = wday(data, label = T, abbr = F, locale = loc)
  )
  
  
  ##  AJUSTA ATRIBUTOS
  ##  ----------------
  dim.dates %<>% mutate(across(
    c(mes_nome_compl, mes_nome_abrev, dia_nome_abrev, dia_nome_compl),
    ~ stri_trans_general(str = ., id = g$constants$STR_TRANSLIT) %>% 
      str_to_upper()
  ))
  
  
  ##  GERA SURROGATE KEY
  ##  ------------------
  dim.dates %<>% mutate(
    data_sk = as.character(data) %>% 
      str_remove_all('-') %>% 
      as.numeric()
  ) %>% relocate(data_sk)
  
  return(list('dim.ocor.data' = dim.dates))
}


#' Selectiona todas as datas presentes nos dataframes
select_dates <- function(datasets) {
  box::use(
    dplyr[select, mutate, across, everything, distinct, bind_rows, arrange],
    tidyr[pivot_longer], lubridate[is.Date, is.POSIXct],
    purrr[map], magrittr[`%<>%`, `%>%`]
  )
  
  dim.dates <- map(datasets, ~select(., where(~ is.Date(.) | is.POSIXct(.))))
  dim.dates <- map(dim.dates, ~mutate(., across(.fns = as.Date)))
  dim.dates <- map(dim.dates, function(x) {
    x %<>% pivot_longer(
      cols = everything(),
      values_to = 'data'
    ) %>% select(-name) %>% 
      distinct()
  })
  
  dim.dates <- bind_rows(dim.dates) %>%
    arrange(data)
  
  return(dim.dates)
}

