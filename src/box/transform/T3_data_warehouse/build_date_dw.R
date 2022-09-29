box::use(
  lubridate[year, quarter, month, mday, wday, is.Date, is.POSIXct],
  dplyr[arrange, across, everything, select, tibble, bind_rows, distinct],
  dplyr[mutate, relocate, all_of, filter],
  stringr[str_remove_all, str_to_upper],
  stringi[stri_trans_general],
  assertthat[assert_that],
  purrr[map, every, keep],
  tidyr[pivot_longer],
  rlang[.data]
)

box::use(G.LOG   = ../../globals/Z2_global_logging)
box::use(G.CONST = ../../globals/Z3_global_constants)


#' Constrói dimensão data
#' @param datasets `list`. Lista de dataframes.
#' @export
#' 
build_dims <- function(datasets) {
  G.LOG$log_dw(
    proc_msg = 'DIM: data',
    
    expr = {
      var       <- 'data'
      var_sk    <- 'data_sk'
      dim.dates <- select_dates(datasets, col_date = var)
      dim.dates <- create_date_sk(dim.dates, col_date = var, col_sk = var_sk)
      dim.dates <- relocate(dim.dates, all_of(var_sk))
      
      ##  GERA ATRIBUTOS ----
      loc <- G.CONST$constants$LOCALE
      
      dim.dates <- mutate(
        .data          = dim.dates,
        ano            = year(.data[[var]]),
        trimestre      = quarter(.data[[var]]),
        mes_num        = month(.data[[var]]),
        mes_nome_abrev = month(.data[[var]], label = T, locale = loc),
        mes_nome_compl = month(.data[[var]], label = T, abbr = F, locale = loc),
        dia_mes        = mday(.data[[var]]),
        dia_semana     = wday(.data[[var]]),
        dia_nome_abrev = wday(.data[[var]], label = T, locale = loc),
        dia_nome_compl = wday(.data[[var]], label = T, abbr = F, locale = loc)
      )
      
      ##  AJUSTA ATRIBUTOS ----
      dim.dates <- mutate(.data = dim.dates, across(
        .cols = c(
          mes_nome_compl, 
          mes_nome_abrev, 
          dia_nome_abrev,
          dia_nome_compl
        ),
        .fns = ~ stri_trans_general(
          str = str_to_upper(.),
          id = G.CONST$constants$STR_TRANSLIT
        )
      ))
      
      dim.dates
    }
  )
}



#' Selectiona todas as datas presentes em uma lista de dataframes
#' @param datasets `list`. Lista de dataframes.
#' @param col_date `string`. Nome da coluna que sustentará as datas.
#' 
select_dates <- function(datasets, col_date = 'data') {
  assert_that(every(datasets, is.data.frame))
  dates <- map(datasets, ~ get_dates_all(., col_date = col_date))
  dates <- bind_rows(dates)
  dates <- distinct(dates)
  dates <- arrange(dates, .data[[col_date]])
  dates <- filter(dates, !is.na(.data[[col_date]]))
  return(dates)
}



#' Seleciona e converte datas
#' @description Seleciona e converte todas as colunas que contêm data
#'   em um data frame.
#' @param df `data.frame`.
#' @param col_date `string`. Nome da coluna que sustentará as datas.
#' 
get_dates_all <- function(df, col_date) {
  dates <- keep(df, ~ is.Date(.) | is.POSIXct(.))
  if (ncol(dates) == 0) { return(tibble()) }
  dates <- mutate(dates, across(.fns = as.Date))
  dates <- pivot_longer(dates, cols = everything(), values_to = col_date)
  dates <- select(dates, -name)
  return(dates)
}



#' Cria Surrogate Keys para datas
#' @param df `data.frame`.
#' @param col_date `string`. Variável que contém as datas.
#' @param col_sk `string`. Variável que conterá as Surrogate Keys.
#' 
create_date_sk <- function(df, col_date, col_sk) {
  df <- mutate(df, '{col_sk}' := parse_date_sk(.data[[col_date]]))
  return(df)
}


#' Transforma data em Surrogate Key
#' @param x `Date`.
#' 
parse_date_sk <- function(x) {
  assert_that(is.Date(x))
  x <- as.character(x)
  x <- str_remove_all(x, '-')
  x <- as.numeric(x)
  return(x)
}
