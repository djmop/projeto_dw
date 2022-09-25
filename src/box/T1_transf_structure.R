#' Concatena conjuntos de data.frames com colunas idênticas
#' 
#' @description Data.frames contendo colunas com nomes e ordens idênticos
#'   são concatenados em um data.frame único. 
#' @param x Lista de data.frames.
#' @return Lista de data.frames concatenados (Sn = dataset, Dn = dicionário).
#' @export
#' 
bind_by_cols <- function(x) {
  box::use(
    purrr[map, keep, every], glue[glue], dplyr[bind_rows],
    stringr[str_detect], g = ./zzz_globals
  )
  assertthat::assert_that(every(x, is.data.frame))
  
  col.sets <- list()
  df.sets  <- list()
  
  s = 0; d = 0
  for (i in seq_along(x)) {
    df <- x[[i]]
    df.name <- names(x[i])
    col.set <- keep(col.sets, ~ identical(., colnames(df)))
    
    if (length(col.set) == 0) {
      is.dict <- str_detect(df.name, g$regex_patts$dict_files)
      if (is.dict) {
        d = d + 1
        col.set.nm <- glue("D{d}")
      } else {
        s = s + 1
        col.set.nm <- glue("S{s}")
      }
      col.sets[[col.set.nm]] <- colnames(df)
    } else {
      col.set.nm <- names(col.set)
    }
    df.sets[[col.set.nm]][[df.name]] <- df
  }
  df.sets <- map(df.sets, bind_rows, .id = 'arquivo_origem')
  return(df.sets)
}



#' Transformação estrutural
#' @param x list. Lista de datasets.
#' @export
#' 
str_transform <- function(datasets) {
  box::use(cli[cli_process_start, cli_process_done], glue[glue])
  datasets <- purrr::map2(
    datasets,
    names(datasets),
    function(.x, .y) {
      cli_process_start(glue('Transformação estrutural: `{.y}`'))
      str_transformation <- str_func_mapper(.y)
      cli_process_done()
      return(str_transformation(.x))
    }
  )
  
  datasets <- purrr::modify_depth(
    datasets, 2,
    dplyr::mutate,
    across(.fns = stringr::str_squish)
  )
  
  return(datasets)
}

#' Mapeia função de transformação
str_func_mapper <- function(dataset.name) {
  func <- switch(dataset.name,
    logradouros         = str_transf_logradouros,
    ocorrencias         = str_transf_ocorrencias,
    pessoas_envolvidas  = str_transf_pessoas,
    veiculos_envolvidos = str_transf_veiculos
  )
  return(func)
}

#' Transformação estrutural individualizada para Logradouros
str_transf_logradouros <- function(dataset) {
  box::use(
    dplyr[rowwise, mutate, ungroup, rename, any_of, bind_rows],
    purrr[map], janitor[make_clean_names], magrittr[`%>%`],
    stringr[str_detect], g = ./zzz_globals
  )
  
  
  dict.names <- names(dataset)
  dict.names <- dict.names[str_detect(dict.names, 'D')]
  
  output <- list(data = NULL, dict = NULL)
  output$dict <- dataset[dict.names] %>% bind_rows() %>%
    rowwise() %>% 
    mutate(nome_do_campo = make_clean_names(nome_do_campo)) %>% 
    ungroup()
  
  dataset[dict.names] <- NULL

  
  dataset <- map(dataset, ~ rename(., any_of(g$constants$COLNAME_CONV)))
  dataset <- bind_rows(dataset)
  output$data <- dataset
  
  return(output)
}

#' Transformação estrutural individualizada para Ocorrências
str_transf_ocorrencias <- function(dataset) {
  box::use(
    dplyr[rowwise, mutate, ungroup, rename, any_of, bind_rows],
    purrr[map], janitor[make_clean_names], magrittr[`%>%`],
    stringr[str_detect], g = ./zzz_globals
  )
  
  dict.names <- names(dataset)
  dict.names <- dict.names[str_detect(dict.names, 'D')]
  
  output <- list(data = NULL, dict = NULL)
  output$dict <- dataset[dict.names] %>% bind_rows() %>%
    rowwise() %>% 
    mutate(nome_do_campo = make_clean_names(nome_do_campo)) %>% 
    ungroup()
  
  dataset[dict.names] <- NULL
  
  dataset <- map(dataset, ~ rename(., any_of(g$constants$COLNAME_CONV)))
  dataset <- bind_rows(dataset)
  output$data <- dataset
  
  return(output)
}

#' Transformação estrutural individualizada para Pessoas Envolvidas
str_transf_pessoas <- function(dataset) {
  box::use(
    dplyr[rowwise, mutate, ungroup, rename, any_of, bind_rows],
    purrr[map], janitor[make_clean_names], magrittr[`%>%`],
    stringr[str_detect], g = ./zzz_globals
  )
  
  dict.names <- names(dataset)
  dict.names <- dict.names[str_detect(dict.names, 'D')]
  
  output <- list(data = NULL, dict = NULL)
  output$dict <- dataset[dict.names] %>% bind_rows() %>%
    rowwise() %>% 
    mutate(nome_do_campo = make_clean_names(nome_do_campo)) %>% 
    ungroup()
  
  dataset[dict.names] <- NULL
  
  dataset <- map(dataset, ~ rename(., any_of(g$constants$COLNAME_CONV)))
  dataset <- bind_rows(dataset)
  output$data <- dataset
  
  return(output)
}

#' Transformação estrutural individualizada para Veículos Envolvidos
str_transf_veiculos <- function(dataset) {
  box::use(
    dplyr[rowwise, mutate, ungroup, rename, any_of, bind_rows],
    purrr[map], janitor[make_clean_names], magrittr[`%>%`],
    stringr[str_detect], g = ./zzz_globals
  )
  
  dict.names <- names(dataset)
  dict.names <- dict.names[str_detect(dict.names, 'D')]
  
  output <- list(data = NULL, dict = NULL)
  output$dict <- dataset[dict.names] %>% bind_rows() %>%
    rowwise() %>% 
    mutate(nome_do_campo = make_clean_names(nome_do_campo)) %>% 
    ungroup()
  
  dataset[dict.names] <- NULL
  
  dataset <- map(dataset, ~ rename(., any_of(g$constants$COLNAME_CONV)))
  dataset <- bind_rows(dataset)
  output$data <- dataset
  
  return(output)
}
