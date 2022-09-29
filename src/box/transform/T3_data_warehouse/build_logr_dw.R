box::use(dplyr[distinct, filter, left_join, select, arrange, mutate, relocate, n])

box::use(G.FUNC = ../../globals/Z4_global_functions)
box::use(G.LOG  = ../../globals/Z2_global_logging)

box::use(LOGR = ./dim_logr/dim_logradouro)
box::use(BAIR = ./dim_logr/dim_bairro)



#' Constrói dimensões de logradouros
#' @param dataset_logr Dataset de logradouros.
#' @export
#' 
build_dims <- function(dataset_logr) {
  dataset_logr <- G.FUNC$type_convert(df = dataset_logr)
  
  ##  Dimensões  ----
  dims <- list()
  dims[['logradouro']] <- LOGR$dim_logradouro(dataset_logr)
  dims[['bairro']]     <- BAIR$dim_bairro(dataset_logr)
  
  return(dims)
}


#  FATO: Logradouros Envolvidos  -----------------------------------------------
#' 
#' @description Constrói tabela Fato de logradouros
#' @param dataset_logr Dataset de logradouros.
#' @param dim_ocor Dimensão ocorrências.
#' @export
#' 
build_fact <- function(dataset_logr, dim_ocor) {
  G.LOG$log_dw(
    proc_msg = 'FACT: logradouros_envolvidos',
    expr = {
    # Converte tipos de dados  ----
      fact <- G.FUNC$type_convert(df = dataset_logr)
      
    # Surrogate Keys (FK)  ----
      fact <- left_join(
        x = fact, y = select(dim_ocor, boletim_nk, boletim_sk),
        by = 'boletim_nk'
      )
      
    # Surrogate Key (PK)  ----
      fact <- arrange(.data = fact, boletim_sk, logradouros_sequencial)
      fact <- mutate(.data = fact, log_envolvido_sk = 1:n())
      
    # Remove atributos das dimensões  ----
      fact <- select(
        .data = fact,
        -c(boletim_nk,
           data_hora_boletim,
           logradouro_nome,
           logradouro_tipo_nk,
           logradouro_tipo_desc,
           bairro_nome,
           bairro_tipo_nk,
           bairro_tipo_desc)
      )
      
    # Reorganiza estrutura  ----
      fact <- relocate(
        .data = fact,
        log_envolvido_sk,
        boletim_sk,
        bairro_sk,
        logradouro_sk, 
        logradouros_sequencial
      )
      
      fact
    }
  )
}
