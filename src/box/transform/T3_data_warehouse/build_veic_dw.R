box::use(dplyr[distinct, left_join, select, arrange, mutate, relocate, n])

box::use(G.FUNC = ../../globals/Z4_global_functions)
box::use(G.LOG  = ../../globals/Z2_global_logging)

box::use(CGV = ./dim_veic/dim_categ_veiculo)
box::use(SPV = ./dim_veic/dim_especie_veiculo)
box::use(SIT = ./dim_veic/dim_situacao_veiculo)
box::use(SOC = ./dim_veic/dim_tipo_socorro)


#' Constrói dimensões de veículos
#' @param dataset_veic Dataset de veículos.
#' @export
#' 
build_dims <- function(dataset_veic) {
  dataset_veic <- G.FUNC$type_convert(df = dataset_veic)
  
  ##  Dimensões  ----
  dims <- list()
  dims[['categoria_veiculo']] <- CGV$dim_categoria_veiculo(dataset_veic)
  dims[['situacao_veiculo']]  <- SIT$dim_situacao_veiculo(dataset_veic)
  dims[['especie_veiculo']]   <- SPV$dim_especie_veiculo(dataset_veic)
  dims[['tipo_socorro']]      <- SOC$dim_tipo_socorro(dataset_veic)
  
  return(dims)
}



#  FATO: Veículos Envolvidos  --------------------------------------------------
#' 
#' @description Constrói tabela Fato de veículos
#' @param dataset_veic Dataset de veículos.
#' @param dim_ocor Dimensão ocorrências.
#' @export
#' 
build_fact <- function(dataset_veic, dim_ocor) {
  
  G.LOG$log_dw(
    proc_msg = 'FACT: veiculos_envolvidos',
    expr = {
    # Converte tipos de dados  ----
      fact <- G.FUNC$type_convert(df = dataset_veic)
      
    # Surrogate Keys (FK)  ----
      fact <- left_join(
        x = fact,
        y = select(dim_ocor, boletim_nk, boletim_sk),
        by = 'boletim_nk'
      )
      
    # Reorganiza estrutura  ----
      fact <- relocate(
        .data = fact,
        boletim_sk,
        veiculo_sequencial,
        categoria_sk,
        especie_sk,
        situacao_sk,
        tipo_socorro_sk,
        arquivo_origem
      )
      
    # Remove atributos das dimensões  ----
      fact <- select(
        .data = fact,
        -c(
          boletim_nk,
          data_hora_boletim,
          categoria_desc,
          especie_desc,
          situacao_desc,
          tipo_socorro_desc
        )
      )
      
    # Surrogate Key (PK)  ----
      fact <- arrange(fact, boletim_sk, veiculo_sequencial)
      fact <- mutate(fact, veiculo_envolvido_sk = 1:n())
      fact <- relocate(fact, veiculo_envolvido_sk)
      fact
    }
  )
}
