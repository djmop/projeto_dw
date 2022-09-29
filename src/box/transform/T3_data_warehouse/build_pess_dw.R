box::use(dplyr[case_when, mutate, relocate, select, rename])
box::use(dplyr[distinct, left_join, across, arrange, n])

box::use(G.LOG  = ../../globals/Z2_global_logging)
box::use(G.FUNC = ../../globals/Z4_global_functions)

box::use(HAB = ./dim_pess/dim_categ_habilitacao)
box::use(SEV = ./dim_pess/dim_severidade)
box::use(SXO = ./dim_pess/dim_sexo)


#' Constrói dimensões de Pessoas
#' @param dataset_pess Dataset Pessoas.
#' @export
#' 
build_dims <- function(dataset_pess) {
  dataset_pess <- G.FUNC$type_convert(df = dataset_pess)
  
  ##  Dimensões  ----
  dimensions <- list()
  dimensions[['categ_habilitacao']] <- HAB$dim_categ_habilitacao(dataset_pess)
  dimensions[['severidade']]        <- SEV$dim_severidade(dataset_pess)
  dimensions[['sexo']]              <- SXO$dim_sexo(dataset_pess)
  
  return(dimensions)
}



#  FATO: Pessoas Envolvidas  -----------------------------------------------
#' 
#' Constrói tabela Fato de Pessoas Envolvidas
#' @param dataset_pess Dataset Pessoas.
#' @param dim_ocor Dimensão Ocorrências.
#' @param dim_sexo Dimensão Sexo.
#' @param dim_categ_hab Dimensão Categoria de Habilitação.
#' @param dim_especie_veic Dimensão Espécie do Veículo.
#' @export
#' 
build_fact <- function(dataset_pess, dim_ocor, dim_sexo, dim_categ_hab,
                       dim_especie_veic) {
  G.LOG$log_dw(
    proc_msg = 'FACT: pessoas_envolvidas',
    
    expr = {
    # Converte tipos de dados  ----
      fact <- G.FUNC$type_convert(df = dataset_pess)
      
    # Surrogate Keys (FK)  ----
      fact <- left_join(
        x = fact,
        y = select(.data = dim_ocor, boletim_nk, boletim_sk),
        by = 'boletim_nk'
      )
      
      fact <- left_join(
        x = fact,
        y = select(.data = dim_sexo, sexo_nk, sexo_sk),
        by = 'sexo_nk'
      )
      
      fact <- left_join(
        x = fact,
        y = select(
          .data = dim_categ_hab,
          categ_habilitacao_nk,
          categ_habilitacao_sk
        ),
        by = 'categ_habilitacao_nk'
      )
      
      fact <- left_join(
        x = fact,
        y = select(.data = dim_especie_veic, especie_desc, especie_sk),
        by = c('especie_veiculo_desc' = 'especie_desc')
      )
      
      fact <- rename(.data = fact, especie_veiculo_sk = especie_sk)
      
      
    # Surrogate Key (PK)  ----
      fact <- arrange(.data = fact, boletim_sk, pessoa_sequencial) 
      fact <- mutate(.data = fact, pessoa_envolvida_sk = 1:n())
      fact <- relocate(.data = fact, pessoa_envolvida_sk) 
      
    # Remove atributos das dimensões  ----
      fact <- select(
        .data = fact,
        -c(
          boletim_nk,
          data_hora_boletim,
          severidade_desc,
          sexo_nk,
          categ_habilitacao_nk,
          categ_habilitacao_desc,
          especie_veiculo_desc
        )
      )
      
    # Reorganiza estrutura  ----
      fact <- relocate(
        .data = fact,
        pessoa_envolvida_sk,
        boletim_sk,
        pessoa_sequencial,
        especie_veiculo_sk,
        severidade_sk,
        categ_habilitacao_sk,
        sexo_sk,
        idade,
        data_nascimento,
        condutor,
        cinto_seguranca,
        embreagues,
        pedestre,
        passageiro,
        arquivo_origem
      )
      
      fact
    }
  )
}
