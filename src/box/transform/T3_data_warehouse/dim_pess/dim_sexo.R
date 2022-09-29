box::use(dplyr[distinct, mutate, across, case_when, arrange, relocate])

box::use(G.LOG  = ../../../globals/Z2_global_logging)


#  DIM: Sexo  ------------------------------------------------------------
#' @export
#' 
dim_sexo <- function(dataset_pess) {
  G.LOG$log_dw(
    proc_msg = 'DIM: sexo',
    expr = {
      dims <- distinct(.data = dataset_pess, sexo_nk)
      
      # Surrogate Key (PK)  ----   
      dims <- mutate(.data = dims, across(
        .cols = sexo_nk,
        .names = 'sexo_sk',
        .fns  = ~ case_when(
          . == '0' ~  0,
          . == 'F' ~  1,
          . == 'M' ~  2,
          TRUE ~ -1
        )
      ))
      
      # Descrição em extenso  ----
      dims <- mutate(.data = dims, across(
        .cols = sexo_nk,
        .names = 'sexo_desc',
        .fns  = ~ case_when(
          . == '0' ~ 'NAO INFORMADO',
          . == 'F' ~ 'FEMININO',
          . == 'M' ~ 'MASCULINO',
          TRUE ~ 'ERRO'
        )
      ))
      
      # Reorganiza estrutura  ----
      dims <- relocate(.data = dims, sexo_sk, sexo_nk, sexo_desc)
      dims <- arrange(.data = dims, sexo_sk)
      dims
    }
  )
}
