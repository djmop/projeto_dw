box::use(dplyr[distinct, arrange])

box::use(G.LOG  = ../../../globals/Z2_global_logging)



#  DIM: Situação veículo  -----------------------------------------------------
#' @export
#' 
dim_situacao_veiculo <- function(dataset_veic) {
  G.LOG$log_dw(
    proc_msg = 'DIM: situacao_veiculo',
    expr = {
      dims <- distinct(dataset_veic, situacao_sk, situacao_desc)
      dims <- arrange(dims, situacao_sk)
      dims
    }
  )
}
