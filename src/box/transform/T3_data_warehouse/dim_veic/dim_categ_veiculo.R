box::use(dplyr[distinct, arrange])

box::use(G.LOG  = ../../../globals/Z2_global_logging)



#  DIM: Categoria veículo  -----------------------------------------------------
#' @export
#' 
dim_categoria_veiculo <- function(dataset_veic) {
  G.LOG$oversee(
    proc_msg = 'DIM: categoria_veiculo',
    expr = {
      dims <- distinct(dataset_veic, categoria_sk, categoria_desc)
      dims <- arrange(dims, categoria_sk)
      dims
    }
  )
}
