box::use(dplyr[distinct, arrange])

box::use(G.LOG  = ../../../globals/Z2_global_logging)



#  DIM: Espécie veículo  -------------------------------------------------------
#' @export
#' 
dim_especie_veiculo <- function(dataset_veic) {
  G.LOG$oversee(
    proc_msg = 'DIM: especie_veiculo',
    expr = {
      dims <- distinct(dataset_veic, especie_sk, especie_desc)
      dims <- arrange(dims, especie_sk)
      dims
    }
  )
}
