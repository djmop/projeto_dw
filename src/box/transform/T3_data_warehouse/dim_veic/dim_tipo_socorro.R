box::use(dplyr[distinct, arrange])

box::use(G.LOG  = ../../../globals/Z2_global_logging)



#  DIM: Tipo socorro  ----------------------------------------------------------
#' @export
#' 
dim_tipo_socorro <- function(dataset_veic) {
  G.LOG$log_dw(
    proc_msg = 'DIM: tipo_socorro',
    expr = {
      dims <- distinct(dataset_veic, tipo_socorro_sk, tipo_socorro_desc)
      dims <- arrange(dims, tipo_socorro_sk)
      dims
    }
  )
}
