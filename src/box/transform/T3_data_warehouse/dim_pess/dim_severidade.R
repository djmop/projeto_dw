box::use(dplyr[distinct, arrange])

box::use(G.LOG  = ../../../globals/Z2_global_logging)


#  DIM: Severidade  ------------------------------------------------------------
#' @export
#' 
dim_severidade <- function(dataset_pess) {
  G.LOG$log_dw(
    proc_msg = 'DIM: severidade',
    expr = {
      dims <- distinct(.data = dataset_pess, severidade_sk, severidade_desc)
      dims <- arrange(.data = dims, severidade_sk)
      dims
    }
  )
}
