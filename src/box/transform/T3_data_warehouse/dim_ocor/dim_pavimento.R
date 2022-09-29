box::use(dplyr[distinct, arrange, filter])
box::use(G.LOG  = ../../../globals/Z2_global_logging)

#  DIM: Pavimento  -------------------------------------------------------------
#' @export
#' 
dim_pavimento <- function(dataset_ocor) {
  G.LOG$log_dw(
    proc_msg = 'DIM: pavimento',
    expr = {
      dataset_ocor <- distinct(dataset_ocor, pavimento_sk, pavimento_desc)
      dataset_ocor <- arrange(dataset_ocor, pavimento_sk)
      filter(dataset_ocor, !is.na(pavimento_sk))
    }
  )
}
