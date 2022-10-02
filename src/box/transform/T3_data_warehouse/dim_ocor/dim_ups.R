box::use(dplyr[distinct, arrange, filter])
box::use(G.LOG  = ../../../globals/Z2_global_logging)

#  DIM: UPS  -------------------------------------------------------------------
#' @export
#' 
dim_ups <- function(dataset_ocor) {
  G.LOG$oversee(
    proc_msg = 'DIM: ups',
    expr = {
      dataset_ocor <- distinct(dataset_ocor, ups_valor_sk, ups_desc)
      dataset_ocor <- arrange(dataset_ocor, ups_valor_sk)
      filter(dataset_ocor, !is.na(ups_valor_sk))
    }
  )
}
