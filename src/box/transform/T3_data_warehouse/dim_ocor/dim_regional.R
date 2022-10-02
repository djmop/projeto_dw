box::use(dplyr[distinct, arrange, filter])
box::use(G.LOG  = ../../../globals/Z2_global_logging)

#  DIM: Regional  --------------------------------------------------------------
#' @export
#' 
dim_regional <- function(dataset_ocor) {
  G.LOG$oversee(
    proc_msg = 'DIM: regional',
    expr = {
      dataset_ocor <- distinct(dataset_ocor, regional_sk, regional_desc)
      dataset_ocor <- arrange(dataset_ocor, regional_sk)
      filter(dataset_ocor, !is.na(regional_sk))
    }
  )
}
