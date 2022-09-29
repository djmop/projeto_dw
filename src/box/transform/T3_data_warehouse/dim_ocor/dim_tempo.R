box::use(dplyr[distinct, arrange, filter])
box::use(G.LOG  = ../../../globals/Z2_global_logging)

#  DIM: Tempo  -----------------------------------------------------------------
#' @export
#' 
dim_tempo <- function(dataset_ocor) {
  G.LOG$log_dw(
    proc_msg = 'DIM: tempo',
    expr = {
      dataset_ocor <- distinct(dataset_ocor, tempo_sk, tempo_desc)
      dataset_ocor <- arrange(dataset_ocor, tempo_sk)
      filter(dataset_ocor, !is.na(tempo_sk))
    }
  )
}
