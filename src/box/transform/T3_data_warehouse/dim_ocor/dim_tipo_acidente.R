box::use(dplyr[distinct, arrange, mutate, n, relocate])
box::use(G.LOG  = ../../../globals/Z2_global_logging)

#  DIM: Tipo acidente  ---------------------------------------------------------
#' @export
#' 
dim_tipo_acidente <- function(dataset_ocor) {
  G.LOG$oversee(
    proc_msg = 'DIM: tipo_acidente',
    expr = {
      dataset_ocor <- distinct(dataset_ocor, acidente_tipo_nk, acidente_tipo_desc)
      dataset_ocor <- arrange(dataset_ocor, acidente_tipo_nk)
      dataset_ocor <- mutate(dataset_ocor, acidente_tipo_sk = 1:n())
      relocate(dataset_ocor, acidente_tipo_sk)
    }
  )
}


