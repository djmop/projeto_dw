box::use(dplyr[distinct, mutate, case_when, relocate, arrange])
box::use(G.LOG  = ../../../globals/Z2_global_logging)

#  DIM: Origem boletim  --------------------------------------------------------
#' @export
#' 
dim_origem_boletim <- function(dataset_ocor) {
  G.LOG$log_dw(
    proc_msg = 'DIM: origem_boletim',
    expr = {
      dataset_ocor <- distinct(dataset_ocor, boletim_origem) 
      dataset_ocor <- mutate(
        dataset_ocor,
        boletim_origem_sk = case_when(
          boletim_origem == 'NAO INFORMADO'      ~ 0,
          boletim_origem == 'CORPO DE BOMBEIROS' ~ 1,
          boletim_origem == 'POLICIA CIVIL'      ~ 2,
          boletim_origem == 'POLICIA MILITAR'    ~ 3,
          boletim_origem == 'POLICIA RODOVIARIA' ~ 4,
          TRUE ~ -1
        )
      )
      dataset_ocor <- relocate(dataset_ocor, boletim_origem_sk)
      arrange(dataset_ocor, boletim_origem_sk)
    }
  )
}
