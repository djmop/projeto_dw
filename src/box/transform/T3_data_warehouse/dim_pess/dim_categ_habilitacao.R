box::use(dplyr[distinct, arrange, mutate, n, relocate])

box::use(G.LOG  = ../../../globals/Z2_global_logging)


#  DIM: Categoria habilitação  ------------------------------------------------------------
#' @export
#' 
dim_categ_habilitacao <- function(dataset_pess) {
  G.LOG$log_dw(
    proc_msg = 'DIM: categ_habilitacao',
    expr = {
      dims <- distinct(
        .data = dataset_pess,
        categ_habilitacao_nk, categ_habilitacao_desc
      )
      
      # Surrogate Key (PK)  ---- 
      dims <- arrange(.data = dims, categ_habilitacao_nk)
      dims <- mutate(.data = dims, categ_habilitacao_sk = 1:n())
      
      # Reorganiza estrutura  ----
      dims <- relocate(.data = dims, categ_habilitacao_sk)
      dims
    }
  )
}
