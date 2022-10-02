box::use(dplyr[distinct, arrange, filter, select, group_by, ungroup, mutate])

box::use(G.LOG  = ../../../globals/Z2_global_logging)


#  DIM: Logradouro  ------------------------------------------------------------
#' @export
#' 
dim_logradouro <- function(dataset_logr) {
  G.LOG$oversee(
    proc_msg = 'DIM: logradouro',
    expr = {
      
      dims <- select(
        .data = dataset_logr,
        logradouro_sk,
        logradouro_tipo_nk,
        logradouro_tipo_desc,
        logradouro_nome,
        data_hora_boletim        # Apenas para filtragem
      )
      
      #  Removendo Missing Datas ----
      dims <- filter(.data = dims, !is.na(logradouro_sk))
      
      #  Removendo duplicatas totais ----
      dims <- distinct(
        .data = dims,
        logradouro_sk,
        logradouro_tipo_nk,
        logradouro_tipo_desc,
        logradouro_nome,
        .keep_all = TRUE
      )
      
      #  Removendo PKs duplicadas ----
      dims <- group_by(dims, logradouro_sk)
      dims <- mutate(dims, nchr_nm = nchar(logradouro_nome))
      dims <- filter(dims, nchr_nm == max(nchr_nm))
      dims <- filter(dims, data_hora_boletim == max(data_hora_boletim))
      dims <- ungroup(dims)
      
      #  Reordenando estrutura ----
      dims <- select(
        .data = dims,
        logradouro_sk,
        logradouro_tipo_nk,
        logradouro_tipo_desc,
        logradouro_nome
      )
      
      dims <- arrange(.data = dims, logradouro_sk)
      dims
    }
  )
}
