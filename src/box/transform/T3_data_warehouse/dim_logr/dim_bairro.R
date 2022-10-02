box::use(dplyr[distinct, arrange, filter, select, group_by, ungroup, mutate])

box::use(G.LOG  = ../../../globals/Z2_global_logging)


#  DIM: Bairro  ----------------------------------------------------------------
#' @export
#' 
dim_bairro <- function(dataset_logr) {
  G.LOG$oversee(
    proc_msg = 'DIM: bairro',
    expr = {
      
      dims <- select(
        .data = dataset_logr,
        bairro_sk,
        bairro_tipo_nk, 
        bairro_tipo_desc,
        bairro_nome,
        data_hora_boletim        # Apenas para filtragem
      )
      
      #  Removendo Missing Datas ----
      dims <- filter(.data = dims, !is.na(bairro_sk))
      
      #  Removendo duplicatas totais ----
      dims <- distinct(
        .data = dims,
        bairro_sk,
        bairro_tipo_nk, 
        bairro_tipo_desc,
        bairro_nome,
        .keep_all = TRUE
      )
      
      #  Removendo PKs duplicadas ----
      dims <- group_by(dims, bairro_sk)
      dims <- mutate(dims, nchr_nm = nchar(bairro_nome))
      dims <- filter(dims, nchr_nm == max(nchr_nm))
      dims <- filter(dims, data_hora_boletim == max(data_hora_boletim))
      dims <- ungroup(dims)
      
      #  Reordenando estrutura ----
      dims <- select(
        .data = dims,
        bairro_sk,
        bairro_tipo_nk, 
        bairro_tipo_desc,
        bairro_nome
      )
      
      dims <- arrange(.data = dims, bairro_sk)
      dims
    }
  )
}
