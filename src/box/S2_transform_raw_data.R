box::use(transf_str = ./transform/T1_structure/structure_transf)
box::use(transf_val = ./transform/T2_values/value_transf)
box::use(ASRT = ./globals/Z5_global_assertions)
box::use(
  checkmate[assert_list, check_data_frame],
  purrr[map2],
  glue[glue]
)


#' TRANSFORMAÇÃO - PARTE 1 (ESTRUTURA E VALORES)
#' @param raw_data list. Dados brutos.
#' @export
#' 
transform_raw_data <- function(raw_data) {
  assert_list(raw_data)
  ASRT$assert_depth(raw_data, 2, check_data_frame)
  
# AGRUPA DADOS COM COLUNAS IDÊNTICAS  ------------------------------------------
  data_sets <- purrr::map2(
    raw_data, names(raw_data),
    ~ transf_str$bind_by_cols(.x, .y)
  )
  
# TRANSFORMAÇÃO  ---------------------------------------------------------------
  for (i in seq_along(data_sets)) {
    data_set <- data_sets[[i]]
    set_name <- names(data_sets)[i]
    data_set <- transf_str$str_transform(data_set, set_name)
    data_set <- transf_val$val_transform(data_set, set_name)
    data_sets[[i]] <- data_set
  }
  
  return(data_sets)
}

