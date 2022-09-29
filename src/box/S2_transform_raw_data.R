box::use(cli[cli_alert])
box::use(transf_str = ./transform/T1_structure/structure_transf)
box::use(transf_val = ./transform/T2_values/value_transf)


#' TRANSFORMAÇÃO - PARTE 1 (ESTRUTURA E VALORES)
#' @param raw_data list. Dados brutos.
#' @export
#' 
transform_raw_data <- function(raw_data) {
  cat('\n\n')
  
  cli_alert('Processando dados brutos')
  
# AGRUPA DADOS COM COLUNAS IDÊNTICAS  ------------------------------------------
  datasets <- purrr::map2(
    raw_data, names(raw_data),
    ~ transf_str$bind_by_cols(.x, .y)
  )
  
# TRANSFORMAÇÃO  ---------------------------------------------------------------
  for (i in seq_along(datasets)) {
    data_set <- datasets[[i]]
    set_name <- names(datasets)[i]

    data_set <- transf_str$str_transform(data_set, set_name)
    data_set <- transf_val$val_transform(data_set, set_name)

    datasets[[i]] <- data_set
  }
  
  cli_alert('Dados brutos processados')
  
  return(datasets)
}
