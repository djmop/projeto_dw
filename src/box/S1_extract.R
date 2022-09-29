box::use(cli[cli_alert])
box::use(extractor = ./extract/E1_rawdata_download)
box::use(loader    = ./extract/E2_rawdata_load)


#' EXTRAÇÃO DE DADOS BRUTOS
#' @param download `boolean`. Realiza download, se verdadeiro.
#' @export
#' 
extract <- function(download = TRUE) {
  cat('\n\n')
  
  cli_alert('Carregando dados')
  
  
  ##  BAIXA CSVs
  ##  ----------
  if (download) extractor$download_raw_data()
  
  
  ##  CARREGA DADOS
  ##  -------------
  raw.data <- loader$load_raw_data()
  
  
  ##  CHECAGEM DE ERROS
  ##  -----------------
  loader$validate_loaded_data(raw.data)
  
  
  cli_alert('Dados carregados')
  
  return(raw.data$data)
}
