
#   ============================================================================
#   IMPORTAÇÃO                                                              ####
box::use(
  extractor  = ./box/raw_data_download,
  loader     = ./box/load_raw_data,
  str_transf = ./box/structural_transformation,
  val_transf = ./box/value_transformation
)

box::reload(extractor)
box::reload(loader)
box::reload(str_transf)
box::reload(val_transf)



#   ============================================================================
#   [E] EXTRAÇÃO                                                            ####

##  BAIXA CSVs
##  ----------
extractor$download_raw_data()


##  CARREGA DADOS
##  -------------
raw.data <- loader$load_raw_data()


##  CHECA ERROS
##  -----------
if (length(raw.data$err) > 0) {
  purrr::walk2(
    names(raw.data$err),
    raw.data$err,
    ~cat(paste(.x, .y, sep = '\n'), '\n')
  )
  cli::cli_abort('Falha no carregamento de dados brutos')
}



#   ============================================================================
#   [T1] TRANSFORMAÇÃO - PARTE 1                                            ####

##  AGRUPA DADOS COM COLUNAS IDÊNTICAS
##  ----------------------------------
datasets <- purrr::map(raw.data$data, str_transf$bind_by_cols)


##  TRANSFORMAÇÃO ESTRUTURAL POR DATASET
##  ------------------------------------
datasets <- str_transf$str_transform(datasets)


##  TRANSFORMAÇÃO DE VALORES POR DATASET
##  ------------------------------------
datasets <- val_transf$val_transform(datasets)


##  LIBERA MEMÓRIA
##  --------------
rm(raw.data)
gc()



#   ============================================================================
#   [T2] TRANSFORMAÇÃO - PARTE 2 (DW)                                       ####



