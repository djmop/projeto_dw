
#   ============================================================================
#   [E] EXTRAÇÃO                                                            ####

##  IMPORTAÇÃO
##  ----------
box::use(
  extractor = ./box/E1_raw_data_download,
  loader    = ./box/E2_raw_data_load
)

box::reload(extractor)
box::reload(loader)


##  BAIXA CSVs
##  ----------
# extractor$download_raw_data()


##  CARREGA DADOS
##  -------------
raw.data <- loader$load_raw_data()


##  CHECAGEM DE ERROS
##  -----------------
if (length(raw.data$err) > 0) {
  purrr::walk2(
    names(raw.data$err),
    raw.data$err,
    ~cat(paste(.x, .y, sep = '\n'), '\n')
  )
  cli::cli_abort('Falha no carregamento de dados brutos')
}



##  LIBERA MEMÓRIA
##  --------------
box::unload(extractor)
box::unload(loader)





#   ============================================================================
#   [T1] TRANSFORMAÇÃO - PARTE 1                                            ####

##  IMPORTAÇÃO
##  ----------
box::use(
  transf_str = ./box/T1_transf_structure,
  transf_val = ./box/T2_transf_values
)

box::reload(transf_str)
box::reload(transf_val)


##  AGRUPA DADOS COM COLUNAS IDÊNTICAS
##  ----------------------------------
datasets <- purrr::map(raw.data$data, transf_str$bind_by_cols)


##  TRANSFORMAÇÃO ESTRUTURAL POR DATASET
##  ------------------------------------
datasets <- transf_str$str_transform(datasets)


##  TRANSFORMAÇÃO DE VALORES POR DATASET
##  ------------------------------------
datasets <- transf_val$val_transform(datasets)



##  LIBERA MEMÓRIA
##  --------------
box::unload(transf_str)
box::unload(transf_val)
rm(raw.data)
gc()





#   ============================================================================
#   [T2] TRANSFORMAÇÃO - PARTE 2 (DW)                                       ####

##  IMPORTAÇÃO
##  ----------
box::use(
  filters  = ./box/T3_dw_filter_records,
  dim_date = ./box/T4_dw_build_dim_date,
  dim_ocor = ./box/T5_dw_build_dim_ocor
)

box::reload(filters)
box::reload(dim_date)
box::reload(dim_ocor)


##  SEPARA DICIONÁRIOS
##  ------------------
dicts    <- purrr::map(datasets, ~ .[['dict']])
datasets <- purrr::map(datasets, ~ .[['data']])


##  REMOVE REGISTROS INCOMPLETOS OU INCONSISTENTES
##  ----------------------------------------------
datasets <- filters$complete_records(datasets)


##  CONSTRÓI DIMENSÕES
##  ------------------
dimensions <- list()

aux <- dim_date$build(datasets)
dimensions <- c(dimensions, aux)

aux <- dim_ocor$build(datasets)
dimensions <- c(dimensions, aux)



##  LIBERA MEMÓRIA
##  --------------
box::unload(filters)
box::unload(dim_date)
box::unload(dim_ocor)
rm(aux)
gc()






