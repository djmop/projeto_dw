box::use(cli[cli_alert])
box::use(filters = ./transform/T3_data_warehouse/filter_records_dw)
box::use(dw_date = ./transform/T3_data_warehouse/build_date_dw)
box::use(dw_ocor = ./transform/T3_data_warehouse/build_ocor_dw)
box::use(dw_logr = ./transform/T3_data_warehouse/build_logr_dw)
box::use(dw_veic = ./transform/T3_data_warehouse/build_veic_dw)
box::use(dw_pess = ./transform/T3_data_warehouse/build_pess_dw)

#' TRANSFORMAÇÃO - PARTE 2 (DW)
#' @param dtsets `list`. Lista de dtsets.
#' @export
#' 
transform_dw <- function(dtsets) {
  cat('\n\n')
  
  cli_alert('Construindo tabelas do Data Warehouse')
  
  
  ##  REMOVE REGISTROS INCOMPLETOS OU INCONSISTENTES
  ##  ----------------------------------------------
  dtsets <- purrr::map(dtsets, ~ .[['data']])
  dtsets <- filters$complete_records(dtsets)
  
  
  ##  CONSTRÓI TABELAS DATA WAREHOUSE
  ##  -------------------------------
  dw <- list()
  
  # DIM: DATA
  # ---------
  dw[['dim.data']] <- dw_date$build_dims(dtsets)
  
  # DIMS: OCORRÊNCIAS
  # -----------------
  aux <- dw_ocor$build_dims(dtsets$ocorrencias)
  dw[['dim.ocorrencias']]         <- aux$ocorrencias
  dw[['dim.ocor.origem_boletim']] <- aux$origem_boletim
  dw[['dim.ocor.tipo_acidente']]  <- aux$tipo_acidente
  dw[['dim.ocor.pavimento']]      <- aux$pavimento
  dw[['dim.ocor.valor_ups']]      <- aux$valor_ups
  dw[['dim.ocor.regional']]       <- aux$regional
  dw[['dim.ocor.tempo']]          <- aux$tempo
  
  # DIMS: LOGRADOUROS
  # -----------------
  aux <- dw_logr$build_dims(dtsets$logradouros)
  dw[['dim.logr.logradouro']] <- aux$logradouro
  dw[['dim.logr.bairro']]     <- aux$bairro
  
  # DIMS: VEÍCULOS
  # --------------
  aux <- dw_veic$build_dims(dtsets$veiculos_envolvidos)
  dw[['dim.especie_veiculo']]        <- aux$especie_veiculo
  dw[['dim.veic.categoria_veiculo']] <- aux$categoria_veiculo
  dw[['dim.veic.situacao_veiculo']]  <- aux$situacao_veiculo
  dw[['dim.veic.tipo_socorro']]      <- aux$tipo_socorro
  
  # DIMS: PESSOAS
  # -------------
  aux <- dw_pess$build_dims(dtsets$pessoas_envolvidas)
  dw[['dim.pess.categ_habilitacao']] <- aux$categ_habilitacao
  dw[['dim.pess.severidade']]        <- aux$severidade
  dw[['dim.pess.sexo']]              <- aux$sexo
  
  
  # FATO: LOGRADOUROS ENVOLVIDOS
  # ----------------------------
  aux <- dw_logr$build_fact(dtsets$logradouros, dw[['dim.ocorrencias']])
  dw[['fato.logradouros_envolvidos']] <- aux
  
  # FATO: VEÍCULOS ENVOLVIDOS
  # -------------------------
  aux <- dw_veic$build_fact(dtsets$veiculos_envolvidos, dw[['dim.ocorrencias']])
  dw[['fato.veiculos_envolvidos']] <- aux
  
  # FATO: PESSOAS ENVOLVIDAS
  # ------------------------
  aux <- dw_pess$build_fact(
    dataset_pess     = dtsets$pessoas_envolvidas,
    dim_especie_veic = dw[['dim.especie_veiculo']],
    dim_categ_hab    = dw[['dim.pess.categ_habilitacao']],
    dim_ocor         = dw[['dim.ocorrencias']],
    dim_sexo         = dw[['dim.pess.sexo']]
  )
  dw[['fato.pessoas_envolvidas']] <- aux
  
  
  
  cli_alert('Tabelas do Data Warehouse finalizadas')
  cat('\n\n')
  
  return(dw)
}



