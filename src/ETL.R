box::use(G.LOG = ./box/globals/Z2_global_logging)
box::use(EXT   = ./box/S1_extract)
box::use(TRD   = ./box/S2_transform_raw_data)
box::use(TDW   = ./box/S3_transform_dw)


logfile <- G.LOG$start_log()
clock <- G.LOG$Clock$new()

tryCatch(
  expr = {
    cli::cli_alert('Processo iniciado em: {Sys.time()}')
    
    clock$tic()
    
    #  EXTRAÇÃO  ===============================================================
    raw.data <- EXT$extract(download = FALSE)
    
    #  TRANSFORMAÇÃO  ==========================================================
    data.sets      <- TRD$transform_raw_data(raw.data)
    data.warehouse <- TDW$transform_dw(data.sets)
    
    rm(raw.data, data.sets)
    
    
    #  CARGA  ==================================================================
    dim_data                    <- data.warehouse$dim.data
    dim_ocorrencias             <- data.warehouse$dim.ocorrencias
    dim_ocor_origem_boletim     <- data.warehouse$dim.ocor.origem_boletim
    dim_ocor_tipo_acidente      <- data.warehouse$dim.ocor.tipo_acidente
    dim_ocor_pavimento          <- data.warehouse$dim.ocor.pavimento
    dim_ocor_valor_ups          <- data.warehouse$dim.ocor.valor_ups
    dim_ocor_regional           <- data.warehouse$dim.ocor.regional
    dim_ocor_tempo              <- data.warehouse$dim.ocor.tempo
    dim_logr_logradouro         <- data.warehouse$dim.logr.logradouro
    dim_logr_bairro             <- data.warehouse$dim.logr.bairro
    dim_especie_veiculo         <- data.warehouse$dim.especie_veiculo
    dim_veic_categoria_veiculo  <- data.warehouse$dim.veic.categoria_veiculo
    dim_veic_situacao_veiculo   <- data.warehouse$dim.veic.situacao_veiculo
    dim_veic_tipo_socorro       <- data.warehouse$dim.veic.tipo_socorro
    dim_pess_categ_habilitacao  <- data.warehouse$dim.pess.categ_habilitacao
    dim_pess_severidade         <- data.warehouse$dim.pess.severidade
    dim_pess_sexo               <- data.warehouse$dim.pess.sexo
    fato_logradouros_envolvidos <- data.warehouse$fato.logradouros_envolvidos
    fato_veiculos_envolvidos    <- data.warehouse$fato.veiculos_envolvidos
    fato_pessoas_envolvidas     <- data.warehouse$fato.pessoas_envolvidas
    
    rm(data.warehouse)
    
    cli::cli_alert('Processo finalizado em: {Sys.time()}')
    cli::cli_alert('Tempo total decorrido: {clock$toc()}')
  },
  
  error = function(e) {
    cat('\n\n')
    cli::cli_alert_danger('[ERROR] Processo ABORTADO em: {Sys.time()}')
    cat('\nMENSAGEM: \n')
    print(e)
    stop(e)
  },
  
  finally = {
    #  LIBERA MEMÓRIA  =========================================================
    rm(clock)
    box::unload(EXT)
    box::unload(TRD)
    box::unload(TDW)
    G.LOG$close_log(logfile)
    box::unload(G.LOG)
    gc()
  }  
)
