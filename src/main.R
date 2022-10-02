setwd(base::file.path(project_root, 'src'))


run_etl <- function(proj_root, download = TRUE, log = TRUE) {
  box::use(fs[dir_ls])
  box::use(
    G.PATH = ./box/globals/Z1_global_paths,
    G.LOG  = ./box/globals/Z2_global_logging,
    EXT    = ./box/S1_extract,
    TRD    = ./box/S2_transform_raw_data,
    TDW    = ./box/S3_transform_dw
  )
  
  tryCatch(
    expr = {
      logger <- G.LOG$Logger$new(output = log)
      logger$start('PROCESSO ETL')
      
      rawdir_path <- G.PATH$dirs$raw_data
      has_rawdata <- FALSE
      
      if (dir.exists(rawdir_path)) {
        rawfiles <- dir_ls(rawdir_path, recurse = T, glob = '*.csv')
        has_rawdata <- length(rawfiles) > 0
      }
      
      if (!has_rawdata)
        download <- TRUE
      
    # EXTRAÇÃO  ================================================================
      logger$add_title('Extração')
      raw.data <- EXT$extract(download = download)
      
      
    # TRANSFORMAÇÃO  ===========================================================
      logger$add_title('Transformação')
      data.sets      <- TRD$transform_raw_data(raw.data)
      
      logger$add_title('Construção do DW')
      data.warehouse <- TDW$transform_dw(data.sets)
      
      rm(raw.data, data.sets)
      
      
    # CARGA  ===================================================================
      dim_data                    <<- data.warehouse$dim.data
      dim_ocorrencias             <<- data.warehouse$dim.ocorrencias
      dim_ocor_origem_boletim     <<- data.warehouse$dim.ocor.origem_boletim
      dim_ocor_tipo_acidente      <<- data.warehouse$dim.ocor.tipo_acidente
      dim_ocor_pavimento          <<- data.warehouse$dim.ocor.pavimento
      dim_ocor_ups                <<- data.warehouse$dim.ocor.ups
      dim_ocor_regional           <<- data.warehouse$dim.ocor.regional
      dim_ocor_tempo              <<- data.warehouse$dim.ocor.tempo
      dim_logr_logradouro         <<- data.warehouse$dim.logr.logradouro
      dim_logr_bairro             <<- data.warehouse$dim.logr.bairro
      dim_especie_veiculo         <<- data.warehouse$dim.especie_veiculo
      dim_veic_categoria_veiculo  <<- data.warehouse$dim.veic.categoria_veiculo
      dim_veic_situacao_veiculo   <<- data.warehouse$dim.veic.situacao_veiculo
      dim_veic_tipo_socorro       <<- data.warehouse$dim.veic.tipo_socorro
      dim_pess_categ_habilitacao  <<- data.warehouse$dim.pess.categ_habilitacao
      dim_pess_severidade         <<- data.warehouse$dim.pess.severidade
      dim_pess_sexo               <<- data.warehouse$dim.pess.sexo
      fato_logradouros_envolvidos <<- data.warehouse$fato.logradouros_envolvidos
      fato_veiculos_envolvidos    <<- data.warehouse$fato.veiculos_envolvidos
      fato_pessoas_envolvidas     <<- data.warehouse$fato.pessoas_envolvidas
      
      rm(data.warehouse)
      
      logger$add_title('Finalizando')
      logger$close()
    },
    
    error = function(e) {
      logger$dump(e$call, e$message)
      stop(e)
    },
    
    finally = {
    # LIBERA MEMÓRIA  ==========================================================
      rm(logger)
      box::unload(G.LOG)
      box::unload(EXT)
      box::unload(TRD)
      box::unload(TDW)
      gc()
    }  
  )
}


