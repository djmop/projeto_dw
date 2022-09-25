#' Filtra registros completos
#' @param datasets. Lista de data.frames de entidades (sem dicionários).
#' @export
#' 
complete_records <- function(datasets) {
  box::use(
    dplyr[select, any_of, bind_rows, distinct, group_by,
          count, filter, mutate, rowwise, ungroup],
    purrr[map], magrittr[`%<>%`, `%>%`, not],
    cli[cli_process_start, cli_process_done, cli_process_failed]
  )
  
  tryCatch(
    expr = {
      cli_process_start('Removendo registros incompletos', .auto_close = F)
      
      occurrences <- map(datasets, ~select(., any_of('boletim_nk')))
      occurrences %<>% bind_rows()
      occurrences %<>% distinct()
      
      ##  REMOVE OCORRÊNCIAS DUPLICADAS E INCONSISTENTES
      ##  ----------------------------------------------
      duplicated_ocr <- occurrences %>%
        group_by(boletim_nk) %>%
        count() %>% 
        filter(n > 1)
      
      occurrences %<>% filter(not(boletim_nk %in% duplicated_ocr$boletim_nk))
      
      ##  SELECIONA APENAS REGISTROS COMPLETOS
      ##  ------------------------------------
      occurrences %<>% mutate(
        logr = boletim_nk %in% datasets[['logradouros']]$boletim_nk,
        ocor = boletim_nk %in% datasets[['ocorrencias']]$boletim_nk,
        pess = boletim_nk %in% datasets[['pessoas_envolvidas']]$boletim_nk,
        veic = boletim_nk %in% datasets[['veiculos_envolvidos']]$boletim_nk
      )
      
      occurrences %<>% rowwise() %>%
        filter(sum(logr, ocor, pess, veic) == 4) %>% 
        ungroup()
      
      datasets %<>% map(., function(x) {
        x %>% filter(boletim_nk %in% occurrences$boletim_nk)
      })
      
      cli_process_done()
      return(datasets)
    },
    
    error = function(e) {
      cli_process_failed()
      print(e)
      stop()
    }
  )
}
