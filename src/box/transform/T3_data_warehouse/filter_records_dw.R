box::use(
  dplyr[select, any_of, bind_rows, distinct, group_by],
  dplyr[count, filter, mutate, rowwise, ungroup],
  purrr[map],
  magrittr[not]
)
box::use(G.LOG = ../../globals/Z2_global_logging)


#' Filtra registros completos
#' @param datasets. Lista de data.frames de entidades (sem dicionários).
#' @export
#' 
complete_records <- function(data_sets) {
  G.LOG$oversee(
    proc_msg = 'Eliminando inconsistências',
    
    expr = {
      bol <- map(data_sets, ~select(., any_of('boletim_nk')))
      bol <- bind_rows(bol)
      bol <- distinct(bol)
      
    # Remove registros com duplicatas ----
      bol.dpl <- group_by(.data = bol, boletim_nk)
      bol.dpl <- count(x = bol.dpl) 
      bol.dpl <- filter(.data = bol.dpl, n > 1)
      
      bol <- filter(.data = bol, not(boletim_nk %in% bol.dpl$boletim_nk))
      
    # Seleciona apenas registros completos ----
      bol <- mutate(
        .data = bol,
        logr = boletim_nk %in% data_sets[['logradouros']]$boletim_nk,
        ocor = boletim_nk %in% data_sets[['ocorrencias']]$boletim_nk,
        pess = boletim_nk %in% data_sets[['pessoas_envolvidas']]$boletim_nk,
        veic = boletim_nk %in% data_sets[['veiculos_envolvidos']]$boletim_nk
      )
      
      bol <- rowwise(bol)
      bol <- filter(.data = bol, sum(logr, ocor, pess, veic) == 4)
      bol <- ungroup(bol)
      
      filtered <- map(data_sets, function(x) {
        filter(.data = x, boletim_nk %in% bol$boletim_nk)
      })
      
      filtered
    }
  )
}
