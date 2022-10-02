box::use(OCR = ./dim_ocor/dim_ocorrencias)
box::use(OGB = ./dim_ocor/dim_origem_boletim)
box::use(PAV = ./dim_ocor/dim_pavimento)
box::use(REG = ./dim_ocor/dim_regional)
box::use(TMP = ./dim_ocor/dim_tempo)
box::use(TAC = ./dim_ocor/dim_tipo_acidente)
box::use(UPS = ./dim_ocor/dim_ups)

box::use(G.FUNC = ../../globals/Z4_global_functions)


#' Constrói dimensões de ocorrências
#' @param dataset_ocor `data.frame`. Tabela de ocorrências.
#' @export
#' 
build_dims <- function(dataset_ocor) {
  dataset_ocor <- G.FUNC$type_convert(df = dataset_ocor)
  
  ##  Dimensões  ----
  dims <- list()
  
  dims[['origem_boletim']] <- OGB$dim_origem_boletim(dataset_ocor)
  dims[['tipo_acidente']]  <- TAC$dim_tipo_acidente(dataset_ocor)
  dims[['pavimento']]      <- PAV$dim_pavimento(dataset_ocor)
  dims[['regional']]       <- REG$dim_regional(dataset_ocor)
  dims[['tempo']]          <- TMP$dim_tempo(dataset_ocor)
  dims[['ups']]            <- UPS$dim_ups(dataset_ocor)
  
  dims[['ocorrencias']] <- OCR$dim_ocorrencias(
    dataset_ocor       = dataset_ocor,
    dim_tipo_acidente  = dims$tipo_acidente,
    dim_origem_boletim = dims$origem_boletim
  )
  
  return(dims)
}
