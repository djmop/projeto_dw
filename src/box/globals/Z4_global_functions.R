#  .............................................................................
#  FUNÇÕES GLOBAIS                                                          ####

## func: Conversão de tipos ( + ) ----------------------------------------------
#' @title Converte tipos de dados em tabelas
#' @param df `data.frame`
#' @export
#' 
type_convert <- function(df) {
  box::use(readr[type_convert, cols, col_guess])
  box::use(purrr[some])
  
  has_chr_col <- some(df, is.character)
  if (!has_chr_col) {
    return(df)
  }
  
  converted.df <- type_convert(df = df, na = character(),
                               col_types = cols(.default = col_guess()))
  
  return(converted.df)
}
