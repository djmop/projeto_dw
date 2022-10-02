box::use(
  dplyr[bind_rows, rowwise, mutate, ungroup, rename, any_of],
  janitor[make_clean_names],
  purrr[map, keep, every],
  checkmate[assert_list, check_data_frame, check_true],
  checkmate[makeAssertCollection, reportAssertions],
  stringr[str_detect],
  glue[glue]
)

box::use(ASRT    = ../../globals/Z5_global_assertions)
box::use(G.LOG   = ../../globals/Z2_global_logging)
box::use(G.CONST = ../../globals/Z3_global_constants)



## Concatena conjuntos de data.frames com colunas idênticas ( + ) ==============
#' 
#' @description Data.frames contendo colunas com nomes e ordens idênticos
#'   são concatenados em um data.frame único. 
#' @param data_sets Lista de data.frames.
#' @export
#' 
bind_by_cols <- function(data_sets, set_names) {
  G.LOG$oversee(
    proc_msg = glue('Detectando conjuntos:  {set_names}', .trim = F),
    
    expr = {
      ASRT$assert_depth(data_sets, 1, check_data_frame)
      
      data.col.sets <- list()
      col.sets      <- list()
      snb <- set_name_builder$new()
      
      for (i in seq_along(data_sets)) {
        df.name <- names(data_sets[i])
        df.data <- data_sets[[i]];
        is.dict <- str_detect(df.name, G.CONST$regex_patts$dict_files)
        col.set <- keep(col.sets, ~ identical(., colnames(df.data)))
        
        if (length(col.set) > 0)
          colset.name <- names(col.set)
        else
          colset.name <- snb$build_name(is.dict)
        
        data.col.sets[[colset.name]][[df.name]] <- df.data
      }
      
      data.sets <- map(data.col.sets, bind_rows, .id = 'arquivo_origem')
      data.sets
    }    
  )
}


## Transformação estrutural ( + ) ==============================================
#' @param data_colsets `list`. Conjuntos de datasets similares.
#' @param set_name `string`. Nome do dataset.
#' @export
#' 
str_transform <- function(data_colsets, set_name) {
  data.set <- G.LOG$oversee(
    glue('Ajustando estrutura:   {set_name}', .trim = F),
    expr = { do_transform(data_colsets) }
  )
  return(data.set)
}



## Função de transformação ( - ) ===============================================
#' @description Função geral de transformação estrutural
#' 
do_transform <- function(data_colsets) {
  ASRT$assert_depth(data_colsets, 1, check_data_frame)
  ASRT$assert_depth(data_colsets, 1, function(x) {
    check_true(nrow(x) > 0 & ncol(x) > 0)
  })
  
  is.dict <- str_detect(names(data_colsets), 'D')
  dicts   <- data_colsets[is.dict]
  dataset <- data_colsets[!is.dict]
  
# Processing dictionaries ------------------------------------------------------
  dicts <- bind_rows(dicts)
  dicts <- rowwise(dicts)
  dicts <- mutate(dicts, nome_do_campo = make_clean_names(nome_do_campo))
  dicts <- ungroup(dicts)

# Processing datasets ----------------------------------------------------------  
  dataset <- map(dataset, ~ rename(., any_of(G.CONST$constants$COLNAME_CONV)))
  dataset <- bind_rows(dataset)
  
  
  output <- list(data = dataset, dict = dicts)
  return(output)
}



## Gera nome para os colsets ( - ) =============================================
set_name_builder <- R6::R6Class(
  'set_name_builder',
  private = list(
    d.num = 0, s.num = 0,
    get_prefix  = function(is.dict) {
      ifelse(is.dict, 'D', 'S')
    },
    get_counter = function(is.dict) {
      if (is.dict) {
        private$d.num <- private$d.num + 1
        return(private$d.num)
      } else {
        private$s.num <- private$s.num + 1
        return(private$s.num)
      }
    }
  ),
  public = list(
    build_name = function(is.dict) {
      name <- glue(private$get_prefix(is.dict), private$get_counter(is.dict))
      return(name)
    }
  )
)
