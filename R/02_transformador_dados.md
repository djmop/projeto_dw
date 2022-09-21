# Transformação de dados - Projeto BI (PUC)

Daniel T. Nunes 2022-09-11 03:00

## Pacotes

``` r
library(stringr)
library(tibble)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)

library(lubridate)
library(janitor)
library(glue)

library(magrittr)
library(here)
```

## Carregamento

``` r
load.csvs <- function(dir.paths) {
  csvs <- list()
  
  for (dir.path in dir.paths) {
    dir.name <- basename(dir.path)
    csv.files <- fs::dir_ls(
      here(dir.path),
      type = 'file',
      glob = '*.csv'
    )
    
    logs <- list(e = list(), w = list())
    
    tryCatch(
      expr = {
        cli::cli_process_start(glue::glue("Leitura de csvs: `{dir.name}`"))
        
        csv.dfs <- map(
          csv.files,
          ~read_delim(
            .,
            delim          = ';',
            locale         = locale(encoding = 'latin1'),
            show_col_types = FALSE,
            progress       = FALSE,
            col_types      = cols(.default = col_character()),
            name_repair    = 'minimal'
          )
        )
        
        csv.dfs <- map(csv.dfs, ~janitor::clean_names(.[]))
        names(csv.dfs)   <- basename(str_remove(csv.files, r'{\.csv}'))
        csvs[[dir.name]] <- csv.dfs
        
        cli::cli_process_done()
      },
      error = function(e) {
        cli::cli_process_failed()
        i <- length(logs$e) + 1
        logs$err[[i]] <- e
      },
      
      warning = function(w) {
        j <- length(logs$w) + 1
        logs$wrn[[j]] <- w
      }
    )
  }
  
  return(list(log = logs, data = csvs))
}
```

``` r
dir.paths <- fs::dir_ls(here('raw_data'), type = 'directory')
csvs <- load.csvs(dir.paths)

detach("package:readr", unload=TRUE)
```

<br />

## Transformação

### [Organizando variações estruturais]{.smallcaps}

``` r
#' Create sets of data.frames with identical column names.
#' 
#' @description Data.frames comprising identical column names and
#'   column order are put into the same set for further treatment.
#' @param x List of data.frames.
#' @return List of grouped data.frames with identical columns.
#' 
group_by_cols <- function(x) {
  assertthat::assert_that(every(x, is.data.frame))
  
  col.sets <- list()
  df.sets  <- list()
  
  s = 0
  for (i in seq_along(x)) {
    df <- x[[i]]
    col.set <- keep(col.sets, ~ identical(., colnames(df)))
    
    if (length(col.set) == 0) {
      s = s + 1
      col.set.nm <- glue("S{s}")
      col.sets[[col.set.nm]] <- colnames(df)
    } else {
      col.set.nm <- names(col.set)
    }
    
    df.nm <- names(x[i])
    df.sets[[col.set.nm]][[df.nm]] <- df
  }
  return(df.sets)
}
```

``` r
##  Concatena datasets comuns
##  -------------------------
df.sets <- map(csvs$data, group_by_cols)
df.sets <- map(df.sets, ~map(., bind_rows, .id = "arquivo_original"))
```

<br />

### [Transformação estrutural por dataset]{.smallcaps}

``` r
output <- list()
```

#### *Logradouros*

``` r
dataset <- df.sets$logradouros

##  Dicionários
##  -----------
dataset$S1 %<>% rowwise() %>%
  mutate(nome_do_arquivo = make_clean_names(nome_do_arquivo)) %>%
  ungroup()

output[['logradouros']][['dict']] <- dataset$S1
dataset['S1'] <- NULL


##  Conversão de nomes de campos
##  ----------------------------
kpitoolkit::compare_colnames(dataset, sort = TRUE)
```

    ## # A tibble: 25 × 4
    ##    colnames                    S2    S3    S4
    ##    <chr>                    <dbl> <dbl> <dbl>
    ##  1 arquivo_original             1     1     1
    ##  2 data_boletim                 1     1     1
    ##  3 descricao_tipo_bairro        1     1     1
    ##  4 no_bairro                   NA     1     1
    ##  5 no_boletim                  NA     1     1
    ##  6 no_imovel                   NA     1     1
    ##  7 no_imovel_proximo           NA     1     1
    ##  8 no_logradouro               NA     1     1
    ##  9 no_municipio                NA     1     1
    ## 10 nome_bairro                  1     1     1
    ## 11 nome_logradoro_anterior      1     1    NA
    ## 12 nome_logradouro              1     1     1
    ## 13 nome_logradouro_anterior    NA    NA     1
    ## 14 nome_municipio               1     1     1
    ## 15 numero_bairro                1    NA    NA
    ## 16 numero_boletim               1    NA    NA
    ## 17 numero_imovel                1    NA    NA
    ## 18 numero_imovel_proximo        1    NA    NA
    ## 19 numero_logradouro            1    NA    NA
    ## 20 numero_municipio             1    NA    NA
    ## 21 seq_logradouros             NA     1     1
    ## 22 sequencia_logradouros        1    NA    NA
    ## 23 tipo_bairro                  1     1     1
    ## 24 tipo_logradouro              1     1     1
    ## 25 tipo_logradouro_anterior     1     1     1

``` r
conversions <- c(
  data_hora_boletim        = 'data_boletim',
  num_bairro               = 'no_bairro',
  num_boletim              = 'no_boletim',
  num_imovel               = 'no_imovel',
  num_imovel_proximo       = 'no_imovel_proximo',
  num_logradouro           = 'no_logradouro',
  num_municipio            = 'no_municipio',
  num_bairro               = 'numero_bairro',
  num_boletim              = 'numero_boletim',
  num_imovel               = 'numero_imovel',
  num_imovel_proximo       = 'numero_imovel_proximo',
  num_logradouro           = 'numero_logradouro',
  num_municipio            = 'numero_municipio',
  nome_logradouro_anterior = 'nome_logradoro_anterior',
  sequencia_logradouros    = 'seq_logradouros'
)

dataset <- map(dataset, ~ rename(., any_of(conversions)))

kpitoolkit::compare_colnames(dataset, sort = TRUE)
```

    ## # A tibble: 17 × 4
    ##    colnames                    S2    S3    S4
    ##    <chr>                    <dbl> <dbl> <dbl>
    ##  1 arquivo_original             1     1     1
    ##  2 data_hora_boletim            1     1     1
    ##  3 descricao_tipo_bairro        1     1     1
    ##  4 nome_bairro                  1     1     1
    ##  5 nome_logradouro              1     1     1
    ##  6 nome_logradouro_anterior     1     1     1
    ##  7 nome_municipio               1     1     1
    ##  8 num_bairro                   1     1     1
    ##  9 num_boletim                  1     1     1
    ## 10 num_imovel                   1     1     1
    ## 11 num_imovel_proximo           1     1     1
    ## 12 num_logradouro               1     1     1
    ## 13 num_municipio                1     1     1
    ## 14 sequencia_logradouros        1     1     1
    ## 15 tipo_bairro                  1     1     1
    ## 16 tipo_logradouro              1     1     1
    ## 17 tipo_logradouro_anterior     1     1     1

``` r
##  Fusão de tabelas
##  ----------------
dataset %<>% bind_rows()

output[['logradouros']][['data']] <- dataset
```

<br />

#### *Ocorrências*

``` r
dataset <- df.sets$ocorrencias

##  Dicionários
##  -----------
dataset$S1 %<>% rowwise() %>%
  mutate(nome_do_arquivo = make_clean_names(nome_do_arquivo)) %>%
  ungroup()

##  Conversão de nomes de campos
##  ----------------------------
conversions <- c(
  num_boletim = 'numero_boletim',
  desc_ups    = 'descricao_ups'
)

dataset[['S2']] %<>% rename(any_of(conversions))

output[['ocorrencias']][['dict']] <- dataset$S1
output[['ocorrencias']][['data']] <- dataset$S2
```

<br />

#### *Pessoas envolvidas*

``` r
dataset <- df.sets$pessoas_envolvidas

##  Dicionários
##  -----------
dataset$S1 %<>% rowwise() %>%
  mutate(nome_do_arquivo = make_clean_names(nome_do_arquivo)) %>%
  ungroup()

output[['pessoas_envolvidas']][['dict']] <- dataset$S1
dataset['S1'] <- NULL


##  Conversão de nomes de campos
##  ----------------------------
kpitoolkit::compare_colnames(dataset, sort = TRUE)
```

    ## # A tibble: 25 × 5
    ##    colnames                  S2    S3    S4    S5
    ##    <chr>                  <dbl> <dbl> <dbl> <dbl>
    ##  1 arquivo_original           1     1     1     1
    ##  2 categoria_habilitacao      1     1     1     1
    ##  3 cinto_seguranca            1     1     1     1
    ##  4 cod_severidade            NA    NA     1     1
    ##  5 cod_severidade_antiga      1     1     1     1
    ##  6 codigo_severidade          1     1    NA    NA
    ##  7 condutor                   1     1     1     1
    ##  8 data_hora_boletim          1     1     1     1
    ##  9 declaracao_obito           1     1     1     1
    ## 10 desc_severidade            1     1     1     1
    ## 11 descricao_habilitacao      1     1     1     1
    ## 12 embreagues                 1     1     1     1
    ## 13 especie_veiculo            1     1     1     1
    ## 14 idade                      1     1     1     1
    ## 15 indicador_passageiro      NA     1    NA    NA
    ## 16 indicador_pedestre        NA     1    NA    NA
    ## 17 indicador_usa_capacete    NA     1    NA    NA
    ## 18 nascimento                 1     1     1     1
    ## 19 no_boletim                NA    NA    NA     1
    ## 20 no_envolvido              NA    NA     1     1
    ## 21 num_boletim                1     1     1    NA
    ## 22 numero_envolvido           1     1    NA    NA
    ## 23 passageiro                NA    NA     1     1
    ## 24 pedestre                  NA    NA     1     1
    ## 25 sexo                       1     1     1     1

``` r
conversions <- c(
  num_envolvido    = 'no_envolvido',
  num_envolvido    = 'numero_envolvido',
  passageiro       = 'indicador_passageiro',
  passageiro       = 'passageiro',
  pedestre         = 'indicador_pedestre',
  pedestre         = 'pedestre',
  cod_severidade   = 'codigo_severidade',
  usa_capacete     = 'indicador_usa_capacete',
  num_boletim      = 'no_boletim',
  desc_habilitacao = 'descricao_habilitacao'
)

dataset <- map(dataset, ~ rename(., any_of(conversions)))

kpitoolkit::compare_colnames(dataset, sort = TRUE)
```

    ## # A tibble: 20 × 5
    ##    colnames                 S2    S3    S4    S5
    ##    <chr>                 <dbl> <dbl> <dbl> <dbl>
    ##  1 arquivo_original          1     1     1     1
    ##  2 categoria_habilitacao     1     1     1     1
    ##  3 cinto_seguranca           1     1     1     1
    ##  4 cod_severidade            1     1     1     1
    ##  5 cod_severidade_antiga     1     1     1     1
    ##  6 condutor                  1     1     1     1
    ##  7 data_hora_boletim         1     1     1     1
    ##  8 declaracao_obito          1     1     1     1
    ##  9 desc_habilitacao          1     1     1     1
    ## 10 desc_severidade           1     1     1     1
    ## 11 embreagues                1     1     1     1
    ## 12 especie_veiculo           1     1     1     1
    ## 13 idade                     1     1     1     1
    ## 14 nascimento                1     1     1     1
    ## 15 num_boletim               1     1     1     1
    ## 16 num_envolvido             1     1     1     1
    ## 17 passageiro               NA     1     1     1
    ## 18 pedestre                 NA     1     1     1
    ## 19 sexo                      1     1     1     1
    ## 20 usa_capacete             NA     1    NA    NA

``` r
##  Fusão de tabelas
##  ----------------
dataset %<>% bind_rows()

output[['pessoas_envolvidas']][['data']] <- dataset
```

<br />

#### *Veículos envolvidos*

``` r
dataset <- df.sets$veiculos_envolvidos

##  Dicionários
##  -----------
dataset$S1 %<>% rowwise() %>%
  mutate(nome_do_arquivo = make_clean_names(nome_do_arquivo)) %>%
  ungroup()

output[['veiculos_envolvidos']][['dict']] <- dataset$S1
dataset['S1'] <- NULL


##  Conversão de nomes de campos
##  ----------------------------
kpitoolkit::compare_colnames(dataset, sort = TRUE)
```

    ## # A tibble: 19 × 3
    ##    colnames                  S2    S3
    ##    <chr>                  <dbl> <dbl>
    ##  1 arquivo_original           1     1
    ##  2 cod_categ                 NA     1
    ##  3 cod_especie               NA     1
    ##  4 cod_situacao              NA     1
    ##  5 codigo_categoria           1    NA
    ##  6 codigo_especie             1    NA
    ##  7 codigo_situacao            1    NA
    ##  8 data_hora_boletim          1     1
    ##  9 desc_situacao             NA     1
    ## 10 desc_tipo_socorro         NA     1
    ## 11 descricao_categoria        1     1
    ## 12 descricao_especie          1     1
    ## 13 descricao_situacao         1    NA
    ## 14 descricao_tipo_socorro     1    NA
    ## 15 no_boletim                NA     1
    ## 16 numero_boletim             1    NA
    ## 17 seq_veic                  NA     1
    ## 18 sequencial_veiculo         1    NA
    ## 19 tipo_socorro               1     1

``` r
conversions <- c(
  num_boletim        = 'no_boletim',
  num_boletim        = 'numero_boletim',
  cod_categoria      = 'cod_categ',
  cod_categoria      = 'codigo_categoria',
  cod_situacao       = 'cod_situacao',
  cod_situacao       = 'codigo_situacao',
  cod_especie        = 'cod_especie',
  cod_especie        = 'codigo_especie',
  desc_situacao      = 'desc_situacao',
  desc_situacao      = 'descricao_situacao',
  desc_tipo_socorro  = 'descricao_tipo_socorro',
  desc_categoria     = 'descricao_categoria',
  desc_especie       = 'descricao_especie',
  sequencial_veiculo = 'seq_veic'
)

dataset <- map(dataset, ~ rename(., any_of(conversions)))

kpitoolkit::compare_colnames(dataset, sort = TRUE)
```

    ## # A tibble: 12 × 3
    ##    colnames              S2    S3
    ##    <chr>              <dbl> <dbl>
    ##  1 arquivo_original       1     1
    ##  2 cod_categoria          1     1
    ##  3 cod_especie            1     1
    ##  4 cod_situacao           1     1
    ##  5 data_hora_boletim      1     1
    ##  6 desc_categoria         1     1
    ##  7 desc_especie           1     1
    ##  8 desc_situacao          1     1
    ##  9 desc_tipo_socorro      1     1
    ## 10 num_boletim            1     1
    ## 11 sequencial_veiculo     1     1
    ## 12 tipo_socorro           1     1

``` r
##  Fusão de tabelas
##  ----------------
dataset %<>% bind_rows()

output[['veiculos_envolvidos']][['data']] <- dataset
```

<br /><br />

### [Transformação de valores]{.smallcaps}

``` r
output <- map(output, ~map(., mutate, across(.fns = str_squish)))
```

<br />

#### [*Logradouro*]{.smallcaps}

1.  **Validação de tipos**

``` r
dct <- output$logradouros$dict
aux <- readr::type_convert(output$logradouros$data)
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   arquivo_original         = col_character(),
    ##   num_boletim              = col_character(),
    ##   data_hora_boletim        = col_character(),
    ##   num_municipio            = col_double(),
    ##   nome_municipio           = col_character(),
    ##   sequencia_logradouros    = col_double(),
    ##   num_logradouro           = col_double(),
    ##   tipo_logradouro          = col_character(),
    ##   nome_logradouro          = col_character(),
    ##   tipo_logradouro_anterior = col_character(),
    ##   nome_logradouro_anterior = col_character(),
    ##   num_bairro               = col_double(),
    ##   nome_bairro              = col_character(),
    ##   tipo_bairro              = col_character(),
    ##   descricao_tipo_bairro    = col_character(),
    ##   num_imovel               = col_character(),
    ##   num_imovel_proximo       = col_double()
    ## )

<br />

2.  **Resultados**

-   *Campos inconsistentes ou desnecessários:*

+-------------------------------------------------------+-------------------------------------------------------------------------+--------------------------------------+-----------+
| Campo                                                 | Notas                                                                   | Solução                              | Fixed     |
+=======================================================+=========================================================================+======================================+:=========:+
| `num_imovel`                                          | Campo livre para digitação (irregular)                                  | won't fix                            | won't fix |
+-------------------------------------------------------+-------------------------------------------------------------------------+--------------------------------------+-----------+
| `num_municipio`                                       | Campo contém apenas um valor                                            | Remoção do campo                     | x         |
|                                                       |                                                                         |                                      |           |
|                                                       | `(1 = BELO HORIZONTE)`                                                  |                                      |           |
+-------------------------------------------------------+-------------------------------------------------------------------------+--------------------------------------+-----------+
| `desc_tipo_bairro` , `nome_logradouro`, `nome_bairro` | Valores inconsistentes                                                  | Padronização para                    | x         |
|                                                       |                                                                         |                                      |           |
|                                                       | `["NÃO INFORMADO", "Nï¿½O INFORMADO"]`                                  | `NAO INFORMADO`                      |           |
+-------------------------------------------------------+-------------------------------------------------------------------------+--------------------------------------+-----------+
| `desc_tipo_log`                                       | Não existe descrição para logradouro (Existe em outra tabela?)          | Gerado a partir de dados pesquisados | x         |
+-------------------------------------------------------+-------------------------------------------------------------------------+--------------------------------------+-----------+
| `num_imovel_proximo`                                  | Não há dados úteis. Obs.: unique do campo retorna os seguintes valores: | Remoção do campo                     | x         |
|                                                       |                                                                         |                                      |           |
|                                                       | `["0", "0 ,", "0,"]`                                                    |                                      |           |
+-------------------------------------------------------+-------------------------------------------------------------------------+--------------------------------------+-----------+

<br />

3.  **Fix**

``` r
aux %<>% mutate(data_hora_boletim = parse_date_time(
  data_hora_boletim, '%d/%m/%Y %H:%M'
))

aux %<>% mutate(across(
  c(
    descricao_tipo_bairro,
    nome_logradouro,
    nome_bairro
  ),
  ~case_when(
    str_detect(., 'N.+O INFORMADO') ~ 'NAO INFORMADO',
    TRUE ~ .
  )
))

aux %<>% mutate(across(
  c(
    descricao_tipo_bairro,
    nome_logradouro,
    nome_bairro
  ),
  ~stringi::stri_trans_general(str = ., id = "Latin-ASCII")
))

##  Source: https://fazenda.pbh.gov.br/iss/cmc/forms.htm
##  ----------------------------------------------------
aux %<>% mutate(across(
  tipo_logradouro,
  ~ case_when(
    . == 'ACS' ~ 'ACESSO',
    . == 'ALA' ~ 'ALAMEDA',
    . == 'AVE' ~ 'AVENIDA',
    . == 'BEC' ~ 'BECO',
    . == 'CAM' ~ 'CAMINHO',
    . == 'ELV' ~ 'ELEVADO',
    . == 'ELP' ~ 'ESPACO LIVRE PARA PEDESTRE',
    . == 'EST' ~ 'ESTRADA',
    . == 'MAR' ~ 'MARGINAL',
    . == 'PCA' ~ 'PRACA',
    . == 'QTF' ~ 'QUARTEIRAO FECHADO',
    . == 'RDP' ~ 'RUA DE PEDESTRE',
    . == 'RMA' ~ 'REDE DE MANILHAS',
    . == 'ROD' ~ 'RODOVIA',
    . == 'RTN' ~ 'RETORNO',
    . == 'RUA' ~ 'RUA',
    . == 'TRE' ~ 'TREVO',
    . == 'TRI' ~ 'TRINCHEIRA',
    . == 'TRV' ~ 'TRAVESSA',
    . == 'TUN' ~ 'TUNEL',
    . == 'VDP' ~ 'VIA DE PEDESTRE',
    . == 'VDT' ~ 'VIADUTO',
    . == 'VIA' ~ 'VIA'
  ),
  .names = 'desc_tipo_logradouro'
))


aux %<>% select(-num_municipio, -num_imovel_proximo)



##  REMOVENDO DUPLICATAS
##  --------------------
aux %<>% distinct()



##  SALVANDO ALTERAÇÕES
##  -------------------
output$logradouros$data <- aux
```

<br />

#### [*Ocorrências*]{.smallcaps}

1.  **Validação de tipos**

``` r
dct <- output$ocorrencias$dict
aux <- readr::type_convert(output$ocorrencias$data)
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default             = col_character(),
    ##   cod_tempo            = col_double(),
    ##   cod_pavimento        = col_double(),
    ##   cod_regional         = col_double(),
    ##   velocidade_permitida = col_double(),
    ##   valor_ups            = col_double(),
    ##   valor_ups_antiga     = col_double()
    ## )
    ## ℹ Use `spec()` for the full column specifications.

<br />

2.  **Resultados**

-   *Campos inconsistentes ou desnecessários:*

| Campo                                                             | Notas                                                       | Solução         |   Fixed   |
|-------------------------------------------------------------------|-------------------------------------------------------------|-----------------|:---------:|
| `origem_boletim`                                                  | Valores trimados: `["CORPO DE BOMBEI", "POLICIA RODOVIA"]`  | completar nomes |     x     |
| `local_sinalizado`, `hora_informada`, `descricao_ups`             | Acentos: `["NÃO", "NÃO INFORMADO"]`                         | remover acentos |     x     |
| `coordenada_x`, `coordenada_y`                                    | Missing datas: `["0000000000.00"]`                          | alterar para NA |     x     |
| `data_alteracao_smsa`, `valor_ups_antiga`, `descricao_ups_antiga` | Inexistem valores: `"00/00/0000"` , `0` , `"NÃO INFORMADO"` | remover campos  |     x     |
| `coordenada_x`, `coordenada_y`                                    | Leading zero: `["0061011635.00", …,  "0060770301.00"]`      | won't fix       | won't fix |
| `desc_regional`                                                   | Apresenta `3473` missing data                               | won't fix       | won't fix |

<br />

3.  **Fix**

``` r
aux %<>% mutate(across(
  c(data_hora_boletim, data_inclusao),
  ~ parse_date_time(., '%d/%m/%Y %H:%M')
))


aux %<>% mutate(across(
  c(
    hora_informada,
    desc_tipo_acidente,
    origem_boletim,
    indicador_fatalidade,
    local_sinalizado,
    desc_ups
  ),
  ~stringi::stri_trans_general(str = ., id = "Latin-ASCII")
))

aux %<>% mutate(across(
  c(origem_boletim),
  ~ case_when(
    . == "CORPO DE BOMBEI" ~ 'CORPO DE BOMBEIROS',
    . == "POLICIA RODOVIA" ~ 'POLICIA RODOVIARIA',
    . == 'NI' ~ 'NAO INFORMADO',
    TRUE ~ .
  )
))

aux %<>% mutate(across(
  c(desc_regional),
  ~ case_when(
    cod_regional == 0 ~ 'NAO INFORMADO',
    TRUE ~ .
  )
))

aux %<>% select(
  -data_alteracao_smsa,
  -valor_ups_antiga,
  -descricao_ups_antiga
)

aux %<>% mutate(across(
  c(coordenada_x, coordenada_y),
  ~case_when(
    . == '0000000000.00' ~ NA_character_,
    TRUE ~ .
  )
))

aux$velocidade_permitida %<>% na_if(., 0)


##  REMOVENDO DUPLICATAS
##  --------------------
aux %<>% distinct()



##  SALVANDO ALTERAÇÕES
##  -------------------
output$ocorrencias$data <- aux
```

<br />

#### [*Pessoas envolvidas*]{.smallcaps}

1.  **Validação de tipos**

``` r
dct <- output$pessoas_envolvidas$dict
aux <- readr::type_convert(output$pessoas_envolvidas$data)
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   .default              = col_character(),
    ##   num_envolvido         = col_double(),
    ##   cod_severidade        = col_double(),
    ##   idade                 = col_double(),
    ##   declaracao_obito      = col_double(),
    ##   cod_severidade_antiga = col_double(),
    ##   x                     = col_logical()
    ## )
    ## ℹ Use `spec()` for the full column specifications.

<br />

2.  **Resultados**

-   *Campos inconsistentes ou desnecessários:*

+-----------------------------------------------------------------------+--------------------------------------------------------------------------+---------------------------------------+-------+
| Campo                                                                 | Notas                                                                    | Solução                               | Fixed |
+=======================================================================+==========================================================================+=======================================+:=====:+
| `condutor`, `pedestre`, `passageiro`, `cinto_seguranca`, `embreagues` | Acentos                                                                  | Remover acentos                       | x     |
+-----------------------------------------------------------------------+--------------------------------------------------------------------------+---------------------------------------+-------+
| `usa_capacete`, `x`, `declaracao_obito`, `cod_severidade_antiga`      | Sem valores                                                              | Remover campos                        | x     |
+-----------------------------------------------------------------------+--------------------------------------------------------------------------+---------------------------------------+-------+
| `condutor`, `pedestre`, `passageiro`                                  | Sem padrão                                                               | Padronizar valores                    | x     |
|                                                                       |                                                                          |                                       |       |
|                                                                       | `["0", "N", "NÃO", "NÃO INFORMADO", "S", "SIM"]`                         |                                       |       |
+-----------------------------------------------------------------------+--------------------------------------------------------------------------+---------------------------------------+-------+
| `sexo`                                                                | Missing data `"0"`                                                       | Alterar para NA                       | x     |
+-----------------------------------------------------------------------+--------------------------------------------------------------------------+---------------------------------------+-------+
| `idade`, `nascimento`                                                 | Missing data `"0"`, `"00/00/0000"`                                       | `["0", "00/00/0000"]` -\> ambos NA    | x     |
|                                                                       |                                                                          |                                       |       |
|                                                                       | Obs.: Alguns casos apresentam idade sem data de nascimento e vice-versa. | `[<valor>, "00/00/0000"]` -\> nasc NA |       |
|                                                                       |                                                                          |                                       |       |
|                                                                       |                                                                          | `["0", <valor>]` -\> calcular idade   |       |
+-----------------------------------------------------------------------+--------------------------------------------------------------------------+---------------------------------------+-------+
| `idade` , `nascimento`                                                | Valores incompatíveis ou inconsistentes                                  | Alterar para NA                       | x     |
+-----------------------------------------------------------------------+--------------------------------------------------------------------------+---------------------------------------+-------+

<br />

3.  **Fix**

``` r
##  REMOÇÃO DE CAMPOS SEM DADOS
##  ---------------------------
aux %<>% select(-c(
  usa_capacete,
  x
))


##  REMOÇÃO DE ACENTOS
##  ------------------
aux %<>% mutate(across(
  c(
    condutor,
    pedestre,
    passageiro,
    cinto_seguranca,
    embreagues
  ),
  ~stringi::stri_trans_general(str = ., id = "Latin-ASCII")
))


##  PADRONIZAÇÃO
##  ------------
aux %<>% mutate(across(
  c(condutor, pedestre, passageiro, cinto_seguranca, embreagues),
  ~ case_when(
    . == '0' | . == 'NAO INFORMADO' | is.na(.) ~ 'NI',
    . == 'N' ~ 'NAO',
    . == 'S' ~ 'SIM',
    TRUE ~ .
  )
))

aux %<>% mutate(across(
  c(categoria_habilitacao),
  ~ case_when(
    desc_habilitacao == 'NAO SE APLICA' ~ 'NA',
    . == 'N' | is.na(.) ~ 'NI',
    TRUE ~ .
  )
))

aux %<>% mutate(across(
  c(desc_habilitacao),
  ~ case_when(
    is.na(.) ~ 'NAO INFORMADO',
    TRUE ~ .
  )
))

aux %<>% mutate(across(
  c(especie_veiculo),
  ~ case_when(
    is.na(.) ~ 'NAO INFORMADO',
    TRUE ~ .
  )
))


##  SEXO
##  ----
aux %<>% mutate(across(
  c(sexo),
  ~ case_when(
    not(. %in% c('M', 'F')) ~ '0',
    TRUE ~ .
  )
))


##  AJUSTE DATAS
##  ------------
aux %<>% mutate(across(data_hora_boletim, ~parse_date_time(., '%d/%m/%Y %H:%M')))

aux$nascimento %<>% na_if(., '00/00/0000')

aux %<>% mutate(across(nascimento, ~dmy(.)))


##  AJUSTE DE IDADE E DATA DE NASCIMENTO
##  ------------------------------------
aux %<>% mutate(across(
  idade,
  ~case_when(
    . == '0' & is.na(nascimento) ~ NA_real_,
    TRUE ~ .
  )
))

aux %<>% mutate(
  idade_new = as.duration(
    interval(nascimento, lubridate::date(data_hora_boletim))
  ) %/% dyears()
) %>% relocate(idade_new, .after = idade)

aux %<>% mutate(
  idade_diff = idade_new - idade
) %>% relocate(idade_diff, .after = idade_new)

aux %<>% mutate(across(
  nascimento,
  ~ case_when(
    is.na(idade_new) | is.na(idade_diff) ~ .,
    idade_new < 0 | idade_new > 125 ~ NA_Date_,
    TRUE ~ .
  )
))

aux %<>% mutate(across(
  idade_new,
  ~ case_when(
    is.na(idade_new) | is.na(idade_diff) ~ .,
    idade_new < 0 | idade_new > 125 ~ NA_real_,
    TRUE ~ .
  )
))

aux %<>% select(-idade, -idade_diff)
aux %<>% rename(idade = idade_new)


##  REMOVENDO DUPLICATAS
##  --------------------
aux %<>% distinct()



##  SALVANDO ALTERAÇÕES
##  -------------------
output$pessoas_envolvidas$data <- aux
```

<br />

#### [*Veículos envolvidos*]{.smallcaps}

1.  **Validação de tipos**

``` r
dct <- output$veiculos_envolvidos$dict
aux <- readr::type_convert(output$veiculos_envolvidos$data)
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   arquivo_original = col_character(),
    ##   num_boletim = col_character(),
    ##   data_hora_boletim = col_character(),
    ##   sequencial_veiculo = col_double(),
    ##   cod_categoria = col_double(),
    ##   desc_categoria = col_character(),
    ##   cod_especie = col_double(),
    ##   desc_especie = col_character(),
    ##   cod_situacao = col_double(),
    ##   desc_situacao = col_character(),
    ##   tipo_socorro = col_double(),
    ##   desc_tipo_socorro = col_character()
    ## )

<br />

2.  **Resultados**

-   *Campos inconsistentes ou desnecessários:*

| Campo                                                                  | Notas                  | Solução         | Fixed |
|------------------------------------------------------------------------|------------------------|-----------------|:-----:|
| `desc_categoria`, `desc_especie`, `desc_situacao`, `desc_tipo_socorro` | Presença de acentuação | Remover acentos |   x   |

<br />

3.  **Fix**

``` r
##  REMOÇÃO DE ACENTOS
##  ------------------
aux %<>% mutate(across(
  c(
    desc_categoria,
    desc_especie,
    desc_situacao,
    desc_tipo_socorro
  ),
  ~stringi::stri_trans_general(str = ., id = "Latin-ASCII")
))


##  AJUSTANDO TIPO DATA
##  -------------------
aux %<>% mutate(across(
  c(data_hora_boletim),
  ~ parse_date_time(., '%d/%m/%Y %H:%M')
))


##  REMOVENDO DUPLICATAS
##  --------------------
aux %<>% distinct()



##  SALVANDO ALTERAÇÕES
##  -------------------
output$veiculos_envolvidos$data <- aux
```

### [Salvando transformação]{.smallcaps}

``` r
if (!dir.exists(here('data'))) {
  dir.create(here('data'))
}

for (i in seq_along(output)) {
  dt <- output[[i]]$data
  nm <- names(output)[i]
  readr::write_csv(dt, here('data', glue('{nm}.csv')))
}
```

### [Liberando memória]{.smallcaps}

``` r
rm(list = ls())

purrr::walk(
  names(sessionInfo()$otherPkgs), 
  ~ detach(
    paste0('package:', .),
    character.only = TRUE,
    unload = TRUE,
    force = TRUE
  )
)

rstudioapi::restartSession()
```
