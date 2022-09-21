# Geração de DW - Projeto BI (PUC)

Daniel T. Nunes 2022-09-11 03:00

## Pacotes

``` r
library(magrittr)

library(dplyr)
library(readr)
library(tibble)
library(tidyr)
library(purrr)
library(lubridate)

library(here)
```

<br />

## Funções auxiliares

``` r
sample.values <- function(x, .max = 30) {
  vals <- map(x, ~unique(.))
  res <- map(vals, function(y) {
    if (length(y) > .max)
      return(sample(y, .max))
    else
      return(y)
  })
  return(map(res, sort))
}

search.elems <- function(x, pattern) {
  x[stringr::str_detect(names(x), pattern = pattern)]
}
```

<br />

## Carregamento

``` r
filenames <- fs::dir_ls(path = here('data'), type = 'file')

datasets <- purrr::map(filenames, read_csv)
names(datasets) <- stringr::str_remove(basename(filenames), r'(\.csv)')
rm(filenames)
```

<br />

## Filtra registros incompletos

``` r
boletins <- map(
  datasets,
  ~select(., any_of('num_boletim'))
)

boletins %<>% bind_rows()
boletins %<>% distinct()


##  REMOVE BOLETINS DUPLICADOS E INCONSISTENTES
##  -------------------------------------------
dupl <- boletins %>%
  group_by(num_boletim) %>%
  count()

dupl %<>% filter(n > 1)

boletins %<>% filter(not(num_boletim %in% dupl$num_boletim))
rm(dupl)


##  SELECIONA APENAS REGISTROS COMPLETOS
##  ------------------------------------
boletins %<>% mutate(
  logr = num_boletim %in% datasets$logradouros$num_boletim,
  ocor = num_boletim %in% datasets$ocorrencias$num_boletim,
  pess = num_boletim %in% datasets$pessoas_envolvidas$num_boletim,
  veic = num_boletim %in% datasets$veiculos_envolvidos$num_boletim
)

boletins %<>% rowwise() %>%
  filter(sum(logr, ocor, pess, veic) == 4) %>% 
  ungroup()

boletins %<>% select(-c(logr, ocor, pess, veic))
```

``` r
datasets %<>% map(., function(x){
  x %>% filter(num_boletim %in% boletins$num_boletim)
})

rm(boletins)
```

<br />

## Gerando dimensões

``` r
output <- list()
```

### [Data]{.smallcaps}

``` r
loc <- 'pt_BR.utf8'

dim.data <- tibble(
  data = c(
    date(datasets$ocorrencias$data_hora_boletim),
    date(datasets$ocorrencias$data_inclusao),
    datasets$pessoas_envolvidas$nascimento
  )
) %>% distinct()


##  GERA ATRIBUTOS
##  --------------
dim.data %<>% mutate(
  ano            = year(data),
  trimestre      = quarter(data),
  mes_num        = month(data),
  mes_nome_abrev = month(data, label = TRUE, locale = loc),
  mes_nome_compl = month(data, label = TRUE, abbr = FALSE, locale = loc),
  dia_mes        = mday(data),
  dia_semana     = wday(data),
  dia_nome       = wday(data, label = TRUE, locale = loc)
)

##  AJUSTA ATRIBUTOS
##  ----------------
dim.data %<>% mutate(across(
  c(mes_nome_compl, mes_nome_abrev, dia_nome),
  ~ stringi::stri_trans_general(str = ., id = "Latin-ASCII") %>% 
    stringr::str_to_upper()
))


##  GERA SURROGATE KEY
##  ------------------
dim.data %<>% mutate(
  data_sk = as.character(data) %>% 
    stringr::str_remove_all('-') %>% 
    as.numeric()
)

dim.data %<>% relocate(data_sk)


##  SALVA DIMENSÃO
##  --------------
output[['dim.data']] <- dim.data

rm(dim.data, loc)
```

<br />

### [Ocorrências]{.smallcaps}

``` r
ocorrencias <- datasets$ocorrencias


##  AJUSTA DATAS
##  ------------
ocorrencias %<>% mutate(
  data_inclusao_sk = as.character(date(data_inclusao)) %>% 
    stringr::str_remove_all('-') %>% 
    as.numeric(),
  hora_inclusao = paste(
    stringr::str_pad(hour(data_inclusao), 2, pad = '0'),
    stringr::str_pad(minute(data_inclusao), 2, pad = '0'),
    stringr::str_pad(second(data_inclusao), 2, pad = '0'),
    sep = ':'
  ),
  data_boletim_sk = as.character(date(data_hora_boletim)) %>% 
    stringr::str_remove_all('-') %>% 
    as.numeric(),
  hora_boletim = paste(
    stringr::str_pad(hour(data_hora_boletim), 2, pad = '0'),
    stringr::str_pad(minute(data_hora_boletim), 2, pad = '0'),
    stringr::str_pad(second(data_hora_boletim), 2, pad = '0'),
    sep = ':'
  )
)

ocorrencias %<>% relocate(
  data_boletim_sk,
  hora_boletim,
  data_inclusao_sk, 
  hora_inclusao,
  .after = num_boletim
)

ocorrencias %<>% rename(
  'tempo_sk'          = cod_tempo,
  'pavimento_sk'      = cod_pavimento,
  'regional_sk'       = cod_regional,
  'desc_pavimento'    = pavimento,
  'valor_ups_sk'      = valor_ups,
  'cod_tipo_acidente' = tipo_acidente
)


##  DIM: TIPO ACIDENTE
##  ------------------
output[['dim.ocor.tipo_acidente']] <- ocorrencias %>%
  distinct(cod_tipo_acidente, desc_tipo_acidente) %>% 
  arrange(cod_tipo_acidente) %>% 
  mutate(tipo_acidente_sk = 1:n()) %>% 
  relocate(tipo_acidente_sk)


##  DIM: TEMPO
##  ----------
output[['dim.ocor.tempo']] <- ocorrencias %>%
  distinct(tempo_sk, desc_tempo) %>% 
  arrange(tempo_sk) %>%
  filter(!is.na(tempo_sk))


##  DIM: PAVIMENTO
##  --------------
output[['dim.ocor.pavimento']] <- ocorrencias %>%
  distinct(pavimento_sk, desc_pavimento) %>% 
  arrange(pavimento_sk) %>%
  filter(!is.na(pavimento_sk))


##  DIM: REGIONAL
##  -------------
output[['dim.ocor.regional']] <- ocorrencias %>%
  distinct(regional_sk, desc_regional) %>% 
  arrange(regional_sk) %>%
  filter(!is.na(regional_sk))



##  DIM: ORIGEM BOLETIM
##  -------------------
output[['dim.ocor.origem_boletim']] <- ocorrencias %>%
  distinct(origem_boletim) %>% 
  arrange(origem_boletim) %>% 
  mutate(origem_boletim_sk = 1:n()) %>% 
  relocate(origem_boletim_sk)


##  DIM: VALOR UPS
##  --------------
output[['dim.ocor.valor_ups']] <- ocorrencias %>%
  distinct(valor_ups_sk, desc_ups) %>% 
  arrange(valor_ups_sk) %>%
  filter(!is.na(valor_ups_sk))


##  FATO: OCORRENCIA
##  ----------------
ocorrencias %<>% select(-c(
  data_hora_boletim,
  data_inclusao,
  desc_tipo_acidente,
  desc_tempo,
  desc_pavimento,
  desc_regional,
  desc_ups
))

ocorrencias %<>% left_join(
  output[["dim.ocor.tipo_acidente"]] %>%
    select(cod_tipo_acidente, tipo_acidente_sk),
  by = 'cod_tipo_acidente'
)

ocorrencias %<>% left_join(
  output[["dim.ocor.origem_boletim"]] %>%
    select(origem_boletim, origem_boletim_sk),
  by = 'origem_boletim'
)


# Gerando SK
ocorrencias %<>%
  arrange(data_boletim_sk, hora_boletim) %>% 
  mutate(boletim_sk = 1:n())


# Realocação
ocorrencias %<>% relocate(
  boletim_sk,
  num_boletim,
  data_inclusao_sk,
  hora_inclusao,
  data_boletim_sk,
  hora_boletim,
  hora_informada,
  tipo_acidente_sk,
  valor_ups_sk,
  indicador_fatalidade,
  tempo_sk,
  pavimento_sk,
  regional_sk,
  origem_boletim_sk,
  local_sinalizado,
  velocidade_permitida,
  coordenada_x,
  coordenada_y,
  arquivo_original,
)

ocorrencias %<>% select(-c(
  cod_tipo_acidente,
  origem_boletim
))


##  DIM: OCORRÊNCIAS
##  ----------------
output[['dim.ocorrencias']] <- ocorrencias

rm(ocorrencias)
```

<br />

### [Logradouros]{.smallcaps}

``` r
logradouros <- datasets$logradouros

logradouros %<>% select(-data_hora_boletim)
logradouros %<>% left_join(
  output[["dim.ocorrencias"]] %>% select(num_boletim, boletim_sk),
  by = 'num_boletim'
)

logradouros %<>% rename(
  'logradouro_sk'       = num_logradouro,
  'bairro_sk'           = num_bairro,
  'desc_tipo_bairro'    = descricao_tipo_bairro,
  'cod_tipo_logradouro' = tipo_logradouro,
  'cod_tipo_bairro'     = tipo_bairro
)


##  DIM: LOGRADOURO
##  ---------------
output[['dim.logr.logradouro']] <- logradouros %>%
  distinct(
    logradouro_sk,
    cod_tipo_logradouro,
    desc_tipo_logradouro,
    nome_logradouro
  ) %>% 
  arrange(logradouro_sk) %>%
  filter(!is.na(logradouro_sk))


##  DIM: BAIRRO
##  -----------
output[['dim.logr.bairro']] <- logradouros %>%
  distinct(bairro_sk, cod_tipo_bairro, desc_tipo_bairro, nome_bairro) %>% 
  arrange(bairro_sk) %>%
  filter(!is.na(bairro_sk))


##  FATO: LOGRADOUROS ENVOLVIDOS
##  ----------------------------
logradouros %<>% select(-c(
  num_boletim,
  nome_municipio,
  cod_tipo_logradouro,
  nome_logradouro,
  tipo_logradouro_anterior,
  nome_logradouro_anterior,
  nome_bairro,
  cod_tipo_bairro,
  desc_tipo_bairro,
  desc_tipo_logradouro,
  num_imovel
))

logradouros %<>%
  arrange(boletim_sk, sequencia_logradouros) %>% 
  mutate(log_envolvido_sk = 1:n())

logradouros %<>% relocate(
  log_envolvido_sk,
  boletim_sk,
  bairro_sk,
  logradouro_sk, 
  sequencia_logradouros
)

output[['fato.logradouros_envolvidos']] <- logradouros


##  SALVA FATO
##  ----------
rm(logradouros)
```

<br />

### [Veículos envolvidos]{.smallcaps}

``` r
veiculos <- datasets$veiculos_envolvidos
veiculos %<>% select(-data_hora_boletim)

veiculos %<>% rename(
  'categoria_veiculo_sk'   = cod_categoria,
  'especie_veiculo_sk'     = cod_especie,
  'desc_especie_veiculo'   = desc_especie,
  'situacao_veiculo_sk'    = cod_situacao,
  'tipo_socorro_sk'        = tipo_socorro,
  'desc_categoria_veiculo' = desc_categoria,
  'desc_situacao_veiculo'  = desc_situacao,
)


##  DIM: CATEGORIA VEÍCULO
##  ----------------------
output[['dim.veic.categoria_veiculo']] <- veiculos %>% 
  distinct(categoria_veiculo_sk, desc_categoria_veiculo) %>% 
  arrange(categoria_veiculo_sk)


##  DIM: ESPÉCIE VEÍCULO
##  --------------------
output[['dim.veic.especie_veiculo']] <- veiculos %>% 
  distinct(especie_veiculo_sk, desc_especie_veiculo) %>% 
  arrange(especie_veiculo_sk)


##  DIM: SITUAÇÃO NO ACIDENTE
##  -------------------------
output[['dim.veic.situacao_veiculo']] <- veiculos %>% 
  distinct(situacao_veiculo_sk, desc_situacao_veiculo) %>% 
  arrange(situacao_veiculo_sk)


##  DIM: TIPO DE SOCORRO
##  --------------------
output[['dim.veic.tipo_socorro']] <- veiculos %>% 
  distinct(tipo_socorro_sk, desc_tipo_socorro) %>% 
  arrange(tipo_socorro_sk)



##  FATO: VEÍCULOS ENVOLVIDOS
##  -------------------------
veiculos %<>% left_join(
  output[["dim.ocorrencias"]] %>% select(num_boletim, boletim_sk),
  by = 'num_boletim'
)

veiculos %<>% relocate(
  boletim_sk,
  sequencial_veiculo,
  categoria_veiculo_sk,
  especie_veiculo_sk,
  situacao_veiculo_sk,
  tipo_socorro_sk,
  arquivo_original
)

veiculos %<>% select(-c(
  num_boletim,
  desc_categoria_veiculo,
  desc_especie_veiculo,
  desc_situacao_veiculo,
  desc_tipo_socorro
))


# Gera SK
veiculos %<>% 
  arrange(boletim_sk, sequencial_veiculo) %>% 
  mutate(veiculo_envolvido_sk = 1:n()) %>% 
  relocate(veiculo_envolvido_sk)


##  SALVA FATO
##  ----------
output[['fato.veiculos_envolvidos']] <- veiculos

rm(veiculos)
```

<br />

### [Pessoas envolvidas]{.smallcaps}

``` r
pessoas <- datasets$pessoas_envolvidas
pessoas %<>% select(-data_hora_boletim)

pessoas %<>% rename(
  'severidade_sk'          = cod_severidade,
  'cod_sexo'               = sexo,
  'cod_categ_habilitacao'  = categoria_habilitacao,
  'desc_categ_habilitacao' = desc_habilitacao,
  'desc_especie_veiculo'   = especie_veiculo
)

pessoas$cod_categ_habilitacao %<>% replace_na(., 'NA')


##  DIM: SEVERIDADE
##  ---------------
output[['dim.pess.severidade']] <- pessoas %>% 
  distinct(severidade_sk, desc_severidade) %>% 
  arrange(severidade_sk)


##  DIM: SEXO
##  ---------
output[['dim.pess.sexo']] <- pessoas %>% 
  distinct(cod_sexo) %>% 
  mutate(
    sexo_sk = case_when(
      cod_sexo == '0' ~ 0,
      cod_sexo == 'F' ~ 1,
      cod_sexo == 'M' ~ 2
    ),
    desc_sexo = case_when(
      cod_sexo == '0' ~ 'NAO INFORMADO',
      cod_sexo == 'F' ~ 'FEMININO',
      cod_sexo == 'M' ~ 'MASCULINO'
    )
  ) %>% relocate(sexo_sk, cod_sexo, desc_sexo) %>% 
  arrange(sexo_sk)


##  DIM: CATEGORIA HABILITAÇÃO
##  --------------------------
output[['dim.pess.categ_habilitacao']] <- pessoas %>% 
  distinct(cod_categ_habilitacao, desc_categ_habilitacao) %>%
  arrange(cod_categ_habilitacao) %>% 
  mutate(categ_habilitacao_sk = 1:n()) %>% 
  relocate(categ_habilitacao_sk)


##  FATO: PESSOAS ENVOLVIDAS
##  ------------------------
pessoas %<>% left_join(
  output[["dim.ocorrencias"]] %>% select(num_boletim, boletim_sk),
  by = 'num_boletim'
)

pessoas %<>% left_join(
  output[["dim.pess.sexo"]] %>% select(cod_sexo, sexo_sk),
  by = 'cod_sexo'
)

pessoas %<>% left_join(
  output[["dim.pess.categ_habilitacao"]] %>%
    select(cod_categ_habilitacao, categ_habilitacao_sk),
  by = 'cod_categ_habilitacao'
)

pessoas %<>% left_join(
  output[["dim.veic.especie_veiculo"]] %>%
    select(especie_veiculo_sk, desc_especie_veiculo),
  by = c('desc_especie_veiculo')
)


pessoas %<>% relocate(
  boletim_sk,
  num_envolvido,
  especie_veiculo_sk,
  severidade_sk,
  categ_habilitacao_sk,
  sexo_sk,
  idade,
  nascimento,
  condutor,
  cinto_seguranca,
  embreagues,
  pedestre,
  passageiro,
  arquivo_original
)

pessoas %<>% select(-c(
  num_boletim,
  desc_severidade,
  cod_sexo,
  cod_categ_habilitacao,
  desc_categ_habilitacao,
  declaracao_obito,
  cod_severidade_antiga,
  desc_especie_veiculo
))


# Gera SK
pessoas %<>%
  arrange(boletim_sk, num_envolvido) %>% 
  mutate(pessoa_envolvida_sk = 1:n()) %>% 
  relocate(pessoa_envolvida_sk)


##  SALVA FATO
##  ----------
output[['fato.pessoas_envolvidas']] <- pessoas

rm(pessoas)
```

<br />

## Salva DW

``` r
if (!dir.exists(here('dw'))) {
  dir.create(here('dw'))
}

for (i in seq_along(output)) {
  dt <- output[[i]]
  nm <- stringr::str_replace(names(output)[i], r'{\.}', '-')
  readr::write_csv(dt, here('dw', glue::glue('{nm}.csv')))
}
```

## Libera memória

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
