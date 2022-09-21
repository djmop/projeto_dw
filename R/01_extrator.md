# Extrator CSVs - Projeto BI (PUC)

Daniel T. Nunes 2022-09-10 23:30

## Pacotes

``` r
library(RSelenium)
library(doParallel)
library(foreach)

library(tidyverse)
library(magrittr)
library(here)
```

## Carrega URLs

``` r
bhtrans <- read_csv(here('config/bhtrans_urls.csv'))
urls <- bhtrans[['url']]
names(urls) <- bhtrans[['name']]
```

## Cria Servidor

``` r
rD <- rsDriver(
  browser = 'firefox',
  verbose = FALSE,
  port    = 4813L
)

remDr <- rD[["client"]]
```

## Abre Cliente

``` r
remDr$open()
```

## Coleta URLs dos CSVs

``` r
csvs <- list()

for(i in seq_along(urls)) {
  remDr$navigate(urls[i])
  
  search.elems <- remDr$findElements(
    using = 'class',
    value = 'resource-url-analytics'
  )
  
  csv.urls <- c()
  
  for (csv.elem in search.elems) {
    csv.url  <- csv.elem$getElementAttribute('href') %>% unlist()
    csv.name <- basename(csv.url) %>%
      str_remove(r'{\.csv}') %>%
      str_replace_all('-', '_')
    
    csv.urls[csv.name] <- csv.url
  }
  
  csvs[[names(urls[i])]] <- csv.urls
}
```

## Finaliza Servidor

``` r
rD$server$process$finalize()
```

## Cria diretórios

``` r
create_dir <- function(dir.name) {
  dir.path <- here('raw_data', dir.name)
  if (!dir.exists(dir.path))
    dir.create(path = dir.path, recursive = TRUE)
}

success <- map(names(urls), ~create_dir(.)) %>% unlist()

if (!all(success)) {
  cli::cli_abort("Falha na criação de diretórios")
}
```

## Baixa CSVs

``` r
myCluster <- makeCluster(
  detectCores() - 1,
  type = 'PSOCK',
  outfile = here('log/download.log')
)

registerDoParallel(myCluster)

for (i in seq_along(csvs)) {
  folder.name <- names(csvs)[i]
  folder.csvs <- csvs[[i]]
  
  foreach(j = seq_along(folder.csvs)) %dopar% {
    library(glue)
    library(here)
    library(httr)
    
    down.url <- folder.csvs[j]
    csv.name <- glue::glue("{names(down.url)}.csv")
    csv.path <- here::here('raw_data', folder.name, csv.name)
    
    httr::GET(
      url = down.url,
      httr::write_disk(path = csv.path, overwrite = TRUE)
    )
  }
}

stopCluster(myCluster)
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
