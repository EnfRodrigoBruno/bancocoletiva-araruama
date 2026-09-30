# ============================================================
# 18_extrair_populacao_idade.R — População por faixa etária
# Versão 3.0.1
# ============================================================
# Fonte única: POPSVS (FTP DATASUS)
# Arquivo Brasil-wide (~25MB compactado), filtrado para Araruama.
#
# Entrada : (nada)
# Saída   : dados_brutos/populacao/populacao_araruama_idade_YYYY.parquet
#           dados_brutos/populacao/populacao_araruama_idade.parquet
# ============================================================

sys.source(getExportedValue("here", "here")("codigo", "00_setup.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "01_utils.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "05_extracoes_comuns.R"),
           envir = globalenv())

registrar_log("=== Início 18_extrair_populacao_idade.R v3.0.1 ===")

FORCAR <- identical(Sys.getenv("FORCAR_DOWNLOADS"), "true")

montar_fontes_pop_idade <- function(ano) {
  url <- url_popsvs(ano)
  list(
    list(nome = "popsus_ftp", expr = sprintf('{
      url <- "%s"
      tmp     <- tempfile(fileext = ".zip")
      tmp_dir <- tempfile("popsvs_")
      on.exit(unlink(c(tmp, tmp_dir), recursive = TRUE), add = TRUE)
      dir.create(tmp_dir, showWarnings = FALSE)

      h <- curl::new_handle()
      curl::handle_setopt(h, connecttimeout = 30, timeout = 120)
      curl::curl_download(url, tmp, quiet = TRUE, mode = "wb", handle = h)

      arqs <- unzip(tmp, exdir = tmp_dir)
      dbf  <- arqs[grepl("\\\\.dbf$", arqs, ignore.case = TRUE)][1]
      foreign::read.dbf(dbf, as.is = TRUE)
    }', url))
  )
}

filtro_pop_idade <- function(ano) {
  sprintf('{
    names(d) <- tolower(names(d))
    col_mun <- intersect(c("cod_mun", "codmun", "codigo"), names(d))[1]
    if (is.na(col_mun)) stop("coluna municipio ausente")
    x <- trimws(as.character(d[[col_mun]]))
    d <- d[x == "%s", , drop = FALSE]
    if (nrow(d) == 0) return(d)
    d$ano_arquivo <- %d
    d$fonte       <- "POPSVS/DATASUS"
    d$consultado_em <- Sys.time()
    d
  }', COD_IBGE_ARARUAMA_7, ano)
}

extrair_por_item(
  itens            = ANOS_POP,
  dir_saida        = dir_brutos_pop,
  nome_arquivo_fn  = function(ano) sprintf("populacao_araruama_idade_%d.parquet", ano),
  montar_fontes_fn = montar_fontes_pop_idade,
  filtro_fn        = filtro_pop_idade,
  timeout_s        = 120,
  cap_mb           = 800,
  forcar           = FORCAR,
  titulo           = "18_extrair_populacao_idade.R — POPSVS",
  min_mb_inicio    = 400
)

# --- Consolidar ---
arquivos <- list.files(dir_brutos_pop,
                       pattern = "^populacao_araruama_idade_\\d{4}\\.parquet$",
                       full.names = TRUE)
if (length(arquivos) > 0) {
  lista <- purrr::map(arquivos, arrow::read_parquet)
  tipos <- purrr::map(lista, ~ purrr::map_chr(.x, ~ class(.x)[1]))
  tipos_consol <- purrr::transpose(tipos) |> purrr::map(~ unique(unlist(.x)))
  mistas <- names(tipos_consol)[purrr::map_int(tipos_consol, length) > 1]
  if (length(mistas) > 0) {
    lista <- purrr::map(lista, function(df) {
      for (col in mistas) if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
      df
    })
  }
  pop_idade <- dplyr::bind_rows(lista)
  rm(lista); limpar_memoria()
  salvar_parquet(pop_idade,
                 file.path(dir_brutos_pop, "populacao_araruama_idade.parquet"))
  cat(sprintf("\n  Consolidado: %s registros | %d anos\n",
              fmt_num(nrow(pop_idade)),
              length(unique(pop_idade$ano_arquivo))))
}

registrar_log("=== Fim 18_extrair_populacao_idade.R v3.0.1 ===")