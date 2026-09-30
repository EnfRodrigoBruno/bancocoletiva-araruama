# ============================================================
# 12_extrair_sinasc_micro.R — Microdados do SINASC
# Versão 3.0.2 — Cap 2GB + ordem microdatasus primeiro
# ============================================================
# Mudanças v3.0.2:
#   - microdatasus PRIMÁRIO (o ftp_datasusr falhou em quase tudo
#     por problema de AS com arrow.so)
#   - cap_mb: 800 → 2000
#   - timeout: 180 → 60
#   - Valida parquet mínimo (>= 8 bytes) na consolidação
# ============================================================

sys.source(getExportedValue("here", "here")("codigo", "00_setup.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "01_utils.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "05_extracoes_comuns.R"),
           envir = globalenv())

# (logo após os 3 sys.source, antes do registrar_log)
dir_tmp_projeto <- file.path(raiz, "registros", "tmp")
dir.create(dir_tmp_projeto, recursive = TRUE, showWarnings = FALSE)
lixo <- list.files(dir_tmp_projeto, full.names = TRUE,
                   all.files = TRUE, no.. = TRUE)
if (length(lixo) > 0) unlink(lixo, recursive = TRUE, force = TRUE)
Sys.setenv(TMPDIR = dir_tmp_projeto)
Sys.setenv(TMP    = dir_tmp_projeto)
Sys.setenv(TEMP   = dir_tmp_projeto)
options(tmpdir = dir_tmp_projeto)

registrar_log("=== Início 12_extrair_sinasc_micro.R v3.0.2 ===")

FORCAR <- identical(Sys.getenv("FORCAR_DOWNLOADS"), "false")

# 2023–2024 não estão em nenhum diretório do FTP (confirmado)
ANOS_SEM_PUBLICACAO <- c(2023L, 2024L)

montar_fontes_sinasc_micro <- function(ano) {
  uf     <- UF_PROJETO
  nov    <- url_sinasc_nov(ano, uf)
  prelim <- url_sinasc_prelim(ano, uf)

  list(
    # microdatasus PRIMÁRIO (rápido e confiável)
    list(nome = "microdatasus", expr = sprintf('{
      microdatasus::fetch_datasus(
        year_start = %d, year_end = %d,
        uf = "%s", information_system = "SINASC"
      )
    }', ano, ano, uf)),

    list(nome = "ftp_nov", expr = sprintf('{
      url <- "%s"
      tmp <- tempfile(fileext = ".dbc")
      on.exit(unlink(tmp), add = TRUE)
      h <- curl::new_handle()
      curl::handle_setopt(h, connecttimeout = 30, timeout = 60)
      curl::curl_download(url, tmp, quiet = TRUE, mode = "wb", handle = h)
      if (file.size(tmp) < 1000) stop("download vazio")
      datasusr::read_datasus_dbc(tmp)
    }', nov)),

    list(nome = "ftp_prelim", expr = sprintf('{
      url <- "%s"
      tmp <- tempfile(fileext = ".dbc")
      on.exit(unlink(tmp), add = TRUE)
      h <- curl::new_handle()
      curl::handle_setopt(h, connecttimeout = 30, timeout = 60)
      curl::curl_download(url, tmp, quiet = TRUE, mode = "wb", handle = h)
      if (file.size(tmp) < 1000) stop("download vazio")
      datasusr::read_datasus_dbc(tmp)
    }', prelim))
  )
}

filtro_sinasc_micro <- function(ano) {
  sprintf('{
    names(d) <- tolower(names(d))
    col_mun <- intersect(
      c("codmunres", "codmunresid", "codmun", "codigo_municipio"),
      names(d)
    )[1]
    if (is.na(col_mun)) stop("coluna municipio ausente")
    x <- trimws(as.character(d[[col_mun]]))
    x[nchar(x) == 7] <- substr(x[nchar(x) == 7], 1, 6)
    d$ano_arquivo <- %d
    d[x == "%s", , drop = FALSE]
  }', ano, COD_IBGE_ARARUAMA_6)
}

anos_alvo <- setdiff(ANOS_SIM, ANOS_SEM_PUBLICACAO)
if (length(ANOS_SEM_PUBLICACAO) > 0) {
  cat(sprintf("\n  ℹ Anos sem publicação DATASUS: %s\n",
              paste(ANOS_SEM_PUBLICACAO, collapse = ", ")))
  cat("    (marcados como 'aguardando publicação', não como erro)\n")
}

extrair_por_item(
  itens            = anos_alvo,
  dir_saida        = dir_brutos_sinasc_micro,
  nome_arquivo_fn  = function(ano) sprintf("sinasc_micro_araruama_%d.parquet", ano),
  montar_fontes_fn = montar_fontes_sinasc_micro,
  filtro_fn        = filtro_sinasc_micro,
  timeout_s        = 60,
  cap_mb           = 2000,
  forcar           = FORCAR,
  titulo           = "12_extrair_sinasc_micro.R — Microdados",
  min_mb_inicio    = 400
)

# --- Consolidar com validação de integridade ---
arquivos <- list.files(dir_brutos_sinasc_micro,
                       pattern = "^sinasc_micro_araruama_\\d{4}\\.parquet$",
                       full.names = TRUE)

if (length(arquivos) > 0) {
  # Remove arquivos < 8 bytes (corrompidos)
  tamanhos <- file.size(arquivos)
  ruins <- arquivos[tamanhos < 8]
  if (length(ruins) > 0) {
    cat(sprintf("  Removendo %d arquivos corrompidos (< 8 bytes)\n",
                length(ruins)))
    unlink(ruins)
    arquivos <- arquivos[tamanhos >= 8]
  }

  lista <- purrr::map(arquivos, arrow::read_parquet)
  tipos <- purrr::map(lista, ~ purrr::map_chr(.x, ~ class(.x)[1]))
  tipos_consol <- purrr::transpose(tipos) |> purrr::map(~ unique(unlist(.x)))
  mistas <- names(tipos_consol)[purrr::map_int(tipos_consol, length) > 1]
  if (length(mistas) > 0) {
    cat(sprintf("  Colunas com tipos mistos: %d\n", length(mistas)))
    lista <- purrr::map(lista, function(df) {
      for (col in mistas) if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
      df
    })
  }
  sinasc_micro <- dplyr::bind_rows(lista)
  rm(lista); limpar_memoria()
  salvar_parquet(sinasc_micro,
                 file.path(dir_brutos_sinasc_micro,
                           "sinasc_micro_araruama.parquet"))
  cat(sprintf("\n  Consolidado: %s nascidos vivos | %d colunas\n",
              fmt_num(nrow(sinasc_micro)), ncol(sinasc_micro)))
}

registrar_log("=== Fim 12_extrair_sinasc_micro.R v3.0.2 ===")