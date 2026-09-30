# ============================================================
# 13_extrair_sinan.R — SINAN: 7 agravos prioritários
# Versão 3.1.3 — TMPDIR no projeto + encoding tolerante
# ============================================================
# Mudanças v3.1.3:
#   - FIX: /tmp é tmpfs com só 3.9 GB. Redireciona TMPDIR
#     para <raiz>/registros/tmp (no disco raiz, 177 GB livres).
#   - FIX: iconv() em names() antes de tolower() evita erro
#     "input string is invalid UTF-8" em CHIKBR (e outros).
#   - FIX: limpa órfãos no TMPDIR a cada execução.
#   - Mantidos: leitor microdatasus, filtro à prova de NA.
# ============================================================

sys.source(getExportedValue("here", "here")("codigo", "00_setup.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "01_utils.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "05_extracoes_comuns.R"),
           envir = globalenv())

# --- v3.1.3: redireciona temporários para o disco do projeto ---
dir_tmp_projeto <- file.path(raiz, "registros", "tmp")
dir.create(dir_tmp_projeto, recursive = TRUE, showWarnings = FALSE)

# Limpa órfãos de execuções anteriores
lixo <- list.files(dir_tmp_projeto, full.names = TRUE,
                   all.files = TRUE, no.. = TRUE)
if (length(lixo) > 0) {
  cat(sprintf("  Limpando %d arquivos órfãos em %s\n",
              length(lixo), dir_tmp_projeto))
  unlink(lixo, recursive = TRUE, force = TRUE)
}

# Variáveis de ambiente (afeta subprocessos callr)
Sys.setenv(TMPDIR = dir_tmp_projeto)
Sys.setenv(TMP    = dir_tmp_projeto)
Sys.setenv(TEMP   = dir_tmp_projeto)
options(tmpdir = dir_tmp_projeto)

cat(sprintf("  TMPDIR redirecionado para: %s\n", dir_tmp_projeto))
cat(sprintf("  Espaço livre lá: %s\n",
            system(sprintf("df -h %s | tail -1 | awk '{print $4}'",
                           shQuote(dir_tmp_projeto)),
                   intern = TRUE, ignore.stderr = TRUE)))

registrar_log("=== Início 13_extrair_sinan.R v3.1.3 ===")

FORCAR <- identical(Sys.getenv("FORCAR_DOWNLOADS"), "true")

FTP_SINAN <- "ftp://ftp.datasus.gov.br/dissemin/publicos/SINAN/DADOS"

CATALOGO <- list(
  dengue = list(nome_curto = "dengue", ftp_prefix = "DENGBR",
                anos_finais = 2000:2025, anos_prelim = 2026),
  tuberculose = list(nome_curto = "tuberculose", ftp_prefix = "TUBEBR",
                     anos_finais = 2001:2019, anos_prelim = 2020:2025),
  hanseniase = list(nome_curto = "hanseniase", ftp_prefix = "HANSBR",
                    anos_finais = 2001:2023, anos_prelim = 2024:2026),
  sifilis_congenita = list(nome_curto = "sifilis_congenita", ftp_prefix = "SIFCBR",
                           anos_finais = integer(0), anos_prelim = 2007:2025),
  chikungunya = list(nome_curto = "chikungunya", ftp_prefix = "CHIKBR",
                     anos_finais = 2014:2025, anos_prelim = 2026),
  zika = list(nome_curto = "zika", ftp_prefix = "ZIKABR",
              anos_finais = 2015:2025, anos_prelim = 2026),
  violencia = list(nome_curto = "violencia", ftp_prefix = "VIOLBR",
                   anos_finais = 2009:2024, anos_prelim = 2025)
)

montar_fontes_sinan <- function(item) {
  agravo <- item$agravo
  ano    <- item$ano
  subdir <- item$subdir
  prefix <- CATALOGO[[agravo]]$ftp_prefix

  url <- sprintf("%s/%s/%s%02d.dbc",
                 FTP_SINAN, subdir, prefix, ano %% 100)

  list(
    list(nome = sprintf("ftp_%s", tolower(subdir)),
         expr = sprintf('{
      url <- "%s"
      tmp <- tempfile(fileext = ".dbc")
      on.exit(unlink(tmp), add = TRUE)
      h <- curl::new_handle()
      curl::handle_setopt(h, connecttimeout = 30, timeout = 120)
      curl::curl_download(url, tmp, quiet = TRUE, mode = "wb", handle = h)
      if (file.size(tmp) < 1000) stop("download vazio")
      microdatasus::read_dbc(tmp)
    }', url))
  )
}

# --- v3.1.3: filtro com iconv (tolerante a Latin-1) ---
filtro_sinan <- function(item) {
  sprintf('{
    names(d) <- iconv(names(d), from = "", to = "UTF-8", sub = "byte")
    names(d) <- tolower(names(d))
    col_mun <- intersect(
      c("id_mn_resi", "id_municip", "munic_res", "codmunres",
        "municipio", "cod_munic", "id_municipio"),
      names(d)
    )[1]
    if (is.na(col_mun)) stop("coluna municipio ausente")
    x <- as.character(d[[col_mun]])
    # v3.1.4: iconv nos VALORES (não só nos nomes) — CHIKBR 2021
    # tem bytes Latin-1 em alguns registros
    x <- iconv(x, from = "", to = "UTF-8", sub = "byte")
    x <- trimws(x)
    idx <- which(nchar(x) == 7)
    if (length(idx) > 0) x[idx] <- substr(x[idx], 1, 6)
    d$ano_arquivo <- %d
    d$agravo      <- "%s"
    d[x %%in%% "%s", , drop = FALSE]
  }', item$ano, item$agravo, COD_IBGE_ARARUAMA_6)
}

dir_sinan_base <- file.path(dir_dados_brutos, "sinan")
dir.create(dir_sinan_base, recursive = TRUE, showWarnings = FALSE)

for (ag in names(CATALOGO)) {
  info  <- CATALOGO[[ag]]
  dir_ag <- file.path(dir_sinan_base, ag)
  dir.create(dir_ag, recursive = TRUE, showWarnings = FALSE)

  itens_ag <- list()
  for (a in info$anos_finais) {
    itens_ag[[length(itens_ag) + 1]] <- list(
      agravo = ag, ano = a, subdir = "FINAIS",
      rotulo = sprintf("%s/%d/FINAIS", ag, a)
    )
  }
  for (a in info$anos_prelim) {
    itens_ag[[length(itens_ag) + 1]] <- list(
      agravo = ag, ano = a, subdir = "PRELIM",
      rotulo = sprintf("%s/%d/PRELIM", ag, a)
    )
  }

  cat(sprintf("\n═══ %s (%d anos) ═══\n", ag, length(itens_ag)))

  extrair_por_item(
    itens            = itens_ag,
    dir_saida        = dir_ag,
    nome_arquivo_fn  = function(it) sprintf("sinan_%s_araruama_%d.parquet",
                                             it$agravo, it$ano),
    montar_fontes_fn = montar_fontes_sinan,
    filtro_fn        = filtro_sinan,
    timeout_s        = 240,
    cap_mb           = 3000,
    forcar           = FORCAR,
    titulo           = sprintf("13_extrair_sinan.R — %s", ag),
    min_mb_inicio    = 1200
  )
}

# --- Consolidação por agravo ---
cat("\n═══ Consolidando por agravo ═══\n")
for (ag in names(CATALOGO)) {
  dir_ag  <- file.path(dir_sinan_base, ag)
  arquivos <- list.files(dir_ag,
                         pattern = sprintf("^sinan_%s_araruama_\\d{4}\\.parquet$", ag),
                         full.names = TRUE)
  if (length(arquivos) == 0) {
    cat(sprintf("  %-20s (nenhum arquivo)\n", ag))
    next
  }

  tamanhos <- file.size(arquivos)
  ruins <- arquivos[tamanhos < 8]
  if (length(ruins) > 0) unlink(ruins)
  arquivos <- arquivos[tamanhos >= 8]
  if (length(arquivos) == 0) next

  lista <- purrr::map(arquivos, arrow::read_parquet)
  tipos <- purrr::map(lista, ~ purrr::map_chr(.x, ~ class(.x)[1]))
  tipos_consol <- purrr::transpose(tipos) |> purrr::map(~ unique(unlist(.x)))
  mistas <- names(tipos_consol)[purrr::map_int(tipos_consol, length) > 1]
  if (length(mistas) > 0) {
    cat(sprintf("  %-20s %d colunas com tipos mistos -> coerção para character\n",
                ag, length(mistas)))
    lista <- purrr::map(lista, function(df) {
      for (col in mistas) if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
      df
    })
  }
  consol <- dplyr::bind_rows(lista)
  rm(lista); limpar_memoria()

  salvar_parquet(consol,
                 file.path(dir_ag, sprintf("sinan_%s_araruama.parquet", ag)))
  cat(sprintf("  %-20s %s registros | %d colunas\n",
              ag, fmt_num(nrow(consol)), ncol(consol)))
}

registrar_log("=== Fim 13_extrair_sinan.R v3.1.3 ===")