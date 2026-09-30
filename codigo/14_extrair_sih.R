# ============================================================
# 14_extrair_sih.R — SIH/SUS: internações hospitalares
# Versão 3.0.3 — Cap 2GB + consolidação tolerante
# ============================================================
# Mudanças v3.0.3:
#   - cap_mb: 800 → 2000
#   - timeout: 120 → 60
#   - Consolidação valida cada parquet antes de ler
# ============================================================

sys.source(getExportedValue("here", "here")("codigo", "00_setup.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "01_utils.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "05_extracoes_comuns.R"),
           envir = globalenv())

registrar_log("=== Início 14_extrair_sih.R v3.0.3 ===")

FORCAR <- identical(Sys.getenv("FORCAR_DOWNLOADS"), "true")

ANOS_SIH  <- 2008:2024
MESES_SIH <- 1:12

montar_fontes_sih <- function(item) {
  ano <- item$ano; mes <- item$mes; uf <- UF_PROJETO
  list(
    list(nome = "microdatasus", expr = sprintf('{
      microdatasus::fetch_datasus(
        year_start = %d, year_end = %d,
        month_start = %d, month_end = %d,
        uf = "%s", information_system = "SIH-RD"
      )
    }', ano, ano, mes, mes, uf))
  )
}

filtro_sih <- function(item) {
  sprintf('{
    col_mun <- intersect(
      c("MUNIC_RES", "MUNIC_RESID", "MUNICIPIO", "COD_MUNIC",
        "MUNICIP", "CODMUNRES"),
      names(d)
    )[1]
    if (is.na(col_mun)) stop("coluna municipio ausente")
    x <- trimws(as.character(d[[col_mun]]))
    x[nchar(x) == 7] <- substr(x[nchar(x) == 7], 1, 6)
    d$ano_arquivo <- %d
    d$mes_arquivo <- %d
    d[x == "%s", , drop = FALSE]
  }', item$ano, item$mes, COD_IBGE_ARARUAMA_6)
}

itens_sih <- list()
for (ano in ANOS_SIH) {
  for (mes in MESES_SIH) {
    itens_sih[[length(itens_sih) + 1]] <- list(
      ano = ano, mes = mes,
      rotulo = sprintf("%d/%02d", ano, mes)
    )
  }
}

extrair_por_item(
  itens            = itens_sih,
  dir_saida        = dir_brutos_sih,
  nome_arquivo_fn  = function(it) sprintf("sih_araruama_%d_%02d.parquet",
                                           it$ano, it$mes),
  montar_fontes_fn = montar_fontes_sih,
  filtro_fn        = filtro_sih,
  timeout_s        = 60,
  cap_mb           = 2000,
  forcar           = FORCAR,
  titulo           = "14_extrair_sih.R — Internações hospitalares",
  min_mb_inicio    = 300
)

# --- Consolidação tolerante a arquivos corrompidos ---
arquivos <- list.files(dir_brutos_sih,
                       pattern = "^sih_araruama_\\d{4}_\\d{2}\\.parquet$",
                       full.names = TRUE)

if (length(arquivos) > 0) {
  tamanhos <- file.size(arquivos)
  ruins <- arquivos[tamanhos < 8]
  if (length(ruins) > 0) {
    cat(sprintf("  Removendo %d arquivos corrompidos (< 8 bytes)\n",
                length(ruins)))
    unlink(ruins)
  }
  arquivos <- arquivos[tamanhos >= 8]

  lista <- list()
  for (f in arquivos) {
    d <- tryCatch(arrow::read_parquet(f), error = function(e) NULL)
    if (is.null(d)) {
      cat(sprintf("  ⚠ pulando %s (leitura falhou)\n", basename(f)))
      next
    }
    lista[[basename(f)]] <- d
  }

  if (length(lista) > 0) {
    tipos <- purrr::map(lista, ~ purrr::map_chr(.x, ~ class(.x)[1]))
    tipos_consol <- purrr::transpose(tipos) |> purrr::map(~ unique(unlist(.x)))
    mistas <- names(tipos_consol)[purrr::map_int(tipos_consol, length) > 1]
    if (length(mistas) > 0) {
      lista <- purrr::map(lista, function(df) {
        for (col in mistas) if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
        df
      })
    }
    sih <- dplyr::bind_rows(lista)
    rm(lista); limpar_memoria()
    salvar_parquet(sih, file.path(dir_brutos_sih, "sih_araruama.parquet"))
    cat(sprintf("\n  Consolidado: %s internações | %d colunas\n",
                fmt_num(nrow(sih)), ncol(sih)))
  }
}

registrar_log("=== Fim 14_extrair_sih.R v3.0.3 ===")