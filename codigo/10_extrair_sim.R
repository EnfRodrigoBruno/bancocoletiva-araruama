# ============================================================
# 10_extrair_sim.R — Microdados do SIM (Araruama)
# Versão 3.1.0 — cap 2GB + timeout 60s
# ============================================================
# Mudanças v3.1.0:
#   - cap_mb: 1000 → 2000 (padrão do projeto, arrow.so).
#   - timeout_s: 180 → 60 (SIM via microdatasus é rápido).
#   - year_start/year_end passados nomeados (evita armadilha
#     do argumento posicional descrita na referência).
#   - microdatasus PRIMÁRIO mantido. FTP datasusr como fallback.
# ============================================================

sys.source(getExportedValue("here", "here")("codigo", "00_setup.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "01_utils.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "05_extracoes_comuns.R"),
           envir = globalenv())

registrar_log("=== Início 10_extrair_sim.R v3.1.0 ===")

FORCAR <- identical(Sys.getenv("FORCAR_DOWNLOADS"), "true")

montar_fontes_sim <- function(ano) {
  uf  <- UF_PROJETO
  url <- url_sim_dores(ano, uf)

  list(
    # --- 1ª tentativa: microdatasus (rápido, sempre funciona) ---
    list(nome = "microdatasus", expr = sprintf('{
      microdatasus::fetch_datasus(
        year_start = %d,
        year_end   = %d,
        uf         = "%s",
        information_system = "SIM-DO",
        stop_on_error = FALSE,
        quiet = TRUE
      )
    }', ano, ano, uf)),

    # --- 2ª tentativa: FTP direto (fallback) ---
    list(nome = "ftp_datasusr", expr = sprintf('{
      url <- "%s"
      tmp <- tempfile(fileext = ".dbc")
      on.exit(unlink(tmp), add = TRUE)
      h <- curl::new_handle()
      curl::handle_setopt(
        h,
        connecttimeout = 30,
        timeout        = 90
      )
      curl::curl_download(url, tmp, quiet = TRUE, mode = "wb", handle = h)
      if (file.size(tmp) < 1000) stop("download vazio")
      datasusr::read_datasus_dbc(tmp)
    }', url))
  )
}

filtro_sim <- function(ano) {
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

extrair_por_item(
  itens            = ANOS_SIM,
  dir_saida        = dir_brutos_sim,
  nome_arquivo_fn  = function(ano) sprintf("sim_araruama_%d.parquet", ano),
  montar_fontes_fn = montar_fontes_sim,
  filtro_fn        = filtro_sim,
  timeout_s        = 60,
  cap_mb           = 2000,
  forcar           = FORCAR,
  titulo           = "10_extrair_sim.R — Microdados do SIM",
  min_mb_inicio    = 400
)

# --- Consolidação (v3.1.0) ---
consolidar_anuais(
  dir_entrada   = dir_brutos_sim,
  padrao        = "^sim_araruama_\\d{4}\\.parquet$",
  arquivo_saida = file.path(dir_brutos_sim, "sim_araruama.parquet")
)

registrar_log("=== Fim 10_extrair_sim.R v3.1.0 ===")