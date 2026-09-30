# ============================================================
# 19_extrair_previas_tabnet.R — Prévias TabNet (2025–2026)
# Versão 3.0.3 — Nomes de linha com acentuação correta
# ============================================================
# Mudanças v3.0.3:
#   - Nomes de `linha` com acentuação correta
#   - cap_mb: 500 → 2000
#   - timeout: 120 → 120
#   - NÃO usa TabNet web (está inacessível no ambiente)
# ============================================================

sys.source(getExportedValue("here", "here")("codigo", "00_setup.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "01_utils.R"),
           envir = globalenv())
sys.source(getExportedValue("here", "here")("codigo", "05_extracoes_comuns.R"),
           envir = globalenv())

registrar_log("=== Início 19_extrair_previas_tabnet.R v3.0.3 ===")

FORCAR <- identical(Sys.getenv("FORCAR_DOWNLOADS"), "true")

MUN_FILTRO <- "330020 ARARUAMA"
CONTEUDO   <- "Óbitos p/Residênc"

DIMENSOES <- list(
  list(sufixo = "total",
       linhas = c("Município", "Municipio"),
       col_mun = TRUE),
  list(sufixo = "sexo",
       linhas = c("Sexo"),
       col_mun = FALSE),
  list(sufixo = "faixa_etaria",
       linhas = c("Faixa Etária", "Faixa Etaria"),
       col_mun = FALSE),
  list(sufixo = "capitulo_cid",
       linhas = c("Capítulo CID-10", "Capitulo CID-10"),
       col_mun = FALSE)
  # dimensão "mes" descartada — datasus::sim() não expõe como linha.
  # Informação mensal virá do DTOBITO do SIM microdados (script 20).
)

montar_fontes_previa <- function(item) {
  ano <- item$ano
  dim <- DIMENSOES[[item$dim_idx]]
  lapply(dim$linhas, function(linha) {
    list(nome = sprintf("tabnet_%s", gsub(" ", "_", linha)),
         expr = sprintf('{
      d <- datasus::sim(
        conjunto    = "obitos",
        abrangencia = "municipio",
        uf          = "%s",
        linha       = "%s",
        coluna      = "--Não-Ativa--",
        conteudo    = "%s",
        periodo     = "%d",
        filtros     = list(municipio = "%s")
      )
      as.data.frame(d)
    }', UF_PROJETO, linha, CONTEUDO, ano, MUN_FILTRO))
  })
}

filtro_previa <- function(item) {
  ano <- item$ano
  dim <- DIMENSOES[[item$dim_idx]]
  sprintf('{
    col_dim <- names(d)[1]
    col_val <- names(d)[2]
    d <- d[!grepl("^TOTAL$", d[[col_dim]]), , drop = FALSE]
    %s
    if (nrow(d) == 0) return(d)
    d <- dplyr::rename(d, categoria = 1, obitos = 2)
    d$ano_obito <- %d
    d$status    <- "preliminar"
    d$fonte     <- "TabNet/DATASUS"
    d$dimensao  <- "%s"
    d$consultado_em <- Sys.time()
    d
  }',
  if (dim$col_mun) sprintf('d <- d[grepl("^330020", d[[col_dim]]), , drop = FALSE]')
    else "",
  ano, dim$sufixo)
}

itens_previa <- list()
for (ano in ANOS_PREVIA) {
  for (i in seq_along(DIMENSOES)) {
    itens_previa[[length(itens_previa) + 1]] <- list(
      ano = ano, dim_idx = i,
      rotulo = sprintf("%d/%s", ano, DIMENSOES[[i]]$sufixo)
    )
  }
}

extrair_por_item(
  itens            = itens_previa,
  dir_saida        = dir_brutos_previas,
  nome_arquivo_fn  = function(it) sprintf(
    "sim_araruama_%d_%s.parquet", it$ano, DIMENSOES[[it$dim_idx]]$sufixo
  ),
  montar_fontes_fn = montar_fontes_previa,
  filtro_fn        = filtro_previa,
  timeout_s        = 120,
  cap_mb           = 2000,
  forcar           = FORCAR,
  titulo           = "19_extrair_previas_tabnet.R — Prévias",
  min_mb_inicio    = 300
)

# --- Consolidação (v3.1.0) ---
# 1 arquivo por dimensão, juntando 2025+2026
for (dim in DIMENSOES) {
  consolidar_anuais(
    dir_entrada   = dir_brutos_previas,
    padrao        = sprintf("^sim_araruama_\\d{4}_%s\\.parquet$",
                            dim$sufixo),
    arquivo_saida = file.path(dir_brutos_previas,
                              sprintf("sim_araruama_%s.parquet",
                                      dim$sufixo))
  )
}

registrar_log("=== Fim 19_extrair_previas_tabnet.R v3.0.3 ===")