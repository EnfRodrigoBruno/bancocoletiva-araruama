# ============================================================
# fontes_prioridade.R — Ordem de prioridade das fontes
# ============================================================
# Define, para cada sistema, a ordem em que as fontes devem ser
# tentadas. A ordem default é uma heurística (mirror R2 primeiro,
# FTP DATASUS como fallback confiável, microdatasus por último).
#
# Após rodar 02_benchmark_fontes.R, revise o arquivo sugerido em
# registros/fontes_prioridade_sugerida.R e copie para cá.
#
# Uso nos scripts de extração:
#   config <- ler_config_fontes()
#   ordem  <- config$SIM
# ============================================================

FONTES_PRIORIDADE <- list(
  SIM           = c("healthbR", "ftp_datasusr", "microdatasus"),
  SINASC_MICRO  = c("healthbR", "ftp_datasusr", "microdatasus"),
  SINAN         = c("healthbR", "ftp_datasusr"),
  SIH           = c("healthbR", "microdatasus"),
  CNES          = c("healthbR", "datasusr"),
  SIPNI         = c("healthbR"),
  POPULACAO     = c("brpop", "sidrar", "popsus")
)