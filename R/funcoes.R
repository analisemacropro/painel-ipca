# Funções auxiliares do painel IPCA

#' Baixa uma série do SGS (Banco Central do Brasil)
#'
#' @param codigo Código da série no SGS (ex.: 433 para o IPCA mensal).
#' @param inicio Data inicial no formato "dd/mm/aaaa" ou objeto Date.
#' @return data.frame com as colunas `data` (Date) e `valor` (numeric).
baixar_sgs <- function(codigo, inicio = "01/01/2000") {
  if (inherits(inicio, "Date")) inicio <- format(inicio, "%d/%m/%Y")

  url <- sprintf(
    "https://api.bcb.gov.br/dados/serie/bcdata.sgs.%s/dados?formato=json&dataInicial=%s",
    codigo, inicio
  )

  dados <- jsonlite::fromJSON(url)

  data.frame(
    data  = as.Date(dados$data, format = "%d/%m/%Y"),
    valor = as.numeric(dados$valor)
  )
}

#' Acumula uma taxa mensal em janelas de 12 meses
#'
#' @param taxa Vetor de variações mensais em % (ex.: 0.5 para 0,5%).
#' @return Vetor do mesmo tamanho com o acumulado em 12 meses, em %.
#'   As 11 primeiras posições são NA.
acumular_12m <- function(taxa) {
  fator <- 1 + taxa / 100
  n <- length(fator)
  acum <- rep(NA_real_, n)
  if (n >= 12) {
    for (i in 12:n) acum[i] <- (prod(fator[(i - 11):i]) - 1) * 100
  }
  acum
}
