library(shiny)
library(bslib)
library(ggplot2)
library(ellmer)
library(shinychat)

source("R/funcoes.R")

# Dados: IPCA mensal (SGS 433) direto da API do BCB
ipca <- baixar_sgs(433, inicio = "01/01/2015")
ipca$acum_12m <- acumular_12m(ipca$valor)

# Série em texto para ancorar as respostas do assistente
tabela_txt <- paste(
  sprintf("%s | %.2f | %s",
          format(ipca$data, "%m/%Y"), ipca$valor,
          ifelse(is.na(ipca$acum_12m), "NA", sprintf("%.2f", ipca$acum_12m))),
  collapse = "\n"
)

prompt_sistema <- paste0(
  "Você é um analista de conjuntura econômica especializado em inflação brasileira. ",
  "Responda sempre em português do Brasil, de forma clara e objetiva, como num ",
  "relatório de conjuntura: descreva o nível, a tendência recente e comparações ",
  "entre períodos.\n\n",
  "Regras:\n",
  "- Use APENAS os dados do IPCA abaixo (BCB/SGS, série 433). Não invente números ",
  "nem use dados externos (Selic, câmbio, expectativas, metas etc.).\n",
  "- Se a pergunta exigir informação que não está nos dados, diga que o painel ",
  "não tem essa informação.\n",
  "- Cite mês e valor ao mencionar um número. Variação em % ao mês; acumulado em % em 12 meses.\n\n",
  "Dados (mês | variação mensal % | acumulado 12 meses %):\n",
  tabela_txt
)

ui <- page_sidebar(
  title = "Painel IPCA",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    title = "Chat",
    width = 380,
    chat_mod_ui("chat")
  ),
  card(
    card_header("IPCA acumulado em 12 meses (%)"),
    plotOutput("grafico")
  ),
  card(
    card_header("IPCA: últimos seis meses"),
    tableOutput("tabela")
  )
)

server <- function(input, output, session) {
  cliente <- chat_google_gemini(system_prompt = prompt_sistema)
  chat_mod_server("chat", cliente)

  output$grafico <- renderPlot({
    dados <- ipca[!is.na(ipca$acum_12m), ]
    ggplot(dados, aes(data, acum_12m)) +
      geom_line(color = "#0d3b66", linewidth = 1) +
      labs(x = NULL, y = "% em 12 meses",
           caption = "Fonte: BCB/SGS, série 433") +
      theme_minimal(base_size = 13)
  })

  output$tabela <- renderTable({
    ult <- tail(ipca, 6)
    data.frame(
      `Mês` = format(ult$data, "%m/%Y"),
      `Variação mensal (%)` = ult$valor,
      `Acumulado 12 meses (%)` = ult$acum_12m,
      check.names = FALSE
    )
  }, digits = 2)
}

shinyApp(ui, server)
