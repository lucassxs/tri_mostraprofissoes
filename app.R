rm(list=ls())
library(shiny)
library(irtoys)
library(bslib)
library(feather) # salvar

#my_function <- source("tri_mostraprofissoes.R")

name1  <-c(1,0,0,1,1,0,1,0,1,1,1,1,1,1) ## campos reativos
name2   <-c(0,0,0,0,0,0,1,0,1,1,0,1,0,0) ## campos reativos
name3   <- c(1,0,0,1,1,0,1,0,1,1,1,1,1,1) ## campos reativos
name4    <- c(0,0,0,0,0,0,1,0,1,1,0,0,0,0) ## campos reativos
name5  <- c(0,0,1,0,1,1,1,0,1,1,1,1,0,1) ## campos reativos
name6  <- c(0,1,1,0,0,1,1,0,1,1,1,1,1,1) ## campos reativos
resposta <-rbind(name1,name2,name3,name4,name5,name6)
Alturas.Reais  <- c(180,173,185,168,170,179) ## campos reativos

dados <- data.frame(resposta, Alturas.Reais)
rownames(dados) <- c('A1', 'A2', 'A3', 'A4', 'A5', 'A6') #; str(dados)

est <- function(resp, dados) {
  altura.par <- matrix(c(
    0.30, 2.519, 0,
    1.18, 1.224, 0,
    1.47, 1.244, 0,
    0.89, 1.330, 0,
    2.21, 1.091, 0,
    1.80, 1.143, 0,
    4.09, -0.103, 0,
    1.11, 2.642, 0,
    1.14, -0.788, 0,
    3.19, 0.055, 0,
    1.16, 0.427, 0,
    2.37, 0.256, 0,
    1.74, 0.829, 0,
    2.81, 0.533, 0
  ),14,3, byrow=TRUE)
  
  theta <- eap(resp, altura.par, qu=normal.qu())
  estim <- (theta[,1]-mean(theta[,1]))/sd(theta[,1]) * sd(dados$Alturas.Reais) + mean(dados$Alturas.Reais)
  return(estim)
}


#ui shiny
ui <- fluidPage(
  titlePanel("Calcule sua altura"),
  
  theme = bs_theme(version=4, bootswatch='minty'),
  
  textInput("first_name", "Primeiro Nome", value = "Nome", width = NULL, placeholder = NULL),
  textInput("last_name",  "Último Nome", value = "Ultimo Nome", width = NULL, placeholder = NULL),
  
  
  numericInput("num1", "1. Na cama, eu frequentemente sinto frio nos pés.", value = "0", min = 0, max = 1),
  numericInput("num2", "2. Eu frequentemente desço subo as escadas de dois em dois degraus.", value = "0", min = 0, max = 1),
  numericInput("num3", "3. Eu acho que me daria bem em um time de basquete devido a minha altura.", value = "0", min = 0, max = 1),
  numericInput("num4", "4. Como policial, eu impressionaria muito.", value = "0", min = 0, max = 1),
  numericInput("num5", "5. Na maioria dos carros eu me sinto desconfortável.", value = "0", min = 0, max = 1),
  numericInput("num6", "6. Eu literalmente olho para meus colegas de cima para baixo.", value = "0", min = 0, max = 1),
  numericInput("num7", "7. Você é capaz de pegar um objeto no alto de um armário, sem usar escada?", value = "1", min = 0, max = 1),
  numericInput("num8", "8. Você abaixa quando vai passar por uma porta?", value = "0", min = 0, max = 1),
  numericInput("num9", "9. Você consegue guardar a bagagem no porta-malas do avião/ônibus?", value = "1", min = 0, max = 1),
  numericInput("num10", "10. Você regulava o banco do carro para trás?", value = "1", min = 0, max = 1),
  numericInput("num11", "11. Normalmente quando você está andando de carona lhe oferecem o banco da frente?", value = "0", min = 0, max = 1),
  numericInput("num12", "12. Quando você e várias pessoas vão tirar fotos, formando-se três fileiras, onde
ninguém ficará agachado, você costuma ficar atrás?", value = "0", min = 0, max = 1),
  numericInput("num13", "13. Você tem dificuldade para se acomodar no ônibus?", value = "0", min = 0, max = 1),
  numericInput("num14", "14. Em uma fila, por ordem de tamanho, você é sempre colocado atrás?", value = "0", min = 0, max = 1),
  
  numericInput("num15", "15. Qual a sua altura real?", value = "", min = 0, max = 250),
  
  actionButton("Submit", "Calcular"),
  
  #tableOutput("result"),
  #plotOutput("grafico"),
  tableOutput("estimativa")
)

#server shiny
server <- function(input, output){
  result <- eventReactive(input$Submit,
                          {
                            respostas <- c(input$num1, input$num2,input$num3,input$num4,input$num5,input$num6,input$num7,input$num8,input$num9,input$num10,input$num11,input$num12,input$num13,input$num14)
                            newrow    <- c(respostas, input$num15)
                            nomes     <<- paste(input$first_name, input$last_name)
                            dados     <<- rbind(dados, newrow)
                            rownames(dados)[nrow(dados)] <- nomes
                            dados$Altura.Estimada <- est(resp=as.matrix(dados[,1:14]), dados=dados)
                            print(dados)
                          })
  
  dipersion_plot <- eventReactive(input$Submit, plot(y=dados$Altura.Estimada, x=dados$Alturas.Reais, pch=16, cex=2, ylab='Altura estimada', xlab='Altura real'))
  
  altura_estimada <- eventReactive(input$Submit, est(resp=as.matrix(dados[,1:14]), dados=dados)[nrow(dados)])
  
  output$result <- renderTable({result()}, row.names=T)
  output$grafico <- renderPlot({dipersion_plot()})
  output$estimativa <- renderTable({altura_estimada()})
  
}




shinyApp(ui, server)
