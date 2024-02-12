library(shiny) # Build app
library(rvest) # Read data in html
library(stringi) # Manipulate strings
library(stringr) # Manipulate strings
library(glue) # Format strings
library(shinyscreenshot) # Capture app screen 

# Define UI for app that show the informations required ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Atuação do Ministério Público Federal (MPF)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Checkboxes for choosing activities ----
      checkboxGroupInput("activities",
                  label = h4("Escolha as atividades-fim de interesse:"),
                  choices = list("Inquéritos Civis", "Inquéritos Policiais", 
                              "Procedimentos Preparatórios", "Procedimentos Investigatórios", 
                              "Audiências Públicas", "Recomendações Expedidas", 
                              "Termos de Ajustamento de Conduta"),
                  selected = ""
      ),
      
      # Input: Buttom for save informations
      actionButton("shot", "Salvar informações"),
      
      # Output: General informations
      uiOutput("generalInfo")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      uiOutput("pageview")
      
    )
  )
)

# Define server logic required to provide informations ----
server <- function(input, output){
  
  # Create a list of the requested choices ----
  actionInputs <- reactive({
    
    req(input$activities)
    
  })
  
  # Prepare URLs to search ----
  urlPrep <- function(){

    url_base <- "http://www.transparencia.mpf.mp.br/conteudo/atividade-fim"

    opt0 <- actionInputs()
    
    opt <- c()
    for(i in 1:length(opt0)){
      opt <- cbind(opt, gsub(" ","-",stri_trans_general(tolower(opt0[i]),"Latin-ASCII")))
    }

    url_all = str_c(url_base,"/",opt)
    
    return(url_all)

  }

  # Collect data from each URL
  urlSearch <- function(url){
    
    html <- read_html(url)
    
    info_title <- html %>%
      html_nodes('body') %>%
      html_nodes(xpath = "//*[@class='']") %>%
      html_elements('h1') %>%
      html_text2()
    
    info_content <- html %>%
      html_nodes('body') %>%
      html_nodes(xpath = "//*[@id='content-core']") %>%
      html_elements('p') %>%
      html_text2()
    
    info_link <- html %>%
      html_nodes('body') %>%
      html_nodes(xpath = "//*[@id='content-core']") %>%
      html_nodes('a') %>%
      html_attr('href')
    
    return(HTML(paste0('<br><p><strong>',info_title,'</strong></p><p>',
                info_content[1],'</p><p><u>Link para consulta (com filtros por 
                data, responsável, local, entre outros)</u>: ',
                glue('<a href={tail(info_link,1)}>'),tail(info_link,1),'</a></p><br>')))
    
  }
  
  # Map output page
  output$pageview <- renderUI({
      
      Map(urlSearch, urlPrep())
      
  })
  
  # General informations for users
  output$generalInfo <- renderUI({
    
    HTML(
    "<br><br><i>Fonte:</i><a href='http://www.transparencia.mpf.mp.br/conteudo/atividade-fim'>
    Atividade-Fim do MPF</a>")
    
  })
  
  # Capture screen with informations
  observeEvent(input$shot, {
    
    screenshot(filename="infoMPF", id="pageview")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)