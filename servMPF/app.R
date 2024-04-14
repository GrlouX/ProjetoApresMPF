library(shiny) # Build app
library(rvest) # Read data in html
library(stringi) # Manipulate strings
library(stringr) # Manipulate strings
library(glue) # Format strings
library(shinyscreenshot) # Capture app screen 

# Define UI for app that show the informations required ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Principais Serviços do Ministério Público Federal (MPF)"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Checkboxes for choosing services ----
      checkboxGroupInput("services",
                  label = h4("Escolha os serviços desejados:"),
                  choices = list("Denúncia (para pessoas físicas em geral)",
                                 "Protocolo (para pessoas jurídicas em geral)",
                                 "Petição (para as partes em procedimentos do MPF)",
                                 "Ouvidoria (para qualquer pessoa)",
                                 "LGPD (para qualquer pessoa)"),
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
    
    req(input$services)
    
  })
  
  # Prepare URLs to search ----
  urlPrep <- function(){
    
    url_base <- "https://www.mpf.mp.br/mpfservicos"
    
    opt0 <- actionInputs()
    
    opt <- c()
    for(i in 1:length(opt0)){
      opt <- cbind(opt, stri_trans_general(tolower(str_split(opt0[i]," ")[[1]][1]),"Latin-ASCII"))
    }
    
    url_all = str_c(url_base,"/",opt)
    
    return(url_all)
    
  }
  
  # Collect data from each URL
  urlSearch <- function(url){
    
    url0 <- str_sub(url,1,33)
    
    html <- read_html(url0)
    
    txt0 <- str_sub(url,35)
    
    if(txt0 == 'denuncia'){
      txt <- 'denuncias'
    }else if(txt0 == 'peticao'){
      txt <- 'peticaoassinat' 
    }else if(txt0 == 'ouvidoria'){
      txt <- 'ouvido' 
    }else{txt <- txt0}
    
    info_title <- html %>%
      html_nodes('body') %>%
      html_nodes(xpath = glue("//*[@class='card-servico3 link-{txt}-card']")) %>%
      html_elements('h3') %>%
      html_text2()
    
    info_content <- html %>%
      html_nodes('body') %>%
      html_nodes(xpath = glue("//*[@class='card-servico3 link-{txt}-card']")) %>%
      html_elements('ul')
    
    # info_explain <- html %>%
    #   html_nodes('body') %>%
    #   html_nodes(xpath = glue("//*[@class='card-servico3 link-{txt}-card']")) %>%
    #   html_elements('p') %>%
    #   html_text2()
    
    info_link <- url
    
    return(HTML(paste0('<br><p><strong>',info_title[1],'</strong></p><p>',
                info_content[1],'</p><p><u>Link para acesso e mais detalhes</u>: ',
                glue('<a href={info_link}>'),info_link,'</a></p><br>')))
    
  }
  
  # Map output page
  output$pageview <- renderUI({
      
      Map(urlSearch, urlPrep())
      
  })
  
  # General informations for users
  output$generalInfo <- renderUI({
    
    HTML(
    "<br><br><i>Fonte:</i><a href='https://www.mpf.mp.br/servicos/carta-de-servicos-ao-cidadao'>
    Carta de Serviços ao Cidadão</a>")
    
  })
  
  # Capture screen with informations
  observeEvent(input$shot, {
    
    screenshot(filename="servMPF", id="pageview")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)