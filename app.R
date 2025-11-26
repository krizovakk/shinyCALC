

library(bslib)
library(readxl)
library(dplyr)
library(ggplot2)

library(tidyverse)
library(writexl)
library(lubridate) # prace s datumy
library(zoo) # ?
library(shiny) # aplikace
library(rvest) # html
library(DT) # render table


# ---------------------------------------------------- UI

ui <- page_fillable(
  
  titlePanel("Kalkulačka fixní ceny ZP"),
  input_dark_mode(id = "mode"), 
  
  layout_columns( 
    
    card( 
      
      card_header("Vstupní profil klienta"),
      
      fileInput(
        inputId = "upload", 
        label = "", 
        buttonLabel = "Nahraj profil",
        placeholder = "",
        accept = c(".xls", ".xlsx")),
      plotOutput("plot") 
      
    ),
    
    card( 
      
      card_header("Výpočet ceny"),
      
      dateRangeInput(
        inputId = "date",
        label = "Období dodávky",
        separator = " - ",
        start = Sys.Date(),
        # end = Sys.Date() + 1,
        # min = Sys.Date() - 14,
        # max = Sys.Date() + 14
      ),
      
      actionButton(
        inputId = "run", 
        label = "Výpočet ceny"),
      # h4("Vysledna fixni cena:"),
      DT::DTOutput("results"),
      
      textAreaInput( 
        inputId = "text", 
        label = "Poznámka", 
        value = "(možnost napsat komentář do pdf)"), 
      
    )
  )
)

# ui <- fluidPage(
#   
#   titlePanel("Kalkulačka fixní ceny ZP"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       fileInput("upload", 
#                 label = "", 
#                 buttonLabel = "Nahraj profil",
#                 placeholder = "",
#                 accept = c(".xls", ".xlsx")),
#       plotOutput("plot") 
#       
#     ),
#     
#     mainPanel(
#       
#       dateRangeInput(
#         inputId = "date",
#         label = "Období dodávky",
#         separator = " - ",
#         start = Sys.Date(),
#         # end = Sys.Date() + 1,
#         # min = Sys.Date() - 14,
#         # max = Sys.Date() + 14
#       ),
#       
#       actionButton("run", "Vypocet ceny"),
#       # h4("Vysledna fixni cena:"),
#       tableOutput("results")
#     )
#   )
# )

# ---------------------------------------------------- SERVER

server <- function(input, output, session) {

  # ---- REAKTIVNÍ NAČTENÍ EXCELU ----
  data_upload <- reactive({
    req(input$upload)
    profil <- read_excel(input$upload$datapath)
    profil
  })
  
  # ---- PLOT ----
  output$plot <- renderPlot({
    profil <- data_upload()
    req(profil)
    
    ggplot(profil, aes(datum, profilMWh)) +
      geom_line(linewidth = 2, color = "gold") +
      labs(x = "měsíc dodávky",
           y = "profil spotřeby [MWh]",
           title = "Profil spotřeby klienta") +
      scale_x_datetime(date_breaks = "1 month", date_labels = "%Y-%m") +
      scale_y_continuous(breaks = seq(0, 700, by = 50))+
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
  })
  
  observeEvent(input$run, {
    req(input$date)
    
    delOd <- input$date[1]
    delDo <- input$date[2]
    
    print(delOd)
    print(delDo)
  })
  
  observeEvent(input$run, {
    req(input$upload, input$date)
    
    # načtení nahraného profilu
    profil <- read_excel(input$upload$datapath) %>%
      mutate(mesic = month(datum),
             rok = year(datum))
    
    delOd <- input$date[1]
    delDo <- input$date[2]
    
    source("analyzaFun.R") # načte funkci analyza_data()
    
    result <- analyza_data(profil, delOd, delDo, path = "data/")
    
    output$results <- DT::renderDT(result)
  })
  
  # observeEvent(input$run, {
  #   
  #   req(input$upload, input$date)
  #   
  #   # uložíme profil
  #   profil <- input$upload
  #   
  #   # vytvoříme lokální prostředí pro analyza.R
  #   local_env <- new.env()
  #   local_env$profil <- profil
  #   local_env$delOd <- input$date[1]
  #   local_env$delDo <- input$date[2]
  #   
  #   # spustíme analyza.R v tom prostředí
  #   source("analyzaFun.R", local = local_env)
  #   
  #   # výstup fix_cena si vezmeme zpět
  #   fix_cena <- local_env$fix_cena
  #   
  #   output$results <- renderTable({
  #     fix_cena
  #   })
  # })
  
  # observeEvent(input$upload, {
  #   df <- read_excel(input$upload$datapath, col_names = TRUE)
  #   print(names(df))
  #   print(head(df))
  # })
  
  # observeEvent(input$run, {
  #   print("WORKING DIRECTORY:")
  #   print(getwd())
  #   print("FILES IN CURRENT DIR:")
  #   print(list.files())
  #   print("FILES IN DATA DIR:")
  #   print(list.files("data"))
  # })
  

  # observeEvent(input$run, {
  #   req(input$upload)
  #   
  #   # cesta k dočasnému souboru
  #   file_path <- input$file$datapath
  #   
  #   # načteme externí skript
  #   source("analyza.R")
  #   
  #   # spustíme funkci z analyza.R
  #   result <- analyza_data(file_path)
  #   
  #   # výsledek pošleme do UI
  #   output$results <- renderTable({
  #     result
  #   })
  #   output$value <- renderText({input$text})
  # })
}

# ---------------------------------------------------- APP

shinyApp(ui, server)


