
library(shiny) # aplikace
library(bslib) # apliakce (layouty)

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(writexl)

library(lubridate) # prace s datumy
library(zoo) # ?
library(rvest) # html
library(DT) # render table


# ---------------------------------------------------- UI


ui <- page_fillable(
  
  titlePanel("Kalkulačka fixní ceny ZP"),
  input_dark_mode(id = "mode"), 
  
  layout_columns( # cards beside each other
    
    card( 
      
      card_header("Vstupní profil klienta"),
      
      textInput( 
        inputId = "text1", 
        label = tagList("Obchodník", span("*", style = "color:red")), 
        placeholder = ""
      ),
      
      textInput( 
        inputId = "text2", 
        label = tagList("Zákazník", span("*", style = "color:red")),
        placeholder = "",
        value = ""
      ) %>% 
        tagAppendAttributes(required = "required" # snaha o nastaveni povinneho pole
        ),
      
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
      DT::DTOutput("results"),
      
      # textAreaInput( 
      #   inputId = "text", 
      #   label = "Poznámka", 
      #   value = "(možnost napsat komentář do pdf)"), 
      # 
      
      downloadButton("downloadReport", 
                     label = "Stáhnout PDF report")
      
      
    )
  )
)


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
  
  # ---- ZADANI OBDOBI DODAVKY ----
  
  observeEvent(input$run, {
    req(input$date)
    
    start <- as.Date(format(input$date[1], "%Y-%m-01"))
    end   <- as.Date(format(input$date[2], "%Y-%m-01"))

    updateDateRangeInput( # uprava datumu na cele mesice
      session,
      "date",
      start = start,
      end = end
    )
    
    delOd <- start
    delDo <- end
    
  })
  
  # ---- SPUSTENI VYPOCTU ----
  # ---- GENEROVANI PDF ----
  
  observeEvent(input$run, {
    req(input$upload, input$date, input$text1, input$text2)
   
    profil <- read_excel(input$upload$datapath) %>%
      mutate(mesic = month(datum),
             rok = year(datum))  # načtení nahraného profilu
    
    start <- as.Date(format(input$date[1], "%Y-%m-01"))
    end <- as.Date(format(input$date[2], "%Y-%m-01"))
    obch <- input$text1
    zak <- input$text2
    
    delOd <- start
    delDo <- end
    
    source("analyzaFun.R") 
    
    result <- analyza_data(profil, delOd, delDo, 
                           obch = obch, zak = zak, 
                           path = "data/")
   
    fix_cena <- result$fix_cena
    
    req(fix_cena)
    if(!is.data.frame(fix_cena)) fix_cena <- as.data.frame(fix_cena)
    
    output$results <- DT::renderDT(
      fix_cena,
      rownames = FALSE,
      options = list(dom = 't', ordering = FALSE)
    )
    
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste0("VypocetFixCenyZP_report_", Sys.time(), ".pdf")
      },
      content = function(file) {
        rmarkdown::render(
          input = "report.Rmd",
          output_file = file,
          params = list(
            obchodnik = input$text1,
            zakaznik = input$text2,
            datum_od = input$date[1],
            datum_do = input$date[2],
            profil = data_upload(),
            fwd = result$fwd,
            otc = result$otc,
            fix_cena = result$fix_cena
          ),
          envir = new.env(parent = globalenv())
        )
      }
    )
  })
}


# ---------------------------------------------------- APP


shinyApp(ui, server)


