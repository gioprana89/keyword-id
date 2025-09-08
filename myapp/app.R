library(shinyscreenshot)
library(scales)
library(readxl)
library(ggplot2)
library(stringr)
library(shiny)
library(ggthemes)
library(dplyr)
#library(shinyAce)
#source("chooser.R")

#library(lavaan)

#library(mnormt)
#library(curl)
#library(plspm)


########################################
########UI (User Interface)#############
########################################

connected_paper_by_keyword_ui <- function(id) {
  
  
  
  ns <- NS(id)
  
  fluidPage(
    
    textInput(ns("get_keyword"),
    "Input One Keyword", 
    "Online Gambling"),
    

    actionButton(ns("go"), "Find Article!", class = "btn-primary"),
    
    
    br(),
    br(),    br(),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    h1("Selected Articles",style="text-shadow: -1px 0 blue,
               0 1px blue, 1px 0 blue, 0 -1px blue; text-align:center;font-size:30px"         ),
    
    
    
    
    
    br(),
    
    DT::DTOutput(ns("open_data_article")),
    
    br(),
    br(),    br(),
    
    
    h1("Analysis of Keywords",style="text-shadow: -1px 0 blue,
               0 1px blue, 1px 0 blue, 0 -1px blue; text-align:center;font-size:30px"         ),
    
    
    br(),           
    
    textInput(ns("get_number_of_keyword_display_in_line_chart"),
              "Number of Keywords in Line Chart", 
              "5"),
    
    
    
    tabsetPanel(
      
      
      tabPanel("300 x 300",
               
               actionButton(ns('cetak_gambar_300_300_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_300_300_type1"), width = "300px", height = "300px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("500 x 300",
               
               
               actionButton(ns('cetak_gambar_500_300_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_500_300_type1"), width = "500px", height = "300px" )),
               
               br()
               
      ),
      
      
      
      
      tabPanel("700 x 300",
               
               
               actionButton(ns('cetak_gambar_700_300_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_700_300_type1"), width = "700px", height = "300px" )),
               
               br()
               
      ),
      
      
      
      
      tabPanel("900 x 300",
               
               
               actionButton(ns('cetak_gambar_900_300_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_900_300_type1"), width = "900px", height = "300px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("1100 x 300",
               
               
               actionButton(ns('cetak_gambar_1100_300_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1100_300_type1"), width = "1100px", height = "300px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("1200 x 300",
               
               
               actionButton(ns('cetak_gambar_1200_300_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1200_300_type1"), width = "1200px", height = "300px" )),
               
               br()
               
      ),
      
      
      tabPanel("1300 x 300",
               
               actionButton(ns('cetak_gambar_1300_300_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1300_300_type1"), width = "1300px", height = "300px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("1400 x 300",
               
               actionButton(ns('cetak_gambar_1400_300_type1'),'Print'),
               
               br(),
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1400_300_type1"), width = "1400px", height = "300px" )),
               
               br()
               
      ),
      
      
      
      
      
      
      
      
      
      
      
      tabPanel("300 x 500",
               
               actionButton(ns('cetak_gambar_300_500_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_300_500_type1"), width = "300px", height = "500px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("500 x 500",
               
               
               actionButton(ns('cetak_gambar_500_500_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_500_500_type1"), width = "500px", height = "500px" )),
               
               br()
               
      ),
      
      
      
      
      tabPanel("700 x 500",
               
               
               actionButton(ns('cetak_gambar_700_500_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_700_500_type1"), width = "700px", height = "500px" )),
               
               br()
               
      ),
      
      
      
      
      tabPanel("900 x 500",
               
               
               actionButton(ns('cetak_gambar_900_500_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_900_500_type1"), width = "900px", height = "500px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("1100 x 500",
               
               
               actionButton(ns('cetak_gambar_1100_500_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1100_500_type1"), width = "1100px", height = "500px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("1200 x 500",
               
               
               actionButton(ns('cetak_gambar_1200_500_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1200_500_type1"), width = "1200px", height = "500px" )),
               
               br()
               
      ),
      
      
      tabPanel("1300 x 500",
               
               actionButton(ns('cetak_gambar_1300_500_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1300_500_type1"), width = "1300px", height = "500px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("1400 x 500",
               
               actionButton(ns('cetak_gambar_1400_500_type1'),'Print'),
               
               br(),
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1400_500_type1"), width = "1400px", height = "500px" )),
               
               br()
               
      ),
      
      
      
      
      
      
      
      
      
      
      
      tabPanel("300 x 700",
               
               actionButton(ns('cetak_gambar_300_700_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_300_700_type1"), width = "300px", height = "700px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("500 x 700",
               
               
               actionButton(ns('cetak_gambar_500_700_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_500_700_type1"), width = "500px", height = "700px" )),
               
               br()
               
      ),
      
      
      
      
      tabPanel("700 x 700",
               
               
               actionButton(ns('cetak_gambar_700_700_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_700_700_type1"), width = "700px", height = "700px" )),
               
               br()
               
      ),
      
      
      
      
      tabPanel("900 x 700",
               
               
               actionButton(ns('cetak_gambar_900_700_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_900_700_type1"), width = "900px", height = "700px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("1100 x 700",
               
               
               actionButton(ns('cetak_gambar_1100_700_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1100_700_type1"), width = "1100px", height = "700px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("1200 x 700",
               
               
               actionButton(ns('cetak_gambar_1200_700_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1200_700_type1"), width = "1200px", height = "700px" )),
               
               br()
               
      ),
      
      
      tabPanel("1300 x 700",
               
               actionButton(ns('cetak_gambar_1300_700_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1300_700_type1"), width = "1300px", height = "700px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("1400 x 700",
               
               actionButton(ns('cetak_gambar_1400_700_type1'),'Print'),
               
               br(),
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1400_700_type1"), width = "1400px", height = "700px" )),
               
               br()
               
      ),
      
      
      
      
      tabPanel("300 x 900",
               
               actionButton(ns('cetak_gambar_300_900_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_300_900_type1"), width = "300px", height = "900px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("500 x 900",
               
               
               actionButton(ns('cetak_gambar_500_900_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_500_900_type1"), width = "500px", height = "900px" )),
               
               br()
               
      ),
      
      
      
      
      tabPanel("700 x 900",
               
               
               actionButton(ns('cetak_gambar_700_900_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_700_900_type1"), width = "700px", height = "900px" )),
               
               br()
               
      ),
      
      
      
      
      tabPanel("900 x 900",
               
               
               actionButton(ns('cetak_gambar_900_900_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_900_900_type1"), width = "900px", height = "900px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("1100 x 900",
               
               
               actionButton(ns('cetak_gambar_1100_900_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1100_900_type1"), width = "1100px", height = "900px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("1200 x 900",
               
               
               actionButton(ns('cetak_gambar_1200_900_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1200_900_type1"), width = "1200px", height = "900px" )),
               
               br()
               
      ),
      
      
      tabPanel("1300 x 900",
               
               actionButton(ns('cetak_gambar_1300_900_type1'),'Print'),
               
               br(),
               
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1300_900_type1"), width = "1300px", height = "900px" )),
               
               br()
               
      ),
      
      
      
      tabPanel("1400 x 900",
               
               actionButton(ns('cetak_gambar_1400_900_type1'),'Print'),
               
               br(),
               
               shinycssloaders::withSpinner(plotOutput(ns("grafik_garis_1400_900_type1"), width = "1400px", height = "900px" )),
               
               br()
               
      )
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    ), #akhir dari tabset panel
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ###############Distribusi Frekuensi Data Keyword######################
    
    
    br(),
    
    br(),
    
    DT::DTOutput(ns("distribusi_frekuensi_data_keywords")),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
               
    
    
    br()
    
  ) #Akhir Fluidpage
  
  
} #Akhir dari connected_paper_by_keyword_ui

#Akhir dari connected_paper_by_keyword_ui
#Akhir dari connected_paper_by_keyword_ui
#Akhir dari connected_paper_by_keyword_ui
#Akhir dari connected_paper_by_keyword_ui











































































########################################
################Server##################
########################################



connected_paper_by_keyword_server <- function(input, output, session) {
  
  
  fungsi_hitung_artikel_terpilih <- function()
  {
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    cek_keyword = input$get_keyword
    
    simpan_indeks <- vector(mode = "numeric")
    simpan_kata <- vector(mode = "character")
    k = 0
    data_keyword <- dat[,"Keywords"]
    
    for(i in 1 : length(data_keyword))
    {
      
      X <- data_keyword[i]
      
      X <- tolower(X) #mengubah menjadi huruf kecil
      cek_keyword <- tolower(cek_keyword) #mengubah menjadi huruf kecil
      
      X <- unlist(strsplit(as.character(X), "  ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), " ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";  ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), "; ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";", fixed = TRUE))
      
      ada_keyword <- cek_keyword %in% X
      
      if(ada_keyword == TRUE)
      {
        k = k + 1
        simpan_indeks[k] = i
        simpan_kata <- c(simpan_kata, X)
        
      }
      
    }
    
    
    data_terpilih <- dat[c(simpan_indeks),]
    
    
    return(data_terpilih)
    
  }
  
  
  
  
  observeEvent(input$go, {
    
  
  
  output$open_data_article <- DT::renderDT({
    
    data_artikel_terpilih <- fungsi_hitung_artikel_terpilih()
    print(data_artikel_terpilih)
    
    
    
  }) #Akhir renderDT 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###################Fungsi Grafik############
  
  
  
  
  
  
  
  fungsi_kirim_grafik <- function()
  {
    
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    cek_keyword = input$get_keyword
    
    simpan_indeks <- vector(mode = "numeric")
    simpan_kata <- vector(mode = "character")
    k = 0
    data_keyword <- dat[,"Keywords"]
    
    for(i in 1 : length(data_keyword))
    {
      
      X <- data_keyword[i]
      
      X <- tolower(X) #mengubah menjadi huruf kecil
      cek_keyword <- tolower(cek_keyword) #mengubah menjadi huruf kecil
      
      X <- unlist(strsplit(as.character(X), "  ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), " ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";  ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), "; ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";", fixed = TRUE))
      
      ada_keyword <- cek_keyword %in% X
      
      if(ada_keyword == TRUE)
      {
        k = k + 1
        simpan_indeks[k] = i
        simpan_kata <- c(simpan_kata, X)
        
      }
      
    }
    tabel <- table(simpan_kata)
    
    
    nama <- names(tabel)
    
    frekuensi <- unlist(tabel)
    names(frekuensi) <- NULL
    
    frekuensi <- unlist(frekuensi)
    frekuensi <- as.numeric(frekuensi)
    
    
    
    
    
    persentase <- frekuensi / sum(frekuensi) * 100
    
    persentase <- round(persentase, digits = 2)
    
    nama <- unlist(nama)
    
    
    data_tabel <- data.frame(nama, frekuensi, persentase)
    
    data_tabel <- arrange(data_tabel, desc(frekuensi) )
    
    
    
    urutan <- data_tabel[, "nama"]
    
    data_tabel[,1] <- factor(data_tabel[,1], levels = c(urutan) )
    
    
    
    jumlah <- input$get_number_of_keyword_display_in_line_chart
    
    jumlah <- unlist(jumlah)
    jumlah <- as.numeric(jumlah)
    
    data_tabel2 <- data_tabel[c(1:jumlah),]
    
    
    
    library(ggplot2)
    # Basic line plot with points
    p <- ggplot(data = data_tabel2, aes(x = nama, y = frekuensi, group = 1)) +
      geom_line( ) +  geom_point() + coord_flip() + xlab("Keywords") + ylab("Frequency") + theme_base()
    
    return(p)
    
    
    
    
    
  }
  
  
  
  
  
  
  
  
  #################
  
  
  
  
  
  
  ##########300 x 300
  
  output$grafik_garis_300_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_300_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_300_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  ##########500 x 300
  
  output$grafik_garis_500_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_500_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_500_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 300
  
  output$grafik_garis_700_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_700_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_700_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  ##########900 x 300
  
  output$grafik_garis_900_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_900_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_900_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1100 x 300
  
  output$grafik_garis_1100_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1100_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1100_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 300
  
  output$grafik_garis_1200_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1200_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1200_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1300 x 300
  
  output$grafik_garis_1300_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1300_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1300_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########1400 x 300
  
  output$grafik_garis_1400_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1400_300_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1400_300_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########300 x 500
  
  output$grafik_garis_300_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_300_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_300_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  ##########500 x 500
  
  output$grafik_garis_500_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_500_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_500_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 500
  
  output$grafik_garis_700_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_700_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_700_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  ##########900 x 500
  
  output$grafik_garis_900_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_900_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_900_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1100 x 500
  
  output$grafik_garis_1100_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1100_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1100_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 500
  
  output$grafik_garis_1200_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1200_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1200_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1300 x 500
  
  output$grafik_garis_1300_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1300_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1300_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########1400 x 500
  
  output$grafik_garis_1400_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1400_500_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1400_500_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########300 x 700
  
  output$grafik_garis_300_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_300_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_300_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  ##########500 x 700
  
  output$grafik_garis_500_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_500_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_500_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 700
  
  output$grafik_garis_700_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_700_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_700_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  ##########900 x 700
  
  output$grafik_garis_900_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_900_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_900_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1100 x 700
  
  output$grafik_garis_1100_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1100_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1100_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 700
  
  output$grafik_garis_1200_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1200_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1200_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1300 x 700
  
  output$grafik_garis_1300_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1300_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1300_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########1400 x 700
  
  output$grafik_garis_1400_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1400_700_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1400_700_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########300 x 900
  
  output$grafik_garis_300_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_300_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_300_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  ##########500 x 900
  
  output$grafik_garis_500_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_500_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_500_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 900
  
  output$grafik_garis_700_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_700_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_700_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  ##########900 x 900
  
  output$grafik_garis_900_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_900_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_900_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1100 x 900
  
  output$grafik_garis_1100_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1100_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1100_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 900
  
  output$grafik_garis_1200_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1200_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1200_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########1300 x 900
  
  output$grafik_garis_1300_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1300_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1300_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########1400 x 900
  
  output$grafik_garis_1400_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  observeEvent(input$cetak_gambar_1400_900_type1,{
    
    
    screenshot(
      #selector = "#gambar3",
      filename = "Chart",
      id = "grafik_garis_1400_900_type1",
      scale = 1,
      timer = 0
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$distribusi_frekuensi_data_keywords <- DT::renderDT({
    
    
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    cek_keyword = input$get_keyword
    
    simpan_indeks <- vector(mode = "numeric")
    simpan_kata <- vector(mode = "character")
    k = 0
    data_keyword <- dat[,"Keywords"]
    
    for(i in 1 : length(data_keyword))
    {
      
      X <- data_keyword[i]
      
      X <- tolower(X) #mengubah menjadi huruf kecil
      cek_keyword <- tolower(cek_keyword) #mengubah menjadi huruf kecil
      
      X <- unlist(strsplit(as.character(X), "  ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), " ;", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";  ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), "; ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ";", fixed = TRUE))
      
      ada_keyword <- cek_keyword %in% X
      
      if(ada_keyword == TRUE)
      {
        k = k + 1
        simpan_indeks[k] = i
        simpan_kata <- c(simpan_kata, X)
        
      }
      
    }
    tabel <- table(simpan_kata)
    
    
    nama <- names(tabel)
    
    frekuensi <- unlist(tabel)
    names(frekuensi) <- NULL
    
    frekuensi <- unlist(frekuensi)
    frekuensi <- as.numeric(frekuensi)
    
    
    
    
    
    persentase <- frekuensi / sum(frekuensi) * 100
    
    persentase <- round(persentase, digits = 2)
    
    nama <- unlist(nama)
    
    
    data_tabel <- data.frame(nama, frekuensi, persentase)
    
    data_tabel <- arrange(data_tabel, desc(frekuensi) )
    
    
    
    urutan <- data_tabel[, "nama"]
    
    data_tabel[,1] <- factor(data_tabel[,1], levels = c(urutan) )
    
    
    colnames(data_tabel) = c("Keywords", "Frequency", "Percentage (%)")
    
    
    print(data_tabel)
    
    
    
  }) #Akhir renderDT 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  }) #Akhir go
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
} #akhir dari connected_paper_by_keyword_server

#akhir dari connected_paper_by_keyword_server
#akhir dari connected_paper_by_keyword_server
#akhir dari connected_paper_by_keyword_server

















































































ui <- fluidPage(
  
  
  #includeHTML("intro_home.html"),
  
  
  uiOutput("connected_paper_by_keyword"),
  
  
  br()
  
) #Akhir dari UI











server <- function(input, output) {
  
  
  
  
  
  output$connected_paper_by_keyword <- renderUI({
    
    
    
    #source("module//connected_paper_by_keyword.R")
    callModule(module = connected_paper_by_keyword_server, id = "connected_paper_by_keyword")
    connected_paper_by_keyword_ui(id = "connected_paper_by_keyword")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
} #Akhir dari server










shinyApp(ui, server)














