library(shinyscreenshot)
library(scales)
library(readxl)
library(ggplot2)
library(stringr)
library(shiny)
library(ggthemes)
library(dplyr)



library(tidytext)

library(wordcloud)




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
    
    
    includeHTML("informasi.html"),
    

    
    
    #uiOutput(ns("tampilkan_untuk_input_kata_kunci")),
    
 
    
    
    #uiOutput(ns("tampilkan_kotak_kata_kunci")),
    
    

   # actionButton(ns("go"), "Find Article!", class = "btn-primary"),
    
    
    
    
    br(),
    
    
    
    
    
    br(),
    
    
    br(),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
   # shinycssloaders::withSpinner(DT::DTOutput(ns("distribusi_frekuensi_data_keywords"))),
    
    
    
    
    
    
    
    
    
    
    
    
    
   tabsetPanel(
     
     
     
     
     
     
     
     
     
     
     
     tabPanel(title = tags$h5( tags$img(src = "katakunci-unscreen.gif", width = "30px"), 'Available Keywords'),
              
              
              h1("Available Keywords",style="text-shadow: -1px 0 blue,
               0 1px blue, 1px 0 blue, 0 -1px blue; text-align:center;font-size:30px"         ),
              
              
              br(),
              
              shinycssloaders::withSpinner(DT::DTOutput(ns("katakunci_yang_tersedia"))),
              
              
              
              br(),
              
              
              
              
              
              
              
              #######################Analisis Wordcloud
              
              
              
              
              fluidRow(
                column(4,
                       
                       radioButtons(ns("warna_wordcloud_all"),
                                    
                                    "Theme of Words:", 
                                    c("Blues" = "Blues", "BuGn"="BuGn",
                                      "BuPu"="BuPu", "GnBu"="GnBu", "Greens"="Greens", "YlOrRd"="YlOrRd", "YlOrBr" = "YlOrBr", "YlGnBu" = "YlGnBu",
                                      "Spectral" = "Spectral", "RdYlGn" = "RdYlGn", "YlGn" = "YlGn",
                                      "RdBu" = "RdBu", "RdGy" = "RdGy", "RdYlBu" = "RdYlBu",
                                      "PiYG" = "PiYG", "PRGn" = "PRGn", "PuOr" = "PuOr",
                                      "Purples" = "Purples", "RdPu" = "RdPu", "BrBG" = "BrBG"), inline=TRUE, selected = "Spectral"   ),
                       
                       
                       
                       
                       br()
                       
                       
                ),
                
                
                column(4,
                       
                       
                       
                       
                       sliderInput(ns("max_words_all"), "max.words:",
                                   min = 1, max = 1000,
                                   value = 5),
                       
                       
                       
                       
                       sliderInput(ns("n.brewer.pal_all"), "n.brewer.pal:",
                                   min = 1, max = 100,
                                   value = 10),
                       
                       #n.brewer.pal
                       
                       
                       br()
                       
                       
                ),
                
                
                
                column(4,
                       
                       
                       
                       sliderInput(ns("min_freq_all"), "min.freq:",
                                   min = 1, max = 1000,
                                   value = 1),
                       
                       
                       textAreaInput(ns("rot.per_all"), 
                                     "rot.per", value = "0.35", height = 70, width = 100),
                       
                       
                       #rot.per=0.35
                       
                       
                       #min.freq = 4
                       
                       br()
                       
                       
                )
                
                
                
              ), #Akhir fluidrow
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud_full"))  ),
              
              
              
              
              
              
              
              
              
              br()
              
              
              
              
     ), #Akhir tabpanel Available Keyword
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     tabPanel(title = tags$h5( tags$img(src = "articles-unscreen.gif", width = "30px"), 'Find & Analysis Articles Using Keywords'),
              
              
              
              textInput(ns("get_keyword"),
                        "Input One Keyword", 
                        "Online Gambling"),
              
              br(),
              
              uiOutput(ns("tampilkan_pilihan_variabel_yang_akan_ditampilkan")),
              
              br(),
              
              uiOutput(ns("tampilkan_select_articles")),
              
              
              
              
              br(),
              
              
              uiOutput(ns("tampilkan_data_artikel")),
              
              
              
              
              
              
              
              
              
              uiOutput(ns("tampilkan_analisis_keyword")),
              
              
              
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
              
              
              
              
              uiOutput(ns("tampilkan_frekuensi_keyword")),
              
              
              br(),
              
              
              
              
              
              
              
              #######################Analisis Wordcloud
              
              
              
              
              fluidRow(
                column(4,
                       
                       radioButtons(ns("warna_wordcloud"),
                                    
                                    "Theme of Words:", 
                                    c("Blues" = "Blues", "BuGn"="BuGn",
                                      "BuPu"="BuPu", "GnBu"="GnBu", "Greens"="Greens", "YlOrRd"="YlOrRd", "YlOrBr" = "YlOrBr", "YlGnBu" = "YlGnBu",
                                      "Spectral" = "Spectral", "RdYlGn" = "RdYlGn", "YlGn" = "YlGn",
                                      "RdBu" = "RdBu", "RdGy" = "RdGy", "RdYlBu" = "RdYlBu",
                                      "PiYG" = "PiYG", "PRGn" = "PRGn", "PuOr" = "PuOr",
                                      "Purples" = "Purples", "RdPu" = "RdPu", "BrBG" = "BrBG"), inline=TRUE, selected = "Spectral"   ),
                       
                       
                       
                       
                       br()
                       
                       
                ),
                
                
                column(4,
                       
                       
                       
                       
                       sliderInput(ns("max_words"), "max.words:",
                                   min = 1, max = 1000,
                                   value = 5),
                       
                       
                       
                       
                       sliderInput(ns("n.brewer.pal"), "n.brewer.pal:",
                                   min = 1, max = 100,
                                   value = 10),
                       
                       #n.brewer.pal
                       
                       
                       br()
                       
                       
                ),
                
                
                
                column(4,
                       
                       
                       
                       sliderInput(ns("min_freq"), "min.freq:",
                                   min = 1, max = 1000,
                                   value = 1),
                       
                       
                       textAreaInput(ns("rot.per"), 
                                     "rot.per", value = "0.35", height = 70, width = 100),
                       
                       
                       #rot.per=0.35
                       
                       
                       #min.freq = 4
                       
                       br()
                       
                       
                )
                
                
                
              ), #Akhir fluidrow
              
              
              
              
              
              
              br(),
              
              
              
              
              shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud"))  ),
              
              
              #shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud"), width = "1400px", height = "900px" )),
              
              
              
              
              
              
              
              br()
              
              
     ), #Akhir tabpanel selected articles
    
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     tabPanel(title = tags$h5( tags$img(src = "unique-number-unscreen.gif", width = "30px"), 'Find & Analysis Articles Using Unique ID'),
              
              
              
              
              
              
              br()
              
              
              
              
     ), #akhir tab Find & Analysis Articles Using Unique ID
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
   
   
   tabPanel(title = tags$h5( tags$img(src = "all-articles-unscreen.gif", width = "30px"), 'All Articles'),
            
            
            h1("All Articles",style="text-shadow: -1px 0 blue,
               0 1px blue, 1px 0 blue, 0 -1px blue; text-align:center;font-size:30px"         ),
            
            
            br(),
            
            
            
            uiOutput(ns("tampilkan_pilihan_variabel_yang_akan_ditampilkan_all")),
            
            br(),
            
            uiOutput(ns("tampilkan_data_all")),
            
            
            
            
            
            
            br()
            
            
   ), #Akhir tab all articles
     
     
     
     
     
   
   
   
   
   
   tabPanel(title = tags$h5( tags$img(src = "list-journal-unscreen.gif", width = "30px"), 'List of Journal'),
            
            
            
            includeHTML("team.html"),
            
            
            br()
            
            
            
            
   ), #Akhir tabpanel About
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
     
     
     
     
       
       tabPanel(title = tags$h5( tags$img(src = "team.gif", width = "30px"), 'The Team'),
                
                
                
                includeHTML("team.html"),
                
                
                br()
                
                
                
                
       ) #Akhir tabpanel About
     
     
     
     
     
     
     
     
     
     
     
     
   ), #Akhir tabset panel
     
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
               
    
    
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
  
  
  
  
  
  
  #############All Articles###############
  
  
  
  
  
  
  output$tampilkan_pilihan_variabel_yang_akan_ditampilkan_all <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_variabel_yang_akan_ditampilkan_all"), 
                       label="Select Information:", choices = c( kirim_nama_variabel()), 
                       selected=c("Title of Article", "Author", "Number of Author", 
                                  "Year", "Volume", "Issue", "Page", "Name of Journal", "Keywords", "ISSN"), inline = TRUE )
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  output$tampilkan_data_all <- renderUI({
    
    shinycssloaders::withSpinner(DT::DTOutput(session$ns("open_data_all_article")))
    
  })
  
  
  
  
  #######################################
  
  
  output$open_data_all_article <- DT::renderDT({
    
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
    
    
    nama_terpilih <- input$terpilih_variabel_yang_akan_ditampilkan_all
    
    
    
    
    
    #print(data_artikel_terpilih[nama_terpilih])
    
    
    
    
    print(dat[nama_terpilih])
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####################
  ###################
  
  
  
  
  
  
  
  
  
  
  
  
  
  #observeEvent(input$go, {
    
    
    
    output$tampilkan_data_artikel <- renderUI({
      
      shinycssloaders::withSpinner(DT::DTOutput(session$ns("open_data_article")))
      
    })
    
    
    
    output$open_data_article <- DT::renderDT({
      
      data_artikel_terpilih <- fungsi_hitung_artikel_terpilih()
      
      
      ###########
      
      
      nama_terpilih <- input$terpilih_variabel_yang_akan_ditampilkan
      
      
      
      
      
      print(data_artikel_terpilih[nama_terpilih])
      
      
      
    }) #Akhir renderDT 
    
    
    
    
    
    output$tampilkan_select_articles <- renderUI({
      
      h1("Selected Articles",style="text-shadow: -1px 0 blue,
               0 1px blue, 1px 0 blue, 0 -1px blue; text-align:center;font-size:30px"         )
      
    })
    
    
    
    
    
    
    
    
    output$tampilkan_frekuensi_keyword <- renderUI({
      
      shinycssloaders::withSpinner(DT::DTOutput(session$ns("distribusi_frekuensi_data_keywords")))
      
    })
    
    
    
    
    
    
    output$tampilkan_analisis_keyword <- renderUI({
      
      h1("Analysis of Keywords",style="text-shadow: -1px 0 blue,
               0 1px blue, 1px 0 blue, 0 -1px blue; text-align:center;font-size:30px"         )
      
    })
    
    
    
    
    
    
    
    
    
    
    
    output$tampilkan_pilihan_variabel_yang_akan_ditampilkan <- renderUI({
      
      
      
      
      checkboxGroupInput(session$ns("terpilih_variabel_yang_akan_ditampilkan"), 
                         label="Select Information:", choices = c( kirim_nama_variabel()), 
      selected=c("Title of Article", "Author", "Number of Author", 
                 "Year", "Volume", "Issue", "Page", "Name of Journal", "Keywords", "ISSN"), inline = TRUE )
      
      

      
    })
    
    
    
    
    
    
    
    
    
    
    kirim_nama_variabel <- function()
    {
      
      nama <- c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
      
      return(nama)
      
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    output$katakunci_yang_tersedia <- DT::renderDT({
      
      dat <- read_xlsx("data_paper.xlsx")
      dat <- as.data.frame(dat)
      
      colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                        "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                        "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
      
      
      
      
      
      ambil_keyword <- dat[,"Keywords"]
      ambil_keyword <- as.data.frame(ambil_keyword)
      
      simpan_keyword <- vector(mode = "character")
      
      
      for(i in 1 :  length(ambil_keyword[,1])  )
      {
        
        X <- ambil_keyword[i,1]
        
        X <- tolower(X) #mengubah menjadi huruf kecil
        
        
        X <- unlist(strsplit(as.character(X), "  ;", fixed = TRUE))
        X <- unlist(strsplit(as.character(X), " ;", fixed = TRUE))
        X <- unlist(strsplit(as.character(X), ";  ", fixed = TRUE))
        X <- unlist(strsplit(as.character(X), "; ", fixed = TRUE))
        X <- unlist(strsplit(as.character(X), ";", fixed = TRUE))
        #X <- unlist(strsplit(as.character(X), ", ", fixed = TRUE))
        #X <- unlist(strsplit(as.character(X), "  ,", fixed = TRUE))
        #X <- unlist(strsplit(as.character(X), " ,", fixed = TRUE))
        #X <- unlist(strsplit(as.character(X), ",", fixed = TRUE))
        
        
        simpan_keyword = c(simpan_keyword, X)
        
        
        
        
      }
      
      
      
      
      
      tabel <- table(simpan_keyword)
      nama <- names(tabel)
      
      frekuensi <- unlist(tabel)
      names(frekuensi) <- NULL
      
      frekuensi <- unlist(frekuensi)
      frekuensi <- as.numeric(frekuensi)
      
      
      
      
      
      persentase <- frekuensi / sum(frekuensi) * 100
      
      persentase <- round(persentase, digits = 2)
      
      nama <- unlist(nama)
      
      
      data_tabel <- data.frame(nama, frekuensi, persentase)
      
      colnames(data_tabel) <- c("Keywords", "Frequency", "Percentage (%)")
      
      print(data_tabel)
      
      
      
      
      
      
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
 ## }) #Akhir go
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #####################
  
  
  fungsi_hitung_artikel_terpilih <- function()
  {
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
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
      #X <- unlist(strsplit(as.character(X), ", ", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), "  ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), " ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ",", fixed = TRUE))
      
      
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###################Fungsi Grafik############
  
  
  
  
  
  
  
  fungsi_kirim_grafik <- function()
  {
    
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
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
      #X <- unlist(strsplit(as.character(X), ", ", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), "  ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), " ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ",", fixed = TRUE))
      
      
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
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
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
      #X <- unlist(strsplit(as.character(X), ", ", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), "  ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), " ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ",", fixed = TRUE))
      
      
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ########################Grafik
  
  
  fungsi_grafik_wordcloud <- function()
  {
    
    
    
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
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
      #X <- unlist(strsplit(as.character(X), ", ", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), "  ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), " ,", fixed = TRUE))
      #X <- unlist(strsplit(as.character(X), ",", fixed = TRUE))
      
      
      ada_keyword <- cek_keyword %in% X
      
      if(ada_keyword == TRUE)
      {
        k = k + 1
        simpan_indeks[k] = i
        simpan_kata <- c(simpan_kata, X)
        
      }
      
    }
    
    
    
    
    simpan_keyword = simpan_kata
    
    
    simpan_keyword_hapus_spasi <- gsub(" ", "", simpan_keyword)
    simpan_keyword_hapus_spasi <- gsub("-", "", simpan_keyword_hapus_spasi)
    simpan_keyword_hapus_spasi <- gsub("'", "", simpan_keyword_hapus_spasi)
    
    
    
    
    
    
    
    text <- simpan_keyword_hapus_spasi
    
    jumlah_teks <- length(text)
    
    
    
    text_df <- data_frame(line = 1:jumlah_teks , text = text)

    simpan_kata <- text_df %>%
      unnest_tokens(word, text)
    
    rot.per <- read.csv(text=input$rot.per, header = FALSE, sep="", na.strings=c("","NA","."))
    rot.per = unlist(rot.per)
    rot.per = as.numeric(rot.per)
    angka_rot.per <- rot.per
    

    
p <-    simpan_kata %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = input$max_words,
                     min.freq = input$min_freq,           
                     random.order=FALSE, rot.per = angka_rot.per,            
                     colors=brewer.pal(input$n.brewer.pal,    input$warna_wordcloud  )))
    
    

    
    
    return(p)
    
    
    
    
  } #Akhir fungsi grafik
    
  
  
  
  
  
  
  
  
  output$grafik_wordcloud <- renderPlot({
    
    p <- fungsi_grafik_wordcloud()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################Grafik wordcloud full
  
  
  
  
  output$grafik_wordcloud_full <- renderPlot({
    

    
    
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
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
      X <- unlist(strsplit(as.character(X), ", ", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), "  ,", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), " ,", fixed = TRUE))
      X <- unlist(strsplit(as.character(X), ",", fixed = TRUE))
      
      
  
        simpan_kata <- c(simpan_kata, X)
        
      
      
    }
    
    
    
    
    simpan_keyword = simpan_kata
    
    
    simpan_keyword_hapus_spasi <- gsub(" ", "", simpan_keyword)
    simpan_keyword_hapus_spasi <- gsub("-", "", simpan_keyword_hapus_spasi)
    simpan_keyword_hapus_spasi <- gsub("'", "", simpan_keyword_hapus_spasi)
    
    
    
    
    
    
    
    text <- simpan_keyword_hapus_spasi
    
    jumlah_teks <- length(text)
    
    
    
    text_df <- data_frame(line = 1:jumlah_teks , text = text)
    
    simpan_kata <- text_df %>%
      unnest_tokens(word, text)
    
    rot.per <- read.csv(text=input$rot.per_all, header = FALSE, sep="", na.strings=c("","NA","."))
    rot.per = unlist(rot.per)
    rot.per = as.numeric(rot.per)
    angka_rot.per <- rot.per
    
    
    
    p <-    simpan_kata %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = input$max_words_all,
                     min.freq = input$min_freq_all,           
                     random.order=FALSE, rot.per = angka_rot.per,            
                     colors=brewer.pal(input$n.brewer.pal_all,    input$warna_wordcloud_all  )))
    
    
    
    
    
    print(p)
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
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














