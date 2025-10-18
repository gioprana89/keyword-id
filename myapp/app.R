library(shinyscreenshot)
library(scales)
library(readxl)
library(ggplot2)
library(stringr)
library(shiny)
library(ggthemes)
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)

library(tidytext)

library(wordcloud)

library(widyr)
library(ggthemes)

library(tidygraph)
#library(tidyverse)







           

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
     
     
     
     
     tabPanel(title = tags$h5( tags$img(src = "selected journal new.gif", width = "30px"), 'Journal Selection'),
              
              
            #  h1("Select Journal",style="text-shadow: -1px 0 blue,
             #  0 1px blue, 1px 0 blue, 0 -1px blue; text-align:center;font-size:30px"         ),
              
             # br(),
              
              #shinycssloaders::withSpinner(verbatimTextOutput(ns("informasi_cetak"))),
              
              
              
              
              
              br(),
              
              #shinycssloaders::withSpinner(uiOutput(ns("tampilkan_informasi_semua_jurnal_dan_jumlah_artikel"))),
              
              
              br(),
              
              
              
              
              
              
              
              
              
              
              fluidRow(
                column(4,
                       
                       uiOutput(ns("tampilkan_nama_jurnal_kesehatan")),
                       
                       br()
                       
                ),
                
                
                
                column(4,
                       
                       
                       uiOutput(ns("tampilkan_nama_jurnal_psikologi")),
                       
                       br()
                       
                ),
                
                
                
                column(4,
                       
                       uiOutput(ns("tampilkan_nama_jurnal_ekonomi")),
                       
                       br()
                       
                )
                
                
                
                
                
              ), #Akhir Fluidrow
              
              
              
           
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              fluidRow(
                column(4,
                       
                       uiOutput(ns("tampilkan_nama_jurnal_agama_dan_hukum")),
                       
                       br()
                       
                ),
                
                
                
                column(4,
                       
                       
                       uiOutput(ns("tampilkan_nama_jurnal_matematika")),
                       
                       br()
                       
                ),
                
                
                
                column(4,
                       
                       uiOutput(ns("tampilkan_nama_jurnal_Science_and_Engineering")),
                     
                       br()
                       
                )
                
                
                
                
                
              ), #Akhir Fluidrow
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              br(),
              
              
           
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
                       
              
              
              
              br()       
              
     ), #Akhir tab journal selection
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     tabPanel(title = tags$h5( tags$img(src = "jurnal-terpilih-unscreen.gif", width = "30px"), 'The Journal You Choose'),
              
              
              # h1("Select Journal",style="text-shadow: -1px 0 blue,
             # 0 1px blue, 1px 0 blue, 0 -1px blue; text-align:center;font-size:30px"         ),
              
              #br(),
              
              shinycssloaders::withSpinner(verbatimTextOutput(ns("informasi_cetak"))),
              
              
              


br()



), #Akhir tab the journal you choose
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     tabPanel(title = tags$h5( tags$img(src = "katakunci-unscreen.gif", width = "30px"), 'Available Keywords'),
              
              
              
              shinycssloaders::withSpinner(verbatimTextOutput(ns("informasi_cetak_untuk_tab_available_keywords"))),
              
              
              br(),
              
              
              h1("Available Keywords",style="text-shadow: -1px 0 blue,
               0 1px blue, 1px 0 blue, 0 -1px blue; text-align:center;font-size:30px"         ),
              
              
              br(),
              
              shinycssloaders::withSpinner(DT::DTOutput(ns("katakunci_yang_tersedia"))),
              
              
              
              br(),
              
              
              
              
              fluidRow(
                column(4,
                       
                       
                       
                       
                       sliderInput(ns("grafik_coocur_jumlah_coocur_keseluruhan_ya"), "number of co-occurrences:",
                                   min = 1, max = 1000,
                                   value = 100),
                       
                       
                       br()
                       
                       
                ),
                
                
                column(4,
                       
                       
                       sliderInput(ns("grafik_coocur_ukuran_teks_keseluruhan_ya"), "text size:",
                                   min = 1, max = 20,
                                   value = 5, step = 0.1),
                       
                       
                       br()
                       
                       
                ),
                
                
                column(4,
                       
                       radioButtons(ns("grafik_coocur_tipe_grafik_keseluruhan_ya"),
                                    
                                    "Type of Graph:", 
                                    c("1" = "1", "2"="2",
                                      "3"="3", "4"="4", "5" = "5"), inline=TRUE, selected = "1"   ),
                       
                       
                       
                       
                       br()
                       
                       
                )
                
                
                
              ),
              
              
              shinycssloaders::withSpinner(plotOutput(ns("grafik_pemetaan_kata_kunci_keseluruhan"), width = "100%", height = "900px" )),
              
              
              br(),
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
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud_full"), width = "100%", height = "900px" ) ),
              
              
              
              
              
              
              
              
              
              br()
              
              
              
              
     ), #Akhir tabpanel Available Keyword
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     tabPanel(title = tags$h5( tags$img(src = "articles-unscreen.gif", width = "30px"), 'Find & Analysis Articles Using Keywords'),
              
              
              
              textInput(ns("get_keyword"),
                        "Input One Keyword", 
                        "financial performance"),
              
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
              
              
              
              
              shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud"), width = "100%", height = "900px")  ),
              
              
              #shinycssloaders::withSpinner(plotOutput(ns("grafik_wordcloud"), width = "1400px", height = "900px" )),
              
              
              
              
              
              
              
              
              
              
              
              br(),
              
              br(),
              
              br(),
              
              
              
              fluidRow(
                column(4,
                       
                       
                       
                       
                       sliderInput(ns("grafik_coocur_jumlah_coocur"), "number of co-occurrences:",
                                   min = 1, max = 1000,
                                   value = 20),
                       
                       
                       br()
                       
                       
                ),
                
                
                column(4,
                       
                       
                       sliderInput(ns("grafik_coocur_ukuran_teks"), "text size:",
                                   min = 1, max = 20,
                                   value = 3, step = 0.1),
                       
                       
                       br()
                       
                       
                ),
                
                
                column(4,
                       
                       radioButtons(ns("grafik_coocur_tipe_grafik"),
                                    
                                    "Type of Graph:", 
                                    c("1" = "1", "2"="2",
                                      "3"="3", "4"="4", "5" = "5"), inline=TRUE, selected = "1"   ),
                       
                       
                       
                       
                       br()
                       
                       
                )
                
                
                
              ),
              
              ##########5 Oktober 2025############
              
              
              
              
              
              
              #shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci_1"), width = "100%" )),
              
              
              
              
              
              tabsetPanel(
                
                
                tabPanel("300 x 300",
                         
                         #actionButton(ns('cetak_gambar_300_300_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_300_300_type1"), width = "300px", height = "300px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("500 x 300",
                         
                         
                         #actionButton(ns('cetak_gambar_500_300_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_500_300_type1"), width = "500px", height = "300px" )),
                         
                         br()
                         
                ),
                
                
                
                
                tabPanel("700 x 300",
                         
                         
                      #   actionButton(ns('cetak_gambar_700_300_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_700_300_type1"), width = "700px", height = "300px" )),
                         
                         br()
                         
                ),
                
                
                
                
                tabPanel("900 x 300",
                         
                         
                      #   actionButton(ns('cetak_gambar_900_300_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_900_300_type1"), width = "900px", height = "300px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("1100 x 300",
                         
                         
                       #  actionButton(ns('cetak_gambar_1100_300_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1100_300_type1"), width = "1100px", height = "300px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("1200 x 300",
                         
                         
                       #  actionButton(ns('cetak_gambar_1200_300_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1200_300_type1"), width = "1200px", height = "300px" )),
                         
                         br()
                         
                ),
                
                
                tabPanel("1300 x 300",
                         
                       #  actionButton(ns('cetak_gambar_1300_300_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1300_300_type1"), width = "1300px", height = "300px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("1400 x 300",
                         
                       #  actionButton(ns('cetak_gambar_1400_300_type1'),'Print'),
                         
                         br(),
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1400_300_type1"), width = "1400px", height = "300px" )),
                         
                         br()
                         
                ),
                
                
                
                
                
                
                
                
                
                
                
                tabPanel("300 x 500",
                         
                       #  actionButton(ns('cetak_gambar_300_500_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_300_500_type1"), width = "300px", height = "500px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("500 x 500",
                         
                         
                       #  actionButton(ns('cetak_gambar_500_500_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_500_500_type1"), width = "500px", height = "500px" )),
                         
                         br()
                         
                ),
                
                
                
                
                tabPanel("700 x 500",
                         
                         
                       #  actionButton(ns('cetak_gambar_700_500_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_700_500_type1"), width = "700px", height = "500px" )),
                         
                         br()
                         
                ),
                
                
                
                
                tabPanel("900 x 500",
                         
                         
                      #   actionButton(ns('cetak_gambar_900_500_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_900_500_type1"), width = "900px", height = "500px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("1100 x 500",
                         
                         
                       #  actionButton(ns('cetak_gambar_1100_500_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1100_500_type1"), width = "1100px", height = "500px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("1200 x 500",
                         
                         
                     #    actionButton(ns('cetak_gambar_1200_500_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1200_500_type1"), width = "1200px", height = "500px" )),
                         
                         br()
                         
                ),
                
                
                tabPanel("1300 x 500",
                         
                    #     actionButton(ns('cetak_gambar_1300_500_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1300_500_type1"), width = "1300px", height = "500px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("1400 x 500",
                         
                     #    actionButton(ns('cetak_gambar_1400_500_type1'),'Print'),
                         
                         br(),
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1400_500_type1"), width = "1400px", height = "500px" )),
                         
                         br()
                         
                ),
                
                
                
                
                
                
                
                
                
                
                
                tabPanel("300 x 700",
                         
                      #   actionButton(ns('cetak_gambar_300_700_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_300_700_type1"), width = "300px", height = "700px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("500 x 700",
                         
                         
                     #    actionButton(ns('cetak_gambar_500_700_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_500_700_type1"), width = "500px", height = "700px" )),
                         
                         br()
                         
                ),
                
                
                
                
                tabPanel("700 x 700",
                         
                         
                    #     actionButton(ns('cetak_gambar_700_700_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_700_700_type1"), width = "700px", height = "700px" )),
                         
                         br()
                         
                ),
                
                
                
                
                tabPanel("900 x 700",
                         
                         
                   #      actionButton(ns('cetak_gambar_900_700_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_900_700_type1"), width = "900px", height = "700px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("1100 x 700",
                         
                         
                     #    actionButton(ns('cetak_gambar_1100_700_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1100_700_type1"), width = "1100px", height = "700px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("1200 x 700",
                         
                         
                     #    actionButton(ns('cetak_gambar_1200_700_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1200_700_type1"), width = "1200px", height = "700px" )),
                         
                         br()
                         
                ),
                
                
                tabPanel("1300 x 700",
                         
                     #    actionButton(ns('cetak_gambar_1300_700_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1300_700_type1"), width = "1300px", height = "700px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("1400 x 700",
                         
                     #    actionButton(ns('cetak_gambar_1400_700_type1'),'Print'),
                         
                         br(),
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1400_700_type1"), width = "1400px", height = "700px" )),
                         
                         br()
                         
                ),
                
                
                
                
                tabPanel("300 x 900",
                         
                    #     actionButton(ns('cetak_gambar_300_900_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_300_900_type1"), width = "300px", height = "900px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("500 x 900",
                         
                         
                   #      actionButton(ns('cetak_gambar_500_900_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_500_900_type1"), width = "500px", height = "900px" )),
                         
                         br()
                         
                ),
                
                
                
                
                tabPanel("700 x 900",
                         
                         
                     #    actionButton(ns('cetak_gambar_700_900_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_700_900_type1"), width = "700px", height = "900px" )),
                         
                         br()
                         
                ),
                
                
                
                
                tabPanel("900 x 900",
                         
                         
                     #    actionButton(ns('cetak_gambar_900_900_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_900_900_type1"), width = "900px", height = "900px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("1100 x 900",
                         
                         
                    #     actionButton(ns('cetak_gambar_1100_900_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1100_900_type1"), width = "1100px", height = "900px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("1200 x 900",
                         
                         
                    #     actionButton(ns('cetak_gambar_1200_900_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1200_900_type1"), width = "1200px", height = "900px" )),
                         
                         br()
                         
                ),
                
                
                tabPanel("1300 x 900",
                         
                     #    actionButton(ns('cetak_gambar_1300_900_type1'),'Print'),
                         
                         br(),
                         
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1300_900_type1"), width = "1300px", height = "900px" )),
                         
                         br()
                         
                ),
                
                
                
                tabPanel("1400 x 900",
                         
                     #    actionButton(ns('cetak_gambar_1400_900_type1'),'Print'),
                         
                         br(),
                         
                         shinycssloaders::withSpinner(plotOutput(ns("pemetaan_kata_kunci1_1400_900_type1"), width = "1400px", height = "900px" )),
                         
                         br()
                         
                )
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
              ), #akhir dari tabset panel
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
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
            
            
            
            h1("List of Journal",style="text-shadow: -1px 0 blue,
               0 1px blue, 1px 0 blue, 0 -1px blue; text-align:center;font-size:30px"         ),
            
            
            br(),
            
            
            
            uiOutput(ns("tampilkan_data_list_jurnal")),
            
            
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
  
  
  
  
  ###########12 Oktober 2025, Jurnal Kesehatan
  ###########12 Oktober 2025, Jurnal Kesehatan
  ###########12 Oktober 2025, Jurnal Kesehatan
  ###########12 Oktober 2025, Jurnal Kesehatan
  ###########12 Oktober 2025, Jurnal Kesehatan
  ###########12 Oktober 2025, Jurnal Kesehatan
  
  
  fungsi_nama_jurnal_kesehatan <- function()
  {
    
    ISSN_jurnal_kesehatan <- c("23563656", "25407872", "30316502", "24771570", "2540881X", "27754952", "26204126", "25409301", "28299760", "25485334", "29618681", "2541092X", "23564067")
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
    indeks <- dat[,"ISSN"] %in% ISSN_jurnal_kesehatan
    indeks <- which(indeks == TRUE)
    data_jurnal_kesehatan <- dat[c(indeks),]
    
    dat <- data_jurnal_kesehatan
    
    
    grup <- group_by(dat, `Name of Journal`, ISSN)
    
    data_jurnal <- grup %>% summarise(
      freq = n()
      
    )
    
    data_jurnal <- as.data.frame(data_jurnal)
    
    colnames(data_jurnal) = c("Journal", "ISSN", "Number of Articles in Our Database")
    
    
    
    informasi_jurnal_jumlah_artikel <- vector(mode = "character")
    
    for( i in 1 : length(data_jurnal[,1]))
    {
      
      nama <- data_jurnal[i,"Journal"]
      issn <- data_jurnal[i,"ISSN"]
      jumlah <- data_jurnal[i,"Number of Articles in Our Database"]
      informasi_jurnal_jumlah_artikel[i] <- paste0(nama,"--",issn," (",jumlah,")")
      
    }
    
    data_jurnal_update <- data.frame(data_jurnal, informasi_jurnal_jumlah_artikel)
    colnames(data_jurnal_update) <- c("Journal", "ISSN", "Number of Articles in Our Database", "Information")
    
    
    return(informasi_jurnal_jumlah_artikel)
    
    
  }
  
  
  
  
  
  
  
  ##############
  ##############
  ##############
  
  
  
  output$tampilkan_nama_jurnal_kesehatan <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_fungsi_nama_jurnal_kesehatan"), 
                       label="Health:", choices = c( fungsi_nama_jurnal_kesehatan()), 
                       selected=c(   ), inline = FALSE )
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###########12 Oktober 2025, Jurnal Psikologi
  ###########12 Oktober 2025, Jurnal Psikologi
  ###########12 Oktober 2025, Jurnal Psikologi
  ###########12 Oktober 2025, Jurnal Psikologi
  ###########12 Oktober 2025, Jurnal Psikologi
  ###########12 Oktober 2025, Jurnal Psikologi
  
  
  fungsi_nama_jurnal_psikologi <- function()
  {
    
    ISSN_jurnal_psikologi <- c("25277456", "26213893", "26543672", "26545713", "26564173", "27156206", "27208958",
                               "27210626", "27227669", "24772674", "25285858", "25481800", "25496166", "25499882", "25796321", 
                               "25801228", "25807331", "26155168", "26158183", "2460867X", "25496468", "2541450X", "25796518",
                               "26158558", "23021098", "25022903")
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
    indeks <- dat[,"ISSN"] %in% ISSN_jurnal_psikologi
    indeks <- which(indeks == TRUE)
    data_jurnal_psikologi <- dat[c(indeks),]
    
    dat <- data_jurnal_psikologi
    
    
    grup <- group_by(dat, `Name of Journal`, ISSN)
    
    data_jurnal <- grup %>% summarise(
      freq = n()
      
    )
    
    data_jurnal <- as.data.frame(data_jurnal)
    
    colnames(data_jurnal) = c("Journal", "ISSN", "Number of Articles in Our Database")
    
    
    
    informasi_jurnal_jumlah_artikel <- vector(mode = "character")
    
    for( i in 1 : length(data_jurnal[,1]))
    {
      
      nama <- data_jurnal[i,"Journal"]
      issn <- data_jurnal[i,"ISSN"]
      jumlah <- data_jurnal[i,"Number of Articles in Our Database"]
      informasi_jurnal_jumlah_artikel[i] <- paste0(nama,"--",issn," (",jumlah,")")
      
    }
    
    data_jurnal_update <- data.frame(data_jurnal, informasi_jurnal_jumlah_artikel)
    colnames(data_jurnal_update) <- c("Journal", "ISSN", "Number of Articles in Our Database", "Information")
    
    
    return(informasi_jurnal_jumlah_artikel)
    
    
  }
  
  
  
  
  
  
  
  ##############
  ##############
  ##############
  
  
  
  output$tampilkan_nama_jurnal_psikologi <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_fungsi_nama_jurnal_psikologi"), 
                       label="Psychology:", choices = c( fungsi_nama_jurnal_psikologi()), 
                       selected=c(    ), inline = FALSE )
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###########12 Oktober 2025, Jurnal ekonomi
  ###########12 Oktober 2025, Jurnal ekonomi
  ###########12 Oktober 2025, Jurnal ekonomi
  ###########12 Oktober 2025, Jurnal ekonomi
  ###########12 Oktober 2025, Jurnal ekonomi
  ###########12 Oktober 2025, Jurnal ekonomi
  
  
  fungsi_nama_jurnal_ekonomi <- function()
  {
    
    ISSN_jurnal_ekonomi <- c("25273027", "24069280", "25275143", "25974564", "2621668X", "24610771", 
                             "20895879", "26150689", "25286528", "25498800", "23387238", "23388137")
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
    indeks <- dat[,"ISSN"] %in% ISSN_jurnal_ekonomi
    indeks <- which(indeks == TRUE)
    data_jurnal_ekonomi <- dat[c(indeks),]
    
    dat <- data_jurnal_ekonomi
    
    
    grup <- group_by(dat, `Name of Journal`, ISSN)
    
    data_jurnal <- grup %>% summarise(
      freq = n()
      
    )
    
    data_jurnal <- as.data.frame(data_jurnal)
    
    colnames(data_jurnal) = c("Journal", "ISSN", "Number of Articles in Our Database")
    
    
    
    informasi_jurnal_jumlah_artikel <- vector(mode = "character")
    
    for( i in 1 : length(data_jurnal[,1]))
    {
      
      nama <- data_jurnal[i,"Journal"]
      issn <- data_jurnal[i,"ISSN"]
      jumlah <- data_jurnal[i,"Number of Articles in Our Database"]
      informasi_jurnal_jumlah_artikel[i] <- paste0(nama,"--",issn," (",jumlah,")")
      
    }
    
    data_jurnal_update <- data.frame(data_jurnal, informasi_jurnal_jumlah_artikel)
    colnames(data_jurnal_update) <- c("Journal", "ISSN", "Number of Articles in Our Database", "Information")
    
    
    return(informasi_jurnal_jumlah_artikel)
    
    
  }
  
  
  
  
  
  
  
  ##############
  ##############
  ##############
  
  
  
  output$tampilkan_nama_jurnal_ekonomi <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_fungsi_nama_jurnal_ekonomi"), 
                       label="Economy:", choices = c( fungsi_nama_jurnal_ekonomi()), 
                       selected=c(    ), inline = FALSE )
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###########12 Oktober 2025, Jurnal agama_dan_hukum
  ###########12 Oktober 2025, Jurnal agama_dan_hukum
  ###########12 Oktober 2025, Jurnal agama_dan_hukum
  ###########12 Oktober 2025, Jurnal agama_dan_hukum
  ###########12 Oktober 2025, Jurnal agama_dan_hukum
  ###########12 Oktober 2025, Jurnal agama_dan_hukum
  
  
  fungsi_nama_jurnal_agama_dan_hukum <- function()
  {
    
    ISSN_jurnal_agama_dan_hukum <- c("25802763", "27215040")
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
    indeks <- dat[,"ISSN"] %in% ISSN_jurnal_agama_dan_hukum
    indeks <- which(indeks == TRUE)
    data_jurnal_agama_dan_hukum <- dat[c(indeks),]
    
    dat <- data_jurnal_agama_dan_hukum
    
    
    grup <- group_by(dat, `Name of Journal`, ISSN)
    
    data_jurnal <- grup %>% summarise(
      freq = n()
      
    )
    
    data_jurnal <- as.data.frame(data_jurnal)
    
    colnames(data_jurnal) = c("Journal", "ISSN", "Number of Articles in Our Database")
    
    
    
    informasi_jurnal_jumlah_artikel <- vector(mode = "character")
    
    for( i in 1 : length(data_jurnal[,1]))
    {
      
      nama <- data_jurnal[i,"Journal"]
      issn <- data_jurnal[i,"ISSN"]
      jumlah <- data_jurnal[i,"Number of Articles in Our Database"]
      informasi_jurnal_jumlah_artikel[i] <- paste0(nama,"--",issn," (",jumlah,")")
      
    }
    
    data_jurnal_update <- data.frame(data_jurnal, informasi_jurnal_jumlah_artikel)
    colnames(data_jurnal_update) <- c("Journal", "ISSN", "Number of Articles in Our Database", "Information")
    
    
    return(informasi_jurnal_jumlah_artikel)
    
    
  }
  
  
  
  
  
  
  
  ##############
  ##############
  ##############
  
  
  
  output$tampilkan_nama_jurnal_agama_dan_hukum <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_fungsi_nama_jurnal_agama_dan_hukum"), 
                       label="Religion & Law:", choices = c( fungsi_nama_jurnal_agama_dan_hukum()), 
                       selected=c(   ), inline = FALSE )
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###########12 Oktober 2025, Jurnal matematika
  ###########12 Oktober 2025, Jurnal matematika
  ###########12 Oktober 2025, Jurnal matematika
  ###########12 Oktober 2025, Jurnal matematika
  ###########12 Oktober 2025, Jurnal matematika
  ###########12 Oktober 2025, Jurnal matematika
  
  
  fungsi_nama_jurnal_matematika <- function()
  {
    
    ISSN_jurnal_matematika <- c("25491040", "25805754")
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
    indeks <- dat[,"ISSN"] %in% ISSN_jurnal_matematika
    indeks <- which(indeks == TRUE)
    data_jurnal_matematika <- dat[c(indeks),]
    
    dat <- data_jurnal_matematika
    
    
    grup <- group_by(dat, `Name of Journal`, ISSN)
    
    data_jurnal <- grup %>% summarise(
      freq = n()
      
    )
    
    data_jurnal <- as.data.frame(data_jurnal)
    
    colnames(data_jurnal) = c("Journal", "ISSN", "Number of Articles in Our Database")
    
    
    
    informasi_jurnal_jumlah_artikel <- vector(mode = "character")
    
    for( i in 1 : length(data_jurnal[,1]))
    {
      
      nama <- data_jurnal[i,"Journal"]
      issn <- data_jurnal[i,"ISSN"]
      jumlah <- data_jurnal[i,"Number of Articles in Our Database"]
      informasi_jurnal_jumlah_artikel[i] <- paste0(nama,"--",issn," (",jumlah,")")
      
    }
    
    data_jurnal_update <- data.frame(data_jurnal, informasi_jurnal_jumlah_artikel)
    colnames(data_jurnal_update) <- c("Journal", "ISSN", "Number of Articles in Our Database", "Information")
    
    
    return(informasi_jurnal_jumlah_artikel)
    
    
  }
  
  
  
  
  
  
  
  ##############
  ##############
  ##############
  
  
  
  output$tampilkan_nama_jurnal_matematika <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_fungsi_nama_jurnal_matematika"), 
                       label="Mathematics:", choices = c( fungsi_nama_jurnal_matematika()), 
                       selected=c(   ), inline = FALSE )
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###########15 Oktober 2025, Jurnal Science_and_Engineering
  ###########15 Oktober 2025, Jurnal Science_and_Engineering
  ###########15 Oktober 2025, Jurnal Science_and_Engineering
  ###########15 Oktober 2025, Jurnal Science_and_Engineering
  ###########15 Oktober 2025, Jurnal Science_and_Engineering
  ###########15 Oktober 2025, Jurnal Science_and_Engineering
  
  
  fungsi_nama_jurnal_Science_and_Engineering <- function()
  {
    
    ISSN_jurnal_Science_and_Engineering <- c("27222578")
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
    indeks <- dat[,"ISSN"] %in% ISSN_jurnal_Science_and_Engineering
    indeks <- which(indeks == TRUE)
    data_jurnal_Science_and_Engineering <- dat[c(indeks),]
    
    dat <- data_jurnal_Science_and_Engineering
    
    
    grup <- group_by(dat, `Name of Journal`, ISSN)
    
    data_jurnal <- grup %>% summarise(
      freq = n()
      
    )
    
    data_jurnal <- as.data.frame(data_jurnal)
    
    colnames(data_jurnal) = c("Journal", "ISSN", "Number of Articles in Our Database")
    
    
    
    informasi_jurnal_jumlah_artikel <- vector(mode = "character")
    
    for( i in 1 : length(data_jurnal[,1]))
    {
      
      nama <- data_jurnal[i,"Journal"]
      issn <- data_jurnal[i,"ISSN"]
      jumlah <- data_jurnal[i,"Number of Articles in Our Database"]
      informasi_jurnal_jumlah_artikel[i] <- paste0(nama,"--",issn," (",jumlah,")")
      
    }
    
    data_jurnal_update <- data.frame(data_jurnal, informasi_jurnal_jumlah_artikel)
    colnames(data_jurnal_update) <- c("Journal", "ISSN", "Number of Articles in Our Database", "Information")
    
    
    return(informasi_jurnal_jumlah_artikel)
    
    
  }
  
  
  
  
  
  
  
  ##############
  ##############
  ##############
  
  
  
  output$tampilkan_nama_jurnal_Science_and_Engineering <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_fungsi_nama_jurnal_Science_and_Engineering"), 
                       label="Science & Engineering:", choices = c( fungsi_nama_jurnal_Science_and_Engineering()), 
                       selected=c(    ), inline = FALSE )
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##############11 Oktober 2025###############
  
  
  fungsi_tampilkan_informasi_semua_jurnal_dan_jumlah_artikel <- function()
  {
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
    
    ###################
    
    ISSN_jurnal_kesehatan <- c("23563656", "25407872", "30316502", "24771570", "2540881X", "27754952", "26204126", "25409301", "28299760", "25485334", "29618681", "2541092X", "23564067")
    

    indeks <- dat[,"ISSN"] %in% ISSN_jurnal_kesehatan
    indeks <- which(indeks == FALSE) #ambil artikel yang bukan dari jurnal kesehatan
    dat <- dat[c(indeks),]
    
    
    
    ###################
    
    ISSN_jurnal_psikologi <- c("25277456", "26213893", "26543672", "26545713", "26564173", "27156206", "27208958",
                               "27210626", "27227669", "24772674", "25285858", "25481800", "25496166", "25499882", "25796321", 
                               "25801228", "25807331", "26155168", "26158183", "2460867X", "25496468", "2541450X", "25796518",
                               "26158558", "23021098", "25022903")
    
    
    indeks <- dat[,"ISSN"] %in% ISSN_jurnal_psikologi
    indeks <- which(indeks == FALSE) #ambil artikel yang bukan dari jurnal psikologi
    dat <- dat[c(indeks),]
    
    
    
    ###################
    
    ISSN_jurnal_ekonomi <- c("25273027", "24069280", "25275143", "25974564", 
                             "2621668X", "24610771", "20895879", "26150689", 
                             "25286528", "25498800", "23387238", "23388137")
    
    indeks <- dat[,"ISSN"] %in% ISSN_jurnal_ekonomi
    indeks <- which(indeks == FALSE) #ambil artikel yang bukan dari jurnal ekonomi
    dat <- dat[c(indeks),]
    
    
    
    ############################
    
    
    ISSN_jurnal_agama_dan_hukum <- c("25802763", "27215040")
    
    indeks <- dat[,"ISSN"] %in% ISSN_jurnal_agama_dan_hukum
    indeks <- which(indeks == FALSE) #ambil artikel yang bukan dari jurnal agama dan hukum
    dat <- dat[c(indeks),]
    
    
    
    
    
    
    ############################
    
    
    ISSN_jurnal_matematika <- c("25491040", "25805754")
    
    indeks <- dat[,"ISSN"] %in% ISSN_jurnal_matematika
    indeks <- which(indeks == FALSE) #ambil artikel yang bukan dari jurnal matematika
    dat <- dat[c(indeks),]
    
    
    
    
    
    
    
    
    ############################
    
    
    ISSN_jurnal_Science_and_Engineering <- c("27222578")
    
    indeks <- dat[,"ISSN"] %in% ISSN_jurnal_Science_and_Engineering
    indeks <- which(indeks == FALSE) #ambil artikel yang bukan dari jurnal bidang science and enginering
    dat <- dat[c(indeks),]
    
    
    
    
    
    
    
    
    
    ############################################Jurnal yang Dipilih#############################
    ############################################Jurnal yang Dipilih#############################
    ############################################Jurnal yang Dipilih#############################
    ############################################Jurnal yang Dipilih#############################
    ############################################Jurnal yang Dipilih#############################
    ############################################Jurnal yang Dipilih#############################
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    grup <- group_by(dat, `Name of Journal`, ISSN)
    
    data_jurnal <- grup %>% summarise(
      freq = n()
      
    )
    
    data_jurnal <- as.data.frame(data_jurnal)
    
    colnames(data_jurnal) = c("Journal", "ISSN", "Number of Articles in Our Database")
    
    
    
    informasi_jurnal_jumlah_artikel <- vector(mode = "character")
    
    for( i in 1 : length(data_jurnal[,1]))
    {
      
      nama <- data_jurnal[i,"Journal"]
      issn <- data_jurnal[i,"ISSN"]
      jumlah <- data_jurnal[i,"Number of Articles in Our Database"]
      informasi_jurnal_jumlah_artikel[i] <- paste0(nama,"--",issn," (",jumlah,")")
      
    }
    
    data_jurnal_update <- data.frame(data_jurnal, informasi_jurnal_jumlah_artikel)
    colnames(data_jurnal_update) <- c("Journal", "ISSN", "Number of Articles in Our Database", "Information")
    
    
    return(informasi_jurnal_jumlah_artikel)
    
    
    
  }
  
  
  
  
  
  
  
  
  
  
  
  ##############
  
  
  
  
  output$tampilkan_informasi_semua_jurnal_dan_jumlah_artikel <- renderUI({
    
    
    
    
    checkboxGroupInput(session$ns("terpilih_tampilkan_informasi_semua_jurnal_dan_jumlah_artikel"), 
                       label="Select Journal(s):", choices = c( fungsi_tampilkan_informasi_semua_jurnal_dan_jumlah_artikel()), 
                       selected=c( fungsi_tampilkan_informasi_semua_jurnal_dan_jumlah_artikel()   ), inline = TRUE )
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  output$informasi_cetak <- renderPrint({
    
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
    
    
    jumlah_jurnal <- length(  levels(     as.factor(    dat[,"Name of Journal"]       )         )        )
    jumlah_artikel <- length(   dat[,1]     )
    
    
    cat(sprintf("Number of Journal in Our Database: %d\n\n", jumlah_jurnal))
    cat(sprintf("Number of Article in Our Database: %d\n\n", jumlah_artikel))
    
    
    cat(sprintf("The Journal That You Choose: \n\n"))
    
    
    
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    
    jurnal_terpilih <- vector(mode = "character")
    
    if( length(input$terpilih_fungsi_nama_jurnal_matematika) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_matematika)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_kesehatan) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_kesehatan)
    }
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_psikologi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_psikologi)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_ekonomi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_ekonomi)
    }
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_agama_dan_hukum) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_agama_dan_hukum)
    }
    
    
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_Science_and_Engineering) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_Science_and_Engineering)
    }
    
    
    
    
   # terpilih_artikel_di_jurnal_matematika <- input$terpilih_fungsi_nama_jurnal_matematika
    
    
    #terpilih_artikel_di_jurnal_kesehatan <- input$terpilih_fungsi_nama_jurnal_kesehatan
    #terpilih_artikel_di_jurnal_psikologi <-  input$terpilih_fungsi_nama_jurnal_psikologi
    #terpilih_artikel_di_jurnal_ekonomi <- input$terpilih_fungsi_nama_jurnal_ekonomi
    #terpilih_artikel_di_jurnal_agama_dan_kesehatan <-  input$terpilih_fungsi_nama_jurnal_agama_dan_hukum
    
    

    
    
    print(jurnal_terpilih)
    
    
    
    simpan_nama_jurnal_terpilih <- ""
    
    
    for(i in 1 : length(jurnal_terpilih))
    {
      a <-  strsplit(jurnal_terpilih[i], "--")
      
      simpan_nama_jurnal_terpilih[i] <- a[[1]][1]
    }
    
    
    
    #print(simpan_nama_jurnal_terpilih)
    
    indeks_jurnal_terpilih <- dat[,"Name of Journal"] %in% simpan_nama_jurnal_terpilih 
    indeks_jurnal_terpilih <- which(indeks_jurnal_terpilih == TRUE)
    
    
    jumlah_jurnal_terpilih <- length(indeks_jurnal_terpilih)
    
    cat(sprintf("With Number of Article: %d", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    
  })
  
  
  
  
  #################
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #############All Articles###############
  
  
  
  
  output$informasi_cetak_untuk_tab_available_keywords <- renderPrint({
    
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
    
    
    jumlah_jurnal <- length(  levels(     as.factor(    dat[,"Name of Journal"]       )         )        )
    jumlah_artikel <- length(   dat[,1]     )
    
    
    #cat(sprintf("Number of Journal in Our Database: %d\n\n", jumlah_jurnal))
    #cat(sprintf("Number of Article in Our Database: %d\n\n", jumlah_artikel))
    
    
    #cat(sprintf("The Journal That You Choose: \n\n"))
    
    
    
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    
    jurnal_terpilih <- vector(mode = "character")
    
    if( length(input$terpilih_fungsi_nama_jurnal_matematika) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_matematika)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_kesehatan) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_kesehatan)
    }
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_psikologi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_psikologi)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_ekonomi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_ekonomi)
    }
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_agama_dan_hukum) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_agama_dan_hukum)
    }
    
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_Science_and_Engineering) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_Science_and_Engineering)
    }
    
    
    
    # terpilih_artikel_di_jurnal_matematika <- input$terpilih_fungsi_nama_jurnal_matematika
    
    
    #terpilih_artikel_di_jurnal_kesehatan <- input$terpilih_fungsi_nama_jurnal_kesehatan
    #terpilih_artikel_di_jurnal_psikologi <-  input$terpilih_fungsi_nama_jurnal_psikologi
    #terpilih_artikel_di_jurnal_ekonomi <- input$terpilih_fungsi_nama_jurnal_ekonomi
    #terpilih_artikel_di_jurnal_agama_dan_kesehatan <-  input$terpilih_fungsi_nama_jurnal_agama_dan_hukum
    
    
    
    
    
    #print(jurnal_terpilih)
    
    
    
    simpan_nama_jurnal_terpilih <- ""
    
    
    for(i in 1 : length(jurnal_terpilih))
    {
      a <-  strsplit(jurnal_terpilih[i], "--")
      
      simpan_nama_jurnal_terpilih[i] <- a[[1]][1]
    }
    
    
    
    #print(simpan_nama_jurnal_terpilih)
    
    indeks_jurnal_terpilih <- dat[,"Name of Journal"] %in% simpan_nama_jurnal_terpilih 
    indeks_jurnal_terpilih <- which(indeks_jurnal_terpilih == TRUE)
    
    
    jumlah_jurnal_terpilih <- length(indeks_jurnal_terpilih)
    
    #cat(sprintf("With Number of Article: %d", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    #######################
    #######################
    
    
    
    cat(sprintf("Number of Journal That You Choose: %d Journal\n\n", length(jurnal_terpilih)))
    
    
    cat(sprintf("Number of Article: %d\n\n", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #######Dengan jumlah keywords
    
    dat <- dat[c(indeks_jurnal_terpilih),]
    
    
    
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
    
    
    cat(sprintf("Number of Keywords: %d",  length(data_tabel[,"Keywords"])     ))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #########################
  
  
  
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #######################List of Journal#########################
  #######################List of Journal#########################
  #######################List of Journal#########################
  #######################List of Journal#########################
  
  
  
  
  
  output$tampilkan_data_list_jurnal <- renderUI({
    
    shinycssloaders::withSpinner(DT::DTOutput(session$ns("tampilkan_data_list_jurnal_basisdata")))
    
  })
  
  
  
  
  
  
  
  
  
  output$tampilkan_data_list_jurnal_basisdata <- DT::renderDT({
    
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    data_paper <- dat
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
    
    
    grup <- group_by(data_paper, `Nama Jurnal`, ISSN)
    
    data_jurnal <- grup %>% summarise(
      freq = n()
      
    )
    
    data_jurnal <- as.data.frame(data_jurnal)
    
    colnames(data_jurnal) = c("Journal", "ISSN", "Number of Articles in Our Database")
    
print(data_jurnal)
    
    
    
    
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
      
      
      
      jumlah_jurnal <- length(  levels(     as.factor(    dat[,"Name of Journal"]       )         )        )
      jumlah_artikel <- length(   dat[,1]     )
      
      
      #cat(sprintf("Number of Journal in Our Database: %d\n\n", jumlah_jurnal))
      #cat(sprintf("Number of Article in Our Database: %d\n\n", jumlah_artikel))
      
      
      #cat(sprintf("The Journal That You Choose: \n\n"))
      
      
      
      ################Terpilih dari jurnal matematika##################
      ################Terpilih dari jurnal matematika##################
      ################Terpilih dari jurnal matematika##################
      ################Terpilih dari jurnal matematika##################
      
      jurnal_terpilih <- vector(mode = "character")
      
      if( length(input$terpilih_fungsi_nama_jurnal_matematika) != 0 )
      {
        
        jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_matematika)
      }
      
      
      
      
      if( length(input$terpilih_fungsi_nama_jurnal_kesehatan) != 0 )
      {
        
        jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_kesehatan)
      }
      
      
      if( length(input$terpilih_fungsi_nama_jurnal_psikologi) != 0 )
      {
        
        jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_psikologi)
      }
      
      
      
      
      if( length(input$terpilih_fungsi_nama_jurnal_ekonomi) != 0 )
      {
        
        jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_ekonomi)
      }
      
      
      
      if( length(input$terpilih_fungsi_nama_jurnal_agama_dan_hukum) != 0 )
      {
        
        jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_agama_dan_hukum)
      }
      
      
      
      
      
      if( length(input$terpilih_fungsi_nama_jurnal_Science_and_Engineering) != 0 )
      {
        
        jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_Science_and_Engineering)
      }
      
      
      
      
      # terpilih_artikel_di_jurnal_matematika <- input$terpilih_fungsi_nama_jurnal_matematika
      
      
      #terpilih_artikel_di_jurnal_kesehatan <- input$terpilih_fungsi_nama_jurnal_kesehatan
      #terpilih_artikel_di_jurnal_psikologi <-  input$terpilih_fungsi_nama_jurnal_psikologi
      #terpilih_artikel_di_jurnal_ekonomi <- input$terpilih_fungsi_nama_jurnal_ekonomi
      #terpilih_artikel_di_jurnal_agama_dan_kesehatan <-  input$terpilih_fungsi_nama_jurnal_agama_dan_hukum
      
      
      
      
      
      #print(jurnal_terpilih)
      
      
      
      simpan_nama_jurnal_terpilih <- ""
      
      
      for(i in 1 : length(jurnal_terpilih))
      {
        a <-  strsplit(jurnal_terpilih[i], "--")
        
        simpan_nama_jurnal_terpilih[i] <- a[[1]][1]
      }
      
      
      
      #print(simpan_nama_jurnal_terpilih)
      
      indeks_jurnal_terpilih <- dat[,"Name of Journal"] %in% simpan_nama_jurnal_terpilih 
      indeks_jurnal_terpilih <- which(indeks_jurnal_terpilih == TRUE)
      
      
      jumlah_jurnal_terpilih <- length(indeks_jurnal_terpilih)
      
      #cat(sprintf("With Number of Article: %d", jumlah_jurnal_terpilih))
      
      
      
      
      
      
      
      
      #######################
      #######################
      
      
      
      cat(sprintf("Number of Journal That You Choose: %d Journal\n\n", length(jurnal_terpilih)))
      
      
      cat(sprintf("Number of Article: %d\n\n", jumlah_jurnal_terpilih))
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      #######Dengan jumlah keywords
      
      dat <- dat[c(indeks_jurnal_terpilih),]
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
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
    
    
    
    jumlah_jurnal <- length(  levels(     as.factor(    dat[,"Name of Journal"]       )         )        )
    jumlah_artikel <- length(   dat[,1]     )
    
    
    #cat(sprintf("Number of Journal in Our Database: %d\n\n", jumlah_jurnal))
    #cat(sprintf("Number of Article in Our Database: %d\n\n", jumlah_artikel))
    
    
    #cat(sprintf("The Journal That You Choose: \n\n"))
    
    
    
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    
    jurnal_terpilih <- vector(mode = "character")
    
    if( length(input$terpilih_fungsi_nama_jurnal_matematika) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_matematika)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_kesehatan) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_kesehatan)
    }
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_psikologi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_psikologi)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_ekonomi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_ekonomi)
    }
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_agama_dan_hukum) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_agama_dan_hukum)
    }
    
    
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_Science_and_Engineering) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_Science_and_Engineering)
    }
    
    
    
    
    # terpilih_artikel_di_jurnal_matematika <- input$terpilih_fungsi_nama_jurnal_matematika
    
    
    #terpilih_artikel_di_jurnal_kesehatan <- input$terpilih_fungsi_nama_jurnal_kesehatan
    #terpilih_artikel_di_jurnal_psikologi <-  input$terpilih_fungsi_nama_jurnal_psikologi
    #terpilih_artikel_di_jurnal_ekonomi <- input$terpilih_fungsi_nama_jurnal_ekonomi
    #terpilih_artikel_di_jurnal_agama_dan_kesehatan <-  input$terpilih_fungsi_nama_jurnal_agama_dan_hukum
    
    
    
    
    
    #print(jurnal_terpilih)
    
    
    
    simpan_nama_jurnal_terpilih <- ""
    
    
    for(i in 1 : length(jurnal_terpilih))
    {
      a <-  strsplit(jurnal_terpilih[i], "--")
      
      simpan_nama_jurnal_terpilih[i] <- a[[1]][1]
    }
    
    
    
    #print(simpan_nama_jurnal_terpilih)
    
    indeks_jurnal_terpilih <- dat[,"Name of Journal"] %in% simpan_nama_jurnal_terpilih 
    indeks_jurnal_terpilih <- which(indeks_jurnal_terpilih == TRUE)
    
    
    jumlah_jurnal_terpilih <- length(indeks_jurnal_terpilih)
    
    #cat(sprintf("With Number of Article: %d", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    #######################
    #######################
    
    
    
    cat(sprintf("Number of Journal That You Choose: %d Journal\n\n", length(jurnal_terpilih)))
    
    
    cat(sprintf("Number of Article: %d\n\n", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #######Dengan jumlah keywords
    
    dat <- dat[c(indeks_jurnal_terpilih),]
    
    
    
    
    
    
    
    
    
    
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
    
    
    
    jumlah_jurnal <- length(  levels(     as.factor(    dat[,"Name of Journal"]       )         )        )
    jumlah_artikel <- length(   dat[,1]     )
    
    
    #cat(sprintf("Number of Journal in Our Database: %d\n\n", jumlah_jurnal))
    #cat(sprintf("Number of Article in Our Database: %d\n\n", jumlah_artikel))
    
    
    #cat(sprintf("The Journal That You Choose: \n\n"))
    
    
    
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    
    jurnal_terpilih <- vector(mode = "character")
    
    if( length(input$terpilih_fungsi_nama_jurnal_matematika) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_matematika)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_kesehatan) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_kesehatan)
    }
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_psikologi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_psikologi)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_ekonomi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_ekonomi)
    }
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_agama_dan_hukum) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_agama_dan_hukum)
    }
    
    
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_Science_and_Engineering) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_Science_and_Engineering)
    }
    
    
    
    # terpilih_artikel_di_jurnal_matematika <- input$terpilih_fungsi_nama_jurnal_matematika
    
    
    #terpilih_artikel_di_jurnal_kesehatan <- input$terpilih_fungsi_nama_jurnal_kesehatan
    #terpilih_artikel_di_jurnal_psikologi <-  input$terpilih_fungsi_nama_jurnal_psikologi
    #terpilih_artikel_di_jurnal_ekonomi <- input$terpilih_fungsi_nama_jurnal_ekonomi
    #terpilih_artikel_di_jurnal_agama_dan_kesehatan <-  input$terpilih_fungsi_nama_jurnal_agama_dan_hukum
    
    
    
    
    
    #print(jurnal_terpilih)
    
    
    
    simpan_nama_jurnal_terpilih <- ""
    
    
    for(i in 1 : length(jurnal_terpilih))
    {
      a <-  strsplit(jurnal_terpilih[i], "--")
      
      simpan_nama_jurnal_terpilih[i] <- a[[1]][1]
    }
    
    
    
    #print(simpan_nama_jurnal_terpilih)
    
    indeks_jurnal_terpilih <- dat[,"Name of Journal"] %in% simpan_nama_jurnal_terpilih 
    indeks_jurnal_terpilih <- which(indeks_jurnal_terpilih == TRUE)
    
    
    jumlah_jurnal_terpilih <- length(indeks_jurnal_terpilih)
    
    #cat(sprintf("With Number of Article: %d", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    #######################
    #######################
    
    
    
    cat(sprintf("Number of Journal That You Choose: %d Journal\n\n", length(jurnal_terpilih)))
    
    
    cat(sprintf("Number of Article: %d\n\n", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #######Dengan jumlah keywords
    
    dat <- dat[c(indeks_jurnal_terpilih),]
    
    
    
    
    
    
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
    
    
    
    jumlah_jurnal <- length(  levels(     as.factor(    dat[,"Name of Journal"]       )         )        )
    jumlah_artikel <- length(   dat[,1]     )
    
    
    #cat(sprintf("Number of Journal in Our Database: %d\n\n", jumlah_jurnal))
    #cat(sprintf("Number of Article in Our Database: %d\n\n", jumlah_artikel))
    
    
    #cat(sprintf("The Journal That You Choose: \n\n"))
    
    
    
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    
    jurnal_terpilih <- vector(mode = "character")
    
    if( length(input$terpilih_fungsi_nama_jurnal_matematika) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_matematika)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_kesehatan) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_kesehatan)
    }
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_psikologi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_psikologi)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_ekonomi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_ekonomi)
    }
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_agama_dan_hukum) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_agama_dan_hukum)
    }
    
    
    
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_Science_and_Engineering) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_Science_and_Engineering)
    }
    
    
    
    # terpilih_artikel_di_jurnal_matematika <- input$terpilih_fungsi_nama_jurnal_matematika
    
    
    #terpilih_artikel_di_jurnal_kesehatan <- input$terpilih_fungsi_nama_jurnal_kesehatan
    #terpilih_artikel_di_jurnal_psikologi <-  input$terpilih_fungsi_nama_jurnal_psikologi
    #terpilih_artikel_di_jurnal_ekonomi <- input$terpilih_fungsi_nama_jurnal_ekonomi
    #terpilih_artikel_di_jurnal_agama_dan_kesehatan <-  input$terpilih_fungsi_nama_jurnal_agama_dan_hukum
    
    
    
    
    
    #print(jurnal_terpilih)
    
    
    
    simpan_nama_jurnal_terpilih <- ""
    
    
    for(i in 1 : length(jurnal_terpilih))
    {
      a <-  strsplit(jurnal_terpilih[i], "--")
      
      simpan_nama_jurnal_terpilih[i] <- a[[1]][1]
    }
    
    
    
    #print(simpan_nama_jurnal_terpilih)
    
    indeks_jurnal_terpilih <- dat[,"Name of Journal"] %in% simpan_nama_jurnal_terpilih 
    indeks_jurnal_terpilih <- which(indeks_jurnal_terpilih == TRUE)
    
    
    jumlah_jurnal_terpilih <- length(indeks_jurnal_terpilih)
    
    #cat(sprintf("With Number of Article: %d", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    #######################
    #######################
    
    
    
    cat(sprintf("Number of Journal That You Choose: %d Journal\n\n", length(jurnal_terpilih)))
    
    
    cat(sprintf("Number of Article: %d\n\n", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #######Dengan jumlah keywords
    
    dat <- dat[c(indeks_jurnal_terpilih),]
    
    
    
    
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
    
    
    
    jumlah_jurnal <- length(  levels(     as.factor(    dat[,"Name of Journal"]       )         )        )
    jumlah_artikel <- length(   dat[,1]     )
    
    
    #cat(sprintf("Number of Journal in Our Database: %d\n\n", jumlah_jurnal))
    #cat(sprintf("Number of Article in Our Database: %d\n\n", jumlah_artikel))
    
    
    #cat(sprintf("The Journal That You Choose: \n\n"))
    
    
    
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    
    jurnal_terpilih <- vector(mode = "character")
    
    if( length(input$terpilih_fungsi_nama_jurnal_matematika) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_matematika)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_kesehatan) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_kesehatan)
    }
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_psikologi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_psikologi)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_ekonomi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_ekonomi)
    }
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_agama_dan_hukum) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_agama_dan_hukum)
    }
    
    
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_Science_and_Engineering) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_Science_and_Engineering)
    }
    
    
    
    # terpilih_artikel_di_jurnal_matematika <- input$terpilih_fungsi_nama_jurnal_matematika
    
    
    #terpilih_artikel_di_jurnal_kesehatan <- input$terpilih_fungsi_nama_jurnal_kesehatan
    #terpilih_artikel_di_jurnal_psikologi <-  input$terpilih_fungsi_nama_jurnal_psikologi
    #terpilih_artikel_di_jurnal_ekonomi <- input$terpilih_fungsi_nama_jurnal_ekonomi
    #terpilih_artikel_di_jurnal_agama_dan_kesehatan <-  input$terpilih_fungsi_nama_jurnal_agama_dan_hukum
    
    
    
    
    
    #print(jurnal_terpilih)
    
    
    
    simpan_nama_jurnal_terpilih <- ""
    
    
    for(i in 1 : length(jurnal_terpilih))
    {
      a <-  strsplit(jurnal_terpilih[i], "--")
      
      simpan_nama_jurnal_terpilih[i] <- a[[1]][1]
    }
    
    
    
    #print(simpan_nama_jurnal_terpilih)
    
    indeks_jurnal_terpilih <- dat[,"Name of Journal"] %in% simpan_nama_jurnal_terpilih 
    indeks_jurnal_terpilih <- which(indeks_jurnal_terpilih == TRUE)
    
    
    jumlah_jurnal_terpilih <- length(indeks_jurnal_terpilih)
    
    #cat(sprintf("With Number of Article: %d", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    #######################
    #######################
    
    
    
    cat(sprintf("Number of Journal That You Choose: %d Journal\n\n", length(jurnal_terpilih)))
    
    
    cat(sprintf("Number of Article: %d\n\n", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #######Dengan jumlah keywords
    
    dat <- dat[c(indeks_jurnal_terpilih),]
    
    
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
    
    
    
    jumlah_jurnal <- length(  levels(     as.factor(    dat[,"Name of Journal"]       )         )        )
    jumlah_artikel <- length(   dat[,1]     )
    
    
    #cat(sprintf("Number of Journal in Our Database: %d\n\n", jumlah_jurnal))
    #cat(sprintf("Number of Article in Our Database: %d\n\n", jumlah_artikel))
    
    
    #cat(sprintf("The Journal That You Choose: \n\n"))
    
    
    
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    
    jurnal_terpilih <- vector(mode = "character")
    
    if( length(input$terpilih_fungsi_nama_jurnal_matematika) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_matematika)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_kesehatan) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_kesehatan)
    }
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_psikologi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_psikologi)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_ekonomi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_ekonomi)
    }
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_agama_dan_hukum) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_agama_dan_hukum)
    }
    
    
    
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_Science_and_Engineering) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_Science_and_Engineering)
    }
    
    
    
    # terpilih_artikel_di_jurnal_matematika <- input$terpilih_fungsi_nama_jurnal_matematika
    
    
    #terpilih_artikel_di_jurnal_kesehatan <- input$terpilih_fungsi_nama_jurnal_kesehatan
    #terpilih_artikel_di_jurnal_psikologi <-  input$terpilih_fungsi_nama_jurnal_psikologi
    #terpilih_artikel_di_jurnal_ekonomi <- input$terpilih_fungsi_nama_jurnal_ekonomi
    #terpilih_artikel_di_jurnal_agama_dan_kesehatan <-  input$terpilih_fungsi_nama_jurnal_agama_dan_hukum
    
    
    
    
    
    #print(jurnal_terpilih)
    
    
    
    simpan_nama_jurnal_terpilih <- ""
    
    
    for(i in 1 : length(jurnal_terpilih))
    {
      a <-  strsplit(jurnal_terpilih[i], "--")
      
      simpan_nama_jurnal_terpilih[i] <- a[[1]][1]
    }
    
    
    
    #print(simpan_nama_jurnal_terpilih)
    
    indeks_jurnal_terpilih <- dat[,"Name of Journal"] %in% simpan_nama_jurnal_terpilih 
    indeks_jurnal_terpilih <- which(indeks_jurnal_terpilih == TRUE)
    
    
    jumlah_jurnal_terpilih <- length(indeks_jurnal_terpilih)
    
    #cat(sprintf("With Number of Article: %d", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    #######################
    #######################
    
    
    
    cat(sprintf("Number of Journal That You Choose: %d Journal\n\n", length(jurnal_terpilih)))
    
    
    cat(sprintf("Number of Article: %d\n\n", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #######Dengan jumlah keywords
    
    dat <- dat[c(indeks_jurnal_terpilih),]
    
    
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################Grafik Pemetaan Kata Kunci: 5 Oktober 2025
  ##################Grafik Pemetaan Kata Kunci: 5 Oktober 2025
  ##################Grafik Pemetaan Kata Kunci: 5 Oktober 2025
  ##################Grafik Pemetaan Kata Kunci: 5 Oktober 2025
  ##################Grafik Pemetaan Kata Kunci: 5 Oktober 2025
  
  
  
  
  ###########Grafik Pemetaan Kata Kunci Keseluruhan##############
  
  
  
  
  output$grafik_pemetaan_kata_kunci_keseluruhan<- renderPlot({
    
    
    
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
    
    
    jumlah_jurnal <- length(  levels(     as.factor(    dat[,"Name of Journal"]       )         )        )
    jumlah_artikel <- length(   dat[,1]     )
    
    
    #cat(sprintf("Number of Journal in Our Database: %d\n\n", jumlah_jurnal))
    #cat(sprintf("Number of Article in Our Database: %d\n\n", jumlah_artikel))
    
    
    #cat(sprintf("The Journal That You Choose: \n\n"))
    
    
    
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    
    jurnal_terpilih <- vector(mode = "character")
    
    if( length(input$terpilih_fungsi_nama_jurnal_matematika) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_matematika)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_kesehatan) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_kesehatan)
    }
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_psikologi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_psikologi)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_ekonomi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_ekonomi)
    }
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_agama_dan_hukum) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_agama_dan_hukum)
    }
    
    
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_Science_and_Engineering) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_Science_and_Engineering)
    }
    
    
    
    # terpilih_artikel_di_jurnal_matematika <- input$terpilih_fungsi_nama_jurnal_matematika
    
    
    #terpilih_artikel_di_jurnal_kesehatan <- input$terpilih_fungsi_nama_jurnal_kesehatan
    #terpilih_artikel_di_jurnal_psikologi <-  input$terpilih_fungsi_nama_jurnal_psikologi
    #terpilih_artikel_di_jurnal_ekonomi <- input$terpilih_fungsi_nama_jurnal_ekonomi
    #terpilih_artikel_di_jurnal_agama_dan_kesehatan <-  input$terpilih_fungsi_nama_jurnal_agama_dan_hukum
    
    
    
    
    
    #print(jurnal_terpilih)
    
    
    
    simpan_nama_jurnal_terpilih <- ""
    
    
    for(i in 1 : length(jurnal_terpilih))
    {
      a <-  strsplit(jurnal_terpilih[i], "--")
      
      simpan_nama_jurnal_terpilih[i] <- a[[1]][1]
    }
    
    
    
    #print(simpan_nama_jurnal_terpilih)
    
    indeks_jurnal_terpilih <- dat[,"Name of Journal"] %in% simpan_nama_jurnal_terpilih 
    indeks_jurnal_terpilih <- which(indeks_jurnal_terpilih == TRUE)
    
    
    jumlah_jurnal_terpilih <- length(indeks_jurnal_terpilih)
    
    #cat(sprintf("With Number of Article: %d", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    #######################
    #######################
    
    
    
    cat(sprintf("Number of Journal That You Choose: %d Journal\n\n", length(jurnal_terpilih)))
    
    
    cat(sprintf("Number of Article: %d\n\n", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #######Dengan jumlah keywords
    
    dat <- dat[c(indeks_jurnal_terpilih),]
    
    
    
    ambil_kata_kunci <- dat[c("Keywords")]
    
    
    
    for(i in 1 : length(ambil_kata_kunci[,1]))
    {
      
      x <- ambil_kata_kunci[i,1]
      
      simpan_keyword_hapus_spasi <- gsub(" ", "", x)
      simpan_keyword_hapus_spasi <- gsub("-", "", simpan_keyword_hapus_spasi)
      simpan_keyword_hapus_spasi <- gsub("'", "", simpan_keyword_hapus_spasi)
      
      ambil_kata_kunci[i,1] = simpan_keyword_hapus_spasi
      ambil_kata_kunci[i,1] = tolower(ambil_kata_kunci[i,1])
      
      
      
    }
    
    
    data_keywords_terpilih_new <- ambil_kata_kunci
    
    
    nasa_judol <- data_frame(id = c(1 :  length(data_keywords_terpilih_new[,1])  ), 
                             keyword = data_keywords_terpilih_new$Keywords ) %>% unnest(keyword)
    
    nasa_judol <-  nasa_judol %>%  unnest_tokens(word, keyword) %>%  anti_join(stop_words)
    
    title_word_pairs <- nasa_judol %>%
      pairwise_count(word, id, sort = TRUE, upper = FALSE)
    
    
    gambar = 0    
    
    # print(title_word_pairs)
    
    
    nasa_judol <- data_frame(id = c(1 :  length(data_keywords_terpilih_new[,1])  ), 
                             keyword = data_keywords_terpilih_new$Keywords ) %>% unnest(keyword)
    
    nasa_judol <-  nasa_judol %>%  unnest_tokens(word, keyword) %>%  anti_join(stop_words)
    
    title_word_pairs <- nasa_judol %>%
      pairwise_count(word, id, sort = TRUE, upper = FALSE)
    
    
    gambar = 0    
    
    # print(title_word_pairs)
    
    cooccur <-  title_word_pairs
    
    
    wordnetwork <- head(cooccur, input$grafik_coocur_jumlah_coocur_keseluruhan_ya)
    gm1 <- graph_from_data_frame(wordnetwork)
    gambar1 <- ggraph(gm1, layout = 'kk') + 
      geom_edge_density(aes(fill = n)) + 
      geom_edge_link(alpha = 0.7, color = "#57d3e6") +
      geom_node_point(aes(size = degree(gm1)), colour = "#a83268") +
      geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks_keseluruhan_ya, repel=TRUE) +
      theme(legend.position="none")
    
    
    
    gambar2 <- ggraph(gm1, layout = 'kk') + 
      geom_edge_density(aes(fill = n)) + 
      geom_edge_link(alpha = 0.7, color = "#57d3e6") +
      geom_node_point(aes(size = degree(gm1)), colour = "#a83268") +
      geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks_keseluruhan_ya, repel=TRUE) + theme_classic() +
      theme(legend.position="none")
    
    
    
    
    
    
    gambar3 <- ggraph(gm1, layout = 'linear', circular = TRUE) + 
      geom_edge_arc(color = "orange", width=0.7) +
      geom_node_point(aes(size = degree(gm1)), alpha = igraph::degree(gm1), 
                      colour = "#a83268") +
      geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks_keseluruhan_ya, repel=TRUE) +
      theme_classic() +
      theme(legend.position="none")
    
    
    
    
    gambar4 <- ggraph(gm1, layout = 'linear', circular = TRUE) + 
      geom_edge_arc(color = "#57d3e6", width=0.7) +
      geom_node_point(aes(size = degree(gm1)), alpha = igraph::degree(gm1), 
                      colour = "#a83268") +
      geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks_keseluruhan_ya, repel=TRUE) +
      theme_classic() +
      theme(legend.position="none")
    
    
    
    
    
    wordnetwork2 <- head(cooccur, input$grafik_coocur_jumlah_coocur_keseluruhan_ya)
    wordnetwork2 <- graph_from_data_frame(wordnetwork2)
    gambar5 <- ggraph(wordnetwork2, layout = "fr") +
      geom_edge_link(aes(width = n, edge_alpha = n), edge_colour = "#ed9de9") +
      geom_node_point(aes(size = igraph::degree(wordnetwork2)), shape = 1, color = "black") +
      geom_node_text(aes(label = name), col = "darkblue", size = input$grafik_coocur_ukuran_teks_keseluruhan_ya) +
      theme_classic() +
      theme(legend.position="none")
    
    
    
    
    # gm1 <- igraph::as_data_frame(wordnetwork)
    #gm1 <- graph_from_data_frame(wordnetwork)
    
    #print("ugiiiiiiiiii")
    #print(wordnetwork)
    
    
    if(input$grafik_coocur_tipe_grafik_keseluruhan_ya == "1")
    {
      gambar = gambar1
    }
    
    
    if(input$grafik_coocur_tipe_grafik_keseluruhan_ya == "2")
    {
      gambar = gambar2
    }
    
    
    if(input$grafik_coocur_tipe_grafik_keseluruhan_ya == "3")
    {
      gambar = gambar3
    }
    
    
    
    if(input$grafik_coocur_tipe_grafik_keseluruhan_ya == "4")
    {
      gambar = gambar4
    }
    
    
    if(input$grafik_coocur_tipe_grafik_keseluruhan_ya == "5")
    {
      gambar = gambar5
    }
    
    
    
    
    
    
    print(gambar)
    
    
  })
  
  
  
  
  
  
  
  
  
  ################
  ################
  
  
  
  output$pemetaan_kata_kunci_1 <- renderPlot({
    
    
    
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
    
    
    jumlah_jurnal <- length(  levels(     as.factor(    dat[,"Name of Journal"]       )         )        )
    jumlah_artikel <- length(   dat[,1]     )
    
    
    #cat(sprintf("Number of Journal in Our Database: %d\n\n", jumlah_jurnal))
    #cat(sprintf("Number of Article in Our Database: %d\n\n", jumlah_artikel))
    
    
    #cat(sprintf("The Journal That You Choose: \n\n"))
    
    
    
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    
    jurnal_terpilih <- vector(mode = "character")
    
    if( length(input$terpilih_fungsi_nama_jurnal_matematika) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_matematika)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_kesehatan) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_kesehatan)
    }
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_psikologi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_psikologi)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_ekonomi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_ekonomi)
    }
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_agama_dan_hukum) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_agama_dan_hukum)
    }
    
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_Science_and_Engineering) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_Science_and_Engineering)
    }
    
    
    
    
    # terpilih_artikel_di_jurnal_matematika <- input$terpilih_fungsi_nama_jurnal_matematika
    
    
    #terpilih_artikel_di_jurnal_kesehatan <- input$terpilih_fungsi_nama_jurnal_kesehatan
    #terpilih_artikel_di_jurnal_psikologi <-  input$terpilih_fungsi_nama_jurnal_psikologi
    #terpilih_artikel_di_jurnal_ekonomi <- input$terpilih_fungsi_nama_jurnal_ekonomi
    #terpilih_artikel_di_jurnal_agama_dan_kesehatan <-  input$terpilih_fungsi_nama_jurnal_agama_dan_hukum
    
    
    
    
    
    #print(jurnal_terpilih)
    
    
    
    simpan_nama_jurnal_terpilih <- ""
    
    
    for(i in 1 : length(jurnal_terpilih))
    {
      a <-  strsplit(jurnal_terpilih[i], "--")
      
      simpan_nama_jurnal_terpilih[i] <- a[[1]][1]
    }
    
    
    
    #print(simpan_nama_jurnal_terpilih)
    
    indeks_jurnal_terpilih <- dat[,"Name of Journal"] %in% simpan_nama_jurnal_terpilih 
    indeks_jurnal_terpilih <- which(indeks_jurnal_terpilih == TRUE)
    
    
    jumlah_jurnal_terpilih <- length(indeks_jurnal_terpilih)
    
    #cat(sprintf("With Number of Article: %d", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    #######################
    #######################
    
    
    
    cat(sprintf("Number of Journal That You Choose: %d Journal\n\n", length(jurnal_terpilih)))
    
    
    cat(sprintf("Number of Article: %d\n\n", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #######Dengan jumlah keywords
    
    dat <- dat[c(indeks_jurnal_terpilih),]
    
    
    
    
    
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
    
    
    
  
    
    data_keywords_terpilih <- dat[c(simpan_indeks),]
    
    
    data_keywords_terpilih_new <- data_keywords_terpilih[c("Keywords")]
    
    

    
    
    for(i in 1 : length(data_keywords_terpilih_new[,1]))
    {
      
      x <- data_keywords_terpilih_new[i,1]
      
      simpan_keyword_hapus_spasi <- gsub(" ", "", x)
      simpan_keyword_hapus_spasi <- gsub("-", "", simpan_keyword_hapus_spasi)
      simpan_keyword_hapus_spasi <- gsub("'", "", simpan_keyword_hapus_spasi)
      
      data_keywords_terpilih_new[i,1] = simpan_keyword_hapus_spasi
      
      
      
      
    }
    
    
    
    
    
    
    
    nasa_judol <- data_frame(id = c(1 :  length(data_keywords_terpilih_new[,1])  ), 
                             keyword = data_keywords_terpilih_new$Keywords ) %>% unnest(keyword)
    
    nasa_judol <-  nasa_judol %>%  unnest_tokens(word, keyword) %>%  anti_join(stop_words)
    
    title_word_pairs <- nasa_judol %>%
      pairwise_count(word, id, sort = TRUE, upper = FALSE)
    
    
   
    
    cooccur <-  title_word_pairs
    
    wordnetwork <- head(cooccur, 10)
    wordnetwork <- graph_from_data_frame(wordnetwork)
    gambar <- ggraph(wordnetwork, layout = "fr") +
      geom_edge_link(aes(width = n, edge_alpha = n), edge_colour = "#ed9de9") +
      geom_node_point(aes(size = igraph::degree(wordnetwork)), shape = 1, color = "black") +
      geom_node_text(aes(label = name), col = "darkblue", size = 3) + theme(legend.position="none")
    
    
    print(gambar)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################
  
  
  
  fungsi_kirim_grafik_pemetaan_kata_kunci1 <- function()
  {
    
    
    
    
    
    dat <- read_xlsx("data_paper.xlsx")
    dat <- as.data.frame(dat)
    
    colnames(dat) = c("Number", "Title of Article", "Author", "Number of Author", "Year", "Volume", "Issue", 
                      "Page", "Name of Journal", "Keywords", "ISSN", "Abstract", "Article's Source", "Sinta", 
                      "Scopus", "Scope", "Already Downloaded?", "Date", "Unique ID")
    
    
    
    jumlah_jurnal <- length(  levels(     as.factor(    dat[,"Name of Journal"]       )         )        )
    jumlah_artikel <- length(   dat[,1]     )
    
    
    #cat(sprintf("Number of Journal in Our Database: %d\n\n", jumlah_jurnal))
    #cat(sprintf("Number of Article in Our Database: %d\n\n", jumlah_artikel))
    
    
    #cat(sprintf("The Journal That You Choose: \n\n"))
    
    
    
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    ################Terpilih dari jurnal matematika##################
    
    jurnal_terpilih <- vector(mode = "character")
    
    if( length(input$terpilih_fungsi_nama_jurnal_matematika) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_matematika)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_kesehatan) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_kesehatan)
    }
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_psikologi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_psikologi)
    }
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_ekonomi) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_ekonomi)
    }
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_agama_dan_hukum) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_agama_dan_hukum)
    }
    
    
    
    
    
    if( length(input$terpilih_fungsi_nama_jurnal_Science_and_Engineering) != 0 )
    {
      
      jurnal_terpilih <- c(jurnal_terpilih, input$terpilih_fungsi_nama_jurnal_Science_and_Engineering)
    }
    
    
    
    
    # terpilih_artikel_di_jurnal_matematika <- input$terpilih_fungsi_nama_jurnal_matematika
    
    
    #terpilih_artikel_di_jurnal_kesehatan <- input$terpilih_fungsi_nama_jurnal_kesehatan
    #terpilih_artikel_di_jurnal_psikologi <-  input$terpilih_fungsi_nama_jurnal_psikologi
    #terpilih_artikel_di_jurnal_ekonomi <- input$terpilih_fungsi_nama_jurnal_ekonomi
    #terpilih_artikel_di_jurnal_agama_dan_kesehatan <-  input$terpilih_fungsi_nama_jurnal_agama_dan_hukum
    
    
    
    
    
    #print(jurnal_terpilih)
    
    
    
    simpan_nama_jurnal_terpilih <- ""
    
    
    for(i in 1 : length(jurnal_terpilih))
    {
      a <-  strsplit(jurnal_terpilih[i], "--")
      
      simpan_nama_jurnal_terpilih[i] <- a[[1]][1]
    }
    
    
    
    #print(simpan_nama_jurnal_terpilih)
    
    indeks_jurnal_terpilih <- dat[,"Name of Journal"] %in% simpan_nama_jurnal_terpilih 
    indeks_jurnal_terpilih <- which(indeks_jurnal_terpilih == TRUE)
    
    
    jumlah_jurnal_terpilih <- length(indeks_jurnal_terpilih)
    
    #cat(sprintf("With Number of Article: %d", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    #######################
    #######################
    
    
    
    cat(sprintf("Number of Journal That You Choose: %d Journal\n\n", length(jurnal_terpilih)))
    
    
    cat(sprintf("Number of Article: %d\n\n", jumlah_jurnal_terpilih))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #######Dengan jumlah keywords
    
    dat <- dat[c(indeks_jurnal_terpilih),]
    
    
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
    
    
    
    
    
    data_keywords_terpilih <- dat[c(simpan_indeks),]
    
    
    data_keywords_terpilih_new <- data_keywords_terpilih[c("Keywords")]
    
    
    
    
    
    for(i in 1 : length(data_keywords_terpilih_new[,1]))
    {
      
      x <- data_keywords_terpilih_new[i,1]
      
      simpan_keyword_hapus_spasi <- gsub(" ", "", x)
      simpan_keyword_hapus_spasi <- gsub("-", "", simpan_keyword_hapus_spasi)
      simpan_keyword_hapus_spasi <- gsub("'", "", simpan_keyword_hapus_spasi)
      
      data_keywords_terpilih_new[i,1] = simpan_keyword_hapus_spasi
      
      
      
      
    }
    
    
    
    
    
    nasa_judol <- data_frame(id = c(1 :  length(data_keywords_terpilih_new[,1])  ), 
                             keyword = data_keywords_terpilih_new$Keywords ) %>% unnest(keyword)
    
    nasa_judol <-  nasa_judol %>%  unnest_tokens(word, keyword) %>%  anti_join(stop_words)
    
    title_word_pairs <- nasa_judol %>%
      pairwise_count(word, id, sort = TRUE, upper = FALSE)
    

    gambar = 0    
    
   # print(title_word_pairs)
    
    cooccur <-  title_word_pairs
    

    wordnetwork <- head(cooccur, input$grafik_coocur_jumlah_coocur)
    gm1 <- graph_from_data_frame(wordnetwork)
   gambar1 <- ggraph(gm1, layout = 'kk') + 
      geom_edge_density(aes(fill = n)) + 
      geom_edge_link(alpha = 0.7, color = "#57d3e6") +
      geom_node_point(aes(size = degree(gm1)), colour = "#a83268") +
      geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks, repel=TRUE) +
      theme(legend.position="none")
    
   
   
   gambar2 <- ggraph(gm1, layout = 'kk') + 
     geom_edge_density(aes(fill = n)) + 
     geom_edge_link(alpha = 0.7, color = "#57d3e6") +
     geom_node_point(aes(size = degree(gm1)), colour = "#a83268") +
     geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks, repel=TRUE) + theme_classic() +
     theme(legend.position="none")
   
   
   
   
   
   
   gambar3 <- ggraph(gm1, layout = 'linear', circular = TRUE) + 
     geom_edge_arc(color = "orange", width=0.7) +
     geom_node_point(aes(size = degree(gm1)), alpha = igraph::degree(gm1), 
                     colour = "#a83268") +
     geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks, repel=TRUE) +
     theme_classic() +
     theme(legend.position="none")
   
   
   
   
   gambar4 <- ggraph(gm1, layout = 'linear', circular = TRUE) + 
     geom_edge_arc(color = "#57d3e6", width=0.7) +
     geom_node_point(aes(size = degree(gm1)), alpha = igraph::degree(gm1), 
                     colour = "#a83268") +
     geom_node_text(aes(label = name), size = input$grafik_coocur_ukuran_teks, repel=TRUE) +
     theme_classic() +
     theme(legend.position="none")
   
   
   
   
   
   wordnetwork2 <- head(cooccur, input$grafik_coocur_jumlah_coocur)
   wordnetwork2 <- graph_from_data_frame(wordnetwork2)
   gambar5 <- ggraph(wordnetwork2, layout = "fr") +
     geom_edge_link(aes(width = n, edge_alpha = n), edge_colour = "#ed9de9") +
     geom_node_point(aes(size = igraph::degree(wordnetwork2)), shape = 1, color = "black") +
     geom_node_text(aes(label = name), col = "darkblue", size = input$grafik_coocur_ukuran_teks) +
     theme_classic() +
     theme(legend.position="none")
   
   
   
    
   # gm1 <- igraph::as_data_frame(wordnetwork)
    #gm1 <- graph_from_data_frame(wordnetwork)
    
#print("ugiiiiiiiiii")
#print(wordnetwork)
    
    
    if(input$grafik_coocur_tipe_grafik == "1")
    {
      gambar = gambar1
    }
    
    
    if(input$grafik_coocur_tipe_grafik == "2")
    {
      gambar = gambar2
    }
    
    
    if(input$grafik_coocur_tipe_grafik == "3")
    {
      gambar = gambar3
    }
    
   
   
   if(input$grafik_coocur_tipe_grafik == "4")
   {
     gambar = gambar4
   }
   
   
   if(input$grafik_coocur_tipe_grafik == "5")
   {
     gambar = gambar5
   }
   
    
    return(gambar)
    
  }
  
  
  
  
  #################
  
  
  
  
  
  
  ##########300 x 300
  
  output$pemetaan_kata_kunci1_300_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  ##########500 x 300
  
  output$pemetaan_kata_kunci1_500_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  ##########700 x 300
  
  output$pemetaan_kata_kunci1_700_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########900 x 300
  
  output$pemetaan_kata_kunci1_900_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########1100 x 300
  
  output$pemetaan_kata_kunci1_1100_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 300
  
  output$pemetaan_kata_kunci1_1200_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########1300 x 300
  
  output$pemetaan_kata_kunci1_1300_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  ##########1400 x 300
  
  output$pemetaan_kata_kunci1_1400_300_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########300 x 500
  
  output$pemetaan_kata_kunci1_300_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  ##########500 x 500
  
  output$pemetaan_kata_kunci1_500_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 500
  
  output$pemetaan_kata_kunci1_700_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  ##########900 x 500
  
  output$pemetaan_kata_kunci1_900_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  ##########1100 x 500
  
  output$pemetaan_kata_kunci1_1100_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  ##########1200 x 500
  
  output$pemetaan_kata_kunci1_1200_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  ##########1300 x 500
  
  output$pemetaan_kata_kunci1_1300_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  ##########1400 x 500
  
  output$pemetaan_kata_kunci1_1400_500_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########300 x 700
  
  output$pemetaan_kata_kunci1_300_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  ##########500 x 700
  
  output$pemetaan_kata_kunci1_500_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 700
  
  output$pemetaan_kata_kunci1_700_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  ##########900 x 700
  
  output$pemetaan_kata_kunci1_900_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########1100 x 700
  
  output$pemetaan_kata_kunci1_1100_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########1200 x 700
  
  output$pemetaan_kata_kunci1_1200_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########1300 x 700
  
  output$pemetaan_kata_kunci1_1300_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  ##########1400 x 700
  
  output$pemetaan_kata_kunci1_1400_700_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  ##########300 x 900
  
  output$pemetaan_kata_kunci1_300_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  ##########500 x 900
  
  output$pemetaan_kata_kunci1_500_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  ##########700 x 900
  
  output$pemetaan_kata_kunci1_700_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  ##########900 x 900
  
  output$pemetaan_kata_kunci1_900_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  ##########1100 x 900
  
  output$pemetaan_kata_kunci1_1100_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########1200 x 900
  
  output$pemetaan_kata_kunci1_1200_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  
  
  ##########1300 x 900
  
  output$pemetaan_kata_kunci1_1300_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
    print(p)
    
    
  })
  
  
  
  
  
  
  
  ##########1400 x 900
  
  output$pemetaan_kata_kunci1_1400_900_type1 <- renderPlot({
    
    p <- fungsi_kirim_grafik_pemetaan_kata_kunci1()
    
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














