library(shiny)

shinyUI(navbarPage("Sentiment Analysis",
                   tabPanel("Deskripsi",
                            titlePanel("Sentiment Analysis"),
                            (
                              navlistPanel(
                                tabPanel(verbatimTextOutput("notif")),
                                tabPanel("Start",
                                         textInput("text", " Topik apa yang ingin anda analisa ? "),
                                         numericInput("n", " Seberapa Banyak tweet yang ingin anda analisa ? "
                                                      , 100, min = 50, max = 2000),
                                         dateRangeInput('dateRange',
                                                        label = 'Pilih rentang waktu untuk tweet yang akan di analisa',
                                                        start = Sys.Date() - 5, end = Sys.Date() 
                                         ),
                                         actionButton("mulai","Mulai")
                                ),
                                tabPanel("Pengertian & Tujuan Sentiment Analysis",
                                         strong("Apakah pengertian dari istilah Sentiment Analysis ?"),
                                         p("Sentiment analysis atau opinion mining merupakan proses memahami, 
                                           mengekstrak dan mengolah data tekstual secara otomatis untuk mendapatkan informasi sentiment 
                                           yang terkandung dalam suatu kalimat opini. Analisis sentimen dilakukan untuk melihat pendapat atau 
                                           kecenderungan opini terhadap sebuah masalah atau objek oleh seseorang, apakah cenderung berpandangan 
                                           atau beropini negatif atau positif (Bo Pang, 2002). "),
                                         
                                         
                                         p("Jadi aplikasi Sentiment Analysis ini dibangun untuk mengumpulkan dan mengolah opini masyarakat dalam bentuk tweet 
                                           yang terdapat dalam media sosial Twitter agar menjadi lebih bermanfaat, dengan keluaran akhir yaitu 
                                           mengidentifikasi dan mengkategorisasi opini pendapat dari tweet berbahasa indonesia terhadap suatu topik 
                                           tertentu dengan ukuran positif, negatif atau netral.")
                                         ),
                                tabPanel("Manfaat",
                                         strong("Manfaat dari Aplikasi"),
                                         p("1.	Meluaskan sudut pandang dalam mengidentifikasi dan mengklarifikasi stereotip anggapan opini 
                                           masyarakat dari media sosial terhadap suatu hal."),
                                         p("2.	Dapat membantu perusahaan/instansi dalam pengambilan keputusan strategi marketing pasar 
                                           dengan melakukan evaluasi berdasarkan penilaian opini masyarakat dari media sosial terhadap produk 
                                           perusahaan itu sendiri."),
                                         p("3.	Dapat mengukur tingkat kepuasan masyarakat terhadap suatu hal dengan akurat, cepat dan 
                                           ekonomis karena sumber data berasal langsung dari opini masyarakat yang di salurkan 
                                           melalui media sosial.")
                                         
                                         )
                               
                                )
                              
                                )
                            
                                ),

                   
                               tabPanel("Hasil",
                                        titlePanel("Hasil Sentiment Analysis"),
                                        (
                                          navlistPanel(
                                            tabPanel(verbatimTextOutput("info_user")),
                                            tabPanel("Raw Tweet",
                                                     tableOutput('Raw')),                                  
                                            tabPanel("Sumber Tweet",
                                                     plotOutput('sumbertweetplot')),
                                            tabPanel("Sumber Tweet (Table)",
                                                     tableOutput('sumbertweetplot_tbl')),
                                            
                                            tabPanel("============================================="),
                                            
                                            tabPanel("Tambahkan Referensi Kata (Bag of words Lexicon) ",
                                                     textInput("TambahKataPos", " Kata Positif apa yang ingin anda tambahkan ? ", value=""),
                                                     actionButton("tambahkatapositif","Tambahkan"),
                                                    
                                                     tags$hr(),
                                                     
                                                     textInput("TambahKataNeg", " Kata Negatif apa yang ingin anda tambahkan ? ", value=""),
                                                     actionButton("tambahkatanegatif","Tambahkan")
                                                     
                                                     ),
                                            
                                            tabPanel("Tambahkan Referensi Kata (Bag of words Naive Bayes) ",
                                                     tabsetPanel(type = "tabs", 
                                                     tabPanel("Kalimat Positif (Bag of words Naive Bayes)",  
                                                              tags$br(),
                                                              textInput("TambahKataPosNB", " Kata Positif apa yang ingin anda tambahkan ? ", value=""),
                                                              
                                                              selectizeInput('SubjekPos', 'Subjek', choices = list(
                                                                'strongsubj','weaksubj')
                                                              ),
                                                              
                                                              actionButton("tambahkatapositifNB","Tambahkan")
                            
                                                              ),
                                                     tabPanel("Kalimat Negatif (Bag of words Naive Bayes)", 
                                                              tags$br(),
                                                              textInput("TambahKataNegNB", " Kata Negatif apa yang ingin anda tambahkan ? ", value=""),
                                                              
                                                              selectizeInput('SubjekNeg', 'Subjek', choices = list(
                                                                'strongsubj','weaksubj')
                                                              ),
                                                              
                                                              actionButton("tambahkatanegatifNB","Tambahkan")
                                                              
                                                     )   
                                                     )
                                            ),
                                                    
                                                     
                                            tabPanel("============================================="),
                                            
                                            
                                            tabPanel("Hasil Sentiment Analysis dengan Metode Lexicon / Basic sentiment",
                                                     tableOutput('training')),
                                            
                                            tabPanel("Grafik Histogram Sentiment Analysis dengan Metode Lexicon / Basic sentiment",
                                                     plotOutput("plot3"),
                                                     verbatimTextOutput("mean")),
                                            
                                            tabPanel("Grafik Batang Total Jumlah Kemunculan Kalimat Sifat dari Dataset (Bag of Words)",
                                                     plotOutput("plot2"),
                                                     verbatimTextOutput("munculsifat")),
                                            
                                            tabPanel("Grafik Batang Polarity Sentiment Analysis dengan Metode Lexicon / Basic Sentiment",
                                                     plotOutput("plot5"),
                                                     verbatimTextOutput("munculpol")),
                                            
                                            tabPanel("============================================="),
                                            
                                            tabPanel("Tabel Polarity & Score Tweet Sentiment Analysis dengan Metode Naive Bayes library(sentR)",
                                                    tableOutput('training_pack')),
                                            
                                            tabPanel("Tabel Emosi Tweet Sentiment Analysis dengan library(sentimenT)",
                                                     tableOutput('Tabel_Tweet_emo')),
      
                                            tabPanel("Grafik Batang Polarity Sentiment Analysis dengan Metode Naive Bayes",
                                                     plotOutput("plot1"),
                                                     verbatimTextOutput("munculpol_naive")),
                                            
                                            tabPanel("Grafik Batang Emosi Sentiment Analysis dengan library(sentimenT)",
                                                     plotOutput("plot4"))
                                            
                                            )
                                          
                                        )
                                        
                               ),
                   
                               tabPanel("Facts",
                                        titlePanel("Sentiment Analysis - Facts"),
                                        (
                                          navlistPanel(
                                            tabPanel(verbatimTextOutput("info_user_NB")),
                                            tabPanel("Akurasi Lexicon dengan Conf.Matrix",
                                                     verbatimTextOutput("AccLex")),
                                            tabPanel("Akurasi Naive Bayes dengan Conf.Matrix",
                                                     verbatimTextOutput("AccNb")),
                                            tabPanel("Tweet Positif",
                                                     tableOutput('positif_tr')),
                                            tabPanel("Tweet Negatif",
                                                     tableOutput('negatif_tr')),
                                            tabPanel("Frekuensi Kata",
                                                     tableOutput('freqterms')),
                                            tabPanel("Word Cloud",
                                                     plotOutput("wordcloud")),
                                            tabPanel("Network of Words",
                                                     plotOutput("jaringankata")),
                                            tabPanel("Topic Model",
                                                     tableOutput("TopicModel"))
      
                                          )
                                        
                                        )
                               
                               )
                            
                            )
                   
                            )
        
                              




