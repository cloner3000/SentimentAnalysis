shinyServer(function(input, output, session) {
  
  #================================================================================================#
  #================================== Library Yg di gunakan =======================================#
  #================================================================================================#
  
  library("shiny")
  library("readr")
  library("tm")
  library("wordcloud")
  library("twitteR")
  library("plyr")
  library("stringr")
  library("caret")
  library("shinyIncubator")
  library("RColorBrewer")
  library("Rstem")
  library("sentiment")
  library("ggplot2")
  library("graph") 
  library("Rgraphviz") 
  library("RTextTools")
  library("e1071")
  library("dplyr")
  library("caret")
  library("sentR")
  
  #================================================================================================#
  #================================== API Twitter Apps ============================================#
  #================================================================================================#
  
  api_key<- "nSkmvzxWHyA7tcMau4Qld2QCU"
  api_secret<- "C2GrWxC2OdgfhcUB3dkWhQUzjdbVCVZtx2FMNWpSNkUuo0ixON"
  access_token<- "1923244903-XzlNBhwgyBR9JT8dtcYvD7uZ40eGPMNJW3455J3"
  access_token_secret<- "fBE4SwSslPxovUQBAUVRWhRC4d8dGRSLZu7RG2Lba6iN8"
  setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
  
  #================================================================================================#
  #========================= Menarik dan memberishkan tweet dari karakter yg asing ================#
  #================================================================================================#
  
  #reactive awal start app
  observeEvent(input$mulai, {
    
    tweets= searchTwitter(input$text, n= input$n, lang="id",  since= format(input$dateRange[1]),  
                          until=format(input$dateRange[2]))
    
    tweetstext <- sapply(tweets, function(x) x$getText())
    
    #MEMBERSIHKAN TWEET
    cleanTweets<- function(tweet){
      
      #membersihkan html link
      tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
      
      #membersihkan retweet
      tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
      
      #membersihkan hastag
      tweet = gsub("#\\w+", " ", tweet)
      
      #membersihkan @at
      tweet = gsub("@\\w+", " ", tweet)
      
      #membersihkan tanda baca
      tweet = gsub("[[:punct:]]", " ", tweet)
      
      #membersihkan nomor
      tweet = gsub("[[:digit:]]", " ", tweet)
      
      ##membersihkan spasi dan tabs
      tweet = gsub("[ \t]{2,}", " ", tweet)
      tweet = gsub("^\\s+|\\s+$", "", tweet)

      tweet = catch.error(tweet)

    }
    
    catch.error = function(x)
    {
      #membuat missing value
      y = NA

      catch_error = tryCatch(tolower(x), error=function(e) e)

      if (!inherits(catch_error, "error"))
        y = tolower(x)

      return(y)
    }
    
    cleanTweetsAndRemoveNAs<- function(tweetstext) {
      TweetsCleaned = sapply(tweetstext, cleanTweets)
      #membershikan tweet yang NA / missing
      TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
      names(TweetsCleaned) = NULL
      #membersihkan tweet yang berulang
      #TweetsCleaned = unique(TweetsCleaned)
      TweetsCleaned = TweetsCleaned 
    }
    
    TweetsCleaned = cleanTweetsAndRemoveNAs(tweetstext)
    
    #cek,sort,unique bag of words
    
    #bag of words positif
    bowpositif<- read.table("~/Rstudio/Sentiment Analysis lib - MochamadFerdyOktaviansyah/positif_fixed.txt", quote="\"", comment.char="")
    bowpositif<-unique(bowpositif)
    bowpositif<-noquote(bowpositif)
    bowpositif<-bowpositif[order(bowpositif$V1),]
    write.table(bowpositif, 'positif_fixed.txt', sep="\n" , col.names = F, row.names = F, quote = F)
    
    #bag of words negatif
    bownegatif<- read.table("~/Rstudio/Sentiment Analysis lib - MochamadFerdyOktaviansyah/negatif_fixed.txt", quote="\"", comment.char="")
    bownegatif<-unique(bownegatif)
    bownegatif<-noquote(bownegatif)
    bownegatif<-bownegatif[order(bownegatif$V1),]
    write.table(bownegatif, 'negatif_fixed.txt', sep="\n" , col.names = F, row.names = F, quote = F)
    
    #bag of words naivebayes
    subjectivity2 <- read.csv("~/Rstudio/Sentiment Analysis lib - MochamadFerdyOktaviansyah/subjectivity2.csv", header=FALSE, sep=";")
    subjectivity2<-subjectivity2[!duplicated(subjectivity2[,c('V1')]),]
    subjectivity2 <- subjectivity2[order(subjectivity2$V1),]
    write.table(subjectivity2, file = "subjectivity2.csv",row.names=FALSE, na="",col.names=FALSE, quote = FALSE, sep=";")
    
    #================================================================================================#
    #================================== Main Function  ==============================================#
    #================================================================================================#
    
    #export txt
    write.table(TweetsCleaned, file = "TweetTarik.txt",row.names=FALSE, na="",col.names=FALSE, sep="\n")
    
    #main func
    perhitungan = function(sentences, words.positive, words.negative, words.negation,.progress='none')
    {
      require(plyr)
      require(stringr)
      
      scores = laply(sentences, function(sentence, words.positive, words.negative, words.negation) {
        
        # menghilangkan digit, tanda baca 
        sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', sentence)))
        
        sentence = tolower(sentence)
        
        # memisahkan / memotong tiap kalimat satu persatu
        word.list=str_split(sentence,'\\s+')
        
        words=unlist(word.list)
        
        # mencocokan kemunculan kalimat, positif,negatif negasi
        
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        negasi.matches = match(words, negasi.words)
        
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        negasi.matches = !is.na(negasi.matches)
        
        words_matched=words[pos.matches]
        words_matched2=words[neg.matches]
        words_matched3=words[negasi.matches]
        
        Emotion[is.na(Emotion)] = "unknown"
        
        # menghitung negasi dan skor
        
        #MENGHITUNG TOTAL POSITIF DAN NEGATIF
        
        idxnegasi <- which(words %in% c(words_matched3))
        idxnegasipertama=idxnegasi[1]
        idxnegasikedua=idxnegasi[2]
        idxnegasiketiga=idxnegasi[3]
        idxnegasikeempat=idxnegasi[4]
        
        
        # positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
        # kurangpositif=sum(str_count(positif_pertama, "TRUE"))       
        # 
        # kurangpositif[is.na(kurangpositif)]<-0
        # print(kurangpositif)
        
        #Negasi 24 Okt
        if(is.na(idxnegasi[1]&&is.na(idxnegasi[2])&&is.na(idxnegasi[3]))){
          score = sum(pos.matches)-sum(neg.matches)
          #score="tidak ada negasi" #checked
        }
        
        #Negasi jika hanya 1 kalimat negasi yg terdapat dalam satu kalimat tweet
        else if(length(idxnegasi)>0){
          
          #score="satu negasi"
          if(length(idxnegasipertama)>0&&is.na(idxnegasi[2])){
            
            if(idxnegasipertama==length(words)){
              score = sum(pos.matches) - sum(neg.matches)
              #score="negasi 1 (diakhir words)" #checked
            }
            
            else if((idxnegasipertama+1)==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              score = (sum(pos.matches)-(kurangipositif)) - (sum(neg.matches)-(kuranginegatif))
              
              #score="negasi 1 ada di hampir akhir kalimat -1" #checked
            }
            
            else if((length(words)-idxnegasipertama)>=2){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              score = (sum(pos.matches)-(kurangipositif)) - (sum(neg.matches)-(kuranginegatif))
              
              #score="negasi 1 ada di tengah kalimat" #checked
            }
            
            
            else{
              score = (sum(pos.matches)) - (sum(neg.matches))
              #score="negasi 1 else" #checked
            }
            
            
          }
          
          #score="dua negasi"
          else if(length(idxnegasikedua)>0&&is.na(idxnegasi[3])){
            
            if(idxnegasikedua==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              
              score = (sum(pos.matches)-(kurangipositif)) - (sum(neg.matches)-(kuranginegatif))
              
              #score="negasi 2 ada di akhir kalimat" #checked
            }
            
            else if((idxnegasikedua+1)==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              
              score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              #score="negasi 2 ada di hampir akhir kalimat -1" #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)==1){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              #score="negasi 2 ada tengah kalimat jeda bersebelahan"  #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              #score="negasi 2 ada tengah kalimat jeda 2" #checked
            }
            
            else{
              score = round((sum(pos.matches)) - (sum(neg.matches)))
              #score="negasi 2 else"
            }
            
            
          }
          
          #3 negasi
          else if(length(idxnegasiketiga)>0){
            
            
            #jika index ketiga di akhir kalimat
            if((idxnegasikedua-idxnegasipertama)==1&&idxnegasiketiga==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              #score="negasi 3 ada di akhir kalimat jeda bersebelahan" #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2&&idxnegasiketiga==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              #score="negasi 3 ada di akhir kalimat jeda 2" #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)==1&&(idxnegasiketiga+1)==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              score = (sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3)) - (sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              #score="negasi 3 ada di hampir akhir kalimat -1 jeda bersebelahan" #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2&&(idxnegasiketiga+1)==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              score = (sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3)) - (sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              #score="negasi 3 ada di hampir akhir kalimat -1 jeda 2" #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)==1&&(idxnegasiketiga-idxnegasikedua)==1){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              score = (sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3)) - (sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              #score="negasi 3 ada tengah kalimat idx 1 2 3 jeda bersebelahan"  #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)==1&&(idxnegasiketiga-idxnegasikedua)>=2){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              score = (sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3)) - (sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              #score="negasi 3 ada tengah kalimat idx 1 dan 2 jeda bersebelahan"  #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2&&(idxnegasiketiga-idxnegasikedua)==1){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              score = (sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3)) - (sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              #score="negasi 3 ada tengah kalimat idx 2 dan 3 jeda bersebelahan"  #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2&&(idxnegasiketiga-idxnegasikedua)>=2){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              score = (sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3)) - 
                      (sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              #score="negasi 3 ada tengah kalimat idx 1 2 3 tidak jeda bersebelahan"  #checked
            }
            
            
            else{
              
              score = sum(pos.matches)-sum(neg.matches)
              #score="else negasi 3"
              
              
            }
            
            
          }
          
        }
        
        #score = sum(pos.matches) - sum(neg.matches)
        
        return (score)
      }, pos.words, neg.words, .progress=.progress )
      
      hasil_manual = laply(sentences, function(sentence, words.positive, words.negative, words.negation) {
        
        #membersihkan digit
        sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', sentence)))
        
        #lowercase
        sentence = tolower(sentence)
        
        #memisahkan spasi
        word.list=str_split(sentence,'\\s+')
        
        words=unlist(word.list)
        
        #mendapatkan kecocokan secara boolean dari tiap kata dengan secara lexicon
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        negasi.matches = match(words, negasi.words)
        
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        negasi.matches = !is.na(negasi.matches)
        
        #tampil kalimat yg di refer dari dataset txt
        words_matched=words[pos.matches]
        words_matched2=words[neg.matches]
        words_matched3=words[negasi.matches]
        
        idxnegasi <- which(words %in% c(words_matched3))
        idxnegasipertama=idxnegasi[1]
        idxnegasikedua=idxnegasi[2]
        idxnegasiketiga=idxnegasi[3]
        idxnegasikeempat=idxnegasi[4]
        
        potong=strsplit(words_matched," ")
        print(kalimatpositif<-unlist(potong)[1:length(words_matched)])
        kalimatpositif
        #kalimatpositif=data.frame(a)
        
        if(is.na(idxnegasi[1]&&is.na(idxnegasi[2])&&is.na(idxnegasi[3]))){
          total_positif=sum(pos.matches)
          total_negatif=sum(neg.matches)
          #score="tidak ada negasi" #checked
          
          if(total_positif > total_negatif){
            hasil <- "Positif";
          }
          
          else if(total_positif < total_negatif){
            hasil <- "Negatif";
          }
          
          else if(total_positif==total_negatif){
            hasil <- "Netral";
          }
          
        }
        
        #Negasi jika hanya 1 kalimat negasi yg terdapat dalam satu kalimat tweet
        else if(length(idxnegasi)>0){
          
          #score="satu negasi"
          if(length(idxnegasipertama)>0&&is.na(idxnegasi[2])){
            
            if(idxnegasipertama==length(words)){
              
              #perhitungan sifat positif,negatif,netral
              total_positif=sum(pos.matches)
              total_negatif=sum(neg.matches)
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
            }
            
            else if((idxnegasipertama+1)==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches)-(kurangipositif))
              total_negatif=(sum(neg.matches)-(kuranginegatif))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)-(kurangipositif)) - (sum(neg.matches)-(kuranginegatif))
              
            }
            
            else if((length(words)-idxnegasipertama)>=2){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches)-(kurangipositif))
              total_negatif=(sum(neg.matches)-(kuranginegatif))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)-(kurangipositif)) - (sum(neg.matches)-(kuranginegatif))
              
            }
            
            
            else{
              
              #perhitungan sifat positif,negatif,netral
              
              total_positif=sum(pos.matches)
              total_negatif=sum(neg.matches)
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)) - (sum(neg.matches))
              
            }
            
            
          }
          
          #score="dua negasi"
          else if(length(idxnegasikedua)>0&&is.na(idxnegasi[3])){
            
            if(idxnegasikedua==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches)-(kurangipositif))
              total_negatif=(sum(neg.matches)-(kuranginegatif))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)-(kurangipositif)) - (sum(neg.matches)-(kuranginegatif))
              
            }
            
            else if((idxnegasikedua+1)==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches)-(kurangipositif+kurangipositif2))
              total_negatif=(sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
            }
            
            else if((idxnegasikedua-idxnegasipertama)==1){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches)-(kurangipositif+kurangipositif2))
              total_negatif=(sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches)-(kurangipositif+kurangipositif2))
              total_negatif=(sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
            }
            
            else{
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches))
              total_negatif=(sum(neg.matches))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = round((sum(pos.matches)) - (sum(neg.matches)))
              
            }
            
          }
          
          #3 negasi
          else if(length(idxnegasiketiga)>0){
            
            
            #jika index ketiga di akhir kalimat
            if((idxnegasikedua-idxnegasipertama)==1&&idxnegasiketiga==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches)-(kurangipositif+kurangipositif2))
              total_negatif=(sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2&&idxnegasiketiga==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches)-(kurangipositif+kurangipositif2))
              total_negatif=(sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
            }
            
            else if((idxnegasikedua-idxnegasipertama)==1&&(idxnegasiketiga+1)==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3))
              total_negatif=(sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)+kurangipositif3) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2)+kuranginegatif3)
              
              score="negasi 3 ada di hampir akhir kalimat -1 jeda bersebelahan" #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2&&(idxnegasiketiga+1)==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3))
              total_negatif=(sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)+kurangipositif3) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2)+kuranginegatif3)
              
            }
            
            else if((idxnegasikedua-idxnegasipertama)==1&&(idxnegasiketiga-idxnegasikedua)==1){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3))
              total_negatif=(sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)+kurangipositif3) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2)+kuranginegatif3)
              
            }
            
            else if((idxnegasikedua-idxnegasipertama)==1&&(idxnegasiketiga-idxnegasikedua)>=2){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3))
              total_negatif=(sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)+kurangipositif3) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2)+kuranginegatif3)
              
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2&&(idxnegasiketiga-idxnegasikedua)==1){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3))
              total_negatif=(sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)+kurangipositif3) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2)+kuranginegatif3)
              
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2&&(idxnegasiketiga-idxnegasikedua)>=2){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              #perhitungan sifat positif,negatif,netral
              total_positif=(sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3))
              total_negatif=(sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)+kurangipositif3) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2)+kuranginegatif3)
              
            }
            
            
            else{
              
              #perhitungan sifat positif,negatif,netral
              total_positif=sum(pos.matches)
              total_negatif=sum(neg.matches)
              
              if(total_positif > total_negatif){
                hasil <- "Positif";
              }
              
              else if(total_positif < total_negatif){
                hasil <- "Negatif";
              }
              
              else if(total_positif==total_negatif){
                hasil <- "Netral";
              }
              
              #score = sum(pos.matches)-sum(neg.matches)
              
              
            }
            
            
          }
          
        }
        
        
        Emotion[is.na(Emotion)] = "unknown"
        
        # Now get the score as total positive sentiment minus the total negatives
        
        
        #MENGHITUNG TOTAL POSITIF DAN NEGATIF
        
        return(hasil)
      }, pos.words, neg.words, .progress=.progress )
      
      #menjumlahkan total kalimat positif
      total_pos = laply(sentences, function(sentence, words.positive, words.negative, words.negation) {
        
        #membersihkan digit
        sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', sentence)))
        #lowercase
        sentence = tolower(sentence)
        #memisahkan spasi
        word.list=str_split(sentence,'\\s+')
        words=unlist(word.list)
        
        #mendapatkan kecocokan secara boolean dari tiap kata dengan secara lexicon
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        
        #tampil kalimat yg di refer dari dataset txt
        words_matched=words[pos.matches]
        words_matched2=words[neg.matches]
        
        f_positif=(sum(pos.matches))
        Emotion[is.na(Emotion)] = "unknown"
        
        return(f_positif)
      }, pos.words, neg.words, .progress=.progress )
      
      #menjumlahkan total kalimat negatif
      total_neg = laply(sentences, function(sentence, words.positive, words.negative, words.negation) {
        
        sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', sentence)))
        
        sentence = tolower(sentence)
        
        word.list=str_split(sentence,'\\s+')
        
        words=unlist(word.list)
        
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        
        #tampil kalimat yg di refer dari dataset txt
        words_matched=words[pos.matches]
        words_matched2=words[neg.matches]
        
        f_negatif=(sum(neg.matches))

        Emotion[is.na(Emotion)] = "unknown"
        
        return(f_negatif)
      }, pos.words, neg.words, .progress=.progress )
      
      #menampilkan kalimat positif yg muncul per tweet
      diketahui_positif = laply(sentences, function(sentence, words.positive, words.negative, words.negation) {
        
        sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', sentence)))
        
        sentence = tolower(sentence)
        
        word.list=str_split(sentence,'\\s+')
        
        words=unlist(word.list)
        
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        
        #tampil kalimat yg di refer dari dataset txt
        words_matched=words[pos.matches]
        words_matched2=words[neg.matches]
        
        #tampil kalimat yg di refer dari dataset txt
        words_matched=words[pos.matches]
        
        kumpul_positif=paste(words[pos.matches], collapse =' ')    
        
        return(kumpul_positif)
      }, pos.words, neg.words, .progress=.progress )
      
      #menampilkan kalimat negatif yg muncul per tweet
      diketahui_negatif = laply(sentences, function(sentence, words.positive, words.negative, words.negation) {
        
        sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', sentence)))
        
        sentence = tolower(sentence)
        
        word.list=str_split(sentence,'\\s+')
        
        words=unlist(word.list)
        
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        
        #tampil kalimat yg di refer dari dataset txt
        words_matched=words[pos.matches]
        words_matched2=words[neg.matches]
        
        #tampil kalimat yg di refer dari dataset txt
        words_matched=words[pos.matches]
        
        kumpul_negatif=paste(words[neg.matches], collapse =' ')    
        
        return(kumpul_negatif)
      }, pos.words, neg.words, .progress=.progress )
      
      #menampilkan kalimat negatif yg muncul per tweet
      diketahui_negasi = laply(sentences, function(sentence, words.positive, words.negative, words.negation) {
        
        sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', sentence)))
        
        sentence = tolower(sentence)
        
        word.list=str_split(sentence,'\\s+')
        
        words=unlist(word.list)
        
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        negasi.matches = match(words, negasi.words)
        
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        negasi.matches = !is.na(negasi.matches)
        
        words_matched=words[pos.matches]
        words_matched2=words[neg.matches]
        words_matched3=words[negasi.matches]
        
        #tampil kalimat yg di refer dari dataset txt
        words_matched=words[pos.matches]
        
        kumpul_negasi=paste(words[negasi.matches], collapse =' ')    
        
        return(kumpul_negasi)
      }, pos.words, neg.words, .progress=.progress )
      
      chknegasi = laply(sentences, function(sentence, words.positive, words.negative, words.negation) {
        
        # Let first remove the Digit, Punctuation character and Control characters:
        sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', sentence)))
        
        # Then lets convert all to lower sentence case:
        sentence = tolower(sentence)
        
        # Now lets split each sentence by the space delimiter
        word.list=str_split(sentence,'\\s+')
        
        words=unlist(word.list)
        
        # Get the boolean match of each words with the positive & negative opinion-lexicon
        
        pos.matches = match(words, pos.words)
        neg.matches = match(words, neg.words)
        negasi.matches = match(words, negasi.words)
        
        pos.matches = !is.na(pos.matches)
        neg.matches = !is.na(neg.matches)
        negasi.matches = !is.na(negasi.matches)
        
        words_matched=words[pos.matches]
        words_matched2=words[neg.matches]
        words_matched3=words[negasi.matches]
        
        Emotion[is.na(Emotion)] = "unknown"
        
        # Now get the score as total positive sentiment minus the total negatives
        
        #MENGHITUNG TOTAL POSITIF DAN NEGATIF
        
        
        idxnegasi <- which(words %in% c(words_matched3))
        idxnegasipertama=idxnegasi[1]
        idxnegasikedua=idxnegasi[2]
        idxnegasiketiga=idxnegasi[3]
        idxnegasikeempat=idxnegasi[4]
        
        
        # positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
        # kurangpositif=sum(str_count(positif_pertama, "TRUE"))       
        # 
        # kurangpositif[is.na(kurangpositif)]<-0
        # print(kurangpositif)
        
        #Negasi 24 Okt
        if(is.na(idxnegasi[1]&&is.na(idxnegasi[2])&&is.na(idxnegasi[3]))){
          #score = sum(pos.matches)-sum(neg.matches)
          check_negasi="tidak ada negasi" #checked
        }
        
        #Negasi jika hanya 1 kalimat negasi yg terdapat dalam satu kalimat tweet
        else if(length(idxnegasi)>0){
          
          #score="satu negasi"
          if(length(idxnegasipertama)>0&&is.na(idxnegasi[2])){
            
            if(idxnegasipertama==length(words)){
              #score = sum(pos.matches) - sum(neg.matches)
              check_negasi="negasi 1 (diakhir words)" #checked
            }
            
            else if((idxnegasipertama+1)==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              #score = (sum(pos.matches)-(kurangipositif)) - (sum(neg.matches)-(kuranginegatif))
              
              check_negasi="negasi 1 ada di hampir akhir kalimat -1" #checked
            }
            
            else if((length(words)-idxnegasipertama)>=2){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              #score = (sum(pos.matches)-(kurangipositif)) - (sum(neg.matches)-(kuranginegatif))
              
              check_negasi="negasi 1 ada di tengah kalimat" #checked
            }
            
            
            else{
              #score = (sum(pos.matches)) - (sum(neg.matches))
              check_negasi="negasi 1 else" #checked
            }
            
            
          }
          
          #score="dua negasi"
          else if(length(idxnegasikedua)>0&&is.na(idxnegasi[3])){
            
            if(idxnegasikedua==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              
              #score = (sum(pos.matches)-(kurangipositif)) - (sum(neg.matches)-(kuranginegatif))
              
              check_negasi="negasi 2 ada di akhir kalimat" #checked
            }
            
            else if((idxnegasikedua+1)==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              check_negasi="negasi 2 ada di hampir akhir kalimat -1" #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)==1){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              check_negasi="negasi 2 ada tengah kalimat jeda bersebelahan"  #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              check_negasi="negasi 2 ada tengah kalimat jeda 2" #checked
            }
            
            else{
              #score = round((sum(pos.matches)) - (sum(neg.matches)))
              check_negasi="negasi 2 else"
            }
            
            
          }
          
          #3 negasi
          else if(length(idxnegasiketiga)>0){
            
            
            #jika index ketiga di akhir kalimat
            if((idxnegasikedua-idxnegasipertama)==1&&idxnegasiketiga==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              check_negasi="negasi 3 ada di akhir kalimat jeda bersebelahan" #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2&&idxnegasiketiga==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              #score = (sum(pos.matches)-(kurangipositif+kurangipositif2)) - (sum(neg.matches)-(kuranginegatif+kuranginegatif2))
              
              check_negasi="negasi 3 ada di akhir kalimat jeda 2" #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)==1&&(idxnegasiketiga+1)==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              #score = (sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3)) - (sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              check_negasi="negasi 3 ada di hampir akhir kalimat -1 jeda bersebelahan" #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2&&(idxnegasiketiga+1)==length(words)){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              #score = (sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3)) - (sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              check_negasi="negasi 3 ada di hampir akhir kalimat -1 jeda 2" #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)==1&&(idxnegasiketiga-idxnegasikedua)==1){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+1)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              #score = (sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3)) - (sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              check_negasi="negasi 3 ada tengah kalimat idx 1 2 3 jeda bersebelahan"  #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)==1&&(idxnegasiketiga-idxnegasikedua)>=2){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+1)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+1)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              #score = (sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3)) - (sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              check_negasi="negasi 3 ada tengah kalimat idx 1 dan 2 jeda bersebelahan"  #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2&&(idxnegasiketiga-idxnegasikedua)==1){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+1)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+1)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              #score = (sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3)) - (sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              check_negasi="negasi 3 ada tengah kalimat idx 2 dan 3 jeda bersebelahan"  #checked
            }
            
            else if((idxnegasikedua-idxnegasipertama)>=2&&(idxnegasiketiga-idxnegasikedua)>=2){
              
              positif_pertama<-pos.matches[idxnegasipertama:(idxnegasipertama+2)]
              kurangipositif=sum(str_count(positif_pertama, "TRUE"))
              kurangipositif[is.na(kurangipositif)]<-0
              
              positif_kedua<-pos.matches[idxnegasikedua:(idxnegasikedua+2)]
              kurangipositif2=sum(str_count(positif_kedua, "TRUE"))
              kurangipositif2[is.na(kurangipositif2)]<-0
              
              positif_ketiga<-pos.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kurangipositif3=sum(str_count(positif_ketiga, "TRUE"))
              kurangipositif3[is.na(kurangipositif3)]<-0
              
              negatif_pertama<-neg.matches[idxnegasipertama:(idxnegasipertama+2)]
              kuranginegatif=sum(str_count(negatif_pertama, "TRUE"))
              kuranginegatif[is.na(kuranginegatif)]<-0
              
              negatif_kedua<-neg.matches[idxnegasikedua:(idxnegasikedua+2)]
              kuranginegatif2=sum(str_count(negatif_kedua, "TRUE"))
              kuranginegatif2[is.na(kuranginegatif2)]<-0
              
              negatif_ketiga<-neg.matches[idxnegasiketiga:(idxnegasiketiga+2)]
              kuranginegatif3=sum(str_count(negatif_ketiga, "TRUE"))
              kuranginegatif3[is.na(kuranginegatif3)]<-0
              
              #score = (sum(pos.matches)-((kurangipositif+kurangipositif2)+kurangipositif3)) - (sum(neg.matches)-((kuranginegatif+kuranginegatif2)+kuranginegatif3))
              
              check_negasi="negasi 3 ada tengah kalimat idx 1 2 3 tidak jeda bersebelahan"  #checked
            }
            
            
            else{
              
              #score = sum(pos.matches)-sum(neg.matches)
              check_negasi="else negasi 3"
              
              
            }
            
            
          }
          
        }
        
        #score = sum(pos.matches) - sum(neg.matches)
        
        return (check_negasi)
      }, pos.words, neg.words, .progress=.progress )
      
      return(data.frame(text=sentences, score=scores, 
                        hasil=hasil_manual, 
                        f_positif=total_pos, 
                        f_negatif= total_neg, 
                        kumpul_positif=diketahui_positif, 
                        kumpul_negatif=diketahui_negatif,
                        check_negasi=chknegasi,
                        kumpul_negasi=diketahui_negasi))
    }
    
    tabelfrekuensi = function(sentences){
      require(plyr)
      require(stringr)
      
      #================================================================================================#
      #============================ Potong Kata menjadi Tabel Freq  ===================================#
      #================================================================================================#
      
      sentences = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '', gsub('\\d+', '', sentences)))
      
      sentences = tolower(sentences)
      
      word.list=str_split(sentences,'\\s+')
      
      words=unlist(word.list)
      
      potong=strsplit(words," ")
      kalimatpotong<-unlist(potong)[1:length(words)]
      
      
      return(data.frame(kalimatpotong))
    }
    
    classify.naivebayes <- function(sentences, pstrong=0.5,
                                    pweak=1.0, prior=1.0, ...) {
      
      matrix <- classify.dtm(sentences, ...)
      
      lexicon  <- read.csv("~/Rstudio/Sentiment Analysis lib - MochamadFerdyOktaviansyah/subjectivity2.csv", header=FALSE, sep=";")
      counts <- list(positive = length(which(lexicon[,3]=="positive")), 
                     negative = length(which(lexicon[,3]=="negative")),
                     total = nrow(lexicon))
      
      documents <- c()
      
      #membuat dtm, potong2 kata
      for (i in 1:nrow(matrix)) {
        scores <- list(positive=0, negative=0)
        doc <- matrix[i,]
        words <- findFreqTerms(doc, lowfreq=1)
        
        #cek kata
        for (word in words) {
          index <- pmatch(word,lexicon[,1],nomatch=0)
        
          #mendapatkan nomor urutan keberapa di dataset naive bayes
          if (index > 0) {
            entry <- lexicon[index,]
            
            polarity <- as.character(entry[[2]])
            category <- as.character(entry[[3]])
            count <- counts[[category]]
             
            # hitung score join per kata
            score <- pweak
            if (polarity == "strongsubj") score <- pstrong
            score <- abs(log(score*prior/count))
            scores[[category]] <- scores[[category]]+score
            
          }		
        }
        
        #untuk menghitung nilai naive bayes suatu tweet
        #score base
        for (key in names(scores)) {
          count <- counts[[key]]
          total <- counts[["total"]]
          score <- abs(log(count/total))
          scores[[key]] <- scores[[key]]+score
          
        }
        
        best_fit <- names(scores)[which.max(unlist(scores))]

        ratio <- abs(scores$positive/scores$negative)
        
        if (ratio > 0.90 && ratio < 1.10)
          best_fit <- "neutral"
        
        documents <- rbind(documents,c(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit))
      }
      
      colnames(documents) <- c("POS","NEG","POS/NEG","SENT")
      return(documents)
    }
    
   
    #================================================================================================#
    #============================= Memanggil dari bag of words txt ==================================#
    #================================================================================================#
    
    #tarik bag of words
    pos = scan('positif_fixed.txt', what = 'character', comment.char = ';')
    neg = scan('negatif_fixed.txt', what = 'character', comment.char = ';')
    negasi = scan('negation_fixed.txt', what = 'character', comment.char = ';')
    
    pos.words = pos
    neg.words = neg
    negasi.words = negasi 
    
    #tambahkan kata
    observeEvent(input$tambahkatapositif, {
    
      TambahKataPosForm = NULL
      
      TambahKataPosForm=unlist(strsplit(input$TambahKataPos,'\\s+'))
      
      if(length(TambahKataPosForm > 0)){
      write(TambahKataPosForm,file="positif_fixed.txt",append=TRUE)
      updateTextInput(session, "TambahKataPos", value = "")
      }

      TambahKataPosForm = NULL
      
    })
    
    observeEvent(input$tambahkatanegatif, {
      
      TambahKataNegForm = NULL
      
      TambahKataNegForm=unlist(strsplit(input$TambahKataNeg,'\\s+'))
      
      if(length(TambahKataNegForm > 0)){
      write(TambahKataNegForm,file="negatif_fixed.txt",append=TRUE)
      updateTextInput(session, "TambahKataNeg", value = "")
      }
      
      TambahKataNegForm = NULL
      
    })
    
    observeEvent(input$tambahkatapositifNB, {
      
        subjectivity2 <- read.csv("~/Rstudio/Sentiment Analysis lib - MochamadFerdyOktaviansyah/subjectivity2.csv", header=FALSE, sep=";")

        Pos<-data.frame(input$TambahKataPosNB,input$SubjekPos,"positive")
        names(Pos)<-c("V1","V2","V3")
        subjectivity2<- rbind(subjectivity2, Pos)
        write.table(subjectivity2, file = "subjectivity2.csv",row.names=FALSE, na="",col.names=FALSE, quote = FALSE, sep=";")
        updateTextInput(session, "TambahKataPosNB", value = "")
      
    })
    
    observeEvent(input$tambahkatanegatifNB, {
      
      subjectivity2 <- read.csv("~/Rstudio/Sentiment Analysis lib - MochamadFerdyOktaviansyah/subjectivity2.csv", header=FALSE, sep=";")
      
      Neg<-data.frame(input$TambahKataNegNB,input$SubjekNeg,"negative")
      names(Neg)<-c("V1","V2","V3")
      subjectivity2<- rbind(subjectivity2, Neg)
      write.table(subjectivity2, file = "subjectivity2.csv",row.names=FALSE, na="",col.names=FALSE, quote = FALSE, sep=";")
      updateTextInput(session, "TambahKataNegNB", value = "")
      
    })
    
    #================================================================================================#
    #=========== Memanggil package library r SentimenT Naive Bayes Otomatis bag of words txt=========#
    #================================================================================================#
    #======================================== Emosi =================================================#
    #================================================================================================#
    #================================================================================================#
    
    TweetsClassEmo = classify_emotion(TweetsCleaned, algorithm="bayes", prior=1.0)
    
    Emotion = TweetsClassEmo[,7]
    
    Emotion[is.na(Emotion)] = "unknown"
    
    emotion=Emotion
    
    Tabel_Tweet_emo = data.frame(TweetsClassEmo)
    
    #================================================================================================#
    #========================================= Polarity =============================================#
    #================================================================================================#
    
    TweetsClassPol = classify.naivebayes(TweetsCleaned)
    
    Pos1=TweetsClassPol[,1] 
    Neg1=TweetsClassPol[,2] 
    Pos1_Neg1=TweetsClassPol[,3] 
    Best_fit=TweetsClassPol[,4] 
    
    SentimentDataFrame <- data.frame(Tweet=TweetsCleaned, Score_Positif=Pos1, Score_Negatif=Neg1, Polarity=Best_fit, stringsAsFactors=FALSE)
    
    exporttweet_nb=SentimentDataFrame[,c("Polarity","Tweet")]
    
    write.table(exporttweet_nb, 'HasilLexicon_nb.csv', sep="," , row.names = FALSE)
    
    #================================================================================================#
    #========================= Basic Lexicon & Naive Bayes Native ===================================#
    #================================================================================================#
    
    Result = perhitungan(TweetsCleaned, pos.words, neg.words, negasi.words)
    SentimentDataFrame_tampil = data.frame(Tweet=TweetsCleaned, 
                                           Polarity=Result$hasil, 
                                           Score=Result$score, 
                                           Positif=Result$kumpul_positif, 
                                           Negatif=Result$kumpul_negatif,
                                           Check_Negasi=Result$check_negasi,
                                           Negasi=Result$kumpul_negasi,
                                           stringsAsFactors=FALSE)
    
    #jumlah row training
    jumlah_tweet=nrow(SentimentDataFrame_tampil)
    
    #naive bayes tahap 3 dataframe positif
    positif = SentimentDataFrame_tampil[SentimentDataFrame_tampil$Polarity == "Positif",]
    positif_tr=positif[,"Tweet", drop=FALSE]
    
    #naive bayes tahap 3 dataframe negatif
    negatif = SentimentDataFrame_tampil[SentimentDataFrame_tampil$Polarity == "Negatif",]
    negatif_tr=negatif[,"Tweet", drop=FALSE]
    
    tbl_freq = tabelfrekuensi(TweetsCleaned)
    
    #print dataframe nb_tahap34 AKA freq_table
    df_tbl_freq = data.frame(Kata=tbl_freq$kalimatpotong)
    
    tweetpositif <- data.frame(positif_tr)
    tweetnegatif <- data.frame(negatif_tr)
    
    #tweetpositif
    write.table(tweetpositif, file = "TweetTarik2.txt",row.names=FALSE, na="",col.names=FALSE, sep=",")
    
    #tweetpositif
    write.table(tweetnegatif, file = "TweetTarik3.txt",row.names=FALSE, na="",col.names=FALSE, sep=",")
    
    #export hasil df SentimentDataFrame_tampil
    exporttweet=SentimentDataFrame_tampil[c("Polarity","Tweet")]

    write.table(exporttweet, 'HasilLexicon_lex.csv', sep="," , row.names = FALSE)
    
    #================================================================================================#
    #============================================ Etc ===============================================#
    #================================================================================================#
    
    tweets.df <- twListToDF(tweets)
    tweets.df[,5]<-as.character(tweets.df[,5])
    
    jumlah_tweet_raw=length(tweets.df$text)
    
    write.table(tweets.df$text, file = "TweetTarik_df.txt",row.names=FALSE, na="",col.names=FALSE, sep="\n")
  
    cols <- colorRampPalette(brewer.pal(11, "PiYG"))
    myPal <- cols(length(unique(tweets.df[,5])))
    
    w = table(tweets.df$statusSource)
    souce_tweet_table = as.data.frame(w)
    names(souce_tweet_table)[1] = 'Sumber Tweet'
    names(souce_tweet_table)[2] = 'Frekuensi'
    sort(souce_tweet_table$Frekuensi)

    sumbertweetplot_pl <- function (sentiment_dataframe,title) {
      library(ggplot2)
      ggplot(sentiment_dataframe, aes(x=statusSource)) +
        geom_bar(aes(y=..count.., fill=statusSource)) +
        scale_fill_manual(values = myPal) +
        ggtitle(title) +
        theme(legend.position='right') + ylab('Jumlah Tweet') + xlab('Sumber Tweet')
    }
    
    output$sumbertweetplot <- renderPlot({
      sumbertweetplot_pl(tweets.df, 'Sumber Tweet')
    })
    
    
    #================================================================================================#
    #============================== Word Cloud / Network of Words ===================================#
    #================================================================================================#
    
    mach_corpus = Corpus(VectorSource(TweetsCleaned))
    tdm = TermDocumentMatrix(mach_corpus,
                             control = list(removePunctuation = TRUE,
                                            removeNumbers = TRUE, 
                                            tolower = TRUE))
    
    #Frekuensi Kalimat
    word.freq = rowSums(as.matrix(tdm))
    word.freq = sort(word.freq,  word.freq >= 3, decreasing=TRUE) 
    dm = data.frame(word=names(word.freq), freq=word.freq)
    
    #titik tengah untuk temukan network freq
    tengah=round(((nrow(tdm)*50)/100)/60)
 
    
    freq.terms <- findFreqTerms(tdm, lowfreq =tengah)
    
    #================================================================================================#
    #======================================= Topic Model ============================================#
    #================================================================================================#
 
    dtm <- as.DocumentTermMatrix(tdm)
    
    #jika ada yang empty tweet maka dapat diseleksi 
    rowTotals <- apply(dtm , 1, sum)
    dtm.new   <- dtm[rowTotals> 0, ]
    
    library("topicmodels")
    lda <- LDA(dtm.new, k = 10)
    #sort topic
    term <- terms(lda, 10)
    
    #================================================================================================#
    #==================== Conf Mat Lexicon / Dengan Naive Bayes di dalamnya =========================#
    #================================================================================================#
    
    df<- read.csv("~/Rstudio/Sentiment Analysis lib - MochamadFerdyOktaviansyah/HasilLexicon_lex.csv", stringsAsFactors = FALSE)
    #library (dplyr)
    #untuk transpos
    glimpse(df)
    set.seed(1)
    df <- df[sample(nrow(df)), ]
    df <- df[sample(nrow(df)), ]
    glimpse(df)
    
    df$Polarity <- as.factor(df$Polarity)
    
    #library(tm)
    corpus <- Corpus(VectorSource(df$Tweet))
    corpus
    inspect(corpus[1:3])
    
    #membuat titik tengah pembagian data
    pembulat=(round(((nrow(df)*75)/100)))
  
    corpus.clean <- corpus %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(stripWhitespace)
    
    #memotong / memisahkan kata 
    dtm <- DocumentTermMatrix(corpus.clean)

    df.train <- df[1:pembulat,]
    df.test <- df[(pembulat+1):nrow(df),]
    
    dtm.train <- dtm[1:pembulat,]
    dtm.test <- dtm[(pembulat+1):nrow(df),]
  
    corpus.clean.train <- corpus.clean[1:pembulat]
    corpus.clean.test <- corpus.clean[(pembulat+1):nrow(df)]
    
    #cek dimensi data
    dim(dtm.train)
    
    #membuat daftar kalimat frekuensi
    fivefreq <- findFreqTerms(dtm.train, 2)
    length((fivefreq))
    
    #dictionary mereferensikan kalimat yang di cari 
    dtm.train.nb <- DocumentTermMatrix(corpus.clean.train, control=list(dictionary = fivefreq))
    
    dim(dtm.train.nb)
    
    dtm.test.nb <- DocumentTermMatrix(corpus.clean.test, control=list(dictionary = fivefreq))
    
    dim(dtm.train.nb)
    
    #labeling hasil prediksi yes or no
    #jika suatu kata muncul dalam tweet maka yes
    convert_count <- function(x) {
     y <- ifelse(x > 0, 1,0)
    y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
    y
    }

    #trainNB untuk mengumpulkan kalimat frekuensi dan menghitung score per kata
    #mencari kalimat fivefreq di df asli
    trainNB <- apply(dtm.train.nb, 2, convert_count)
    testNB <- apply(dtm.test.nb, 2, convert_count)
    
    #yes = ada kalimat tersebut, no = tidak ada kalimat tersebut dalam sebuah tweet
    #menghitung score per kata dengan cara mencari kalimat trainNB yang muncul pada dataframe
    #df.train lalu membuat perhitungan probabilitas ada tidaknya kalimat tersebut muncul di tweet lain
    #library(e1071)
    classifier <- naiveBayes(trainNB, df.train$Polarity, laplace = 1, drop.unused.level = FALSE) 
    
    #Jika error subscript out of bounds, tambahkan jumlah tweet yg di tarik
    
    #memprediksi hasil naive bayes  tweet df.test
    #dengan mereferensikan nilai dari training data
    pred <- predict(classifier, newdata=testNB) 
    
    print(pred)
    
    #confusion matrix
    table("Predictions"= pred,  "Actual" = df.test$Polarity )
    
    conf.mat <- confusionMatrix(pred, df.test$Polarity)
    
    conf.mat$byClass
   
    conf.mat$overall
    
    conf.mat$overall['Accuracy']
    
    #================================================================================================#
    #==================== Conf Mat SentR / Dengan Naive Bayes di dalamnya ===========================#
    #================================================================================================#
    
    sentR_data <- read.csv("~/Rstudio/Sentiment Analysis lib - MochamadFerdyOktaviansyah/HasilLexicon_nb.csv", stringsAsFactors = FALSE)
    glimpse(sentR_data)
    set.seed(1)
    sentR_data <- sentR_data[sample(nrow(sentR_data)), ]
    sentR_data <- sentR_data[sample(nrow(sentR_data)), ]
    glimpse(sentR_data)
    
    sentR_data$Polarity <- as.factor(sentR_data$Polarity)
    
    corpus2 <- Corpus(VectorSource(sentR_data$Tweet))
    corpus2
    inspect(corpus2[1:3])
    
    pembulat2=(round(((nrow(sentR_data)*75)/100)))
    
    corpus2.clean <- corpus2 %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(stripWhitespace)
    
    dtm2 <- DocumentTermMatrix(corpus2.clean)
    
    sentR_data.train <- sentR_data[1:pembulat2,]
    sentR_data.test <- sentR_data[(pembulat2+1):nrow(sentR_data),]
    
    dtm2.train <- dtm2[1:pembulat2,]
    dtm2.test <- dtm2[(pembulat2+1):nrow(sentR_data),]
    
    corpus2.clean.train <- corpus2.clean[1:pembulat2]
    corpus2.clean.test <- corpus2.clean[(pembulat2+1):nrow(sentR_data)]
    
    dim(dtm2.train)
    
    fivefreq2 <- findFreqTerms(dtm2.train, 2)
    length((fivefreq2))
    
    dtm2.train.nb <- DocumentTermMatrix(corpus2.clean.train, control=list(dictionary = fivefreq2))
    
    dim(dtm2.train.nb)
    
    dtm2.test.nb <- DocumentTermMatrix(corpus2.clean.test, control=list(dictionary = fivefreq2))
    
    dim(dtm2.train.nb)
    
    convert_count <- function(x) {
      y <- ifelse(x > 0, 1,0)
      y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
      y
    }
    
    trainNB2 <- apply(dtm2.train.nb, 2, convert_count)
    testNB2 <- apply(dtm2.test.nb, 2, convert_count)
    
    classifier2 <- naiveBayes(trainNB2, sentR_data.train$Polarity, laplace = 1, drop.unused.level = FALSE) 
    
    #Jika error subscript out of bounds, tambahkan jumlah tweet yg di tarik
    pred2 <- predict(classifier2, newdata=testNB2) 
    
    print(pred2)
    
    table("Predictions"= pred2,  "Actual" = sentR_data.test$Polarity )
    
    conf2.mat <- confusionMatrix(pred2, sentR_data.test$Polarity)
    
    conf2.mat$byClass
    
    conf2.mat$overall
    
    conf2.mat$overall['Accuracy']
    
    if(length(conf2.mat$overall['Accuracy']) > 0){
      notif="Proses telah selesai, silahkan menuju tabs Hasil"
    }
    else{
      notif=NULL
    }
    
    #================================================================================================#
    #================================== Printing Section ============================================#
    #================================================================================================#
    
    plotSentiments1 <- function (sentiment_dataframe,title) {
      library(ggplot2)
      ggplot(sentiment_dataframe, aes(x=Polarity)) +
        geom_bar(aes(y=..count.., fill=Polarity)) +
        scale_fill_brewer(palette="PRGn") +
        ggtitle(title) +
        theme(legend.position='right') + ylab('Jumlah Tweet') + xlab('Kategori Sifat')
    }
    
    plotSentiments2<- function (sentiment_dataframe,title) {
      library(ggplot2)
      ggplot(sentiment_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
        scale_fill_brewer(palette="Dark2") +
        ggtitle(title) +
        theme(legend.position='right') + ylab('Jumlah Tweet') + xlab('Kategori Emosi')
    }
    
    output$training <- renderTable({
      SentimentDataFrame_tampil
    })
    
    output$training_pack <- renderTable({
      SentimentDataFrame
    })
    
    output$Tabel_Tweet_pol<- renderTable({
      Tabel_Tweet_pol
    })
    
    output$Tabel_Tweet_emo<- renderTable({
      Tabel_Tweet_emo
    })
    
    output$positif_tr <- renderTable({
      positif_tr
    })
    
    output$negatif_tr<- renderTable({
      negatif_tr
    })
    
    output$info_user <- renderText({
      
      #paste satu persatu hasil inputan dari user
      inputan=paste("Topik yang akan di analisa :", input$text)
      jumlah_alokasi=paste("Jumlah Tweet yang ingin didapat :", input$n)
      tgl=paste("Rentang waktu tweet :", input$dateRange[1], "sampai",  input$dateRange[2])
      
      #jumlah kolom tweet yang didapatkan
      Result = perhitungan(TweetsCleaned, pos.words, neg.words, negasi.words)
      
      #hitung total kolom dataframe
      SentimentDataFrame_tampil = data.frame(Tweet=TweetsCleaned, stringsAsFactors=FALSE)
      jumlah_tweet=nrow(SentimentDataFrame_tampil)
      
      total_tweet_raw=paste("Jumlah Tweet mentah (Raw) yang didapat :", jumlah_tweet_raw)
      
      total_tweet=paste("Jumlah Tweet yang diolah (sentences) :", jumlah_tweet)
      
      paste(inputan, jumlah_alokasi,  total_tweet_raw, total_tweet, tgl, sep="\n")
      #sep="\n"
    })
    
    output$info_user_NB <- renderText({
      
      #paste satu persatu hasil inputan dari user
      inputan=paste("Topik yang akan di analisa :", input$text)
      jumlah_alokasi=paste("Jumlah Tweet yang ingin didapat :", input$n)
      tgl=paste("Rentang waktu tweet :", input$dateRange[1], "sampai",  input$dateRange[2])
      
      #jumlah kolom tweet yang didapatkan
      Result = perhitungan(TweetsCleaned, pos.words, neg.words, negasi.words)
      
      #hitung total kolom dataframe
      SentimentDataFrame_tampil = data.frame(Tweet=TweetsCleaned, stringsAsFactors=FALSE)
      jumlah_tweet=nrow(SentimentDataFrame_tampil)
      
      total_tweet_raw=paste("Jumlah Tweet mentah (Raw) yang didapat :", jumlah_tweet_raw)
      
      total_tweet=paste("Jumlah Tweet yang diolah (sentences) :", jumlah_tweet)
      
      paste(inputan, jumlah_alokasi,  total_tweet_raw, total_tweet, tgl, sep="\n")
      #sep="\n"
    })
    
    output$munculsifat <- renderText({
      Result = perhitungan(TweetsCleaned, pos.words, neg.words, negasi.words)
      
      f_positif_tarik=sum(Result$f_positif)
      f_negatif_tarik=sum(Result$f_negatif)
      
      total_semua_sifat=f_positif_tarik+f_negatif_tarik
      
      f_persentase_positif=round((f_positif_tarik/total_semua_sifat)*100)
      f_persentase_negatif=round((f_negatif_tarik/total_semua_sifat)*100)
      
      Persentase_Kemunculan_Kalimat_Positif=paste("Persentase Kemunculan Kalimat Positif :",  f_persentase_positif, "%")
      Persentase_Kemunculan_Kalimat_Negatif=paste("Persentase Kemunculan Kalimat Negatif :",  f_persentase_negatif, "%")
      
      paste(Persentase_Kemunculan_Kalimat_Positif, Persentase_Kemunculan_Kalimat_Negatif, sep="\n")
      #sep="\n"
    })
    
    output$munculpol <- renderText({
      Result = perhitungan(TweetsCleaned, pos.words, neg.words, negasi.words)
      Positif=sum(str_count(Result$hasil, "Positif"))
      Netral=sum(str_count(Result$hasil, "Netral"))
      Negatif=sum(str_count(Result$hasil, "Negatif"))
      
      Persentase_Pol_Positif=paste("Total Polarity (Sifat) Positif :",  Positif)
      Persentase_Pol_Netral=paste("Total Polarity (Sifat) Netral :",  Netral)
      Persentase_Pol_Negatif=paste("Total Polarity (Sifat) Negatif :",  Negatif)
      
      paste(Persentase_Pol_Positif, Persentase_Pol_Netral, Persentase_Pol_Negatif, sep="\n")
      #sep="\n"
    })
    
    output$munculpol_naive <- renderText({

      Positif=sum(str_count(SentimentDataFrame$Polarity, "positive"))
      Netral=sum(str_count(SentimentDataFrame$Polarity, "neutral"))
      Negatif=sum(str_count(SentimentDataFrame$Polarity, "negative"))
      
      Persentase_Pol_Positif=paste("Total Polarity (Sifat) Positif :",  Positif)
      Persentase_Pol_Netral=paste("Total Polarity (Sifat) Netral :",  Netral)
      Persentase_Pol_Negatif=paste("Total Polarity (Sifat) Negatif :",  Negatif)
      
      paste(Persentase_Pol_Positif, Persentase_Pol_Netral, Persentase_Pol_Negatif, sep="\n")
      #sep="\n"
    })
    
    output$plot1 <- renderPlot({
      plotSentiments1(SentimentDataFrame, 'Sentiment Analysis of Tweets on Twitter')
    })
    
    output$plot2 <- renderPlot({
      Result = perhitungan(TweetsCleaned, pos.words, neg.words, negasi.words)
      
      f_positif_tarik=sum(Result$f_positif)
      f_negatif_tarik=sum(Result$f_negatif)
      
      total_semua_sifat=f_positif_tarik+f_negatif_tarik
      
      f_persentase_positif=round((f_positif_tarik/total_semua_sifat)*100)
      f_persentase_negatif=round((f_negatif_tarik/total_semua_sifat)*100)
      
      colors = c("#33FF90", "#28DBE3", "#DE4A35")
      
      A <- c(f_persentase_positif, f_persentase_negatif)
      barplot(A, main="Grafik Batang Total Jumlah Kemunculan Kalimat Sifat dari Dataset (Bag of Words)", xlab="Kategorisasi Sifat", ylab="Total Jumlah", 
              names.arg=c("Positif","Negatif"),
              col=colors,
              border=NA)
    })
    
    output$plot3 <- renderPlot({
      Result = perhitungan(TweetsCleaned, pos.words, neg.words, negasi.words)
      score <- as.numeric(Result$score)   
      hist(score, main =paste("Score Rata-rata" ), col=topo.colors(10))
      output$mean <- renderText({
        paste("Mean Sentiment score", mean(score))
      })
      
    })
    
    output$plot4 <- renderPlot({
      SentimentDataFrame = data.frame(text=TweetsCleaned, emotion=Emotion, polarity=Best_fit, stringsAsFactors=FALSE)
      
      SentimentDataFrame = within(SentimentDataFrame, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
      plotSentiments2(SentimentDataFrame, 'Emosi pada Tweet')
    })
    
    output$plot5 <- renderPlot({
      Result = perhitungan(TweetsCleaned, pos.words, neg.words, negasi.words)
      Positif=sum(str_count(Result$hasil, "Positif"))
      Netral=sum(str_count(Result$hasil, "Netral"))
      Negatif=sum(str_count(Result$hasil, "Negatif"))
      
      A <- c(Positif, Netral,  Negatif)
      
      colors = c("#33FF90", "#28DBE3", "#DE4A35")
      barplot(A, main="Grafik Batang Polarity Sentiment Analysis dengan Metode Lexicon", xlab="Kategorisasi Sifat", ylab="Total Jumlah",
              names.arg=c("Positif","Netral","Negatif"),
              col=colors,
              border=NA)
      
      
    })
    
    output$wordcloud <- renderPlot({
      wordcloud(dm$word, dm$freq, min.freq = 3, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
      png("MachineLearningCloud.png", width=5, height=5, units="in", res=550)
    })
    
    output$jaringankata <- renderPlot({
      plot(tdm, term = freq.terms, corThreshold = 0.3, weighting = T)
    })
    
    output$freqterms <- renderTable({
      freqs = data.frame(Frekuensi=word.freq)
    })
    
    output$AccLex<- renderPrint({
      conf.mat
    })
    
    output$AccNb<- renderPrint({
      conf2.mat
    })
    
    output$TopicModel <- renderTable({
      Topic_Model <- apply(term, MARGIN = 2, paste, collapse = ", ")
      data.frame(Topic_Model)
    })
    
    output$Raw <- renderTable({
      tweets.df
    })
    
    output$notif <- renderText({
      notif
    })
    
    output$sumbertweetplot_tbl <- renderTable({
      souce_tweet_table[with(souce_tweet_table, order(-Frekuensi)), ]
     
    })

    
  })
  
  
})
  



