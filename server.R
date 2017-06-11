library("twitteR")
library("ROAuth")
library("RCurl")
library("tm")
library("wordcloud")

library(shiny)
library(shinyLP)
library(httr)

consumer_key = "Enter yours"
consumer_secret = "Enter yours"
access_token = "Enter yours"
access_secret = "Enter yours"
options(httr_oauth_cache=T) 


# This will enable the use of a local file to cache OAuth 
# access credentials between R sessions.
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)



shinyServer(

  function(input, output,session) {
  
    

  #load database
    data(moviedb)
    
    
    output$fn = reactive({ifelse((input$searchm) %in% moviedb$search_name, "Found","Not Found")})
    
    #movie name
    output$name = reactive({
      
      n <- input$searchm
      name <- moviedb[(moviedb$search_name == n),1]
      return(name)
    })
    
    
    
  #insta valuebox
  output$instabox <- renderValueBox({
    n <- input$searchm
    ins <- URLencode(moviedb[(moviedb$search_name == n),13])
    get_ins<-httr::GET(ins)
    get2ins<-httr::content(get_ins, as="parsed")
    get2inst<-get2ins$data$media_count
    
    
    valueBox( value = get2inst, subtitle = "#hashcount", icon = icon("instagram"),
      color = "purple"
    )
  })
    
  
    #youtube value box
    output$youtubebox <- renderValueBox({
      n <- input$searchm
      ins <- URLencode(moviedb[(moviedb$search_name == n),8])
      get_lk<-httr::GET(ins)
      get2lk<-httr::content(get_lk, as="parsed")
      get2lik<-as.numeric(get2lk$items[[1]]$statistics$likeCount )
      get2dlik<-as.numeric(get2lk$items[[1]]$statistics$dislikeCount )
      add<-get2dlik+get2lik
      
      youtube <- paste0(sprintf('%.2f',((get2lik / add) * 100 )),"%")

      valueBox(
        value = youtube, subtitle = "youtube Hits", icon = icon("youtube", lib = "font-awesome"),
        color = "red"
      )
    })
    
    
    
    #IMDB valuebox
    output$'IMDB rating' <- renderValueBox({
      n <- input$searchm
      rating <- moviedb[(moviedb$search_name == n),3]
      
      valueBox(
        value = rating, subtitle = "IMDB Rating", icon = icon("imdb"),
        color = "olive"
      )
    })
    
  
    #Facebook valuebox  
    output$fb <- renderValueBox({
      n <- input$searchm
      ins <- URLencode(moviedb[(moviedb$search_name == n),17])
      get_ins<-httr::GET(ins)
      get2ins<-httr::content(get_ins, as="parsed")
      get2inst<-get2ins$likes
      
      valueBox(
        value = get2inst, subtitle = "Page Likes", icon = icon("facebook"),
        color = "blue"
      )
    })
    
  
    #poster
    output$poster <- renderUI({
      n <- input$searchm
      ins <- URLencode(moviedb[(moviedb$search_name == n),2])
      
      tags$img(src=ins,height="350",width="360")
      #iframe(width="300",height = "100%", url_link=ins)
      
    })
    
    #Description
  output$description = reactive({
    
    n <- input$searchm
    description <- toString(moviedb[(moviedb$search_name == n),4])
    return(description)
  })
  
  
  output$director = reactive({
    
    n <- input$searchm
    director <- toString(moviedb[(moviedb$search_name == n),5])
    return(director)
  })
  
  output$writers = reactive({
    
    n <- input$searchm
    writers <- toString(moviedb[(moviedb$search_name == n),6])
    return(writers)
  })
  
  
  output$stars = reactive({
    
    n <- input$searchm
    stars <- toString(moviedb[(moviedb$search_name == n),7])
    return(stars)
  })  
  
  
  #trailer
  output$video <- renderUI({
    n <- input$searchm
    ins <- URLencode(moviedb[(moviedb$search_name == n),9])
    
    iframe(width = "670", height = "350",url_link=ins)
    
  })
  
  
  #youtube details
  output$vc = reactive({
    n <- input$searchm
    ins <- URLencode(moviedb[(moviedb$search_name == n),8])
    get_vc<-httr::GET(ins)
    get2vc<-httr::content(get_vc, as="parsed")
    get2vcn<-get2vc$items[[1]]$statistics$viewCount
    return(get2vcn)
  })
  
  output$ylike = reactive({
    n <- input$searchm
    ins <- URLencode(moviedb[(moviedb$search_name == n),8])
    get_lk<-httr::GET(ins)
    get2lk<-httr::content(get_lk, as="parsed")
    get2lik<-get2lk$items[[1]]$statistics$likeCount
    return(get2lik)
  })
  
  
  output$ydlike = reactive({
    n <- input$searchm
    ins <- URLencode(moviedb[(moviedb$search_name == n),8])
    get_lk<-httr::GET(ins)
    get2lk<-httr::content(get_lk, as="parsed")
    get2dlik<-get2lk$items[[1]]$statistics$dislikeCount
    return(get2dlik)
  })
  
  
  output$ycm = reactive({
    n <- input$searchm
    ins <- URLencode(moviedb[(moviedb$search_name == n),8])
    get_lk<-httr::GET(ins)
    get2lk<-httr::content(get_lk, as="parsed")
    get2cm<-get2lk$items[[1]]$statistics$commentCount
    return(get2cm)
  })
  
  
  #GOOGLE data
  output$gt <- renderUI({
    n <- input$searchm
    gt <- URLencode(moviedb[(moviedb$search_name == n),15])
    iframe(width = "700", height = "330",url_link=gt)
    
  })
  
  
  

 
  #twitter Analysis
  
  TweetFrame<-function(twtList)
  {
    
    df<- do.call("rbind",lapply(twtList,as.data.frame))
    #removes emoticons
    df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text)
    return (df$text)
  }
  
  
  
  pos.words=scan('./positive-words.txt', what='character',comment.char=';')
  neg.words=scan('./negative-words.txt', what='character',comment.char=';')
  
  wordDatabase<-function()
  {
    pos.words<<-c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader')
    neg.words<<-c(neg.words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')
  }
  
  score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  {
    require(plyr)
    require(stringr)
    list=lapply(sentences, function(sentence, pos.words, neg.words)
    {
      sentence = gsub('[[:punct:]]',' ',sentence)
      sentence = gsub('[[:cntrl:]]','',sentence)
      sentence = gsub('\\d+','',sentence)
      sentence = gsub('\n','',sentence)
      
      sentence = tolower(sentence)
      word.list = str_split(sentence, '\\s+')
      words = unlist(word.list)
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      pp=sum(pos.matches)
      nn = sum(neg.matches)
      score = sum(pos.matches) - sum(neg.matches)
      list1=c(score, pp, nn)
      return (list1)
    }, pos.words, neg.words)
    score_new=lapply(list, `[[`, 1)
    pp1=score=lapply(list, `[[`, 2)
    nn1=score=lapply(list, `[[`, 3)
    
    scores.df = data.frame(score=score_new, text=sentences)
    positive.df = data.frame(Positive=pp1, text=sentences)
    negative.df = data.frame(Negative=nn1, text=sentences)
    
    list_df=list(scores.df, positive.df, negative.df)
    return(list_df)
  }
  
  library(reshape)
  sentimentAnalyser<-function(result)
  {
    #Creating a copy of result data frame
    test1=result[[1]]
    test2=result[[2]]
    test3=result[[3]]
    
    #Creating three different data frames for Score, Positive and Negative
    #Removing text column from data frame
    test1$text=NULL
    test2$text=NULL
    test3$text=NULL
    #Storing the first row(Containing the sentiment scores) in variable q
    q1=test1[1,]
    q2=test2[1,]
    q3=test3[1,]
    qq1=melt(q1, var='Score')
    qq2=melt(q2, var='Positive')
    qq3=melt(q3, var='Negative') 
    qq1['Score'] = NULL
    qq2['Positive'] = NULL
    qq3['Negative'] = NULL
    #Creating data frame
    table1 = data.frame(Text=result[[1]]$text, Score=qq1)
    table2 = data.frame(Text=result[[2]]$text, Score=qq2)
    table3 = data.frame(Text=result[[3]]$text, Score=qq3)
    
    #Merging three data frames into one
    table_final=data.frame(Text=table1$Text, Positive=table2$value, Negative=table3$value, Score=table1$value)
    return(table_final)
  }
  
  percentage<-function(table_final)
  {
    #Positive Percentage
    
    #Renaming
    posSc=table_final$Positive
    negSc=table_final$Negative
    
    #Adding column
    table_final$PosPercent = posSc/ (posSc+negSc)
    
    #Replacing Nan with zero
    pp = table_final$PosPercent
    pp[is.nan(pp)] <- 0
    table_final$PosPercent = pp*100
    
    #Negative Percentage
    
    #Adding column
    table_final$NegPercent = negSc/ (posSc+negSc)
    
    #Replacing Nan with zero
    nn = table_final$NegPercent
    nn[is.nan(nn)] <- 0
    table_final$NegPercent = nn*100
    
    return(table_final)
  }
  
  wordDatabase()
  
  twtList<-reactive({twtList<-searchTwitter(input$searchm, n=100, lang="en") })
  tweets<-reactive({tweets<-TweetFrame(twtList() )})
  
  result<-reactive({result<-score.sentiment(tweets(), pos.words, neg.words, .progress='none')})
  
  table_final<-reactive({table_final<-sentimentAnalyser(  result() )})
  table_final_percentage<-reactive({table_final_percentage<-percentage(  table_final() )})
  
  output$tabledata<-renderTable(table_final_percentage())
  
  
  trend_table<-reactive({trend_table<-toptrends(  input$trendingTable ) })
  output$trendtable<-renderTable(trend_table() )
  
  #WORDCLOUD
  
  wordclouds<-function(text)
  {
    library(tm)
    corpus<-Corpus(VectorSource(text))
    
    clean_text <- tm_map(corpus, removePunctuation)
    
    clean_text <- tm_map(clean_text, content_transformer(tolower))
    clean_text <- tm_map(clean_text, removeWords, stopwords("english"))
    clean_text <- tm_map(clean_text, removeNumbers)
    clean_text <- tm_map(clean_text, stripWhitespace)
    return(clean_text)
  }
  
  text_word<-reactive({text_word<-wordclouds(  tweets() ) })
  output$word<-renderPlot({wordcloud(text_word(), col=colors(length(text_word)) , 
                                     rot.per=0.3,random.order=F,max.words=80,  scale=c(4.5,1.5))		
  })
  
  
  #PIE CHART
  
  slices <- reactive({c(sum(table_final()$Positive), sum(table_final()$Negative)) })
  labels <- c("Positive", "Negative")
  library(plotrix)
  output$piechart<-renderPlot({pie3D(slices(), labels = labels, col=rainbow(length(labels)),
                                     explode=0.00, main="Sentiment Analysis") })

  
  
  extract.hashes = function(vec){
    
    hash.pattern = "#[[:alpha:]]+"
    have.hash = grep(x = vec, pattern = hash.pattern)
    
    hash.matches = gregexpr(pattern = hash.pattern,
                            text = vec[have.hash])
    extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
    
    df = data.frame(table(tolower(unlist(extracted.hash))))
    colnames(df) = c("tag","freq")
    df = df[order(df$freq,decreasing = TRUE),]
    return(df)
  }

  }
)
