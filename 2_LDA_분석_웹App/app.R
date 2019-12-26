##################################################################
# 연구동향 분석 서비스
##################################################################

rm(list=ls())

setwd("/srv/shiny-server/ldaa")
options(shiny.maxRequestSize=5000*1024^2) 


# Shiny

library(shiny)
library(ggpubr)
library(NLP4kec) # Custom Library
library(readr)
library(topicmodels)
library(LDAvis)
library(servr)
library(readr)
library(tm)
library(slam)
library(dplyr)
library(ggplot2) 
library(readxl)
library(scales)
library(lubridate)
library(KoNLP)

rpath = ""
df <- NULL
temp <- NULL
assign("rpath", "", envir = .GlobalEnv)

# Create Shiny app ----

a <- NULL
topic_clustering <- function(dpath,SEED, k,rpath,us_words,morph_type)
{
  #형태소 분석기 실행
  SEED <- as.numeric(SEED)
  k <- as.numeric(k)
  
  baseData <- df
  parsedData <- baseData
  content <- as.vector(unlist(baseData["text"]))
  print(content)
  a <<- content
  if(morph_type == "mecab"){
    text <- r_extract_noun(content, language = "ko")
  } else if(morph_type == "hannanum"){
    text <- content
    for(cidx in 1:length(content)){
      text[cidx] <- paste(unlist(extractNoun(a[cidx]),use.names = FALSE),collapse = " ")
    }
  } else{
    text <- content
  }
  

  #분석 결과 가져오기
  parsedData["text"] <- text
  idx <- which(!is.na(parsedData["text"]))
  parsedData <- parsedData[idx,]
  
  print("실행1")
  
  #컬럼명 변경하기
  colnames(parsedData) = c("id","date","name","text")
  
  write.csv(parsedData,paste(rpath,"input_data.csv",sep=""))
  
  ## 단어간 스페이스 하나 더 추가하기 ##
  parsedDataRe = parsedData
  parsedDataRe$text = gsub(" ","  ",parsedDataRe$text)
  
  ##################################################################
  #Text Pre-processing
  ##################################################################
  print("실행2")
  #Corpus 생성
  corp<-VCorpus(VectorSource(parsedDataRe$text))
  #특수문자 제거
  corp <- tm_map(corp, removePunctuation)
  #소문자로 변경
  #corp <- tm_map(corp, tolower)
  us_words = unlist(strsplit(us_words,","))
  s_words <- c("전략", "연구", "평가", "마련", "조사", "관리", "보다", "분석", "구축","연구","대한","결과","나타","필요","개념","필요","발생","통해","분석","위해","또한","관련","활용","방법","제시","가지","목적","다양","방안","고려","대상","따라서","최근","적용","사용","둘째","수준","확인","다음","구성","기존","조사","문제","각각","거나","이루","로써","가장","과정","정도","정성","대해","진행","때문","까지","도록","현재","경우","간의","로서","하나","주로","사실","포함","어져","실제","자료","다른","이후","제안","가장","동시","수행","우선","한국","파악","도출","중심","검토","주요","의미","실시","중요","향후","첫째","향상","단계","선정","가능성","가능","바탕","분야","논의","통한","로부터","논문","우리나라","면서")
  s_words <- c(s_words, us_words)
  
  #특정 단어 삭제
  corp <- tm_map(corp, removeWords, s_words)
  #동의어 처리
  #for (j in seq(corp))
  #{
  #  corp[[j]] <- gsub("kei", "한국환경정책평가연구원", corp[[j]])
  #}
  ##################################################################
  
  #Document Term Matrix 생성 (단어 Length는 2로 세팅)
  dtm<-DocumentTermMatrix(corp, control=list(removeNumbers=FALSE, wordLengths=c(2,Inf)))
  ## 한글자 단어 제외하기 ##
  colnames(dtm) = trimws(colnames(dtm))
  dtm = dtm[,nchar(colnames(dtm)) > 0]
  #Sparse Terms 삭제
  dtm <- removeSparseTerms(dtm, as.numeric(0.997))
  dtm
  ##Remove low tf-idf col and row
  term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
  new_dtm <- dtm[,term_tfidf >= 0]
  print("실행3")
  # Get_index
  
  doc_idx <- which(term_tfidf >= 0)
  
  new_dtm <- new_dtm[row_sums(new_dtm)>0,]
  
  ############################################
  ## Running LDA
  ############################################
  
  
  #LDA 실행
  lda_tm <- LDA(new_dtm, control=list(seed=SEED), k)
  
  #토픽별 핵심단어 저장하기
  term_topic <- terms(lda_tm, 150)
  
  #토픽별 핵심 단어 파일로 출력하기
  filePathName = paste(rpath,"term_topic.csv",sep="")
  write.table(term_topic, filePathName, sep=",", row.names=FALSE)
  
  #문서별 토픽 번호 저장하기
  doc_topic <- topics(lda_tm, 1)
  doc_topic_df <- as.data.frame(doc_topic)
  doc_topic_df$rown <- as.integer(row.names(doc_topic_df))
  
  #문서별 토픽 확률값 계산하기
  doc_Prob <- posterior(lda_tm)$topics
  doc_Prob_df <- as.data.frame(doc_Prob)
  filePathName = paste(rpath,"doc_prob_df.csv",sep="")
  write.table(doc_Prob_df, filePathName, sep=",", row.names=FALSE)
  
  #최대 확률값 찾기
  doc_Prob_df$maxProb = apply(doc_Prob_df, 1, max)
  filePathName = paste(rpath,"doc_prob_df.csv",sep="")
  write.table(doc_Prob_df, filePathName, sep=",", row.names=FALSE)
  
  #문서별 토픽번호 및 확률값 추출하기
  doc_Prob_df$rown = doc_topic_df$rown
  parsedData$rown = as.numeric(row.names(parsedData))
  id_topic <- merge(doc_topic_df, doc_Prob_df, by="rown")
  id_topic <- merge(id_topic, parsedData, by="rown", all.y = TRUE)
  id_topic <- subset(id_topic,select=c("rown","id","name","doc_topic","text","maxProb"))
  
  #문서별 토픽 번호 및 확률값 출력하기
  filePathName = paste(rpath,"id_topic.txt",sep="")
  write.table(id_topic, filePathName, sep="\t",row.names=FALSE,quote=FALSE)
  
  #단어별 토픽 확률값 출력하기
  posterior(lda_tm)$terms
  filePathName = paste(rpath,"lda_tm.csv",sep="")
  write.table(posterior(lda_tm)$terms, filePathName, sep=",", row.names=FALSE)
  
  #########################################
  ## Make visualization
  #########################################
  
  # phi는 각 단어별 토픽에 포함될 확률값 입니다.
  phi <- posterior(lda_tm)$terms %>% as.matrix
  # theta는 각 문서별 토픽에 포함될 확률값 입니다.
  theta <- posterior(lda_tm)$topics %>% as.matrix
  # vocab는 전체 단어 리스트 입니다.
  vocab <- colnames(phi)
  
  # 각 문서별 문서 길이를 구합니다.
  doc_length <- vector()
  doc_topic_df<-as.data.frame(doc_topic)
  
  for( i in as.numeric(row.names(doc_topic_df))){
    temp <- corp[[i]]$content
    doc_length <- c(doc_length, nchar(temp[1]))
  }
  
  # 각 단어별 빈도수를 구합니다.
  new_dtm_m <- as.matrix(new_dtm)
  freq_matrix <- data.frame(ST = colnames(new_dtm_m),
                            Freq = colSums(new_dtm_m))
  
  
  # 위에서 구한 값들을 파라메터 값으로 넘겨서 시각화를 하기 위한 데이터를 만들어 줍니다.
  json_lda <- createJSON(phi = phi, theta = theta,
                         vocab = vocab,
                         doc.length = doc_length,
                         term.frequency = freq_matrix$Freq)
  #mds.method = jsPCA
  #mds.method = canberraPCA
  
  serVis(json_lda, out.dir='../lda_result/', open.browser=FALSE)
  filePathName = paste(rpath,"index.html",sep="")
  file.copy('../lda_result/index.html', filePathName)
  
}


# Define server logic to read selected file ----
server <- function(input, output) {
  
  #print(input$seed)
  #print(input$nTopics)
  
  observeEvent(input$do, {
    output$ldavis <- renderText("Processing...")
    output$doc_topics <- renderText("Processing...")
    
    print("why")
    s_date <- Sys.Date()
    set.seed(Sys.time())
    rn <- sample(1:100000000,1)
    
    rpath <<- paste("../lda_result/",as.character(s_date), "_" ,rn , "_",sep="")
    topic_clustering(input$file1$datapath, input$seed,input$nTopics,rpath,input$s_words,input$morph_type)
    filePathName = paste(rpath,"id_topic.txt",sep="")
    id_topic_mat <- read.delim(filePathName,quote="",check.names=F)
    
    filePathName = paste(rpath,"index.html",sep="")
    url <- a("Link", href=filePathName,target="_blank")
    
    
    output$ldavis <- renderUI({
      tagList("\n\n LDA result :", url)
    })
    
    
    output$doc_topics <- renderTable({
      fields <- c("name","doc_topic")
      return (id_topic_mat[,fields])
    })
    
    
    output$topicdist <- renderPlot({
      doc_topics <- as.vector(unlist(id_topic_mat[,"doc_topic"]))
      doc_topics <- table(doc_topics) 
      doc_topics <- doc_topics / sum(doc_topics)
      doc_topics <- as.data.frame(doc_topics)
      
      pie <- ggplot(doc_topics, aes(x = "", y=Freq, fill = factor(doc_topics))) + geom_bar(width = 2, stat = "identity") + theme(axis.line = element_blank(),  plot.title = element_text(hjust=0.5)) + 
        labs(fill="class", x=NULL, y=NULL, title="Topic Distribution")
      pie + coord_polar(theta = "y", start=0)
      
    },res=170)
    
  })
  
  output$trends <- renderPlot({
    
    # 년도별 Visualization
    id_topic_mat <- read.delim(paste(rpath,"id_topic.txt",sep=""),quote="",check.names=F)
    input_data_mat <- read_csv(paste(rpath,"input_data.csv",sep=""))
    
    fields <- c("id","doc_topic")
    
    mat1 <- id_topic_mat[,fields]
    
    fields <- c("id","date")
    mat2 <- input_data_mat[,fields]
    
    result_mat <- cbind(mat1,mat2)
    
    result_dates <- as.character(unlist(result_mat[,"date"]))
    result_dates <- as.Date(result_dates,"%Y%m%d")
    result_topics <- as.factor(unlist(result_mat[,"doc_topic"]))
    
    print(result_dates)
    
    result_df <- data.frame(result_dates,result_topics)
    
    # 주제 할당이 되지 않는 경우는 제외
    idx <- which(!is.na(result_df[,"result_topics"]))
    
    result_df <- result_df[idx,]
    
    pt <- input$period_type
    
    if(pt == "years"){
      result_df$result_dates <- year(result_df$result_dates)
    } else if(pt == "quarters"){
      months <- month(result_df$result_dates)
      result <- rep("",length(months))
      
      idx1 <- which(months == 1 | months == 2 | months == 3)
      idx2 <- which(months == 4 | months == 5 | months == 6)
      idx3 <- which(months == 7 | months == 8 | months == 9)
      idx4 <- which(months == 10 | months == 11 | months == 12)
      
      result[idx1] <- "Q1"
      result[idx2] <- "Q2"
      result[idx3] <- "Q3"
      result[idx4] <- "Q4"
      
      result_df$result_dates <- paste0(year(result_df$result_dates),"-", result)
      
    } else if(pt == "months"){
      m_list <- month(result_df$result_dates)
      
      for(midx in 1:length(m_list)){
        m <- as.numeric(m_list[midx])
        if(m < 10){
          m_list[midx] <- paste(0,m,sep="") 
        }
      }
      result_df$result_dates <- paste0(year(result_df$result_dates),"-", m_list)
    } else{
      result_df$result_dates <- paste0(year(result_df$result_dates),"-", month(result_df$result_dates), day(result_df$result_dates))
    }
    
    result_df <- as.data.frame(result_df %>% group_by(result_dates, result_topics) %>% tally)
    write.table(result_df, "../lda_result/result_df.csv", sep=",", row.names=FALSE)
    
    graph <- ggplot()
    graph <- graph + labs(fill="Topic") + xlab("Date") + ylab("Count")
    graph <- graph + geom_bar(data = result_df,stat="identity",aes(x = result_dates, y = n,fill=result_topics))
    #graph
    #graph <- graph + geom_text(data = result_df, aes(x = Date, label = paste0(Topic*100,'%')), size = 4, position = position_stack(vjust = 0.5))
    
    # Graph2
    
    result_df2 <- result_df
    result_df2 <- result_df2 %>% 
      group_by(result_dates) %>% 
      mutate(n=n/sum(n))
    
    print(result_df2[1:5,])
    graph2 <- ggplot()
    graph2 <- graph2 + labs(fill="Topic") + xlab("Date") + ylab("Proportion")
    graph2 <- graph2 + geom_line(data=result_df2, aes(x = result_dates, y = n, group=result_topics, color=result_topics,position="fill"),size=2,stat="identity")
    graph2 <- graph2 + scale_y_continuous(labels = scales::percent_format(accuracy = 2L))
    graph2 <- graph2 + scale_color_discrete(name = "Topic", labels = sort(unique(result_df[,"result_topics"])))
    
    ggarrange(graph, graph2, ncol=1,nrow=2)
  })
  
  
  
  output$data <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    file <- input$file1
    df <<- read_excel(file$datapath,1)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
}

##################################################################
# 연구동향 분석 서비스
##################################################################

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("LDA 연구동향 분석 서비스"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput('file1', '엑셀 파일 선택(*.xlsx)',  accept = c(".xlsx")),
      
      # Horizontal line ----
      tags$hr(),
      
      textInput(inputId = "nTopics", label = "토픽수", value = "5"),
      textInput(inputId = "seed", label = "시드", value = "2007"),
      radioButtons("period_type", "",  choices = c(년 = "years", 분기= "quarters", 월= "months", 일 = "days"), selected = "years", inline=T),
      textInput(inputId = "s_words", label = "Stop Words", value = ""),
      radioButtons(inputId = "morph_type", "형태소 분석기",
                   choices = c(은닢한전= "mecab",
                               한나눔= "hannanum",
                               띄워쓰기 = "space")),
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      actionButton("do", "분석 실행")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      
      
      tabsetPanel(type = "tabs",
                  tabPanel("데이터", tableOutput("data")),
                  tabPanel("LDA 시각화", tableOutput("ldavis")),
                  tabPanel("문서 주제", tableOutput("doc_topics")),
                  tabPanel("주제 분포", plotOutput("topicdist", width = "100%", height= 800)),
                  tabPanel("동향 분석", plotOutput("trends", width = "100%", height= 800))
      )
    )
    
  )
)


# Create Shiny app ----
shinyApp(ui, server)
