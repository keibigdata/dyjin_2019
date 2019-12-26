##################################################################
# 키워드 연관 네트워크 분석 서비스
##################################################################

rm(list=ls())
#setwd("/srv/shiny-server/kn2")
setwd("/srv/shiny-server/kna")
options(shiny.maxRequestSize=5000*1024^2) 
options(shiny.plot.res=300)

# Shiny
library(shiny)
library(NLP4kec)
library(readr)
library(KoNLP)
library(arules)
library(igraph)
library(combinat)
library(DT)
library(showtext)
library(extrafont)
library(readxl)
library(writexl)

loadfonts()
showtext_auto()
font_add("NanumBarunGothic", "NanumBarunGothic.ttf")
indi_num <- 100

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("키워드 연관 네트워크 분석"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput('file1', '엑셀 파일 선택(*.xlsx)',  accept = c(".xlsx")),
      
      selectInput(inputId = "category", "카테고리",  choices=''),
      textInput(inputId = "start_date", label = "시작날짜", value = "20100101"),
      textInput(inputId = "end_date", label = "끝날짜", value = "20171231"),
      actionButton("filtering", "필터링 실행"),
      
      # Horizontal line ----
      tags$hr(),
      
      textInput(inputId = "SEED", label = "시드", value = "1001"),
      textInput(inputId = "p_supp", label = "지지도", value = "0.01"),
      textInput(inputId = "p_conf", label = "신뢰도", value = "0.3"),
      textInput(inputId = "n_rel", label = "관계수", value = "30"),
      textInput(inputId = "s_words", label = "불용어", value = ""),
      checkboxInput("chk", label = "2개 이상 관계 활용", value = FALSE),
      radioButtons(inputId = "morph_type", "형태소 분석기",
                   choices = c(한나눔= "hannanum",
                                  띄워쓰기 = "space")),
      radioButtons(inputId = "ordering", "정렬방법",
                   choices = c(Lift = "lift",
                               Support = "support")),
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      actionButton("do", "분석실행"), 
      width=2
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("데이터", DT::dataTableOutput("data")),
                  tabPanel("연관분석", tableOutput("result")),
                  tabPanel("네트워크 시각화", plotOutput("plot", width = "100%", height= 800),downloadButton("dBtn", "Download")),
                  tabPanel("네트워크 지표", tableOutput("indi"),downloadButton("indi_dBtn", "Download")),
                  tabPanel("메뉴얼",  includeMarkdown("kn_manual.Rmd"))
      )
    )
  )
)


##################################################################
# 키둬드 네트워크 분석 서비스
##################################################################

s_date <- Sys.Date()
rn <- sample(1:100000000,1)
fn1 <- paste("./result/", s_date,"_",rn,".csv",sep="")
fn2 <- paste("./result/", s_date,"_",rn,".pdf",sep="")
fn3 <- paste("./result/", s_date,"_indi_",rn,".xlsx",sep="")


strsplit_space <- function(x)
{
  as.vector(unlist(strsplit(x," ")))
}

assoc_analysis <- function(p_supp, p_conf,SEED,n_rel,s_words,ordering,is_mwords,morph_type)
{
  
  n_rel <- as.numeric(n_rel)
  
  s_words = unlist(strsplit(s_words,","))
  rules <- as.vector(df$text)
  rules <- rules[!is.na(rules)]
  
  if(morph_type == "hannanum"){
    tran <- Map(extractNoun, rules)
  } else{
    tran <- Map(strsplit_space,rules)
  }
  tran <- unique(tran)
  
  tran <- sapply(tran, unique)
  tran <- Filter(function(x){length(x)>=2},tran)
  tran <- sapply(tran, function(x) {Filter(function(y) {nchar(y) > 1 && is.hangul(y)},x)})
  tran <- sapply(tran, function(x) {Filter(function(y) {all(y != s_words)},x)})
  tran <- Filter(function(x){length(x)>=2},tran)
  
  
  names(tran) <- paste("Tr", 1:length(tran), sep="")
  wordtran <- as(tran, "transactions")
  
  wordtab <- crossTable(wordtran)
  
  
  
  #지지도와 신뢰도를 낮게 설정할수록 결과 자세히 나옴
  ares <- apriori(wordtran, parameter=list(supp=as.numeric(p_supp), conf=as.numeric(p_conf))) 
  ares <- sort(ares, by=ordering)
  
  #print(ares)
  
  tryCatch(
    
    {
      result_mat <- as.matrix(inspect(ares))
      colnames(result_mat)[2] <- "->"
      
      idx <- which(result_mat[,"lhs"] != "{}")
      idx <- intersect(idx,which(as.numeric(result_mat[,"count"]) >= 2))
      print("Whyerror3")
      result_mat <- result_mat[idx,]
      ares <- ares[idx,]
      
      if(length(idx) < n_rel)
        n_rel <- length(idx)
      
      idx <- 1:n_rel
      
      # {2개 이상인것} 인것 제거 (2보다 작은것 제거)
      if(is_mwords == FALSE){
        rnum <- 2
        idx <- intersect(idx,as.vector(which(sapply(result_mat[,"lhs"],function(x){length(strsplit(x,",")[[1]])}) < rnum)))
      }
      
      result_mat <- result_mat[idx,]
      print(ares)
      print(max(idx))
      #ares <- ares[[idx]]
      
      # rules <- labels(ares, ruleSep=" ")
      # rules <- gsub("\\{","",rules)
      # rules <- gsub("\\}","",rules)
      # rules <- sapply(rules, strsplit, " ",  USE.NAMES=F)
      # rulemat <- do.call("rbind", rules)
      
      # print(head(rulemat))
      # idx에 포함되는것은 단어들간의 연관관계가 아니기 때문에 제외
      sub_result_mat <- result_mat[,c("lhs","rhs")]
      sub_result_mat <- gsub("\\{","",sub_result_mat)
      sub_result_mat <- gsub("\\}","",sub_result_mat)
      ruleg <- graph.edgelist(sub_result_mat,directed=F) 
      # ruleg <- graph.edgelist(rulemat[idx,],directed=F) 
      
      
      #### 단어근접중심성파악 ####
      
      closen <- igraph::closeness(ruleg)
      
      
      #### node(vertex), link(edge) 크기 조절 (복잡) ####
      
      V(ruleg)$size<- igraph::degree(ruleg)/ (0.25 * (n_rel/20))
      
      #### node(vertex), link(edge) 크기 조절 (단순) ####
      ruleg <- simplify(ruleg)
      
      indi1 <- sort(igraph::degree(ruleg)/(length(degree(ruleg))-1), decreasing=T)
      indi1 <- cbind(names(indi1),round(as.vector(indi1),3))
      indi2 <- sort(igraph::closeness(ruleg), decreasing=T)
      indi2 <- cbind(names(indi2),round(as.vector(indi2),3))
      indi3 <- sort(igraph::betweenness(ruleg,normalized = TRUE), decreasing=T)
      indi3 <- cbind(names(indi3),round(as.vector(indi3),3))
      indi4 <- sort(igraph::eigen_centrality(ruleg)$vector, decreasing=T)
      indi4 <- cbind(names(indi4),round(as.vector(indi4),3))
      
      indi <- cbind(indi1,indi2,indi3,indi4)
      colnames(indi) <- c("연결중심성","값","근접중심성","값","매개중심성","값","고유중심성","값")
      
      try({
        indi <- indi[1:indi_num,]
      })
      
      print(head(indi))
      set.seed(SEED)
      #plot(ruleg)
      
      #### 매개중심성 #### 
      btw<-igraph::betweenness(ruleg)
      btw.score<-round(btw)+1
      btw.colors<-rev(heat.colors(max(btw.score)))
      V(ruleg)$color<-btw.colors[btw.score]
      #V(ruleg)$degree<-degree(ruleg)
      #V(ruleg)$label.cex<-2*(V(ruleg)$degree / max(V(ruleg)$degree))
      ret <- list('result_mat' = result_mat, 'ruleg' = ruleg, 'indi' = indi)
      return (ret)
    },
    error=function(error_message) {
      message("This is my custom message.")
      message("And below is the error message from R:")
      return(NA)
    }
  )
}


df <- NULL

# Define server logic to read selected file ----
server <- function(input, output,session) {
  
  observeEvent(input$do, {
    req(input$file1)
    output$result <- renderText({"Processing..."})
    print(input$chk)
    
    ret <- assoc_analysis(input$p_supp,input$p_conf,input$SEED,input$n_rel,input$s_words,input$ordering,input$chk,input$morph_type)
    
    print(ret)
    output$result <- renderTable({
      validate(
        need(ret, '데이터 또는 인자를 다시 입력하세요.')
      )
      
      return(ret$result_mat)
    })
    
    output$plot <- renderPlot({
      validate(
        need(ret, '데이터 또는 인자를 다시 입력하세요.')
      )
      
      cairo_pdf(fn2,family="NanumBarunGothic")
      plot(ret$ruleg,vertex.label.family="NanumBarunGothic",vertex.label.cex=0.7)
      dev.off()
      plot(ret$ruleg,vertex.label.family="NanumBarunGothic",vertex.label.cex=1.5)
      
    })
    output$indi <- renderTable({
      validate(
        need(ret, '데이터 또는 인자를 다시 입력하세요.')
      )
      
      result_mat <- data.frame(ret$indi)
      write_xlsx(result_mat, path = fn3)
      return(result_mat)
    })
    
  })
  
  observeEvent(input$filtering, {
    
    output$data <- DT::renderDataTable({
      
      req(input$file1)
      file <- input$file1
      df <<- read_excel(file$datapath,1)
      
      df_idx <- 1:nrow(df)
      
      print(df_idx)
      category <- input$category
      start_date <- input$start_date
      end_date <- input$end_date
      
      category_idx <- which(df[,"category"] == category)
      start_idx <- which(df[,"date"] >= as.numeric(start_date))
      end_idx <- which(df[,"date"] <= as.numeric(end_date))
      
      if(length(category_idx) == 0)
        category_idx <- df_idx
      
      if(length(start_idx) == 0)
        start_idx <- df_idx
      
      if(length(end_idx) == 0)
        end_idx <- df_idx
      
      c_idx <- intersect(intersect(category_idx,start_idx),end_idx)
      df <- df[c_idx,]
      
      if(input$disp == "head") {
        return(head(df))
      }
      else {
        return(df)
      }
      
    })
    
  })
  
  
  
  output$data <- DT::renderDataTable({
    req(input$file1)
    file <- input$file1
    df <<- read_excel(file$datapath,1)
    
    category_list <- df$category
    updateSelectInput(session, "category",
                      choices = category_list
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  output$dBtn <- downloadHandler(
    filename = function(){
      paste("output_network_",rn,".pdf",sep="")
    },
    content = function(file){
      file.copy(fn2, file)
    }
    
    
  )
  
  
  output$indi_dBtn <- downloadHandler(
    filename = function(){
      paste("output_indi_",rn,".xlsx",sep="")
    },
    content = function(file){
      file.copy(fn3, file)
    }
    
    
  )
  
}



# Create Shiny app ----
shinyApp(ui, server)
