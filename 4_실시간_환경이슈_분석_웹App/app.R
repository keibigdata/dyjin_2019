library(dplyr)
library(shiny)
library(plotly)
library(ggplot2)
library(ggpubr)
library(showtext)
library(ggrepel)
library(dplyr)
library(tidyr)
library(igraph)
library(viridis)
library(reshape2)
library(ggthemes)

showtext.auto()

options(shiny.maxRequestSize=5000*1024^2) 
options(shiny.plot.res=250)
font_add("NanumBarunGothic", "NanumBarunGothic.ttf")


# Define UI for dataset viewer app ----
idx_path <- "/media/Data/Realtime_Analysis/Indexing/" 
kf_path <- "/media/Data/Realtime_Analysis/Keyword_Frequency/" 
kn_path <- "/media/Data/Realtime_Analysis/Keyword_Network/" 
ds_path <- "/media/Data/Realtime_Analysis/Doc_Summary/" 
kgt_path <- "/media/Data/Realtime_Analysis/Keyword_Group_Trends/" 
kgf_path <- "/media/Data/Realtime_Analysis/Keyword_Group_Frequency/" 
kco_path <- "/media/Data/Realtime_Analysis/Keyword_Cooccurence/" 

kco_label_list <- as.vector(unlist(read.delim("/media/Realtime_Analysis/data/c_keyword_list.txt",header=FALSE)))
corpus_type1 = "NN"
corpus_type2 = "ME"

# 키워드 빈도수 몇개 까지 출력 할것인지 설정
kf_num <- 100

period <- 7
cnt <- 15
fcnt <- 20
diff_cnt <- 15
n_rel <- 70
SEED <- 1024

# 트랜드 엔트리 출력 갯수
dd <- 15

sys_dt <- format(Sys.Date()-1,"%Y-%m-%d")

ui <- fluidPage(
  
  # App title ----
  titlePanel("환경이슈 분석 서비스"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Specify the number of observations to view ----
      textInput("dt", "날짜", sys_dt),
      radioButtons("pr", "기간설정",c(7,14,30,60,180,365)),
      actionButton("update", "업데이트"),
      width = 2
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("데이터", fluidRow(
          h4("네이버 뉴스"),
          column(12, DT::dataTableOutput("indexing1")),
          column(12,h1("")),
          h4("환경부 보도자료/e-환경뉴스"),
          column(12, DT::dataTableOutput("indexing2"))
        )),
        
        tabPanel("키워드 빈도", fluidRow(
          column(6,h4("네이버 뉴스"),plotOutput("kf_plot1",height='600px')),
          column(6,h4("환경부 보도자료/e-환경뉴스"),plotOutput("kf_plot2",height='600px')),
          column(6,tableOutput("kf_table1")),
          column(6,tableOutput("kf_table2"))
        )),
        
        tabPanel("키워드 트랜드",fluidRow(
          column(12,h4("네이버 뉴스"), plotlyOutput("issue_p1",height = "100%")),
          column(12,h4("환경부 보도자료/e-환경뉴스"), plotlyOutput("issue_p2",height = "100%"))
        )),
        
        tabPanel("키워드 빈도변화",fluidRow(
          column(12,h4("네이버 뉴스"), plotOutput("kc_plot1",width="90%"),h4("환경부 보도자료/e-환경뉴스"),plotOutput("kc_plot2",width="90%"))
        )),
        
        tabPanel("키워드 패턴변화",fluidRow(
          #column(12,h4("네이버 뉴스"), plotOutput("kc_plot1",height= '600px'),h4("환경부 보도자료/e-환경뉴스"),plotOutput("kc_plot2",height= '600px'))
          column(12,h4("네이버 뉴스"), plotOutput("kco_plot1",width = "100%", height = "600px"))
          #column(12,h3("네이버 뉴스"), plotlyOutput("ss",height = "100%"))
          #column(12,h3("환경부 보도자료/e-환경뉴스"), plotlyOutput("issue_p2",height = "100%"))
        )),
        
        
        tabPanel("키워드 네트워크",fluidRow(
          column(12,h4("네이버 뉴스"), plotOutput("kn_plot1",height='800',width='100%')),
          column(12,h4("환경부 보도자료/e-환경뉴스"), plotOutput("kn_plot2",height='800', width='100%'))
        )),
        
        tabPanel("요약문장",fluidRow(
          column(6,h3("네이버 뉴스"), tableOutput("ds_table1")),
          column(6,h3("환경부 보도자료/e-환경뉴스"), tableOutput("ds_table3"))
        )),
        
        tabPanel("키워드 그룹 빈도",fluidRow(
          column(12,h4("네이버 뉴스"),plotOutput("kgf_plot1"),h4("환경부 보도자료/e-환경뉴스"),plotOutput("kgf_plot2"),tableOutput("kgf_table"))
        )),
        
        tabPanel("키워드 그룹 트랜드",fluidRow(
          column(12,h4("네이버 뉴스")),
          column(12,plotOutput("kgt_plot1",height='450')),
          column(12,h4("환경부 보도자료/e-환경뉴스")),
          plotOutput("kgt_plot2",height='450')
        ))
        
        #tabPanel("LDA", DT::dataTableOutput("indexing4"))
        
      ), width=9
    )
  )
)



server <- function(input, output) {
  
  datasetInput1 <- eventReactive(input$update, {
    fn <- paste(idx_path,input$dt,'_', input$pr ,'_NN_idx.txt',sep="")
    dt <- read.delim(fn,header=FALSE,sep = ",")
    dt <- dt[,2:3]
    colnames(dt) <- c("Date","Title")
    dt
  }, ignoreNULL = FALSE)
  
  
  datasetInput2 <- eventReactive(input$update, {
    fn <- paste(idx_path,input$dt,'_', input$pr ,'_ME_idx.txt',sep="")
    dt <- read.delim(fn,header=FALSE,sep = ",")
    dt <- dt[,2:3]
    colnames(dt) <- c("Date","Title")
    dt
  }, ignoreNULL = FALSE)
  
  kgf  <- eventReactive(input$update, {
    fn <- paste(kgf_path,input$dt,'_',input$pr,'_','NN_ME_KGF.txt',sep="")
    
    dt <- as.matrix(read.delim(fn,header=FALSE,sep = ","))
    colnames(dt) <- dt[1,]
    dt <- dt[-1,]
    dt
  }, ignoreNULL = FALSE)
  
  kco1  <- eventReactive(input$update,{
    
    c_melted_cormat <- NULL
    n_plot <- 10
    for(i in 0:(n_plot-1)){
      dt <- as.character(as.Date(input$dt) - as.numeric(input$pr) *i)
      dt2 <- as.character(as.Date(input$dt) - as.numeric(input$pr) *i + as.numeric(input$pr) )
      id <- paste(dt, " ~ ",dt2,sep="")
      fn <- paste(kco_path,dt,"_",input$pr,"_",corpus_type1 , "_", 'KCO','.txt',sep="")
      cm <- as.matrix(read.delim(fn,sep=",",header=FALSE))
      
      colnames(cm) <- kco_label_list
      row.names(cm) <- kco_label_list
      cm <- as.matrix(cm)
      #cm[cm < quantile(cm,0.7)] <- 0
      
      melted_cormat <- melt(cm)
      melted_cormat <- cbind(id,melted_cormat)
      colnames(melted_cormat)[1] <- "id"
      
      
      c_melted_cormat <- rbind(c_melted_cormat ,melted_cormat)
    }
    
    gg <- ggplot(c_melted_cormat, aes(x=Var2, y=Var1, fill=value))
    gg <- gg + geom_tile(color="white", size=0.1)
    gg <- gg + scale_fill_viridis()
    gg <- gg + facet_wrap(~id, ncol=5)
    gg <- gg + coord_equal()
    gg <- gg + labs(x=NULL, y=NULL, title="")
    gg <- gg + theme_tufte(base_family="wqy-microhei")
    gg <- gg + theme(axis.ticks=element_blank())
    gg <- gg + theme(plot.title=element_text(size=15))
    gg <- gg + theme(axis.text=element_text(size=12,vjust=0,hjust=1))
    gg <- gg + theme(axis.text.x=element_text(angle=90))
    gg <- gg + theme(panel.border=element_blank())
    gg <- gg + theme(legend.position="none")
    gg
  }, ignoreNULL = FALSE)
  
  
  kf1 <- eventReactive(input$update, {
    kf_fn1 <- paste(kf_path,input$dt,"_",input$pr,"_",corpus_type1 , "_", 'TF','.txt',sep="")
    kf_fn2 <- paste(kf_path,input$dt,"_",input$pr,"_",corpus_type1 , "_", 'norm_TF','.txt',sep="")
    
    kf_dt1 <- read.delim(kf_fn1,header=FALSE,sep = ",")
    kf_dt2 <- read.delim(kf_fn2,header=FALSE,sep = ",")
    
    kf_dt <- cbind(kf_dt1,kf_dt2)
    colnames(kf_dt) <- c("순위","단어","TF","순위","단어","TF-IDF")
    kf_dt
  }, ignoreNULL = FALSE)
  
  kf2 <- eventReactive(input$update, {
    kf_fn1 <- paste(kf_path,input$dt,"_",input$pr,"_",corpus_type2 , "_", 'TF','.txt',sep="")
    kf_fn2 <- paste(kf_path,input$dt,"_",input$pr,"_",corpus_type2 , "_", 'norm_TF','.txt',sep="")
    
    kf_dt1 <- read.delim(kf_fn1,header=FALSE,sep = ",")
    kf_dt2 <- read.delim(kf_fn2,header=FALSE,sep = ",")
    
    kf_dt <- cbind(kf_dt1,kf_dt2)
    colnames(kf_dt) <- c("순위","단어","TF","순위","단어","TF-IDF")
    kf_dt
    
  }, ignoreNULL = FALSE)
  
  ds1 <- eventReactive(input$update, {
    #ds_fn1 <- paste(ds_path,input$dt,"_",input$pr,"_",corpus_type1 , "_", cnt,'_TR_Summary','.txt',sep="")
    #ds_fn1 <- paste(ds_path,input$dt,"_",input$pr,"_",corpus_type1 , "_", 'DTC','_TR_Summary','.txt',sep="")
    ds_fn2 <- paste(ds_path,input$dt,"_",input$pr,"_",corpus_type1 , "_", 'DBSCAN','_TR_Summary','.txt',sep="")
    
    #ds_dt1 <- read.delim(ds_fn1,header=FALSE,sep = ",")
    ds_dt2 <- read.delim(ds_fn2,header=FALSE,sep = ",")
    colnames(ds_dt2) <- c("순위","문장","문서수")
    #ds_dt <- rbind(ds_dt1,ds_dt2)
    #colnames(ds_dt1) <- c("순위","문장")
    #colnames(ds_dt) <- c("순위","문장","문서수")
    #ds_dt <- ds_dt[order(ds_dt[,3]),]
    #ds_dt <- ds_dt[,1:2]
    #ds_dt[,1] <- 1:nrow(ds_dt)
    ds_dt <- ds_dt2
    ds_dt <- ds_dt[order(-ds_dt[,3]),]
    ds_dt[,1] <- 1:nrow(ds_dt)
    
    colnames(ds_dt) <- c("순위","문장","문서수")
    ds_dt
    
  }, ignoreNULL = FALSE)
  
  # ds2 <- eventReactive(input$update, {
  #   ds_fn1 <- paste(ds_path,input$dt,"_",input$pr,"_",corpus_type1 , "_", 'DTC','_TR_Summary','.txt',sep="")
  #   print(ds_fn1)
  #   ds_dt1 <- read.delim(ds_fn1,header=FALSE,sep = ",")
  #   colnames(ds_dt1) <- c("순위","문장","문서수")
  #   ds_dt <- ds_dt1
  #   ds_dt
  #   
  # }, ignoreNULL = FALSE)
  
  ds3 <- eventReactive(input$update, {
    #ds_fn1 <- paste(ds_path,input$dt,"_",input$pr,"_",corpus_type1 , "_", cnt,'_TR_Summary','.txt',sep="")
    #ds_fn1 <- paste(ds_path,input$dt,"_",input$pr,"_",corpus_type1 , "_", 'DTC','_TR_Summary','.txt',sep="")
    ds_fn2 <- paste(ds_path,input$dt,"_",input$pr,"_",corpus_type2 , "_", 'DBSCAN','_TR_Summary','.txt',sep="")
    
    #ds_dt1 <- read.delim(ds_fn1,header=FALSE,sep = ",")
    ds_dt2 <- read.delim(ds_fn2,header=FALSE,sep = ",")
    colnames(ds_dt2) <- c("순위","문장","문서수")
    #ds_dt <- rbind(ds_dt1,ds_dt2)
    #colnames(ds_dt1) <- c("순위","문장")
    #colnames(ds_dt) <- c("순위","문장","문서수")
    #ds_dt <- ds_dt[order(ds_dt[,3]),]
    #ds_dt <- ds_dt[,1:2]
    #ds_dt[,1] <- 1:nrow(ds_dt)
    ds_dt <- ds_dt2
    ds_dt <- ds_dt[order(-ds_dt[,3]),]
    ds_dt[,1] <- 1:nrow(ds_dt)
    
    colnames(ds_dt) <- c("순위","문장","문서수")
    ds_dt
    
  }, ignoreNULL = FALSE)
  
 # ds4 <- eventReactive(input$update, {
 #   ds_fn1 <- paste(ds_path,input$dt,"_",input$pr,"_",corpus_type2 , "_", 'DTC','_TR_Summary','.txt',sep="")
 #   ds_dt1 <- read.delim(ds_fn1,header=FALSE,sep = ",")
 #   colnames(ds_dt1) <- c("순위","문장","문서수")
 #   ds_dt <- ds_dt1
 #   ds_dt
 #   
 # }, ignoreNULL = FALSE)
  
  
  p1 <- eventReactive(input$update, {
    cur_dt <- as.Date(input$dt)
    dt <- format(cur_dt,"%Y-%m-%d")
    fn <- paste(kf_path, dt,'_',input$pr,'_', corpus_type1,'_TF.txt',sep="")
    dat <- read.delim(fn,header=FALSE,sep = "," )
    
    period <- as.numeric(input$pr)
    
    # 주요 단어 추출
    words <- as.vector(dat[1:cnt,2])
    print(words)
    
    result_mat <- matrix(ncol=cnt,nrow=dd)
    colnames(result_mat) <- words
    dt_list <- c()
    for(i in 0:(dd-1)){
      dt <- format(cur_dt-period*i,"%Y-%m-%d")
      dt_list <-c(dt_list, dt)
      fn <- paste(kf_path, dt,'_',input$pr,'_', corpus_type1,'_TF.txt',sep="")
      dat <- read.delim(fn,header=FALSE,sep = "," )
      colnames(dat) <- c("no","word","cnt")
      for(w in words){
        cnt_len <- length(which(dat[,"word"] == w))
        if(cnt_len == 0){
          result_mat[i+1,w] <- 0
        }
        else{
          result_mat[i+1,w] <- dat[dat[,"word"] == w,"cnt"]
        }
      }
    }
    
    dt_list <- as.Date(dt_list)
    result_df <- data.frame(result_mat)
    result_df <- cbind(dt_list,result_df)
    
    
    p <-plot_ly(
      result_df,
      type = 'scatter',
      mode = 'line+markers',
      line = list(width = 3)
    ) %>%    
      layout(title="이슈 키워드별 트렌드",
             xaxis = list(title = "날짜"),
             yaxis = list(title="키워드 언급 수"))
    
    
    col_names <- colnames(result_df)
    col_names <- col_names[-which(col_names == 'dt_list')]
    
    for (trace in col_names) {
      p <- p %>% add_trace(x = ~dt_list,y = as.formula(paste0("~`", trace, "`")), name = trace)
    }
    p
    
  }, ignoreNULL = FALSE)
  
  p2 <- eventReactive(input$update, {
    cur_dt <- as.Date(input$dt)
    dt <- format(cur_dt,"%Y-%m-%d")
    fn <- paste(kf_path, dt,'_', input$pr ,'_', corpus_type2,'_TF.txt',sep="")
    dat <- read.delim(fn,header=FALSE,sep = "," )
    words <- as.vector(dat[1:cnt,2])
    period <- as.numeric(input$pr)
    
    dat <- list()
    #attach(dat)
    
    result_mat <- matrix(ncol=cnt,nrow=dd)
    colnames(result_mat) <- words
    dt_list <- c()
    for(i in 0:(dd-1)){
      dt <- format(cur_dt-period*i,"%Y-%m-%d")
      dt_list <-c(dt_list, dt)
      fn <- paste(kf_path, dt,'_', input$pr ,'_', corpus_type2,'_TF.txt',sep="")
      dat <- read.delim(fn,header=FALSE,sep = "," )
      colnames(dat) <- c("no","word","cnt")
      for(w in words){
        cnt_len <- length(which(dat[,"word"] == w))
        if(cnt_len == 0){
          result_mat[i+1,w] <- 0
        }
        else{
          result_mat[i+1,w] <- dat[dat[,"word"] == w,"cnt"]
        }
      }
    }
    
    dt_list <- as.Date(dt_list)
    result_df <- data.frame(result_mat)
    result_df <- cbind(dt_list,result_df)
    
    
    p <-plot_ly(
      result_df,
      type = 'scatter',
      mode = 'line+markers',
      line = list(width = 3)
    ) %>%    
      layout(title="이슈 키워드별 트렌드",
             xaxis = list(title = "날짜"),
             yaxis = list(title="키워드 언급 수"))
    
    
    col_names <- colnames(result_df)
    col_names <- col_names[-which(col_names == 'dt_list')]
    
    for (trace in col_names) {
      p <- p %>% add_trace(x = ~dt_list,y = as.formula(paste0("~`", trace, "`")), name = trace)
    }
    p
  }, ignoreNULL = FALSE)
  
  kn1 <- eventReactive(input$update, {
    kn_fn <- paste(kn_path,input$dt,"_",input$pr,"_",corpus_type1,'_AA.txt',sep="")
    print(kn_fn)
  }, ignoreNULL = FALSE)
  
  kn2 <- eventReactive(input$update, {
    kn_fn <- paste(kn_path,input$dt,"_",input$pr,"_",corpus_type2,'_AA.txt',sep="")
    print(kn_fn)
  }, ignoreNULL = FALSE)
  
  kgt1 <- eventReactive(input$update,{
    kgt <- NULL
    dt_list <- NULL
    for(i in 0:(diff_cnt-1)){
      dt <- as.character(as.Date(input$dt) - as.numeric(input$pr) * (diff_cnt-i))
      dt_list <- c(dt_list,dt)
      fn <- paste(kgt_path,dt,'_', input$pr,"_",corpus_type1,'_KGT.txt',sep="")
      temp <- read.delim(fn, sep = ",")
      colnames(temp) <- c("category",dt)
      kgt <- cbind(kgt,temp[,dt])
      
      
    }
    row.names(kgt) <- temp[,"category"]
    
    kgt <- rbind(dt_list,kgt)
    
    row.names(kgt)[1] <- "date"
    kgt
  }, ignoreNULL = FALSE)
  
  kgt2 <- eventReactive(input$update,{
    kgt <- NULL
    dt_list <- NULL
    for(i in 0:(diff_cnt-1)){
      dt <- as.character(as.Date(input$dt) - as.numeric(input$pr) * (diff_cnt-i))
      dt_list <- c(dt_list,dt)
      fn <- paste(kgt_path,dt,'_', input$pr,"_",corpus_type2,'_KGT.txt',sep="")
      temp <- read.delim(fn, sep = ",")
      colnames(temp) <- c("category",dt)
      kgt <- cbind(kgt,temp[,dt])
      
      
    }
    row.names(kgt) <- temp[,"category"]
    
    kgt <- rbind(dt_list,kgt)
    
    row.names(kgt)[1] <- "date"
    kgt
  }, ignoreNULL = FALSE)
  
  
  output$indexing1 <- DT::renderDataTable({
    datasetInput1()
  }, selection = 'none',options = list(order = list(list(1, 'asc')),pageLength = 10, lengthChange = FALSE))
  
  output$indexing2 <- DT::renderDataTable({
    datasetInput2()
  }, selection = 'none',options = list(order = list(list(1, 'asc')),pageLength = 10, lengthChange = FALSE))
  
  output$kn_plot1 <- renderPlot({
    fn <- kn1()
    rulemat <- as.matrix(read.delim(fn,header=FALSE,sep=','))
    ruleg <- graph.edgelist(rulemat,directed=F) 
    
    
    V(ruleg)$size<- degree(ruleg)/ (0.25 * (n_rel/20))
    ruleg<-simplify(ruleg)
    
    set.seed(SEED)
    
    btw<-betweenness(ruleg)
    btw.score<-round(btw)+1
    btw.colors<-rev(heat.colors(max(btw.score)))
    V(ruleg)$color<-btw.colors[btw.score]
    plot(ruleg,vertex.label.family="NanumBarunGothic",vertex.label.cex=1.3)
  },deleteFile = FALSE)
  
  output$kn_plot2 <- renderPlot({
    fn <- kn2()
    rulemat <- as.matrix(read.delim(fn,header=FALSE,sep=','))
    ruleg <- graph.edgelist(rulemat,directed=F) 
    
    
    V(ruleg)$size<- degree(ruleg)/ (0.25 * (n_rel/20))
    ruleg<-simplify(ruleg)
    
    set.seed(SEED)
    
    
    btw<-betweenness(ruleg)
    btw.score<-round(btw)+1
    btw.colors<-rev(heat.colors(max(btw.score)))
    V(ruleg)$color<-btw.colors[btw.score]
    plot(ruleg,vertex.label.family="NanumBarunGothic",vertex.label.cex=1.3)
  },deleteFile = FALSE)
  
  
  # 키워드 그룹 플롯
  output$kgf_plot1 <- renderPlot({
    kgf1 <- kgf()
    result_mat <- NULL
    nc <- round(ncol(kgf1) /3)
    cnt <- 50
    
    
    for(i in 1:nc){
      #print(i)
      topic <- colnames(kgf1)[(i-1)*3+2]
      keyword_list <- kgf1[,(i-1)*3+2]
      cnt_list <- kgf1[,(i-1)*3+3]
      
      temp_mat <- cbind(keyword_list, cnt_list,topic)
      result_mat <- rbind(result_mat,temp_mat)
    }
    
    result_mat <- as.data.frame(result_mat)
    colnames(result_mat) <- c("키워드","빈도","그룹")
    result_mat <- result_mat[result_mat[,1] != "None",]
    result_mat <- result_mat[result_mat[,2] != "0.000",]
    result_mat <- result_mat[order(result_mat[,1]),]
    result_mat[,2] <- as.numeric(as.vector(result_mat[,2]))
    result_mat <- result_mat[order(-1*result_mat[,2]),]
    
    
    result_mat <- result_mat[1:cnt,]
    
    ggdotchart(result_mat, x="키워드", y="빈도",
               color=c("그룹"),                                # Color by groups
               sorting = "descending",                       # Sort value in descending order
               add = "segments",                             # Add segments from y = 0 to dots
               dot.size = 10,                                 # Large dot size
               label = "빈도",
               font.label = list(color = "white", size = 14,  vjust = 0.6),               
               xlab="",
               ylab="빈도",
               ggtheme = theme_pubr()                        # ggplot2 theme
    ) + theme(text = element_text(size=16)) + theme(legend.title = element_blank()) + guides(colour = guide_legend(nrow = 1))
  })
  
  
  # 키워드 그룹 플롯
  output$kgf_plot2 <- renderPlot({
    kgf2 <- kgf()
    
    result_mat <- NULL
    
    nc <- round(ncol(kgf2) /3)
    cnt <- 50
    for(i in 1:nc){
      #print(i)
      topic <- colnames(kgf2)[(i-1)*3+2]
      keyword_list <- kgf2[,(i-1)*3+2]
      cnt_list <- kgf2[,(i-1)*3+4]
      
      temp_mat <- cbind(keyword_list, cnt_list,topic)
      result_mat <- rbind(result_mat,temp_mat)
    }
    
    result_mat <- as.data.frame(result_mat)
    colnames(result_mat) <- c("키워드","빈도","그룹")
    result_mat <- result_mat[result_mat[,1] != "None",]
    result_mat <- result_mat[result_mat[,2] != "0.000",]
    result_mat <- result_mat[order(result_mat[,1]),]
    result_mat[,2] <- as.numeric(as.vector(result_mat[,2]))
    result_mat <- result_mat[order(-1*result_mat[,2]),]
    result_mat <- result_mat[1:cnt,]
    
    
    
    ggdotchart(result_mat, x="키워드", y="빈도",
               color=c("그룹"),                                # Color by groups
               sorting = "descending",                       # Sort value in descending order
               add = "segments",                             # Add segments from y = 0 to dots
               dot.size = 10,                                 # Large dot size
               label = "빈도",
               font.label = list(color = "white", size = 14,  vjust = 0.6),               
               xlab="",
               ylab="빈도",
               ggtheme = theme_pubr()                        # ggplot2 theme
    ) + theme(text = element_text(size=16)) + theme(legend.title = element_blank()) + guides(colour = guide_legend(nrow = 1))
  })
  
  
  output$kf_plot1 <- renderPlot({
    kf1 <- kf1()
    kf1 <- kf1[1:fcnt,1:3]
    
    ggdotchart(kf1, x = "단어", y = "TF",
               color = "blue",                                # Color by groups
               sorting = "descending",                       # Sort value in descending order
               add = "segments",                             # Add segments from y = 0 to dots
               rotate = TRUE,                                # Rotate vertically
               dot.size = 12,                                 # Large dot size
               label = "TF",
               font.label = list(color = "white", size = 14,  vjust = 0.6),               
               xlab="",
               ggtheme = theme_pubr()                        # ggplot2 theme
    ) + theme(text = element_text(size=16)) + theme(legend.title = element_blank())
  })
  
  
  output$kf_plot2 <- renderPlot({
    kf2 <- kf2()
    kf2 <- kf2[1:fcnt,1:3]
    
    ggdotchart(kf2, x = '단어', y = 'TF',
               color = c("blue"),                                # Color by groups
               sorting = "descending",                     
               add = "segments",                           
               rotate = TRUE,                              
               dot.size = 12,       
               label = "TF",
               font.label = list(color = "white", size = 14, vjust = 0.6),   
               xlab="",
               ggtheme = theme_pubr()   
               
    ) + theme(text = element_text(size=16))
    
  })
  
  
  output$kf_table1 <- renderTable({
    kf1 <- kf1()
    kf1[1:kf_num,]
  },width = "100%")
  output$kf_table2 <- renderTable({
    kf2 <- kf2()
    kf2[1:kf_num,]
  },width = "100%")
  
  
  output$ds_table1 <- renderTable({
    ds1()
  })
  
  output$ds_table2 <- renderTable({
    ds2()
  })
  
  output$ds_table3 <- renderTable({
    ds3()
  })
  
  output$ds_table4 <- renderTable({
    ds4()
  })
  
  output$issue_p1 <- renderPlotly({
    p1()
  })
  
  output$issue_p2 <- renderPlotly({
    p2()
  })
  output$kgf_table <- renderTable({
    kgf()
  })
  
  
  
  # 키워드 변화 분석
  
  output$kc_plot1 <- renderPlot({
    n_words <- 20
    kf_mat <- NULL
    g_thres <- 0.3
    diff_period <- dd
    
    # Collect Union Words
    
    k_list <- NULL
    
    dt <- as.character(as.Date(input$dt),"%Y-%m-%d")
    kf_fn <- paste(kf_path,dt,"_",input$pr,"_",corpus_type1 , "_", 'TF','.txt',sep="")
    kf_dt <- as.matrix(read.delim(kf_fn,header=FALSE,sep = ","))
    k_list <- union(k_list,kf_dt[1:n_words,2])
    
    result_mat <- matrix(nrow=length(k_list),ncol=4)
    result_mat <- as.data.frame(result_mat)
    
    result_mat[,1] <- k_list
    
    for(i in 1:length(k_list)){
      kf_cnt_list <- NULL
      for(j in 0:(diff_period-1)){
        k <- k_list[i]
        
        dt <- as.character(as.Date(input$dt)-j*as.numeric(input$pr),"%Y-%m-%d")
        kf_fn <- paste(kf_path,dt,"_",input$pr,"_",corpus_type1 , "_", 'TF','.txt',sep="")
        kf_dt <- as.matrix(read.delim(kf_fn,header=FALSE,sep = ","))
        
        kf_cnt <- kf_dt[kf_dt[,2] == k,3]
        if(length(kf_cnt) == 0)
          kf_cnt <- 0
        kf_cnt_list <- c(kf_cnt_list, kf_cnt)
      }
      
      # 가로축
      kf_cnt_list <- as.numeric(kf_cnt_list)
      result_mat[i,2] <- sum(kf_cnt_list)
      
      
      # 세로축
      result_mat[i,3] <- log((kf_cnt_list[1] + 1) / median(kf_cnt_list + 1),2)
      
      # 그룹 지정
      x <- length(kf_cnt_list):1
      y <- kf_cnt_list
      
      
      result_mat[i,4] <- sd(kf_cnt_list) / mean(kf_cnt_list+1)
    }
    
    print(result_mat)
    gidx1 <- which(result_mat[,4] > quantile(result_mat[,4],1-g_thres))
    
    result_mat[gidx1,4] <- 1
    result_mat[-gidx1,4] <- 2
    
    
    colnames(result_mat)[1] <- "keyword"
    colnames(result_mat)[2] <- "freq"
    colnames(result_mat)[3] <- "zscore"
    colnames(result_mat)[4] <- "group"
    
    
    p <- ggplot(result_mat) +  geom_point(aes(x=freq, y=zscore), size = 2, color = 'grey') + theme_classic(base_size = 20) + theme(legend.position="none")
    p <- p + geom_label_repel(
      aes(freq, zscore, fill = factor(group), label = result_mat[,1]),
      color = 'white', angle = 45, size=5,
      box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + xlab("키워드 빈도") + ylab("키워드 빈도 비율 변화")
    
    p <- p + geom_vline(xintercept=median(result_mat[,2]), linetype='dashed', color='gray', size=0.5)
    p <- p + geom_hline(yintercept=median(result_mat[,3]), linetype='dashed', color='gray', size=0.5)
    p <- p + theme(axis.title=element_text(size=15))
    p
  })
  
  
  output$kc_plot2 <- renderPlot({
    n_words <- 20
    kf_mat <- NULL
    g_thres <- 0.3
    diff_period <- dd
    
    # Collect Union Words
    
    k_list <- NULL
    
    dt <- as.character(as.Date(input$dt),"%Y-%m-%d")
    kf_fn <- paste(kf_path,dt,"_",input$pr,"_",corpus_type2 , "_", 'TF','.txt',sep="")
    kf_dt <- as.matrix(read.delim(kf_fn,header=FALSE,sep = ","))
    k_list <- union(k_list,kf_dt[1:n_words,2])
    
    result_mat <- matrix(nrow=length(k_list),ncol=4)
    result_mat <- as.data.frame(result_mat)
    
    result_mat[,1] <- k_list
    
    for(i in 1:length(k_list)){
      kf_cnt_list <- NULL
      for(j in 0:(diff_period-1)){
        k <- k_list[i]
        
        dt <- as.character(as.Date(input$dt)-j*as.numeric(input$pr),"%Y-%m-%d")
        kf_fn <- paste(kf_path,dt,"_",input$pr,"_",corpus_type2 , "_", 'TF','.txt',sep="")
        kf_dt <- as.matrix(read.delim(kf_fn,header=FALSE,sep = ","))
        
        kf_cnt <- kf_dt[kf_dt[,2] == k,3]
        if(length(kf_cnt) == 0)
          kf_cnt <- 0
        kf_cnt_list <- c(kf_cnt_list, kf_cnt)
      }
      
      # 가로축
      kf_cnt_list <- as.numeric(kf_cnt_list)
      result_mat[i,2] <- sum(kf_cnt_list)
      
      
      # 세로축
      result_mat[i,3] <- log((kf_cnt_list[1] + 1) / median(kf_cnt_list + 1),2)
      
      # 그룹 지정
      x <- length(kf_cnt_list):1
      y <- kf_cnt_list
      
      
      result_mat[i,4] <- sd(kf_cnt_list) / mean(kf_cnt_list+1)
    }
    
    print(result_mat)
    gidx1 <- which(result_mat[,4] > quantile(result_mat[,4],1-g_thres))
    
    result_mat[gidx1,4] <- 1
    result_mat[-gidx1,4] <- 2
    
    
    colnames(result_mat)[1] <- "keyword"
    colnames(result_mat)[2] <- "freq"
    colnames(result_mat)[3] <- "zscore"
    colnames(result_mat)[4] <- "group"
    
    
    p <- ggplot(result_mat) +  geom_point(aes(x=freq, y=zscore), size = 2, color = 'grey') + theme_classic(base_size = 20) + theme(legend.position="none")
    p <- p + geom_label_repel(
      aes(freq, zscore, fill = factor(group), label = result_mat[,1]),
      color = 'white', angle = 45, size=5,
      box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + xlab("키워드 빈도") + ylab("키워드 빈도 비율 변화")
    
    p <- p + geom_vline(xintercept=median(result_mat[,2]), linetype='dashed', color='gray', size=0.5)
    p <- p + geom_hline(yintercept=median(result_mat[,3]), linetype='dashed', color='gray', size=0.5)
    p <- p + theme(axis.title=element_text(size=15))
    p
  })
  
  
  
  output$kgt_plot1 <- renderPlot({
    kgt <- kgt1()
    kgt <- t(kgt)
  
    kgt <- data.frame(kgt)

    kgt_temp <- kgt
    kgt_temp <- kgt_temp[,-1]
    kgt_temp <- apply(kgt_temp, 2, as.integer)
    print(kgt_temp)
    kgt_sum <- apply(kgt_temp,1,sum)

    kgt_temp <- round(kgt_temp / kgt_sum,3)
    kgt[,-1] <- kgt_temp
    
    category <- colnames(kgt)
    
    kgt <- kgt%>% gather(key='category',value='value',category,-'date')
    
    kgt$value <- as.numeric(kgt$value)
    kgt$date <- as.Date(kgt$date)
    graph <- ggplot(kgt,aes(x = date, y = value, fill = category) )
    graph <- graph + geom_bar(stat='identity',width = as.numeric(input$pr)*5/7) 
    graph <- graph + geom_text(aes(x = date, label = paste0(value*100,'%')), size = 4, position = position_stack(vjust = 0.5)) 
    graph <- graph + theme_classic() 
    graph <- graph + theme(axis.text.x = element_text(angle=0, size=14)) + theme(axis.text.y = element_text(angle=0, size=14))
    #graph <- graph +scale_fill_brewer(palette = "Set3")
    graph <- graph + theme(legend.position="top") + theme(legend.title = element_blank()) + theme(legend.text=element_text(size=14))
    graph <- graph + theme(axis.title.y = element_text(size=16))
    graph <- graph + xlab("") + ylab("비율")
    graph <- graph + guides(col = guide_legend(nrow = 1),override.aes = list(alpha = 1))
    graph
  })
  
  
  output$kgt_plot2 <- renderPlot({
    kgt <- kgt2()
    kgt <- t(kgt)
    
    kgt <- data.frame(kgt)
    
    kgt_temp <- kgt
    kgt_temp <- kgt_temp[,-1]
    kgt_temp <- apply(kgt_temp, 2, as.integer)
    print(kgt_temp)
    kgt_sum <- apply(kgt_temp,1,sum)
    
    kgt_temp <- round(kgt_temp / kgt_sum,3)
    kgt[,-1] <- kgt_temp
    
    category <- colnames(kgt)
    
    kgt <- kgt%>% gather(key='category',value='value',category,-'date')
    
    kgt$value <- as.numeric(kgt$value)
    kgt$date <- as.Date(kgt$date)
    graph <- ggplot(kgt,aes(x = date, y = value, fill = category) )
    graph <- graph + geom_bar(stat='identity',width = as.numeric(input$pr)*5/7) 
    graph <- graph + geom_text(aes(x = date, label = paste0(value*100,'%')), size = 4, position = position_stack(vjust = 0.5)) 
    graph <- graph + theme_classic() 
    graph <- graph + theme(axis.text.x = element_text(angle=0, size=14)) + theme(axis.text.y = element_text(angle=0, size=14))
    #graph <- graph +scale_fill_brewer(palette = "Set3")
    graph <- graph + theme(legend.position="top") + theme(legend.title = element_blank()) + theme(legend.text=element_text(size=14))
    graph <- graph + theme(axis.title.y = element_text(size=16))
    graph <- graph + xlab("") + ylab("비율")
    graph <- graph + guides(col = guide_legend(nrow = 1),override.aes = list(alpha = 1))
    graph
  })
  output$kco_plot1 <- renderPlot({
    kco1()
  })
}

# Create Shiny app ----
shinyApp(ui, server)