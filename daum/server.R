library(shiny)
library(dplyr)
library(scales)

shinyServer(function(input, output, session) {

	list_c <- reactive({
	inFile2 <- input$file2
	if (is.null(inFile2)){
    	return(data.frame())
    	} else {
	list<-read.csv(inFile2$datapath)
  names(list)<-c('name')
  list
	}
	})	

	data_t <- reactive({
	inFile1 <- input$file1
	if (is.null(inFile1)){
    	return(data.frame())
    	} else {
	data<-read.table(inFile1$datapath, fileEncoding="UTF-16LE",sep="\t",fill=T,header = TRUE)
	data<-data[complete.cases(data[,1:ncol(data)]),]
  names(data)<-c('name','PU','conv','revenue','session','reg','n_session','bounce')
  data
	}
	})

  data_u <- reactive({
  inFile3 <- input$file3
  if (is.null(inFile3)){
      return(data.frame())
      } else {
  data<-read.table(inFile3$datapath, fileEncoding="UTF-16LE",sep="\t",fill=T,header = TRUE)
  names(data)<-c('name','user')
  # data<-data[1:nrow(data)-4,]
  data
  }
  })

	output$table2 <- DT::renderDataTable(DT::datatable({
    data_t2 <- data_t()
    list_c <- list_c()
    data_u <- data_u()
    if (!is.null(input$file1)&!is.null(input$file2)) {
      data_t2 <- left_join(list_c, data_t2, by=c('name'))
      data_t2 <- left_join(data_t2, data_u, by=c('name'))
      data_t2 <- data_t2[c(1,9,2:8)]
    }
    data_t2
  }))

	output$table3 <- DT::renderDataTable(DT::datatable({
    data_t3 <- data_t()
    list_c <- list_c()
    data_u <- data_u()
    if (!is.null(input$file1)&!is.null(input$file2)) {
      data_t3 <- anti_join(data_t3, list_c, by=c('name'))
      data_t3 <- filter(data_t3, name!=c('daument_01'))
      data_t3 <- filter(data_t3, name!=c(''))
      data_t3 <- left_join(data_t3, data_u, by=c('name'))
      # data_t3 <- data_t3[2:ncol(data_t3)][is.na(data_t3[2:ncol(data_t3)])]<-0
      data_t3$PU<-as.numeric(gsub("\\W", "", data_t3[,c('PU')]))
      data_t3$conv<-as.numeric(gsub("\\W", "", data_t3[,c('conv')]))
      data_t3$revenue<-as.numeric(gsub("\\W", "", data_t3[,c('revenue')]))
      data_t3$session<-as.numeric(gsub("\\W", "", data_t3[,c('session')]))
      data_t3$user<-as.numeric(gsub("\\W", "", data_t3[,c('user')]))
      data_t3$n_session<-as.numeric(gsub("\\W", "", data_t3[,c('n_session')]))/10000
      data_t3$bounce<-as.numeric(gsub("\\W", "", data_t3[,c('bounce')]))/10000
      data_t3$bounce.num<-with(data_t3, session*bounce)
      data_t3$new.num<-with(data_t3, session*n_session)
      data_t3 <- data_t3 %>% summarise(PU=sum(PU),conv=sum(conv),revenue=sum(revenue),session=sum(session),reg=sum(reg),bounce.num=sum(bounce.num),new.num=sum(new.num), user=sum(user))
      data_t3 <- mutate(data_t3, n_session=new.num/session, bounce=bounce.num/session)
      data_t3$n_session <- percent(data_t3$n_session)
      data_t3$bounce <- percent(data_t3$bounce)
      data_t3 <- data_t3[c(8,1:5,9:10)]
    }
    data_t3
  }))
})
