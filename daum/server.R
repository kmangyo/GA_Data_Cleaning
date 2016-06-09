library(shiny)
library(dplyr)

shinyServer(function(input, output, session) {

#	output$contents <- renderTable({
#	inFile1 <- input$file1
#	if (is.null(inFile1)){
#    	return(data.frame())
#    	} else {
#	data<-read.table(inFile1$datapath, fileEncoding="UTF-16LE",sep="\t",fill=T,header = TRUE)
#	data<-data[complete.cases(data[,1:ncol(data)]),]
#    data
#	}
#	})

#	data <- reactive({
#	inFile1 <- input$file1
#	if (is.null(inFile1)){
#    	return(data.frame())
#    	} else {
#	data<-read.table(inFile1$datapath, fileEncoding="UTF-16LE",sep="\t",fill=T,header = TRUE)
#	data<-data[complete.cases(data[,1:ncol(data)]),]
#    name<-unique(as.character(data$광고.콘텐츠))
#    name
#	}
#	})	

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

#	observe({
#		updateSelectInput(
#		session,
#		"input",
#		choices=data())
#	})

	data_t <- reactive({
	inFile1 <- input$file1
	if (is.null(inFile1)){
    	return(data.frame())
    	} else {
	data<-read.table(inFile1$datapath, fileEncoding="UTF-16LE",sep="\t",fill=T,header = TRUE)
	data<-data[complete.cases(data[,1:ncol(data)]),]
  names(data)<-c('name','n_conv','conv','revenue','session','reg','n_session','bounce')
  data
	}
	})

#	output$out3 <- renderPrint(input$input)

#	output$table1 <- DT::renderDataTable(DT::datatable({
#    data <- data_t()
#    if (!is.null(input$input)) {
#      data <- filter(data, 광고.콘텐츠 %in% input$input) # data[data[1] == input$input,]
#      data$순.구매수<-as.numeric(gsub("\\W", "", data[,c('순.구매수')]))
#      data$거래수<-as.numeric(gsub("\\W", "", data[,c('거래수')]))
#      data$수익<-as.numeric(gsub("\\W", "", data[,c('수익')]))
#      data$세션<-as.numeric(gsub("\\W", "", data[,c('세션')]))
#      data$새로운.세션..<-as.numeric(gsub("\\W", "", data[,c('새로운.세션..')]))/10000
#      data$이탈률<-as.numeric(gsub("\\W", "", data[,c('이탈률')]))/10000
#      data$bounce.num<-with(data, 세션*이탈률)
#      data$new.num<-with(data, 세션*새로운.세션..)
#      data <- data %>% summarise(순구매수=sum(순.구매수),거래수=sum(거래수),수익=sum(수익),세션=sum(세션),회원가입=sum(회원가입..목표.4.완료.수.),새로운.세션=sum(new.num)/sum(세션),이탈률=sum(bounce.num)/sum(세션))
#    }
#    data
#  }))

	output$table2 <- DT::renderDataTable(DT::datatable({
    data_t2 <- data_t()
    list_c <- list_c()
    if (!is.null(input$file1)&!is.null(input$file2)) {
      data_t2 <- left_join(list_c, data_t2, by=c('name'))
      #data_t2 <- data_t2[2:ncol(data_t2)][is.na(data[2:ncol(data_t2)])]<-0
    }
    data_t2
  }))

	output$table3 <- DT::renderDataTable(DT::datatable({
    data_t3 <- data_t()
    list_c <- list_c()
    if (!is.null(input$file1)&!is.null(input$file2)) {
      data_t3 <- anti_join(data_t3, list_c, by=c('name'))
      data_t3 <- filter(data_t3, name!=c('daument_01'))
      data_t3 <- filter(data_t3, name!=c(''))
      #data_t3 <- data_t3[2:ncol(data_t3)][is.na(data_t3[2:ncol(data_t3)])]<-0
      data_t3$n_conv<-as.numeric(gsub("\\W", "", data_t3[,c('n_conv')]))
      data_t3$conv<-as.numeric(gsub("\\W", "", data_t3[,c('conv')]))
      data_t3$revenue<-as.numeric(gsub("\\W", "", data_t3[,c('revenue')]))
      data_t3$session<-as.numeric(gsub("\\W", "", data_t3[,c('session')]))
      data_t3$n_session<-as.numeric(gsub("\\W", "", data_t3[,c('n_session')]))/10000
      data_t3$bounce<-as.numeric(gsub("\\W", "", data_t3[,c('bounce')]))/10000
      data_t3$bounce.num<-with(data_t3, session*bounce)
      data_t3$new.num<-with(data_t3, session*n_session)
      data_t3 <- data_t3 %>% summarise(n_conv=sum(n_conv),conv=sum(conv),revenue=sum(revenue),session=sum(session),reg=sum(reg),bounce.num=sum(bounce.num),new.num=sum(new.num))
      data_t3 <- mutate(data_t3, n_session=new.num/session, bounce=bounce.num/session)
      data_t3 <- data_t3[c(1:5,8,9)]
    }
    data_t3
  }))

})
