library(shiny)
library(dplyr)
library(stringi)
library(tidyr)

shinyServer(function(input, output, session) {

	output$contents <- renderTable({
	inFile <- input$file1
	if (is.null(inFile)){
    	return(data.frame())
    	} else {
	data<-read.table(inFile$datapath, fileEncoding="UTF-16LE",sep="\t",fill=T,header = TRUE)
	data<-data[complete.cases(data[,1:ncol(data)]),]
    data
	}
	})

	data <- reactive({
	inFile <- input$file1
	if (is.null(inFile)){
    	return(data.frame())
    	} else {
	data<-read.table(inFile$datapath, fileEncoding="UTF-16LE",sep="\t",fill=T,header = TRUE)
	data<-data[complete.cases(data[,1:ncol(data)]),]
    name<-unique(as.character(data$매체))
    name
	}
	})	

	observe({
		updateSelectInput(
		session,
		"input",
		choices=data())
	})

	data_t <- reactive({
	inFile <- input$file1
	if (is.null(inFile)){
    	return(data.frame())
    	} else {
	data<-read.table(inFile$datapath, fileEncoding="UTF-16LE",sep="\t",fill=T,header = TRUE)
	data<-data[complete.cases(data[,1:ncol(data)]),]
	}
	})

	data_t1 <- reactive({
	inFile1 <- input$file2
	if (is.null(inFile1)){
    	return(data.frame())
    	} else {
	data_t1<-read.table(inFile1$datapath, fileEncoding="UTF-16LE",sep="\t",fill=T,header = TRUE)
	data_t1<-data_t1[,c('소스.매체','사용자')]
	}
	})

	output$out3 <- renderPrint(input$input)

	output$table <- DT::renderDataTable(DT::datatable({
    data <- data_t()
    if (!is.null(input$input)) {
      data <- filter(data, 매체 %in% input$input) # data[data[1] == input$input,]
      data$순.구매수<-as.numeric(gsub("\\W", "", data[,c('순.구매수')]))
      data$수익<-as.numeric(gsub("\\W", "", data[,c('수익')]))
      data$세션<-as.numeric(gsub("\\W", "", data[,c('세션')]))
      data$새로운.세션..<-as.numeric(gsub("\\W", "", data[,c('새로운.세션..')]))/10000
      data$이탈률<-as.numeric(gsub("\\W", "", data[,c('이탈률')]))/10000
      data$bounce.num<-with(data, 세션*이탈률)
      data$new.num<-with(data, 세션*새로운.세션..)
      data <- data %>% summarise(순구매수=sum(순.구매수),거래수=sum(거래수),수익=sum(수익),세션=sum(세션),회원가입=sum(회원가입..목표.4.완료.수.),새로운.세션=sum(new.num)/sum(세션),이탈률=sum(bounce.num)/sum(세션))
    }
    data
  }))

	output$table1 <- DT::renderDataTable(DT::datatable({
    data1 <- data_t1()
    if (!is.null(input$input)) {
     	data1$num <- stri_count_regex(data1$소스.매체, "/")
		data1 <- subset(data1, num>0)
		data1 <- data1 %>% separate(소스.매체, c("소스", "media"), " / ")
		data1 <- filter(data1, media %in% input$input)
		data1$사용자 <- as.numeric(gsub("\\W", "", data1[,c('사용자')]))
		names(data1)[3]<-'user'
		data1 <- data1 %>% summarise(user=sum(user))
    }
    data1
  }))
})
