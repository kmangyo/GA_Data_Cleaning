library(shiny)
library(dplyr)

shinyServer(function(input, output, session) {

	list_c <- reactive({
	inFile2 <- input$file2
	if (is.null(inFile2)){
    	return(data.frame())
    	} else {
	list<-read.csv(inFile2$datapath)
  names(list)<-c('광고.콘텐츠')
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
  data
	}
	})

  data_u <- reactive({
  inFile3 <- input$file3
  if (is.null(inFile3)){
      return(data.frame())
      } else {
  data<-read.table(inFile3$datapath, fileEncoding="UTF-16LE",sep="\t",fill=T,header = TRUE)
  data
  }
  })

	output$table2 <- DT::renderDataTable(DT::datatable({
    data_t2 <- data_t()
    list_c <- list_c()
    data_u <- data_u()
    if (!is.null(input$file1)&!is.null(input$file2)) {
      data_t2 <- left_join(list_c, data_t2, by=c('광고.콘텐츠'))
      data_t2 <- left_join(data_t2, data_u, by=c('광고.콘텐츠'))
      data_t2 <- data_t2[c(1,11,3:9)]
    }
    data_t2
  }))

	output$table3 <- DT::renderDataTable(DT::datatable({
    data_t3 <- data_t()
    list_c <- list_c()
    data_u <- data_u()
    if (!is.null(input$file1)&!is.null(input$file2)) {
      data_t3 <- anti_join(data_t3, list_c, by=c('광고.콘텐츠'))
      data_t3 <- filter(data_t3, 광고.콘텐츠!=c('daument_01'))
      data_t3 <- filter(data_t3, 광고.콘텐츠!=c(''))
      data_t3 <- left_join(data_t3, data_u, by=c('광고.콘텐츠'))
      data_t3$순.구매수<-as.numeric(gsub("\\W", "", data_t3[,c('순.구매수')]))
      data_t3$거래수<-as.numeric(gsub("\\W", "", data_t3[,c('거래수')]))
      data_t3$수익<-as.numeric(gsub("\\W", "", data_t3[,c('수익')]))
      data_t3$세션<-as.numeric(gsub("\\W", "", data_t3[,c('세션')]))
      data_t3$사용자<-as.numeric(gsub("\\W", "", data_t3[,c('사용자')]))
      data_t3$새로운.세션..<-as.numeric(gsub("\\W", "", data_t3[,c('새로운.세션..')]))/10000
      data_t3$이탈률<-as.numeric(gsub("\\W", "", data_t3[,c('이탈률')]))/10000
      data_t3$bounce.num<-with(data_t3, 세션*이탈률)
      data_t3$new.num<-with(data_t3, 세션*새로운.세션..)
      data_t3 <- data_t3 %>% summarise(순.구매수=sum(순.구매수),거래수=sum(거래수),수익=sum(수익),세션=sum(세션),회원가입=sum(회원가입..목표.4.완료.수.),bounce.num=sum(bounce.num),new.num=sum(new.num), 사용자=sum(사용자))
      data_t3 <- mutate(data_t3, 새로운.세션=new.num/세션, 이탈률=bounce.num/세션)
      data_t3 <- data_t3[c(8,1:5,9:10)]
    }
    data_t3
  }))

})
