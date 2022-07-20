library(shiny)
library(tidyverse)
library(plotly)

server <- function(input, output, session) {

  Data_df <- reactive({
    withProgress(message = 'Preparing stocks dataset', value = 0, {
      awal <- as.character(Sys.Date()-100)
      akhir <- as.character(Sys.Date())
      t1 <- ISOdate(as.numeric(substr(awal,1,4)),
                    as.numeric(substr(awal,6,7)),
                    as.numeric(substr(awal,9,10)),hour=0)
      t2 <- ISOdate(as.numeric(substr(akhir,1,4)),
                    as.numeric(substr(akhir,6,7)),
                    as.numeric(substr(akhir,9,10)),hour=0)
      
      #list saham yang digunakan
      stock <- c("^JKSE","PTPP.JK","MNCN.JK","CTRA.JK","BBNI.JK","SIDO.JK","TLKM.JK")
      
      #crawling historis saham
      url <- c()
      for (i in (1:length(stock))){
        url[i] <- paste("https://query1.finance.yahoo.com/v7/finance/download/",
                        stock[i],
                        "?period1=",
                        as.integer(t1),
                        "&period2=",
                        as.integer(t2),
                        "&interval=1d&events=history",
                        sep="")
      }
      IHSG <- read.csv(url[1])
      PTPP <- read.csv(url[2])
      MNCN <- read.csv(url[3])
      CTRA <- read.csv(url[4])
      BBNI <- read.csv(url[5])
      SIDO <- read.csv(url[6])
      TLKM <- read.csv(url[7])
      
      IHSG$Date <- as.Date(IHSG$Date, "%Y-%m-%d")
      PTPP$Date <- as.Date(PTPP$Date, "%Y-%m-%d")
      MNCN$Date <- as.Date(MNCN$Date, "%Y-%m-%d")
      CTRA$Date <- as.Date(CTRA$Date, "%Y-%m-%d")
      BBNI$Date <- as.Date(BBNI$Date, "%Y-%m-%d")
      SIDO$Date <- as.Date(SIDO$Date, "%Y-%m-%d")
      TLKM$Date <- as.Date(TLKM$Date, "%Y-%m-%d")
      
      cor<-c()
      cor[1]<-cor(IHSG$Close, IHSG$Close)
      cor[2]<-cor(IHSG$Close, PTPP$Close)
      cor[3]<-cor(IHSG$Close, MNCN$Close)
      cor[4]<-cor(IHSG$Close, CTRA$Close)
      cor[5]<-cor(IHSG$Close, BBNI$Close)
      cor[6]<-cor(IHSG$Close, SIDO$Close)
      cor[7]<-cor(IHSG$Close, TLKM$Close)
      
      #lihat sig korelasi data
      p_val<-c()
      p_val[1]<-if(cor.test(IHSG$Close, IHSG$Close)$p.value > 0.05){"Tidak Berkorelasi"}else{"Berkorelasi"}
      p_val[2]<-if(cor.test(IHSG$Close, PTPP$Close)$p.value > 0.05){"Tidak Berkorelasi"}else{"Berkorelasi"}
      p_val[3]<-if(cor.test(IHSG$Close, MNCN$Close)$p.value > 0.05){"Tidak Berkorelasi"}else{"Berkorelasi"}
      p_val[4]<-if(cor.test(IHSG$Close, CTRA$Close)$p.value > 0.05){"Tidak Berkorelasi"}else{"Berkorelasi"}
      p_val[5]<-if(cor.test(IHSG$Close, BBNI$Close)$p.value > 0.05){"Tidak Berkorelasi"}else{"Berkorelasi"}
      p_val[6]<-if(cor.test(IHSG$Close, SIDO$Close)$p.value > 0.05){"Tidak Berkorelasi"}else{"Berkorelasi"}
      p_val[7]<-if(cor.test(IHSG$Close, IHSG$Close)$p.value > 0.05){"Tidak Berkorelasi"}else{"Berkorelasi"}
      
      
      Data_df <- cbind(stock,
                       rbind(IHSG%>%tail(1), PTPP%>%tail(1), MNCN%>%tail(1),
                             CTRA%>%tail(1), BBNI%>%tail(1), SIDO%>%tail(1),
                             TLKM%>%tail(1)),
                       cor,p_val)
      Data_df <- Data_df[order(Data_df$cor, decreasing = TRUE),]
    })
  })
  
  Data <- reactive({
    withProgress(message = 'Preparing stocks dataset', value = 0, {
      awal <- as.character(Sys.Date()-100)
      akhir <- as.character(Sys.Date())
      t1 <- ISOdate(as.numeric(substr(awal,1,4)),
                    as.numeric(substr(awal,6,7)),
                    as.numeric(substr(awal,9,10)),hour=0)
      t2 <- ISOdate(as.numeric(substr(akhir,1,4)),
                    as.numeric(substr(akhir,6,7)),
                    as.numeric(substr(akhir,9,10)),hour=0)
      
      #list saham yang digunakan
      stock <- c("^JKSE","PTPP.JK","MNCN.JK","CTRA.JK","BBNI.JK","SIDO.JK","TLKM.JK")
      
      #crawling historis saham
      url <- c()
      for (i in (1:length(stock))){
        url[i] <- paste("https://query1.finance.yahoo.com/v7/finance/download/",
                        stock[i],
                        "?period1=",
                        as.integer(t1),
                        "&period2=",
                        as.integer(t2),
                        "&interval=1d&events=history",
                        sep="")
      }
      IHSG <- read.csv(url[1])
      PTPP <- read.csv(url[2])
      MNCN <- read.csv(url[3])
      CTRA <- read.csv(url[4])
      BBNI <- read.csv(url[5])
      SIDO <- read.csv(url[6])
      TLKM <- read.csv(url[7])
      
      IHSG$Date <- as.Date(IHSG$Date, "%Y-%m-%d")
      PTPP$Date <- as.Date(PTPP$Date, "%Y-%m-%d")
      MNCN$Date <- as.Date(MNCN$Date, "%Y-%m-%d")
      CTRA$Date <- as.Date(CTRA$Date, "%Y-%m-%d")
      BBNI$Date <- as.Date(BBNI$Date, "%Y-%m-%d")
      SIDO$Date <- as.Date(SIDO$Date, "%Y-%m-%d")
      TLKM$Date <- as.Date(TLKM$Date, "%Y-%m-%d")
      
      Data<-list(IHSG,PTPP,MNCN,CTRA,BBNI,SIDO,TLKM)
    })
  })
  
  IHSG_df <- reactive({
    withProgress(message = 'Preparing IHSG dataset', value = 0, {
    awal <- as.character(Sys.Date()-100)
    akhir <- as.character(Sys.Date())
    t1 <- ISOdate(as.numeric(substr(awal,1,4)),
                  as.numeric(substr(awal,6,7)),
                  as.numeric(substr(awal,9,10)),hour=0)
    t2 <- ISOdate(as.numeric(substr(akhir,1,4)),
                  as.numeric(substr(akhir,6,7)),
                  as.numeric(substr(akhir,9,10)),hour=0)
    
    #list saham yang digunakan
    stock <- "^JKSE"
    #crawling historis saham
    url <- paste("https://query1.finance.yahoo.com/v7/finance/download/",
                    stock,
                    "?period1=",
                    as.integer(t1),
                    "&period2=",
                    as.integer(t2),
                    "&interval=1d&events=history",
                    sep="")
    IHSG_df <- read.csv(url)
    })
  })
  

  
##### rendering output #####
  output$kode_saham_1 <- renderText({
    Data_df()[2,1]
  })
  output$kode_saham_2 <- renderText({
    Data_df()[3,1]
  })
  output$kode_saham_3 <- renderText({
    Data_df()[4,1]
  })
  output$kode_saham_4 <- renderText({
    Data_df()[5,1]
  })
  output$kode_saham_5 <- renderText({
    Data_df()[6,1]
  })
  output$kode_saham_6 <- renderText({
    Data_df()[7,1]
  })
  
  
  
  output$harga_saham_1 <- renderText({
    paste("Rp.",format(Data_df()$Close[2], nsmall=0, big.mark="."))
  })
  output$harga_saham_2 <- renderText({
    paste("Rp.",format(Data_df()$Close[3], nsmall=0, big.mark="."))
  })
  output$harga_saham_3 <- renderText({
    paste("Rp.",format(Data_df()$Close[4], nsmall=0, big.mark="."))
  })
  output$harga_saham_4 <- renderText({
    paste("Rp.",format(Data_df()$Close[5], nsmall=0, big.mark="."))
  })
  output$harga_saham_5 <- renderText({
    paste("Rp.",format(Data_df()$Close[6], nsmall=0, big.mark="."))
  })
  output$harga_saham_6 <- renderText({
    paste("Rp.",format(Data_df()$Close[7], nsmall=0, big.mark="."))
  })
  
  
  output$korelasi_saham_1 <- renderText({
    paste("Korelasi :",round(Data_df()$cor[2],2),"(",Data_df()$p_val[2],")")
  })
  output$korelasi_saham_2 <- renderText({
    paste("Korelasi :",round(Data_df()$cor[3],2),"(",Data_df()$p_val[3],")")
  })
  output$korelasi_saham_3 <- renderText({
    paste("Korelasi :",round(Data_df()$cor[4],2),"(",Data_df()$p_val[4],")")
  })
  output$korelasi_saham_4 <- renderText({
    paste("Korelasi :",round(Data_df()$cor[5],2),"(",Data_df()$p_val[5],")")
  })
  output$korelasi_saham_5 <- renderText({
    paste("Korelasi :",round(Data_df()$cor[6],2),"(",Data_df()$p_val[6],")")
  })
  output$korelasi_saham_6 <- renderText({
    paste("Korelasi :",round(Data_df()$cor[7],2),"
          (",Data_df()$p_val[7],")")
  })
  
  
  output$total_covid_death <- renderText({
    format(sum(latest_covid_df()$total_deaths, na.rm = T), nsmall=0, big.mark=".")
  })
  
  output$sum_people_fully_vaccinated <- renderText({
    x <- vaccinations_df() %>% 
      drop_na(people_fully_vaccinated) %>% 
      group_by(location) %>% 
      filter(date == max(date)) %>% 
      summarize(sum_people_fully_vaccinated = sum(people_fully_vaccinated)) %>%
      # group_by() %>%
      summarize(sum_people_fully_vaccinated = sum(sum_people_fully_vaccinated))
    
    format(x$sum_people_fully_vaccinated, nsmall=0, big.mark=".")
  })
  
  output$sum_total_vaccinations <- renderText({
    x <- vaccinations_df() %>% 
      drop_na(total_vaccinations) %>% 
      group_by(location) %>% 
      filter(date == max(date)) %>% 
      summarize(sum_total_vaccinations = sum(total_vaccinations)) %>% 
      # group_by() %>% 
      summarize(sum_total_vaccinations = sum(sum_total_vaccinations))
    
    format(x$sum_total_vaccinations, nsmall=0, big.mark=".")
  })
  
  output$slider_year <- renderUI({
    data <- vaccinations_df() %>%
      drop_na(total_vaccinations)
    
    sliderInput(
      "slider_year", 
      "", 
      min=min(data$date), 
      max=max(data$date), 
      value=max(data$date), 
      animate=animationOptions(loop = TRUE, interval = 1000)
    )
  })
  
  output$plot_IHSG <- renderPlotly({
    df<-IHSG_df()
    hovertxt <- Map(function(x,y)paste0(x, ":", y), names(df), df)
    hovertxt <- Reduce(function(x, y)paste0(x, "<br&;gt;", y), hovertxt)
    #plot
    plot_ly(df, x = ~Date, xend = ~Date, hoverinfo = "none",
            color = ~Close>Open, colors = c("#00b386","#ff6666")) %>%
      add_segments(y = ~Low, yend = ~High, line = list(width = 1, color = "white")) %>%
      add_segments(y = ~Open, yend = ~Close, line = list(width = 3)) %>%
      add_markers(y = ~(Low + High)/2, hoverinfo = "text",
                  text = hovertxt, marker = list(color = "transparent")) %>%
      layout(showlegend = FALSE,
             yaxis = list(title = "Harga Penutup", domain = c(0, 0.9)),
             annotations = list(
               list(xref = "paper", yref = "paper",
                    x = 0, y = 1, showarrow = F,
                    xanchor = "left", yanchor = "top",
                    align = "left",
                    text = paste0("<b>IHSG</b>")),    
               list(xref = "paper", yref = "paper",
                    x = 0.75, y = 1, showarrow = F,
                    xanchor = "left", yanchor = "top",
                    align = "left",
                    text = paste(range(df$Date), collapse = " : "),
                    font = list(size = 8))),
             paper_bgcolor = '#222327', 
             plot_bgcolor = '#222327',
             font = list(
               color = '#bdbdbd'
             ))
  })
  
  output$plot_PTPP<- renderPlotly({
    Data_PTPP<-Data()[[2]]
    Data_PTPP %>%
      plot_ly(
        x = ~Date, 
        y = ~Close,
        mode = "lines+markers"
      ) %>% 
      layout(
        height = 200,
        paper_bgcolor = '#222327', 
        plot_bgcolor = '#222327', 
        font = list(
          color = '#bdbdbd'
        )
      ) %>% 
      highlight(on = "plotly_click", off = "plotly_doubleclick")
  })
  
  output$plot_MNCN<- renderPlotly({
    Data_MNCN<-Data()[[3]]
    Data_MNCN %>%
      plot_ly(
        x = ~Date, 
        y = ~Close,
        mode = "lines+markers"
      ) %>% 
      layout(
        height = 200,
        paper_bgcolor = '#222327', 
        plot_bgcolor = '#222327', 
        font = list(
          color = '#bdbdbd'
        )
      ) %>% 
     highlight(on = "plotly_click", off = "plotly_doubleclick")
  })
  output$plot_CTRA<- renderPlotly({
    Data_CTRA<-Data()[[4]]
    Data_CTRA %>%
      plot_ly(
        x = ~Date, 
        y = ~Close,
        mode = "lines+markers"
      ) %>% 
      layout(
        height = 200,
        paper_bgcolor = '#222327', 
        plot_bgcolor = '#222327', 
        font = list(
          color = '#bdbdbd'
        )
      ) %>% 
      highlight(on = "plotly_click", off = "plotly_doubleclick")
  })
  output$plot_BBNI<- renderPlotly({
    Data_BBNI<-Data()[[5]]
    Data_BBNI %>%
      plot_ly(
        x = ~Date, 
        y = ~Close,
        mode = "lines+markers"
      ) %>% 
      layout(
        height = 200,
        paper_bgcolor = '#222327', 
        plot_bgcolor = '#222327', 
        font = list(
          color = '#bdbdbd'
        )
      ) %>% 
      highlight(on = "plotly_click", off = "plotly_doubleclick")
  })
  output$plot_SIDO<- renderPlotly({
    Data_SIDO<-Data()[[6]]
    Data_SIDO %>%
      plot_ly(
        x = ~Date, 
        y = ~Close,
        mode = "lines+markers"
      ) %>% 
      layout(
        height = 200,
        paper_bgcolor = '#222327', 
        plot_bgcolor = '#222327', 
        font = list(
          color = '#bdbdbd'
        )
      ) %>% 
      highlight(on = "plotly_click", off = "plotly_doubleclick")
  })
  output$plot_TLKM<- renderPlotly({
    Data_TLKM<-Data()[[7]]
    Data_TLKM %>%
      plot_ly(
        x = ~Date, 
        y = ~Close,
        mode = "lines+markers"
      ) %>% 
      layout(
        height = 200,
        paper_bgcolor = '#222327', 
        plot_bgcolor = '#222327', 
        font = list(
          color = '#bdbdbd'
        )
      ) %>% 
      highlight(on = "plotly_click", off = "plotly_doubleclick")
  })
  
}