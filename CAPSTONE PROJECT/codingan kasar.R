awal <- as.character(Sys.Date()-50)
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

Data_df <- list(IHSG,PTPP,MNCN,CTRA,BBNI,SIDO,TLKM)
Data_df
df<-IHSG

Data_df <- cbind(stock,
                 rbind(IHSG%>%tail(1), PTPP%>%tail(1), MNCN%>%tail(1),
                       CTRA%>%tail(1), BBNI%>%tail(1), SIDO%>%tail(1),
                       TLKM%>%tail(1)),
                 cor)
Data_df <- Data_df[order(Data_df$cor, decreasing = TRUE),]
Data_df[2,1]
#lihat korelasi data
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

core<-cor.test(IHSG$Close, PTPP$Close)$p.value
core$p.value

library(quantmod)
library(plotly)
hovertxt <- Map(function(x,y)paste0(x, ":", y), names(df), df)
hovertxt <- Reduce(function(x, y)paste0(x, "<br&;gt;", y), hovertxt)
#plot
plot_ly(df, x = ~Date, xend = ~Date, hoverinfo = "none",
        color = ~Close>Open, colors = c("#00b386","#ff6666")) %>%
  add_segments(y = ~Low, yend = ~High, line = list(width = 1, color = "black")) %>%
  add_segments(y = ~Open, yend = ~Close, line = list(width = 3)) %>%
  add_markers(y = ~(Low + High)/2, hoverinfo = "text",
              text = hovertxt, marker = list(color = "transparent")) %>%
  layout(showlegend = FALSE,
         yaxis = list(title = "Harga", domain = c(0, 0.9)),
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
         plot_bgcolor = "#f2f2f2")
