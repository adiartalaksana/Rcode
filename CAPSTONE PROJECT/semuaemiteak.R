awal <- as.character(Sys.Date()-100)
akhir <- as.character(Sys.Date())
t1 <- ISOdate(as.numeric(substr(awal,1,4)),
              as.numeric(substr(awal,6,7)),
              as.numeric(substr(awal,9,10)),hour=0)
t2 <- ISOdate(as.numeric(substr(akhir,1,4)),
              as.numeric(substr(akhir,6,7)),
              as.numeric(substr(akhir,9,10)),hour=0)

#list saham yang digunakan
stock <- c("^JKSE","AGRO.JK","AMAR.JK","ARTO.JK","BABP.JK","BACA.JK","BBCA.JK",
           "BBHI.JK","BBKP.JK","BBMD.JK","BBNI.JK","BBRI.JK","BBSI.JK","BBTN.JK",
           "BBYB.JK","BCIC.JK","BDMN.JK","BEKS.JK","BGTG.JK","BINA.JK","BJBR.JK",
           "BJTM.JK","BKSW.JK","BMAS.JK","BMRI.JK","BNBA.JK","BNGA.JK","BNII.JK",
           "BNLI.JK","BRIS.JK","BSIM.JK","BTPN.JK","BTPS.JK","BVIC.JK","SDRA.JK",
           "DNAR.JK","INPC.JK","MAYA.JK","MEGA.JK","NISP.JK","NOBU.JK","PNBN.JK",
           "PNBS.JK")

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

list_saham<-list()
for (i in (1:length(stock))){
  list_saham[[i]]<-read.csv(url[i])
  list_saham[[i]]$Date <- as.Date(list_saham[[i]]$Date, "%Y-%m-%d")
}

cor<-c(rep(0,length(stock)))
for (i in(1:length(stock))){
  cor[i]<-cor(list_saham[[1]]$Close, list_saham[[i]]$Close)
}

Data_df<-data.frame()
for (i in (1:length(stock))){
  Data_df <- rbind(Data_df, list_saham[[i]]%>%tail(1))
}

Data_df <- cbind(stock,
                 Data_df,
                 cor,
                 abs(cor))


Data_df <- Data_df[order(Data_df$`abs(cor)`, decreasing = TRUE),]











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
