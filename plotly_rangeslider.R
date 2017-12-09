library(plotly)
library(quantmod)
library(dplyr)

setwd("C:/Users/Aeint Thet Ngon/Documents/Georgetown University/Data Viz/Takehome")

Sys.setenv("plotly_username"="aeint31")
Sys.setenv("plotly_api_key"="64YagDOY29TsffYihgol")
write.csv(ds, "ds1_plotly.csv")
#ds <- read.csv("plotly_rangesliderdata.csv")
ds <- read.csv("ds1_plotly.csv")
ds$X.1 <- NULL
ds$X.2 <- NULL
ds$X <- NULL
ds$totalwound <- NULL
p <- plot_ly(ds, x = ~date) %>%
  add_lines(y = ~totalkilled, name = "Number of people killed") %>%
#  add_lines(y = ~totalwound, name = "Number of people wounded") %>%
  add_lines(y = ~n, name = "Number number of attacks") %>%
  layout(
    title = "Number of wounded and killed",
    xaxis = list(
      rangeselector = list(
        buttons = list(
          list(
            count = 3,
            label = "3 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 6,
            label = "6 mo",
            step = "month",
            stepmode = "backward"),
          list(
            count = 1,
            label = "1 yr",
            step = "year",
            stepmode = "backward"),
          list(
            count = 1,
            label = "YTD",
            step = "year",
            stepmode = "todate"),
          list(step = "all"))),
      
      rangeslider = list(type = "date")),
    
    yaxis = list(title = "Count"))

api_create(p, filename = "rangeslider_killedandtotalattacks")

