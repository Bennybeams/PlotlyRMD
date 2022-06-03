library(flexdashboard)
library(ggplot2)
library(data.table)
library(DT)
library(plotly)
library(ggwordcloud)
library(gganimate)
library(scales)
library(kableExtra)
library(dichromat)
library(ggwordcloud)
library(gganimate)
library(wordcloud2) 
library(dplyr)
library(collapsibleTree)
library(tidyr)
library(lubridate)
library(stringr)

path <- "S:/donnees/public/data.gouv.fr"
ECON1016 <- readRDS(sprintf("%s/ECON1016.rds",path))
EMPL1019 <- readRDS(sprintf("%s/EMPL1019.rds",path))
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7", "#345687")

# General settings
# Templates
HTemplate <- '%{x} | %{y:.0f}<extra></extra>'
xaxis_template <- list(title = '',
                       zeroline = TRUE,
                       showline = TRUE)
yaxis_template <- list(title = '',
                       zeroline = TRUE,
                       showline = TRUE,
                       showticklabels = TRUE,
                       showgrid = TRUE)
# Plots with plotly

# Line chart
smartPlotly.line <- function (df,
                        axisLabelRef='Salaries',
                        legendOrient = 'h',
                        colorset=ispfPalette, 
                        digits=0, 
                        milSep=' ', 
                        decSep='.', 
                        hoverPrefix='',
                        hoverSuffix='',
                        hoverSep=c(' | ',' : '),
                        hoverDate=T,
                        hoverFeat=T){
  maxValues <- lapply(colnames(df[,2:ncol(df)]), FUN=function(x){
    Y = list()
    Y = append(Y,max(df[,..x]))
  })
  maxValue <- 0

  hovertxt <- lapply(colnames(df[,2:ncol(df)]), FUN=function(x){
    message(x)
    if (hoverDate==T && hoverFeat==T){
      message("date & feat")
      Y <- paste0(hoverPrefix,
                  df[[1]],
                  hoverSep[1],
                  x,
                  hoverSep[2],
                  formatC((df[[x]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = 'f'),
                  hoverSuffix)
    }else if(hoverDate==T && hoverFeat==F){
      message("date without feat")
      Y <- paste0(hoverPrefix,
                  df[[1]],
                  hoverSep[2],
                  formatC((df[[x]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = 'f'),
                  hoverSuffix)
    }else if(hoverDate==F && hoverFeat==T){
      message("feat without date")
      Y <- paste0(hoverPrefix,
                  x,
                  hoverSep[2],
                  formatC((df[[x]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = 'f'),
                  hoverSuffix)
    }else{
      message("no date & no feat")
      Y <- paste0(hoverPrefix,
                  formatC((df[[x]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = 'f'),
                  hoverSuffix)
      }
    })
    
  p <- plot_ly(data = df,
               name = colnames(df)[2],
               line = list(color = colorset[1]),
               type = 'scatter',
               mode = 'line')
  for(i in 2:ncol(df)){
    maxValue <- max(maxValue,as.numeric(maxValues[[i-1]]))
    cat(sprintf(">>> Creating line for feature [%s]\n",colnames(df)[i]))
    p <- p %>% add_trace(x = as.formula(paste0('~`', colnames(df)[1],'`')),
                         y = as.formula(paste0('~`', colnames(df)[i],'`')),
                         name = colnames(df)[i],
                         text=hovertxt[[i-1]],
                         hoverinfo='text',
                         line = list(color = colorset[i]))
  }
  
  ticklabels <- seq(from=0, to=round(maxValue), by=maxValue/5)
  ticktexts <- c(0,formatC(ticklabels, big.mark = milSep, format = "d"))
  p <- p %>% layout(yaxis=list(tickvals = ticklabels,
                                          ticktext = ticktexts))
  p <- p %>% layout(yaxis = yaxis_template, 
                            xaxis = xaxis_template,
                            legend = list(orientation = 'h'))
  p <- p %>% config(locale = 'fr') 
  return(p)
}

