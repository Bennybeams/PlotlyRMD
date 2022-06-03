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
plotlyLine <- function (df,
                        #axisLabelRef='Salaries',
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
  
  #ticklabels <- seq(from=0, to=round(max(data[,..axisLabelRef])), by=5000)
  #ticktexts <- c(0,formatC(ticklabels, big.mark = " ",format = "d"))
  
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
    
    
  
  
  #hovertxt <- list()
  #if (hoverDate==T) {
  #  hovertxt[1] <- paste0( hoverPrefix,
  #                         colnames(df)[1],
  #                         hoverSep,
  #                         formatC((df[[1]]), 
  #                                 big.mark = milSep,
  #                                 decimal.mark=decSep,
  #                                 digits = digits,
  #                                 format = 'f'),
  #                         hoverSuffix)
  #} else {
  #  toto <- paste0( hoverPrefix,
  #                         colnames(df)[2],
  #                         hoverSep,
  #                         formatC((df[[2]]), 
  #                                 big.mark = milSep,
  #                                 decimal.mark=decSep,
  #                                 digits = digits,
  #                                 format = 'f'),
  #                         hoverSuffix)
  #}
  
  p <- plot_ly(data = df,
               name = colnames(df)[2],
               line = list(color = colorset[1]),
               #x = as.formula(paste0('~`', colnames(df)[1],'`')),
               #y = as.formula(paste0('~`', colnames(df)[2],'`')),
               type = 'scatter',
               mode = 'line')
  for(i in 2:ncol(df)){
    cat(sprintf(">>> Creating line for feature [%s]\n",colnames(df)[i]))
    p <- p %>% add_trace(x = as.formula(paste0('~`', colnames(df)[1],'`')),
                         y = as.formula(paste0('~`', colnames(df)[i],'`')),
                         name = colnames(df)[i],
                         hovertext=hovertxt[i-1],
                         #hovertext = hovertxt[i-1],
                         hovertemplate = '%{hovertext}<extra>toto</extra>',
                         line = list(color = colorset[i]))
  }
   
  #EMP.g1 <- EMP.g1 %>% layout(yaxis = list(separatethousands = T,tickformat = '.0f'))
  #EMP.g1 <- EMP.g1 %>% layout(yaxis=list(tickvals = ticklabels,
  #                                       ticktext = ticktexts))
  #EMP.g1 <- EMP.g1 %>% layout(yaxis = yaxis_template, 
  #                            xaxis = xaxis_template,
  #                            legend = list(orientation = 'h'))
  #EMP.g1 <- EMP.g1 %>% config(locale = 'fr') 
  return(p)
}

