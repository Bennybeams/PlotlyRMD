library(flexdashboard)
library(data.table)
library(DT)
library(plotly)
library(ggwordcloud)
library(gganimate)
library(scales)
library(dichromat)
library(ggwordcloud)
library(wordcloud2) 
library(dplyr)
library(collapsibleTree)
library(tidyr)
library(stringr)
library(plyr)

path <- "S:/donnees/public/data.gouv.fr"
ECON1016 <- readRDS(sprintf("%s/ECON1016.rds",path))
TOUR1083 <- readRDS(sprintf("%s/TOUR1083.rds",path))
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
smartPlotly.line <- function (dt,
                              x=1,
                              ticksRound=-1,
                              legendOrient = 'h',
                              colorset=c("blue","green","red","orange","tail"),
                              digitExclude=c(),
                              digits=0, 
                              milSep=' ', 
                              decSep='.', 
                              hoverPrefix='',
                              hoverSuffix='',
                              hoverSep=c(' | ',' : '),
                              hoverX=T,
                              hoverY=T,
                              hoverTimeFormat=NA,
                              smoothLine=c(T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T,T),
                              size=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                              style=c("solid","solid","solid","solid","solid","solid","solid","solid","solid","solid","solid","solid","solid"),
                              chartLocale="fr"){
    
  dt <- dt %>% relocate(x)
  hovertxt <- lapply(colnames(dt[,2:ncol(dt)]), FUN=function(feat){
    if (feat %in% digitExclude) digits <- 0
    if (which(colnames(dt) == feat) %in% digitExclude) digits <- 0
    
    if (hoverX==T && hoverY==T && !is.na(hoverTimeFormat)){
      Y <- paste0(hoverPrefix,
                  format(dt[[x]],hoverTimeFormat),
                  hoverSep[1],
                  feat,
                  hoverSep[2],
                  formatC((dt[[feat]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = 'f'),
                  hoverSuffix)
    }else if(hoverX==T && hoverY==F && !is.na(hoverTimeFormat)){
      Y <- paste0(hoverPrefix,
                  format(dt[[x]],hoverTimeFormat),
                  hoverSep[2],
                  formatC((dt[[feat]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = 'f'),
                  hoverSuffix)
    }else if (hoverX==T && hoverY==T && is.na(hoverTimeFormat)){
      Y <- paste0(hoverPrefix,
                  dt[[x]],
                  hoverSep[1],
                  feat,
                  hoverSep[2],
                  formatC((dt[[feat]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = 'f'),
                  hoverSuffix)
    }else if(hoverX==T && hoverY==F && is.na(hoverTimeFormat)){
      Y <- paste0(hoverPrefix,
                  dt[[x]],
                  hoverSep[2],
                  formatC((dt[[feat]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = 'f'),
                  hoverSuffix)
    }else if(hoverX==F && hoverY==T){
      Y <- paste0(hoverPrefix,
                  feat,
                  hoverSep[2],
                  formatC((dt[[feat]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = 'f'),
                  hoverSuffix)
    }else{
      Y <- paste0(hoverPrefix,
                  formatC((dt[[feat]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = 'f'),
                  hoverSuffix)
      }
    })
    
  p <- plot_ly(data = dt,
               type = 'scatter',
               mode = 'line')
  for(i in 2:ncol(dt)){
    if (smoothLine[i]==T) lineShape = list(shape = 'spline', width = size[i-1], dash = style[i-1], color=colorset[i-1])
    else lineShape = list(color=colorset[1], width = size[i-1], dash = style[i-1])
    cat(sprintf(">>> Creating line for feature [%s] : size = %s | color = %s | dash = %s\n",colnames(dt)[i], size[i], colorset[i], style[i]))
    p <- p %>% add_trace(x = as.formula(paste0('~`', colnames(dt)[x],'`')),
                         y = as.formula(paste0('~`', colnames(dt)[i],'`')),
                         name = colnames(dt)[i],
                         text=hovertxt[[i-1]],
                         hoverinfo='text',
                         line = lineShape)
  }
  
  maxValue=max(dt[,-..x])
  minValue=min(dt[,-..x])
  dataOrder <- floor(log10(abs(max(dt[,-..x]))))
  ticksRound <- round(abs(maxValue)/5, digits = -(dataOrder-1))
  ticklabels <- round_any(seq(from=round(minValue), to=round_any(maxValue, ticksRound), by=ticksRound),ticksRound)
  ticktexts <- c(formatC(ticklabels, big.mark = milSep, format = "f", digits = digits))
  p <- p %>% layout(yaxis=list(tickvals = ticklabels,
                                          ticktext = ticktexts))
  p <- p %>% layout(yaxis = yaxis_template, 
                    xaxis = xaxis_template,
                    legend = list(orientation = legendOrient))
  if (minValue==0) {
    cat(">>> WARNING : min value = 0, y axis range start at 0\n")
    p <- p %>% layout(yaxis=list(range=list(0,maxValue)))
  }
  p <- p %>% config(locale = chartLocale) 
  return(p)
}

# Bar chart
smartPlotly.bar <- function (dt,
                             x=1,
                             ticksRound=-1,
                             legendOrient = 'h',
                             colorset=c("blue","green","red","orange","tail"),
                             digitExclude=c(),
                             digits=2, 
                             milSep=' ', 
                             decSep=',', 
                             hoverPrefix='',
                             hoverSuffix='',
                             hoverSep=c(' | ',' : '),
                             hoverX=T,
                             hoverY=T,
                             hoverTimeFormat=NA,
                             inBarInfo=c(T),
                             hoverInfo=c(T),
                             chartLocale="fr",
                             legendformat="f",
                             axisformat="s"){
  
  dt <- dt %>% relocate(x)
  hovertxt <- lapply(colnames(dt[,2:ncol(dt)]), FUN=function(feat){
    if (feat %in% digitExclude) digits <- 0
    if (which(colnames(dt) == feat) %in% digitExclude) digits <- 0
    
    if (hoverX==T && hoverY==T && !is.na(hoverTimeFormat)){
      Y <- paste0(hoverPrefix,
                  format(dt[[1]],hoverTimeFormat),
                  hoverSep[1],
                  feat,
                  hoverSep[2],
                  formatC((dt[[feat]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = legendformat),
                  hoverSuffix)
    }else if(hoverX==T && hoverY==F && !is.na(hoverTimeFormat)){
      Y <- paste0(hoverPrefix,
                  format(dt[[1]],hoverTimeFormat),
                  hoverSep[2],
                  formatC((dt[[feat]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = legendformat),
                  hoverSuffix)
    }else if (hoverX==T && hoverY==T && is.na(hoverTimeFormat)){
      Y <- paste0(hoverPrefix,
                  dt[[1]],
                  hoverSep[1],
                  feat,
                  hoverSep[2],
                  formatC((dt[[feat]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = legendformat),
                  hoverSuffix)
    }else if(hoverX==T && hoverY==F && is.na(hoverTimeFormat)){
      Y <- paste0(hoverPrefix,
                  dt[[1]],
                  hoverSep[2],
                  formatC((dt[[feat]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = legendformat),
                  hoverSuffix)
    }else if(hoverX==F && hoverY==T){
      Y <- paste0(hoverPrefix,
                  feat,
                  hoverSep[2],
                  formatC((dt[[feat]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = legendformat),
                  hoverSuffix)
    }else{
      Y <- paste0(hoverPrefix,
                  formatC((dt[[feat]]), 
                          big.mark = milSep,
                          decimal.mark=decSep,
                          digits = digits,
                          format = legendformat),
                  hoverSuffix)
    }
  })
  
  p <- plot_ly(data = dt,
               type = 'bar')
  for(i in 2:ncol(dt)){
    cat(sprintf(">>> Creating line for feature [%s] : color = %s \n",colnames(dt)[i], colorset[i]))
    if ((is.na(inBarInfo[i]) || inBarInfo[i]==T) && (is.na(hoverInfo[i]) || hoverInfo[i]==T)) {
      cat(">>> Inbar info and hover info \n")
      p <- p %>% add_trace(x = as.formula(paste0('~`', colnames(dt)[1],'`')),
                           y = as.formula(paste0('~`', colnames(dt)[i],'`')),
                           name = colnames(dt)[i],
                           text=hovertxt[[i-1]],
                           hoverinfo='text')
    }else if ((!is.na(inBarInfo[i]) || inBarInfo[i]==F) && (is.na(hoverInfo[i]) || hoverInfo[i]==T)) {
      cat(">>> No inbar info but hover info \n")
      p <- p %>% add_trace(x = as.formula(paste0('~`', colnames(dt)[1],'`')),
                           y = as.formula(paste0('~`', colnames(dt)[i],'`')),
                           name = colnames(dt)[i],
                           text='',
                           hoverinfo="text",
                           hovertext = hovertxt[[i-1]])
      
    }else if ((is.na(inBarInfo[i]) || inBarInfo[i]==T) && (!is.na(hoverInfo[i]) || hoverInfo[i]==F)) {
      cat(">>> Inbar info but no hover info \n")
      p <- p %>% add_trace(x = as.formula(paste0('~`', colnames(dt)[1],'`')),
                           y = as.formula(paste0('~`', colnames(dt)[i],'`')),
                           name = colnames(dt)[i],
                           text=hovertxt[[i-1]],
                           hoverinfo='text')
      
    }else if ((!is.na(inBarInfo[i]) || inBarInfo[i]==F) && (is.na(hoverInfo[i]) || hoverInfo[i]==F)) {
      cat(">>> no info at all \n")
      p <- p %>% add_trace(x = as.formula(paste0('~`', colnames(dt)[1],'`')),
                           y = as.formula(paste0('~`', colnames(dt)[i],'`')),
                           name = colnames(dt)[i])
    }
  }
  
  maxValue=max(dt[,-1])
  minValue=min(dt[,-1])
  if (maxValue>999){
    cat(">>> WARNING : max value > 999, applying thousan separator milsep\n")
    dataOrder <- floor(log10(abs(max(dt[,-1]))))
    ticksRound <- round(abs(maxValue)/5, digits = -(dataOrder-1))
    ticklabels <- round_any(seq(from=round(minValue), to=round_any(maxValue, ticksRound), by=ticksRound),ticksRound)
    ticktexts <- c(formatC(ticklabels, big.mark = milSep, format = axisformat, digits = digits))
    p <- p %>% layout(yaxis=list(tickvals = ticklabels,
                                 ticktext = ticktexts))
  }
  p <- p %>% layout(yaxis = yaxis_template, 
                    xaxis = xaxis_template,
                    legend = list(orientation = legendOrient))
  if (minValue==0) {
    cat(">>> WARNING : min value = 0, y axis range start at 0\n")
    p <- p %>% layout(yaxis=list(range=list(0,maxValue)))
  }
  p <- p %>% config(locale = chartLocale) 
  return(p)
}

