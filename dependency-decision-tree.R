
suppressPackageStartupMessages({
  if (!require('shiny')){install.packages('shiny')}; library(shiny)
  if (!require('pastecs')){install.packages('pastecs')}; library(pastecs)
  if (!require('rpart')){install.packages('rpart')}; library(rpart)
  if (!require('dplyr')){install.packages('dplyr')}; library(dplyr)
  if (!require('Hmisc')){install.packages('Hmisc')}; library(Hmisc)
  if (!require('randomForest')){install.packages('randomForest')}; library(randomForest)
  if (!require('hydroGOF')){install.packages('hydroGOF')}; library(hydroGOF)
  if (!require('sparkline')){install.packages('sparkline')}; library(sparkline)
  if (!require('partykit')){install.packages('partykit')}; library(partykit)visNetwork
  if (!require('visNetwork')){install.packages('visNetwork')}; library(visNetwork)
  if (!require('DT')){install.packages('DT')}; library(DT)
})