
suppressPackageStartupMessages({
  if (!require('shiny')){install.packages('shiny')}; library(shiny)
  if (!require('pastecs')){install.packages('pastecs')}; library(pastecs)
  if (!require('rpart')){install.packages('rpart')}; library(rpart)
  if (!require('party')){install.packages('party')}; library(party)
  if (!require('dplyr')){install.packages('dplyr')}; library(dplyr)
  if (!require('Hmisc')){install.packages('Hmisc')}; library(Hmisc)
  if (!require('randomForest')){install.packages('randomForest')}; library(randomForest)
  
  if (!require('devtools')){install.packages("devtools")}; library(devtools) 
  if (!require('hydroTSM')){install_github("hzambran/hydroTSM")}; library('hydroTSM') 
  if (!require('hydroGOF')){install_github("hzambran/hydroGOF")}; library('hydroGOF')
  
  #if (!require('hydroGOF')){install.packages('hydroGOF')}; library(hydroGOF)
  if (!require('sparkline')){install.packages('sparkline')}; library(sparkline)
  if (!require('partykit')){install.packages('partykit')}; library(partykit)
  if (!require('visNetwork')){install.packages('visNetwork')};library(visNetwork)
  if (!require('DT')){install.packages('DT')};library(DT)
  if (!require('tidyr')){install.packages('tidyr')};library(tidyr)
  #install_github("hzambran/hydroTSM");install_github("hzambran/hydroGOF")
})

