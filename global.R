Sys.setlocale(category = "LC_ALL", locale = "cht")
library('shiny')
library('RMySQL')
library('plyr')
library('dplyr')
library('reshape2')
library('rdrop2')
library('data.table')
library('DT')
library('ggplot2')
library('plotly')
library('shinydashboard')
library('leaflet')
library('shinyjs')
library('V8')
library('shinythemes')
source('./src/revice.R')
options(scipen=999) #avoid scientific number type


new_old_customer_2016<-read.csv('./data/new_old_customer_2016.csv',stringsAsFactors = FALSE)
revenue_2016<-read.csv('./data/revenue_2016.csv',stringsAsFactors = FALSE)
customer<-read.csv('./data/customer.csv',stringsAsFactors = FALSE,encoding="UTF-8",sep=',',quote="")
revenue<-read.csv('./data/revenue.csv',stringsAsFactors = FALSE)
customer_rev_2017<-read.csv('./data/customer_rev_2017.csv',stringsAsFactors = FALSE,encoding="UTF-8",sep=',',quote="")
  
customer<-revice(customer)
customer_rev_2017<-revice(customer_rev_2017)

user <- 'user'
password <- 'password'