#------------------------------TESIS_CODE--------------------------------------

rm(list = ls()) #Clear the work space

#Load required packages

library(readxl) #Allows to read xls or xlsx files
library(openxlsx) #Read, write and edit xlsx files
library(xts) #eXtensible Time Series
library(data.table) #Fast aggregation of large data
library(tidytable) #A tidy interface to 'data.table'
library(lubridate) #Make dealing with dates a little easier
library(tidyverse) #Set of packages that work in harmony

#Defining the working directory

setwd("C:/Users/David Gomez/Desktop/TESIS/DATOS")

#Function to be able to discard stations without data

agg <- function(x) {
  if (sum(is.na(x)) >= 0.25 * length(x)) { #defined percentage
    return(NA)
  }
  else {
    return(sum(x, na.rm = TRUE))
  }
}

#Reading precipitation information

data_ppt <- read.xlsx("DatosIDW_221205.xlsx", "PTPM_CON")

#Sorting the precipitation data by dates

data_ppt <- as.xts(data_ppt[ , -1], 
                   order.by = as.Date(data_ppt[ , 1],
                                      format = "%Y-%m-%d"))

#Obtaining the annual precipitation of each station

data_y_ppt <- aggregate(data_ppt, by = year(index(data_ppt)), agg) %>% 
  data.frame()

#---------------------------------

datos <- read_excel("PPT.xlsx") %>% 
  as.data.table() %>%
  rename.(fecha = Fecha) %>%
  mutate.( year = as.factor(year(ymd(fecha)) )) %>%
  select.(-fecha) %>%
  as.data.table()

# Crear vectores con nombres de columnas y factores
cols_to_sum <- datos %>% select.(where(is.numeric)) %>% names()
resultado <- datos[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), 
                   .SDcols = cols_to_sum, by = year]
#--------------------------------

ppt <- transform(ppt, Fecha = as.Date(Fecha))

ppt$year <- as.integer(format(as.Date(ppt$Fecha), "%Y"))

result <- aggregate(. ~ year, data = ppt[, -1], sum, na.rm = T)

result2 <- aggregate()

years <- c(2000:2003) #Range of years

est_ppt <- read_excel("est_PPT.xlsx")

colnames <- c(as.character(ppt[1,2:ncol(ppt)]))

df <- data.frame(matrix(nrow = nrow(ppt), ncol = ncol(ppt)))

ppt$year <- as.integer(format(as.Date(df$Fecha), "%Y"))

colnames(df) <- colnames

rm(a)




ppt_16040050 <- list(1:366)
ppt_23185010 <- list()

df <- data.frame()
suma <- data.frame()
suma2 <- data.frame()
for (i in 2000:2002) {
  j <- as.character(i)
  
  year_ppt <- ppt %>% filter(grepl(j, Fecha))
  
  suma <- as.data.frame(t(colSums(year_ppt[,2:ncol(year_ppt)], na.rm = T)))
}

a <- as.character(2000)
year_ppt <- ppt %>% filter(grepl(a, Fecha)) 

year_ppt_2 <- ppt %>% filter(grepl('2001', Fecha))



suma <- colSums(year_ppt[,2:ncol(year_ppt)], na.rm = T)

suma_2 <- colSums(year_ppt_2[,2:ncol(year_ppt_2)], na.rm = T)

df[1,1] <- suma

for (i in years) {
  j <- as.character(i)
  ppt_16040050[[j]] <- rowSums()
}


for (i in seq_along(est_ppt)) {
  print(est_ppt)
}



