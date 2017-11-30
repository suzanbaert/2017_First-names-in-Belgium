library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)


#Importing files
setwd("E:/R projects/01 Belgie namen/No More Joke's - First names in Belgium")
file_girls <- "First names girls 1995-2016.xls"
sheetnames_girls <- excel_sheets(file_girls) [1:22]
file_boys <- "First names boys 1995-2016.xls"
sheetnames_boys <- excel_sheets(file_girls) [1:22]


#Trial set to build up the function later on
# input1995 <- read_excel(file, "1995")
# data1995<- input1995[,2:3]
# data1995$Year <- 1995L
# data1995$Region <- "Total Belgium"
# 
# input1996 <- read_excel(file, "1996")
# data1996<- input1996[,2:3]
# data1996$Year <- 1996L
# data1996$Region <- "Total Belgium"
# 
# testdata<- bind_rows(data1995, data1996)



#A function that read a sheet(representing a year), and returns Total Belgium data
read_excelsheet <- function (file, sheetname) {
  input <- read_excel(file, sheetname)
  data <- input[,2:3]
  data$Year <- as.integer(sheetname)
  data$Region <- "Total Belgium"
  return(data)
}

#Reading all sheets 
data_g <- data.frame()
for (i in sheetnames_girls) {
  data_g <- bind_rows(data_g,read_excelsheet(file_girls, i))
}

data_b <- data.frame()
for (i in sheetnames_boys) {
  data_b <- bind_rows(data_b,read_excelsheet(file_boys, i))
}

#Combine girls and boys in 1 database
data_g$Gender <- "Girls"
data_b$Gender <- "Boys"
data <- data.frame(bind_rows(data_g, data_b))

#Renaming columns and clean-up
colnames(data)[1:2] <- c("Name", "Count")
data<- select(data, Region, Year, Gender, Name, Count)
head(data)
rm(data_b)
rm(data_g)


#Searching for Joke
Joke <- filter(data, Name=="Joke")
Joke %>% 
  group_by (Year) %>% 
  summarise(Count)

#Adding empty rows until 2016
Joke <- Joke %>%
  rbind(list("Total Belgium", 2013, "Girls", "Joke", 0)) %>% 
  rbind(list("Total Belgium", 2014, "Girls", "Joke", 0)) %>% 
  rbind(list("Total Belgium", 2015, "Girls", "Joke", 0)) %>% 
  rbind(list("Total Belgium", 2016, "Girls", "Joke", 0)) 



#Plotting Jokes
ggplot(data=Joke, aes(Year, Count))+
  geom_line(color="#88398A", size=1)+
  labs(y="Number of occurences", title="Girls born with the name Joke in Belgium")+
  theme(plot.title = element_text(colour = "#88398A"))


#Plotting Mathilde
ggplot(data=subset(data_girls, Name=="Mathilde"), aes(Year, Count))+
  geom_line(color="#88398A")+
  labs(yl="Number of occurences", title="Girls born with the name Mathilde in Belgium")+
  theme(plot.title = element_text(colour = "#88398A"))





#Plotting all girls names
data_girls<-subset(data, Gender=="Girls")
ggplot(data_girls, aes(Year, Count, shape=Name))+
  geom_line(color="#FFFEFF", show.legend=FALSE)

ggplot(data_girls)+
  geom_line(aes(Year, Count, shape=Name), color="#FFFEFF", show.legend=FALSE)



#Double plots for Mathilde
ggplot(data_girls, aes(Year, Count, shape=Name))+
  geom_line(color="#CFCCCF", show.legend=FALSE)+
  geom_line(data=subset(data_girls, Name=="Mathilde"),color="#88398A", size=1)+
  labs(yl="Number of occurences", title="Girls born with the name Mathilde in Belgium")+
  theme(plot.title = element_text(colour = "#88398A"))

#Double plots for Joke
ggplot(data_girls, aes(Year, Count, shape=Name))+
  geom_line(color="#CFCCCF", show.legend=FALSE)+
  geom_line(data=subset(data_girls, Name=="Joke"),color="#88398A", size=1)+
  labs(yl="Number of occurences", title="Girls born with the name Joke in Belgium")+
  theme(plot.title = element_text(colour = "#88398A"))


#Suzan and variants
suzan<-filter(data_girls, Name=="Suzan"|Name=="Suzanne")
ggplot(data_girls, aes(Year, Count, shape=Name))+
  geom_line(color="#CFCCCF", show.legend=FALSE)+
  geom_line(data=suzan,color="#88398A", size=1)+
  labs(yl="Number of occurences", title="Girls born with the name Suzan in Belgium")+
  theme(plot.title = element_text(colour = "#88398A"))
