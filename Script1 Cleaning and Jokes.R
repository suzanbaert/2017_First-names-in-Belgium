library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)


#Importing files
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

#Reading all sheets and return one masterdatabase
data_girls <- data.frame()
for (i in sheetnames_girls) {
  data_girls <- bind_rows(data_girls,read_excelsheet(file_girls, i))
}


data_boys <- data.frame()
for (i in sheetnames_girls) {
  data_boys <- bind_rows(data_boys,read_excelsheet(file_boys, i))
}

data_girls$Gender <- "Girls"
data_boys$Gender <- "Boys"
data <- data.frame(bind_rows(data_girls, data_boys))
colnames(data)[1:2] <- c("Name", "Count")
data<- select(data, Region, Year, Gender, Name, Count)
head(data)


#writing clean datafile
write.csv(data, "First names in Belgium 1995-2016 cleaned.csv")


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
ggplot(data=Joke2, aes(Year, Count))+
  geom_line(color="#88398A", size=1)+
  labs(ylab="Number of occurences", title="Babies born with the name Joke in Belgium")+
  theme(plot.title = element_text(colour = "#88398A"))
