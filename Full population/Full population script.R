setwd("E:/R projects/PP1---First-names-in-Belgium")

#Loading packages
library(readxl)
library(tidyr)
library(dplyr)


#importing data
sheetnames <- excel_sheets("First names full Belgian Population.xls")
data_men <- read_excel("First names full Belgian Population.xls", sheet="mannen", skip=1)
data_women <- read_excel("First names full Belgian Population.xls", sheet="vrouwen", skip=1)



#building a function that reads two columns of a sheet returning the data for an age group
read_data<- function(filename, column, gender, agegroup) {
  data <- filename[,column:(column+1)]
  colnames(data) <- c("Name", "Count")
  data$Gender <- gender
  data$Agegroup <- agegroup
  return(data)
}


#running the function fo all agegroups and genders
data <- read_data(data_men, 15, "Men", "<18y")
data <- bind_rows(data, read_data(data_men, 18, "Men", "18-64y"))
data <- bind_rows(data, read_data(data_men, 21, "Men", ">65y"))
data <- bind_rows(data, read_data(data_women, 15, "Women", "<18y"))
data <- bind_rows(data, read_data(data_women, 18, "Women", "18-64y"))
data <- bind_rows(data, read_data(data_women, 21, "Women", ">65y"))

#clean data file
data <- select(data, Gender, Agegroup, Name, Count)
write.csv(data, "Cleaned file - First names Belgian Population anno 2013.csv")


#calculation proportions
data %>%
  filter(Gender=="Women")%>%
  group_by(Agegroup) %>%
  summarise(sum=sum(Count))
