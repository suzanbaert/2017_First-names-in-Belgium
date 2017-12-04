setwd("E:/R projects/PP1---First-names-in-Belgium")

#Loading packages
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)


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
data <- read_data(data_men, 15, "Men", "minus18")
data <- bind_rows(data, read_data(data_men, 18, "Men", "18to64"))
data <- bind_rows(data, read_data(data_men, 21, "Men", "plus65"))
data <- bind_rows(data, read_data(data_women, 15, "Women", "minus18"))
data <- bind_rows(data, read_data(data_women, 18, "Women", "18to64"))
data <- bind_rows(data, read_data(data_women, 21, "Women", "plus65"))

#clean data file
data <- select(data, Gender, Agegroup, Name, Count)
data <- na.omit(data)
write.csv(data, "Cleaned file - First names Belgian Population anno 2013.csv")

#clean up workspace
rm(data_women)
rm(data_men)


#getting ready for plotting
#need a wide dataframe to enable comparisons
data_wide <- data %>% 
  spread(Agegroup, Count)
data_wide_nilNA <- na.omit(data_wide)


#ggplot
ggplot(data = subset(data_wide_nilNA, Gender=="Women"), aes(x=minus18, y=plus65, label=Name))+
  geom_point()+
  geom_text(nudge_x=800)
