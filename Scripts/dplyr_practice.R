# install.packages("WDI")
# install.packages("dplyr")
# install.packages("reshape")
install.packages("tidyverse")
library(tid)
library(WDI)
library(dplyr)
library(reshape)

#wdi_def <- read.csv("E:/datasets/WDI_Definition and Source.csv", header = T)
dat <- read_csv("E:/datasets/WDI_Data.csv", header = T, check.names = T, stringsAsFactors = F) #Check.names for 
#auto adjusting wrong variable names

head(dat)
str(dat)
summary(dat)

colnames(dat)
dat <- dplyr::rename(dat, Series.Name = ï..Series.Name) #Notice it changed column name to LHS of = and also


#Unique Indicators series name
unique(dat$Series.Name)

#filter single coulmn condition
dat[dat$Country.Name == "Afghanistan", ]

#filter mutiple column conditions 
dat[dat$Country.Name == "India" & dat$Series.Name == "Surface area (sq. km)",]

#filter mutiple column conditions
dat[(dat$Country.Name == "India" | dat$Country.Name == "Pakistan") & (dat$Series.Name == "Poverty headcount ratio at national poverty lines (% of population)"),]

#Sort on column value
arrange(dat, dat$Country.Code)

#sort on multiple columns
tail(arrange(dat, dat$Country.Code, dat$Country.Name))

#Select Vs rename

select(dat, Country.Code, Country.Name) # output of two coulmns as selected

#returned all other columns
#Note dplyr rename gets masked when you load reshape library

dplyr::rename(iris, petal_length = Petal.Length)

#Select distinct values
distinct(select(dat, Series.Name))

distinct(select(dat, Country.Code, Series.Name))

head(dat)
dim(dat)

#Unpivoting Data

melt_dat <- melt(dat, id.vars = c("Country.Code", "Country.Name", "Series.Code", "Series.Name"))

melt_dat #Notice that it melted other variables not menioned in melt command and created
#two new columns for all other variable and their count.
#fantastically reshaped in R

#Use Group by and summarise for pivoting functions

str(melt_dat)

#change all classes to continuous variables and rename column

melt_dat$Country.Code <-  as.character(melt_dat$Country.Code)
melt_dat$Country.Name <-  as.character(melt_dat$Country.Name)
melt_dat$Series.Code <- as.character(melt_dat$Series.Code)
melt_dat$Series.Name <- as.character(melt_dat$Series.Name)

melt_dat <- dplyr::rename(melt_dat, Yearx=variable)

melt_dat$Yearx <- as.character(melt_dat$Yearx)
melt_dat$value <- as.numeric(melt_dat$value)


#group by and summarise melt_dat
melt_dat %>% filter(!is.na(Series.Name) | Series.Name != " ")%>%
  group_by(Series.Name, Yearx, value)%>%
  summarise(count=n(), total=sum(value), avg=mean(value))

head(melt_dat$Series.Name)

summary(melt_dat$Series.Name)
