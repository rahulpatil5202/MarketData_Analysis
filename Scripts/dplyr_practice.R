install.packages("WDI")
install.packages("dplyr")
install.packages("reshape")

library(WDI)
library(dplyr)
library(reshape)

#wdi_def <- read.csv("E:/datasets/WDI_Definition and Source.csv", header = T)
dat <- read.csv("E:/datasets/WDI_Data.csv", header = T)

head(dat)
str(dat)
summary(dat)

#Unique Indicators series name
unique(dat$ï..Series.Name)

#filter single coulmn condition
dat[dat$Country.Name == "Afghanistan", ]

#filter mutiple column conditions 
dat[dat$Country.Name == "India" & dat$ï..Series.Name == "Surface area (sq. km)",]

#filter mutiple column conditions
dat[(dat$Country.Name == "India" | dat$Country.Name == "Pakistan") & dat$ï..Series.Name == "Poverty headcount ratio at national poverty lines (% of population)",]

#Sort on column value
arrange(dat, dat$Country.Code)

#sort on multiple columns
tail(arrange(dat, dat$Country.Code, dat$Country.Name))

#Select Vs rename

select(dat, Country.Code, Country.Name) # output of two coulmns as selected

colnames(dat)

rename(dat, Series.Name = ï..Series.Name) #Notice it changed column name to LHS of = and also
#returned all other columns
rename(iris, petal_length = Petal.Length)

#Select distinct values
distinct(select(dat, ï..Series.Name))

distinct(select(dat, Country.Code, ï..Series.Name))

head(dat)

#Unpivoting Data

melt_dat <- melt(dat, id.vars = c("Country.Code", "Country.Name", "Series.Code", "ï..Series.Name"))

melt_dat #Notice that it melted other variables not menioned in melt command and created
#two new columns for all other variable and their count.
#fantastically reshaped in R

