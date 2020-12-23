##Setting the data enviroment
rm(list = ls()) #cleaning data enviroment 
options(scipen = 999) #setting scipen

########PREMIER LEAGUE & CHAMPIONSHIP###########

#loading the dataset
setwd("C:/Users/Omer Gralnik/Desktop/Datasets-Premier League/Football Arrests")
getwd()
list.files()

pl <- read.csv("Premier League and Championship arrests 1987_1994_Csv.csv", header = T)
#view(pl)

#making amends to the data frame, designing it to the way we want
colnames(pl) <- c('City','87_88_attendance','87_88_arrests','87_88_arr_per',
                  '88_89_attendance','88_89_arrests','88_89_arr_per',
                  '89_90_attendance','89_90_arrests','89_90_arr_per',
                  '90_91_attendance','90_91_arrests','90_91_arr_per',
                  '91_92_attendance','91_92_arrests','91_92_arr_per',
                  '92_93_attendance','92_93_arrests','92_93_arr_per',
                  '93_94_attendance','93_94_arrests','93_94_arr_per')
pl <- subset(pl, City != 'City'&City != 'Total')
pl[] <- lapply(pl, function(x) if(is.factor(x)) factor(x) else x)

#checking the stracture of the data
str(pl) 
names(pl)

#converting the variables into numeric ones
pl[,c(2,5,8,11,14,17,20)] <-  sapply(pl[,c(2,5,8,11,14,17,20)],
                                     function(x){gsub(pattern = ",",replacement = "", x)})
pl[,c(4,7,10,13,16,19,22)] <-  sapply(pl[,c(4,7,10,13,16,19,22)],
                                     function(x){gsub(pattern = "%",replacement = "", x)})
pl[,c(3,6,9,12,15,18,21)] <-  sapply(pl[,c(3,6,9,12,15,18,21)],
                                      function(x){gsub(pattern = ",",replacement = "", x)})
pl[2:22] <- sapply(pl[2:22], function(x) as.numeric(as.character(x)))
pl$City <- as.character(pl$City)


#checking the new data frame summary and structure
summary(pl)
str(pl)

#install.packages("data.table") #RUN ONLY ONCE!!
library(data.table)

#we want the data to be in a data table form
dt <- as.data.table(pl)

#we want to transform the dataset into a panel one
#first we nead to create a column for each variable (Arrests/Attendance/Arrests percent)
colAttendance <- c("87_88_attendance", "88_89_attendance", "89_90_attendance",
                   "90_91_attendance", "91_92_attendance", "92_93_attendance",
                    "93_94_attendance")
colArrests <- c("87_88_arrests", "88_89_arrests", "89_90_arrests",
                "90_91_arrests", "91_92_arrests", "92_93_arrests",
                "93_94_arrests")
colArrPer <- c("87_88_arr_per", "88_89_arr_per", "89_90_arr_per",
               "90_91_arr_per", "91_92_arr_per", "92_93_arr_per",
               "93_94_arr_per")

#now we can combine those variables using the data table form and transform it into panel
panel_pl <- melt(dt, id.vars = 'City' ,measure = list(colAttendance, colArrests, colArrPer),variable.name = 'Season' ,
               value.name = c("Attendance", "Arrests", "Arrests_Percent"))
panel_pl <- panel_pl[order(City)] #ordering the data by city

#defining the new levels to the 'Season' variable
levels(panel_pl$Season) <- c('1987/1988','1988/1989','1989/1990','1990/1991','1991/1992','1992/1993','1993/1994')

#creating a new variable determines wheter the season was before or after the disaster
#1 = after the disaster | 0 = before the disaster
panel_pl$post <- as.factor(ifelse(panel_pl$Season=="1987/1988"|
                          panel_pl$Season=="1988/1989"|panel_pl$Season=="1989/1990" ,0,1))
panel_pl$Attendance_thosand <- panel_pl$Attendance/1000


#after all those changes we need to check again the data's structure and summary
summary(panel_pl)
str(panel_pl)

#install.packages("ggplot2") #RUN ONLY ONCE!!
library(ggplot2)

#Descriptive statistics
ggplot(panel_pl, aes(x = Arrests , y = Attendance))+
  geom_point()+
  geom_smooth(method = lm)+
  ggtitle('Arrests by Attendance (Premier/Championship)')

ggplot(panel_pl, aes(x = Arrests , y = Attendance, color = as.factor(Season)))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(title = 'Arrests by Attendance per season (Premier/Championship)', color = 'Season')

ggplot(panel_pl, aes(x = Arrests , y = Attendance))+
  facet_grid(~panel_pl$post, labeller = as_labeller(c("0" = "Before", "1" = "After")))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(title = 'Arrests by Attendance before and after the disaster (Premier/Championship)')

ggplot(data = panel_pl) +
  geom_line(aes(x = Season , y = Arrests))+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title="Arrests Trend(Premier/Championship)")

ggplot(panel_pl , aes(x = Arrests , color = as.factor(City))) +
  geom_boxplot()+
  labs(title = 'Arrests boxplot by each City (Premier/Championship)' ,color = 'City')

ggplot(data = subset(panel_pl, Arrests < 400), aes(x = Arrests))+
  geom_histogram(bins = 30)+
  labs(title = 'Arrests Histogram by Season - under 400 (Premier/Championship)')+
  ylab('Number of cities')+
  facet_grid(~Season)+
  scale_x_continuous(breaks = seq(0,400,50))+
  theme(plot.title = element_text(hjust = 0.5))


#install.packages("stargazer") #RUN ONLY ONCE!!
library(stargazer)

rm(pl) #removing unused dataset 

#Now we will run the same code on the 3rd&4th divisions datasets

########3RD & 4TH DIVISION###########

other<- read.csv("Third_Fourth Division arrests 1987_1994 Csv.csv", header = T)
  
colnames(other) <- c('City','87_88_attendance','87_88_arrests','87_88_arr_per',
                       '88_89_attendance','88_89_arrests','88_89_arr_per',
                       '89_90_attendance','89_90_arrests','89_90_arr_per',
                       '90_91_attendance','90_91_arrests','90_91_arr_per',
                       '91_92_attendance','91_92_arrests','91_92_arr_per',
                       '92_93_attendance','92_93_arrests','92_93_arr_per',
                       '93_94_attendance','93_94_arrests','93_94_arr_per')
  
other <- subset(other, City != 'City'&City != 'Total')
other[] <- lapply(other, function(x) if(is.factor(x)) factor(x) else x)
  
str(other) #checking the stracture of the data
names(other)
  
#converting the variables into numeric ones
other[,c(2,5,8,11,14,17,20)] <-  sapply(other[,c(2,5,8,11,14,17,20)],
                                          function(x){gsub(pattern = ",",replacement = "", x)})
other[,c(4,7,10,13,16,19,22)] <-  sapply(other[,c(4,7,10,13,16,19,22)],
                                           function(x){gsub(pattern = "%",replacement = "", x)})
other[,c(3,6,9,12,15,18,21)] <-  sapply(other[,c(3,6,9,12,15,18,21)],
                                          function(x){gsub(pattern = ",",replacement = "", x)})
other[2:22] <- sapply(other[2:22], function(x) as.numeric(as.character(x)))
other$City <- as.character(other$City)
  
#checking the new data frame summary and structure
summary(other)
str(other)
  
dt_other <- as.data.table(other)
  
colAttendance <- c("87_88_attendance", "88_89_attendance", "89_90_attendance",
                     "90_91_attendance", "91_92_attendance", "92_93_attendance",
                     "93_94_attendance")
colArrests <- c("87_88_arrests", "88_89_arrests", "89_90_arrests",
                  "90_91_arrests", "91_92_arrests", "92_93_arrests",
                  "93_94_arrests")
colArrPer <- c("87_88_arr_per", "88_89_arr_per", "89_90_arr_per",
                 "90_91_arr_per", "91_92_arr_per", "92_93_arr_per",
                 "93_94_arr_per")
  
  
panel_other <- melt(dt_other, id.vars = 'City' ,measure = list(colAttendance, colArrests, colArrPer),variable.name = 'Season' ,
                      value.name = c("Attendance", "Arrests", "Arrests_Percent"))
panel_other <- panel_other[order(City)]
  
levels(panel_other$Season) <- c('1987/1988','1988/1989','1989/1990','1990/1991','1991/1992','1992/1993','1993/1994')
  
#creating a new variable determines wheter the season was before or after the disaster
#1 = after the disaster | 0 = before the disaster
panel_other$post <- as.factor(ifelse(panel_other$Season=="1987/1988"|
                             panel_other$Season=="1988/1989"|panel_other$Season=="1989/1990" ,0,1))

panel_other$Attendance_thosand <- panel_other$Attendance/1000

summary(panel_other)
str(panel_other)
  
ggplot(panel_other, aes(x = Arrests , y = Attendance))+
    geom_point()+
    geom_smooth(method = lm)+
    ggtitle('Arrests by Attendance (3rd_4th Division)')
  
ggplot(panel_other, aes(x = Arrests , y = Attendance, color = as.factor(Season)))+
    geom_point()+
    geom_smooth(method = lm)+
    labs(title = 'Arrests by Attendance per season (3rd_4th Division)', color = 'Season')

ggplot(panel_other, aes(x = Arrests , y = Attendance))+
  facet_grid(~panel_other$post, labeller = as_labeller(c("0" = "Before", "1" = "After")))+
  geom_point()+
  geom_smooth(method = lm)+
  labs(title = 'Arrests by Attendance before and after the disaster (3rd_4th Division)')  

ggplot(data = panel_other) +
  geom_line(aes(x = Season , y = Arrests))+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title="Arrests Trend(3rd_4th Division)") #note that there is a outlier on season 1991/1992
 
ggplot(panel_other , aes(x = Arrests , color = as.factor(City))) +
    geom_boxplot()+
    labs(title = 'Arrests boxplot by each City (3rd_4th Division)' ,color = 'City')
  
ggplot(data = panel_other, aes(x = Arrests_Percent))+
    geom_density(fill = 'violet')+
    labs(title = 'Arrests Percent density graph (3rd_4th Division)')
  
ggplot(data = subset(panel_other, Arrests < 50), aes(x = Arrests))+
  geom_histogram(bins = 30)+
  labs(title = 'Arrests Histogram by Season - under 50 (Premier/Championship)')+
  ylab('Number of cities')+
  facet_grid(~Season)+
  scale_x_continuous(breaks = seq(0,50,5))+
  theme(plot.title = element_text(hjust = 0.5))

rm(other) #removing unused dataset




##########################  TOTAL  ################################################


#we want to create a total dataset containing both of the previus frames
total <- rbind(panel_pl, panel_other)
total$treat <- as.factor(ifelse(total$City %in% panel_pl$City , 1 , 0)) #dummy variable
dt_total <- as.data.table(total) #creating data_table


total$year <- format(as.Date(total$Season, format="%Y"),"%Y")
total$year <- as.numeric(total$year)

#checking the summary and structure of our new data frame
summary(total)
str(total)

#stargazer(total, out = 'TotalSummary.html') #Summary table creation (Run only ONCE!!)

ggplot(data = total, aes(x = Arrests_Percent))+
  facet_grid(~treat, labeller = as_labeller(c("0" = "3rd_4th", "1" = "1st_2nd")))+
  geom_density(fill = 'violet')+
  labs(title = 'Arrests Percent density graph')

ggplot(data = total) +
  geom_line(aes(x = Season , y = Arrests_Percent))+
  facet_grid(~treat, labeller = as_labeller(c("0" = "3rd_4th", "1" = "1st_2nd")))+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  labs(title="Arrests Percent Trend")+
  theme(plot.title = element_text(hjust = 0.5))


#now create a table of Arrests Variable's summary by each city
Arrests_by_city = dt_total[ ,.(Min = min(Arrests),
                                      firstqu = quantile(Arrests, 0.25),
                                      Median = median(Arrests),
                                      Mean = mean(Arrests),
                                      thirdqu = quantile(Arrests, 0.75),
                                      Max = max(Arrests)
                                      ) ,by = 'City']
#stargazer(Arrests_by_city,summary = F, out = 'Arrests_summary_by_city.html') #RUN ONLY ONCE!!!

#the estimation of the Difference-in-Difference model
model1 <- lm(Arrests~post+treat+post*treat , data = total)
summary(model1)

#Creating the Difference-in-Difference table
trial <- matrix(c(model1$coefficients[1],
                  model1$coefficients[1]+model1$coefficients[2],
                  model1$coefficients[1]+model1$coefficients[3],
                  model1$coefficients[1]+model1$coefficients[4]+model1$coefficients[2]+model1$coefficients[3]), ncol=2)
colnames(trial) <- c('treat=0', 'treat=1')
rownames(trial) <- c('post=0', 'post=1')
trial <- as.table(trial)
trial <- rbind(trial ,total =  c(model1$coefficients[2],model1$coefficients[4]+model1$coefficients[2]))
trial <- cbind(trial ,total = c(model1$coefficients[3],model1$coefficients[4]+model1$coefficients[3],model1$coefficients[4]))

#stargazer(trial,flip = T,digits.extra = 2 , out = 'Difference-in-Difference.html') #Run only ONCE!!




##########################REGIONS########################################



#Loading the regions data
getwd()
setwd("C:/Users/Omer Gralnik/Desktop/Datasets-Premier League/Datasets")
list.files()
region <- read.csv("Regions_csv.csv", header = T)

#Checking the data
names(region)
str(region)
summary(region)


#Loadin the list indicating every city's region
citybyregion <- read.csv("City_by_region.csv", header = F)
colnames(citybyregion) <-  c('City','region')


#Creating a trend for expenditure for beer and tobacco by region
ggplot(data = region, aes(x = year, y = percent_from_income))+
  geom_line(aes(color = region), size = 1) +
  theme_minimal()+theme(axis.text.x=element_text(angle=60, hjust=1),
                        plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(1987,1993,1))+
  labs(title = "Consumers Expenditure on Tabacco and Beer percent by Income", x = 'Year')+
  scale_y_continuous(name = 'Percent by income', limits = c(0,0.8))+
  geom_vline(xintercept = 1989 , color = 'red')

#adding the 'region' variable to the total dataset
total <- merge(total,citybyregion,by= "City")
rm(citybyregion)

ggplot(data = region, aes(x = year, y = income_per_head))+
  geom_line(aes(color = region), size = 1) +
  theme_minimal()+theme(axis.text.x=element_text(angle=60, hjust=1),
                        plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(1987,1993,1))+
  labs(title = "Income per head by region", x = 'Year')+
  scale_y_continuous(name = 'Income by head', limits = c(5000,12000))+
  geom_vline(xintercept = 1989 , color = 'red')


by(region$income_per_head, region$region, summary)






######################UNEMPLOYMENT#################################






#loading unemployment dataset
list.files()
unemp <- read.csv("labour force by region_csv.csv" , header = T)
unemp <- unemp[97:103,1:11]

#converting the variables into numeric
unemp[,1:11] <-  sapply(unemp[,1:11],function(x){gsub(pattern = ",",replacement = "", x)})
unemp[,1:11] <- sapply(unemp[,1:11], function(x) as.numeric(as.character(x)))


#checking the data's structure and summary
str(unemp)
summary(unemp)
names(unemp)

#renaming the variables to be as same as the regions names in the total dataset
levels(total$region)
names(unemp) <- c('year',"Northeast","Northwest","Yorkshire Humberside","East Midlands","West Midlands",
                  "East","Greater London","SouthEast","Southwest","Wales")

#changing the data frame to data table form
dt_unemp <- as.data.table(unemp)

#creating a vector of regions
colregion <-c("Northeast","Northwest","Yorkshire Humberside","East Midlands","West Midlands",
              "East","Greater London","SouthEast","Southwest","Wales")

#creating the panel dataset
panel_unemp <- melt(dt_unemp, id.vars = 'year' ,measure = colregion,
                    variable.name = 'region', value.name = 'unemployed')

#adding the unemployment variable to the region's data
region <- merge(region,panel_unemp,by= c("region",'year'))
rm(unemp) #removing unused data
rm(panel_unemp) #removing unused data

#creating a trend of unemployment by region
ggplot(data = region, aes(x = year, y = unemployed))+
  geom_line(aes(color = region), size = 1) +
  theme_minimal()+theme(axis.text.x=element_text(angle=60, hjust=1),
                        plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks = seq(1987,1993,1))+
  labs(title = "unemployment trend by region", x = 'Year')+
  scale_y_continuous(name = '# of unemployed', limits = c(900000,6000000))+
  geom_vline(xintercept = 1989 , color = 'red')




##########################MERGING THE DATA SETS INTO THE TOTAL ONE####################################





#checking summary and structure
summary(region)
summary(total)
str(region)
str(total)

#Changing letter case of column names of the total's dataset
names(total) <- tolower(names(total))

#creating the new total dataset
total <- merge(total,region, by = c('region','year'))
total <- total[order(city)]

#stargazer summaries of the data-Run ONCE!!
#stargazer(total[treat==1],out = 'summary_treat.html')
#stargazer(total[treat==0],out = 'summary_validation.html')
#stargazer(total,out = 'summary_total.html')

#generating new variable for beer and tobacco and removing the pervious variables
total$beer_tobacco <- total$beer+total$tobacco
total$beer <- NULL
total$tobacco <- NULL




######################POPULATION################################################




#loadind the data set and subsetting it
list.files()
pop_by_city <- read.csv("City_by_region_new.csv",header = F)
pop_by_city <- pop_by_city[,c(1,5)]
colnames(pop_by_city) <- c('city','percent')

#merging the datasets
total <- merge(total,pop_by_city,by = 'city')

#generating the population variable by city
total$population_new <- round(total$population*total$percent,0)

#removing unused variables & datasets
total$percent <- NULL
total$population <- NULL
rm(pop_by_city)

#changing the population variable name
names(total)[names(total) == "population_new"] <- "population"

#checking the data's summary and structure
str(total)
summary(total)

#now create an arrests by population trend
#but first we will generate the variables of unemployment and population devided by thousand
total$unemployed_thousand <- total$unemployed/1000
total$population_thousand <- total$population/1000
total$arrest_vs_population <- total$arrests/total$population_thousand

trend_treat <- dt[,.(Season = unique(subset(total,treat == 1)$season),
                     Arrests = by(subset(total,treat == 1)$arrests,subset(total,treat == 1)$season,mean),
                     Arrests_vs_pop = by(subset(total,treat == 1)$arrest_vs_population, subset(total,treat == 1)$season, mean))]
trend_validation <- dt[,.(Season = unique(subset(total,treat == 0)$season),
                          Arrests = by(subset(total,treat == 0)$arrests,subset(total,treat == 0)$season,mean),
                          Arrests_vs_pop = by(subset(total,treat == 0)$arrest_vs_population, subset(total,treat == 0)$season, mean))]
trend_total <- rbind(trend_treat,trend_validation)
trend_total$treat <- as.factor(ifelse(trend_total$Arrests>90,1,0))
rm(trend_treat) #removing the unused data
rm(trend_validation) #removing the unused data

#install.packages("lubridate") #RUN ONLY ONCE!!
library(lubridate) #with this package we will create a year variable
trend_total$year <- format(as.Date(trend_total$Season, format="%Y"),"%Y")
trend_total$year <- as.numeric(trend_total$year)

#Creating a Trend graph of the Arrests variable by season
ggplot(data = trend_total, aes(x = year, y = Arrests))+
  geom_line(aes(color = treat), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()+theme(axis.text.x=element_text(angle=60, hjust=1),
                        plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(name = 'Arrests', limits = c(0,150))+
  labs(title = 'Arrests Trend', x = 'Season')+
  scale_x_continuous(breaks = seq(1987,1993,1))+
  geom_vline(xintercept = 1989 , color = 'red')

#Creating a Trend graph of the Arrests by population variable for each season
ggplot(data = trend_total, aes(x = year, y = Arrests_vs_pop))+
  geom_line(aes(color = treat), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()+theme(axis.text.x=element_text(angle=60, hjust=1),
                        plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(name = 'Arrests per Population', limits = c(0,0.5))+
  labs(title = 'Arrests by Population Trend', x = 'Season')+
  scale_x_continuous(breaks = seq(1987,1993,1))+
  geom_vline(xintercept = 1989 , color = 'red')





######################Descriptive Statistics#####################################





summary(lm(attendance_thosand~treat, data = total))
summary(lm(arrests~treat, data = total))
summary(lm(arrests_percent~treat, data = total))
by(total$arrests, total$treat, mean)
by(total$arrests_percent, total$treat, mean)
by(total$attendance_thosand, total$treat, mean)
by(total$arrests, total$treat, sd)
by(total$arrests_percent, total$treat, sd)
by(total$attendance_thosand, total$treat, sd)


des <- data.table('Variable'=c('arrests','arrests percent','attendance (thousand)','population (thousand)','Income per head',
                               'Percent from income','Unemployed (thousand)'),
                  'Validate'=c(paste(round(mean(subset(total,treat=='0')$arrests),2),'(',round(sd(subset(total,treat=='0')$arrests),2),')'),
                               paste(round(mean(subset(total,treat=='0')$arrests_percent),3),'(',round(sd(subset(total,treat=='0')$arrests_percent),3),')'),
                               paste(round(mean(subset(total,treat=='0')$attendance_thosand),2),'(',round(sd(subset(total,treat=='0')$attendance_thosand),2),')'),
                               paste(round(mean(subset(total,treat=='0')$population_thousand),1),'(',round(sd(subset(total,treat=='0')$population_thousand),1),')'),
                               paste(round(mean(subset(total,treat=='0')$income_per_head),1),'(',round(sd(subset(total,treat=='0')$income_per_head),1),')'),
                               paste(round(mean(subset(total,treat=='0')$percent_from_income),3),'(',round(sd(subset(total,treat=='0')$percent_from_income),3),')'),
                               paste(round(mean(subset(total,treat=='0')$unemployed_thousand),1),'(',round(sd(subset(total,treat=='0')$unemployed_thousand),1),')')),
                  'Treat'   =c(paste(round(mean(subset(total,treat=='1')$arrests),2),'(',round(sd(subset(total,treat=='1')$arrests),2),')'),
                               paste(round(mean(subset(total,treat=='1')$arrests_percent),3),'(',round(sd(subset(total,treat=='1')$arrests_percent),3),')'),
                               paste(round(mean(subset(total,treat=='1')$attendance_thosand),2),'(',round(sd(subset(total,treat=='1')$attendance_thosand),2),')'),
                               paste(round(mean(subset(total,treat=='1')$population_thousand),1),'(',round(sd(subset(total,treat=='1')$population_thousand),1),')'),
                               paste(round(mean(subset(total,treat=='1')$income_per_head),1),'(',round(sd(subset(total,treat=='1')$income_per_head),1),')'),
                               paste(round(mean(subset(total,treat=='1')$percent_from_income),3),'(',round(sd(subset(total,treat=='1')$percent_from_income),3),')'),
                               paste(round(mean(subset(total,treat=='1')$unemployed_thousand),1),'(',round(sd(subset(total,treat=='1')$unemployed_thousand),1),')')),
                  'Diff'    =c(paste(round(summary(lm(arrests~treat, data = total))$coefficients[2,1],2),'(',round(summary(lm(arrests~treat, data = total))$coefficients[2,2],2),')'),
                               paste(round(summary(lm(arrests_percent~treat, data = total))$coefficients[2,1],3),'(',round(summary(lm(arrests_percent~treat, data = total))$coefficients[2,2],3),')'),
                               paste(round(summary(lm(attendance_thosand~treat, data = total))$coefficients[2,1],2),'(',round(summary(lm(attendance_thosand~treat, data = total))$coefficients[2,2],2),')'),
                               paste(round(summary(lm(population_thousand~treat, data = total))$coefficients[2,1],1),'(',round(summary(lm(population_thousand~treat, data = total))$coefficients[2,2],1),')'),
                               paste(round(summary(lm(income_per_head~treat, data = total))$coefficients[2,1],1),'(',round(summary(lm(income_per_head~treat, data = total))$coefficients[2,2],1),')'),
                               paste(round(summary(lm(percent_from_income~treat, data = total))$coefficients[2,1],3),'(',round(summary(lm(percent_from_income~treat, data = total))$coefficients[2,2],3),')'),
                               paste(round(summary(lm(unemployed_thousand~treat, data = total))$coefficients[2,1],2),'(',round(summary(lm(unemployed_thousand~treat, data = total))$coefficients[2,2],2),')')))
#stargazer(des,out = 'Summary Statistics.html',summary = F,rownames = F, column.sep.width = '20pt',title = 'Summary Statistics')





############################Creating regressions tables##################################3





#install.packages("plm") #RUN ONLY ONCE!!
library(plm)

#TREAT GROUP ONLY
#stargazer(lm(arrests~post, data = subset(total,treat == 1)),
#          lm(arrests~post+attendance_thosand, data = subset(total,treat == 1)),
#          lm(arrests~post+attendance_thosand+beer_tobacco, data = subset(total,treat == 1)),
#          lm(arrests~post+attendance_thosand+beer_tobacco+unemployed_thousand, data = subset(total,treat == 1)),
#          lm(arrests~post+attendance_thosand+beer_tobacco+unemployed_thousand+population_thousand, data = subset(total,treat == 1)),
#          plm(arrests~post+attendance_thosand+beer_tobacco+unemployed_thousand+population_thousand, model = 'within',index = c('city','year'), data = subset(total,treat == 1)),
#          out = 'Regressions table_treat.html' , title = 'Regressions table_treat' , intercept.bottom = F)

#VALIDATION vs. TREAT
#stargazer(lm(arrests~post, data = subset(total,treat == 0)),
#          lm(arrests~post+attendance_thosand, data = subset(total,treat == 0)),
#          lm(arrests~post+attendance_thosand+beer_tobacco, data = subset(total,treat == 0)),
#          lm(arrests~post+attendance_thosand+beer_tobacco+unemployed_thousand, data = subset(total,treat == 0)),
#          lm(arrests~post+attendance_thosand+beer_tobacco+unemployed_thousand+population_thousand, data = subset(total,treat == 0)),
#          plm(arrests~post+attendance_thosand+beer_tobacco+unemployed_thousand+population_thousand, model = 'within',index = c('city','year'), data = subset(total,treat == 0)),
#          lm(arrests~post, data = subset(total,treat == 1)),
#          lm(arrests~post+attendance_thosand, data = subset(total,treat == 1)),
#          lm(arrests~post+attendance_thosand+beer_tobacco, data = subset(total,treat == 1)),
#          lm(arrests~post+attendance_thosand+beer_tobacco+unemployed_thousand, data = subset(total,treat == 1)),
#          lm(arrests~post+attendance_thosand+beer_tobacco+unemployed_thousand+population_thousand, data = subset(total,treat == 1)),
#          plm(arrests~post+attendance_thosand+beer_tobacco+unemployed_thousand+population_thousand, model = 'within',index = c('city','year'), data = subset(total,treat == 1)),
#          out = 'Regressions table_treat_vs_validate.html' , title = 'Regressions table_treat_validate',
#           column.separate = c(6,6), column.labels = c('Validate','Treat'), intercept.bottom = F)

#TOTAL
#stargazer(lm(arrests~post, data = total),
#          lm(arrests~post+treat, data = total),
#          lm(arrests~post+treat+attendance_thosand, data = total),
#          lm(arrests~post+treat+attendance_thosand+beer_tobacco, data = total),
#          lm(arrests~post+treat+attendance_thosand+beer_tobacco+unemployed_thousand, data = total),
#          lm(arrests~post+treat+attendance_thosand+beer_tobacco+unemployed_thousand+population_thousand, data = total),
#          plm(arrests~post+attendance_thosand+beer_tobacco+unemployed_thousand+population_thousand, model = 'within',index = c('city','year'), data = total),
#          out = 'Regressions table_total.html', title = 'Regressions table - Total' ,intercept.bottom = F)


#Difference-in-Difference regressions table
#stargazer(lm(arrests~post*treat, data = total),
#          lm(arrests~post*treat+attendance_thosand, data = total),
#          lm(arrests~post*treat+attendance_thosand+beer_tobacco, data = total),
#          lm(arrests~post*treat+attendance_thosand+beer_tobacco+unemployed_thousand, data = total),
#          lm(arrests~post*treat+attendance_thosand+beer_tobacco+unemployed_thousand+population_thousand, data = total),
#          out = 'DID Regressions table.html', title = 'Difference-in-Difference' ,intercept.bottom = F)




#write.csv(total, 'total.csv')




