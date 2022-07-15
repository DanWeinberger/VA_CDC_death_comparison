#library(feather)
library(data.table)
library(readr)
library(pbapply)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(cdlTools)
library(table1)
#the geographic resolution missing from the public data
# 
# 
# #https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm
# file.names1<- list('MULT2014.USAllCnty.txt', 'MULT2015.USAllCnty.txt','MULT2016.USAllCnty.txt','MULT2017.USAllCnty.txt',
#              'Mort2018US.AllCnty.txt','MULT2019US.AllCnty.txt','MULT2020.USAllCnty.txt')
# 
# all.ds <- lapply(file.names1, function(x){
#   d1 <- read_fwf(file=paste0("./CDC_tapes/CONFIDENTIAL/extracted/" ,x),
#                  fwf_positions(start=c(20,21,23,65,69,102,445,70,71, 79,484,146,167,174,181,188,195,202,209,216,223,230,237,244,251,258,265,272,279,286,293,300),
#                                end=c(  20,22,25,66,69,105,446,  70,73, 80,486,149, 171,178,185,192,199,206,213,220,227,234,241,248,255,262,269,276,283,290,297,304),
#                                col_names = c('res_status','state','county','month','sex','year','race','age_detail_class','age_detail_number','agec','hispanic', paste0('icd', 1:21 ) )),
#                   guess_max=10000)
#   return(d1)
# })
# 
# df1 <- bind_rows(all.ds)
# saveRDS(df1, './CDC_tapes/CONFIDENTIAL/compiled_data_county.rds')

df1 <- readRDS('./CDC_tapes/CONFIDENTIAL/compiled_data_county.rds')

df1$hisp_recode <- 999
df1$hisp_recode[df1$hispanic<=199 & df1$hispanic>=100] <- 0
df1$hisp_recode[df1$hispanic >=200 & df1$hispanic <= 299] <- 1
#table(df1$hisp_recode)

# df1$race_ethnicity <- 999
# df1$race_ethnicity[df1$race %in% c('01') & df1$hisp_recode != 1] <- 1 #white, non-Hospanic
# df1$race_ethnicity[df1$race %in% c('02') & df1$hisp_recode != 1]  <- 2 #black, non-Hispanic
# df1$race_ethnicity[ df1$hisp_recode == 1]  <- 3 #Hispanic
# df1$race_ethnicity[ df1$race %in% c('04','05','18','28','48' , '06','07','38','58','68','78') & df1$hisp_recode != 1]  <- 4 #Asian
# df1$race_ethnicity[ df1$race %in% c('03') & df1$hisp_recode != 1]  <- 5 #American Indian
# #table(df1$race_ethnicity)

#RACE: 
#1=Non-Hispanic White
#2=Non-Hispanic- Black
#3= Hispanic
#4-American Indian/Native Alaskan
#5: Asian/Pacific Island

df1$race_recode<- NA
df1$race_recode[df1$hisp_recode == 1] <- 3 #Hispanic

df1$race_recode[df1$race %in% c('01') & df1$hisp_recode != 1] <- 1 #white, non-Hispanic
df1$race_recode[df1$race %in% c('02') & df1$hisp_recode != 1 ]  <- 2 #black, non-Hispanic
df1$race_recode[ df1$race %in% c('03') & df1$hisp_recode != 1 ]  <- 4 #American Indian
df1$race_recode[ df1$race %in% c('04','05','18','28','48' ,'68','78') & df1$hisp_recode != 1]  <- 5 #Asian
df1$race_recode[ df1$race %in% c( '06','07','38','58') & df1$hisp_recode != 1]  <- 5 #Hawaain/Pac Is
df1$race_recode[is.na(df1$race_recode)] <- 999

#RACE RECODE:
#1=Non-Hispanic White
#2=Non-Hispanic- Black
#3= Hispanic
#4-American Indian/Native Alaskan
#5: Asian/Pacific Island

df1$qtr <- NA
df1$qtr[df1$month %in% c('01','02','03')] <- 1
df1$qtr[df1$month %in% c('04','05','06')] <- 2
df1$qtr[df1$month %in% c('07','08','09')] <- 3
df1$qtr[df1$month %in% c('10','11','12')] <- 4

df1$region <- NA
df1$region[df1$state %in% c('ME','VT','NH','MA','CT','RI','NY','NJ','PA')] <- 'Northeast'
df1$region[df1$state %in% c('DE','DC','FL','GA','MD','NC','SC','VA','WV','AL','MS','KY','TN','LA','OK','TX', 'AR')] <- 'South'
df1$region[df1$state %in% c('AZ','CA','CO','ID','NM','MT','UT','WY','NV','AK','CA','HI','OR','WA')] <- 'West'
df1$region[df1$state %in% c('IN','IL','MI','OH','WI','IA','KS','MN','MO','NE','ND','SD')] <- 'Midwest'


#Create aggregate time series for analysis
agg1 <- df1 %>%
  bind_rows() %>% 
  group_by(year, qtr, sex, agec, region, race_recode) %>%
  summarize(N_deaths = n())  %>%
  ungroup  %>%
  tidyr::complete(year,qtr, sex, agec,region,race_recode, fill=list(N_deaths=0)) #fills 0s

saveRDS(agg1,'./Data/Confidential/compiled_sex_age_race_qtr_region.rds')

df1$age_group <- df1$agec
df1$agec <- NA
df1$agec[df1$age_group %in% c('01','02','03','04')] <- "Under 25 Years"
df1$agec[df1$age_group %in% c('05','06')] <- "25-44 years"
df1$agec[df1$age_group %in% c('07','08')] <- "45-64 years"
df1$agec[df1$age_group %in% c('09')] <- "65-74 years"
df1$agec[df1$age_group %in% c('10')] <- "75-84 years"
df1$agec[df1$age_group %in% c('11')] <- '85 years and older'

#descriptive stats
tabstats <- df1[df1$year<=2019,]
tabstats$agey <- as.numeric(tabstats$age_detail_number)
tabstats$agey[tabstats$agey ==999] <- NA
tabstats$sex <- factor(tabstats$sex, c('M','F'))
tabstats$region <- factor(tabstats$region, c('Northeast', 'South', 'Midwest','West'))
tabstats$race_recode <- factor(tabstats$race_recode)
tab1 <- table1( ~agey +sex + race_recode + region, data=tabstats )
tab1
tab1.df <- as.data.frame(tab1)
write.csv(tab1.df,'./outputs/table1_US.csv')


agg2 <- df1 %>%
  bind_rows() %>% 
  group_by(year, qtr, sex, agec, region, race_recode) %>%
  summarize(N_deaths = n())  %>%
  ungroup  %>%
  tidyr::complete(year,qtr, sex, agec,region,race_recode, fill=list(N_deaths=0)) #fills 0s

saveRDS(agg2,'./Data/Confidential/compiled_sex_agec_race_qtr_region.rds')


## Cause specific deaths
df1$one <-1

covid.codes <- c('U071','Z28310','Z28311',"Z86.16", "Z28.9","J1282","M3581") #Define codes for COVID-19 https://www.cdc.gov/mmwr/volumes/70/wr/mm7014e2.htm U07.1 probably only relevant one for 2020

rsv.codes <- c('B974','J121', "J210", 'J205') #Define codes for RSV

pneumococcal.codes <- c('A403','J13','B953','G001')

ld.codes <- c('A481')

icd.cols <- grep('icd',names(df1)) #Define columns with multiple cause of death stats

df.rsv <- pbapply(df1[,icd.cols],2, function(x) x %in% rsv.codes )

df.covid <- pbapply(df1[,icd.cols],2, function(x) x %in% covid.codes )

df.pneumo <- pbapply(df1[,icd.cols],2, function(x) x %in% pneumococcal.codes )

df.leg <- pbapply(df1[,icd.cols],2, function(x) x %in% ld.codes )


df1$covid <- rowSums(df.covid) #how many RSV codes re there per row?
df1$covid <- 1*(df1$covid>0) #convert to binary

covid_counts <- df1 %>%
  filter(year==2020) %>%
  group_by(agec, race_recode, sex,region, qtr) %>%
  summarize(N_covid=sum(covid))
saveRDS(covid_counts,'./Data/confidential/covid_deaths_by_group.rds')

df1$rsv <- rowSums(df.rsv) #how many RSV codes re there per row?
df1$rsv <- 1*(df1$rsv>0) #convert to binary

df1$pneumo <- rowSums(df.pneumo) #how many RSV codes re there per row?
df1$pneumo <- 1*(df1$pneumo>0) #convert to binary

df1$ld <- rowSums(df.leg) #how many RSV codes re there per row?
df1$ld <- 1*(df1$ld>0) #convert to binary

df1$infant <- 0
df1$infant[df1$age_detail_class==1 & df1$age_detail_class==1] <-1
df1$infant[df1$age_detail_class %in% c(4,5,6) ] <-1

df1$agey <- as.numeric(df1$age_detail_number)
df1$agey[df1$age_detail_class==2] <- as.numeric(df1$age_detail_number[df1$age_detail_class==2] )/12
df1$agey[df1$age_detail_class==4] <- as.numeric(df1$age_detail_number[df1$age_detail_class==4] )/365
df1$agey[df1$age_detail_class==5] <- as.numeric(df1$age_detail_number[df1$age_detail_class==5] )/365/24
df1$agey[df1$age_detail_class==6] <- as.numeric(df1$age_detail_number[df1$age_detail_class==6] )/365/24/60

hist(df1$agey[df1$rsv==1 & df1$agey<1])

#AGE DISTRIBUTION RSV DEATHS IN BABIES
df1[df1$rsv==1 & df1$agey<1,] %>%
  group_by(race_recode) %>%
  summarize(ave_age=mean(agey), n=n())

#AND OVERALL
df1[df1$rsv==1 ,] %>%
  group_by(race_recode) %>%
  summarize(ave_age=mean(agey), n=n()) 

test1 <- as.data.frame(df1[df1$rsv==1 ,])
hist(test1[ test1$race_recode==1 ,'agey'])
hist(test1[ test1$race_recode==2 ,'agey'])

#df1$date <- as.Date(paste(df1$year, df1$month, '01', sep='-'))


test <- df1[df1$rsv==1 & df1$agey<5,'agey']
ave.age.rsv <- df1[df1$rsv==1 & df1$agey<5,] 


#looks at seasonality by cause
agg1.season <- df1 %>%
  group_by(year,month, agec) %>%
  summarize(N_deaths = n(), pneumo=sum(pneumo), rsv=sum(rsv), ld=sum(ld)) %>%
  ungroup()

agg1.season$month <- as.numeric(agg1.season$month)



p1 <- ggplot(agg1.season, aes(x=month, y=pneumo, group=year, col=year)) +
  geom_line() +
  ylab("Number of pneumococcal deaths") +
  xlab("Date") +
  theme_classic() +
  theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
  geom_hline(yintercept=0, col='gray', lty=2) +
  facet_wrap(~ agec , scales='free') 
p1

p2 <- ggplot(agg1.season, aes(x=month, y=rsv, group=year, col=year)) +
  geom_line() +
  ylab("Number of RSV deaths") +
  xlab("Date") +
  theme_classic() +
  theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
  geom_hline(yintercept=0, col='gray', lty=2) +
  facet_wrap(~ agec , scales='free') 
p2


p3 <- ggplot(agg1.season, aes(x=month, y=ld, group=year, col=year)) +
  geom_line() +
  ylab("Number of LD deaths") +
  xlab("Date") +
  theme_classic() +
  theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
  geom_hline(yintercept=0, col='gray', lty=2) +
  facet_wrap(~ agec , scales='free') 
p3

agg1.season.allyr <- df1 %>%
  group_by(agec,month) %>%
  summarize(N_deaths = n(), pneumo=sum(pneumo), rsv=sum(rsv), ld=sum(ld)) %>%
  ungroup()

agg1.season.allyr$month <- as.numeric(agg1.season.allyr$month)

p4 <- ggplot(agg1.season.allyr, aes(x=month, y=ld, group=agec)) +
  geom_line() +
  ylab("Number of LD deaths") +
  xlab("Date") +
  theme_classic() +
  geom_hline(yintercept=0, col='gray', lty=2) +
  theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
  facet_wrap(~ agec , scales='free') 
p4

#Aggregate databy year, month, sex, age
agg1 <- all.ds %>%
  bind_rows() %>% 
  group_by(month, year, sex, agec) %>%
  summarize(N_deaths = n())

agg1$date <-    as.Date(paste(agg1$year, agg1$month,'01',sep='-'))

agg1 <- agg1[order(agg1$agec, agg1$sex, agg1$date),] 



ave.age.ldf <-  df1[df1$ld==1,] %>%
  group_by(month) %>%
  summarize(ave.age  = mean(agey) )

# Agec
# 01 ... Under 1 year (includes not stated infant ages)
# 02 ... 1 - 4 years
# 03 ... 5 - 14 years
# 04 ... 15 - 24 years
# 05 ... 25 - 34 years
# 06 ... 35 - 44 years
# 07 ... 45 - 54 years
# 08 ... 55 - 64 years
# 09 ... 65 - 74 years
# 10 ... 75 - 84 years
# 11 ... 85 years and over
# 12 ... Age not stated


#Race
# 01 ... White
# 02 ... Black
# 03 ... American Indian (includes Aleuts and Eskimos)
# 04 ... Chinese
# 05 ... Japanese
# 06 ... Hawaiian (includes Part-Hawaiian)
# 07 ... Filipino
# 18 ... Asian Indian
# 28 ... Korean
# 38 ... Samoan
# 48 ... Vietnamese
# 58 ... Guamanian
# 68 ... Other Asian or Pacific Islander in areas reporting codes 18-58
# 78 ... Combined other Asian or Pacific Islander, includes codes 18-68
# for areas that do not report them separately


#Hispanic:
# 484-486 3 Hispanic Origin
# 100-199 ... Non – Hispanic
# 200-209 … Spaniard
# 210-219 ... Mexican
# 260-269 ... Puerto Rican
# 270-274 ... Cuban
# 275-279 … Dominican
# 220 … Central and South American
# 221-230 ... Central American
# 231-249 … South American
# 250-259 … Latin American
# 280-299 ... Other Hispanic
# 996-999 ... Unknown

#RACE_RECODE: 
#1=Non-Hispanic White
#2=Non-Hispanic- Black
#3= Hispanic
#4-American Indian/Native Alaskan
#5: Asian/Pacific Island