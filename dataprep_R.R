#setting working directory--------------------------
getwd()
setwd("D:\\IITK Data Analytics\\R\\")


#learning how to access files---------------------------
file='bank-full.csv'

df=read.csv(file,stringsAsFactors = F)
#check dataframe....they values are all in 1 column
#to resolve this
df=read.csv(file,stringsAsFactors = F,sep=';')
# other arguments: skip,nrows , header = T/F
df=read.csv(file,stringsAsFactors = F,sep=';',nrows = 1000)

# what if i want "missing" to be read as NA
df= read.csv(file,stringsAsFactors = F,sep=';',nrows=1000,
             na.string='missing')

#-----apply functions---------------------------------------

# a simple for loop to calculate mean of all columns and print 
for(i in 1:ncol(mtcars)){
  
  print((mtcars[,i]))
  
}

#the same can be done using lappy() or sapply()-?gives vectorized output
lapply(mtcars,mean)
lapply(mtcars,mean,na.rm=T ) # incasse their are missing values
sapply(mtcars,mean) #easier to read output

#apply()
apply(mtcars,1,mean) #rows
apply(mtcars,2,mean) #columns

# before applying the function apply converts the data frame to a matrix
# all the values in a matrix should be of same type


#tapply() -> 2nd argument groupbys
tapply(mtcars$mpg, mtcars[,c("am","vs")],mean)


## application of lapply() to read  multiple .txt files from a folder

# Before running these codes , you'll have to set your working directory to the folder "namesbystate".
# You will find this folder inside "Data" folder which you downloaded from LMS
getwd()
file_names=list.files()

file_names=list.files(pattern="*.TXT")
file_names

files=lapply( file_names , read.csv ,stringsAsFactors=F,header=F)

# d=rbind(files[[1]],files[[2]],files[[3]])

# rbind(files[[1]],files[[2]],files[[3]],....)

file=do.call(rbind,files)

# write.csv to write a dataframe to your machine

names(file)=c("state","gender","year","name","count")




#dplyr function ------------------------------------------
library(dplyr)
library(hflights)

str(hflights) # 227496 obs. of  21 variables
View(hflights)

flights=as_tibble(hflights) #to get a better idea of the data
flights

# addcolumns/modify vcolumns : mutate
# conditional subsetting : filter
# remove/select columns : select
# sprt : arrange
#group_by, summarise: groupby,summarise
# ALL THESE FUNCTIONS TAKE THE FIRST ARG AS THE DATAFRAME



#Filter Function---------------
d1=filter(hflights, Month == 1 , DayOfWeek==2) #numerical conditions
d2=filter(hflights,Month==1 & UniqueCarrier %in% c("AA", "UA")) #cat and num conditions




#Select Function-------------------
d3=select(hflights,UniqueCarrier,Year:ArrTime,contains('Taxi'))
d4=select(hflights,- UniqueCarrier,Year:ArrTime,- contains('Taxi'))

select(flights,Year,Month,DayofMonth,DayOfWeek,DepTime,TaxiIn,TaxiOut,
       ArrDelay,DepDelay)
# smarter way to execute the above code : 
d6= select(flights, Year:DepTime,  #range
           contains("Taxi"),       #names w Taxi
           contains("Delay"))     # names w Delay
#select only works with the names of colums
# has nothing to do with the values

# nesting method to select UniqueCarrier and DepDelay columns 
# and filter for delays over 60 minutes
first=select(flights, UniqueCarrier, DepDelay)
second=filter(first,DepDelay > 60)

filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60)




#PIPE OPERATOR---------------------------------------
x=sample(10,6) 
x
sum(sin(log(x)))
x %>% 
  log() %>%    windows shortcut : ctrs +shift + m 
  sin() %>% 
  sum()

  #we will use this logic to prepare our data in a much readable way
  
  # chaining method : there is no sequqnce like SQL
  d5=flights %>% 
    select(UniqueCarrier, DepDelay,contains("Time")) %>% 
    filter(DepDelay > 60) %>% 
    select(contains("Dep")) %>% 
    filter(DepDelay>100)
  
#Arrange function----------------
  d6 = flights %>% 
       select(UniqueCarrier,FlightNum,DepTime,DepDelay,contains('Taxi')) %>% 
       filter(FlightNum%%2==0 & UniqueCarrier %in% c('MQ','CO','WN')) %>% 
        arrange(UniqueCarrier,desc(DepDelay))
  
#Mutate Functions : create new columns--------------------
  d7=flights %>%
    select(Distance, AirTime,DepDelay,TaxiIn,TaxiOut) %>%
    mutate(speed=Distance/AirTime,
           total_taxi=TaxiIn+TaxiOut) %>% 
    select(-TaxiIn,-TaxiOut) %>% 
    mutate(time_diff=AirTime-total_taxi,
           time_diff=time_diff/60) %>% 
    arrange(time_diff) %>% 
    mutate(DepDelay_high=ifelse(DepDelay>60,1,0))

# SUMMARIZE AND GROUPBY-----------------------------
  # create a table grouped by Dest, and then summarise each group by taking the mean of ArrDelay
  
  d8=flights %>%
    group_by(Dest,Month) %>% 
    summarise(avg_delay = mean(ArrDelay,na.rm=T))
  #SUMMARIZE WILL COLLAPSE THE DATA
  #MUTATE WILL DO AND CREATE COLUMNS FOR EACH ROW
  
  
  # n() : gives number of observation/rows
  
  flights %>%
    group_by(Month, DayofMonth) %>%
    summarise(flight_count = n()) %>% 
    arrange(desc(flight_count) )
  
# tally function ,n() , n_distinct(), lag()----------------
  flights %>%
    group_by(Month, DayofMonth) %>%
    tally(sort=TRUE) 
  
  flights %>%
    group_by(Dest) %>%
    summarise(flight_count = n(),
              plane_count = n_distinct(TailNum))
  
  
  
  d9= flights %>%
    group_by(Month) %>%
    summarise(flight_count = n()) %>%
    mutate(lagged_col=lag(flight_count,1),
           change = flight_count - lagged_col)
  
  # filter , select , arrange , mutate , group_by+summarise
  # tbl_df , n(): number of obs , n_distinct(col): number of distinct obs
  # lag : lagged columns

  
  
  
  # for each dest , every month 
  # find out which month has highest count of flights , 
  # second and 5th highest count of flights and least count of flight
  flights = as_tibble(hflights)
  d10= flights %>% 
    group_by(Dest, Month) %>% 
    summarise(monthly_flight_count = n()) %>% 
    arrange (Dest,desc(monthly_flight_count)) %>% 
    #sort(partial=length(flights)-1)[n-1]
    
    
# TIDYR AND LUBRIDATE ---------------------------
