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

#apply functions---------------------------------------

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
  library(lubridate)
  
  # prebuilt functions .... but not often used
  #output will always be of the form yyyy-mm-dd
  ymd("20110604")
  mdy("06-04-2011")
  dmy("04/06/2011")
  
  # y : year
  # m : month
  # d : day
  class(mdy("06-04-2011"))
  
  
  arrive = ydm_hms("2011-06-04 12:00:00", tz = "Pacific/Auckland")
  leave = ymd_hms("2011-08-10 14:00:00", tz = "Pacific/Auckland")
  OlsonNames()
  #to change the timezone internally with_tz( <dateframe>, "Asia/Kolkata")
  with_tz(arrive,'Asia/Kolkata')
  
  #tio get current values
  lubridate::now()
  lubridate::today()
  
  
  
  # doing date calculations ------------------
  leap_year(2011)
  
  ymd(20110101) + dyears(13) #
  
  ymd(20110101) + years(1)
  
  
  leap_year(2020)
  
  ymd(20110101) + dyears(12) #adding exactly 365 days  #only happens for multiples of 12
  
  ymd(20110101) + years(12) # this explicitly add one year
  
  # ymd(20120101) + months(3)
  
  ## Find out how to take difference of two dates 
  ## how to get the output of difference in 
  ## different units : minutes , days, months
  
  d1=ymd(20110101)
  d2=ymd(20110101) + dyears(11)
  d2-d1

  
  #Parse_date_time()----------------------------------
  # d= two digit date
  # y = two digit year
  # b = abbreviated month name
  # Y= 4 digit year
  # B = complete month name
  # p = for  am pm
  # m = month in numbers
  # %H:%M = time is in 24 hrs format
  # %I:%M = time is in 12 hr format , this needs to be accompanied by p
  
  parse_date_time("01-12-Jan","%d-%y-%b")
  
  z=parse_date_time("2012-01-January 10:05 PM","%Y-%d-%B %I:%M %p")
  z + dyears(121)
  
  # how do i covert unix time stamp to R datetime format
  # extracting date time in specfic format from POSIXt object
  format(z,"%Y-%b")
  
  # January/01/12
  format(z,"%B/%d/%y")
  
  
  
  
  #Function can be used seamlessely for vectors as well
  x = c("09-01-01", "09-01-02", "09-01-03")
  parse_date_time(x, "ymd")
  parse_date_time(x, "%y%m%d")
  parse_date_time(x, "%y %m %d")
  
  
  
  
# Creating Dummies ------------------------------------------
  CreateDummies=function(data,var,freq_cutoff=100){
    t=table(data[,var])
    t=t[t>freq_cutoff]
    t=sort(t)
    categories=names(t)[-1]#removing the first one(least frequent..convention)
    categories=names(t)[-1] #neglecting the least occuring value
    
    #datawise operations
    
    for( cat in categories){
      name=paste(var,cat,sep="_")
      name=gsub(" ","",name)
      name=gsub("-","_",name)
      name=gsub("\\?","Q",name)
      
      data[,name]=as.numeric(data[,var]==cat)
    }
    
    data[,var]=NULL
    return(data)
  }
  
  ci=CreateDummies(ci,'marital.status',100)
  
  ci=CreateDummies(ci,'native.country',100)
  
  
  