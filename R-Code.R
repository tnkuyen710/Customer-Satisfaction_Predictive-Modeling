#clear workspace
rm(list = ls()) 

# libraries
library(tidyverse)
library(dplyr)
library(doParallel)
library(caret)
library(smotefamily)
library(pROC)
library(ggplot2)
library(grid)
library(gridExtra)
library(stargazer)
library(reshape2)
library(fastDummies)
library(corrplot)
library(scales)
library(caret)
library(glmnet)
library(kernlab)
library(rpart)
library(ranger)
#library(ipred)
library(xgboost)
library(NeuralNetTools)
library(nnet)


# set working directory 
setwd("/Users/tracie/Desktop/academic/goethe/2-SoSe25/DSMA/[SUBMISSION]/TRAN_Ngoc-Khanh-Uyen_8518013")



# prepare tips dataset --------------------------------------------------------------------------------------------------------------



# load the dataset
load("tipsbusiness_2020-01-01-2021-12-31_Korean_Saint Louis.RData")

# only select the non-business information,
# since the business information may include missing we need to deal with
tips_data = tips_data[,1:9]

# check for variables that are logical vector, and convert to integer
integer64s = sapply(tips_data,function(x){class(x)=="integer64"})
tips_data[,integer64s] = as.integer(tips_data[,integer64s])

# copy dataframe and extract names from cum_u_names column
DailyLevel_data = tips_data
cum_u_names = DailyLevel_data$cum_u_names

# count number of observations from cum_u_names column
Nobs=length(cum_u_names)

# replacing the cum_u_names with the count of reviewers' genders
if(0){ 
  #install.packages("gender")
  library(gender)
  #remotes::install_github("ropensci/gender-data-pkg")
  library(genderdata)
  # best is to do gender extractin in parallel
  library(doParallel)
  
  
  gendersplit=function(x){
    a=max.col(gender(unlist(strsplit(x,",")))[,2:3])
    return(c(male=sum(a==1,na.rm=T),female=sum(a==2,na.rm=T)))
  }
  
  cl=makeCluster(detectCores()/2+2)
  registerDoParallel(cl)
  nameslist=NULL
  for(k in 1:20){
    whichrun=floor(Nobs/20*(k-1)+1):floor(Nobs/20*k)
    a=foreach(i=whichrun,.packages=c("gender"),.noexport = c("DailyLevel_data"),.combine=rbind) %dopar%
      {gendersplit(cum_u_names[i])}
    rownames(a)=NULL
    nameslist=rbind(nameslist,a)
    print(k)
  }
  stopImplicitCluster()
  stopCluster(cl)
  save(file="nameslist_rev.RData",list="nameslist")
}else{
  load("nameslist_rev.RData")  
}

# add 2 gender count columns into dataset, and remove cum_u_names column
DailyLevel_data = cbind(DailyLevel_data,nameslist)  
DailyLevel_data$cum_u_names = NULL

# store the date in date column as correct format, and remove the old name
DailyLevel_data$date <- as.Date(DailyLevel_data$date_tip) 
DailyLevel_data$date_tip <- NULL



# merge with business data ----------------------------------------------------------------------------------------------------------



# load business data
load("business_Korean_Saint Louis.RData")

# check for variables that are logical vector, and convert to integer
integer64s = sapply(output_business,function(x){class(x)=="integer64"})
output_business[,integer64s] = as.integer(output_business[,integer64s])
business_data = output_business

# dealing with the missings
business_data$n_photo[is.na(business_data$n_photo)] = 0

business_data1 <- subset(business_data,select = -c(business_id)) 
# removed this because MICE does not like imputing factors with more than 50 levels

library(mice)

#inspect pattern of missings
md.business=md.pattern(business_data1)

if(md.business[length(md.business)]!=0){ # do the imputation only if there are missings
  # Below, the predictormatrix is specified
  # It is a square matrix of size ncol(data) containing 0/1 data specifying the set of predictors to be used for each target column 
  # Rows correspond to target variables (i.e. variables to be imputed), in the sequence as they appear in data
  # A value of '1' means that the column variable is used as a predictor for the target variable (in the rows)
  # The diagonal of predictorMatrix must be zero
  predictorMatrix <- matrix(0,nrow = ncol(business_data1), ncol = ncol(business_data1)) # Make a matrix of zeros
  colnames(predictorMatrix)=colnames(business_data1)
  row.names(predictorMatrix)=colnames(business_data1)
  predictorMatrix[c("business_price"),] <- 1 #variables "business_price" can be explained by all other variables
  diag(predictorMatrix) <- 0 #diagonal must be zero
  
  #impute data
  business_data1_data_imputed <- mice(business_data1, predictorMatrix = predictorMatrix, m=5, maxit = 50, seed = 500)
  
  summary(business_data1_data_imputed)
  
  #get one of the complete data sets ( 2nd out of 5)
  business_data_complete_data <- complete(business_data1_data_imputed,2)
  
  # bring back the business_id
  business_data_complete_data=cbind(business_id=business_data$business_id,business_data_complete_data)
  
}else{
  business_data_complete_data = business_data
}

# merge 2 dataset DailyLevel_data & business_data_complete_data using business id column
DailyLevel_data = DailyLevel_data%>%
  inner_join(business_data_complete_data,by="business_id")


# make factors out of chr variables
for(j in 1:ncol(DailyLevel_data)){
  if(typeof(DailyLevel_data[,j])=="character")
    DailyLevel_data[,j]=as.factor(DailyLevel_data[,j])
}

# limit the number of categories to Asian, American, Mexican and Others
cat_s=as.character(DailyLevel_data$business_cat)
new_cat_s=c("Others","Asian", "American", "Mexican")

changed=0
for(k in new_cat_s[-1]){
  cat_s[grepl(k,cat_s)]=k
  changed=changed+grepl(k,cat_s)
}
cat_s[changed==0]="Others"
DailyLevel_data$business_cat=as.factor(cat_s)

# n_photos==NA and cum_max_u_elite==NA are actually zeros, therefore, replace them with 0 before imputing
DailyLevel_data$cum_max_u_elite[is.na(DailyLevel_data$cum_max_u_elite)]=0

# check descriptives of the data
Hmisc::describe(DailyLevel_data)


#the complete dataset can be used to estimate predictive models
write.csv(DailyLevel_data, file="DailyLevel_data_tip_Imputed.csv")



# deploy of the functions used for the analysis -------------------------------------------------------------------------------------



extractweather=function(dataset,mindate=min(dataset$date),maxdate=max(dataset$date),
                        latrange=range(dataset$business_lat),longrange=range(dataset$business_long),
                        resol=.5,getdata=FALSE,
                        wear=ifelse("weatherPRCPSNWDSNOWTMAXTMINTOBS.RData"%in%list.files(),"available","navailable"),
                        cl=NULL){
  # queries weather data from ncdc.noaa.gov, in a grid format from mindate until maxdate
  # if not specified, takes the min and maxdate from dataset
  # the geographical range is latitudinal in latrange and longitudinal in long-range
  # the resolution of the grids is determined by resol
  # the data is stored in weatherPRCPSNWDSNOWTMAXTMINTOBS.RData ,
  # if already existing, the weather data is no more extracted
  
  wdatacond=wear=="navailable"
  if(getdata | wdatacond){
    if(is.null(cl)){
      require("doParallel")
      
      cl <- makeCluster(detectCores(),outfile="log1.txt")
      registerDoParallel(cl)
    }
    
    
    # read the station names
    stations=read.delim(url("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"),header = F,quote="",sep="")[,1:3]
    colnames(stations)=c("Station","lat","long")
    stations=stations[strtrim(stations$Station,2)=="US",]
    stations$lat=as.numeric(stations$lat)
    stations$long=as.numeric(stations$long)
    stations=stations[!is.na(stations$lat)|!is.na(stations$long),]
    
    # mindate=min(dataset$date)#"2016-05-01"#
    # maxdate=max(dataset$date)#"2016-05-02"#
    # latrange=range(dataset$business_lat)
    # longrange=range(dataset$business_long)
    
    latseq=c(seq(latrange[1],latrange[2],by=resol),latrange[2])
    longseq=c(seq(longrange[1],longrange[2],by=resol),longrange[2])
    
    wear=NULL
    k=0
    torunlist=NULL
    for(lat in 1:(length(latseq)-1)){#(length(latseq)-1)
      for(lon in 1:(length(longseq)-1)){
        k=k+1
        torunlist=rbind(torunlist,c(lat,lon))
      }
    }
    wear=foreach(i=1:k,.noexport=ls(),.export=c("latseq","longseq","stations","torunlist","mindate","maxdate"))%dopar%
      {  
        # find the station(s) within the boxes
        lat=torunlist[i,1]
        lon=torunlist[i,2]
        rangelat=c(latseq[lat+1],latseq[lat])
        rangelong=c(longseq[lon],longseq[lon+1])
        indx=(stations$lat>rangelat[2])&(stations$lat<rangelat[1])&(stations$long>rangelong[1])&(stations$long<rangelong[2])
        stations_temp=stations[indx,]
        stations_t=paste(stations_temp$Station,collapse=",")
        temp=paste0("dataset=daily-summaries&dataTypes=PRCP,SNWD,SNOW,TMAX,TMIN,TOBS",
                    "&stations=",stations_t,"&startDate=",mindate,"","&endDate=",maxdate)#,
        #"","&boundingBox=",paste(latseq[lat+1],longseq[lon],latseq[lat],longseq[lon+1],sep=","))##90,-180,-90,180
        valid_url <- TRUE
        a=tryCatch(read.csv(url(paste0("https://www.ncei.noaa.gov/access/services/data/v1?",temp))),error=function(e) {valid_url<<-FALSE})
        toreturn=NULL
        if(valid_url)
          toreturn=list(range=cbind(rangelat,rangelong),data=read.csv(url(paste0("https://www.ncei.noaa.gov/access/services/data/v1?",temp))))
        print(c(lat,lon,valid_url))
        return(toreturn)
        #print(c(lat,lon,valid_url))
      }
    
    #on.exit(stopCluster(cl))
    stopCluster(cl)
    save(file="weatherPRCPSNWDSNOWTMAXTMINTOBS.RData",list=c("wear"))
  }else{
    if(wear=="available"){
      load("weatherPRCPSNWDSNOWTMAXTMINTOBS.RData")
    }
  }
  return(wear)
}


weardailyavg=function(wear){
  # this function converts the extracted weather data into daily level data.
  if("weather_data.RData"%in%list.files()){
    load(file="weather_data.RData")
  }else{
    require("doParallel")
    
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    clusterCall(cl,function(x) {library(dplyr)})
    wear_avg=NULL
    k=0
    wear_avg=foreach(i=1:length(wear),.noexport=ls(),.export=c("wear"),.packages = c("dplyr"))%dopar%
      {
        if(is.null(wear[[i]])){
          temp=NULL
        }else{
          temp=wear[[i]]$data %>%
            group_by(DATE) %>%
            summarize(PRCP=mean(PRCP,na.rm = T),SNOW=mean(SNOW,na.rm = T),SNWD=mean(SNWD,na.rm = T),
                      TMAX=mean(TMAX,na.rm = T),TMIN=mean(TMIN,na.rm = T),TOBS=mean(TOBS,na.rm = T))
          temp=list(range=wear[[i]]$range,data=temp)}
        return(temp)
        
      }
    stopCluster(cl)
    weather=NULL
    k=0
    for(i in 1:length(wear_avg)){
      if(is.null(wear[[i]]))
        next
      k=k+1
      weather[[k]]=wear_avg[[i]]
      weather[[k]]$data$DATE=as.Date(weather[[k]]$data$DATE)
    }
    save(file="weather_data.RData",list=c("weather"))
  }
  return(weather)
}


makeLiftPlot <- function(Prediction, Evaluate, ModelName){
  # plots the liftplot, and computes the GINI coefficient.
  iPredictionsSorted <- sort(Prediction,index.return=T,decreasing=T)[2]$ix #extract the index order according to predicted retention
  CustomersSorted <- Evaluate$ch_in_string[iPredictionsSorted] #sort the true behavior of customers according to predictions
  SumChurnReal<- sum(Evaluate$ch_in_string == "ch_in") #total number of real churners in the evaluation set
  CustomerCumulative=seq(nrow(Evaluate))/nrow(Evaluate) #cumulative fraction of customers
  ChurnCumulative=apply(matrix(CustomersSorted=="ch_in"),2,cumsum)/SumChurnReal #cumulative fraction of churners
  ProbTD = sum(CustomersSorted[1:floor(nrow(Evaluate)*.1)]=="ch_in")/floor(nrow(Evaluate)*.1) #probability of churn in 1st decile
  ProbOverall = SumChurnReal / nrow(Evaluate) #overall churn probability
  TDL = ProbTD / ProbOverall
  GINI = sum((ChurnCumulative-CustomerCumulative)/(t(matrix(1,1,nrow(Evaluate))-CustomerCumulative)),na.rm=T)/nrow(Evaluate)
  plot(CustomerCumulative,ChurnCumulative,type="l",main=paste("Lift Curve of", ModelName),xlab="Cumulative fraction of check-ins (sorted by predicted check-in probability)",ylab="Cumulative fraction of check-ins")
  lines(c(0,1),c(0,1),col="blue",type="l",pch=22, lty=2)
  legend(.66,.2,c("According to model","Random selection"),cex=0.8,  col=c("black","blue"), lty=1:2)
  text(0.15,1,paste("TDL = ",round(TDL,2), "; GINI = ", round(GINI,2) ))
  return(data.frame(TDL,GINI))
}



# extract the weather data, and add it to the yelp data -----------------------------------------------------------------------------



if(0){
  #load data
  yelp_data <- read.csv("DailyLevel_data_tip_Imputed.csv",header=TRUE,skipNul = T) #read csv file
  yelp_data$date <- as.Date(yelp_data$date)
  yelp_data$X=NULL
  
  # read the temperature data
  
  rangeX=.01
  latrange=range(yelp_data$business_lat)*c(1-rangeX,1+rangeX) 
  longrange=range(yelp_data$business_long)*c(1+rangeX,1-rangeX)
  
  cl <- makeCluster(detectCores(),outfile="log1.txt")
  registerDoParallel(cl)
  
  wear=extractweather(yelp_data,latrange=latrange,longrange=longrange,resol=.25, cl=cl)
  
  nocl=FALSE
  tryCatch(stopCluster(cl),error=function(e) {nocl<<-TRUE})
  if(!nocl){
    stopCluster(cl)
  }
  
  # take the averages across stations for each coordinate
  weather=weardailyavg(wear)
  
  
  dates=sort(unique(yelp_data$date))
  weatherstations=as.data.frame(t(sapply(weather,function(x){colMeans(x$range)})))
  
  # adding weather data to yelp_data
  if(1){
    stations_by=t(apply(yelp_data[,c("business_lat","business_long")],1,
                        function(x){a=sort((x[1]-weatherstations$rangelat)^2+
                                             (x[2]-weatherstations$rangelong)^2,index.return=T)
                        return(a$ix[1:50])})) # finding the 50 closest stations
    
    # add for example, temperature forecasts to the weather data
    for(i in 1:length(weather)){
      if(nrow(weather[[i]]$data)==0)
        next
      store_weather=weather[[i]]$data
      store_weather$TOBS_1=c(store_weather$TOBS[2:nrow(store_weather)],NA)
      store_weather$TOBS_2=c(store_weather$TOBS[3:nrow(store_weather)],NA,NA)
      store_weather$TOBS_3=c(store_weather$TOBS[4:nrow(store_weather)],NA,NA,NA)
      store_weather$TOBS_4=c(store_weather$TOBS[5:nrow(store_weather)],NA,NA,NA,NA)
      weather[[i]]$data=store_weather
    }
    weatherinf=colnames(store_weather)[-1] 
    
    yelp_data_weather=NULL
    for(i in 1:length(weather)){
      k=1 # start with the closest station
      stores_in=stations_by[,k]==i
      if(sum(stores_in)==0)
        next
      store_weather=weather[[i]]$data
      
      temp=yelp_data[stores_in,]
      temp=merge(temp,store_weather,by.x="date",by.y="DATE",all.x=T)
      yelp_data_weather=rbind(yelp_data_weather,temp)
      print(i)
    }
    
    # now deal with the missings, by going to the next possible station
    temp_indx=is.na(yelp_data_weather[,"TOBS"])|is.na(yelp_data_weather[,"PRCP"])
    k_changed=NULL
    for(i in which(temp_indx)){
      temp_date=yelp_data_weather[i,]$date
      for(k in 2:ncol(stations_by)){
        temp=weather[[stations_by[i,k]]]$data
        if(!is.na(as.numeric(temp[temp$DATE==temp_date,"TOBS"]))&!is.na(as.numeric(temp[temp$DATE==temp_date,"PRCP"])))
          break
      }
      k_changed=c(k_changed,k)
      
      yelp_data_weather[i,weatherinf]=temp[temp$DATE==temp_date,-1]
      #print(i)
    }
    
    # add weekends and quarters
    temp=weekdays(yelp_data_weather$date,abbreviate = T)
    yelp_data_weather$WE=temp=="Sat"|temp=="Sun"
    
    yelp_data_weather$Quarter=as.factor(quarters(yelp_data_weather$date))
    
    #save(file="yelp_data_weather.RData",list=c("yelp_data_weather"))
    write.csv(yelp_data_weather,file="yelp_data_tip_weather.csv")
    
  }
  
}



# adjusting the yelp-data + weather data --------------------------------------------------------------------------------------------



# load the dataset
yelp_data_weather = read.csv(file="yelp_data_tip_weather.csv")

# adjustments to the imported data

yelp_data = yelp_data_weather

yelp_data$date = as.Date(yelp_data$date)
yelp_data$ch_in_string[yelp_data$ch_in>=1]="ch_in"
yelp_data$ch_in_string[yelp_data$ch_in==0]="Noch_in"
yelp_data$ch_in_string <- as.factor(yelp_data$ch_in_string)

# since the performance evaluations are mainly made to check for the minority class, in our case "ch_in"
# therefore, set "noch_in" set reference level
yelp_data$ch_in_string <- relevel(yelp_data$ch_in_string,ref="Noch_in") 

# convert variables to categorical data
yelp_data$business_park=as.factor(yelp_data$business_park)
yelp_data$business_open=as.factor(yelp_data$business_open)
yelp_data$business_cat=as.factor(yelp_data$business_cat)
yelp_data$WE=as.factor(yelp_data$WE)
yelp_data$Quarter=as.factor(yelp_data$Quarter)



# statistics & correlations of dataset-----------------------------------------------------------------------------------------------



numeric_vars <- sapply(yelp_data, is.numeric)
numeric_vars["X"] <- FALSE # remove variable X which is row number from csv file
yelp_numeric <- yelp_data[, numeric_vars]

stargazer(yelp_numeric, type = "text", digits = 3, summary.stat = c("n", "mean", "sd", "min", "median", "max"))

cor_matrix <- cor(yelp_numeric, use = "pairwise.complete.obs")

corrplot(cor_matrix,
         method = "color",      
         type = "upper",       
         tl.cex = 0.8,          
         tl.col = "black",      
         number.cex = 0.7)



# split business_cat, business_open, WE, Quarter variables to smaller variables-----------------------------------------------------------------------------------------------



business_cat_dummies <- model.matrix(~ business_cat - 1, data = yelp_data)
yelp_data <- cbind(yelp_data, business_cat_dummies)

yelp_data$business_open <- as.logical(yelp_data$business_open)
yelp_data$business_open_TRUE <- ifelse(yelp_data$business_open == TRUE, 1, 0)
yelp_data$business_open_FALSE <- ifelse(yelp_data$business_open == TRUE, 1, 0)

yelp_data$WE <- as.logical(yelp_data$WE)
yelp_data$WE_TRUE <- ifelse(yelp_data$WE == TRUE, 1, 0)
yelp_data$WE_FALSE <- ifelse(yelp_data$WE == FALSE, 1, 0)


yelp_data$Quarter <- as.factor(yelp_data$Quarter)
quarter_dummies <- model.matrix(~ Quarter - 1, data = yelp_data)
yelp_data <- cbind(yelp_data, quarter_dummies)

yelp_data$WE <- NULL
yelp_data$Quarter <- NULL
yelp_data$business_cat <- NULL
yelp_data$business_open <- NULL



# variable grouping and explanation-----------------------------------------------------------------------------------------------



# 1) Static variables (physical/business attributes)

business_cat_cols <- grep("^business_cat", colnames(yelp_data), value = TRUE)
static_vars <- c("ch_in", "business_id", "business_open_TRUE", "business_open_FALSE", 
                 "business_price", 
                 "business_lat", "business_long", "business park",
                 business_cat_cols)

static_meaning <- c(
  "Check-in indicator",
  "Unique business identifier",
  "Business is open",
  "Business is not open",
  "Business price category",
  "Latitude coordinate of business",
  "Longitude coordinate of business",
  "Business park",
  rep("Business category", length(business_cat_cols))
)

static_table <- data.frame(Variable = static_vars, Meaning = static_meaning)

print("Static variables (physical/business attributes)")
print(static_table)

# 2) Dynamic variables (user/activity data)
dynamic_vars <- c("ch_in", "cum_n_tips", "cum_max_friends", "cum_max_u_elite", "cum_max_us_fans",
                  "cum_max_us_tip", "male", "female", "n_photo")

dynamic_meaning <- c(
  "Check-in indicator",
  "Cumulative number of tips",
  "Cumulative max friends",
  "Cumulative max elite users",
  "Cumulative max fans",
  "Cumulative max tips by users",
  "Count of male users involved",
  "Count of female users involved",
  "Number of photos"
)

dynamic_table <- data.frame(Variable = dynamic_vars, Meaning = dynamic_meaning)

print("Dynamic variables (user-generated data)")
print(dynamic_table)

# 3) Temporal variables (time-related)
temporal_vars <- c("ch_in", "WE_TRUE", "WE_FALSE", grep("^Quarter", colnames(yelp_data), value=TRUE))

temporal_meaning <- c(
  "Check-in indicator",
  "Day is weekend",
  "Day is not weekend",
  paste(gsub("Quarter", "Quarter ", grep("^Quarter", colnames(yelp_data), value=TRUE)))
)

temporal_table <- data.frame(Variable = temporal_vars, Meaning = temporal_meaning)

print("Temporal variables (time-related)")
print(temporal_table)

# 4) External variables (weather data)
external_vars <- c("ch_in", "PRCP", "SNOW", "SNWD", "TMAX", "TMIN", "TOBS", "TOBS_1", "TOBS_2", "TOBS_3", "TOBS_4")

external_meaning <- c(
  "Check-in indicator",
  "Precipitation amount",
  "Snowfall amount",
  "Snow depth",
  "Maximum temperature",
  "Minimum temperature",
  "Observed temperature",
  "Observed temperature at hour 1",
  "Observed temperature at hour 2",
  "Observed temperature at hour 3",
  "Observed temperature at hour 4"
)

external_table <- data.frame(Variable = external_vars, Meaning = external_meaning)

print("External variables (weather data)")
print(external_table)



# statistics & correlations of data groups-----------------------------------------------------------------------------------------------------------



get_numeric_corr <- function(vars, data){
  vars_exist <- vars[vars %in% colnames(data)] # select only variables that exist and are numeric
  numeric_vars <- vars_exist[sapply(data[, vars_exist], is.numeric)]
  if(length(numeric_vars) < 2){
    message("Not enough numeric variables for correlation in category.")
    return(NULL)
  }
  cor_mat <- cor(data[, numeric_vars], use = "complete.obs")
  return(list(cor_mat = cor_mat, vars = numeric_vars))
}

plot_corr <- function(cor_mat, title){
  corrplot(cor_mat, method = "color", type = "upper", tl.cex = 0.8, tl.col = "black",
           number.cex = 0.7, main = title)
}

# static variables
static_corr <- get_numeric_corr(static_vars, yelp_data)
if(!is.null(static_corr)) plot_corr(static_corr$cor_mat, title = "")
stargazer(yelp_data[, static_corr$vars], type = "text",
          digits = 3, summary.stat = c("n", "mean", "sd", "min", "median", "max"))

# dynamic variables
dynamic_corr <- get_numeric_corr(dynamic_vars, yelp_data)
if(!is.null(dynamic_corr)) plot_corr(dynamic_corr$cor_mat, title = "")
stargazer(yelp_data[, dynamic_corr$vars], type = "text",
          digits = 3, summary.stat = c("n", "mean", "sd", "min", "median", "max"))

# temporal variables
temporal_corr <- get_numeric_corr(temporal_vars, yelp_data)
if(!is.null(temporal_corr)) plot_corr(temporal_corr$cor_mat, title = "")
stargazer(yelp_data[, temporal_corr$vars], type = "text",
          digits = 3, summary.stat = c("n", "mean", "sd", "min", "median", "max"))

# external variables
external_corr <- get_numeric_corr(external_vars, yelp_data)
if(!is.null(external_corr)) plot_corr(external_corr$cor_mat, title = "")
stargazer(yelp_data[, external_corr$vars], type = "text",
          digits = 3, summary.stat = c("n", "mean", "sd", "min", "median", "max"))



# predictive  models ----------------------------------------------------------------------------------------------------------------



# drop columns due to low predictive power and redundant: X, date, business_id, n_photo
#                                                         business_open_TRUE, business_open_FALSE,
#                                                         business_park, SNWD, TMAX, TMIN, 
#                                                         TOBS_1, TOBS_2, TOBS_3, TOBS_4

# split the dataset randomly
set.seed(66)

# make a copy of original data
yelp_data_na=yelp_data

# list of variables in the model
varsin=c("ch_in_string","ch_in","WE_TRUE","WE_FALSE",
         "QuarterQ1", "QuarterQ2", "QuarterQ3", "QuarterQ4",
         "business_price",
         "business_catAmerican", "business_catAsian", "business_catMexican", "business_catOthers",
         "TOBS","PRCP", "SNOW", 
         "business_lat", "business_long",
         "female","male","cum_n_tips","cum_max_friends",
         "cum_max_u_elite","cum_max_us_fans","cum_max_us_tip")


# subset data to the selected columns
yelp_data=subset(yelp_data,select=varsin)

# check size of dataset
datasetsize=nrow(yelp_data)/1 

# randomly rearrange the dataset
x <- yelp_data[sample(1:nrow(yelp_data), datasetsize, replace = F),]

# split dataset into train and evaluate sets 
# split ratio: 72:25
x.train <- x[1:floor(nrow(x)*.75), ]
x.evaluate <- x[(floor(nrow(x)*.75)+1):nrow(x), ]

# set up model formulas
# predict ch_in_string (factor version: "ch_in" or "Noch_in") using all other variables except ch_in_string and ch_in
BaseFormula <- as.formula(paste0("ch_in_string~",paste(varsin[-c(1,2)],collapse = "+")))
# predict ch_in (numeric/binary) using the same predictors
BaseFormula1 <- as.formula(paste0("ch_in~",paste(varsin[-c(1,2)],collapse = "+")))


# create dummies (required for SMOTE)
x.traindum=cbind(x.train[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.train),newdata = x.train))
x.evaluatedum=cbind(x.evaluate[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.evaluate),newdata = x.evaluate))

# class imbalance check
temp=table(x.train[,"ch_in_string"])
print(temp)

# if yes, do random over-sampling:
if(0){
  oversampled=x.train[x.train$ch_in_string==names(temp)[sort.int(temp,index.return=T,decreasing = T)$ix[1]],]
  minclass=names(temp)[sort.int(temp,index.return=T)$ix[1]]
  for(m in 1:(length(temp)-1)){
    minchclass=names(temp)[sort.int(temp,index.return=T)$ix[m]]
    minclassdat=x.train[x.train$ch_in_string==minchclass,]
    minclassdat=minclassdat[sample(1:nrow(minclassdat), sort(temp,decreasing = T)[1] , replace = T),]
    oversampled=rbind(oversampled,minclassdat)
  }
  x.train=oversampled
}

# or do SMOTE:
if(1){
  x.traindum_smote=SMOTE(x.traindum[,-c(1,2)],x.traindum[,2])$data
  names(x.traindum_smote)[ncol(x.traindum_smote)]="ch_in_string"
  x.traindum_smote$ch_in=ifelse(x.traindum_smote$ch_in_string=="ch_in",1,0)
  x.traindum_smote$ch_in_string=as.factor(x.traindum_smote$ch_in_string)
  x.traindum=x.traindum_smote
  rm(x.traindum_smote)
}

sum(is.na(x.traindum[,-c(1,2)]))
sum(is.na(x.traindum[,2]))
colSums(is.na(x.traindum[,-c(1,2)]))

# check for class distribution again
temp=table(x.traindum[,"ch_in_string"])
print(temp)

# there is no class imbalance now


# data for Heuristic machine learning methods

# normalize data (for ML techniques, except logistic regression)

x.trainnorm=predict(preProcess(x.traindum, method = "range"), newdata=x.traindum)
x.evaluatenorm=predict(preProcess(x.evaluatedum, method = "range"), newdata=x.evaluatedum)

# adjust Baseformula to the  version of the data
varsin_dum=varsin[1:2]
for(i in 3:length(varsin)){
  if(!is.null(levels(x[,varsin[i]]))){
    for(j in 2:nlevels(x[,varsin[i]])){ # first level will be considered as the base-level
      varsin_dum=c(varsin_dum,paste(varsin[i],levels(x[,varsin[i]])[j],sep="."))
    }
  }else{
    varsin_dum=c(varsin_dum,varsin[i])
  }
}

# redo the releveling:
x.traindum$ch_in_string=relevel(x.traindum$ch_in_string,ref="Noch_in") 
x.evaluatedum$ch_in_string=relevel(x.evaluatedum$ch_in_string,ref="Noch_in")
x.trainnorm$ch_in_string=relevel(x.trainnorm$ch_in_string,ref="Noch_in") 
x.evaluatenorm$ch_in_string=relevel(x.evaluatenorm$ch_in_string,ref="Noch_in")


BaseFormula_dum <- as.formula(paste0("ch_in_string~",paste(varsin_dum[-c(1,2)],collapse = "+")))
BaseFormula1_dum <- as.formula(paste0("ch_in~",paste(varsin_dum[-c(1,2)],collapse = "+")))

# set threshold probability, usually .5, 
# but better is to set it to the portion of 1's, rather than the default 0.5, which is especially useful for imbalanced datasets
probthres=mean(x.traindum$ch_in)



# 10-fold cross validation settings --------------------------------------------------------------------------------------



ctrl <- trainControl(
  method = "cv", 
  number = 10,
  search = "grid",
  classProbs = TRUE,
  savePredictions = "final",
  allowParallel = FALSE
)



# model training and hyperparameter tunning --------------------------------------------------------------------------------------------------------------------------



# function for evaluation metrics
get_metrics <- function(pred, pred_class, reference, tdl, gini, time_elapsed = NA) {
  cm <- confusionMatrix(factor(pred_class, levels = c("Noch_in", "ch_in")),
                        factor(reference, levels = c("Noch_in", "ch_in")))
  auc <- (gini + 1) / 2
  #rocobj <- tryCatch({
  #  roc(actual, probs, levels = c("Noch_in", "ch_in"), direction = "<")
  #}, error=function(e) NA)
  #auc <- if (is.na(rocobj)[1]) NA else as.numeric(auc(rocobj))
  #auc <- tryCatch({
  #  rocobj <- pROC::roc(reference == "ch_in", pred)
  #  as.numeric(pROC::auc(rocobj))
  #}, error = function(e) NA)
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  specificity <- cm$byClass["Specificity"]
  f1 <- cm$byClass["F1"]
  accuracy <- as.numeric(cm$overall["Accuracy"])
  metrics <- c(
    TDL = tdl,
    GINI = gini,
    AUC = auc,
    F1 = f1 * 100,
    Precision = precision * 100,
    Recall = recall * 100,
    Accuracy = accuracy * 100,
    Specificity = specificity * 100,
    Time = time_elapsed
  )
  names(metrics) <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall", "Accuracy", "Specificity", "Time")
  return(metrics)
}



######### 1) LOGISTIC REGRESSION -----------------------------------------------

### BASE MODEL

ptm_base <- proc.time()

set.seed(123)
x.modelLogit_base <- train(BaseFormula_dum, data = x.traindum, 
                         family = "binomial", trControl = ctrl) 
summary(x.modelLogit_base)

base_time <- proc.time() - ptm_base

x.evaluate$predictionlogit_base <- predict(x.modelLogit_base, newdata=x.evaluatedum, type = "prob")[, "ch_in"]
x.evaluate$predictionlogitclass_base <- ifelse(x.evaluate$predictionlogit_base > probthres, "ch_in", "Noch_in")
x.evaluate$correctlogit_base <- x.evaluate$predictionlogitclass_base == x.evaluate$ch_in_string

LogitOutput_base <- makeLiftPlot(x.evaluate$predictionlogit_base, x.evaluate, "Logistic Regression (Base Model)")
LogitOutput_base$PercCorrect <- mean(x.evaluate$correctlogit_base)*100
Logitconfmatrix_base <- table(x.evaluate$predictionlogitclass_base, x.evaluate$ch_in_string)
LogitOutput_base$TimeElapsed <- base_time[3]

### HYPERPARAMETER TUNING

tune_grid <- expand.grid(
  alpha = 2,
  lambda = 0.05
)

ptm_tune <- proc.time()

set.seed(123)
x.modelLogit_tuned <- train(
  BaseFormula_dum, data = x.traindum, method = "glmnet",
  family = "binomial",
  trControl = ctrl,
  tuneGrid = tune_grid
)

tuned_time <- proc.time() - ptm_tune

print(x.modelLogit_tuned$bestTune)

x.evaluate$predictionlogit_tuned <- predict(x.modelLogit_tuned, newdata=x.evaluatedum, type = "prob")[, "ch_in"]
x.evaluate$predictionlogitclass_tuned <- ifelse(x.evaluate$predictionlogit_tuned > probthres, "ch_in", "Noch_in")
x.evaluate$correctlogit_tuned <- x.evaluate$predictionlogitclass_tuned == x.evaluate$ch_in_string

LogitOutput_tuned <- makeLiftPlot(x.evaluate$predictionlogit_tuned, x.evaluate, "Logistic Regression (Tuned Model)")
LogitOutput_tuned$PercCorrect <- mean(x.evaluate$correctlogit_tuned)*100
Logitconfmatrix_tuned <- table(x.evaluate$predictionlogitclass_tuned, x.evaluate$ch_in_string)
LogitOutput_tuned$TimeElapsed <- tuned_time[3]

### EVALUATION

print("")
print(" # Model 1: Logistic Regression (Base)")
print("Confusion matrix:")
print(Logitconfmatrix_base)

print("")
print(" # Model 1: Logistic Regression (Tuned)")
print(paste("Best alpha:", x.modelLogit_tuned$bestTune$alpha))
print(paste("Best lambda:", x.modelLogit_tuned$bestTune$lambda))
print("Confusion matrix:")
print(Logitconfmatrix_tuned)

lg_base <- get_metrics(
  pred = x.evaluate$predictionlogit_base,
  pred_class = x.evaluate$predictionlogitclass_base,
  reference = x.evaluate$ch_in_string,
  tdl = LogitOutput_base$TDL,
  gini = LogitOutput_base$GINI,
  time_elapsed = LogitOutput_base$TimeElapsed
)

lg_tuned <- get_metrics(
  pred = x.evaluate$predictionlogit_tuned,
  pred_class = x.evaluate$predictionlogitclass_tuned,
  reference = x.evaluate$ch_in_string,
  tdl = LogitOutput_tuned$TDL,
  gini = LogitOutput_tuned$GINI,
  time_elapsed = LogitOutput_tuned$TimeElapsed
)

metrics <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall", "Accuracy", "Specificity", "Time")

results_df <- data.frame(
  Metrics = metrics,
  Base = as.numeric(lg_base),
  Tuned = as.numeric(lg_tuned)
)

stargazer(
  results_df,
  summary = FALSE, rownames = FALSE,
  title = "Logistic Regression Model Performance",
  type = "text", digits = 3
)

# the best tuned model returns performance metrics that are relatively close to the base model
# Recall and Accuracy dropped significantly, meanwhile Specificity increased dramatically
# therefore, considering the trade-offs, the best model for LG is base model 

Logit <- x.modelLogit_base
LogitOutput <- LogitOutput_base
Logitconfmatrix <- Logitconfmatrix_base
logit <- lg_base
x.evaluate$predictionlogit <- x.evaluate$predictionlogit_base
predictionlogit <- x.evaluate$predictionlogit_base



############ 2) NAIVE BAYES ----------------------------------------------------

### BASE MODEL

ptm_nb_base <- proc.time()

set.seed(123)
x.modelNB_base <- train(BaseFormula_dum, data = x.trainnorm, 
                        method = "naive_bayes", trControl = ctrl)

nb_base_time <- proc.time() - ptm_nb_base

x.evaluate$predictionNB_base <- predict(x.modelNB_base, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionNBclass_base <- ifelse(x.evaluate$predictionNB_base[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$correctNB_base <- x.evaluate$predictionNBclass_base == x.evaluate$ch_in_string

NBOutput_base <- makeLiftPlot(x.evaluate$predictionNB_base[,"ch_in"], x.evaluate, "Naive Bayes (Base Model)")
NBOutput_base$PercCorrect <- mean(x.evaluate$correctNB_base)*100
NBconfmatrix_base <- table(x.evaluate$predictionNBclass_base, x.evaluate$ch_in_string)
NBOutput_base$TimeElapsed <- nb_base_time[3]

### HYPERPARAMETER TUNING

grid_nb <- expand.grid(
  laplace   = 10,
  usekernel = TRUE,
  adjust    = 1
)

ptm_nb_tune <- proc.time()

set.seed(123)
x.modelNB_tuned <- train(
  BaseFormula_dum, data = x.trainnorm, method = "naive_bayes",
  trControl = ctrl,
  tuneGrid = grid_nb
)
nb_tuned_time <- proc.time() - ptm_nb_tune

x.evaluate$predictionNB_tuned <- predict(x.modelNB_tuned, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionNBclass_tuned <- ifelse(x.evaluate$predictionNB_tuned[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$correctNB_tuned <- x.evaluate$predictionNBclass_tuned == x.evaluate$ch_in_string

NBOutput_tuned <- makeLiftPlot(x.evaluate$predictionNB_tuned[,"ch_in"], x.evaluate, "Naive Bayes (Tuned Model)")
NBOutput_tuned$PercCorrect <- mean(x.evaluate$correctNB_tuned)*100
NBconfmatrix_tuned <- table(x.evaluate$predictionNBclass_tuned, x.evaluate$ch_in_string)
NBOutput_tuned$TimeElapsed <- nb_tuned_time[3]

### EVALUATION

print("")
print(" # Model 2: Naive Bayes (Base)")
print("Confusion matrix:")
print(NBconfmatrix_base)

print("")
print(" # Model 2: Naive Bayes (Tuned)")
print(paste("Best laplace:", x.modelNB_tuned$bestTune$laplace))
print(paste("Best usekernel:", x.modelNB_tuned$bestTune$usekernel))
print(paste("Best adjust:", x.modelNB_tuned$bestTune$adjust))
print("Confusion matrix:")
print(NBconfmatrix_tuned)

nb_base <- get_metrics(
  pred = x.evaluate$predictionNB_base,
  pred_class = x.evaluate$predictionNBclass_base,
  reference = x.evaluate$ch_in_string,
  tdl = NBOutput_base$TDL,
  gini = NBOutput_base$GINI,
  time_elapsed = NBOutput_base$TimeElapsed
)

nb_tuned <- get_metrics(
  pred = x.evaluate$predictionNB_tuned,
  pred_class = x.evaluate$predictionNBclass_tuned,
  reference = x.evaluate$ch_in_string,
  tdl = NBOutput_tuned$TDL,
  gini = NBOutput_tuned$GINI,
  time_elapsed = NBOutput_tuned$TimeElapsed
)

metrics <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall", "Accuracy", "Specificity", "Time")

results_df <- data.frame(
  Metrics = metrics,
  Base = as.numeric(nb_base),
  Tuned = as.numeric(nb_tuned)
)

stargazer(
  results_df,
  summary = FALSE, rownames = FALSE,
  title = "Naive Bayes Model Performance",
  type = "text", digits = 3
)

# the best tuned model returns performance metrics that are exactly same as base model
# additionally, the base model achieves relatively good performance already
# therefore, the best model for NB is base model 

NB <- x.modelNB_base
NBOutput <-NBOutput_base
NBconfmatrix <- NBconfmatrix_base
nb <- nb_base
x.evaluate$predictionNB <- x.evaluate$predictionNB_base
predictionNB <- x.evaluate$predictionNB_base



############ 3) K-NEAREST NEIGHBORS (KNN) --------------------------------------

### BASE MODEL

ptm_knn_base <- proc.time()

set.seed(123)
x.modelKNN_base <- train(
  BaseFormula_dum, data = x.trainnorm, method = "knn",
  trControl = ctrl
)

knn_base_time <- proc.time() - ptm_knn_base

x.evaluate$predictionKNN_base <- predict(x.modelKNN_base, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionKNNclass_base <- ifelse(x.evaluate$predictionKNN_base[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$correctKNN_base <- x.evaluate$predictionKNNclass_base == x.evaluate$ch_in_string

KNNOutput_base <- makeLiftPlot(x.evaluate$predictionKNN_base[,"ch_in"], x.evaluate, "KNN (Base Model)")
KNNOutput_base$PercCorrect <- mean(x.evaluate$correctKNN_base)*100
KNNconfmatrix_base <- table(x.evaluate$predictionKNNclass_base, x.evaluate$ch_in_string)
KNNOutput_base$TimeElapsed <- knn_base_time[3]

### HYPERPARAMETER TUNING

grid_knn <- expand.grid(k = 25)

ptm_knn_tune <- proc.time()

set.seed(123)
x.modelKNN_tuned <- train(
  BaseFormula_dum, data = x.trainnorm, method = "knn",
  trControl = ctrl,
  tuneGrid = grid_knn
)
knn_tuned_time <- proc.time() - ptm_knn_tune

x.evaluate$predictionKNN_tuned <- predict(x.modelKNN_tuned, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionKNNclass_tuned <- ifelse(x.evaluate$predictionKNN_tuned[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$correctKNN_tuned <- x.evaluate$predictionKNNclass_tuned == x.evaluate$ch_in_string

KNNOutput_tuned <- makeLiftPlot(x.evaluate$predictionKNN_tuned[,"ch_in"], x.evaluate, "KNN (Tuned Model)")
KNNOutput_tuned$PercCorrect <- mean(x.evaluate$correctKNN_tuned)*100
KNNconfmatrix_tuned <- table(x.evaluate$predictionKNNclass_tuned, x.evaluate$ch_in_string)
KNNOutput_tuned$TimeElapsed <- knn_tuned_time[3]

### EVALUATION

print("")
print(" # Model 3: KNN (Base)")
print("Confusion matrix:")
print(KNNconfmatrix_base)

print("")
print(" # Model 3: KNN (Tuned)")
print(paste("Best k:", x.modelKNN_tuned$bestTune$k))
print("Confusion matrix:")
print(KNNconfmatrix_tuned)

knn_base <- get_metrics(
  pred = x.evaluate$predictionKNN_base,
  pred_class = x.evaluate$predictionKNNclass_base,
  reference = x.evaluate$ch_in_string,
  tdl = KNNOutput_base$TDL,
  gini = KNNOutput_base$GINI,
  time_elapsed = KNNOutput_base$TimeElapsed
)

knn_tuned <- get_metrics(
  pred = x.evaluate$predictionKNN_tuned,
  pred_class = x.evaluate$predictionKNNclass_tuned,
  reference = x.evaluate$ch_in_string,
  tdl = KNNOutput_tuned$TDL,
  gini = KNNOutput_tuned$GINI,
  time_elapsed = KNNOutput_tuned$TimeElapsed
)

metrics <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall", "Accuracy", "Specificity", "Time")

results_df <- data.frame(
  Metrics = metrics,
  Base = as.numeric(knn_base),
  Tuned = as.numeric(knn_tuned)
)

stargazer(
  results_df,
  summary = FALSE, rownames = FALSE,
  title = "KNN Model Performance",
  type = "text", digits = 3
)

# the best tuned model performs better than the base model in all important metrics (TDL, GINI, AUC, Precision)
# only 1.6% dropped in F1 due to 3.2% dropped in Recall but Precision relatively stays the same (increased only 0.3%)
# Specificity increased double
# therefore, the best model for KNN is tuned model

KNN <- x.modelKNN_tuned
KNNOutput <- KNNOutput_tuned
KNNconfmatrix <- KNNconfmatrix_tuned
knn <- knn_tuned
x.evaluate$predictionKNN <- x.evaluate$predictionKNN_tuned
predictionKNN <- x.evaluate$predictionKNN_tuned



############ 4) SUPPORT VECTOR MACHINE (SVM) -----------------------------------

### BASE MODEL

ptm_svm_base <- proc.time()

set.seed(123)
x.modelSVM_base <- train(
  BaseFormula_dum, data = x.trainnorm, method = "svmRadial",
  cachesize=12000, tolerance=.01,
  trControl = ctrl
)

svm_base_time <- proc.time() - ptm_svm_base

x.evaluate$predictionSVM_base <- predict(x.modelSVM_base, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionSVMclass_base <- ifelse(x.evaluate$predictionSVM_base[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$correctSVM_base <- x.evaluate$predictionSVMclass_base == x.evaluate$ch_in_string

SVMOutput_base <- makeLiftPlot(x.evaluate$predictionSVM_base[,"ch_in"], x.evaluate, "SVM (Base Model)")
SVMOutput_base$PercCorrect <- mean(x.evaluate$correctSVM_base)*100
SVMconfmatrix_base <- table(x.evaluate$predictionSVMclass_base, x.evaluate$ch_in_string)
SVMOutput_base$TimeElapsed <- svm_base_time[3]

### HYPERPARAMETER TUNING

svm_grid <- expand.grid(
  C = 2,
  sigma = 0.2
)

ptm_svm_tune <- proc.time()

set.seed(123)
x.modelSVM_tuned <- train(
  BaseFormula_dum, data = x.trainnorm, method = "svmRadial",
  cachesize=12000, tolerance=.01,
  trControl = ctrl,
  tuneGrid = svm_grid
)
svm_tuned_time <- proc.time() - ptm_svm_tune

x.evaluate$predictionSVM_tuned <- predict(x.modelSVM_tuned, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionSVMclass_tuned <- ifelse(x.evaluate$predictionSVM_tuned[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$correctSVM_tuned <- x.evaluate$predictionSVMclass_tuned == x.evaluate$ch_in_string

SVMOutput_tuned <- makeLiftPlot(x.evaluate$predictionSVM_tuned[,"ch_in"], x.evaluate, "SVM (Tuned Model)")
SVMOutput_tuned$PercCorrect <- mean(x.evaluate$correctSVM_tuned)*100
SVMconfmatrix_tuned <- table(x.evaluate$predictionSVMclass_tuned, x.evaluate$ch_in_string)
SVMOutput_tuned$TimeElapsed <- svm_tuned_time[3]

### EVALUATION

print("")
print(" # Model 4: SVM (Base)")
print("Confusion matrix:")
print(SVMconfmatrix_base)

print("")
print(" # Model 4: SVM (Tuned)")
print(paste("Best C:", x.modelSVM_tuned$bestTune$C))
print(paste("Best sigma:", x.modelSVM_tuned$bestTune$sigma))
print("Confusion matrix:")
print(SVMconfmatrix_tuned)

svm_base <- get_metrics(
  pred = x.evaluate$predictionSVM_base,
  pred_class = x.evaluate$predictionSVMclass_base,
  reference = x.evaluate$ch_in_string,
  tdl = SVMOutput_base$TDL,
  gini = SVMOutput_base$GINI,
  time_elapsed = SVMOutput_base$TimeElapsed
)

svm_tuned <- get_metrics(
  pred = x.evaluate$predictionSVM_tuned,
  pred_class = x.evaluate$predictionSVMclass_tuned,
  reference = x.evaluate$ch_in_string,
  tdl = SVMOutput_tuned$TDL,
  gini = SVMOutput_tuned$GINI,
  time_elapsed = SVMOutput_tuned$TimeElapsed
)

metrics <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall", "Accuracy", "Specificity", "Time")

results_df <- data.frame(
  Metrics = metrics,
  Base = as.numeric(svm_base),
  Tuned = as.numeric(svm_tuned)
)

stargazer(
  results_df,
  summary = FALSE, rownames = FALSE,
  title = "SVM Model Performance",
  type = "text", digits = 3
)

# the more tuning, the worse the tuned model gets
# all metrics of tuned model become lower except F1, Recall, Accuracy but those are not the most important metrics
# therefore, the best model for SVM is the base model

SVM <- x.modelSVM_base
SVMOutput <- SVMOutput_base
SVMconfmatrix <- SVMconfmatrix_base
svm <- svm_base
x.evaluate$predictionSVM <- x.evaluate$predictionSVM_base
predictionSVM <- x.evaluate$predictionSVM_base



########## 5) DECISION TREE ----------------------------------------------------

### BASE MODEL

ptm_tree_base <- proc.time()

set.seed(123)
x.modelTree_base <- train(
  BaseFormula_dum, data = x.trainnorm, method = "ctree",
  trControl = ctrl
)

tree_base_time <- proc.time() - ptm_tree_base

x.evaluate$predictionTree_base <- predict(x.modelTree_base, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionTreeClass_base <- ifelse(x.evaluate$predictionTree_base[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$predictionTreeClass_base <- factor(x.evaluate$predictionTreeClass_base, levels = c("Noch_in", "ch_in"))
x.evaluate$correctTree_base <- x.evaluate$predictionTreeClass_base == x.evaluate$ch_in_string

TreeOutput_base <- makeLiftPlot(x.evaluate$predictionTree_base[,"ch_in"], x.evaluate, "Decision Tree (Base)")
TreeOutput_base$PercCorrect <- mean(x.evaluate$correctTree_base) * 100
Treeconfmatrix_base <- table(x.evaluate$predictionTreeClass_base, x.evaluate$ch_in_string)
TreeOutput_base$TimeElapsed <- tree_base_time[3]

### HYPERPARAMETER TUNING

grid_dt <- expand.grid(cp = seq(0.0001, 0.5, by = 0.01))

ptm_tree_tune <- proc.time()

set.seed(123)
x.modelTree_tuned <- train(
  BaseFormula_dum, data = x.trainnorm, method = "rpart",
  trControl = ctrl,
  tuneGrid = grid_dt,
  control = rpart.control(minsplit = 1, maxdepth = 5)
)

tree_tuned_time <- proc.time() - ptm_tree_tune

x.evaluate$predictionTree_tuned <- predict(x.modelTree_tuned, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionTreeClass_tuned <- ifelse(x.evaluate$predictionTree_tuned[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$predictionTreeClass_tuned <- factor(x.evaluate$predictionTreeClass_tuned, levels = c("Noch_in", "ch_in"))
x.evaluate$correctTree_tuned <- x.evaluate$predictionTreeClass_tuned == x.evaluate$ch_in_string

TreeOutput_tuned <- makeLiftPlot(x.evaluate$predictionTree_tuned[,"ch_in"], x.evaluate, "Decision Tree (Tuned)")
TreeOutput_tuned$PercCorrect <- mean(x.evaluate$correctTree_tuned) * 100
Treeconfmatrix_tuned <- table(x.evaluate$predictionTreeClass_tuned, x.evaluate$ch_in_string)
TreeOutput_tuned$TimeElapsed <- tree_tuned_time[3]

### EVALUATION

print("")
print(" # Model 5: Decision Tree (Base)")
print("Confusion matrix:")
print(Treeconfmatrix_base)

print("")
print(" # Model 5: Decision Tree (Tuned)")
print(paste("Best cp:", x.modelTree_tuned$bestTune$cp))
print("Confusion matrix:")
print(Treeconfmatrix_tuned)

tree_base <- get_metrics(
  pred = x.evaluate$predictionTree_base,
  pred_class = x.evaluate$predictionTreeClass_base,
  reference = x.evaluate$ch_in_string,
  tdl = TreeOutput_base$TDL,
  gini = TreeOutput_base$GINI,
  time_elapsed = TreeOutput_base$TimeElapsed
)

tree_tuned <- get_metrics(
  pred = x.evaluate$predictionTree_tuned,
  pred_class = x.evaluate$predictionTreeClass_tuned,
  reference = x.evaluate$ch_in_string,
  tdl = TreeOutput_tuned$TDL,
  gini = TreeOutput_tuned$GINI,
  time_elapsed = TreeOutput_tuned$TimeElapsed
)

metrics <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall", "Accuracy", "Specificity", "Time")

results_df <- data.frame(
  Metrics = metrics,
  Base = as.numeric(tree_base),
  Tuned = as.numeric(tree_tuned)
)

stargazer(
  results_df,
  summary = FALSE, rownames = FALSE,
  title = "Decision Tree Model Performance",
  type = "text", digits = 3
)

# the best tuned model performs significantly better than the base model in all important metrics
# TDL increase 0.5, GINI increased 0.3, AUC increased 0.2, F1 decreased slighly due to 3% dropped in Recall
# Accuracy decreased 4% meanwhile Specificity increased 6%
# therefore, the best model for DT is the tuned model

Tree <- x.modelTree_tuned
TreeOutput <- TreeOutput_tuned
Treeconfmatrix <- TreeOutput_tuned
tree <- tree_tuned
x.evaluate$predictionTree <- x.evaluate$predictionTree_tuned
predictionTree <- x.evaluate$predictionTree_tuned



############ 6) RANDOM FOREST --------------------------------------------------

# BASE MODEL

ptm_rf_base <- proc.time()

set.seed(123)
x.modelRF_base <- train(
  BaseFormula_dum, data = x.trainnorm, method = "parRF",
  trControl = ctrl
)

rf_base_time <- proc.time() - ptm_rf_base

x.evaluate$predictionRF_base <- predict(x.modelRF_base, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionRFClass_base <- ifelse(x.evaluate$predictionRF_base[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$predictionRFClass_base <- factor(x.evaluate$predictionRFClass_base, levels = c("Noch_in", "ch_in"))
x.evaluate$correctRF_base <- x.evaluate$predictionRFClass_base == x.evaluate$ch_in_string

RFOutput_base <- makeLiftPlot(x.evaluate$predictionRF_base[,'ch_in'], x.evaluate, "Random Forest (Base)")
RFOutput_base$PercCorrect <- mean(x.evaluate$correctRF_base) * 100
RFconfmatrix_base <- table(x.evaluate$predictionRFClass_base, x.evaluate$ch_in_string)
RFOutput_base$TimeElapsed <- rf_base_time[3]

# HYPERPARAMETER TUNING

grid_rf <- expand.grid(
  mtry = floor(seq(2, sqrt(ncol(x.trainnorm) - 2), length.out = 3)),
  splitrule = c("gini", "extratrees"),
  min.node.size = c(1, 5, 10)
)

ptm_rf_tune <- proc.time()

set.seed(123)
x.modelRF_tuned <- train(
  BaseFormula_dum, data = x.trainnorm, method = "ranger",
  trControl = ctrl,
  tuneGrid = grid_rf,
  num.trees = 100,         
  max.depth = 10,  
  importance = "impurity"
)

rf_tuned_time <- proc.time() - ptm_rf_tune

x.evaluate$predictionRF_tuned <- predict(x.modelRF_tuned, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionRFClass_tuned <- ifelse(x.evaluate$predictionRF_tuned[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$predictionRFClass_tuned <- factor(x.evaluate$predictionRFClass_tuned, levels = c("Noch_in", "ch_in"))
x.evaluate$correctRF_tuned <- x.evaluate$predictionRFClass_tuned == x.evaluate$ch_in_string

RFOutput_tuned <- makeLiftPlot(x.evaluate$predictionRF_tuned[,'ch_in'], x.evaluate, "Random Forest (Tuned)")
RFOutput_tuned$PercCorrect <- mean(x.evaluate$correctRF_tuned) * 100
RFconfmatrix_tuned <- table(x.evaluate$predictionRFClass_tuned, x.evaluate$ch_in_string)
RFOutput_tuned$TimeElapsed <- rf_tuned_time[3]

# EVALUATION

print("")
print(" # Model 6: Random Forest (Base)")
print("Confusion matrix:")
print(RFconfmatrix_base)

print("")
print(" # Model 6: Random Forest (Tuned)")
print(paste("Best mtry:", x.modelRF_tuned$bestTune$mtry))
print(paste("Best splitrule:", x.modelRF_tuned$bestTune$splitrule))
print(paste("Best min.node.size:", x.modelRF_tuned$bestTune$min.node.size))
print("Confusion matrix:")
print(RFconfmatrix_tuned)

rf_base <- get_metrics(
  pred = x.evaluate$predictionRF_base,
  pred_class = x.evaluate$predictionRFClass_base,
  reference = x.evaluate$ch_in_string,
  tdl = RFOutput_base$TDL,
  gini = RFOutput_base$GINI,
  time_elapsed = RFOutput_base$TimeElapsed
)

rf_tuned <- get_metrics(
  pred = x.evaluate$predictionRF_tuned,
  pred_class = x.evaluate$predictionRFClass_tuned,
  reference = x.evaluate$ch_in_string,
  tdl = RFOutput_tuned$TDL,
  gini = RFOutput_tuned$GINI,
  time_elapsed = RFOutput_tuned$TimeElapsed
)

metrics <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall", "Accuracy", "Specificity", "Time")

results_df <- data.frame(
  Metrics = metrics,
  Base = as.numeric(rf_base),
  Tuned = as.numeric(rf_tuned)
)

stargazer(
  results_df,
  summary = FALSE, rownames = FALSE,
  title = "Random Forest Model Performance",
  type = "text", digits = 3
)

# the best tuned model performs significantly better than the base model in terms of TDL 
# GINI, AUC, Precision, Specificity are also higher after tuning
# the tuned model takes less time to run than the base model
# therefore, the best model for RF is tuned model

RF <- x.modelRF_tuned
RFOutput <- RFOutput_tuned
RFconfmatrix <- RFconfmatrix_tuned
rf <- rf_tuned
x.evaluate$predictionRF <- x.evaluate$predictionRF_tuned
predictionRF <- x.evaluate$predictionRF_tuned



############ 7) BAGGING --------------------------------------------------------

# BASE MODEL

ptm_bag_base <- proc.time()

set.seed(123)
x.modelBagging_base <- train(
  BaseFormula_dum,
  data = x.trainnorm,
  method = "treebag",
  trControl = ctrl
)

bag_base_time <- proc.time() - ptm_bag_base

x.evaluate$predictionBagging_base <- predict(x.modelBagging_base, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionBaggingClass_base <- ifelse(x.evaluate$predictionBagging_base[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$predictionBaggingClass_base <- factor(x.evaluate$predictionBaggingClass_base, levels = c("Noch_in", "ch_in"))
x.evaluate$correctBagging_base <- x.evaluate$predictionBaggingClass_base == x.evaluate$ch_in_string

BaggingOutput_base <- makeLiftPlot(x.evaluate$predictionBagging_base[,'ch_in'], x.evaluate, "Bagging (Base)")
BaggingOutput_base$PercCorrect <- mean(x.evaluate$correctBagging_base) * 100
Baggingconfmatrix_base <- table(x.evaluate$predictionBaggingClass_base, x.evaluate$ch_in_string)
BaggingOutput_base$TimeElapsed <- bag_base_time[3]

# HYPERPARAMETER TUNING

#grid_bag <- expand.grid(nbagg = c(10, 25, 50),
                        #maxdepth = c(3, 5, 7), 
                        #minsplit = c(5, 10, 20))

ptm_bag_tune <- proc.time()

set.seed(123)
x.modelBagging_tuned <- train(
  BaseFormula_dum,
  data = x.trainnorm,
  method = "treebag",
  trControl = ctrl,
  nbagg = 100,
  #tuneGrid = grid_bag
)

bag_tuned_time <- proc.time() - ptm_bag_tune

x.evaluate$predictionBagging_tuned <- predict(x.modelBagging_tuned, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionBaggingClass_tuned <- ifelse(x.evaluate$predictionBagging_tuned[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$predictionBaggingClass_tuned <- factor(x.evaluate$predictionBaggingClass_tuned, levels = c("Noch_in", "ch_in"))
x.evaluate$correctBagging_tuned <- x.evaluate$predictionBaggingClass_tuned == x.evaluate$ch_in_string

BaggingOutput_tuned <- makeLiftPlot(x.evaluate$predictionBagging_tuned[,'ch_in'], x.evaluate, "Bagging (Tuned)")
BaggingOutput_tuned$PercCorrect <- mean(x.evaluate$correctBagging_tuned) * 100
Baggingconfmatrix_tuned <- table(x.evaluate$predictionBaggingClass_tuned, x.evaluate$ch_in_string)
BaggingOutput_tuned$TimeElapsed <- bag_tuned_time[3]

# EVALUATION

print("")
print(" # Model 7: Bagging (Base)")
print("Confusion matrix:")
print(Baggingconfmatrix_base)

print("")
print(" # Model 7: Bagging (Tuned)")
print(paste("Best nbagg:", x.modelBagging_tuned$bestTune$nbagg))
print("Confusion matrix:")
print(Baggingconfmatrix_tuned)

bag_base <- get_metrics(
  pred = x.evaluate$predictionBagging_base,
  pred_class = x.evaluate$predictionBaggingClass_base,
  reference = x.evaluate$ch_in_string,
  tdl = BaggingOutput_base$TDL,
  gini = BaggingOutput_base$GINI,
  time_elapsed = BaggingOutput_base$TimeElapsed
)

bag_tuned <- get_metrics(
  pred = x.evaluate$predictionBagging_tuned,
  pred_class = x.evaluate$predictionBaggingClass_tuned,
  reference = x.evaluate$ch_in_string,
  tdl = BaggingOutput_tuned$TDL,
  gini = BaggingOutput_tuned$GINI,
  time_elapsed = BaggingOutput_tuned$TimeElapsed
)

metrics <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall", "Accuracy", "Specificity", "Time")

results_bagging <- data.frame(
  Metrics = metrics,
  Base = as.numeric(bag_base),
  Tuned = as.numeric(bag_tuned)
)

stargazer(
  results_bagging,
  summary = FALSE, rownames = FALSE,
  title = "Bagging Model Performance",
  type = "text", digits = 3
)

# the tuned model performs better in all metrics except TDL 
# TDL dropped 0.3, while GINI increased 0.2, AUC increased 0.1
# all other metrics increased but the difference is relatively small (~0.02%)
# tunning take significantly longer time
# therefore, the best model for Bagging is the base model 

Bagging <- x.modelBagging_base
BaggingOutput <- BaggingOutput_base
Baggingconfmatrix <- Baggingconfmatrix_base
bagging <- bag_base
x.evaluate$predictionBagging <- x.evaluate$predictionBagging_base
predictionBagging <- x.evaluate$predictionBagging_base




############ 8) BOOSTING -------------------------------------------------------

# BASE MODEL
ptm_xgb_base <- proc.time()

set.seed(123)
x.modelXGB_base <- train(
  BaseFormula_dum, data = x.trainnorm, method = 'blackboost',
  trControl = ctrl
)

xgb_base_time <- proc.time() - ptm_xgb_base

x.evaluate$predictionXGB_base <- predict(x.modelXGB_base, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionXGBClass_base <- ifelse(x.evaluate$predictionXGB_base[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$predictionXGBClass_base <- factor(x.evaluate$predictionXGBClass_base, levels = c("Noch_in", "ch_in"))
x.evaluate$correctXGB_base <- x.evaluate$predictionXGBClass_base == x.evaluate$ch_in_string

XGBOutput_base <- makeLiftPlot(x.evaluate$predictionXGB_base[,'ch_in'], x.evaluate, "Boosting (Base)")
XGBOutput_base$PercCorrect <- mean(x.evaluate$correctXGB_base) * 100
XGBconfmatrix_base <- table(x.evaluate$predictionXGBClass_base, x.evaluate$ch_in_string)
XGBOutput_base$TimeElapsed <- xgb_base_time[3]

# HYPERPARAMETER TUNING

grid_xgb <- expand.grid(
  nrounds = 150,       
  max_depth = 2,         
  eta = 0.1,         
  gamma = 0,                   
  colsample_bytree = 1,       
  min_child_weight = 1,        
  subsample = 1                
)

ptm_xgb_tune <- proc.time()

set.seed(123)
x.modelXGB_tuned <- train(
  BaseFormula_dum, data = x.trainnorm, method = "xgbTree",
  trControl = ctrl,
  tuneGrid = grid_xgb,
  verbose = FALSE
)

xgb_tuned_time <- proc.time() - ptm_xgb_tune

x.evaluate$predictionXGB_tuned <- predict(x.modelXGB_tuned, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionXGBClass_tuned <- ifelse(x.evaluate$predictionXGB_tuned[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$predictionXGBClass_tuned <- factor(x.evaluate$predictionXGBClass_tuned, levels = c("Noch_in", "ch_in"))
x.evaluate$correctXGB_tuned <- x.evaluate$predictionXGBClass_tuned == x.evaluate$ch_in_string

XGBOutput_tuned <- makeLiftPlot(x.evaluate$predictionXGB_tuned[,'ch_in'], x.evaluate, "Boosting (Tuned)")
XGBOutput_tuned$PercCorrect <- mean(x.evaluate$correctXGB_tuned) * 100
XGBconfmatrix_tuned <- table(x.evaluate$predictionXGBClass_tuned, x.evaluate$ch_in_string)
XGBOutput_tuned$TimeElapsed <- xgb_tuned_time[3]

# EVALUATION

print("")
print(" # Model 8: Boosting (Base)")
print("Confusion matrix:")
print(XGBconfmatrix_base)

print("")
print(" # Model 8: Boosting (Tuned)")
print(paste("Best nrounds:", x.modelXGB_tuned$bestTune$nrounds))
print(paste("Best max_depth:", x.modelXGB_tuned$bestTune$max_depth))
print(paste("Best eta:", x.modelXGB_tuned$bestTune$eta))
print(paste("Best gamma:", x.modelXGB_tuned$bestTune$gamma))
print(paste("Best colsample_bytree:", x.modelXGB_tuned$bestTune$colsample_bytree))
print(paste("Best min_child_weight:", x.modelXGB_tuned$bestTune$min_child_weight))
print(paste("Best subsample:", x.modelXGB_tuned$bestTune$subsample))
print("Confusion matrix:")
print(XGBconfmatrix_tuned)

xgb_base <- get_metrics(
  pred = x.evaluate$predictionXGB_base,
  pred_class = x.evaluate$predictionXGBClass_base,
  reference = x.evaluate$ch_in_string,
  tdl = XGBOutput_base$TDL,
  gini = XGBOutput_base$GINI,
  time_elapsed = XGBOutput_base$TimeElapsed
)

xgb_tuned <- get_metrics(
  pred = x.evaluate$predictionXGB_tuned,
  pred_class = x.evaluate$predictionXGBClass_tuned,
  reference = x.evaluate$ch_in_string,
  tdl = XGBOutput_tuned$TDL,
  gini = XGBOutput_tuned$GINI,
  time_elapsed = XGBOutput_tuned$TimeElapsed
)

metrics <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall", "Accuracy", "Specificity", "Time")

results_df <- data.frame(
  Metrics = metrics,
  Base = as.numeric(xgb_base),
  Tuned = as.numeric(xgb_tuned)
)

stargazer(
  results_df,
  summary = FALSE, rownames = FALSE,
  title = "Boosting Model Performance",
  type = "text", digits = 3
)

# the bets tuned model performs slightly better than the base model
# TDL dropped 0.2, GINI and AUC increased ~0.02
# F1 improved 5% but due to  8% increase in Recall while Precision stays relatively same
# Accuracy increased 7%
# Specificity dropped significantly but it is not an important metric
# therefore, the best model for Boosting is the base model 

Boosting <- x.modelXGB_base
BoostingOutput <- XGBOutput_base
Boostingconfmatrix <- XGBconfmatrix_base
boosting <- xgb_base
x.evaluate$predictionBoosting <- x.evaluate$predictionXGB_base
predictionBoosting <- x.evaluate$predictionXGB_base




########### 9) NEURAL NETWORK --------------------------------------------------

# BASE MODEL
ptm_nnet_base <- proc.time()

set.seed(123)

mlp_grid = expand.grid(layer1 = 5,
                       layer2 = 5,
                       layer3 = 0)
x.modelNNet_base <- train(BaseFormula_dum, data=x.trainnorm, method='mlpML',tuneGrid=mlp_grid) 

nnet_base_time <- proc.time() - ptm_nnet_base

# plot NNet
if(0){
  NeuralNetTools::plotnet(x.modelNNet_base$finalModel)
}

x.evaluate$predictionNNet_base <- predict(x.modelNNet_base, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionNNetClass_base <- ifelse(x.evaluate$predictionNNet_base[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$predictionNNetClass_base <- factor(x.evaluate$predictionNNetClass_base, levels = c("Noch_in", "ch_in"))
x.evaluate$correctNNet_base <- x.evaluate$predictionNNetClass_base == x.evaluate$ch_in_string

NNetOutput_base <- makeLiftPlot(x.evaluate$predictionNNet_base[,'ch_in'], x.evaluate, "Neural Network (Base)")
NNetOutput_base$PercCorrect <- mean(x.evaluate$correctNNet_base) * 100
NNetconfmatrix_base <- table(x.evaluate$predictionNNetClass_base, x.evaluate$ch_in_string)
NNetOutput_base$TimeElapsed <- nnet_base_time[3]

# HYPERPARAMETER TUNING

# grid_nnet <- expand.grid(
#   size = c(3, 5, 7),
#   decay = c(0, 0.01, 0.1)
# )

ptm_nnet_tune <- proc.time()

grid_nnet <- expand.grid(
  layer1 = 30,   
  layer2 = 2,    
  layer3 = 0           
)

set.seed(123)
x.modelNNet_tuned <- train(
  BaseFormula_dum, data = x.trainnorm, method = "mlpML",
  trControl = ctrl,
  tuneGrid = grid_nnet,
  maxit = 200,
  trace = FALSE
)

# set.seed(123)
# x.modelNNet_tuned <- train(
#   BaseFormula_dum, data = x.trainnorm, method = "nnet",
#   trControl = ctrl,
#   tuneGrid = grid_nnet,
#   maxit = 200,
#   trace = FALSE
# )

nnet_tuned_time <- proc.time() - ptm_nnet_tune

# plot NNet
if(0){
  NeuralNetTools::plotnet(x.modelNNet_tuned$finalModel)
}

x.evaluate$predictionNNet_tuned <- predict(x.modelNNet_tuned, newdata = x.evaluatenorm, type = "prob")
x.evaluate$predictionNNetClass_tuned <- ifelse(x.evaluate$predictionNNet_tuned[,"ch_in"] > probthres, "ch_in", "Noch_in")
x.evaluate$predictionNNetClass_tuned <- factor(x.evaluate$predictionNNetClass_tuned, levels = c("Noch_in", "ch_in"))
x.evaluate$correctNNet_tuned <- x.evaluate$predictionNNetClass_tuned == x.evaluate$ch_in_string

NNetOutput_tuned <- makeLiftPlot(x.evaluate$predictionNNet_tuned[,'ch_in'], x.evaluate, "Neural Network (Tuned)")
NNetOutput_tuned$PercCorrect <- mean(x.evaluate$correctNNet_tuned) * 100
NNetconfmatrix_tuned <- table(x.evaluate$predictionNNetClass_tuned, x.evaluate$ch_in_string)
NNetOutput_tuned$TimeElapsed <- nnet_tuned_time[3]

# EVALUATION

print("")
print(" # Model 9: Neural Network (Base)")
print("Confusion matrix:")
print(NNetconfmatrix_base)

print("")
print(" # Model 9: Neural Network (Tuned)")
print(paste("Best size:", x.modelNNet_tuned$bestTune$size))
print(paste("Best decay:", x.modelNNet_tuned$bestTune$decay))
print("Confusion matrix:")
print(NNetconfmatrix_tuned)

nnet_base <- get_metrics(
  pred = x.evaluate$predictionNNet_base,
  pred_class = x.evaluate$predictionNNetClass_base,
  reference = x.evaluate$ch_in_string,
  tdl = NNetOutput_base$TDL,
  gini = NNetOutput_base$GINI,
  time_elapsed = NNetOutput_base$TimeElapsed
)

nnet_tuned <- get_metrics(
  pred = x.evaluate$predictionNNet_tuned,
  pred_class = x.evaluate$predictionNNetClass_tuned,
  reference = x.evaluate$ch_in_string,
  tdl = NNetOutput_tuned$TDL,
  gini = NNetOutput_tuned$GINI,
  time_elapsed = NNetOutput_tuned$TimeElapsed
)

metrics <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall", "Accuracy", "Specificity", "Time")

results_df <- data.frame(
  Metrics = metrics,
  Base = as.numeric(nnet_base),
  Tuned = as.numeric(nnet_tuned)
)

stargazer(
  results_df,
  summary = FALSE, rownames = FALSE,
  title = "Neural Network Model Performance",
  type = "text", digits = 3
)

# the best tuned model performs better in all important metrics
# TDL increased significantly 0.6, GINI and AUC increased 0.01, F1 increased 5%
# Precision stays relatively same while Recall increased 10%
# therefore, the best model for NNet is the tuned model

NNet <- x.modelNNet_tuned
NNetOutput <- NNetOutput_tuned
NNetconfmatrix <- NNetconfmatrix_tuned
nnet <- nnet_tuned
x.evaluate$predictionNNet <- x.evaluate$predictionNNet_tuned
predictionNNet <- x.evaluate$predictionNNet_tuned



# performance evaluation--------------------------------------------------------------------------------------------------------------------



x.evaluate$predictionLogit     <- predict(Logit, newdata=x.evaluatedum, type="prob")[,"ch_in"]
x.evaluate$predictionNB        <- predict(NB, newdata=x.evaluatenorm, type="prob")[,"ch_in"]
x.evaluate$predictionKNN       <- predict(KNN, newdata=x.evaluatenorm, type="prob")[,"ch_in"]
x.evaluate$predictionSVM       <- predict(SVM, newdata=x.evaluatenorm, type="prob")[,"ch_in"]
x.evaluate$predictionTree      <- predict(Tree, newdata=x.evaluatenorm, type="prob")[,"ch_in"]
x.evaluate$predictionRF        <- predict(RF, newdata=x.evaluatenorm, type="prob")[,"ch_in"]
x.evaluate$predictionBagging   <- predict(Bagging, newdata=x.evaluatenorm, type="prob")[,"ch_in"]
x.evaluate$predictionBoosting  <- predict(Boosting, newdata=x.evaluatenorm, type="prob")[,"ch_in"]
x.evaluate$predictionNNet      <- predict(NNet, newdata=x.evaluatenorm, type="prob")[,"ch_in"]

metric_names <- c("TDL", "GINI", "AUC", "F1", "Precision", "Recall", "Accuracy", "Specificity", "Time")
model_names <- c("LG_FT", "NB_FT", "KNN_FT", "SVM_FT", "DT_FT", 
                 "RF_FT", "Bagging_FT", "Boosting_FT", "NNet_FT")


metrics_table <- matrix(NA, nrow=length(metric_names), ncol=length(model_names))
rownames(metrics_table) <- metric_names
colnames(metrics_table) <- model_names

metrics_table[, "LG_FT"]        <- logit
metrics_table[, "NB_FT"]        <- nb
metrics_table[, "KNN_FT"]       <- knn
metrics_table[, "SVM_FT"]       <- svm
metrics_table[, "DT_FT"]        <- tree
metrics_table[, "RF_FT"]        <- rf
metrics_table[, "Bagging_FT"]   <- bagging
metrics_table[, "Boosting_FT"]  <- boosting
metrics_table[, "NNet_FT"]      <- nnet

metrics_table_round <- round(metrics_table, 3)

stargazer(
  metrics_table_round,
  summary = FALSE, rownames = TRUE,
  title = "Model Evaluation",
  type = "text", digits = 3
)

# heat map for performance metrics (with thresholds settings specifically for each metrics)

metrics_df <- melt(metrics_table_round)
colnames(metrics_df) <- c("Metric", "Model", "Value")

for (i in seq_len(nrow(metrics_df))) {
  metric <- metrics_df$Metric[i]
  val <- metrics_df$Value[i]
  # Adjust scaling for each metric
  if (metric == "TDL") {
    metrics_df$NormValue[i] <- (val - 1) / (5 - 1)
  } else if (metric %in% c("GINI", "AUC")) {
    metrics_df$NormValue[i] <- (val - 0) / (1 - 0)
  } else if (metric %in% c("F1", "Precision", "Recall", "Accuracy", "Specificity")) {
    metrics_df$NormValue[i] <- (val - 0) / (100 - 0)
  } else if (metric == "Time") {
    metrics_df$NormValue[i] <- 1 - (val - 0) / (100 - 0)
  } else {
    metrics_df$NormValue[i] <- NA
  }
}

ggplot(metrics_df, aes(x = Model, y = Metric, fill = NormValue)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = Value), color = "black", size = 3) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(unique(metrics_df$Metric))) +
  scale_fill_gradient2(
    low = "red", mid = "lightyellow", high = "lightblue", 
    midpoint = 0.5, limits = c(0, 1), 
  ) +
  theme_minimal() +
  labs(title = "Fine-Tuned Model Performance") +
  theme(
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),  
    plot.title = element_text(size = 14, hjust = 0.5),
    panel.grid = element_blank(),
    legend.position = "none"  
  )

# ROC curves

model_list <- list(
  "Logistic Regression" = x.evaluate$predictionlogit,
  "Naive Bayes"         = x.evaluate$predictionNB,
  "KNN"                 = x.evaluate$predictionKNN,
  "SVM"                 = x.evaluate$predictionSVM,
  "Decision Tree"       = x.evaluate$predictionTree,
  "Random Forest"       = x.evaluate$predictionRF,
  "Bagging"             = x.evaluate$predictionBagging,
  "Boosting"            = x.evaluate$predictionBoosting,
  "Neural Network"      = x.evaluate$predictionNNet
)

actual <- x.evaluate$ch_in_string

model_colors <- c(
  "Logistic Regression" = "blue",
  "Naive Bayes"         = "red",
  "K-Nearest Neighbor"  = "green3",
  "Support Vector Machine"  = "purple",
  "Decision Tree"       = "orange",
  "Random Forest"       = "brown",
  "Bagging"             = "goldenrod",
  "Boosting"            = "darkcyan",
  "Neural Network"      = "black"
)

plot(NULL, xlim=c(1,0), ylim=c(0,1), xlab="False Positive Rate", ylab="True Positive Rate", main="ROC Curves")
abline(a=0, b=1, lty=2, col="gray")

auc_vals <- numeric(length(model_list))
names(auc_vals) <- names(model_list)

for (model in names(model_list)) {
  probs <- model_list[[model]]
  # Check type:
  if(!is.numeric(probs)) {
    stop(paste0("Predictions for ", model, " are not numeric!"))
  }
  roc_obj <- roc(actual, probs, levels = c("Noch_in", "ch_in"), direction = "<")
  lines(roc_obj, col=model_colors[model], lwd=1)
  auc_vals[model] <- as.numeric(auc(roc_obj))
}

legend_labels <- sprintf("%s (AUC = %.3f)", names(auc_vals), auc_vals)
legend("bottomright", legend=legend_labels, col=model_colors, lwd=1, cex=0.9)

# lift curves

nrow(x.evaluate)
sapply(x.evaluate[, c("predictionlogit", "predictionNB", "predictionKNN", "predictionSVM",
                      "predictionTree", "predictionRF", "predictionBagging", "predictionBoosting",
                      "predictionNNet", "ch_in_string")], length)

OverallGINI <- c(LogitOutput$GINI, NBOutput$GINI, KNNOutput$GINI, SVMOutput$GINI, TreeOutput$GINI, RFOutput$GINI, BaggingOutput$GINI, BoostingOutput$GINI, NNetOutput$GINI)
OverallTDL  <- c(LogitOutput$TDL, NBOutput$TDL, KNNOutput$TDL, SVMOutput$TDL, TreeOutput$TDL, RFOutput$TDL, BaggingOutput$TDL, BoostingOutput$TDL, NNetOutput$TDL)

lift_obj <- lift(ch_in_string ~ predictionlogit + predictionNB + predictionKNN + predictionSVM + 
                   predictionTree + predictionRF + predictionBagging + predictionBoosting + predictionNNet,
                 data = x.evaluate, class = "ch_in")

model_names <- c("Logistic Regression", "Naive Bayes", "KNN", "SVM", 
                 "Decision Tree", "Random Forest", "Bagging", "Boosting", 
                 "Neural Network")

labels_with_metrics <- paste0(model_names, " (GINI: ", round(OverallGINI, 3), 
                              ", TDL: ", round(OverallTDL, 3), ")")

ggplot(lift_obj) +
  ggtitle("Lift Curves") +
  scale_color_discrete(name = NULL, labels = labels_with_metrics) + 
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5, 
      face = "bold", 
      color = "black",
      size = 17,
      margin = margin(b = 10)
    ),
    legend.position = c(0.98, 0.02),
    legend.justification = c("right", "bottom"),
    legend.text = element_text(size = 11),
    legend.key.size = unit(0.9, "lines"),
    legend.background = element_rect(fill = scales::alpha("white", 0.8), color = "gray", size = 0.5)
  )

# TDL & GINI graph

ForGraph <- data.frame(OverallTDL,OverallGINI)

myLeftAxisLabs <- pretty(seq(0, max(ForGraph$OverallTDL), length.out = 10))
myRightAxisLabs <- pretty(seq(0, max(ForGraph$OverallGINI), length.out = 10))

myLeftAxisAt <- myLeftAxisLabs/max(ForGraph$OverallTDL)
myRightAxisAt <- myRightAxisLabs/max(ForGraph$OverallGINI)

ForGraph$OverallTDL1 <- ForGraph$OverallTDL/max(ForGraph$OverallTDL)
ForGraph$OverallGINI1 <- ForGraph$OverallGINI/max(ForGraph$OverallGINI)

op <- par(mar = c(5,4,4,4) + 0.1)

barplot(t(as.matrix(ForGraph[, c("OverallTDL1", "OverallGINI1")])), beside = TRUE, yaxt = "n", 
        names.arg = c("Logistic Regression", "Naive Bayes", "KNN", "SVM", 
                      "Decision Tree", "Random Forest", "Bagging", "Boosting", 
                      "Neural Network"), ylim=c(0, max(c(myLeftAxisAt, myRightAxisAt))), 
        ylab =	"Top Decile Lift", legend = c("TDL","GINI"), args.legend = list(x = "topright", inset = c(0, -0.1)),
        main="Performance of the Machine Learning Algorithms")

axis(2, at = myLeftAxisAt, labels = myLeftAxisLabs)

axis(4, at = myRightAxisAt, labels = myRightAxisLabs)

mtext("GINI Coefficient", side = 4, line = 3, cex = par("cex.lab"))

mtext(c(paste(round(LogitOutput$TimeElapsed,digits=2),"sec"),
        paste(round(KNNOutput$TimeElapsed,digits=2),"sec"),
        paste(round(NBOutput$TimeElapsed,digits=2),"sec"),
        paste(round(SVMOutput$TimeElapsed,digits=2),"sec"),
        paste(round(TreeOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BaggingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BoostingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(RFOutput$TimeElapsed,digits=2),"sec"),
        paste(round(NNetOutput$TimeElapsed,digits=2),"sec")), side = 1, line = 3, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20,23,26))
mtext(c(paste(round(LogitOutput$PercCorrect,digits=0),"%"),
        paste(round(KNNOutput$PercCorrect,digits=0),"%"),
        paste(round(NBOutput$PercCorrect,digits=0),"%"),
        paste(round(SVMOutput$PercCorrect,digits=0),"%"),
        paste(round(TreeOutput$PercCorrect,digits=0),"%"),
        paste(round(BaggingOutput$PercCorrect,digits=0),"%"),
        paste(round(BoostingOutput$PercCorrect,digits=0),"%"),
        paste(round(RFOutput$PercCorrect,digits=0),"%"),
        paste(round(NNetOutput$PercCorrect,digits=0),"%")), side = 1, line = 4, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20,23,26))

mtext("Calc. time", side = 1, line = 3, cex = par("cex.lab"), at = -.8)
mtext("% correct", side = 1, line = 4, cex = par("cex.lab"), at = -.8)



# variable importance--------------------------------------------------------------------------------------------------------------------




# the best 2 models after performance evaluation are RF and Boosting
# therefore, this part looks into variable importance of those 2 models 

# RF
print(varImp(RF))
plot(varImp(RF), main = "Random Forest - Variable Importance")

# Boosting 
print(varImp(Boosting))
plot(varImp(Boosting), main = "Boosting - Variable Importance")






