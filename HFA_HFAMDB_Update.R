# 
# This script processes Selected HFA and HFAMDB data from October 2021
#

# Do NOT recode string when importing
options(stringsAsFactors = FALSE)
options(java.parameters = "-Xmx4g") #for troubles importing metadata

# R packages used by the script 
#install.packages("RODBC")
#install.packages("XLConnect", dependencies=TRUE) (if needed)
#install.packages("jsonlite")
#install.packages("dplyr")
require(XLConnect)
require(RODBC)
require(jsonlite)
require(dplyr)

# Define country groups in StatProd and Data Warehouse
country_groups    <- c("CAR","CARK","E12","E15","ZZZ","NOC","SEE","E27", "SMC", "CIS")
country_groups_DW <- c("CARINFONET","CARINFONET","EU_AFTER_MAY2004","EU_BEFORE_MAY2004","WHO_EURO","NORDIC","SEEHN","EU_MEMBERS", "SMALL", "CIS")

# Importing countries from DW API, and appending it with previously defined country groups
url=paste("http://dw.euro.who.int/api/v3/Country_groups/WHO_EURO")
rd <- readLines(url, warn="F")
aa <- fromJSON(rd, simplifyDataFrame = TRUE)

countries <- aa$countries
countries <- data.frame(rbind(cbind(countries$iso3, countries$who_code),cbind(country_groups_DW,country_groups)))
names(countries) <- c("iso3","who_code")
rm(aa)
# 'countries' table created

# replace country code for North Macedonia
countries$who_code <- gsub("MKD", "FYM", countries$who_code)


#These codes havent passed validation rules in the 2020 update. Waiting on validation results for the 2021 update
#badDWID=c("HFA_67","HFA_68","HFA_69","HFA_266","HFA_267","HFA_268","HFA_91","HFA_265","HFA_263","HFA_262","HFA_264","HFA_260","HFA_261","HFA_590","HFA_601","HFA_463","HFA_466", "HFA_84", "HFA_224","HFA_225", "HFA_226")



#Set the Working Directory below
setwd("C:\\Users\\User\\WHO test")


wb1 <- loadWorkbook("IndicatorList20220713.xlsx")
metadata_orig1  <- readWorksheet(wb1, sheet = "Sheet1", header = T)
metadata <- metadata_orig1
names(metadata)[c(1:2)]<- c("Element", "DWID")


# Read data from StatProd (SQL Server)
myconn <-odbcConnect("StatProd20220709", uid="", pwd="")
myconn
#Import Data and keep only indicators from metadata table, and only countries from countries table
Data <- sqlQuery(myconn, "select * from Data where validTo is NULL and value is not null")
Data <- Data[Data$Element %in% c(metadata$Element),]
Data <- Data[Data$CNTRY %in% c(countries$who_code, country_groups),]

#Import Averages and keep only indicators from metadata table, and only countries from countries table
Averages <- sqlQuery(myconn, "select * from Averages where validTo is NULL and value is not null")
Averages <- Averages[Averages$Element %in% c(metadata$Element),]
Averages <- Averages[Averages$CNTRY %in% c(countries$who_code, country_groups),]

#Import CalculatedData and keep only indicators from metadata table, and only countries from countries table
CalculatedData <- sqlQuery(myconn, "select * from CalculatedData where validTo is NULL and value is not null")
CalculatedData <- CalculatedData[CalculatedData$Element %in% c(metadata$Element),]
CalculatedData <- CalculatedData[CalculatedData$CNTRY %in% c(countries$who_code, country_groups),]

close(myconn)

# End reading from StatProd
#####################################################################

# Appending Data, Averages and CalculatedData into single table (data2021)
data_2022 <- rbind(Data, Averages, CalculatedData)

#These are the indicators which show up in Statprod, but are not defined and do not exist at all in DW: HFA_420, HFA_458, HFA_459, HFA_460, HFA_461, HFA_462, HFA_576, HFA_579
non_API=c("E040701.T", "E992102.T", "E992103.T", "E992104.T", "E992105.T", "E992106.T", "E992704.T", "E992707.T") 
data_2022 <- data_2022[-which(data_2022$Element%in%non_API),]

# Checking number of countries that have not been recoded
check <- length(which(is.na(match(data_2022$CNTRY, countries$who_code))))
cat(paste("\n Number of countries have not been recoded is ", check, "\n"))

# Replace Country codes with iso3 if Country from data2021 match with who_code, instead of iso3 code
if(check == 0){
  data_2022$CNTRY <- countries$iso3[match(data_2022$CNTRY, countries$who_code)]
}



#sort data2021 by Element CNTRY Year
data_2022 <- data_2022[order(data_2022$Element, data_2022$CNTRY, data_2022$Year),]
#Testing for duplicates for Elements,YEAR and ISO avaiable
duplicat4 <- data_2022[which(duplicated(data_2022[,c(1,2,3) ] ) ) ,] #Empty
write.table(duplicat4, file='StatProd_duplicated.csv', row.names=F, col.names=T, append=F, na="",sep=",")

nrow(duplicat4) # 0,  duplicates
####

# ======================------------ DW data from Prod API -----------------------===================

# Get metadata from API, including DWID
url="http://dw.euro.who.int/api/v3/measures/"
rd <- readLines(url, warn="F") 
metaAPI <- fromJSON(rd, simplifyDataFrame = TRUE)
metaAPI<-metaAPI[metaAPI$code %in% unique(metadata$DWID),]
metaAPI <- metaAPI[,1:3]


write.csv(metaAPI,"Indicators_uploaded_2022.csv", row.names = FALSE)

#Only leave those in metaAPI that appear in metadata$DWID 
metaAPI<-metaAPI[metaAPI$code %in% unique(metadata$DWID),]
# Checking missing values (or duplicated)
length(unique(metaAPI$code)) 
length(unique(metadata$DWID)) 

metadata$DWID[!(metadata$DWID %in% c(metaAPI$code))]
metadata = metadata[which(metadata$DWID!="NA"),]

# Creating empty dataset, setting col. names
data_2021 <- data.frame(matrix(ncol = 7, nrow = 0), stringsAsFactors = FALSE)
names(data_2021) <- c("DWID", "DPSNR", "ISO3", "YEAR", "SEX", "Value") 

##################################################################################
#Extract data from DW, one indicator at time. Only indicators selected in metadata

counterSUBNATIONAL <- 0
counterSEX <- 0 
nrow(metaAPI)

for(i in 1:nrow(metaAPI)){ 
  
  # Check for first 20 indicators for(i in 1:20){ 
  
  
  # get measure from API
  url=paste("http://dw.euro.who.int/api/v3/measures/", metaAPI$code[i], sep="")
  rd <- readLines(url, warn="F") 
  tempDataAPI <- fromJSON(rd, simplifyDataFrame = TRUE)
  
  cat(paste("\n       ---------------------------            ", metaAPI$code[i], "                  ---------------------- "))   
  
  externId <- strsplit(tempDataAPI[["metadata"]]$value$code[tempDataAPI[["metadata"]]$code == "EXTERNID"]," ")[[1]]
  
  
  if(length(externId)==1){
    metaAPI$externId[i] <- tempDataAPI[["metadata"]]$value$code[tempDataAPI[["metadata"]]$code == "EXTERNID"]
    nbIndic <- 1
  }else{
    metaAPI$externId[i] <- tempDataAPI[["metadata"]]$value$code[tempDataAPI[["metadata"]]$code == "EXTERNID"]
    nbIndic <- 3
  }
  
  if(match("SEX",tempDataAPI$dimensions$code)){
    
    if(nbIndic == 3){
      if(metaAPI$code[i]!="HFA_463" & metaAPI$code[i]!="HFA_466")
      {
        cat(paste("\n OK: There are 3 DPSNRs "))
        temp <- data.frame(metaAPI$code[i],
                           ifelse(tempDataAPI$data$dimensions$SEX == "MALE", 
                                  externId[2], 
                                  ifelse(tempDataAPI$data$dimensions$SEX == "FEMALE", 
                                         externId[3], externId[1])), 
                           ifelse(tempDataAPI$data$dimensions$COUNTRY!="",
                                  tempDataAPI$data$dimensions$COUNTRY,
                                  tempDataAPI$data$dimensions$COUNTRY_GRP), 
                           tempDataAPI$data$dimensions$YEAR, 
                           tempDataAPI$data$dimensions$SEX, 
                           tempDataAPI$data$value$numeric)
        
        names(temp) <- c("DWID", "DPSNR", "ISO3", "YEAR", "SEX", "Value")
      }
      else 
      {
        cat(paste("\n OK: There are 3 DPSNRs "))
        temp <- data.frame(metaAPI$code[i],
                           ifelse(tempDataAPI$data$dimensions$PLACE_RESIDENCE == "URBAN", 
                                  externId[2], 
                                  ifelse(tempDataAPI$data$dimensions$PLACE_RESIDENCE == "RURAL", 
                                         externId[3], externId[1])), 
                           ifelse(tempDataAPI$data$dimensions$COUNTRY!="",
                                  tempDataAPI$data$dimensions$COUNTRY,
                                  tempDataAPI$data$dimensions$COUNTRY_GRP), 
                           tempDataAPI$data$dimensions$YEAR, 
                           tempDataAPI$data$dimensions$SEX, 
                           tempDataAPI$data$value$numeric)
        
        names(temp) <- c("DWID", "DPSNR", "ISO3", "YEAR", "SEX", "Value")  
        
        temp<-temp[temp$DPSNR %in% c("4300","4310"),]
      }
    } else {
      cat(paste("\n Error: There is only 1 DPSNR "))
      temp <- data.frame(metaAPI$code[i],
                         externId, 
                         ifelse(tempDataAPI$data$dimensions$COUNTRY!="",
                                tempDataAPI$data$dimensions$COUNTRY,
                                tempDataAPI$data$dimensions$COUNTRY_GRP), 
                         tempDataAPI$data$dimensions$YEAR, 
                         tempDataAPI$data$dimensions$SEX, 
                         tempDataAPI$data$value$numeric)
      
      names(temp) <- c("DWID", "DPSNR", "ISO3", "YEAR", "SEX", "Value")    }
    counterSEX <- counterSEX + nbIndic
    cat(paste("\n OK: Attribute SEX is present: ", counterSEX))   
    
  }else{
    cat(paste("\n Error: Attribute SEX is NOT present!!!!!! "))
  }
  
  data_2021 <- rbind(data_2021, temp)
  
}


###Export excel for Stata - do not run!
# write.csv(data_2021,"C:\\Users\\User\\Desktop\\WHO\\2021\\data_2021new.csv", row.names = FALSE)
# write.csv(data_2022,"C:\\Users\\User\\Desktop\\WHO\\2021\\data_2022new.csv", row.names = FALSE)
# 

#Testing for duplicates for Elements,DWID,YEAR and ISO avaiable
duplicates_test <- data_2021[which(duplicated(data_2021[,c(1,3,4,5) ] ) ) ,] #Empty

nrow(duplicates_test) 
#Order by Elements,DWID,YEAR and ISO

duplicates_test=duplicates_test[order( duplicates_test$DWID,duplicates_test$ISO3,duplicates_test$YEAR, duplicates_test$SEX),]

write.table(duplicates_test, file='Duplicated indicators in DW for  European Health for All Database.csv', row.names=F, col.names=T, append=F, na="",sep=",")

length(unique(duplicates_test$DWID)) 
# ======================------------ end DW data from Prod API -----------------------===================

# Creating averages and calculated data that are already in statprod, but not in the DW data so we have 
# to calculate them manually

#This file (metadata.xlsx) is created in full_supplementary_code_20211020.do STATA code

wb1a        <- loadWorkbook("metadata.xlsx")

metadata_orig1  <- readWorksheet(wb1a, sheet = "Sheet1", header = T)

metadata1 <- metadata_orig1[,c(1:7)]

##Do not run, STATA export
#write.csv(metadata,"C:\\Users\\Lenovo\\Desktop\\Copenhagen HFA 2021\\metadata296.csv", row.names = FALSE)

############ Extracting SEX variable from Element
metadata1$SEX <- strsplit(metadata1$Element,"[.]")
metadata1$SEX <- lapply(metadata1$SEX, function(x){ return( unlist(x)[length(unlist(x))])   } )

#Only GINI is not format in this version of update
#metadata1$SEX <- ifelse(metadata1$SEX == "M","MALE",ifelse(metadata1$SEX == "F","FEMALE",ifelse(metadata1$SEX =="T", "ALL", 
#                                                                                                ifelse((metadata1$SEX =="GINI")|(metadata1$SEX =="SocialSupport")|(metadata1$SEX =="1000gOrNational")|(metadata1$SEX =="nationalCriteria")|(metadata1$SEX =="1000g"), "ALL", NA     )     )))

metadata1$SEX <- ifelse(metadata1$SEX == "M","MALE",ifelse(metadata1$SEX == "F","FEMALE",ifelse(metadata1$SEX =="T", "ALL", 
                                                                                                ifelse((metadata1$SEX =="GINI"), "ALL", NA     )     )))




data_2021$DWID=as.character(data_2021$DWID)
data_2021$SEX=as.character(data_2021$SEX)
metadata1$DWID=as.character(metadata1$DWID)
metadata1$SEX=as.character(metadata1$SEX)
data_2021$DWID=trimws(data_2021$DWID)
data_2021$SEX=trimws(data_2021$SEX)
metadata1$DWID=trimws(metadata1$DWID)
metadata1$SEX=trimws(metadata1$SEX)
######################

data_2021a <- merge(data_2021, metadata1, by=c("DWID","SEX"), all.x=TRUE)

data_2021a<-data_2021a[!is.na(data_2021a$Element),]

#################################################


temp1 <- data_2021a[is.na(data_2021a$Element),]
temp1 <- data_2021a[is.na(data_2021a$DWID),]

# Minimum value by Indicator (Element)
data_2021bis <- data_2021a
minValue <- aggregate(data_2021bis$Value, by=list(Category=data_2021bis$Element), FUN=min)
names(minValue) <- c("Element", "DW_minValue")
minValueCNTRY <- aggregate(data_2021bis$Value~data_2021bis$Element+data_2021bis$ISO3, FUN=min)
names(minValueCNTRY) <- c("Element","ISO3", "DW_minValue")

# Maximum value by Indicator (Element)
maxValue <- aggregate(data_2021bis$Value, by=list(Category=data_2021bis$Element), FUN=max)
names(maxValue) <- c("Element", "DW_maxValue")
maxValueCNTRY <- aggregate(data_2021bis$Value~data_2021bis$Element+data_2021bis$ISO3, FUN=max)
names(maxValueCNTRY) <- c("Element","ISO3", "DW_maxValue")

# Average value by Indicator in WHO_EURO (Element)
maxAverage <- aggregate(data_2021bis$Value[data_2021bis$ISO3 == "WHO_EURO"], by=list(Category=data_2021bis$Element[data_2021bis$ISO3 == "WHO_EURO"]), FUN=max)
names(maxAverage) <- c("Element", "DW_maxWHO_EURO")
# Puting calculated values into single table (averages)
averages <- merge(maxValue, minValue, by="Element", all.x = TRUE)
averages <- merge(averages, maxAverage, by="Element", all.x = TRUE)
averages <- merge(metadata1, averages, by="Element", all.x = TRUE)

CNTRYminmax<-merge(maxValueCNTRY,minValueCNTRY, by=c("Element","ISO3"),all.x=TRUE)


averages = averages[,c(1,2,4,5,9:11)]
# Preparing StatProd and DW data for merging
temp2020 <- data_2021bis[,c(1,2,4,5,6,7,8)]
temp2021 <- data_2022[,c(1,2,3,5)]
names(temp2021) <- c("ISO3","YEAR","Element","Value")


# Merge StatProd and DW data
compare <- merge(temp2021, temp2020, by=c("Element","ISO3","YEAR"), all=TRUE)

# Comparing values from SQL data and from DW data, possible vales after comparisons are: Match, update, delete, add, NA
compare$Compare <- ifelse(!is.na(round(compare$Value.x,2) == round(compare$Value.y,2)),ifelse(round(compare$Value.x,2) == round(compare$Value.y,2),"match","update"),
                          ifelse(is.na(compare$Value.x),"delete",
                                 ifelse(is.na(compare$Value.y),"add",NA)))

table(compare$Compare) #Number of occurencies of each possible comparison outcome


# Dropping observations that match
compare2 <- compare[compare$Compare %in% c("delete","add","update"),]
compare3 <- data.frame(compare2[,-c(4,6,7)], stringsAsFactors = F)
compare5 <- compare[compare$Compare %in% c("update"),]


###################################################################################################
# Creating series of excel files that are tracking type of differences between SQL data and DW data

remove_index <-c()
for(i in seq_len(nrow(compare3)-1)){
  
  if((i%%1000) == 0) cat(paste("\n ", i))
  if((compare3$Element[i] == compare3$Element[i+1])&(compare3$ISO3[i] == compare3$ISO3[i+1])&(compare3$Compare[i] == compare3$Compare[i+1]))
  {
    compare3$YEAR[i+1] <- paste(compare3$YEAR[i], compare3$YEAR[i+1], sep=", ")
    remove_index <-c(remove_index,eval(i))
  }
}

compare3 <- compare3[-remove_index,]

compare3$ISO3 <- as.character(compare3$ISO3)
compare3 <- compare3[order(compare3$Element, compare3$Compare, compare3$YEAR),]
remove_index <-c()
for(i in seq_len(nrow(compare3)-1)){
  if((i%%1000) == 0) cat(paste("\n ", i))
  if((compare3$Element[i] == compare3$Element[i+1])&(compare3$YEAR[i] == compare3$YEAR[i+1])&(compare3$Compare[i] == compare3$Compare[i+1])) 
  {
    compare3$ISO3[i+1] <- paste(compare3$ISO3[i], compare3$ISO3[i+1], sep=", ")
    remove_index <-c(remove_index,eval(i))
  }
}
compare3orig=compare3
compare3 <- compare3[-remove_index,]

metadata4 <- metadata1[,c(1,2,4)]

compare3bis <- merge(metadata4, compare3, by="Element", all.y = T)




write.table(compare3bis, file='MDB whichYear_whichCNTRY BAD DWID.csv', row.names=F, col.names=T, append=F, na="",sep=",")

compare4 <- compare3bis

deleted <- compare4[compare4$Compare == "delete",]
added   <- compare4[compare4$Compare == "add",]
updated <- compare4[compare4$Compare == "update",]

analysis_result <- data.frame(metadata4$DWID, metadata4$Element, metadata4$IndicatorName, NA, NA, NA, NA)
names(analysis_result) <- c("DWID","Element","StatProdIndicatorName","added","updated","deleted","not_updated")

analysis_result$added[analysis_result$DWID %in% unique(added$DWID.x)] <- "added"
analysis_result$updated[analysis_result$DWID %in% unique(updated$DWID.x)] <- "updated"
analysis_result$deleted[analysis_result$DWID %in% unique(deleted$DWID.x)] <- "deleted"
analysis_result$not_updated[is.na(analysis_result$added)&is.na(analysis_result$updated)&is.na(analysis_result$deleted)] <- "not updated"

analysis_result$result <- paste(analysis_result$added, analysis_result$updated, analysis_result$deleted, analysis_result$not_updated, sep=', ')
analysis_result$result <- gsub(", NA","", analysis_result$result)
analysis_result$result <- gsub("NA, ","", analysis_result$result)

cat(paste("\n Number of Elements is scope of the project: ", length(unique(data_2022$Element) %in% unique(analysis_result$Element[is.na(analysis_result$not_updated)]))))


# Write files with results of comparisons



write.table(analysis_result, file='MDB_analysis_result.csv', row.names=F, col.names=T, append=F, na="",sep=",")



write.table(deleted, file='deleted.csv', row.names=F, col.names=T, append=F, na="",sep=",")
write.table(added, file='added.csv', row.names=F, col.names=T, append=F, na="",sep=",")
write.table(updated, file='updated.csv', row.names=F, col.names=T, append=F, na="",sep=",")


# =================================== validation rule no.1
# Comparing % of difference in indicators (for country groups).
# Define threshold below (set to 50% by default)
valrule1_threshold <- 50

valrule1_compare <- compare[compare$ISO3 %in% country_groups_DW,]
valrule1_compare$diff <- abs(valrule1_compare$Value.x - valrule1_compare$Value.y)
valrule1_compare <- valrule1_compare[order(valrule1_compare$diff, decreasing = T),]
valrule1_compare <- valrule1_compare[!is.na(valrule1_compare$diff),]
valrule1_compare <- valrule1_compare[valrule1_compare$diff > 0.2,]
for(i in 1:length(valrule1_compare$Element)){
  valrule1_compare$diff_pct[i] = 100*valrule1_compare$diff[i]/min(valrule1_compare$Value.y[i],valrule1_compare$Value.x[i])
}

valrule1_compare <- valrule1_compare[valrule1_compare$diff_pct != Inf,]
valrule1_compare <- valrule1_compare[valrule1_compare$diff_pct > valrule1_threshold,]
valrule1_metadata=metadata4
names(valrule1_metadata)[1]="Element"
valrule1_compare <- merge(valrule1_compare, valrule1_metadata, by=c("Element"), all.x = T) 
valrule1_compare <- valrule1_compare[,c(8,1,5,2,3,4,7,11)]


valrule1_compare <- valrule1_compare[order(valrule1_compare$diff_pct, decreasing = T),]

names(valrule1_compare)[c(1,3,6,7,8)] <-c("StatProdIndicatorName","DWID","StatProd value","DW value","diff (%)")


write.table(valrule1_compare, file='Validation rule 1.csv', row.names=F, col.names=T, append=F, na="",sep=",")

HFA_unique = unique(valrule1_compare$DWID)
valrule1_max = data.frame(matrix(ncol = 5, nrow = 0), stringsAsFactors = FALSE)

for(i in 1:length(HFA_unique)){
  row2=valrule1_compare[valrule1_compare$DWID==HFA_unique[i],]
  row2=row2[which.max(row2$`diff (%)`) ,]
  valrule1_max=rbind(valrule1_max,row2)
}
write.table(valrule1_max, file='Max diff for all indicators in Validation rule 1 for  European Health for All Database.csv', row.names=F, col.names=T, append=F, na="",sep=",")



# =================================== validation rule no.2

# Comparing StatProd values to max Value per indicator (looking for outliers)
# New code, pct. change instead of absolute, and keep only maximum per indicator
# Define threshold below (set to 50% by default)
valrule2_threshold=50

#### Not working now, cannot allocate vector of size n, ran on another computer with just the three needed databases

valrule2_compare <- merge(compare, averages, by="Element", all.x = TRUE)
valrule2_compare <- valrule2_compare[valrule2_compare$Value.x > valrule2_compare$DW_maxValue,-c(5,6,7)]
valrule2_compare$diff <- abs(valrule2_compare$Value.x - valrule2_compare$DW_maxValue)
valrule2_metadata=metadata4
names(valrule2_metadata)[1]="Element"
valrule2_compare <- merge(valrule2_compare, valrule2_metadata, by="Element")
valrule2_compare <- valrule2_compare[order(valrule2_compare$diff, decreasing = T),]
valrule2_compare <- valrule2_compare[,c(14,1,15,2,3,4,10,13,6)]

names(valrule2_compare)[6] <-c("StatProd value")

write.table(valrule2_compare, file='Validation rule 2.csv', row.names=F, col.names=T, append=F, na="",sep=",")

valrule2_compare$diff_pct<-valrule2_compare$diff/valrule2_compare$DW_maxValue*100 #pct
valrule2_compare <- valrule2_compare[valrule2_compare$diff_pct > valrule2_threshold,]
valrule2_compare <- valrule2_compare[order(valrule2_compare$diff_pct, decreasing = T),]
write.table(valrule2_compare, file='Validation rule 2 pct all.csv', row.names=F, col.names=T, append=F, na="",sep=",")


HFA_unique = unique(valrule2_compare$DWID)
valrule2_max = data.frame(matrix(ncol = 10, nrow = 0), stringsAsFactors = FALSE)

for(i in 1:length(HFA_unique)){
  row2=valrule2_compare[valrule2_compare$DWID==HFA_unique[i],]
  row2=row2[which.max(row2$`diff_pct`) ,]
  valrule2_max=rbind(valrule2_max,row2)
}
write.table(valrule2_max, file='Max diff for all indicators in Validation rule 2 for  European Health for All Database.csv', row.names=F, col.names=T, append=F, na="",sep=",")




# =================================== validation rule no.3

# Comparing StatProd values to max Value per indicator per country (looking for outliers)
# New code, pct. change instead of absolute, and keep only maximum per indicator 
# A finer rule than validation rule 2 as the max Values are taken over a smaller set, 
# but the treshhold is higher when locating outliers (200% diff, but it can be redifined below). 
valrule3_threshold=200

valrule3_compare <- merge(compare, CNTRYminmax, by=c("Element","ISO3"), all.x = TRUE)
valrule3_compare <- valrule3_compare[valrule3_compare$Value.x > valrule3_compare$DW_maxValue,]
valrule3_compare$diff <- abs(valrule3_compare$Value.x - valrule3_compare$DW_maxValue)
valrule3_metadata=metadata4
names(valrule3_metadata)[1]="Element"
valrule3_compare <- merge(valrule3_compare, valrule3_metadata, by="Element")
valrule3_compare <- valrule3_compare[order(valrule3_compare$diff, decreasing = T),]
valrule3_compare <- valrule3_compare[,c(1,2,3,4,7,10,12,13,14)]



write.table(valrule3_compare, file='Validation rule 3.csv', row.names=F, col.names=T, append=F, na="",sep=",")

valrule3_compare$diff_pct<-valrule3_compare$diff/valrule3_compare$DW_maxValue*100 #pct
valrule3_compare <- valrule3_compare[valrule3_compare$diff_pct > valrule3_threshold,]
valrule3_compare <- valrule3_compare[order(valrule3_compare$diff_pct, decreasing = T),]
write.table(valrule3_compare, file='Validation rule 3 pct all.csv', row.names=F, col.names=T, append=F, na="",sep=",")


valrule3_compare<-valrule3_compare[valrule3_compare$DW_maxValue!=0,]
Element_unique = unique(valrule3_compare$Element)
valrule3_max = data.frame(matrix(ncol = 10, nrow = 0), stringsAsFactors = FALSE)

for(i in 1:length(Element_unique)){
  row2=valrule3_compare[valrule3_compare$Element==Element_unique[i],]
  row2=row2[which.max(row2$`diff_pct`) ,]
  valrule3_max=rbind(valrule3_max,row2)
}
write.table(valrule3_max, file='Max diff for all indicators in Validation rule 3 for  European Health for All Database.csv', row.names=F, col.names=T, append=F, na="",sep=",")



# =================================== validation rule no.4
# Comparing all new StatProd values to their corresponding values on the DW
# Threshold is set to 500% difference, but it can be changed below
valrule4_threshold=500


valrule4_compare <- compare[compare$Compare %in% c("update"),]
valrule4_compare$diff <- abs(valrule4_compare$Value.x - valrule4_compare$Value.y)
valrule4_compare <- valrule4_compare[order(valrule4_compare$diff, decreasing = T),]
valrule4_compare <- valrule4_compare[!is.na(valrule4_compare$diff),]
for(i in 1:length(valrule4_compare$Element)){
  valrule4_compare$diff_pct[i] = 100*valrule4_compare$diff[i]/min(valrule4_compare$Value.y[i],valrule4_compare$Value.x[i])
}

valrule4_compare <- valrule4_compare[valrule4_compare$diff_pct != Inf,]
valrule4_compare <- valrule4_compare[valrule4_compare$diff_pct > valrule4_threshold,]
valrule4_metadata=metadata4
names(valrule4_metadata)[1]="Element"
valrule4_compare <- merge(valrule4_compare, valrule4_metadata, by=c("Element"), all.x = T) 
valrule4_compare <- valrule4_compare[,c(8,1,5,2,3,4,7,11)]


valrule4_compare <- valrule4_compare[order(valrule4_compare$diff_pct, decreasing = T),]

names(valrule4_compare)[c(1,3,6,7,8)] <-c("StatProdIndicatorName","DWID","StatProd value","DW value","diff (%)")




write.table(valrule4_compare, file='Validation rule 4.csv', row.names=F, col.names=T, append=F, na="",sep=",")

HFA_unique = unique(valrule4_compare$DWID)
valrule4_max = data.frame(matrix(ncol = 5, nrow = 0), stringsAsFactors = FALSE)

for(i in 1:length(HFA_unique)){
  row2=valrule4_compare[valrule4_compare$DWID==HFA_unique[i],]
  row2=row2[which.max(row2$`diff (%)`) ,]
  valrule4_max=rbind(valrule4_max,row2)
}
write.table(valrule4_max, file='Max diff for all indicators in Validation rule 4 for  European Health for All Database.csv', row.names=F, col.names=T, append=F, na="",sep=",")





#--------------------------------- HFA and HFAMDB Indicators Final Output Files Creation
library(readr)
drop_dwid <- read_csv("Validation Rules - Indicators to be Dropped.csv")
drop_dwid_list <-c(drop_dwid$DWID)

ids <- unique(metaAPI$code)#list of indicators
ids = ids[!(ids %in% drop_dwid_list)]

#Set Output File Path Below
outFilePath<- "C:\\Users\\User\\WHO test\\Output Files\\"

totalRows <- 0

# Iterate over the ids
NoDuplicates=c()
for(i in 1:length(ids)){
  
  id <- ids[i]
  
  # get measure from API
  url=paste("http://dw.euro.who.int/api/v3/measures/", id, sep="")
  rd <- readLines(url, warn="F") 
  tempDataAPI <- fromJSON(rd, simplifyDataFrame = TRUE)
  cat(paste("\n       ---------------------------            ", id, "                  ---------------------- "))   
  
  ##Check if HFAMDB or HFA
  indicator_check <- strsplit(id,"[_]")
  indicator_check <- lapply(indicator_check, function(x) x[1])
  
  ##If HFAMDB use old code
  if (indicator_check=="HFAMDB") {
    
    attributes    <- tempDataAPI[["dimensions"]]$code
    AGE_GRP_index <- grep("AGE_",attributes)
    if(length(AGE_GRP_index)!=0) AGE_GRP_name  <- attributes[AGE_GRP_index]
    AGE_GRP_value <- unique(eval(parse(text=paste("tempDataAPI","data","dimensions",eval(AGE_GRP_name),sep="$"))))
    
    # create MEASURE_TYPE
    
    measure_type <- c(unique(tempDataAPI[["data"]]$attributes$MEASURE_TYPE[tempDataAPI[["data"]]$dimensions$COUNTRY != ""]),
                      unique(tempDataAPI[["data"]]$attributes$MEASURE_TYPE[tempDataAPI[["data"]]$dimensions$COUNTRY == ""]))
    
    # end create MEASURE_TYPE
    
    temp <- metadata[metadata$DWID == id,]
    
    result <- data_2022[data_2022$Element %in% temp$Element,]
    result$SEX <- strsplit(result$Element,"[.]")
    result$SEX <- lapply(result$SEX, function(x) x[[2]])
    
    result$SEX <- gsub("T","ALL",result$SEX)
    result$SEX <- gsub("M","MALE",result$SEX)
    result$SEX <- gsub("F","FEMALE",result$SEX)
    
    result$COUNTRY <- ifelse(result$CNTRY %in% country_groups_DW,"",result$CNTRY)
    result$COUNTRY_GRP <- ifelse(result$CNTRY %in% country_groups_DW,result$CNTRY,"")
    result$MEASURE_TYPE <- ifelse(result$CNTRY %in% country_groups_DW,measure_type[2],measure_type[1])
    
    if(length(AGE_GRP_index)!=0){
      result <- data.frame(AGE_GRP_value,
                           result$COUNTRY, 
                           result$COUNTRY_GRP, 
                           result$SEX,
                           result$MEASURE_TYPE,
                           "",
                           result$Year, 
                           result$value)
      names(result) <- c(eval(AGE_GRP_name),"COUNTRY","COUNTRY_GRP","SEX","MEASURE_TYPE","SUBNATIONAL_MDB","YEAR","VALUE")
    }else{
      result <- data.frame(result$COUNTRY, 
                           result$COUNTRY_GRP, 
                           result$SEX,
                           result$MEASURE_TYPE,
                           "",
                           result$Year, 
                           result$value)
      names(result) <- c("COUNTRY","COUNTRY_GRP","SEX","MEASURE_TYPE","SUBNATIONAL_MDB","YEAR","VALUE")
    }
    
    # create INDICATOR section
    
    header <- data.frame(name=rep(NA, 7), value=rep("", 7), stringsAsFactors=FALSE)
    
    header[1,] <- c(as.character("INDICATOR"),id)
    header[2,] <- c("EXTERNID", strsplit(temp$Element[1],"[.]")[[1]][1])
    header[3,] <- c("DATA_SOURCE", as.character(tempDataAPI[["metadata"]]$value$code[tempDataAPI[["metadata"]]$code == "DATA_SOURCE"]))
    header[4,] <- c("UNIT_TYPE", as.character(tempDataAPI[["metadata"]]$value$code[tempDataAPI[["metadata"]]$code == "UNIT_TYPE"]))
    header[5,] <- c("DATA_TYPE_REPRESENTATION", as.character(tempDataAPI[["metadata"]]$value$code[tempDataAPI[["metadata"]]$code == "DATA_TYPE_REPRESENTATION"]))
    header[6,] <- c(NA,NA)
    
    totalRows <- totalRows + nrow(result)
    
    cat(paste("\n       ---------------------------            ", id, "                  ---------------------- "))
    cat(paste("\n       number of rows: ", nrow(result)))   
    cat(paste("\n       number of total rows: ", totalRows, "\n"))   
    cat("\n       attributes: ")
    cat(attributes)
    
    cat("\n-================ duplicated rows ===============- \n")
    cat(as.character(which(duplicated(result[,-ncol(result)]))))
    duplicat <- result[which(duplicated(result[,-ncol(result)])),]
    
    
    cat("\n-================ COUNTRY and COUNTRY_GRP are missing both (rows) ===============- \n")
    cat(as.character(which(is.na(result$COUNTRY)&&is.na(result$COUNTRY_GRP))))
    cat("\n")
    
    # Create the import file
    write.table(header, file=paste(outFilePath,id,".csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")
    write.table(result, file=paste(outFilePath,id,".csv", sep=''), row.names=F, col.name=T, append=T, na="", sep=",")  
    
    #End of HFAMDB indicators!
    
  }  else {      ##Start of HFA part
    
    attributes    <- tempDataAPI[["dimensions"]]$code #not used
    
    # create MEASURE_TYPE
    
    measure_type <- c(unique(tempDataAPI[["data"]]$attributes$MEASURE_TYPE[tempDataAPI[["data"]]$dimensions$COUNTRY != ""]),
                      unique(tempDataAPI[["data"]]$attributes$MEASURE_TYPE[tempDataAPI[["data"]]$dimensions$COUNTRY == ""]))
    
    # end create MEASURE_TYPE
    
    temp <- metadata[metadata$DWID == id,]
    
    result <- data_2022[data_2022$Element %in% temp$Element,]
    
    ####NEW SEX
    result$SEX <- strsplit(result$Element,"[.]")
    result$SEX <- lapply(result$SEX, function(x){ return( unlist(x)[length(unlist(x))])   } )
    result$SEX <- ifelse(result$SEX == "M","MALE",ifelse(result$SEX == "F","FEMALE",ifelse(result$SEX =="T", "ALL", 
                                                                                           ifelse((result$SEX =="GINI")|(result$SEX =="SocialSupport")|(result$SEX =="1000gOrNational")|(result$SEX =="nationalCriteria")|(result$SEX =="1000g"), "ALL", NA     )     )))
    
    
    result$COUNTRY <- ifelse(result$CNTRY %in% country_groups_DW,"",result$CNTRY)
    result$COUNTRY_GRP <- ifelse(result$CNTRY %in% country_groups_DW,result$CNTRY,"")
    result$MEASURE_TYPE <- ifelse(result$CNTRY %in% country_groups_DW,measure_type[2],measure_type[1])
    
    
    result <- data.frame(result$COUNTRY, 
                         result$COUNTRY_GRP, 
                         result$SEX,
                         result$Year,
                         result$MEASURE_TYPE,
                         result$value)
    names(result) <- c("COUNTRY","COUNTRY_GRP","SEX","YEAR","MEASURE_TYPE","VALUE")
    result$VALUE=round(result$VALUE,2)
    
    # create INDICATOR section
    
    header <- data.frame(name=rep(NA, 6), value=rep("", 6), stringsAsFactors=FALSE)
    
    header[1,] <- c(as.character("INDICATOR"),id)
    header[2,] <- c("EXTERNID",as.character(tempDataAPI[["metadata"]]$value$code[tempDataAPI[["metadata"]]$code == "EXTERNID"]) )     #strsplit(temp$Element[1],"[.]")[[1]][1])
    #Second posibility for header[2,] <- c("EXTERNID", " "  )
    #Third possibility for headear[2,] is use element before first "."
    header[3,] <- c("DATA_SOURCE", as.character(tempDataAPI[["metadata"]]$value$code[tempDataAPI[["metadata"]]$code == "DATA_SOURCE"]))
    header[4,] <- c("UNIT_TYPE", as.character(tempDataAPI[["metadata"]]$value$code[tempDataAPI[["metadata"]]$code == "UNIT_TYPE"]))
    header[5,] <- c("DATA_TYPE_REPRESENTATION", as.character(tempDataAPI[["metadata"]]$value$code[tempDataAPI[["metadata"]]$code == "DATA_TYPE_REPRESENTATION"]))
    header[6,] <- c(NA,NA)
    header[7,] <- c(NA,NA)
    
    totalRows <- totalRows + nrow(result)
    
    cat(paste("\n       ---------------------------            ", id, "                  ---------------------- "))
    cat(paste("\n       number of rows: ", nrow(result)))   
    cat(paste("\n       number of total rows: ", totalRows, "\n"))   
    cat("\n       attributes: ")
    cat(attributes)
    
    
    cat("\n-================ COUNTRY and COUNTRY_GRP are missing both (rows) ===============- \n")
    cat(as.character(which(is.na(result$COUNTRY)&&is.na(result$COUNTRY_GRP))))
    cat("\n")
    
    # Create the output file
    write.table(header, file=paste(outFilePath,id,".csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")
    write.table(result, file=paste(outFilePath,id,".csv", sep=''), row.names=F, col.name=T, append=T, na="", sep=",") 
    
  }    
  
  
}


#Manual fixes for issues found in visual inspection

##### HFA INDICATORS

HFA_file=read.csv(file=paste(outFilePath,"HFA_16.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="1.07"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_16.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_17.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="1.12"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_17.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_18.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="1.02"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_18.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_19.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="86440"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_19.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_20.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="44525"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_20.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_21.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="41915"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_21.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_31.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="1990" & HFA_file$V6=="107.4"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="1991" & HFA_file$V6=="112"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="1992" & HFA_file$V6=="939"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="1993" & HFA_file$V6=="1113"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="1994" & HFA_file$V6=="1780"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="1995" & HFA_file$V6=="609.5"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="1997" & HFA_file$V6=="103.7"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="1998" & HFA_file$V6=="99.2"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="1999" & HFA_file$V6=="91.5"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2000" & HFA_file$V6=="101.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2001" & HFA_file$V6=="101.5"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2002" & HFA_file$V6=="102.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2003" & HFA_file$V6=="102.2"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2004" & HFA_file$V6=="106.7"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2005" & HFA_file$V6=="109.6"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2006" & HFA_file$V6=="108.3"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2007" & HFA_file$V6=="116.7"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2008" & HFA_file$V6=="120.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2009" & HFA_file$V6=="101.5"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2010" & HFA_file$V6=="105.7"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2011" & HFA_file$V6=="107.9"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2012" & HFA_file$V6=="101.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2013" & HFA_file$V6=="102.4"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2014" & HFA_file$V6=="101.4"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2015" & HFA_file$V6=="104"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2016" & HFA_file$V6=="112.4"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AZE" & HFA_file$V4=="2017" & HFA_file$V6=="112.9"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="1991" & HFA_file$V6=="194.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="1992" & HFA_file$V6=="1070.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="1993" & HFA_file$V6=="1290.2"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="1994" & HFA_file$V6=="2321"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="1995" & HFA_file$V6=="809.3"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="1996" & HFA_file$V6=="153"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="1997" & HFA_file$V6=="163.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="1998" & HFA_file$V6=="173"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="1999" & HFA_file$V6=="393.7"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2000" & HFA_file$V6=="268.6"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2001" & HFA_file$V6=="161.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2002" & HFA_file$V6=="142.6"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2003" & HFA_file$V6=="128.4"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2004" & HFA_file$V6=="118.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2005" & HFA_file$V6=="110.3"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2006" & HFA_file$V6=="107"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2007" & HFA_file$V6=="108.4"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2008" & HFA_file$V6=="114.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2009" & HFA_file$V6=="113"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2010" & HFA_file$V6=="107.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2011" & HFA_file$V6=="153.2"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2012" & HFA_file$V6=="159.2"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2013" & HFA_file$V6=="118.3"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2014" & HFA_file$V6=="118.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2015" & HFA_file$V6=="113.5"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2016" & HFA_file$V6=="111.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2017" & HFA_file$V6=="106"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2018" & HFA_file$V6=="104.9"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2019" & HFA_file$V6=="105.6"),]
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2020" & HFA_file$V6=="105.5"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="1991" & HFA_file$V6=="260.4"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="1992" & HFA_file$V6=="2608.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="1993" & HFA_file$V6=="939.9"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="1994" & HFA_file$V6=="315.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="1995" & HFA_file$V6=="231.3"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="1996" & HFA_file$V6=="121.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="1997" & HFA_file$V6=="111"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="1998" & HFA_file$V6=="184.4"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="1999" & HFA_file$V6=="136.5"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2000" & HFA_file$V6=="120.2"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2001" & HFA_file$V6=="118.6"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2002" & HFA_file$V6=="115.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2003" & HFA_file$V6=="112"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2004" & HFA_file$V6=="111.7"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2005" & HFA_file$V6=="110.9"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2006" & HFA_file$V6=="109"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2007" & HFA_file$V6=="111.9"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2008" & HFA_file$V6=="113.3"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2009" & HFA_file$V6=="108.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2010" & HFA_file$V6=="108.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2011" & HFA_file$V6=="106.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2012" & HFA_file$V6=="106.6"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2013" & HFA_file$V6=="106.5"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2014" & HFA_file$V6=="111.4"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2015" & HFA_file$V6=="112.9"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2016" & HFA_file$V6=="105.4"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2017" & HFA_file$V6=="102.5"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2018" & HFA_file$V6=="104.3"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2019" & HFA_file$V6=="103"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2020" & HFA_file$V6=="104.9"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TJK" & HFA_file$V4=="1991" & HFA_file$V6=="113"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TJK" & HFA_file$V4=="1992" & HFA_file$V6=="907"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TJK" & HFA_file$V4=="1993" & HFA_file$V6=="2136"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TJK" & HFA_file$V4=="1994" & HFA_file$V6=="240"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TJK" & HFA_file$V4=="1995" & HFA_file$V6=="226.9"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TJK" & HFA_file$V4=="1998" & HFA_file$V6=="49.9"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TJK" & HFA_file$V4=="2010" & HFA_file$V6=="106.5"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TJK" & HFA_file$V4=="2011" & HFA_file$V6=="112.5"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TJK" & HFA_file$V4=="2012" & HFA_file$V6=="105.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TJK" & HFA_file$V4=="2013" & HFA_file$V6=="105.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TJK" & HFA_file$V4=="2014" & HFA_file$V6=="106.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TJK" & HFA_file$V4=="2015" & HFA_file$V6=="105.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TJK" & HFA_file$V4=="2016" & HFA_file$V6=="106.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TJK" & HFA_file$V4=="2017" & HFA_file$V6=="107.2"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="1991" & HFA_file$V6=="94"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="1992" & HFA_file$V6=="1650"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="1993" & HFA_file$V6=="3691.2"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="1994" & HFA_file$V6=="501"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="1995" & HFA_file$V6=="281.7"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="1996" & HFA_file$V6=="139.7"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="1997" & HFA_file$V6=="110.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="1998" & HFA_file$V6=="120"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="1999" & HFA_file$V6=="119"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2000" & HFA_file$V6=="126"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2001" & HFA_file$V6=="106.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2002" & HFA_file$V6=="100.8"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2020"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2020"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2019"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2019"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1990"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1990"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2019"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2019"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1990"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1990"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2020"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2020"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2020"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2020"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2019"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2019"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2019"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2019"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2020"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2020"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2006"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_31.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_40.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2001" & HFA_file$V6=="15.7"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2003" & HFA_file$V6=="52.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2004" & HFA_file$V6=="45.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2005" & HFA_file$V6=="49.5"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2006" & HFA_file$V6=="42.6"),]
HFA_file=HFA_file[-which(HFA_file$V1=="DNK" & HFA_file$V4=="2012" & HFA_file$V6=="0.3"),]
HFA_file=HFA_file[-which(HFA_file$V1=="DNK" & HFA_file$V4=="2013" & HFA_file$V6=="0.4"),]
HFA_file=HFA_file[-which(HFA_file$V1=="HUN" & HFA_file$V4=="1984" & HFA_file$V6=="69.2"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SRB" & HFA_file$V4=="2013" & HFA_file$V6=="2.3"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1984"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1984"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1984"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1984"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1984"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1984"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2001"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_40.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_41.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2001" & HFA_file$V6=="68.3"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2008" & HFA_file$V6=="71.9"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2011" & HFA_file$V6=="72.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2012" & HFA_file$V6=="74.3"),]
HFA_file=HFA_file[-which(HFA_file$V1=="HUN" & HFA_file$V4=="1984" & HFA_file$V6=="25.5"),]
HFA_file=HFA_file[-which(HFA_file$V1=="LUX" & HFA_file$V4=="2006" & HFA_file$V6=="11"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1984"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1984"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1984"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1984"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1984"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1984"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_41.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_84.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="49.63"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_84.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_88.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="49.63"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_88.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_89.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1993" & HFA_file$V6=="2.1"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1993"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_89.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_90.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1979" & HFA_file$V6=="-243.3"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1979"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1979"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1979"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1979"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1979"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1979"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1979"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1979"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_90.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_94.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="11.57"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_94.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_95.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1970" & HFA_file$V6=="-233.94"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1971" & HFA_file$V6=="-249.66"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1972" & HFA_file$V6=="-256.71"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1973" & HFA_file$V6=="-263.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1974" & HFA_file$V6=="-233.56"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1975" & HFA_file$V6=="-238.89"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1976" & HFA_file$V6=="-239.37"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1977" & HFA_file$V6=="-235.64"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1978" & HFA_file$V6=="-239.8"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1979" & HFA_file$V6=="-243.3"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1980" & HFA_file$V6=="-250.44"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1981" & HFA_file$V6=="-262.14"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1982" & HFA_file$V6=="-290.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1983" & HFA_file$V6=="-310.73"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1984" & HFA_file$V6=="-284.83"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1985" & HFA_file$V6=="-278.43"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1986" & HFA_file$V6=="-265.06"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1987" & HFA_file$V6=="-260.7"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1988" & HFA_file$V6=="-262.86"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ROU" & HFA_file$V4=="1989" & HFA_file$V6=="-270.33"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1977"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1977"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1988"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1987"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1975"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1975"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1977"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1977"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1987"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1971"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1971"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1976"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1976"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1985"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1985"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1972"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1972"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1974"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1974"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1972"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1972"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1987"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1972"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1972"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1986"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1986"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1989"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1973"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1973"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1978"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1978"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1973"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1973"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1972"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1972"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1977"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1977"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1983"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1983"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1979"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1979"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1983"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1983"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1986"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1986"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1988"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1971"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1971"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1983"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1983"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1979"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1979"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1986"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1986"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1984"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1984"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1974"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1974"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1985"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1985"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1984"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1984"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1976"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1976"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1971"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1971"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1978"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1978"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1989"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1974"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1974"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1973"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1973"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1971"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1971"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1984"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1984"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1976"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1976"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1985"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1985"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1986"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1986"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1989"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1978"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1978"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1975"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1975"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1989"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1977"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1977"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1976"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1976"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1987"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1988"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1984"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1984"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1985"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1985"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1974"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1974"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1988"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1983"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1983"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1979"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1979"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1979"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1979"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1978"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1978"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1973"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1973"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1975"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1975"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1975"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="1975"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_95.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_109.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2005" & HFA_file$V6=="8.86"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_109.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_110.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2005" & HFA_file$V6=="242.82"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_110.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_112.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2005" & HFA_file$V6=="12.8"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_112.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_113.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2005" & HFA_file$V6=="1309.9"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_113.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_115.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="BLR" & HFA_file$V4=="2005" & HFA_file$V6=="44.71"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_115.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_134.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1996" & HFA_file$V6=="0.02"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1997" & HFA_file$V6=="0.02"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1998" & HFA_file$V6=="0.04"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1999" & HFA_file$V6=="0.02"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1997" & HFA_file$V6=="0.01"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1998" & HFA_file$V6=="0.01"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_134.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_135.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1996" & HFA_file$V6=="0.03"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1997" & HFA_file$V6=="0.03"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1998" & HFA_file$V6=="0.06"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1999" & HFA_file$V6=="0.03"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1997" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1998" & HFA_file$V6=="0.03"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_135.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_136.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1996" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1997" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1998" & HFA_file$V6=="0.01"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1999" & HFA_file$V6=="0.02"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1997" & HFA_file$V6=="0.02"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1998" & HFA_file$V6=="0"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_136.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_137.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1996" & HFA_file$V6=="0.03"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1997" & HFA_file$V6=="0.06"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1998" & HFA_file$V6=="0.06"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1999" & HFA_file$V6=="0.05"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1997" & HFA_file$V6=="0.03"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1998" & HFA_file$V6=="0.08"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_137.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_138.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1996" & HFA_file$V6=="0.05"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1997" & HFA_file$V6=="0.08"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1998" & HFA_file$V6=="0.09"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1999" & HFA_file$V6=="0.09"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1997" & HFA_file$V6=="0.03"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1998" & HFA_file$V6=="0.07"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_138.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_139.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1996" & HFA_file$V6=="0.01"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1997" & HFA_file$V6=="0.04"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1998" & HFA_file$V6=="0.03"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1999" & HFA_file$V6=="0.03"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1997" & HFA_file$V6=="0.04"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1998" & HFA_file$V6=="0.09"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_139.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_140.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1996" & HFA_file$V6=="0.14"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1997" & HFA_file$V6=="0.4"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1998" & HFA_file$V6=="0.22"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1999" & HFA_file$V6=="0.27"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1997" & HFA_file$V6=="0.2"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1998" & HFA_file$V6=="0.65"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_140.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_141.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1996" & HFA_file$V6=="0.23"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1997" & HFA_file$V6=="0.45"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1998" & HFA_file$V6=="0.33"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1999" & HFA_file$V6=="0.59"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1997" & HFA_file$V6=="0.25"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1998" & HFA_file$V6=="0.4"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_141.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_142.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1996" & HFA_file$V6=="0.06"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1997" & HFA_file$V6=="0.32"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1998" & HFA_file$V6=="0.14"),]
HFA_file=HFA_file[-which(HFA_file$V1=="NLD" & HFA_file$V4=="1999" & HFA_file$V6=="0.11"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1997" & HFA_file$V6=="0.14"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1998" & HFA_file$V6=="0.84"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_142.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_191.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2005" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2006" & HFA_file$V6=="0.12"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2009" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2000" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2001" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2002" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2003" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2004" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2006" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="POL" & HFA_file$V4=="1980" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="POL" & HFA_file$V4=="1981" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="POL" & HFA_file$V4=="1982" & HFA_file$V6=="0"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2006"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_191.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_192.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2005" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2006" & HFA_file$V6=="0.24"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2009" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2000" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2001" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2002" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2003" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2004" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2006" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="POL" & HFA_file$V4=="1980" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="POL" & HFA_file$V4=="1981" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="POL" & HFA_file$V4=="1982" & HFA_file$V6=="0"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2003"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_192.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_198.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="LUX" & HFA_file$V4=="2018" & HFA_file$V6=="36.49"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_198.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_199.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="LUX" & HFA_file$V4=="2018" & HFA_file$V6=="196.94"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_199.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_296.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="CYP" & HFA_file$V4=="1999" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="CYP" & HFA_file$V4=="2000" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2012" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2011" & HFA_file$V6=="0"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_296.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_297.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="CYP" & HFA_file$V4=="1999" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="CYP" & HFA_file$V4=="2000" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2012" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2011" & HFA_file$V6=="0"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_297.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_298.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="CYP" & HFA_file$V4=="1999" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="CYP" & HFA_file$V4=="2000" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2012" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="RUS" & HFA_file$V4=="2011" & HFA_file$V6=="0"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_298.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_310.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="AND" & HFA_file$V4=="1997" & HFA_file$V6=="224"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AND" & HFA_file$V4=="2003" & HFA_file$V6=="109"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AND" & HFA_file$V4=="2006" & HFA_file$V6=="125"),]
HFA_file=HFA_file[-which(HFA_file$V1=="GEO" & HFA_file$V4=="2006" & HFA_file$V6=="110"),]
HFA_file=HFA_file[-which(HFA_file$V1=="GEO" & HFA_file$V4=="2007" & HFA_file$V6=="113"),]
HFA_file=HFA_file[-which(HFA_file$V1=="LTU" & HFA_file$V4=="2006" & HFA_file$V6=="103"),]
HFA_file=HFA_file[-which(HFA_file$V1=="LUX" & HFA_file$V4=="2003" & HFA_file$V6=="119"),]
HFA_file=HFA_file[-which(HFA_file$V1=="PRT" & HFA_file$V4=="2001" & HFA_file$V6=="102"),]
HFA_file=HFA_file[-which(HFA_file$V1=="PRT" & HFA_file$V4=="2002" & HFA_file$V6=="102"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SMR" & HFA_file$V4=="1997" & HFA_file$V6=="101"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SMR" & HFA_file$V4=="2000" & HFA_file$V6=="113"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2001"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_310.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_322.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="GBR" & HFA_file$V4=="1995" & HFA_file$V6=="400.83"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1995"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_322.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_323.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="GBR" & HFA_file$V4=="1995" & HFA_file$V6=="232583"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_323.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_351.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="SRB" & HFA_file$V4=="2002" & HFA_file$V6=="6.37"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_351.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_352.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="SRB" & HFA_file$V4=="2002" & HFA_file$V6=="478"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2002"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_352.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_391.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2016" & HFA_file$V6=="0.33"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_391.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_392.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2016" & HFA_file$V6=="146505"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_392.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_404.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="1996" & HFA_file$V6=="1403640"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="1997" & HFA_file$V6=="1429448"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="1998" & HFA_file$V6=="1473059"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="1999" & HFA_file$V6=="1577122"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2000" & HFA_file$V6=="1634157"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2001" & HFA_file$V6=="1657256"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2002" & HFA_file$V6=="1689809"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2003" & HFA_file$V6=="1719796"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2004" & HFA_file$V6=="1774709"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2005" & HFA_file$V6=="1789195"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2006" & HFA_file$V6=="1802332"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2007" & HFA_file$V6=="1812284"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2008" & HFA_file$V6=="157696"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2009" & HFA_file$V6=="146084"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2010" & HFA_file$V6=="146103"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2011" & HFA_file$V6=="143450"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2012" & HFA_file$V6=="140239"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2013" & HFA_file$V6=="139262"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2014" & HFA_file$V6=="112859"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2015" & HFA_file$V6=="104757"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2016" & HFA_file$V6=="102658"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2017" & HFA_file$V6=="95865"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2018" & HFA_file$V6=="93952"),]
HFA_file=HFA_file[-which(HFA_file$V1=="UKR" & HFA_file$V4=="2019" & HFA_file$V6=="88224"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2019"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2019"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2019"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2019"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_404.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_433.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="MCO" & HFA_file$V4=="2020" & HFA_file$V6=="1035.2"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2020"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2020"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2020"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2020"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_433.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_476.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1970" & HFA_file$V6=="1532.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1971" & HFA_file$V6=="1534.09"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1972" & HFA_file$V6=="1553.59"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1973" & HFA_file$V6=="1563.56"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1974" & HFA_file$V6=="1563.03"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1975" & HFA_file$V6=="1548.05"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1976" & HFA_file$V6=="1535.46"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1977" & HFA_file$V6=="1523.63"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1978" & HFA_file$V6=="1545.07"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1979" & HFA_file$V6=="1520.46"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1980" & HFA_file$V6=="1514.51"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1981" & HFA_file$V6=="1548.39"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1982" & HFA_file$V6=="1499.03"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1983" & HFA_file$V6=="1496.51"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1984" & HFA_file$V6=="1489.35"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1985" & HFA_file$V6=="1460.01"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1986" & HFA_file$V6=="1418.36"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1987" & HFA_file$V6=="1369.76"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1988" & HFA_file$V6=="1328.69"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1989" & HFA_file$V6=="1292.26"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1990" & HFA_file$V6=="1244.15"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1991" & HFA_file$V6=="1185.92"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1992" & HFA_file$V6=="666.56"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1993" & HFA_file$V6=="607.79"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1994" & HFA_file$V6=="518.6"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1995" & HFA_file$V6=="479.88"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1996" & HFA_file$V6=="431.39"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1997" & HFA_file$V6=="394.35"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1998" & HFA_file$V6=="375.47"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="1999" & HFA_file$V6=="369.78"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2000" & HFA_file$V6=="358.02"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2001" & HFA_file$V6=="327.36"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2002" & HFA_file$V6=="312.89"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2003" & HFA_file$V6=="305.1"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2004" & HFA_file$V6=="301.19"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2005" & HFA_file$V6=="293.24"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2006" & HFA_file$V6=="288.78"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2007" & HFA_file$V6=="286.35"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2008" & HFA_file$V6=="280.51"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2009" & HFA_file$V6=="275.88"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2010" & HFA_file$V6=="272.61"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2011" & HFA_file$V6=="270.56"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2012" & HFA_file$V6=="261.86"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2013" & HFA_file$V6=="259.42"),]
HFA_file=HFA_file[-which(HFA_file$V1=="SWE" & HFA_file$V4=="2014" & HFA_file$V6=="253.55"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1985"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1985"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1987"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1976"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1976"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1977"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1977"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1972"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1972"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1990"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1990"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1990"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1990"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1988"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1977"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1977"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1978"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1978"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1985"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1985"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1976"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1976"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1979"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1979"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1979"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1979"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1983"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1983"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1978"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1978"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1990"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1990"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1983"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1983"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1986"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1986"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1986"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1986"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1974"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1974"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1990"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1990"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1988"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1988"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1975"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1975"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1988"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1978"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1978"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1978"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1978"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1983"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1983"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1975"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1975"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1984"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1984"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1989"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1984"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1984"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1989"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1989"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1975"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1975"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1976"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1976"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1984"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1984"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1998"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1986"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1986"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1976"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1976"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1973"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1973"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1973"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1973"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1977"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1977"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1989"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1995"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1997"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1996"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1972"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1972"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1971"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1971"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1994"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1979"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1979"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1972"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1972"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1973"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1973"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1974"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1974"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1974"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1974"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1987"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1984"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1984"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1975"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1975"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1974"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1974"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1971"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1971"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1977"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1977"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1973"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1973"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1987"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1972"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1972"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1971"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1971"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1985"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1985"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1983"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1983"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1986"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1986"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1971"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="NORDIC" & HFA_file$V4=="1971"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1985"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1985"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1987"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1979"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1979"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_476.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_534.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="MCO" & HFA_file$V4=="2019" & HFA_file$V6=="110.01"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2019"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2019"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2019"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2019"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_534.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_536.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="AND" & HFA_file$V4=="2009" & HFA_file$V6=="-1.19"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AND" & HFA_file$V4=="2010" & HFA_file$V6=="-1.18"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AND" & HFA_file$V4=="2011" & HFA_file$V6=="-1.22"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AND" & HFA_file$V4=="2012" & HFA_file$V6=="-1.29"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AND" & HFA_file$V4=="2013" & HFA_file$V6=="-1.31"),]
HFA_file=HFA_file[-which(HFA_file$V1=="AND" & HFA_file$V4=="2014" & HFA_file$V6=="-1.37"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2014"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_536.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_542.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="TKM" & HFA_file$V4=="2012" & HFA_file$V6=="102.2"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TKM" & HFA_file$V4=="2013" & HFA_file$V6=="106.4"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TKM" & HFA_file$V4=="2014" & HFA_file$V6=="103"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TKM" & HFA_file$V4=="2015" & HFA_file$V6=="99.2"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TKM" & HFA_file$V4=="2016" & HFA_file$V6=="101.4"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_542.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_586.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2013" & HFA_file$V6=="32.22"),]
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2014" & HFA_file$V6=="29.47"),]
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="1168.28"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_586.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_603.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2006" & HFA_file$V6=="417.89"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2007" & HFA_file$V6=="512.79"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2008" & HFA_file$V6=="534.09"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2009" & HFA_file$V6=="554.6"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2010" & HFA_file$V6=="515.02"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2011" & HFA_file$V6=="568.26"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2012" & HFA_file$V6=="721.51"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2013" & HFA_file$V6=="748.17"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2014" & HFA_file$V6=="696.28"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2015" & HFA_file$V6=="733.79"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2016" & HFA_file$V6=="759.43"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2017" & HFA_file$V6=="906.83"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2018" & HFA_file$V6=="908"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2019" & HFA_file$V6=="927.89"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2019"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2019"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2019"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2019"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2019"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2019"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_603.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_604.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2006" & HFA_file$V6=="1261"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2007" & HFA_file$V6=="1651"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2008" & HFA_file$V6=="1812"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2009" & HFA_file$V6=="1983"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2010" & HFA_file$V6=="1894"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2011" & HFA_file$V6=="2117"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2012" & HFA_file$V6=="2749"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2013" & HFA_file$V6=="2897"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2014" & HFA_file$V6=="2780"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2015" & HFA_file$V6=="2921"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2016" & HFA_file$V6=="3043"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2017" & HFA_file$V6=="3539"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2018" & HFA_file$V6=="3612"),]
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="2019" & HFA_file$V6=="3733"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2019"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2019"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2013"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2013"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2015"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2015"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2012"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2016"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2007"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2010"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2014"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2008"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="2011"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2019"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2019"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2017"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2017"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2019"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="2019"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_604.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")


##### HFAMDB INDICATORS

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_451.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1988" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1992" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2009" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1988" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_451.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_453.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1988" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1992" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2009" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1988" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_453.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_454.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1987" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1988" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1992" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2008" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2009" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1987" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1988" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2008" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_454.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_455.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1988" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1992" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2008" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2009" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1988" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1992" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2008" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2009" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_455.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_456.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1987" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1987" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_456.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_457.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_457.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_458.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1992" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2009" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1992" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2009" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1988"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2009"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_458.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_459.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="MALE" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1994" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1995" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1996" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1997" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1998" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2000" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2001" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2002" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2003" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2005" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="ALB" & HFA_file$V4=="ALL" & HFA_file$V7=="2007" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1987"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1989"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1994"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1995"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1996"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1997"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1998"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2000"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2001"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2002"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2003"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2005"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2006"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="ALL" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="MALE" & HFA_file$V7=="2007"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="SEEHN" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2008"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_459.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_461.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012" & HFA_file$V8=="8.95"),]
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012" & HFA_file$V8=="4.23"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_461.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_466.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="KAZ" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="KAZ" & HFA_file$V4=="ALL" & HFA_file$V7=="1992" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="PRT" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="PRT" & HFA_file$V4=="ALL" & HFA_file$V7=="2004" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1992"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="EU_MEMBERS" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="EU_MEMBERS" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="EU_BEFORE_MAY2004" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="EU_BEFORE_MAY2004" & HFA_file$V4=="ALL" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="EU_MEMBERS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="EU_MEMBERS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
if (length(which(HFA_file$V3=="EU_BEFORE_MAY2004" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="EU_BEFORE_MAY2004" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2004"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_466.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_598.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012" & HFA_file$V8=="2.89"),]
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012" & HFA_file$V8=="1.36"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_598.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_609.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012" & HFA_file$V8=="2.89"),]
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012" & HFA_file$V8=="1.36"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_609.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_623.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012" & HFA_file$V8=="29.67"),]
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012" & HFA_file$V8=="15.43"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_623.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_688.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012" & HFA_file$V8=="32.21"),]
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012" & HFA_file$V8=="16.72"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_688.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_700.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_700.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_702.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_702.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_703.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_703.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_704.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_704.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_705.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_705.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_706.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_706.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_707.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_707.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_708.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_708.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_709.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_709.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_710.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1999" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_710.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")





###### DO NOT RUN PAST THIS POINT FOR 2022 UPDATE
###### THESE ARE THE CHANGES FOR INDICATORS WHICH 
###### WERE OMITTED FROM THE UPDATE DUE TO VALIDATION RULES VIOLATIONS

#####HFA

HFA_file=read.csv(file=paste(outFilePath,"HFA_61.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="29.59"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_61.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_62.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="32.05"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_62.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_63.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="26.98"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_63.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_74.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="28.98"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_74.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_75.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="31.42"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_75.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_76.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="26.39"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_76.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_78.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="16.31"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_78.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_82.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="DEU" & HFA_file$V4=="2018" & HFA_file$V6=="33.87"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="2018"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2018"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_82.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_193.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2005" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2006" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ALB" & HFA_file$V4=="2009" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2000" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2001" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2002" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2003" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2004" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="MNE" & HFA_file$V4=="2006" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="POL" & HFA_file$V4=="1980" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="POL" & HFA_file$V4=="1981" & HFA_file$V6=="0"),]
HFA_file=HFA_file[-which(HFA_file$V1=="POL" & HFA_file$V4=="1982" & HFA_file$V6=="0"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2001"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2001"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2002"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2002"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2000"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2000"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2006"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2006"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1982"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1982"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1981"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1981"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2005"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SEEHN" & HFA_file$V4=="2005"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2004"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2004"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2003"),]
if (length(which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1980"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_AFTER_MAY2004" & HFA_file$V4=="1980"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="2009"),]
if (length(which(HFA_file$V2=="SMALL" & HFA_file$V4=="2003"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="SMALL" & HFA_file$V4=="2003"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_193.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_221.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="1999" & HFA_file$V6=="4.3"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ESP" & HFA_file$V4=="1970" & HFA_file$V6=="40.28"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TKM" & HFA_file$V4=="1991" & HFA_file$V6=="13.83"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TKM" & HFA_file$V4=="1992" & HFA_file$V6=="11.17"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TKM" & HFA_file$V4=="1993" & HFA_file$V6=="14.4"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_221.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_222.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="1999" & HFA_file$V6=="6.22"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ESP" & HFA_file$V4=="1970" & HFA_file$V6=="56.24"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TKM" & HFA_file$V4=="1991" & HFA_file$V6=="18.09"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TKM" & HFA_file$V4=="1992" & HFA_file$V6=="13.34"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TKM" & HFA_file$V4=="1993" & HFA_file$V6=="15.13"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_222.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_223.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="1999" & HFA_file$V6=="2.78"),]
HFA_file=HFA_file[-which(HFA_file$V1=="ESP" & HFA_file$V4=="1970" & HFA_file$V6=="29.17"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TKM" & HFA_file$V4=="1991" & HFA_file$V6=="10.03"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TKM" & HFA_file$V4=="1992" & HFA_file$V6=="9.54"),]
HFA_file=HFA_file[-which(HFA_file$V1=="TKM" & HFA_file$V4=="1993" & HFA_file$V6=="13.68"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_BEFORE_MAY2004" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1993"),]
if (length(which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="EU_MEMBERS" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1991"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1970"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1970"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1992"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_223.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_224.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="1999" & HFA_file$V6=="11.35"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_224.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_225.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="1999" & HFA_file$V6=="17.06"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_225.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFA_226.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(6)))
HFA_file=HFA_file[-which(HFA_file$V1=="KAZ" & HFA_file$V4=="1999" & HFA_file$V6=="8.12"),]
if (length(which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CARINFONET" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="WHO_EURO" & HFA_file$V4=="1999"),]
if (length(which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"))==1) HFA_file=HFA_file[-which(HFA_file$V2=="CIS" & HFA_file$V4=="1999"),]
write.table(HFA_file, file=paste(outFilePath,"HFA_226.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")



#####HFAMDB

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_621.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012" & HFA_file$V8=="36.77"),]
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012" & HFA_file$V8=="18.93"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_621.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_686.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012" & HFA_file$V8=="39.94"),]
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012" & HFA_file$V8=="20.56"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_686.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_695.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012" & HFA_file$V8=="59.85"),]
HFA_file=HFA_file[-which(HFA_file$V2=="GEO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012" & HFA_file$V8=="29.94"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="2012"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="2012"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_695.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_739.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_739.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_745.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_745.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_746.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_746.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_747.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_747.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

HFA_file=read.csv(file=paste(outFilePath,"HFAMDB_748.csv", sep=''),header=FALSE, col.names=paste0("V",seq_len(8)))
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="MALE" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
HFA_file=HFA_file[-which(HFA_file$V2=="TKM" & HFA_file$V4=="ALL" & HFA_file$V7=="1993" & HFA_file$V8=="0"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="ALL" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="FEMALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="WHO_EURO" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CIS" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
if (length(which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"))==1) HFA_file=HFA_file[-which(HFA_file$V3=="CARINFONET" & HFA_file$V4=="MALE" & HFA_file$V7=="1993"),]
write.table(HFA_file, file=paste(outFilePath,"HFAMDB_748.csv", sep=''), row.names=F, col.names=F, append=F, na="",sep=",")

