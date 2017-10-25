# bring in survey data from Excel workbooks

# Notes from Patrick:
# I have shared with you a Dropbox folder which contains every year of transect data.  
# Unfortunately, we do not have data from 2008.  
# Each of the excel spreadsheets are of one year of data and each sheet is one survey split by transects.  
# Years 2014 and 2015 are in a slightly different format.  
# Years 2009-2011 have transects E-10 (11 transects) which include the Entrance-Transect 1.  
# I have included a map that includes the “E” transect.  
# Years 2012-2013 have transect 1-11 (11 transects) in which Transect 1 = Transect E.  
# All other years do not include Transect E.


'%!in%' <- function(x,y)!('%in%'(x,y))





library(tidyverse)
library(devtools)
# devtools::install_github("nacnudus/tidyxl")
library(tidyxl)
# install.packages("readxl")
library(readxl)
library(lubridate)

# files in dropbox
xlpath <- "C:/Users/Tyson/Dropbox/Butterfly Monitoring Project/Butterfly Transect Data"
xlfiles <- list.files(xlpath)

# loop to input data from first 9 years
survlist <- list()
countlist <- list()
errors <- list()
for (i in 1:12){
  year <- as.numeric(unlist(strsplit(xlfiles[i], "[^0-9]+"))[2])
  sheet <- excel_sheets(paste(xlpath, xlfiles[i], sep = "/"))
  # remove blank templates and totals
  # parse dates 
  
  # parse_date_time(sheet, orders = c("md", "dmy"))
  
  for (j in sheet){ 
    
    test <- try(read_excel(paste(xlpath, xlfiles[i], sep = "/"), sheet = j), silent = TRUE)
    if("try-error" %in% class(test)){
      errors[[length(errors) + 1]] <- data.frame(year = i, sheet = j)
      next
    }else{
      
      example <- as.data.frame(read_excel(paste(xlpath, xlfiles[i], sep = "/"), sheet = j))
      
      specrowstart <- grep(pattern = "Species", x = example[,1], ignore.case = TRUE)
      if(length(specrowstart) == 0){
        errors[[length(errors) + 1]] <- data.frame(year = i, sheet = j)
        next #for sheets without the right data
      } 
      specrowend <- which(is.na(example[,1]))
      specrowend <- specrowend[which(specrowend > specrowstart)]
      if(length(specrowend) == 0){
        specrowend <- nrow(example)
      } else{
        specrowend <- min(specrowend[specrowend > specrowstart]) - 1
      }
      if(specrowend <= specrowstart){
        errors[[length(errors) + 1]] <- data.frame(year = i, sheet = j)
        next
      } 
      
      if(year %!in% c(2014, 2015)){
        survinfo <- as.data.frame(t(example[1:specrowstart-1, 2]))
        only_letters <- function(x) { gsub("^([[:alpha:]]*).*$","\\1",x) }
        names(survinfo) <- only_letters(gsub(pattern = " ", replacement = "",
                                             example[1:specrowstart-1, 1]))
        survinfo$SheetName <- j
        survinfo$Year <- year
        survlist[[length(survlist)+1]] <- survinfo
      }else{
        survinfo <-data.frame("Recorder" = NA, "StartTime" = NA, "EndTime" = NA, 
                              "StartTemp"= NA, 
        "EndTemp"= NA, "StartWindspeed"= NA, "EndWindspeed"= NA,
        "SheetName"= j, "Year"= year)
        survlist[[length(survlist)+1]] <- survinfo
      }
      counts <- example[(specrowstart+1):specrowend,]
      names(counts) <- example[(specrowstart - 1), ]
      names(counts)[1] <- "species"
      
      counts <- counts[, names(counts)[names(counts) %in% c("species", "E", as.character(1:11))]]
      counts[is.na(counts)] <- 0
      if("11" %in% names(counts)){
        if("E" %in% names(counts)){
          counts <- counts %>% select(-E)
        }
        names(counts) <- c("species", "E", as.character(1:10))
      }
      if("E" %!in% names(counts)){
        counts$E <- NA
      }
      counts$SheetName <- j
      counts$Year <- year
      
      countlist[[length(countlist)+1]] <- counts
    }
    
  } #end j loop
} #end i loop

out <- list()
for (i in 1:length(countlist)){
  out[[i]] <- survlist[[i]] %>% 
    dplyr::select("Recorder", "StartTime", "EndTime", "StartTemp", 
                  "EndTemp", "StartWindspeed", "EndWindspeed",
                  "SheetName", "Year") %>% 
    right_join(countlist[[i]])
}
# outdf <- bind_rows(out)
outdf <- data.table::rbindlist(out)

# convert Excel times
convertTime <- function(x){
  Hour = (x * 24) %% 24 #For x>1, only the value after decimal will be considered
  Minutes = (Hour %% 1) * 60
  Seconds = (Minutes %% 1) * 60
  
  hrs = ifelse (Hour < 10, paste("0",floor(Hour),sep = ""), as.character(floor(Hour)))
  mins = ifelse (Minutes < 10, paste("0",floor(Minutes),sep = ""), as.character(floor(Minutes)))    
  secs = ifelse (Seconds < 10, paste("0",round(Seconds,0),sep = ""), as.character(round(Seconds,0)))
  
  return(paste(hrs, mins, secs, sep = ":"))
}

outdf <- outdf %>% 
  mutate(StartTime = convertTime(as.numeric(as.character(StartTime))),
         EndTime = convertTime(as.numeric(as.character(EndTime))))

write.csv(x = outdf, file = "combinedData.csv", row.names = FALSE)


specrowstart <- grep(pattern = "Species Name", x = example[,1], fixed = TRUE)
specrowend <- which(is.na(example[,1]))
specrowend <- min(specrowend[specrowend > specrowstart])
survinfo <- as.data.frame(t(example[1:specrowstart-1, 2]))
names(survinfo) <- example[1:specrowstart-1, 1]
survlist[[length(survlist)+1]] <- survinfo

counts <- 




# for Excel integer date numbers and time fractions
# http://stackoverflow.com/questions/19172632/converting-excel-datetime-serial-number-to-r-datetime
# newDateTime <- as.Date(helpData[ , "ExcelNum"], origin = "1899-12-30")



