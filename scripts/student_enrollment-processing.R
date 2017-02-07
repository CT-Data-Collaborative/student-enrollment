# library(stringr)
# library(reshape2)
# library(data.table)
# library(plyr)

##################################################################
#
# Processing Script for Student Enrollment
# Created by Jenna Daly
# On 02/07/2017
#
##################################################################

#rename 's/^/student-/' * #adds `student-` to the beginning of each filename
#rename 's/\./_by-grade./g' *.csv -v #adds `_by-grade` to the end of each filename

# library(plyr)
# library(reshape2)

setwd("/home/jdaly/Desktop/Data/Student Enrollment")
sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path <- (paste0(getwd(), "/", data_location))
raw_enrollment_data <- dir(path, pattern = ".csv")

data1filename <- file.path(getwd(), "raw", "student-enrollment2007-2008_by-grade.csv")
data2filename <- file.path(getwd(), "raw", "student-enrollment2008-2009_by-grade.csv")
data3filename <- file.path(getwd(), "raw", "student-enrollment2009-2010_by-grade.csv")
data4filename <- file.path(getwd(), "raw", "student-enrollment2010-2011_by-grade.csv")
data5filename <- file.path(getwd(), "raw", "student-enrollment2011-2012_by-grade.csv")
data6filename <- file.path(getwd(), "raw", "student-enrollment2012-2013_by-grade.csv")
data7filename <- file.path(getwd(), "raw", "student-enrollment2013-2014_by-grade.csv")
data8filename <- file.path(getwd(), "raw", "student-enrollment2014-2015_by-grade.csv")
data9filename <- file.path(getwd(), "raw", "student-enrollment2015-2016_by-grade.csv")

filenames <- c("data1filename",
               "data2filename",
               "data3filename",
               "data4filename",
               "data5filename",
               "data6filename",
               "data7filename",
               "data8filename",
               "data9filename")

data1 <- read.csv(text=readLines(data1filename)[-(1:5)], header=T, stringsAsFactors=F, strip.white=TRUE, na.strings="*")
data2 <- read.csv(text=readLines(data2filename)[-(1:5)], header=T, stringsAsFactors=F, strip.white=TRUE, na.strings="*")
data3 <- read.csv(text=readLines(data3filename)[-(1:5)], header=T, stringsAsFactors=F, strip.white=TRUE, na.strings="*")
data4 <- read.csv(text=readLines(data4filename)[-(1:5)], header=T, stringsAsFactors=F, strip.white=TRUE, na.strings="*")
data5 <- read.csv(text=readLines(data5filename)[-(1:5)], header=T, stringsAsFactors=F, strip.white=TRUE, na.strings="*")
data6 <- read.csv(text=readLines(data6filename)[-(1:5)], header=T, stringsAsFactors=F, strip.white=TRUE, na.strings="*")
data7 <- read.csv(text=readLines(data7filename)[-(1:5)], header=T, stringsAsFactors=F, strip.white=TRUE, na.strings="*")
data8 <- read.csv(text=readLines(data8filename)[-(1:5)], header=T, stringsAsFactors=F, strip.white=TRUE, na.strings="*")
data9 <- read.csv(text=readLines(data9filename)[-(1:5)], header=T, stringsAsFactors=F, strip.white=TRUE, na.strings="*")

get_year1 <- gsub(".*enrollment\\s*|_by.*", "", data1filename)
get_year2 <- gsub(".*enrollment\\s*|_by.*", "", data2filename)
get_year3 <- gsub(".*enrollment\\s*|_by.*", "", data3filename)
get_year4 <- gsub(".*enrollment\\s*|_by.*", "", data4filename)
get_year5 <- gsub(".*enrollment\\s*|_by.*", "", data5filename)
get_year6 <- gsub(".*enrollment\\s*|_by.*", "", data6filename)
get_year7 <- gsub(".*enrollment\\s*|_by.*", "", data7filename)
get_year8 <- gsub(".*enrollment\\s*|_by.*", "", data8filename)
get_year9 <- gsub(".*enrollment\\s*|_by.*", "", data9filename)

data1$Year <- get_year1 
data2$Year <- get_year2 
data3$Year <- get_year3 
data4$Year <- get_year4 
data5$Year <- get_year5 
data6$Year <- get_year6 
data7$Year <- get_year7 
data8$Year <- get_year8 
data9$Year <- get_year9 

data1$District.Code <- NULL
data2$District.Code <- NULL
data3$District.Code <- NULL
data4$District.Code <- NULL
data5$District.Code <- NULL
data6$District.Code <- NULL
data7$District.Code <- NULL
data8$District.Code <- NULL
data9$District.Code <- NULL

data_all <- rbind(data1,
                  data2,
                  data3,
                  data4,
                  data5,
                  data6,
                  data7,
                  data8,
                  data9)

# convert columns to appropriate classes
data_all$District<-as.factor(data_all$District)
data_all$Year<-as.factor(data_all$Year)
data_all$Total<-as.integer(data_all$Total)

colnames(data_all) <- c("District", "Pre Kindergarten", "Kindergarten", "1", "2", 
                        "3", "4", "5", "6", "7", "8", 
                        "9", "10", "11", "12", "Total", "Year" )

cols_to_stack <- c( "Pre Kindergarten", "Kindergarten", "1", "2", "3", "4", 
                    "5", "6", "7", "8", 
                    "9", "10", "11", "12", "Total")
       
long_row_count = nrow(data_all) * length(cols_to_stack)

long <- reshape(data_all, 
                varying = cols_to_stack, 
                v.names = "Value", 
                timevar = "Grade", 
                times = cols_to_stack, 
                new.row.names = 1:long_row_count,
                direction = "long"
)

fips <- read.csv(paste0(path, "/", "school_district_ref.csv"), stringsAsFactors=F, header=T)

setDT(long)
setDT(fips)
setkey(fips, `District`)
setkey(long, `District`)
enrollment_data_all <- join(long, fips, by="District")

# unique(long$District)
# new_DF <- enrollment_data_all[is.na(enrollment_data_all$FixedDistrict),]
# unique(new_DF$District)

##MISSING DISTRICTS
# Eastern Connecticut Regional Educational Service Center (EASTCONN)     
# Park City Prep Charter School District                            
# Charter School for Young Children on Asylum Hill District              
# Elm City Montessori School District                                
# Booker T. Washington Academy District                                  
# Great Oaks Charter School District                                 
# Stamford Charter School for Excellence District                        
# Capital Preparatory Harbor School Inc. District  

setDT(enrollment_data_all)
enrollment_data_all[, `:=`(
  `Measure Type` = "Number",
  Variable = "Student Enrollment"
)]

enrollment_data_all$District = NULL
setcolorder(enrollment_data_all, c("id", "FixedDistrict", "FIPS", "Year", "Grade", "Measure Type", "Variable", "Value"))
colnames(enrollment_data_all)[2] <- "District"
enrollment_data_all <- enrollment_data_all[order(`District`, `Year`)]

enrollment_data_all$District[enrollment_data_all$District %in% c(NA)] <- "Connecticut"
enrollment_data_all$FIPS[enrollment_data_all$District %in% c("Connecticut")] <- 09

# Write to File
write.table(
  enrollment_data_all,
  file.path(getwd(), "data", "student-enrollment-by-grade_2007-2016.csv"),
  sep = ",",
  row.names = F
)