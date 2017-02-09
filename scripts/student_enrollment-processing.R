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

#terminal calls to run in local directory, to set up filenames for processing
#rename 's/^/student-/' *                #adds `student-` to the beginning of each filename
#rename 's/\./_by-grade./g' *.csv -v     #adds `_by-grade` to the end of each filename

sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path <- (paste0(getwd(), "/", data_location))
raw_data <- dir(path, pattern = ".csv")
enrollment_data <- raw_data[grep("enrollment", raw_data)]

#for loop steps
##  reads in all the data into a data.frame, codes blank cells as NAs (accounts for rows where col# !=complete)
##  sums the number of NAs in each row
##  if (sum of NAs) > (max # of columns-2), removes rows (still allows districts where there is no data (NA for all grades))
##  puts back header
##  extracts year from name, adds year column
##  removes district code column
##  renames and saves new files
#######################
for (i in 1:length(enrollment_data)) {
  current_file <- read.csv(file.path(path, enrollment_data[i]), header=F, skip=0,
                           comment.char = "", check.names=T, stringsAsFactors=F, strip.white=TRUE, 
                           na.strings=c("*", "", " ","NA"))
  max_col <- ncol(current_file)
  current_file<- current_file[!((rowSums(is.na(current_file)) > max_col-2)),]
  colnames(current_file) = current_file[1, ]
  current_file = current_file[-1, ]
  get_year <- gsub(".*enrollment\\s*|_by.*", "", enrollment_data[i])
  current_file$Year <- get_year 
  current_file$"District Code" <- NULL
  assign(paste0("data_", get_year), current_file)
}
######################

#retrieves new files and binds them into one data.frame
dfs <- ls()[sapply(mget(ls(), .GlobalEnv), is.data.frame)]
get_data_only <- grep("^data", dfs, value=T)

data_all <- data.frame(stringsAsFactors=F)

for (i in 1:length(get_data_only)) {
  data_all <- rbind(data_all, get(get_data_only[i]))
}

# convert columns to appropriate classes
data_all$District<-as.factor(data_all$District)
data_all$Year<-as.factor(data_all$Year)
data_all$Total<-as.integer(data_all$Total)

#rename columns
colnames(data_all) <- c("District", "Pre Kindergarten", "Kindergarten", 
                        "1", "2", "3", "4", "5", "6", "7", "8", 
                        "9", "10", "11", "12", "Total", "Year" )

#reshape from wide to long format
cols_to_stack <- c( "Pre Kindergarten", "Kindergarten", 
                    "1", "2", "3", "4", "5", "6", "7", "8", 
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

#merge in FIPS
fips <- read.csv(paste0(path, "/", "school_district_ref.csv"), stringsAsFactors=F, header=T)
enrollment_data_all <- merge(long, fips, by="District")

##MISSING DISTRICTS from school_district_ref.csv
# Eastern Connecticut Regional Educational Service Center (EASTCONN)     
# Park City Prep Charter School District                            
# Charter School for Young Children on Asylum Hill District              
# Elm City Montessori School District                                
# Booker T. Washington Academy District                                  
# Great Oaks Charter School District                                 
# Stamford Charter School for Excellence District                        
# Capital Preparatory Harbor School Inc. District  

#add columns
enrollment_data_all$"Measure Type" <- "Number"
enrollment_data_all$"Variable" <- "Student Enrollment"

#reorder/rename columns
enrollment_data_all <- enrollment_data_all[c("id", "FixedDistrict", "District", "FIPS", "Year", "Grade", "Measure Type", "Variable", "Value")]
enrollment_data_all[3] = NULL
colnames(enrollment_data_all)[2] <- "District"

#order grade levels for sorting
enrollment_data_all$Grade <- factor(enrollment_data_all$Grade, levels = cols_to_stack)

#sort data
enrollment_data_all <- enrollment_data_all[order(enrollment_data_all$District, enrollment_data_all$Year, enrollment_data_all$Grade),]

#re-label "Total" as "Connecticut" and assign FIPS
enrollment_data_all$District[enrollment_data_all$District %in% c(NA)] <- "Connecticut"
enrollment_data_all$FIPS[enrollment_data_all$District %in% c("Connecticut")] <- 09

#create data frame that calls out all entries with no data for enrollment
value_NAs <- enrollment_data_all[is.na(enrollment_data_all$Value),]
value_NAs$Grade <- factor(value_NAs$Grade, levels = c("Pre Kindergarten", "Kindergarten", "1", "2", "3", "4", 
                                                      "5", "6", "7", "8", "9", "10", "11", "12", "Total"))
value_NAs <- value_NAs[order(value_NAs$District, value_NAs$Year, value_NAs$Grade),]

#recode no enrollment data with -9999
enrollment_data_all[["Value"]][is.na(enrollment_data_all[["Value"]])] <- -9999

#return blank in FIPS if not reported
enrollment_data_all$FIPS <- as.character(enrollment_data_all$FIPS)
enrollment_data_all[["FIPS"]][is.na(enrollment_data_all[["FIPS"]])] <- ""

# Write to File
write.table(
  enrollment_data_all,
  file.path(getwd(), "data", "student-enrollment-by-grade_2007-2016.csv"),
  sep = ",",
  row.names = F
)
