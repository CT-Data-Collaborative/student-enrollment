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

#set up working env
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

enrollment_combined <- data.frame(stringsAsFactors=F)

for (i in 1:length(get_data_only)) {
  enrollment_combined <- rbind(enrollment_combined, get(get_data_only[i]))
}

#removes indiv year files
rm(list=ls(pattern="data"))

# convert columns to appropriate classes
enrollment_combined$District<-as.factor(enrollment_combined$District)
enrollment_combined$Year<-as.factor(enrollment_combined$Year)
enrollment_combined$Total<-as.integer(enrollment_combined$Total)

#rename columns
colnames(enrollment_combined) <- c("District", "Pre Kindergarten", "Kindergarten", 
                                   "1", "2", "3", "4", "5", "6", "7", "8", 
                                   "9", "10", "11", "12", "Total", "Year" )

#reshape from wide to long format
cols_to_stack <- c( "Pre Kindergarten", "Kindergarten", 
                    "1", "2", "3", "4", "5", "6", "7", "8", 
                    "9", "10", "11", "12", "Total")

long_row_count = nrow(enrollment_combined) * length(cols_to_stack)

long <- reshape(enrollment_combined, 
                varying = cols_to_stack, 
                v.names = "Value", 
                timevar = "Grade", 
                times = cols_to_stack, 
                new.row.names = 1:long_row_count,
                direction = "long"
)

#reorder columns and remove ID column
long <- long[order(long$District, long$Year),]
long$id <- NULL

#merge in FIPS and match Districts to their Fixed Districts
fips <- read.csv(paste0(path, "/", "school_district_ref.csv"), stringsAsFactors=F, header=T)

merge_long_fips <- merge(long, fips, all=T)

#unique(merge_long_fips$FixedDistrict)

#remove rows where year, grade and value is.na (removes those rows where District(raw) != District(ref))
remove_NAs <- merge_long_fips[!with(merge_long_fips,is.na(merge_long_fips$Year) & is.na(merge_long_fips$Grade) & is.na(merge_long_fips$Value)),]

#unique(remove_NAs$FixedDistrict)

#re-label FixedDistrict=NA as Connecticut (all "Total" rows from District(raw))
remove_NAs$District <- as.character(remove_NAs$District)
remove_NAs <- within(remove_NAs, FixedDistrict[District == 'Total'] <- 'Connecticut')
remove_NAs <- within(remove_NAs, FIPS[District == 'Total'] <- '09')

#unique(remove_NAs$FixedDistrict)

#backfill years (identify missing years)
years <- c(
  "2007-2008", 
  "2008-2009", 
  "2009-2010", 
  "2010-2011", 
  "2011-2012", 
  "2012-2013", 
  "2013-2014", 
  "2014-2015", 
  "2015-2016"
)

#backfill grades
grades <- c(
  "Pre Kindergarten", 
  "Kindergarten", 
  "1", 
  "2", 
  "3", 
  "4", 
  "5", 
  "6", 
  "7", 
  "8", 
  "9", 
  "10", 
  "11", 
  "12", 
  "Total"
)

backfill <- expand.grid(
  `FixedDistrict` = unique(remove_NAs$FixedDistrict),
  `Year` = years,
  `Grade` = grades
)

#backfill has all FixedDistrict, Grade, and Year permutations
backfill <- backfill[order(backfill$FixedDistrict, backfill$Year, backfill$Grade),]

#reorder Grade levels for sorting
remove_NAs$Grade <- factor(remove_NAs$Grade, levels = cols_to_stack)
remove_NAs <- remove_NAs[order(remove_NAs$FixedDistrict, remove_NAs$Year, remove_NAs$Grade),]

backfill_data <- merge(remove_NAs, backfill, all=T) 
backfill_data <- backfill_data[order(backfill_data$FixedDistrict, backfill_data$District),]

# convert columns to appropriate classes
backfill_data$District <- as.character(backfill_data$FixedDistrict)
backfill_data$Year <- as.character(backfill_data$Year)
backfill_data$Grade <- as.character(backfill_data$Grade)

backfill_data <- backfill_data[order(backfill_data$FixedDistrict, backfill_data$District, backfill_data$Year, backfill_data$Grade),]

#reorder columns
backfill_data <- backfill_data[c("FixedDistrict", "District", "FIPS", "Year", "Grade", "Value")]

#finds all districts that report values for the same year, for the same fixed district (only needs to be done if rowcount(backfill) != rowcount(backfill_data)) *should add conditional if statement
duplicates <- backfill_data[(duplicated(backfill_data[c("FixedDistrict","Year","Grade")]) | duplicated(backfill_data[c("FixedDistrict","Year","Grade")], fromLast = TRUE)), ]

duplicates$Value <- as.integer(duplicates$Value)
remove_duplicates <- aggregate(Value ~ FixedDistrict + District + Year + Grade , data = duplicates, sum)

remove_duplicates <- remove_duplicates[c("FixedDistrict", "District", "Year", "Grade", "Value")]
remove_duplicates$Grade <- factor(remove_duplicates$Grade, levels = cols_to_stack)

remove_duplicates <- remove_duplicates[order(remove_duplicates$FixedDistrict, remove_duplicates$District, remove_duplicates$Year, remove_duplicates$Grade),]

#replaces duplicates from backfilled data with aggregated values
enrollment_combined_final <- merge(backfill_data, remove_duplicates, by= c("FixedDistrict", "District", "Year", "Grade"),  all.x = T)
enrollment_combined_final$Value.x[!is.na(enrollment_combined_final$Value.y)] <- enrollment_combined_final$Value.y[!is.na(enrollment_combined_final$Value.y)]
enrollment_combined_final<-enrollment_combined_final[!duplicated(enrollment_combined_final), ]
enrollment_combined_final$Value.y <-NULL

#reconfigure column names
enrollment_combined_final <- enrollment_combined_final[c("FixedDistrict", "District", "FIPS", "Year", "Grade", "Value.x")]
colnames(enrollment_combined_final)[6] <- "Value"
enrollment_combined_final[2] = NULL
colnames(enrollment_combined_final)[1] <- "District"

enrollment_combined_final$District <- as.character(enrollment_combined_final$District)
enrollment_combined_final$Year <- as.character(enrollment_combined_final$Year)
enrollment_combined_final$Grade <- as.character(enrollment_combined_final$Grade)
enrollment_combined_final$Value <- as.integer(enrollment_combined_final$Value)

#add columns
enrollment_combined_final$"Measure Type" <- "Number"
enrollment_combined_final$"Variable" <- "Student Enrollment"

##ADDED DISTRICTS from school_district_ref.csv
# Eastern Connecticut Regional Educational Service Center (EASTCONN)     
# Park City Prep Charter School District                            
# Charter School for Young Children on Asylum Hill District              
# Elm City Montessori School District                                
# Booker T. Washington Academy District                                  
# Great Oaks Charter School District                                 
# Stamford Charter School for Excellence District                        
# Capital Preparatory Harbor School Inc. District  

##REMOVED DISTRICTS from school_district_ref.csv
# Department of Children and Families
# Department of Corrections

enrollment_combined_final <- enrollment_combined_final[c("District", "FIPS", "Year", "Grade", "Measure Type", "Variable", "Value")]

#order grade levels for sorting
enrollment_combined_final$Grade <- factor(enrollment_combined_final$Grade, levels = cols_to_stack)

#sort data
enrollment_combined_final <- enrollment_combined_final[order(enrollment_combined_final$District, enrollment_combined_final$Year, enrollment_combined_final$Grade),]


#create data frame that calls out all entries with no data for enrollment (both for any grade and any year) (important - done before imputation)
##################################################################################################################
value_NAs <- enrollment_combined_final[is.na(enrollment_combined_final$Value),]
value_NAs$Grade <- factor(value_NAs$Grade, levels = c("Pre Kindergarten", "Kindergarten", "1", "2", "3", "4", 
                                                      "5", "6", "7", "8", "9", "10", "11", "12", "Total"))
value_NAs <- value_NAs[order(value_NAs$District, value_NAs$Year, value_NAs$Grade),]
##################################################################################################################

#recode no enrollment data with -9999
enrollment_combined_final[["Value"]][is.na(enrollment_combined_final[["Value"]])] <- -9999

#return blank in FIPS if not reported
enrollment_combined_final$FIPS <- as.character(enrollment_combined_final$FIPS)
enrollment_combined_final[["FIPS"]][is.na(enrollment_combined_final[["FIPS"]])] <- ""


# Write to File
write.table(
  enrollment_combined_final,
  file.path(getwd(), "data", "student-enrollment-by-grade_2007-2016.csv"),
  sep = ",",
  row.names = F
)
