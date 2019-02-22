library(dplyr)
raw_data <- list()
raw_data[["fall2014"]] <- read.csv("documents/fall2014.csv",header = F, stringsAsFactors = F)
raw_data[["spring2015"]] <- read.csv("documents/spring2015.csv", header = F, stringsAsFactors = F)
raw_data[["fall2015"]] <- read.csv("documents/fall2015.csv",header = F, stringsAsFactors = F)
raw_data[["spring2016"]] <- read.csv("documents/spring2016.csv", header = F, stringsAsFactors = F)
raw_data[["summer2016"]] <- read.csv("documents/summer2016.csv", header = F, stringsAsFactors = F)
raw_data[["fall2016"]] <- read.csv("documents/fall2016.csv", header = F, stringsAsFactors = F)


results_list <- list()

section_detector <- function(row, current_section) {
  switch(unlist(row[1]),
         `Test Name` = "header",
         `Section Name` = "section",
         `Sample Number` = "samples",
         `Test_Name` = "tests",
         current_section)
}

# Read through each data.frame in the raw_data list
for( f in 1:length(raw_data) ) {
  frame <- data.frame(raw_data[f])

  results_list[[names(raw_data)[f]]] <- list()

  # data_section <- c("header","section","samples","tests")
  data_section <- ""
  for(r in 1:nrow(frame) ) {
    data_section <- section_detector(frame[r,],data_section)
    results_list[[names(raw_data)[f]]][[data_section]] <- rbind(results_list[[names(raw_data)[f]]][[data_section]],
                                                                  frame[r,])
  }
}

# Get list of just the test results
test_results <- lapply(results_list,
                       function(x)
                         {names(x[["tests"]]) <- results_list[[1]][["tests"]][1,]
                         return(x[["tests"]])})
# Bind all the test results together
tests <- do.call("rbind",test_results)
# Clean up question names
names(tests)[7:26] <- c("Acid",
                        "Astringent",
                        "Barny",
                        "Bitter",
                        "Cooked",
                        "Feed",
                        "Flat",
                        "Foreign_Chemical",
                        "Fruity_Fermented",
                        "Lacks_Freshness",
                        "Light_Oxidized",
                        "Lipid_Oxidized",
                        "Malty",
                        "Milk_Carton",
                        "Rancid",
                        "Unclean",
                        "Coagulated",
                        "Other",
                        "Overall",
                        "Comment")

# Add appropriate factors
tests$Season[grepl("Fall",tests$Test_Name)] <- "Fall"
tests$Season[grepl("Spring",tests$Test_Name)] <- "Spring"
tests$Season[grepl("Summer",tests$Test_Name)] <- "Summer"

tests$Year[grepl("2014",tests$Test_Name)] <- "2014"
tests$Year[grepl("2015",tests$Test_Name)] <- "2015"
tests$Year[grepl("2016",tests$Test_Name)] <- "2016"

tests$Pre_Post[grepl("Pre-",tests$Test_Name)] <- "Pre"
tests$Pre_Post[grepl("Post-",tests$Test_Name)] <- "Post"

tests$Milk_Type[grepl("2%",tests$Test_Name)] <- "2%"
tests$Milk_Type[grepl("SKIM",tests$Test_Name)] <- "Skim"
tests$Milk_Type[grepl("Skim",tests$Test_Name)] <- "Skim"

tests$Defect[grepl("CONTROL",tests$Product_Name)] <- "Control"
tests$Defect[grepl("Malty",tests$Product_Name)] <- "Malty"
tests$Defect[grepl("Fruity",tests$Product_Name)] <- "Fruity_Fermented"
tests$Defect[grepl("Bitter",tests$Product_Name)] <- "Bitter"
tests$Defect[grepl("Acid",tests$Product_Name)] <- "Acid"
tests$Defect[grepl("Light Oxidized",tests$Product_Name)] <- "Light_Oxidized"
tests$Defect[grepl("Milk Carton",tests$Product_Name)] <- "Milk_Carton"
tests$Defect[grepl("Cooked",tests$Product_Name)] <- "Cooked"
tests$Defect[grepl("Flat",tests$Product_Name)] <- "Flat"
tests$Defect[grepl("Feed",tests$Product_Name)] <- "Feed"
tests$Defect[grepl("Rancid",tests$Product_Name)] <- "Rancid"


# Drop extraneous rows
tests <- tests[!is.na(tests$Pre_Post),]

# Now we need to determine which session was "initial training" for each.
# I'm going to do this by adding a column representing the order of the sessions
# and then choosing the minimum for each participant.

tests$Session_order <- paste(tests$Year,tests$Season)
tests$Session_order[tests$Session_order=="2014 Fall"] <- 1
tests$Session_order[tests$Session_order=="2015 Spring"] <- 2
tests$Session_order[tests$Session_order=="2015 Fall"] <- 3
tests$Session_order[tests$Session_order=="2016 Spring"] <- 4
tests$Session_order[tests$Session_order=="2016 Summer"] <- 5
tests$Session_order[tests$Session_order=="2016 Fall"] <- 6
tests$Session_order <- as.numeric(tests$Session_order)

# Choose minimum session
tests %>%
  group_by(Panelist_Code) %>%
  summarize(initial=min(Session_order)) -> panelist_initial

# Merge minimum into dataframe and add column for initial training
merge(tests,panelist_initial,by="Panelist_Code") %>%
  mutate(Initial_Training=Session_order==initial) %>%
  select(-initial,-Session_order) -> tests

# Make scores numeric
for( r in 7:25 ) {
  tests[,r] <- as.numeric(tests[,r])
  tests[is.na(tests[,r]),r] <- 0
}
tests[,7:25]

# See which sessions are marked as initial training for whom; check to make
# sure that they match Nancy's spreadsheet
tests %>%
  group_by(Panelist_Code, Season, Year) %>%
  summarize(initial=all(Initial_Training)) -> initial
# yy662 is present in Nancy's spreadsheet for Spring 2016 but not present
# in the datasheets; netID is actually yy622 in datasheet

# Remove dk657 from Spring 2016; didn't complete Post-Eval
tests %>%
  filter(!(Panelist_Code=="dk657" & Year=="2016" & Season=="Spring")) -> tests

# Remove all nrs13 records
tests %>%
  filter(!(Panelist_Code=="nrs13")) -> tests

# Remove jat324 from Fall 2016; didn't finish testing
tests %>%
  filter(!(Panelist_Code=="jat324" & Year=="2016" & Season=="Fall")) -> tests

# Remove sw38 from Fall 2015, Spring 2016, and Summer 2016; failed first
# re-training but was recorded as passed in error
tests %>%
  filter(!(Panelist_Code=="sw38" & Year=="2015" & Season=="Fall")) %>%
  filter(!(Panelist_Code=="sw38" & Year=="2016" & Season=="Spring")) %>%
  filter(!(Panelist_Code=="sw38" & Year=="2016" & Season=="Summer")) -> tests

# Function takes a row and returns the defect marked by the panelist
defect_detector <- function(x) {
  defects <- c("Acid","Astringent","Barny","Bitter",
               "Cooked","Feed","Flat","Foreign_Chemical",
               "Fruity_Fermented","Lacks_Freshness","Light_Oxidized","Lipid_Oxidized",
               "Malty","Milk_Carton","Rancid","Unclean",
               "Coagulated","Other")
  defect_vector <- x[7:24]
  scored <- defects[defect_vector > 0]
  if(length(scored) == 0) {
    return("Control")
  } else if(length(scored) > 1) {
    return("More than one")
  } else {
    return(scored)
  }
}

# Add a column to record which defect the panelist chose
for( r in 1:nrow(tests) ) {
  tests$Marked_Defect[r] <- defect_detector(tests[r,])
}

# Add a column to record whether the marked defect is correct
tests %>%
  mutate(correct=Defect==Marked_Defect) -> tests

# Add a column to record whether the marked defect is "Control"
tests %>%
  mutate(Control=as.numeric(Marked_Defect=="Control")) -> tests

tests$Defect <- relevel(factor(tests$Defect),ref = "Control")
tests$Pre_Post <- factor(tests$Pre_Post,levels = c("Pre","Post"))
tests$Milk_Type <- factor(tests$Milk_Type)
tests$initial <- "Re-training"
tests$initial[tests$Initial_Training] <- "Initial training"
tests$initial <- factor(tests$initial,levels=c("Initial training","Re-training"))

# Delete everything except tests dataframe
#rm(list=ls()[ls() != "tests"])
rm(list=ls()[!(ls() %in% c("tests","initial_table","m","retraining_table"))])
