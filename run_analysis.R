run_analysis <- function(){
  ##going to use plyr here
  ## a function to load and tidy the individual datasets
  load_UCIHAR <- function(dataset,meanstd.var_names,column.names){
    ## this function will load the measurement, activity index and subject index files
    ## the measurements that are retained are set by meanstd.var_names, and the column names
    ## of the resulting dataset are set by column.names.
    ## input - either "test" or "train" to select the dataset, 
    ## meanstd.var_names, a vector with the indices of the columns to keep
    ## column.names, a character vector giving the column names for the measurements
    ## returns - dataframe of measurements
    
    if (dataset=='test' | dataset=='train'){
      ## load the measurements file and keep only the columns of mean or std deviations
      indata.data <- read.table(paste0('./UCI HAR Dataset/',dataset,'/X_',dataset,'.txt'))
      indata.data <- indata.data[,meanstd.var_names]
      
      ## load the activity index file
      indata.exps <- read.table(paste0('./UCI HAR Dataset/',dataset,'/y_',dataset,'.txt'))
      ##this changes the activity index values to their actual descriptive labels
      for (i in act.labels$V1){
        indata.exps[indata.exps==i] <- as.character(act.labels$V2[i])
      }
      
      ## load the subject id file
      indata.subject <- read.table(paste0('./UCI HAR Dataset/',dataset,'/subject_',dataset,'.txt'))
      
      ## join them together
      indata <- cbind(indata.exps,indata.subject,indata.data)
      
      ##set the column names
      colnames(indata) <- c('activity','subject',column.names)
      indata
    }
  }

  ##Code to perform the steps required of the course project
  
  ##1 - read in the data
  ## read in activity labels
  act.labels <- read.table('./UCI HAR Dataset/activity_labels.txt')
  
  ## read in row variable names
  row_var_names <- read.table('./UCI HAR Dataset/features.txt')
  
  ## find those that correspond to the mean and std of the measurements
  ## note use of the \\b prevents the meanFreq columns from being selected
  meanstd.var_names <- grep('\\bmean()\\b|\\bstd()\\b',row_var_names[,2])
  
  #get the column names, but tidy them up first: set to lower case, 
  #remove dashes and parentheses
  column.names <- as.character(row_var_names[meanstd.var_names,2])
  column.names <- tolower(column.names)
  column.names <- gsub("\\(|\\)|-","",column.names)
  
  ##load the "test" and "training" datasets, subject, and experiment
  ## datasets, passing the columns to be selected and the column names.
  ##load "test"
  test <- load_UCIHAR('test',meanstd.var_names,column.names)
  train <- load_UCIHAR('train',meanstd.var_names,column.names)
  
  ##merge the two datasets
  data.merged <- rbind(test,train)
  
  ## use the dplyr package to summarise the data
  library(dplyr)
  ## group by the activity and subject, then find the mean for each variable
  data.means <- summarise_each(group_by(data.merged,activity,subject), funs(mean))
  ## write the tidy dataset out to a text file
  write.table(x=data.means,file = './tidyDataset.txt',row.names=FALSE) 
}

