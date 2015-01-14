# Create a new environment to encapsulate the problem variables
#if (!exists("assignment1")){
  assignment1 <- new.env()

  # Some information about the data set and its structure
  assignment1$measured.pollutants <- c('sulfate', 'nitrate')
  assignment1$units = list()

  assignment1$get.dir.df <- function(directory, id){
    # What files are in the specified directory?
    file.list = list.files(directory)
  
    # What files are we looking for?
    sought.files <-  sapply( id, function(i)
    {csv.file.name = paste(sprintf("%03d", i), '.csv', sep='')})
  
    # Are all the files found in the directory?
    if (!all(sapply(sought.files, function(file.name){any(file.list == file.name)})))
      stop()
  
    assignment1$units[[directory]] = id
    assignment1[[directory]] <- data.frame(Date = factor(), 
                                           sulfate = numeric(0), 
                                           nitrate= numeric(0),
                                           ID = integer(0))
  
    # If so, load them into the directory dataframe
    for (file in sought.files){
      temp <- read.csv(file = paste(directory, '/', file, sep = ''))
      assignment1[[directory]] <- rbind(assignment1[[directory]], temp)
    }
    rm(temp)
  }
  
  assignment1$get.missing.ids <- function(directory, id){
    # What files are in the specified directory?
    file.list = list.files(directory)
  
    # What files are we looking for?
    sought.files <-  sapply( id, function(i)
      {csv.file.name = paste(sprintf("%03d", i), '.csv', sep='')})
  
    # Are all the files found in the directory?
    if (!all(sapply(sought.files, function(file.name){any(file.list == file.name)})))
      stop()
  
    for (file in sought.files){
      temp <- read.csv(file = paste(directory, '/', file, sep = ''))
      assignment1[[directory]] <- rbind(assignment1[[directory]], temp)
    }
    rm(temp)    
    assignment1$units[[directory]] = unique(c(assignment1$units[[directory]], id))
  }

pollutantmean <- function(directory, pollutant, id = 1:332){
  # Ensure the pollutant arguments are valid
  if (!all(pollutant %in% assignment1$measured.pollutants)) stop()
  
  # Cache data for reuse
  if (!exists( directory, envir = assignment1))
    assignment1$get.dir.df(directory, id)
  
  # Are all the requested ids in memory?
  missing_ids <- !(id %in% assignment1$units[[directory]])
  if (any(missing_ids))
    assignment1$get.missing.ids(directory, id[missing_ids])
  
  ss <- assignment1[[directory]]$ID %in% id
  means <- sapply(subset(assignment1[[directory]][pollutant], ss), mean, na.rm=TRUE)
  means
}

complete <- function(directory, id = 1:332) {
  # Cache data for reuse
  if (!exists( directory, envir = assignment1))
    assignment1$get.dir.df(directory, id)
  
  # Are all the requested ids in memory?
  missing_ids <- !(id %in% assignment1$units[[directory]])
  if (any(missing_ids))
    assignment1$get.missing.ids(directory, id[missing_ids])
  
  # entries of interest
  cases = data.frame(id = integer(0), nobs = integer(0))
  for (i in id) {
    EoI <- assignment1[[directory]]$ID %in% i
    nob <- with(subset(assignment1[[directory]], EoI), sum(!(is.na(sulfate) | is.na(nitrate))))
    cases <- rbind(cases, data.frame(id = i, nobs = nob))
  }
  cases
}

corr <- function(directory, threshold = 0) {
  id = 1:332
  # Cache data for reuse
  if (!exists( directory, envir = assignment1))
    assignment1$get.dir.df(directory, id)
  
  # Are all the requested ids in memory?
  missing_ids <- !(id %in% assignment1$units[[directory]])
  if (any(missing_ids))
    assignment1$get.missing.ids(directory, id[missing_ids])
  
  cases <- complete(directory)
  sufficient_ids <- cases[cases$nobs >= threshold,]$id
  if (length(sufficient_ids) > 0){
    corr_value = numeric(length = length(sufficient_ids))
    for (i in 1:length(sufficient_ids)){
      relevant_obs <- assignment1[[directory]]$ID %in% sufficient_ids[i]
      corr_value[i] <- cor(assignment1[[directory]]$sulfate[relevant_obs], 
                      assignment1[[directory]]$nitrate[relevant_obs], use='pairwise.complete.obs')
      }
      corr_value
    } else {
    corr_value <- numeric(0)
    corr_value
  }
}