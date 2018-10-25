#!/usr/bin/Rscript

# R script to combine excel files. Place the files you wish to combine
# in a single directory named "data", accessible form the current directory

# require(gdata); # Requires Perl, native on OSX and Linux. For MS Windows, see
                # strawberryperl.com

cat("Plant or Interaction data? (P/I): ")
dtype <- readLines(file("stdin"),1)

cat("Data Point #? (1-6): ")
dpoint <- readLines(file("stdin"),1)

if (dtype == 'P') ncol = 10
if (dtype == 'I') ncol = 34


setwd("./interaction_data")
file_list <- list.files() # pattern = *.csv)
# print(file_list)

for (file in file_list){
       
  print(file)
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    # dataset <- read.xls(file, sheet = 1, header = TRUE)
    dataset <- read.csv(file)
    dataset <- subset(dataset, select = c(1:ncol))
    print(ncol(dataset))
  }
  else {
    # tmp <- read.xls(file, sheet = 1, header = TRUE)
    tmp <- read.csv(file)
    tmp <- subset(tmp, select = c(1:ncol))
    dataset <- rbind(dataset, tmp)
    print(ncol(tmp))
  }
}

setwd("..")
if (dtype == 'I') ftype = 'Interactions'
if (dtype == 'P') ftype = 'Plants'
file_name = sprintf("DataPoint_%s_%s.csv", dpoint, ftype)
write.csv(dataset, file_name)
