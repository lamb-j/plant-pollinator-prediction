# Script used for formating the raw data files
# input:  11to13Interactions_v2.csv
# output: int_Mx.RData

# We want to exclude the following:
  #  unknown plant species interactions
  #  unknown insect species interactions
  #  flagged interactions

#-------------------------
# Read in the csv
df_int <- read.csv("11to13Interactions_v2.csv", stringsAsFactors = FALSE)

# Remove redundant/uneeded columns
df_int$year              <- NULL
df_int$order             <- NULL
df_int$DBCODE            <- NULL
df_int$ENTITY            <- NULL
df_int$COMPLEX           <- NULL
df_int$MEADOW            <- NULL
df_int$DATE              <- NULL
df_int$WATCH             <- NULL
df_int$OBSERVER          <- NULL
df_int$PLOT              <- NULL
df_int$MINUTE            <- NULL
df_int$CLOUDS            <- NULL
df_int$WIND              <- NULL
df_int$TEMP              <- NULL
df_int$STATUS            <- NULL
df_int$PLTSP_CODE        <- NULL
df_int$VISSP_CODE        <- NULL
df_int$VECTOR.GUILD      <- NULL
df_int$COLLECTION.NUMBER <- NULL
df_int$NOTES             <- NULL

# Keeping 
  # NO_INT
  # PLTSP_NAME
  # VISSP_NAME
  # FLAG

# Remove rows with:
  #  no interactions
  #  flagged rows
  #  NA plant
  #  NA insect

df_int <- df_int[which(df_int$NO_INT > 0 & df_int$FLAG != 1), ]
df_int <- df_int[complete.cases(df_int[,4]),]

# Create sorted vectors for plant and insect species
iVec <- df_int$VISSP_NAME    
iVec <- unique(iVec)
iVec <- sort(iVec)

pVec <- df_int$PLTSP_NAME    
pVec <- unique(pVec)
pVec <- sort(pVec)

# Initialize a matrix for recording interactions
int_Mx <- matrix(0, ncol = length(iVec), nrow = length(pVec))
colnames(int_Mx) <- iVec
rownames(int_Mx) <- pVec

# Record the interactions
for (i in 1:nrow(df_int)) {
  print(i)

  plant   <- df_int[i,4]
  insect  <- df_int[i,5]
  num_int <- df_int[i,5]

  int_Mx[plant, insect] += num_int
}

save(int_Mx, file = "int_Mx.RData")
