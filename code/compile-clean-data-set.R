# start with all the data
data_set <- read.csv("data/raw-data-44k-records-new-morph-vars.csv")

# make a data frame with unique values of each variable for each species
sp_data <- data.frame(
  tl = c(tapply(data_set$FoodTroph, data_set$SpecCode, mean)),
  guild = c(tapply(data_set$Trophicguild, data_set$SpecCode, function(x) as.character(unique(x)))),
  len = c(tapply(data_set$LengthcmTL, data_set$SpecCode, mean, na.rm = TRUE)),
  ord = c(tapply(data_set$Order_, data_set$SpecCode, function(x) as.character(unique(x)))),
  fresh = c(tapply(data_set$Fresh, data_set$SpecCode, function(x) as.character(unique(x)))),
  invasive = c(tapply(data_set$Invasive, data_set$SpecCode, function(x) as.character(unique(x)))),
  edhd = c(tapply(data_set$EdHd, data_set$SpecCode, mean, na.rm = TRUE)),
  mobd = c(tapply(data_set$MoBd, data_set$SpecCode, mean, na.rm = TRUE)),
  jlhd = c(tapply(data_set$JlHd, data_set$SpecCode, mean, na.rm = TRUE)),
  cfdcpd = c(tapply(data_set$CFdCPd, data_set$SpecCode, mean, na.rm = TRUE)))

# there are some NAs, let's get rid of them
sp_data <- sp_data[apply(sp_data, 1, function(x) !any(is.na(x))), ]

# we don't want crazy big values of npp and discharge messing with things
sp_data$len <- scale(sp_data$len)
sp_data$edhd <- scale(sp_data$edhd)
sp_data$mobd <- scale(sp_data$mobd)
sp_data$jlhd <- scale(sp_data$jlhd)
sp_data$cfdcpd <- scale(sp_data$cfdcpd)
