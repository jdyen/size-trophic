# merge species-level records with fishbase-ecology data set
setwd("~/Dropbox/research/size-trophic/")

# need some help
library(dplyr)

# load raw data sets
tp_data <- read.csv("data/raw-data-44k-records-with-stream-info.csv", stringsAsFactors = FALSE)
ecol_data <- read.csv("data/raw-data-fishbase-ecology.csv", stringsAsFactors = FALSE)
body_mass_data <- read.csv("data/raw-body-mass-data.csv", stringsAsFactors = FALSE)

# just need a few columns from the ecol data set
ecol_data <- ecol_data %>% select(
  herbivory = Herbivory2,
  feeding_type = FeedingType,
  add_rems = AddRems,
  SpecCode = SpecCode
)

# replace "Indo-Malay" with single word
tp_data$X3_Ecoregion <- gsub("-", "", tp_data$X3_Ecoregion)

# make a data frame with unique values of each variable for each species
tp_data <- tp_data %>% group_by(SpecCode) %>% summarise(
  tl = mean(log10(FoodTroph)),
  guild = unique(Trophicguild),
  len = mean(log10(LengthcmTL), na.rm = TRUE),
  ord = unique(Order_),
  ecoregion = sort(unique(X3_Ecoregion))[which.max(tapply(rep(1, length(X3_Ecoregion)), X3_Ecoregion, sum))],
  stream = unique(Stream),
  fresh = unique(Fresh),
  invasive = unique(Invasive),
  npp = mean(nppAvAnn01to15, na.rm = TRUE),
  discharge = mean(avYearDischarge, na.rm = TRUE),
  edhd = mean(EdHd, na.rm = TRUE),
  mobd = mean(MoBd, na.rm = TRUE),
  jlhd = mean(JlHd, na.rm = TRUE),
  cfdcpd = mean(CFdCPd, na.rm = TRUE)
)

# let's join the data sets
data_set <- left_join(tp_data, ecol_data, by = "SpecCode")

# and add in body mass data
body_mass_reduced <- body_mass_data %>%
  select(SpecCode, Wmax)
body_mass_reduced <- body_mass_reduced %>% 
  group_by(SpecCode) %>%
  summarise(
    Wmax = mean(Wmax, na.rm = TRUE)
  ) %>%
  ungroup
body_mass_reduced <- body_mass_reduced %>% mutate(
  Wmax = ifelse(is.nan(Wmax), NA, Wmax)
)
data_set <- left_join(data_set, body_mass_reduced, by = "SpecCode")

# save version with missing body mass data
write.csv(data_set, file = "data/raw-data-by-species-with-body-mass.csv")

# filter out some missing obs
data_set <- data_set %>% filter_all(
  all_vars(!is.na(.))
)

# save the output
write.csv(data_set, file = "data/raw-data-by-species-with-fishbase-info.csv")
