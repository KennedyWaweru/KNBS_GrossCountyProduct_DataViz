library(tidyverse)
library(ggplot2)

setwd("/home/ken/Desktop/knbs-gcp")
gcp_economic_2018 <- read.csv("datasets/gcp-economic-activity-2018.csv")
gcp_economic_2019 <- read.csv("datasets/gcp-economic-activity-2019.csv")
gcp_economic_2020 <- read.csv("datasets/gcp-economic-activity-2020.csv") 

View(gcp_economic_2018)
View(gcp_economic_2019)
View(gcp_economic_2020)

# fix column names by replacing dots with _
names(gcp_economic_2018) <- gsub("[.]+","_",names(gcp_economic_2018))
names(gcp_economic_2019) <- gsub("[.]+","_",names(gcp_economic_2019))
names(gcp_economic_2020) <- gsub("[.]+","_",names(gcp_economic_2020))

# add year column per df
gcp_economic_2018$Year <- as.factor(2018)
gcp_economic_2019$Year <- as.factor(2019)
gcp_economic_2020$Year <- as.factor(2020)

# rbind the datasets to create one df
gcp_economic_sectors <- rbind(gcp_economic_2018, gcp_economic_2019, gcp_economic_2020)
View(gcp_economic_sectors)
# save the df for shiny app use
write.csv(gcp_economic_sectors, "datasets/gcp_economic_sectors.csv",row.names = FALSE)
# start by drawing a grouped bar chart
gcp_economic_sectors %>% filter(County=="KIAMBU"|County=="NYERI"|County=="MERU") %>%
  ggplot() +
  geom_col(aes(x=Year, y=Agriculture_Forestry_and_Mining,fill=County), position=position_dodge2(reverse=TRUE)) +
  labs(title="Agriculture Forestry and Mining", y="GCP, Kshs Million", x="Year") +
  theme_linedraw()

gcp_economic_sectors %>% filter(County=="KIAMBU"|County=="NYERI"|County=="MERU") %>%
  ggplot() +
  geom_col(aes(x=County, y=Agriculture_Forestry_and_Mining,fill=Year), position=position_dodge2()) +
  #geom_text(aes(label=Agriculture_Forestry_and_Mining), vjust=1.6, color="white", size=3.5) +
  labs(title="Agriculture Forestry and Mining", y="GCP, Kshs Million", x="Year") +
  theme_linedraw()
