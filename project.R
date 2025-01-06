# Load necessary libraries
library(dplyr)
library(tidyr)

#2. Load the dataset
cheese_data <- read.csv("cheese_data.csv")
head(cheese_data)

#3.a Total Number of Each ManufacturingType in the Province of BC
bc_cheese <- cheese_data %>%
filter(ManufacturerProvCode == "BC") %>%
count(ManufacturingTypeEn)
print(bc_cheese)

#3.b Total Number of Cheese by Province
cheese_by_province <- cheese_data %>%
count(ManufacturerProvCode)
print(cheese_by_province)
  
#3.c Statistics on MoisturePercent
# i. Cheese Category Type with the Greatest Average Moisture Percent
moisture_by_category <- cheese_data %>%
group_by(CategoryTypeEn) %>%
summarize(AverageMoisture = mean(MoisturePercent, na.rm = TRUE)) %>%
arrange(desc(AverageMoisture))
print(moisture_by_category)
  
# ii. Standard Deviation of Moisture Percent by Category
moisture_sd_by_category <- cheese_data %>%
group_by(CategoryTypeEn) %>%
summarize(MoistureSD = sd(MoisturePercent, na.rm = TRUE))
print(moisture_sd_by_category)

#4. Creating a DataFrame of Cheese in ON with Specific Columns
on_cheese <- cheese_data %>%
filter(ManufacturerProvCode == "ON") %>%
select(CheeseId, ManufacturerProvCode, ManufacturingTypeEn, FlavourEn, MoisturePercent, Organic)
head(on_cheese)

#5. Removing Entries with NA in FlavourEn From DataFrame
on_cheese <- cheese_data %>%
filter(ManufacturerProvCode == "ON") %>%
select(CheeseId, ManufacturerProvCode, ManufacturingTypeEn, FlavourEn, MoisturePercent, Organic) %>%
drop_na(FlavourEn)
head(on_cheese)

#6. Finding the Number of Organic and Non-Organic Cheeses in ON
organic_counts_on <- on_cheese %>%
count(Organic)
print(organic_counts_on)

#7. Creating a DataFrame with MoisturePercent Within 2 Standard Deviations of the Mean in ON
mean_moisture_on <- mean(on_cheese$MoisturePercent, na.rm = TRUE)
sd_moisture_on <- sd(on_cheese$MoisturePercent, na.rm = TRUE)
on_cheese_within_2sd <- on_cheese %>%
filter(MoisturePercent >= (mean_moisture_on - 2 * sd_moisture_on) &
MoisturePercent <= (mean_moisture_on + 2 * sd_moisture_on))
head(on_cheese_within_2sd)