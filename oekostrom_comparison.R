# Load libraries
library(janitor)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)

# Load data from OekoInstitut https://www.ecotopten.de/strom/oekostrom-tarife
raw <- read.csv("Data/oekostrom.csv")

# Clean texts
raw <- clean_names(raw)

raw$grundpreis_monat <- as.numeric(sub(",",".", raw$grundpreis_monat))
raw$arbeitspreis_cent_k_wh <- as.numeric(sub(",",".", raw$arbeitspreis_cent_k_wh))

# Select relevant columns 
greenpower <- raw %>% select(-mindestvertragslaufzeit_monate, -kundigungsfrist_wochen, -preisgarantie_bis, -gesamtkosten_2_personen_jahr, -wer_steht_dahinter, -varianz_tarif) %>%
  na.omit(hersteller) %>% filter(!str_detect(modell, "empfehlenswerter"))
  

# How many providers? 
length(unique(greenpower$hersteller)) # 35

# How many products? 
length(unique(greenpower$modell)) #44

# Calculate yearly and average price for a household that consumes 3500kwh 
greenpower$ann_price_euros <- (greenpower$grundpreis_monat * 12) + (greenpower$arbeitspreis_cent_k_wh*3500) #still in cenrts
greenpower$price_per_kwh <- round(greenpower$ann_price / 3500,2) # price per kwh in cents 
greenpower$ann_price_euros <- round(greenpower$ann_price/100,2) # annual price in euros
greenpower$monthly_price <- round(greenpower$ann_price/12,2) # monthly price in euros

greenpower <- greenpower %>% arrange(price_per_kwh)


# Create a frequency table
attach(greenpower)
breaks <- seq(from = min(price_per_kwh), to=max(price_per_kwh), by=2)  # Create intervals of 5

pop <- cut(price_per_kwh, breaks=breaks, right=TRUE, include.lowest=TRUE) # Create frequency tables for the variable

frequency <- cbind(table(pop))
colnames(frequency) <- c("count")
frequency <- as.data.frame(frequency)
frequency 

detach(greenpower)

# Create a frequency graph
price <- greenpower$price_per_kwh
hist(price, breaks=5, main="Price of Green Electricity",
     xlab = "Price in cents/kWh", col ="limegreen")


# Which labels occur frequently? 
table(greenpower$zertifikat)
ggplot(greenpower, aes(x=zertifikat, fill=zertifikat)) + geom_bar(width=0.7) + theme_linedraw() +
  theme(legend.position = "none") +
  labs(x="", y="Number of products with this label", title="Germany's green electricity labels") +
  ylim(0,50)
  
