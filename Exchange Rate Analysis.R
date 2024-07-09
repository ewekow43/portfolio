# Clear all existing variables from the workspace
rm(list=ls())

# Load necessary library
library(ggplot2)

# Function to fetch exchange rates from NBP for a given year
getNBPRates = function(year) {
  # Construct the URL for the CSV file for the given year
  link = paste('https://www.nbp.pl/kursy/Archiwum/archiwum_tab_a_', year, '.csv', sep = '')
  
  # Read the column names from the first row of the CSV file
  nazwy = read.csv(url(link), header = FALSE, nrows = 1, as.is = TRUE, sep = ";")
  
  # Read the exchange rates data, skipping the first two rows
  kursy = read.csv(url(link), skip = 2, header = FALSE, sep = ";", dec = ",")
  
  # Set the column names
  colnames(kursy) = nazwy
  
  # Convert the date column to Date format
  kursy$dni = as.Date(as.character(kursy$data), "%Y%m%d")
  
  # Convert the USD and EUR exchange rates to numeric format
  kursy$USD = as.numeric(gsub(',', '.', kursy$'1USD'))
  kursy$EUR = as.numeric(gsub(',', '.', kursy$'1EUR'))
  
  return(kursy)
}

# Initialize empty vectors to store dates and exchange rates
dni = c()
usd = c()
eur = c()

# Loop through the years from 2013 to 2020 and fetch the exchange rates
for(y in seq(2013, 2020)) {
  d = getNBPRates(y)
  
  dni = c(dni, d$dni)
  usd = c(usd, d$USD)
  eur = c(eur, d$EUR)
}

# Convert the dates to Date format
dniData = as.Date(dni, origin="1970-01-01")

# Create a data frame with the collected data
k = data.frame(dni = dniData, EUR = eur, USD = usd)

# Plot the exchange rates using ggplot2
p = ggplot() +
  geom_line(data = k, aes(x = dni, y = EUR, colour = "EUR")) + 
  geom_line(data = k, aes(x = dni, y = USD, colour = "USD")) +
  xlab("Date") +
  ylab("Exchange Rate") +
  ylim(3, 4.8) +
  labs(colour = "Legend")

# Display the plot
p

# Display the first few rows of the data frame
head(as.data.frame(k))
