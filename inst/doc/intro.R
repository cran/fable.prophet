## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7
)

## ----setup, results = 'hide'--------------------------------------------------
library(tsibble)
library(fable.prophet)

## ----data, message = FALSE----------------------------------------------------
# Read in the data
lax_passengers <- read.csv("https://raw.githubusercontent.com/mitchelloharawild/fable.prophet/master/data-raw/lax_passengers.csv")

# Tidy and summarise the data for comparison of international and domestic passenger counts
library(dplyr)
library(lubridate)
lax_passengers <- lax_passengers %>%
  mutate(datetime = mdy_hms(ReportPeriod)) %>%
  group_by(month = yearmonth(datetime), type = Domestic_International) %>%
  summarise(passengers = sum(Passenger_Count)) %>%
  ungroup()

lax_passengers

## ----tsibble------------------------------------------------------------------
# Convert to a tsibble
library(tsibble)
lax_passengers <- as_tsibble(lax_passengers, index = month, key = type)
lax_passengers

## ----plot---------------------------------------------------------------------
lax_passengers %>% 
  autoplot(passengers)

## ----spec---------------------------------------------------------------------
prophet(passengers ~ growth("linear") + season("year", type = "multiplicative"))

## ----model--------------------------------------------------------------------
fit <- lax_passengers %>% 
  model(
    mdl = prophet(passengers ~ growth("linear") + season("year", type = "multiplicative")),
  )
fit

## ----components---------------------------------------------------------------
components(fit)

## ----components-plot----------------------------------------------------------
components(fit) %>% 
  autoplot()

## ----components-seasonal------------------------------------------------------
library(ggplot2)
components(fit) %>% 
  ggplot(aes(
    # Plot the month of the time index (month) on the x-axis
    x = month(month, label = TRUE),
    # Plot the annual seasonal term (year) on the y-axis
    y = year, 
    # Colour by the passenger type
    colour = type,
    # Draw separate lines for each type and year
    group = interaction(type, year(month))
  )) +  
  geom_line()

## ----forecast-----------------------------------------------------------------
fc <- fit %>% 
  forecast(h = "3 years")
fc

## ----forecast-plot------------------------------------------------------------
fc %>% 
  autoplot(lax_passengers)

## ----train-accuracy-----------------------------------------------------------
accuracy(fit)

