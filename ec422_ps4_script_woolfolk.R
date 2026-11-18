### Jackson Woolfolk
### problem set 4

install.packages("BVAR")

library(tidyverse)
library(fpp3)
library(BVAR)

## 2.1

### a.
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

### b.
p_sales <- liquor |> autoplot(sales) + labs(
    title = "U.S. Beer, Wine, and Liquor Retail Sales",
    subtitle = "Not seasonally adjusted, millions of dollars",
    x = "Month",
    y = "Sales ($ Millions)"
  ) +
  theme_minimal()
print(p_sales)

p_season <- liquor |>
  gg_season(log(sales)) +
  labs(
    title = "Seasonal Plot of Log Retail Sales",
    subtitle = "Beer, Wine, and Liquor Stores",
    x = "Month",
    y = "Log(Sales)"
  ) +
  theme_minimal()
print(p_season)

written answer

### 2.2
### a.
liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

Written answer

### b.
liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |> 
  features(.innov, ljung_box, lag = 24, dof = 4)
