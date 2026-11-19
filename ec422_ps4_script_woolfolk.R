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

### 2.3
### a.
liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

liquor_cv_fit <- liquor_cv |>
  model(snaive = SNAIVE(sales),
        ets    = ETS(log(sales)),
        arima  = ARIMA(log(sales)))

saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")

How many different forecast origins will be considered in this TS-CV?
  
### b.
liquor_cv_fc <- liquor_cv_fit |>
  forecast(h = 1)

liquor_cv_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

## What are the dates of the first and last forecasts? Report the accuracy results in a table. In your write-up, you can produce a nicely formatted table by piping the results into the kable() function from the knitr package: add |> knitr::kable(digits = 2) to the end of the accuracy() code above. Which model forecasts most accurately? Do the two metrics agree?

### 2.4

liquor_errs <- liquor_cv_fc |>
  as_tibble() |>
  left_join(as_tibble(liquor) |> select(Month, actual = sales),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e)

forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)

### Arima v ETS
dm_arima_ets <- forecast::dm.test(liquor_errs$arima, liquor_errs$ets, h = 1, power = 2)
dm_arima_ets
 
### Arima v Snaive
dm_arima_snaive <- forecast::dm.test(liquor_errs$arima, liquor_errs$snaive, h = 1, power = 2)
dm_arima_snaive
  