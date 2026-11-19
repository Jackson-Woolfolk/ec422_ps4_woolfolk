### Jackson Woolfolk
### problem set 4

install.packages("BVAR")

library(tidyverse)
library(fpp3)
library(BVAR)

### 2.1 a.
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

### 2.1 b.
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

## written answer

### 2.2 a.
liquor_fit <- liquor |>
  model(arima = ARIMA(log(sales)),
        ets   = ETS(log(sales)))

liquor_fit |> select(arima) |> report()
liquor_fit |> select(ets) |> report()

## Written answer

### 2.2 b.
liquor_fit |> select(arima) |> gg_tsresiduals()

liquor_fit |>
  select(arima) |>
  augment() |> 
  features(.innov, ljung_box, lag = 24, dof = 4)

### 2.3 a.
liquor_cv <- liquor |>
  stretch_tsibble(.init = 120, .step = 1) |>
  filter(.id < max(.id))

liquor_cv_fit <- liquor_cv |>
  model(snaive = SNAIVE(sales),
        ets    = ETS(log(sales)),
        arima  = ARIMA(log(sales)))

#saveRDS(liquor_cv_fit, "results/liquor_cv_fit.rds")
liquor_cv_fit <- readRDS("results/liquor_cv_fit.rds")
saveRDS(liquor_cv_fit, file = "results/liquor_cv_fit.rds")

## How many different forecast origins will be considered in this TS-CV?
  
### 2.3 b.
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

##Report both DM statistics and p-values. In two or three sentences: what do you conclude? If you were the liquor store chain, which model would you use?

## Part 3 

### 3 a. 
liquor_combo_fc <- liquor_cv_fit |>
  mutate(combo = (arima + ets) / 2) |>
  forecast(h = 1)

liquor_combo_fc |>
  accuracy(liquor, measures = list(RMSE = RMSE, MAE = MAE))

liquor_combo_fc |> knitr::kable(digits = 2)

##How does combo compare to each of the models that go into it?

### 3 b. 
err_corr <- cor(liquor_errs$arima, liquor_errs$ets)
err_corr
##Based on this correlation, and on the DM test in 2.4, explain in two or three sentences why the combination did or did not beat the best individual model

## Part 4

### 4.1 a.
qd_raw <- read_csv("data/fredqd_2026-09.csv", show_col_types = FALSE) |>
  slice(-(1:2))

### 4.1 b.
growth <- function(x) 100 * difference(log(x))

qd <- qd_raw |>
  mutate(date = as.Date(sasdate, "%m/%d/%Y")) |>
  transmute(date,
            gdp    = growth(GDPC1),     # real GDP
            cons   = growth(PCECC96),   # real consumption
            inv    = growth(GPDIC1),    # real investment
            emp    = growth(PAYEMS),    # payroll employment
            infl   = growth(GDPCTPI),   # GDP price index
            unrate = difference(UNRATE),     # unemployment rate
            ffr    = difference(FEDFUNDS),   # federal funds rate
            gs10   = difference(GS10)) |>    # 10-year Treasury yield
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

y <- qd |> select(-date) |> as.matrix()

nrow(y)
head(y)

### 4.1 c.
N <- nrow(y)
origins <- 120:(N - 1)
actual <- y[121:N, ]

### How many forecasts will we produce for each variable, and what are the dates of the first and last forecast?
  
### 4.2
var_ols_fc <- function(z, p) {
  e <- embed(z, p + 1)            # columns: y_t, then its lags 1 to p
  M <- ncol(z)                    # number of variables
  B <- coef(lm(e[, 1:M] ~ e[, -(1:M)]))
  x <- c(1, t(z[nrow(z):(nrow(z) - p + 1), ]))   # constant, latest p lags
  x %*% B
}

fc_var1 <- NULL
fc_var4 <- NULL
for (T in origins) {
  fc_var1 <- rbind(fc_var1, var_ols_fc(y[1:T, ], 1))
  fc_var4 <- rbind(fc_var4, var_ols_fc(y[1:T, ], 4))
}

rmse_by_var <- function(f) sqrt(colMeans((actual - f)^2))
rmse_by_var(fc_var1)
rmse_by_var(fc_var4)

## Which VAR forecasts better? For how many of the eight variables? Given that the VAR(4) contains the VAR(1) as a special case, why might the smaller model forecast better?

## 4.3 a.
bvar_cv <- function(lambda) {
  pr <- bv_priors(hyper = "lambda",
                  mn = bv_mn(lambda = bv_lambda(mode = lambda, sd = 0.4,
                                                min = 0.999 * lambda,
                                                max = 1.001 * lambda),
                             b = 0))
  fc <- NULL
  for (T in origins) {
    fit_T <- bvar(y[1:T, ], lags = 4, priors = pr,
                  n_draw = 2000, n_burn = 1000, verbose = FALSE)
    x <- c(1, t(y[T:(T - 3), ]))         # constant and 4 lags of each variable
    fc <- rbind(fc, x %*% coef(fit_T))   # forecast of y_{T+1}
  }
  fc
}

set.seed(422)
fc_bvar_03 <- bvar_cv(0.3)
saveRDS(fc_bvar_03, "results/fc_bvar_03.rds")

## 4.3 b.
set.seed(422)
fc_bvar_005 <- bvar_cv(0.05)
saveRDS(fc_bvar_005, "results/fc_bvar_005.rds")

var_results <- tibble(
  Variable = colnames(y),
  `VAR(1)` = rmse_by_var(fc_var1),
  `VAR(4)` = rmse_by_var(fc_var4),
  `BVAR(4), lambda = 0.3` = rmse_by_var(fc_bvar_03),
  `BVAR(4), lambda = 0.05` = rmse_by_var(fc_bvar_005)
)

saveRDS(var_results, "results/var_results.rds")

var_results |> knitr::kable(digits = 3)
##In your write-up, your .qmd can load this table with readRDS() 

### 4.3 c.
e_var4 <- actual[, "gdp"] - fc_var4[, 1]
e_bvar <- actual[, "gdp"] - fc_bvar_03[, 1]
forecast::dm.test(e_var4, e_bvar, h = 1, power = 2)

col_idx <- which(colnames(y) == "infl")
e_var4_infl <- actual[, col_idx] - fc_var4[, col_idx]
e_bvar_infl <- actual[, col_idx] - fc_bvar_03[, col_idx]
forecast::dm.test(e_var4_infl, e_bvar_infl, h = 1, power = 2)

## Part 5

### 5.1 a.
md_raw <- read_csv("data/fredmd_2026-09.csv", show_col_types = FALSE)
tcode <- unlist(md_raw[1, -1])
md <- md_raw[-1, ] |> mutate(date = as.Date(sasdate, "%m/%d/%Y"))

transform_series <- function(x, code) {
  d <- function(v) c(NA, diff(v))
  switch(as.character(code),
         "1" = x,                      # level
         "2" = d(x),                   # first difference
         "3" = d(d(x)),                # second difference
         "4" = log(x),                 # log
         "5" = 100 * d(log(x)),        # growth rate
         "6" = 100 * d(d(log(x))),     # change in the growth rate
         "7" = d(x / dplyr::lag(x) - 1))
}

### 5.1 b.
fm <- map2_dfc(md[names(tcode)], tcode, transform_series) |>
  mutate(date = md$date, .before = 1) |>
  filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))

fm <- fm[, colSums(is.na(fm)) == 0]

dim(fm)

### 5.1 c.
tcode[c("INDPRO", "PAYEMS", "CPIAUCSL", "FEDFUNDS")]

## In a sentence for each: how is each of these four series transformed? Why do you think the transformation chosen for FEDFUNDS differs from the one for INDPRO?

### 5.2 a.
Z <- fm |> select(-date) |> as.matrix()

pc <- prcomp(scale(Z))

summary(pc)$importance[, 1:5]

## What share of the variation in all 121 series do the first three factors explain? This should match the result we saw in class.

### 5.2 b.
F1 <- pc$x[, 1] * sign(cor(pc$x[, 1], fm$INDPRO))

tibble(date = fm$date, F1 = F1) |>
  ggplot(aes(date, F1)) +
  geom_line() +
  labs(title = "First Factor from FRED-MD", x = NULL, y = NULL) +
  theme_minimal()

### 5.2 c.
sort(abs(cor(pc$x[, 1], Z))[1, ], decreasing = TRUE)[1:8]
sort(abs(cor(pc$x[, 2], Z))[1, ], decreasing = TRUE)[1:8]

## The names of the series can be looked up in the FRED-MD documentation, FRED-MD_updated_appendix.pdf, which is posted on Canvas. Based on these series, and on your plot, how would you describe what each factor captures?

### 5.3
ip <- fm$INDPRO
n <- nrow(Z)
init <- which(fm$date == as.Date("1989-12-01"))

e_ar <- NULL
e_fa <- NULL
for (T in init:(n - 1)) {
  F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:3]         # factors using data through T
  d <- data.frame(y_next = ip[2:T], y = ip[1:(T - 1)], F_T[1:(T - 1), ])
  new <- data.frame(y = ip[T], t(F_T[T, ]))
  e_ar <- c(e_ar, ip[T + 1] - predict(lm(y_next ~ y, d), new))
  e_fa <- c(e_fa, ip[T + 1] - predict(lm(y_next ~ ., d), new))
}

c(RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)))
## Why would it be cheating to estimate the factors once, using the full sample, and use those in the horse race?

### 5.4
direct_race <- function(series, h = 12, r = 3) {
  lv <- md |>
    mutate(log_x = log(.data[[series]]),
           g = 100 * (log_x - lag(log_x))) |>
    filter(date >= as.Date("1960-03-01"), date <= as.Date("2019-12-01"))
  yh <- 100 * (lead(lv$log_x, h) - lv$log_x)      # growth over the next h months
  g  <- lv$g                                      # growth over the latest month
  
  e_ar <- NULL
  e_fa <- NULL
  for (T in init:(n - h)) {
    F_T <- prcomp(scale(Z[1:T, ]))$x[, 1:r, drop = FALSE]
    i <- 1:(T - h)
    d <- data.frame(yh = yh[i], g = g[i], F_T[i, , drop = FALSE])
    new <- data.frame(g = g[T], F_T[T, , drop = FALSE])
    e_ar <- c(e_ar, yh[T] - predict(lm(yh ~ g, d), new))
    e_fa <- c(e_fa, yh[T] - predict(lm(yh ~ ., d), new))
  }
  tibble(series, forecasts = length(e_ar),
         RMSE_AR = sqrt(mean(e_ar^2)), RMSE_FA = sqrt(mean(e_fa^2)),
         MAE_AR = mean(abs(e_ar)), MAE_FA = mean(abs(e_fa)),
         DM_pvalue = forecast::dm.test(e_ar, e_fa, h = h, power = 2,
                                       varestimator = "bartlett")$p.value)
}

direct_race("CPIAUCSL")

## Do the factors improve the forecast of 12-month CPI inflation? By what percentage do they reduce the RMSE? Is the difference statistically significant?

