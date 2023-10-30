
library(tidyverse)
library(readxl)
library(lubridate)

arima <- read_csv("01_prediction/best_parameters_arima.csv")
ets <- read_csv("01_prediction/best_parameters_ets.csv")
prophet <- read_csv("01_prediction/best_parameters_pro.csv")
mlp <- read_csv("01_prediction/best_parameters_mlp.csv")
hierarquia <- read_excel("01_prediction/hierarquia_municipios.xlsx")

regioes <- 
  hierarquia |> 
  filter(uf_sigla == "GO") |> 
  select(cod_regsaud, regiao_saude) |> 
  distinct()

# joining dfs -------------------------------------------------------

mlp_results <- 
  mlp |> 
  select(reg, mae, mape) |> 
  rename(mape_mlp = mape,
         mae_mlp = mae) |> 
  filter(str_detect(reg, "^52"))


arima_results <- 
  arima |> 
  select(codibge...1, `mae_arima_2019-2020`,
         `mape_arima_2019-2020`) |> 
  rename(codibge = codibge...1,
         mape_arima = `mape_arima_2019-2020`,
         mae_arima = `mae_arima_2019-2020`)

prophet_results <- 
  prophet |> 
  select(codibge...1, `mae_prophet_2019-2020`,
         `mape_prophet_2019-2020`) |> 
  rename(codibge = codibge...1,
         mape_prophet = `mape_prophet_2019-2020`,
         mae_prophet = `mae_prophet_2019-2020`)

ets_results <- 
  ets |> 
  select(codibge...1, `mae_ets_2019-2020`,
         `mape_ets_2019-2020`) |> 
  rename(codibge = codibge...1,
         mape_ets = `mape_ets_2019-2020`,
         mae_ets = `mae_ets_2019-2020`)

results <- 
  mlp_results |> 
  inner_join(prophet_results, by = c("reg"="codibge")) |> 
  inner_join(arima_results, by = c("reg"="codibge")) |>
  inner_join(ets_results, by = c("reg"="codibge")) |> 
  left_join(regioes, by = c("reg"="cod_regsaud"))  |> 
  select(reg, regiao_saude, starts_with("mae"),
         starts_with("mape"))

# writexl::write_xlsx(results, "results_prediction.xlsx")


# exporting MLP -----------------------------------------------------------

# MLP presents the best results in most of the regions. 
# So we will use MLP 

mlp_select <- mlp[,c(2,22:93)]

mlp_select <- 
  mlp_select |> 
  gather(key = "date", value = "birth", 2:73) |> 
  mutate(birth_s = gsub("/.*","",birth)) |> 
  mutate(birth_s = as.numeric(birth_s),
         birth_s = round(birth_s)) |> 
  select(-birth) 

Sys.setlocale("LC_TIME", "C")

mlp_select2 <- 
  mlp_select |> 
  mutate(complete_date = as.Date(paste0("01-", date), 
                                 format = "%d-%b-%Y")) |> 
  mutate(model = "mlp") |> 
  rename(ibge = reg)

#writexl::write_xlsx(mlp_select2, "live_births_projectios_mlp.xlsx")


# Prepare data to create graphs which evaluate projection performance ----------

# ETS
ets_select <- ets[,c(1,9:81)]

ets_select <- 
  ets_select|>
  select(-codibge...33) |> 
  rename(ibge = codibge...1) |> 
  gather(key = "date", value = "birth", 2:73) |> 
  mutate(birth_s = gsub("/.*","",birth)) |> 
  mutate(birth_s = as.numeric(birth_s),
         birth_s = round(birth_s)) |> 
  select(-birth)

Sys.setlocale("LC_TIME", "C")

ets_select2 <- 
  ets_select |> 
  mutate(complete_date = as.Date(paste0("01-", date), 
                                 format = "%d-%b-%Y")) |> 
  mutate(model = "ets")

# Prophet

prophet_select <- prophet[,c(1,7:79)]

prophet_select <- 
  prophet_select|>
  select(-codibge...31) |> 
  rename(ibge = codibge...1) |> 
  gather(key = "date", value = "birth", 2:73) |> 
  mutate(birth_s = gsub("/.*","",birth)) |> 
  mutate(birth_s = as.numeric(birth_s),
         birth_s = round(birth_s)) |> 
  select(-birth)

Sys.setlocale("LC_TIME", "C")

prophet_select2 <- 
  prophet_select |> 
  mutate(complete_date = as.Date(paste0("01-", date), 
                                 format = "%d-%b-%Y")) |> 
  mutate(model = "prophet")

# ARIMA

arima_select <- arima[,c(1,10:82)]

arima_select <- 
  arima_select |>
  select(-codibge...34) |> 
  rename(ibge = codibge...1) |> 
  gather(key = "date", value = "birth", 2:73) |> 
  mutate(birth_s = gsub("/.*","",birth)) |> 
  mutate(birth_s = as.numeric(birth_s),
         birth_s = round(birth_s)) |> 
  select(-birth)

Sys.setlocale("LC_TIME", "C")

arima_select2 <- 
  arima_select |> 
  mutate(complete_date = as.Date(paste0("01-", date), 
                                 format = "%d-%b-%Y")) |> 
  mutate(model = "arima")


# binding all dfs to plot -------------------------------------------------


df_plot <- rbind(arima_select2,
                 prophet_select2,
                 mlp_select2,
                 ets_select2)

# writexl::write_xlsx(df_plot,"plot_model_evaluation.xlsx")
