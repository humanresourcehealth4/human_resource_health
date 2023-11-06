
library(tidyverse)

# loading real data of births in Goias

real_data <- read_csv("~/GitHub/human_resource_health/02_model_performance_graphs/real_data.csv")


real_data <- 
  real_data |> 
  mutate(date = paste(ano, mes, sep = "-")) |> 
  mutate(complete_date = as.Date(paste(date, "-01", sep=""))) |> 
  rename(ibge = cod_regsaud, 
         birth_s = quantidade) |> 
  select(ibge, date, birth_s, complete_date) |> 
  mutate(model = "real")


# Comparing all models - state level

# reading models

plot_model_evaluation <- read_excel("~/GitHub/human_resource_health/02_model_performance_graphs/plot_model_evaluation_2025.xlsx")

model_state <- 
  plot_model_evaluation |>
  group_by(complete_date, model) |> 
  summarise(birth = sum(birth_s)) 

real_data_state <- 
  real_data |> 
  group_by(complete_date, model) |>
  summarise(birth = sum(birth_s)) 

df_model_state <- 
  rbind(model_state, 
      real_data_state) |> 
  filter(complete_date < "2022-01-01") |> 
  mutate(highlight  = if_else(model == "real" | model == "mlp",
                              1.5, 1)) |> 
  mutate(line_type = if_else(highlight == 1.5, "solid",
                             "dashed"))

df_model_state |> 
  ggplot(aes(x = complete_date, y = birth, 
             col = model, size = highlight,
             linetype = line_type)) + 
  geom_line() +  
  scale_size_identity() + 
  theme_minimal() + xlab("Date") + 
  ylab("# births") + 
  guides(linetype = FALSE) +
  theme(text = element_text(size = 22))


# Comparing models - health region level

plot_model_evaluation <- 
  plot_model_evaluation |> 
  filter(complete_date < "2022-01-01") 
  

df <- rbind(real_data,plot_model_evaluation)

# Creating function

myfunction <- function(health_region, name_health_region, 
                       model) {
  
  df_grafico <- df|>
    filter(ibge == {{health_region}})|>
    mutate(highlight  = if_else(model == "real" | model == {{model}},
                                1.5, 1)) |> 
    mutate(line_type = if_else(highlight == 1.5, "solid",
                               "dashed"))
  
  df_grafico |> 
    ggplot(aes(x = complete_date, y = birth_s, 
               col = model, size = highlight,
               linetype = line_type)) + 
    geom_line() +  
    scale_size_identity() + 
    ggtitle({{name_health_region}}) +
    theme_minimal() + xlab("Date") + 
    ylab("# births") + 
    guides(linetype = FALSE) +
    theme(text = element_text(size = 22))
  
  
}


myfunction("52001","Central","mlp")
myfunction("52002","Centro Sul","mlp")
myfunction("52003", "Entorno Norte","ets")
myfunction("52004", "Entorno Sul","ets")
myfunction("52005", "Estrada de Ferro","mlp")
myfunction("52006", "Nordeste I","ets")
myfunction("52007", "Nordeste II","mlp")
myfunction("52008", "Norte","ets")
myfunction("52009", "Oeste I","ets")
myfunction("52010", "Oeste II","mlp")
myfunction("52011", "Pirineus","mlp")
myfunction("52012", "Rio Vermelho","mlp")
myfunction("52013", "São Patricio I","arima")
myfunction("52014", "Serra da Mesa","arima")
myfunction("52015", "Sudoeste I","ets")
myfunction("52016", "Sudoeste II","ets")
myfunction("52017", "Sul","ets")
myfunction("52018", "São Patricio II","ets")


 