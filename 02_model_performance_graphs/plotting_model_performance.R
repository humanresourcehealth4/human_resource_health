
library(tidyverse)

# loading real data of births in Goias

real_data <- read_csv("02_model_performance_graphs/real_data.csv")


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

plot_model_evaluation <- read_excel("02_model_performance_graphs/plot_model_evaluation.xlsx")

model_state <- 
  plot_model_evaluation |>
  filter(complete_date < "2021-01-01") |> 
  group_by(complete_date, model) |> 
  summarise(birth = sum(birth_s)) 

real_data_state <- 
  real_data |> 
  group_by(complete_date, model) |>
  summarise(birth = sum(birth_s)) 

df_model_state <- 
  rbind(model_state, 
      real_data_state) |> 
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


 