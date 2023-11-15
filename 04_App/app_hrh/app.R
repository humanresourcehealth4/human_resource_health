## APP backup

library(tidyverse)
library(ggplot2)
library(shiny)
library(bslib)
library(readxl)
library(lubridate)
library(renv)

renv::init()

# Reading Data and preparing it ----------------------------------

supply <- vroom::vroom("data/supply.csv")

phc_services <- read_excel("data/services_2025.xlsx") |> 
                  rename(health_region = regiao, date = data, 
                         sus_qtt = qtd, procedure = procedimento, 
                         month_performed = mês_procedimento_realizado, 
                         sigtap = codigo_sigtap, parameter = parametro, 
                         month = mes, type = tipo_procedimento, 
                         target_public = Público, 
                         level_service = nivel_atencao)

procedures_professional <- read_excel("data/calendario-procedimentos.xlsx", 
                                      sheet = "procedimentos_profissionais") |> 
  select(codigo_sigtap, categoria, CBO) |> 
  mutate(codigo_sigtap = as.numeric(codigo_sigtap)) 

qtt_professional <- 
  procedures_professional |> 
  group_by(codigo_sigtap) |> 
  count() |> 
  mutate(n = case_when(codigo_sigtap == '301010080' ~ n/10,
                       codigo_sigtap == '101010010' ~ n/2,
                       TRUE ~ n))

sisab <- read_excel("data/producao_SISAB.xls") |> 
  select(Cod_Regiao_Saude, Porcentagem)


# Preparing APP -----------------------------------------------------------


# Define UI for application that draws a histogram
ui <- 
  fluidPage(
    theme = bs_theme(bootswatch = "minty"),
    # Application title
    titlePanel("Health workforce planning - sensitivy analysis"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        sliderInput("private",
                    "Percentage of SUS dependent population which consume private services",
                    min = 0,
                    max = 0.50,
                    value = 0.10),  
        sliderInput("time1",
                    "Appointment time (in hours):",
                    min = 0.30,
                    max = 0.75,
                    value = 0.50),
        sliderInput("time2",
                    "Educative action (in hours):",
                    min = 0.50,
                    max = 1.50,
                    value = 1.00),
        sliderInput("awt",
                    "Available working time",
                    min = 1350,
                    max = 1600,
                    value = 1512),
        sliderInput("perc_nurse",
                    "Percentage of time in direct activities (Nurse):",
                    min = 0.30,
                    max = 1.00,
                    value = 0.60),
        sliderInput("perc_physician",
                    "Percentage of time in direct activities (Physician):",
                    min = 0.30,
                    max = 1.00,
                    value = 0.50)
        ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot")
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- 
    
    renderPlot({
      
      
      # In the following code:
      # 1) filter for only procedures performed by nurses and physicians 
      # 2) calculate the number of procedures exclusive by cadre according to the last code
      
      
      professional_services <- 
        phc_services |> 
        mutate(total = sus_qtt - (sus_qtt * input$private)) |> 
        mutate(procedure_qtt = total * parameter) |> 
        left_join(qtt_professional, 
                  by = c("sigtap" =
                           "codigo_sigtap")) |> 
        mutate(qtt_cadre = procedure_qtt/n) |> 
        left_join(procedures_professional, by = c( "sigtap"="codigo_sigtap")) |> 
        filter(categoria == "Enfermeiro" | 
                 categoria == "Médico") |> 
        rename(cadre = categoria)
      
      
      demand_hours <- professional_services |> 
        mutate(month_performed = as.Date(month_performed)) |> 
        mutate(year = year(month_performed)) |> 
        filter(year == 2024) |> 
        mutate(time_needed = 
                 if_else(type == "Consultas ou Visitas",
                         qtt_cadre * input$time1,
                         qtt_cadre * input$time2))
      
      prof_demand <- demand_hours |> 
        select(ibge, year, month_performed, CBO, 
               cadre, time_needed) |> 
        mutate(fte40 = time_needed/(input$awt/12)) |> 
        group_by(ibge, month_performed, year, CBO, cadre) |> 
        summarise(total_time = sum(time_needed),
                  fte40_demand = sum(fte40),
                  fte40_demand = round(fte40_demand, 2))
      
      supply_GO <- 
        supply |> 
        mutate(year_month = ym(COMPETEN)) |> 
        left_join(sisab, by = c("cod_regsaud"="Cod_Regiao_Saude")) |> 
        mutate(fte40_supply = CH/(input$awt/12)) |> 
        mutate(direct_supply = if_else(CATEGORIA == "ENFERMEIRO",
                                       fte40_supply * input$perc_nurse,
                                       fte40_supply * input$perc_physician)) |> 
        mutate(net_supply = direct_supply * Porcentagem) |>
        mutate(year_month = year_month + years(2)) |> 
        mutate(Porcentagem = round(Porcentagem, 2),
               fte40_supply = round(fte40_supply, 2),
               direct_supply = round(direct_supply, 2),
               net_supply = round(net_supply, 2)) |> 
        mutate(CATEGORIA = if_else(CATEGORIA == "ENFERMEIRO",
                                   "Enfermeiro","Médico")) |> 
        select(year_month, cod_regsaud, regiao_saude,
               CATEGORIA, CH, Porcentagem, fte40_supply, 
               direct_supply, net_supply)
      
      monthly_demand_supply <- 
        prof_demand |>  
        left_join(supply_GO, by = c("ibge"="cod_regsaud",
                                    "cadre"="CATEGORIA",                          
                                    "month_performed"="year_month")) |> 
        mutate(year = year(month_performed)) |> 
        mutate(result = net_supply - fte40_demand) |> 
        mutate(perc = (100 * net_supply)/fte40_demand)
      
      anual_demand_supply <-
        monthly_demand_supply |> 
        group_by(ibge, regiao_saude, cadre) |> 
        summarise(
          anual_demand = sum(fte40_demand),
          anual_net_supply = sum(net_supply)) |> 
        mutate(anual_demand = round(anual_demand, 2),
               anual_net_supply = round(anual_net_supply, 2)) |> 
        mutate(abs_result = anual_net_supply - anual_demand,
               abs_result = round(abs_result, 2)) |> 
        mutate(perc_result = (100 * anual_net_supply)/anual_demand,
               perc_result = round(perc_result, 2))
      
      anual_demand_supply |> 
        select(-abs_result, 
               -perc_result) |> 
        gather(key = "demand_supply", 
               value = "resultado", 
               4:5) |> 
        mutate(cadre = if_else(
          cadre == "Enfermeiro",
          "Nurse",
          "Physician"),
          demand_supply = if_else(
            demand_supply == "anual_demand","Need","Supply")) |> 
        mutate(resultado = round(resultado)) |> 
        ggplot(aes(x = fct_reorder(regiao_saude,
                                   resultado), 
                   y = resultado, 
                   fill = demand_supply)) + 
        geom_col(position = "dodge") + 
        geom_text(aes(label = resultado, 
                      angle = 45), 
                  position = position_dodge(width = .9)) +
        facet_wrap(~cadre, nrow = 2) + 
        theme_minimal() + xlab("Health Region") + 
        ylab("Demand vs Supply") + 
        theme(text = element_text(size = 20),
              axis.text.x = element_text(angle = 45, hjust=1),
              axis.title.x = element_blank(),
              legend.position = "top",
              legend.title = element_blank())      
    
      
    }, height = 500, width = 750)
}

# Run the application 
shinyApp(ui = ui, server = server)

