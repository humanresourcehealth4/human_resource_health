
library(tidyverse)
library(dplyr)
library(readxl)
library(readr)
library(rio)
library(stringr)

# setwd("C:/Users/Lapei_Cigets/Desktop/CAGED")

#----------------------- CAGED 2022 DEZEMBRO


caged_202207 <-  read.csv2("CAGEDMOV202207.txt", header = T, dec = ".")    
caged_202208 <-  read.csv2("CAGEDMOV202208.txt", header = T, dec = ".")    
caged_202209 <-  read.csv2("CAGEDMOV202209.txt", header = T, dec = ".")    
caged_202210 <-  read.csv2("CAGEDMOV202210.txt", header = T, dec = ".")    
caged_202211 <-  read.csv2("CAGEDMOV202211.txt", header = T, dec = ".")    
caged_202212 <-  read.csv2("CAGEDMOV202212.txt", header = T, dec = ".")    

caged_202301 <-  read.csv2("CAGEDMOV202301.txt", header = T, dec = ".")    
caged_202302 <-  read.csv2("CAGEDMOV202302.txt", header = T, dec = ".")    
caged_202303 <-  read.csv2("CAGEDMOV202303.txt", header = T, dec = ".")    
caged_202304 <-  read.csv2("CAGEDMOV202304.txt", header = T, dec = ".")    
caged_202305 <-  read.csv2("CAGEDMOV202305.txt", header = T, dec = ".")    
caged_202306 <-  read.csv2("CAGEDMOV202306.txt", header = T, dec = ".")    

hierarquia_municipios <- read_excel("hierarquia_municipios.xlsx")



dados <- rbind(caged_202207,
               caged_202208,
               caged_202209,
               caged_202210,
               caged_202211,
               caged_202212,
               caged_202301,
               caged_202302,
               caged_202303,
               caged_202304,
               caged_202305,
               caged_202306)|> 
         filter(stringr::str_detect(cbo2002ocupação,
                                    '^225'))

dados$valor_salario <- gsub(',','.',dados$valorsaláriofixo)
dados$horas <- gsub(',','.',dados$horascontratuais)


dados_t <- dados |> 
            select(uf, município, cbo2002ocupação, valor_salario, 
                   horas) |> 
            mutate(horas = as.integer(horas)) |> 
            mutate(salario = as.numeric(valor_salario)) |>
            filter(horas > 35 & horas < 45) |> 
            filter(salario > 1320)


dados_t <- dados_t |> 
            left_join(hierarquia_municipios,
                      by = c("município"="cod_municipio")) |> 
            filter(uf_sigla == "GO")

salario_medio <- dados_t |> 
                    group_by(uf_sigla) |> 
                    summarise(salario_medio = mean(salario, na.rm = TRUE)) 


# writexl::write_xlsx(salario_medio, "salario_medio_med.xlsx")
