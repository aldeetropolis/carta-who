library(tidyverse)
library(readxl)
library(writexl)

df <- read_xlsx("WHO CARTA SEAR B dataset.xlsx", sheet = "Tnp Px Lab") |> 
  mutate(criteria =
    if_else(score < 5, "<5%", 
      if_else(between(score, 5, 9), "5-<10%", 
        if_else(between(score, 10, 19), "10-<20%",
          if_else(between(score, 20, 29), "20-<30%", 
            if_else(score >= 30, ">=30%", NA))))))

write_xlsx(df, "WHO CARTA SEAR B.xlxs")