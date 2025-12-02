

# ------------------------------------- packages


library(tidyverse)
library(readxl)
library(writexl)
library(lubridate) # prace s datumy
library(zoo) # ?
library(shiny) # aplikace
library(rvest) # cteni html


# ------------------------------------- paths


path <- "C:/Users/krizova/Documents/R/02 cenoveKalkukacky/_vyvoj/"


# ------------------------------------- initial set


low_spot <- 24.30
aktual_spot <- low_spot + 0.4
surcharge <- 0.05 
bsd <- 1.2 

shell.exec(paste0(path, "vstupy/input_fwdKrivka.xlsx")) # open fwd and get most recent values


# ------------------------------------- load inputs


# profil

profil <- read_excel(paste0(path, "vstupy/input_profil.xlsx"), sheet = "List2", col_names = T) %>% 
  mutate(mesic = lubridate::month(datum),
         rok = lubridate::year(datum)) %>% 
  select(datum, rok, mesic, profilMWh)


# forward curve 

fwd <- read_excel(paste0(path, "vstupy/input_fwdKrivka.xlsx"), sheet = "List1") %>% 
  rename("PFC" = NCG, "FX" = 'FX rate')
fwd_akt <- lubridate::date(unique(fwd$akt))
fwd_akt == Sys.Date() # je krivka dnesni? Chceme TRUE.
rm(fwd_akt)

# OTC prices TRAYPORT -- abandoned

# trayport <- "X:/Nakup _ NEW/02_Trayport_Ceny/Data_Trayport_Live.xlsx"
# cal26 <- read_excel(trayport, sheet = "Output", range = "P22", col_names = FALSE)[[1]]
# cal27 <- read_excel(trayport, sheet = "Output", range = "P23", col_names = FALSE)[[1]]
# cal28 <- read_excel(trayport, sheet = "Output", range = "P24", col_names = FALSE)[[1]]
# cal29 <- read_excel(trayport, sheet = "Output", range = "P25", col_names = FALSE)[[1]]
# 
# otc_timestamp <- Sys.time()
# stmp <- paste0("h", hour(otc_timestamp), "m", minute(otc_timestamp))
# temp <- c(as.character(otc_timestamp), cal26, cal27, cal28, cal29) 
# assign(stmp, temp)

# OTC HTML

html <- read_html("X:/OTC/HTML/CZ-VTP.html")
tabulka <- html_table(html, fill = TRUE)[[1]]

otc <- tabulka %>% 
  select("produkt" = X1, "cena" = X2) %>% 
  filter(produkt %in% c("CZ VTP 2026", "CZ VTP 2027", "CZ VTP 2028")) %>% 
  mutate(upd_cena = as.numeric(str_replace(cena, ",", ".")))

cal26 <- otc$upd_cena[otc$produkt=="CZ VTP 2026"]
cal27 <- otc$upd_cena[otc$produkt=="CZ VTP 2027"]
cal28 <- otc$upd_cena[otc$produkt=="CZ VTP 2028"]
tmstmp <- str_remove(tabulka[[1,1]], "\r\n ")
tmstmp

# ------------------------------------- Data_Vstup


join <- profil %>% 
  left_join(fwd, by = c("datum" = "mesic")) %>% 
  group_by(rok) %>% 
  mutate(
    
    # cena komodity
    
         PFCratio = PFC/mean(PFC),
         PFCprepoc = round(case_when(rok == 2026 ~ PFCratio*cal26,
                                     rok == 2027 ~ PFCratio*cal27,
                                     rok == 2028 ~ PFCratio*cal28,
                                     # rok == 2029 ~ PFCratio*cal29, 
                                     TRUE ~ NA), 3),
    # kurz EUR
    
         swapPoint = (FX-low_spot)*1000,
         FXrecalc = aktual_spot+swapPoint/1000+surcharge,
    
         cenaEUR = round(profilMWh*PFCprepoc, 0),
         vazenaCena = profilMWh*FXrecalc) %>% 
  
  ungroup()


# ------------------------------------- Kalkulace


data_vstup <- join %>%
  select(rok, mesic, profilMWh, PFCprepoc, cenaEUR, FXrecalc, vazenaCena)  # final df to match table on sheet Kalkulace


# ------------------------------------- calculate fixed price


# vypocty pod tabulkou

suma_profil <- sum(data_vstup$profilMWh)
suma_cenaEUR <- sum(data_vstup$cenaEUR)
suma_vazenaCena <- sum(data_vstup$vazenaCena)
mean_PFC <- mean(data_vstup$PFCprepoc)
nakup <- suma_cenaEUR/suma_profil
prirazka_nakup <- 0.000 #  ???
kurz <- round(suma_vazenaCena/suma_profil, 2)
prodej_eur <- nakup+prirazka_nakup
prodej_czk <- prodej_eur*kurz

# vypocty nad tabulkou

naklad_profil <- round(nakup-mean_PFC, 3)
fin_cenaEUR <- ceiling(prodej_eur/0.025) * 0.025 # zaokrouhleni na nejblizsi nejvyssi hranici 0,025 
fin_cenaCZK <- ceiling((fin_cenaEUR*kurz)/0.05) * 0.05 # zaokrouhleni na nejblizsi nejvyssi hranici 0,05 

# dodaci obdobi

od <- min(profil$datum)
do <- max(profil$datum) + months(1)


# ------------------------------------- final price


fix_cena <- data.frame(
  Od = od,
  Do = do,
  `Cena [EUR]` = fin_cenaEUR,
  `Cena [CZK]` = fin_cenaCZK,
  `Naklad na profil [EUR]` = naklad_profil,
  `BSD [EUR]` = bsd,
  check.names = FALSE # aby nebyly v nazvu sloupcu misto mezer tecky
)

print(fix_cena)

