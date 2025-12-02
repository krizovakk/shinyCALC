# ------------------------------------------------------------------------------ SETUP


library(tidyverse)
library(readxl)
library(lubridate)

analyza_data <- function(profil, delOd, delDo, obch, zak, path = "data/") {
  
  tms_now <- Sys.Date()
  print(tms_now)
  
  
  # ---------------------------------------------------------------------------- INPUT :: forward - OK
  
  
  a <- read_excel(file.path("data", "input_fwd.xlsx"), 
                  sheet = "Rentry")
  a$mesic <- as.Date(a$mesic, origin = "1899-12-30")
  str(a)
  fwd <- a %>% 
    rename("PFC" = NCG, "FX" = 'FX rate') %>% 
    filter(mesic > tms_now) # hodnoty fwd krivky od nasledujiciho mesice
  # fwd_akt <- lubridate::date(unique(fwd$akt))
  # 
  # # if (any(is.na(fwd$PFC))) stop("Nemáš komplet PFC krivku")
  # # if (any(is.na(fwd$FX))) stop("Nemáš komplet FX krivku")
  
  
  # ---------------------------------------------------------------------------- INPUT :: OTC - OK
  
  
  # csv <- read.csv(file.path(path, "CZ-VTP.csv"), header = TRUE, sep = ",")
  b <- read.csv("X:/OTC/CSV/CZ-VTP.csv", header = TRUE, sep = ",")
  otc <- b %>%
    select("season" = 1, "price" = 2) %>%
    filter(str_detect(season, "^CZ")) %>%
    mutate(
      price = as.numeric(str_replace(price, ",", ".")),
      year = case_when(
        str_detect(season, "2025|25") ~ 2025,
        str_detect(season, "2026|26") ~ 2026,
        str_detect(season, "2027|27") ~ 2027,
        str_detect(season, "2028|28") ~ 2028,
        TRUE ~ NA
      ),
      quater = case_when(
        str_detect(season, "Q1") ~ "Q1",
        str_detect(season, "Q2") ~ "Q2",
        str_detect(season, "Q3") ~ "Q3",
        str_detect(season, "Q4") ~ "Q4",
        TRUE ~ NA
      ),
      month = case_when(
        str_detect(season, "Jan-") ~ 1,
        str_detect(season, "Feb-") ~ 2,
        str_detect(season, "Mar-") ~ 3,
        str_detect(season, "Apr-") ~ 4,
        str_detect(season, "May-") ~ 5,
        str_detect(season, "Jun-") ~ 6,
        str_detect(season, "Jul-") ~ 7,
        str_detect(season, "Aug-") ~ 8,
        str_detect(season, "Sep-") ~ 9,
        str_detect(season, "Oct-") ~ 10,
        str_detect(season, "Nov-") ~ 11,
        str_detect(season, "Dec-") ~ 12,
        TRUE ~ NA
      ),
      cal = as.character(ifelse(str_detect(season, "^CZ VTP \\d{4}$"), paste0("Cal", str_remove(year, "^..")), NA))
    )

  
  # ---------------------------------------------------------------------------- CREATE :: frame - OK
  
  
  frameOd <- as.Date("2025-01-01")
  frameDo <- as.Date("2028-12-31")
  framePer <- seq(from = frameOd, to = frameDo, by = "month")
  
  delPer <- as.POSIXct(seq(from = delOd, to = delDo, by = "month") %>% head(-1)) # head = maze posledni element (1.1.2027)
  print(delPer)
  
  frame <- data.frame(framePer) %>%
    mutate(
      year = year(framePer),
      month = month(framePer),
      quater = case_when(
        month <= 3 ~ "Q1",
        month <= 6 ~ "Q2",
        month <= 9 ~ "Q3",
        month <= 12 ~ "Q4"
      ),
      now = ifelse(year == year(tms_now) & month == month(tms_now), 1, 0), # jaky mesic je ted
      dodavka = ifelse(framePer %in% seq(from = delOd, to = delDo, by = "month"), 1, 0)
    ) %>%
    left_join(profil, by = c("framePer" = "datum")) %>%
    left_join(fwd, by = c("framePer" = "mesic")) %>% 
    mutate(dodavka = ifelse(framePer %in% delPer, 1, 0)) %>%  
    select(framePer, year, quater, month, now, dodavka, profilMWh, PFC, FX)
  
  
  # ---------------------------------------------------------------------------- CREATE :: data_vstup - OK
  
  
  low_spot <- 24.30
  aktual_spot <- low_spot + 0.4
  surcharge <- 0.05 
  bsd <- 1.2
  
  join <- frame %>%

    # cena komodity

    group_by(year) %>%
    mutate(yRatio = PFC/mean(PFC)) %>% # kdyz neni cely rok, hodi pres prumer NA
    ungroup() %>% group_by(year, quater) %>%
    mutate(celyQ = if_else(all(dodavka == 1)&is.na(yRatio), "ANO", "NE"),
           # avg = mean(PFC),
           qRatio = ifelse(celyQ == "ANO", PFC/mean(PFC), NA),
           PFCratio = coalesce(yRatio, qRatio)) %>% ungroup() %>%
    select(-yRatio, -qRatio) %>%
    left_join(otc %>%
                select(year, month, "monPrice" = price), by = c("year", "month")) %>%
    left_join(otc %>%
                select(year, quater, "qPrice" = price), by = c("year", "quater")) %>%
    left_join(otc %>%
                filter(!is.na(cal)) %>%
                select(year, "calPrice" = price), by = c("year")) %>%
    mutate(otcPrice = case_when(celyQ == "NE" & is.na(PFCratio)~ monPrice,
                                celyQ == "ANO" ~ qPrice,
                                TRUE ~ calPrice),
           PFCprepoc = ifelse(!is.na(PFCratio), otcPrice*PFCratio, otcPrice),

           # kurz EUR

           swapPoint = (FX-low_spot)*1000,
           FXrecalc0 = aktual_spot+swapPoint/1000, # +surcharge (ale v excelu je 0)
           FXrecalc = FXrecalc0+surcharge,

           cenaEUR = profilMWh*PFCprepoc,
           vazenaCena = profilMWh*FXrecalc)

  data_vstup <- join %>%
    select(year, month, dodavka, profilMWh, PFCprepoc, cenaEUR, FXrecalc, vazenaCena) %>%
    filter(dodavka == 1) # final df to match table on sheet Kalkulace


  # ---------------------------------------------------------------------------- CALCULATE :: fix_cena - OK
  
  
  # vypocty pod tabulkou

  suma_profil <- round(sum(data_vstup$profilMWh, na.rm = TRUE), 0)
  suma_cenaEUR <- sum(data_vstup$cenaEUR, na.rm = TRUE)
  suma_vazenaCena <- sum(data_vstup$vazenaCena, na.rm = TRUE)
  mean_PFC <- mean(data_vstup$PFCprepoc, na.rm = TRUE)
  nakup <- suma_cenaEUR/suma_profil
  prirazka_nakup <- 0.000 #  ???
  kurz <- suma_vazenaCena/suma_profil
  prodej_eur <- nakup+prirazka_nakup
  prodej_czk <- prodej_eur*kurz

  # # vypocty nad tabulkou

  naklad_profil <- round(nakup-mean_PFC, 2)
  fin_cenaEUR <- ceiling(prodej_eur/0.025) * 0.025 # zaokrouhleni na nejblizsi nejvyssi hranici 0,025
  fin_cenaCZK <- ceiling((fin_cenaEUR*kurz)/0.05) * 0.05 # zaokrouhleni na nejblizsi nejvyssi hranici 0,05

  
  # ---------------------------------------------------------------------------- CREATE :: marze - IP
  
  
  # marzeNazev <- c("minimalni", "doporucena")
  # marzeHodnota <- c(1.2, 6)
  # 
  # marze <- data.frame(
  #   varianta = marzeNazev, 
  #   hodnota = marzeHodnota)
  marzeMin <- 1.2
  marzeDop <- 6
  
  
  # ---------------------------------------------------------------------------- RETURN :: fix_cena - OK
  
  # 
  # fix_cena <- data.frame(
  #   Od = delOd,
  #   Do = delDo,
  #   `Objem [MWh]` = suma_profil,
  #   `Předávací cena EUR` = round(fin_cenaEUR, 2),
  #   `Předávací cena CZK` = round(fin_cenaCZK, 2),
  #   `Náklad na BSD [€]` = bsd,
  #   check.names = FALSE # aby nebyly v nazvu sloupcu misto mezer tecky
  # )
  
  fix_cena <- data.frame(
    Parametr = c(
      "Obchodník",
      "Zákazník",
      "Období dodávky",
      "Objem [MWh]",
      "Cena [€]",
      "Náklad na profil [€]",
      "Náklad na BSD [€]",
      "Marže [€]",
      "Cena pro zákazníka"
    ),
    Hodnota = c(
      obch,
      zak,
      paste0(delOd, " až ", delDo),
      suma_profil,
      fin_cenaEUR,
      naklad_profil,
      bsd,
      paste("Minimální:", marzeMin, " /  Doporučená:", marzeDop),
      paste("Minimální:", fin_cenaEUR+marzeMin, " /  Doporučená:", fin_cenaEUR+marzeDop)
    ),
    check.names = FALSE
  )
  
  colnames(fix_cena) <- NULL
  

  print(fix_cena)
  return(fix_cena)
  # return(tabulka)

}

