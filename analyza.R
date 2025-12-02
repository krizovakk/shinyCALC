

# ------------------------------------- initials
# kontroly dat


# print(html)

# print(delOd)
# print(delDo)
# print(head(fwd))
# print(head(otc))
print(head(profil))


# ------------------------------------------------------------------------------ paths and sys
# stanovi zakladni absolutni cestu
# vytvori casovou znamku s aktualnim datem
# dulezite pro stanoveni prepoctu PFC krivky
# -> pokud neni cely cal, je treba nahradit PFCprepoc hodnotami z html (Q, M)


path <- "C:/Users/krizova/Documents/R/02 cenoveKalkukacky/_vyvoj/"
Sys.setlocale("LC_TIME", "C")
tms_now <- Sys.time()


# ------------------------------------------------------------------------------ initial set
# * sourcing * sourcing * sourcing *
# stanovujeme my na zaklade aktualnich kurzu a strategii
# BSD pro rok 2026 stabilne na 1,2 €


low_spot <- 24.30
aktual_spot <- low_spot + 0.4
surcharge <- 0.05 
bsd <- 1.2


# ------------------------------------------------------------------------------ delivery period
# v app Date Range Selector https://shiny.posit.co/r/components/inputs/date-range-selector/
# nastaveni dodaci periody
# simulace Dodani z puvodniho excelu


# delOd <- input$date[1]
# delDo <- input$date[2]

delPer <- as.POSIXct(seq(from = delOd, to = delDo, by = "month") %>% head(-1)) # head = maze posledni element (1.1.2027)


# ------------------------------------------------------------------------------ INPUT :: forward
# nacteni aktualni forwardove krivky (=PFC)
# uprava: ber jen hodnoty, pro M+1 a dal
# KONTROLA kompletnosti krivky je stezejni



fwd <- readxl::read_excel("data/input_fwdKrivka.xlsx") %>% 
# fwd <- read_excel("C:/Users/krizova/Documents/R/02 cenoveKalkukacky/_vyvoj/vstupy/input_fwdKrivka.xlsx", sheet = "List1") 
  rename("PFC" = NCG, "FX" = 'FX rate') %>%
  filter(mesic > tms_now) # hodnoty fwd krivky od nasledujiciho mesice
fwd_akt <- lubridate::date(unique(fwd$akt))

# **************** kontrola dat ******************

# if (Sys.Date()-fwd_akt>1) {
#   stop("Nemas aktualni FWD data")
# }
# if (length(unique(lubridate::year(fwd$mesic))) != 4) { 
#   stop("Nemas kompletni FWD data")
# }
if (any(is.na(fwd$PFC))) {
  stop("Nemáš komplet PFC krivku")
}
if (any(is.na(fwd$FX))) {
  stop("Nemáš komplet FX krivku")
}
# **************** kontrola dat ******************


# ------------------------------------------------------------------------------ INPUT :: OTC
# nacte a sklidi aktualni html soubor s OTC cenami
# ulozeny na X
# uprava: definice kategorii cen pro dalsi pouziti
# casova znamka pro aktualnost souboru s cenami
# kontrola kompletnosti OTC cen


csv <- read.csv("X:/OTC/CSV/CZ-VTP.csv", header = TRUE, sep = ",")

# html <- read_html("X:/OTC/HTML/CZ-VTP.html") 
# # html <- read_html("data/CZ-VTP.html") %>% 
#   
# tab <- html_table(html, fill = TRUE)[[1]]
# temp <- (tab[[1]][[1]])
# tms_otc <- as.POSIXct(str_remove(temp, "\r\n "), format = "%d.%m.%Y %H:%M", tz = "Europe/Prague")


otc <- csv %>%
# otc <- html_table(html, fill = TRUE)[[1]] %>%
  select("season" = 1, "price" = 2)  %>%
  filter(str_detect(season, "^CZ")) %>%
  mutate(price = as.numeric(str_replace(price, ",", ".")),
         year = case_when(str_detect(season, "2025|25") ~ 2025,
                          str_detect(season, "2026|26") ~ 2026,
                          str_detect(season, "2027|27") ~ 2027,
                          str_detect(season, "2028|28") ~ 2028,
                          str_detect(season, "2029|29") ~ 2029,
                          str_detect(season, "2030|30") ~ 2030,
                          TRUE ~ NA),
         quater = case_when(str_detect(season, "Q1") ~ "Q1",
                            str_detect(season, "Q2") ~ "Q2",
                            str_detect(season, "Q3") ~ "Q3",
                            str_detect(season, "Q4") ~ "Q4",
                            TRUE ~ NA),
         month = case_when(str_detect(season, "Jan-") ~ 1,
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
                           TRUE ~ NA),
         cal = as.character(ifelse(str_detect(season, "^CZ VTP \\d{4}$"), paste0("Cal", str_remove(year, "^..")), NA)))
# filter(!is.na(season))


# **************** kontrola dat ******************

if (any(is.na(otc$price))) {
  print(paste0("Chybi hodnota pro: ", otc$season[is.na(otc$price)]))
  stop("Nekompletni OTC data")
}
# **************** kontrola dat ******************


# ------------------------------------------------------------------------------ INPUT :: profil
# vstupni profil pro naceneni
# musi obsahovat datum ve formatu datum a profil v MWh
# apliakce na zaklade vstupniho profilu vygeneruje graf


profil <- profil %>% 
  mutate(mesic = lubridate::month(datum),
         rok = lubridate::year(datum))

g <- ggplot(profil, aes(datum, profilMWh)) +
  geom_line(linewidth = 2,
            color = "gold")+
  labs(x = "měsíc dodávky",
       y = "profil spotřeby [MWh]",
       title = "Profil spotřeby klienta")+
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%Y-%m")+
  scale_y_continuous(breaks = seq(0, 700, by = 50))+
  theme_light()+
  theme(axis.text.x = element_text(angle=90))
g


# ------------------------------------------------------------------------------ frame
# vytvoreni zakladniho ramce pro vypocet
# rozsah datumu stabilni, vzdy pridat pro dalsi rok
# sloupec "now" oznacuje aktualni mesic
# sloupec "dodavka" oznacuje obdobi dodavky dane delPer
# dale pripojeny ostatni tabulky (profil, PFC a kurz €)
# takto je tabulka pripravena na vypocet


frameOd <- as.Date("2025-01-01")
frameDo <- as.Date("2028-12-31")

framePer <- as.POSIXct(seq(from = frameOd, to = frameDo, by = "month"))
frame <- as.data.frame(framePer) %>% 
  mutate(year = year(framePer),
         month = month(framePer),
         quater = case_when(month <= 3 ~ "Q1",
                            month <= 6 ~ "Q2",
                            month <= 9 ~ "Q3",
                            month <= 12 ~ "Q4"),
         now = ifelse(year == year(tms_now) & month == month(tms_now), 1, 0)) %>% # jaky mesic je ted
  left_join(profil, by = c("framePer" = "datum")) %>% 
  left_join(fwd, by = c("framePer" = "mesic")) %>% 
  # left_join(otc, by = c("year" = "year", "quater" = "quater", "month" = "month")) %>% 
  mutate(dodavka = ifelse(framePer %in% delPer, 1, 0)) %>%  
  select(framePer, year, quater, month, now, dodavka, profilMWh, PFC, FX)


# ------------------------------------------------------------------------------ Data_Vstup
# v y p o c t y
# yRatio = podil jednotliveho mesice na prumeru roku
# celyQ = indikator, zda mame kompletni data pro cely Q
# qRatio = podil jednotliveho mesice na prumeru Q
# PFCratio = finalni sloupec obsahujici podil jednotlivych mesicu ==> simuluje kompletni forwardovou krivku
# j o i n y
# 1. pripojeni OTC cen pro mesice
# 2. pripojeni OTC cen pro kvartaly
# 3. pripojeni OTC cen pro caly
# otcPrice = kde neni komplet Q bereme mesicni, kde je komplet Q bereme Q, pro komplet Y bereme cal
# v y p o c e t
# PFCprepoc = pokud mame PFCratio pocitame otcPrice*PFCratio, pokud ne - bereme M
# s w a p   p o i n t y
# prepocet forwardoveho kurzu €
# f i n a l
# data_vstup = ocisteny soubor, ktery obsahuje vsechny informace pro vypocty na listu Kalkulace (puv. excel)


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


# ------------------------------------------------------------------------------ calculate fixed price


# vypocty pod tabulkou

suma_profil <- sum(data_vstup$profilMWh, na.rm = TRUE)
suma_cenaEUR <- sum(data_vstup$cenaEUR, na.rm = TRUE)
suma_vazenaCena <- sum(data_vstup$vazenaCena, na.rm = TRUE)
mean_PFC <- mean(data_vstup$PFCprepoc, na.rm = TRUE)
nakup <- suma_cenaEUR/suma_profil
prirazka_nakup <- 0.000 #  ???
kurz <- suma_vazenaCena/suma_profil
prodej_eur <- nakup+prirazka_nakup
prodej_czk <- prodej_eur*kurz

# vypocty nad tabulkou

naklad_profil <- nakup-mean_PFC
fin_cenaEUR <- ceiling(prodej_eur/0.025) * 0.025 # zaokrouhleni na nejblizsi nejvyssi hranici 0,025 
fin_cenaCZK <- ceiling((fin_cenaEUR*kurz)/0.05) * 0.05 # zaokrouhleni na nejblizsi nejvyssi hranici 0,05 


# to do: rizikova prirazka - nasobici koeficient ?
# platnost ceny ?


# ------------------------------------------------------------------------------ final price

# to do : Okynka


fix_cena <- data.frame(
  Od = delOd,
  Do = delDo,
  `Cena EUR` = fin_cenaEUR,
  `Cena CZK` = fin_cenaCZK,
  # `Naklad na profil` = naklad_profil,
  `Naklad na BSD` = bsd,
  check.names = FALSE # aby nebyly v nazvu sloupcu misto mezer tecky
)

print(fix_cena)

