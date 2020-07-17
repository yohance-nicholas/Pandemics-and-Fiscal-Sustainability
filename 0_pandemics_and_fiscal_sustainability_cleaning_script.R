# Pandemics and Fiscal Sustainability
# Prepared by Yohance Nicholas
# Version: July 17th 2020

library(tidyverse)
library(tsibble)
library(tidyr)
library(sjmisc)
library(zoo) # for rollmean()


# Create ISO3C Variables --------------------------------------------------
commodity <- c("GUY", "JAM", "TTO") # To create variable which identifies type of economy
oecs <- c("ATG", "DMA", "GRD", "KNA", "LCA", "VCT") # To create variable which identifies OECS Member States
caricom <- c("ATG","BHS","BLZ","BRB","DMA","GRD","GUY","HTI","JAM","KNA","LCA","SUR","TTO","VCT") # To create variable which identifies CARICOM Member States
region <- c("CSS", "EAR", "LTE" , "LAC")
caricom_and_regions <- c("ATG","BHS","BLZ","BRB","DMA","GRD","GUY","HTI","JAM","KNA","LCA","SUR","TTO","VCT","CSS", "EAR", "LTE" , "LAC") # To create variable which identifies CARICOM Member States

# Import WDI Data
library(wbstats)
series <- c("NY.GDP.MKTP.KD",
            "NY.GDP.PCAP.KD",
            "NY.GDP.MKTP.KD.ZG",
            "NY.GDP.PCAP.KD.ZG",
            "GC.DOD.TOTL.GD.ZS",
            "ST.INT.ARVL",
            "GC.REV.XGRT.GD.ZS",
            "GC.TAX.TOTL.GD.ZS",
            "GC.XPN.TOTL.GD.ZS",
            "BN.CAB.XOKA.GD.ZS",
            "SI.POV.NAHC",
            "SI.POV.DDAY",
            "SI.POV.GINI",
            "FP.CPI.TOTL.ZG",
            "SL.UEM.TOTL.NE.ZS",
            "SL.UEM.TOTL.ZS",
            "FM.AST.NFRG.CN",
            "FD.RES.LIQU.AS.ZS",
            "FB.BNK.CAPA.ZS",
            "FB.AST.NPER.ZS",
            "FS.AST.DOMS.GD.ZS",
            "BN.RES.INCL.CD",
            "FI.RES.TOTL.MO")

caricom_data <- wb(indicator = series,
              mrv = 19) %>% 
  filter(iso3c %in% caricom)%>% 
  mutate(economy = if_else(iso3c %in% commodity, "Commodity Based", "Service Based" ),
         oecs = if_else(iso3c %in% oecs, "OECS Member State", "Non-OECS Member State" ),
         caricom = if_else(iso3c %in% caricom, "CARICOM Member State", "Non-CARICOM Member State" ))

wb_countries <- wbcountries() %>% 
  select(iso3c,
         lat,
         long,
         income,
         region) 

caricom_data <- caricom_data %>%  left_join(wb_countries,
                                  by = "iso3c")

caricom_and_regions_data <- wb(indicator = series,
                   mrv = 19) %>% 
  filter(iso3c %in% caricom_and_regions)

by_economy_type <- group_by(caricom_data, economy)
by_income <- group_by(caricom_data, income)
by_oecs <- group_by(caricom_data, oecs)
by_caricom <- group_by(caricom_data, caricom)
by_region <- group_by(caricom_data, region)

# Export Data Set ---------------------------------------------------------
# Caricom Data
write.csv(caricom_data, "1. Data/caricom_data.csv")
saveRDS(caricom_data, "1. Data/caricom_data.Rds")

# Caricom and Region Data
write.csv(caricom_and_regions_data, "1. Data/caricom_and_regions_data.csv")
saveRDS(caricom_and_regions_data, "1. Data/caricom_and_regions_data.Rds")

# Remove Objects no longer required from the Environment --------------------
rm("series", "wb_countries")
