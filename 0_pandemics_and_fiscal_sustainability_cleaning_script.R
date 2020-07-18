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


# Import WDI Data ---------------------------------------------------------


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
         iso2c,
         lat,
         long,
         income,
         region) %>% 
  filter(iso3c %in% caricom)

caricom_iso2c <- wb_countries %>% pull(iso2c) # Create iso2c variable

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



# Import Primary Commodity Prices [COMMP] ---------------------------------
# https://www.imf.org/en/Research/commodity-prices

library(rdbnomics)
commodity_prices <- rdb(api_link = "https://api.db.nomics.world/v22/series/IMF/COMMP?limit=1000&offset=0&q=&observations=1&align_periods=1&dimensions=%7B%22FREQ%22%3A%5B%22M%22%5D%2C%22REF_AREA%22%3A%5B%22W0%22%5D%2C%22COMMODITY%22%3A%5B%22PRAWM_Index%22%2C%22PALUM_USD%22%2C%22PBANSOP_USD%22%2C%22POILBRE_USD%22%2C%22PSALM_USD%22%2C%22PNGASUS_USD%22%2C%22PSUGAISA_USD%22%2C%22POILWTI_USD%22%5D%7D")


# Import Financial Soundness Indicators [FSI] -----------------------------
# https://data.imf.org/?sk=51B096FA-2CD2-40C2-8D09-0699CC1764DA
library(rdbnomics)
fsi_dim <- list(
  FREQ = "A",
  REF_AREA = caricom_iso2c
)

carcom_fsi <- rdb("IMF",
                  "FSI",
                  dimensions = fsi_dim)

# Import World Economic Outlook Data [WEO] --------------------------------
# https://www.imf.org/en/Publications/WEO
weo_dim <- list(
  "weo-country" = caricom
)

caricom_weo <- rdb("IMF",
                   "WEO",
                   dimensions = weo_dim)

# Import Government Finance Statistics (GFS) ------------------------------

# Expenditure by Function of Government (COFOG) [GFSCOFOG]
# Visit https://data.imf.org/?sk=5804C5E1-0502-4672-BDCD-671BCDC565A9

GFSCOFOG_dim <- list(
  FREQ = "A",
  REF_AREA = caricom_iso2c,
  REF_SECTOR = "S13",
  UNIT_MEASURE = "XDC_R_B1GQ"
)

caricom_GFSCOFOG <- rdbnomics::rdb("IMF",
                                   "GFSCOFOG",
                                   dimensions = GFSCOFOG_dim)

# Main Aggregates and Balances [GFSMAB]
# data.imf.org/gfs

gfsmab_dim <- list(
  FREQ = "A",
  REF_AREA = caricom_iso2c
)

caricom_gfsmab <- rdbnomics::rdb("IMF",
                                 "GFSMAB",
                                 dimensions = gfsmab_dim)

# Government Finance Statistics (GFS), Revenue [GFSR] 
gfsr_dim <- list(
  FREQ = "A",
  REF_AREA = caricom_iso2c
)

caricom_gfsr <- rdbnomics::rdb("IMF",
                               "GFSR",
                               dimensions = gfsr_dim)

# Import Balance of Payments (BOP) ----------------------------------------
# https://data.imf.org/?sk=7A51304B-6426-40C0-83DD-CA473CA1FD52

bop_dim <- list(
  FREQ = "A",
  REF_AREA = caricom_iso2c
)

caricom_bop <- rdbnomics::rdb("IMF",
                              "BOP",
                              dimensions = bop_dim)


# Import Monetary and Financial Statistics (MFS) --------------------------
mfs_dim <- list(
  FREQ = "M",
  REF_AREA = caricom_iso2c
)

caricom_mfs <-  rdbnomics::rdb("IMF",
                               "MFS",
                               dimensions = mfs_dim)



# Import International Financial Statistics (IFS) -------------------------
# data.imf.org/ifs

ifs_dim = list(
  FREQ = "M",
  REF_AREA = caricom_iso2c
)

caricom_ifs <- rdbnomics::rdb("IMF",
                              "IFS",
                             dimensions = ifs_dim)


# Import World Revenue Longitudinal Data (WoRLD) [WoRLD] ------------------
world_dim <- list(
  FREQ = "M",
  REF_AREA = caricom_iso2c
)

caricom_world <- rdbnomics::rdb("IMF",
                                "IFS",
                                dimensions = world_dim)
# Export Data Set ---------------------------------------------------------
# Caricom Data
write.csv(caricom_data, "1. Data/caricom_data.csv")
saveRDS(caricom_data, "1. Data/caricom_data.Rds")

# Caricom and Region Data
write.csv(caricom_and_regions_data, "1. Data/caricom_and_regions_data.csv")
saveRDS(caricom_and_regions_data, "1. Data/caricom_and_regions_data.Rds")

# iso2c and iso3c objects
saveRDS(caricom, "1. Data/caricom_iso3c.Rds")
saveRDS(caricom_iso2c, "1. Data/caricom_iso2c.Rds")
saveRDS(oecs, "1. Data/oecs_iso3c.Rds")

# # Merged xlsx workbook
# # Source http://www.sthda.com/english/wiki/writing-data-from-r-to-excel-files-xls-xlsx
# library(xlsx)
# 
# # TODO(yohance.nicholas): Verify solution to error https://stackoverflow.com/questions/21937640/handling-java-lang-outofmemoryerror-when-writing-to-excel-from-r
# # Write the first data set in a new workbook
# write.xlsx2(caricom_gfsr, file = "1. Data/caricom_merged_data.xlsx",
#            sheetName = "GFSR", append = FALSE)
# 
# # Add a second data set in a new worksheet
# write.xlsx2(mtcars, file = "myworkbook.xlsx",
#            sheetName="MTCARS", append=TRUE)
# 
# # Add a third data set
# write.xlsx2(iris, file = "myworkbook.xlsx",
#            sheetName="IRIS", append=TRUE)

# Remove Objects no longer required from the Environment --------------------
rm("series", "wb_countries","GFSCOFOG_dim","weo_dim","fsi_dim")
