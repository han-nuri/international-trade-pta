

############################################################################
############################################################################
###                                                                      ###
###                           SECTION 0: SETUP                           ###
###                                                                      ###
############################################################################
############################################################################

# Packages to use
pacman::p_load(data.table, dplyr, cepiigeodist, foreign, 
               sqldf, gravity, parallel, countrycode, 
               magrittr, Matrix, ggplot2, estimatr, 
               bannerCommenter, stringi, stringr, 
               tidyverse, haven, foreign, stargazer, R6,
               plm)

dta2 = fread("Raw_Data/bilateral_DTA_2.csv")
setDT(dta2)

###########################################################################
###########################################################################
###                                                                     ###
###                      SECTION 1: DATA WRANGLING                      ###
###                                                                     ###
###########################################################################
###########################################################################


##---------------------------------------------------------------
##          SECTION 1.1: DEEP TRADE AGREEMENT DATASET          --
##---------------------------------------------------------------
#retain >= 1990 data
dta2 = dta2[year >= 1990]

#rename ANT to ATG
dta2[iso1 == "ANT", iso1 := "ATG"]
dta2[iso2 == "ANT", iso2 := "ATG"]

#convert iso3c to iso3n
dta2$iso3n_A <- countrycode(dta2$iso1, origin = "iso3c", destination = "iso3n")
dta2$iso3n_B <- countrycode(dta2$iso2, origin = "iso3c", destination = "iso3n")

dta2 %<>% relocate(iso3n_A, .before = year)
dta2 %<>% relocate(iso3n_B, .before = year)



##----------------------------------------------------------------
##           SECTION 1.2: TRADE FLOWS DATASET (GOODS)           --
##----------------------------------------------------------------
# Period: 1990 - 2017
goods_asia <- fread("Raw_Data/ASIA_goods.csv")
goods_brics_africa <- fread("Raw_Data/BRICS+AFRICA_goods.csv")
goods_latinamer_europe_oceania <- fread("Raw_Data/LATIN+EUROPE+OCEANIA_goods.csv")
goods_oecd <- fread("Raw_Data/OECD_goods.csv")

#complete goods dataset
goods <- rbind(goods_asia, goods_brics_africa, goods_latinamer_europe_oceania, goods_oecd)
rm(goods_asia, goods_brics_africa, goods_latinamer_europe_oceania, goods_oecd)

goods <- goods[TIME >= 1990 & TIME <= 2017]

goods <- goods[, c(1,2,4, 5, 12, 13, 17, 21)]

##---------------------------------------------------------------
##         SECTION 1.3: TRADE FLOWS DATASET (SERVICES)         --
##---------------------------------------------------------------
services <- fread("Raw_Data/Services_Dataset.csv")


##---------------------------------------------------------------
##                SECTION 1.4: DISTANCE DATASET                --
##---------------------------------------------------------------

distance <- cepiigeodist::dist_cepii
geography <- cepiigeodist::geo_cepii # geography data
setDT(geography)
setDT(distance)
setnames(distance, "iso_o", "iso_A")
setnames(distance, "iso_d", "iso_B")
setnames(geography, "iso3", "iso_3")

lol = codelist

#creat area column
distance[geography, area_A := i.area , on = c("iso_A" = "iso_3")]
distance[geography, area_B := i.area , on = c("iso_B" = "iso_3")]

# create landlocked column
distance[geography, landlocked_A := i.landlocked , on = c("iso_A" = "iso_3")]
distance[geography, landlocked_B := i.landlocked , on = c("iso_B" = "iso_3")]
rm(geography)


asdf = unique(distance$iso_A[!distance$iso_A %in% goods$COU])

writeClipboard(asdf)

setDT(distance)


distance[iso_A == "AIA", iso_A := "AIA"]
distance[iso_A == "ANT", iso_A := "ATG"]
distance[iso_A == "CCK", iso_A := "CCK"]
distance[iso_A == "COK", iso_A := "COK"]
distance[iso_A == "DJI", iso_A := "DJI"]
distance[iso_A == "ERI", iso_A := "ERI"]
distance[iso_A == "ESH", iso_A := "ESH"]
distance[iso_A == "FLK", iso_A := "FLK"]
distance[iso_A == "FRO", iso_A := "FRO"]
distance[iso_A == "FSM", iso_A := "FSM"]
distance[iso_A == "GIB", iso_A := "GIB"]
distance[iso_A == "GLP", iso_A := "GLP"]
distance[iso_A == "GNB", iso_A := "GNB"]
distance[iso_A == "GNQ", iso_A := "GNQ"]
distance[iso_A == "GUF", iso_A := "GUF"]
distance[iso_A == "HTI", iso_A := "HTI"]
distance[iso_A == "IRQ", iso_A := "IRQ"]
distance[iso_A == "LBR", iso_A := "LBR"]
distance[iso_A == "LBY", iso_A := "LBY"]
distance[iso_A == "MHL", iso_A := "MHL"]
distance[iso_A == "MNP", iso_A := "MNP"]
distance[iso_A == "MTQ", iso_A := "MTQ"]
distance[iso_A == "NFK", iso_A := "NFK"]
distance[iso_A == "NIU", iso_A := "NIU"]
distance[iso_A == "NRU", iso_A := "NRU"]
distance[iso_A == "PAL", iso_A := "PSE"]
distance[iso_A == "PCN", iso_A := "PCN"]
distance[iso_A == "PLW", iso_A := "PLW"]
distance[iso_A == "PRI", iso_A := "PRI"]
distance[iso_A == "PRK", iso_A := "PRK"]
distance[iso_A == "PYF", iso_A := "PYF"]
distance[iso_A == "REU", iso_A := "REU"]
distance[iso_A == "ROM", iso_A := "ROU"]
distance[iso_A == "SHN", iso_A := "SHN"]
distance[iso_A == "SLB", iso_A := "SLB"]
distance[iso_A == "SMR", iso_A := "SMR"]
distance[iso_A == "SOM", iso_A := "SOM"]
distance[iso_A == "SPM", iso_A := "SPM"]
distance[iso_A == "TCA", iso_A := "TCA"]
distance[iso_A == "TCD", iso_A := "TCD"]
distance[iso_A == "TJK", iso_A := "TJK"]
distance[iso_A == "TKL", iso_A := "TKL"]
distance[iso_A == "TKM", iso_A := "TKM"]
distance[iso_A == "TMP", iso_A := "TLS"]
distance[iso_A == "TUV", iso_A := "TUV"]
distance[iso_A == "VGB", iso_A := "VGB"]
distance[iso_A == "VUT", iso_A := "VUT"]
distance[iso_A == "WLF", iso_A := "WLF"]
distance[iso_A == "YUG", iso_A := "YUG"]
distance[iso_A == "ZAR", iso_A := "COD"]

distance[iso_B == "AIA", iso_B := "AIA"]
distance[iso_B == "ANT", iso_B := "ATG"]
distance[iso_B == "CCK", iso_B := "CCK"]
distance[iso_B == "COK", iso_B := "COK"]
distance[iso_B == "DJI", iso_B := "DJI"]
distance[iso_B == "ERI", iso_B := "ERI"]
distance[iso_B == "ESH", iso_B := "ESH"]
distance[iso_B == "FLK", iso_B := "FLK"]
distance[iso_B == "FRO", iso_B := "FRO"]
distance[iso_B == "FSM", iso_B := "FSM"]
distance[iso_B == "GIB", iso_B := "GIB"]
distance[iso_B == "GLP", iso_B := "GLP"]
distance[iso_B == "GNB", iso_B := "GNB"]
distance[iso_B == "GNQ", iso_B := "GNQ"]
distance[iso_B == "GUF", iso_B := "GUF"]
distance[iso_B == "HTI", iso_B := "HTI"]
distance[iso_B == "IRQ", iso_B := "IRQ"]
distance[iso_B == "LBR", iso_B := "LBR"]
distance[iso_B == "LBY", iso_B := "LBY"]
distance[iso_B == "MHL", iso_B := "MHL"]
distance[iso_B == "MNP", iso_B := "MNP"]
distance[iso_B == "MTQ", iso_B := "MTQ"]
distance[iso_B == "NFK", iso_B := "NFK"]
distance[iso_B == "NIU", iso_B := "NIU"]
distance[iso_B == "NRU", iso_B := "NRU"]
distance[iso_B == "PAL", iso_B := "PSE"]
distance[iso_B == "PCN", iso_B := "PCN"]
distance[iso_B == "PLW", iso_B := "PLW"]
distance[iso_B == "PRI", iso_B := "PRI"]
distance[iso_B == "PRK", iso_B := "PRK"]
distance[iso_B == "PYF", iso_B := "PYF"]
distance[iso_B == "REU", iso_B := "REU"]
distance[iso_B == "ROM", iso_B := "ROU"]
distance[iso_B == "SHN", iso_B := "SHN"]
distance[iso_B == "SLB", iso_B := "SLB"]
distance[iso_B == "SMR", iso_B := "SMR"]
distance[iso_B == "SOM", iso_B := "SOM"]
distance[iso_B == "SPM", iso_B := "SPM"]
distance[iso_B == "TCA", iso_B := "TCA"]
distance[iso_B == "TCD", iso_B := "TCD"]
distance[iso_B == "TJK", iso_B := "TJK"]
distance[iso_B == "TKL", iso_B := "TKL"]
distance[iso_B == "TKM", iso_B := "TKM"]
distance[iso_B == "TMP", iso_B := "TLS"]
distance[iso_B == "TUV", iso_B := "TUV"]
distance[iso_B == "VGB", iso_B := "VGB"]
distance[iso_B == "VUT", iso_B := "VUT"]
distance[iso_B == "WLF", iso_B := "WLF"]
distance[iso_B == "YUG", iso_B := "YUG"]
distance[iso_B == "ZAR", iso_B := "COD"]


# merge Goods dataset and distance dataset
# other variables
goods[distance, ':=' (contig = contig,
                      comlang_off = comlang_off,
                      comlang_ethno = comlang_ethno,
                      colony = colony,
                      comcol = comcol,
                      curcol = curcol,
                      col45 = col45,
                      smctry = smctry,
                      dist = dist,
                      distcap = distcap,
                      distw = distw,
                      distwces = distwces), on = c("COU" = "iso_A", "PAR" = "iso_B")]


setnames(goods, "TIME", "year")


rm(distance)

table(is.na(goods$dist))




# keep only export variable, I have the option to analyze imports later
goods <- goods[, Flow := "Exports"]

# remove unnecessary columns in goods data, Value of exports is in thousands USD
goods <- goods[, -c(2,3,5,7)]


banner("SECTION 1.5: POLITY V DATASET", bandChar = "-")


##---------------------------------------------------------------
##                SECTION 1.5: POLITY V DATASET                --
##---------------------------------------------------------------
polity_v <- fread("Raw_Data/Polity_5.csv")

polity_scope <- polity_v[year >= 1990 & year <= 2017]

#the polity dataset uses correlates of war country code, not iso
polity_scope$iso3c <- countrycode(polity_scope$ccode, origin = "cown", destination = "iso3c")

#not matched: yugusloavia, vietnam, south sudan, serbia, montenegro, ethiopia, czech
polity_scope[country == "Yugoslavia", iso3c := "YUG"]
polity_scope[country == "Vietnam", iso3c := "VNM"]
polity_scope[country == "South Sudan", iso3c := "SSD"]
polity_scope[country == "Serbia", iso3c := "SRB"]
polity_scope[country == "Montenegro", iso3c := "MNE"]
polity_scope[country == "Ethiopia", iso3c := "ETH"]
polity_scope[country == "Czechoslovakia", iso3c := "CSK"]

# merge goods data with polity
goods[polity_scope, polity2_A := i.polity2, on = c("year", "COU" = "iso3c")]
goods[polity_scope, polity2_B := i.polity2, on = c("year", "PAR" = "iso3c")]


rm(polity_v, polity_scope)

##---------------------------------------------------------------
##               SECTION 1.6: POPULATION DATASET               --
##---------------------------------------------------------------
population <- fread("Raw_Data/Population_total.csv", header = T)

#reshape from wide to longer format
population = melt(population, measure.vars = c(5:65),
               variable.name = "year",
               value.name = c("population"))

population[, year := as.numeric(as.character(year))]
population <- population[, c(2,6,7)][year >= 1990 & year <= 2017]
setnames(population, "Country Code", "country")

#merge goods with population
goods[population, population_A := population, on = c("year", "COU" = "country")]
goods[population, population_B := population, on = c("year", "PAR" = "country")]

rm(population)

##----------------------------------------------------------------
##                   SECTION 1.7: GDP DATASET                   --
##----------------------------------------------------------------

options(scipen = 999)
GDP <- fread("Raw_Data/GDP.csv", header = T)
GDP <- GDP[,-c(1,3,4)]

#make columns numeric
cols = GDP[, c(2:ncol(GDP))]
GDP[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]

GDP = melt(GDP, measure.vars = c(2:ncol(GDP)),
                  variable.name = "year",
                  value.name = "GDP")

GDP[, year := as.numeric(as.character(year))]

GDP_scope <- GDP[year >= 1990 & year <= 2017]

#GDP growth
GDP_growth <- fread("Raw_Data/GDP_growth_annual_%.csv", header = T)
GDP_growth <- GDP_growth[,-c(1,3,4)]

#make columns numeric
cols = GDP_growth[, c(2:ncol(GDP_growth))]
GDP_growth[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols]

GDP_growth = melt(GDP_growth, measure.vars = c(2:ncol(GDP_growth)),
           variable.name = "year",
           value.name = "GDP_growth")

GDP_growth[, year := as.numeric(as.character(year))]

GDP_growth_scope <- GDP_growth[year >= 1990 & year <= 2017]
GDP_scope[GDP_growth_scope, GDP_growth := GDP_growth, on = c("Country Code", "year")]
rm(GDP_growth_scope)
rm(GDP_growth)
rm(GDP)

# merge goods data with gdp scope data
goods[GDP_scope, GDP_A := GDP, on = c("year", "COU" = "Country Code")]
goods[GDP_scope, GDP_B := GDP, on = c("year", "PAR" = "Country Code")]
goods[GDP_scope, GDP_growth_A := GDP_growth, on = c("year", "COU" = "Country Code")]
goods[GDP_scope, GDP_growth_B := GDP_growth, on = c("year", "PAR" = "Country Code")]
rm(GDP_scope)


# there are sometimes multiple values for one country pair for a specific year, drop the one that is lower
goods = goods[goods[, .I[Value == max(Value)], by= list(COU, PAR, year)]$V1]
setDT(goods)


##----------------------------------------------------------------
##                   SECTION 1.8: FINAL MERGE                   --
##----------------------------------------------------------------

# check for iso3c differences
unique(goods$COU[!goods$COU %in% dta2$iso1])
unique(dta2$iso1[!dta2$iso1 %in% goods$COU])



# Merge goods with dta2
dta2[goods, ':=' (Value = Value,
                  contig = contig,
                  comlang_off = comlang_off,
                      comlang_ethno = comlang_ethno,
                      colony = colony,
                      comcol = comcol,
                      curcol = curcol,
                      col45 = col45,
                      smctry = smctry,
                      dist = dist,
                      distcap = distcap,
                      distw = distw,
                      distwces = distwces,
                  polity2_A = polity2_A,
                  polity2_B = polity2_B,
                  population_A = population_A,
                  population_B = population_B,
                  GDP_A = GDP_A, 
                  GDP_B = GDP_B,
                  GDP_growth_A = GDP_growth_A,
                  GDP_growth_B = GDP_growth_B), on = c("iso1" = "COU", "iso2" = "PAR", "year")]

# only data, where there is a PTA: I don't look at non-pta country pairs
dta2 = dta2[PTA == 1,]

# only keep data points that include independent variable values
dta2 = dta2[!is.na(AD_prov_01),]

# CHECK NA'S
#remove data points that lack my main independent variable: the policy provision columns
# which_nas = lapply(dta2, function(x) table(is.na(x))[2])
# which_na = as.data.frame(unlist(which_nas))

# what happens if i remove all rows that have no polity value
test2 = dta2[!is.na(polity2_A),]
test2 = test2[!is.na(polity2_B),]
test2 = test2[!is.na(GDP_B),]
test2 = test2[!is.na(contig),]
test2 = test2[!is.na(GDP_A),]
test2 = test2[!is.na(GDP_growth_A),]

# which_nas = lapply(test2, function(x) table(is.na(x))[2])
# which_na = as.data.frame(unlist(which_nas))

dta3 = test2




###########################################################################
###########################################################################
###                                                                     ###
###                    SECTION 2: RECODING VARIABLES                    ###
###                                                                     ###
###########################################################################
###########################################################################

banner("SECTION 2.2: PTA BREADTH - SUM OF POLICY AREAS", bandChar = "-")

##----------------------------------------------------------------
##          SECTION 2.1: PTA DEPTH - SUM OF PROVISIONS          --
##----------------------------------------------------------------

# Columns of interest: 6 to 942
# dta3$depth_var = rowSums(dta3[,c(6:942)]) # ==> this variable is already calculated in the variable "depth", dont need to calculate again


##----------------------------------------------------------------
##        SECTION 2.2: PTA BREADTH - SUM OF POLICY AREAS        --
##----------------------------------------------------------------



# 2.2.1: ANTI-DUMPING
dta3[, AD_1 := if_else(AD_prov_04 == 1, 1, 0)]
sum(dta3$AD_1)


# 2.2.2: COMPETITION POLICY
dta3[, CP_2 := if_else(CP_prov_01 == 1 | CP_prov_02 == 1, 1, 0)]
sum(dta3$CP_2)

# 2.2.3: COUNTERVAILING DUTIES
dta3[, CVD_3 := if_else(CVD_prov_03 == 1, 1, 0)]
sum(dta3$CVD_3)

# 2.2.4: ENVIRONMENTAL LAW
dta3[, ENV_4 := if_else(Env_prov_01 == 1 & Env_prov_02 == 1 & Env_prov_03 == 1 & Env_prov_04 == 1 | Env_prov_05 == 1, 1, 0)]
sum(dta3$ENV_4)

# 2.2.5: EXPORT TAXES
dta3[, ET_5 := if_else(ET_prov_02 == 1 | ET_prov_09 == 1 | ET_prov_11 == 1 | ET_prov_13 == 1, 1, 0)]
sum(dta3$ET_5)

# 2.2.6: INVESTMENT
dta3[, INV_6 := ifelse(rowSums(dta3[, grep(pattern = "Inv_", colnames(dta3)), with = F]) >= 1, 1, 0)]
sum(dta3$INV_6)

# 2.2.7: INTELLECTUAL PROPERTY
dta3[, IPR_7 := if_else(c(IPR_prov_01 | IPR_prov_02 | IPR_prov_03 | IPR_prov_04 | IPR_prov_05 | IPR_prov_06 | IPR_prov_07 | IPR_prov_08 | IPR_prov_09) == 1, 1, 0)]
sum(dta3$IPR_7)

# 2.2.8: LABOR MARKET REGULATIONS
dta3[, LM_8 := if_else(c(LM_prov_03|LM_prov_04|LM_prov_05|LM_prov_06 |LM_prov_07|LM_prov_08|LM_prov_09|LM_prov_10|LM_prov_11|
                           LM_prov_12|LM_prov_13|LM_prov_14|LM_prov_15|LM_prov_16|LM_prov_17|LM_prov_18) == 1, 1, 0)]
sum(dta3$LM_8)

# 2.2.9: MIGRATION
dta3[, MIG_9 := ifelse(rowSums(dta3[, grep(pattern = "Mig_", colnames(dta3)), with = F]) >= 1, 1, 0)]
sum(dta3$MIG_9)

# 2.2.10: MOVEMENT OF CAPITALS
dta3[, MOC_10 := ifelse(rowSums(dta3[, grep(pattern = "MoC_p", colnames(dta3)), with = F]) >= 1, 1, 0)]
sum(dta3$MOC_10)


# 2.2.11: PUBLIC PROCUREMENT
dta3[, PP_11 := ifelse(rowSums(dta3[, grep(pattern = "PP_p", colnames(dta3)), with = F]) >= 1, 1, 0)]
sum(dta3$PP_11)


# 2.2.12: RULES OF ORIGIN
dta3[, ROR_12 := ifelse(rowSums(dta3[, grep(pattern = "RoR_p", colnames(dta3)), with = F]) >= 1, 1, 0)]
sum(dta3$ROR_12)


# 2.2.13: SERVICES
dta3[, SER_13 := ifelse(rowSums(dta3[, grep(pattern = "Ser_p", colnames(dta3)), with = F]) >= 1, 1, 0)]
sum(dta3$SER_13)


# 2.2.14: SANITARY AND PHYTOSANITARY
dta3[, SPS_14 := ifelse(rowSums(dta3[, grep(pattern = "SPS_p", colnames(dta3)), with = F]) >= 1, 1, 0)]
sum(dta3$SPS_14)


# 2.2.15: STATE OWNED ENTERPRISES
dta3[, STE_15 := ifelse(rowSums(dta3[, grep(pattern = "STE_p", colnames(dta3)), with = F]) >= 1, 1, 0)]
sum(dta3$STE_15)


# 2.2.16: SUBSIDIES
dta3[, SUB_16 := ifelse(rowSums(dta3[, grep(pattern = "Sub_p", colnames(dta3)), with = F]) >= 1, 1, 0)]
sum(dta3$SUB_16)

# 2.2.17: TECHNICAL BARRIERS
dta3[, TBT_17 := ifelse(rowSums(dta3[, grep(pattern = "TBT_p", colnames(dta3)), with = F]) >= 1, 1, 0)]
sum(dta3$TBT_17)

# 2.2.18: TRADE FACILITATION
dta3[, TF_18 := ifelse(rowSums(dta3[, grep(pattern = "TF_p", colnames(dta3)), with = F]) >= 1, 1, 0)]
sum(dta3$TF_18)


#final rowsum for my second variable
dta3[, breadth := rowSums(dta3[, c(970:987), with = F])]
sum(dta3$breadth)
hist(dta3$breadth)
table(dta3$breadth)


###### Section 2.3: Environmental and Other Provisions, individual depth. ##########
# anti-dumping depth
dta3[, AD_depth := rowSums(dta3[, grep(pattern = "AD_p", colnames(dta3)), with = F])]
hist(dta3$AD_depth)

# Competition protection depth
dta3[, CP_depth := rowSums(dta3[, grep(pattern = "CP_p", colnames(dta3)), with = F])]
hist(dta3$CP_depth)

# Countervailing depth
dta3[, CVD_depth := rowSums(dta3[, grep(pattern = "CVD_p", colnames(dta3)), with = F])]
hist(dta3$CVD_depth)

# export taxes depth
dta3[, ET_depth := rowSums(dta3[, grep(pattern = "ET_p", colnames(dta3)), with = F])]
hist(dta3$ET_depth)

#environment depth
dta3[, ENV_depth := rowSums(dta3[, grep(pattern = "Env_p", colnames(dta3)), with = F])]
hist(dta3$ENV_depth)

# labor market provision depth
dta3[, LM_depth := rowSums(dta3[, grep(pattern = "LM_p", colnames(dta3)), with = F])]
hist(dta3$LM_depth)

# Investment provision depth
dta3[, INV_depth := rowSums(dta3[, grep(pattern = "Inv_p", colnames(dta3)), with = F])]
hist(dta3$INV_depth)

# Intellectual Property 
dta3[, IPR_depth := rowSums(dta3[, grep(pattern = "IPR_p", colnames(dta3)), with = F])]
hist(dta3$IPR_depth)

# Migration Provisions
dta3[, MIG_depth := rowSums(dta3[, grep(pattern = "Mig_p", colnames(dta3)), with = F])]
hist(dta3$MIG_depth)

#MOVEMENT OF CAPITALS
dta3[, MOC_depth := rowSums(dta3[, grep(pattern = "MoC_p", colnames(dta3)), with = F])]
hist(dta3$MOC_depth)

# 2.2.11: PUBLIC PROCUREMENT
dta3[, PP_depth := rowSums(dta3[, grep(pattern = "PP_p", colnames(dta3)), with = F])]
hist(dta3$PP_depth)

# 2.2.12: RULES OF ORIGIN
dta3[, ROR_depth := rowSums(dta3[, grep(pattern = "RoR_p", colnames(dta3)), with = F])]
hist(dta3$ROR_depth)

# Services Provisions depth
dta3[, SER_depth := rowSums(dta3[, grep(pattern = "Ser_p", colnames(dta3)), with = F])]
hist(dta3$SER_depth)

# Sanity and Phytosanitary provisions
dta3[, SPS_depth := rowSums(dta3[, grep(pattern = "SPS_p", colnames(dta3)), with = F])]
hist(dta3$SPS_depth)

# 2.2.15: STATE OWNED ENTERPRISES
dta3[, STE_depth := rowSums(dta3[, grep(pattern = "STE_p", colnames(dta3)), with = F])]
hist(dta3$STE_depth)

# 2.2.16: SUBSIDIES
dta3[, SUB_depth := rowSums(dta3[, grep(pattern = "Sub_p", colnames(dta3)), with = F])]
hist(dta3$SUB_depth)

# 2.2.17: TECHNICAL BARRIERS
dta3[, TBT_depth := rowSums(dta3[, grep(pattern = "TBT_p", colnames(dta3)), with = F])]
hist(dta3$TB_depth)

# 2.2.18: TRADE FACILITATION
dta3[, TF_depth := rowSums(dta3[, grep(pattern = "TF_p", colnames(dta3)), with = F])]
hist(dta3$TF_depth)


###########################################################################
###########################################################################
###                                                                     ###
###                  SECTION 3: DESCRIPTIVE STATISTICS                  ###
###                                                                     ###
###########################################################################
###########################################################################


##----------------------------------------------------------------
##                  SECTION 3.1: SUMMARY TABLE                  --
##----------------------------------------------------------------


dta4 = dta3[, c(1:5, 947:1006)]

summary_data = summary(dta4)

stargazer(dta4,
          type = "html",
          title = "Summary Table",
          out = "summary_table.html")


##---------------------------------------------------------------
##                 SECTION 3.2: VISUALIZATIONS                 --
##---------------------------------------------------------------
hist(log(dta4$Value), breaks = 1000)
hist(dta4$depth, breaks = 300)


# save dta4 file as clean data
write_csv(dta4, "dta4.csv")
write_csv(dta3, "dta_long.csv")