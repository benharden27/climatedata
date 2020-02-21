# Libraries
library(tidyverse)

# directory setup
read_dir <- "inst/extdata/raw/"
write_dir <- "inst/extdata/csv/"

# Carbon dioxide levels ---------------------------------------------------
# I got the Mauna loa data set from the [Keeling group page](http://scrippsco2.ucsd.edu/data/atmospheric_co2/), but you can also get it from [NOAA](https://www.esrl.noaa.gov/gmd/ccgg/trends/data.html)

#Reading in the raw data:

filename <- "inst/extdata/raw/monthly_in_situ_co2_mlo.csv"
df <- read_csv(filename,
               skip = 57,
               col_names = names(read_csv(filename,skip=54)))

# then wrangle
ml <- transmute(df, time = Date_1, co2 = CO2)
ml$co2[ml$co2 < 0] <- NA

# Also from Manoa Loa
filename <- "inst/extdata/raw/monthly_flask_co2_nzd.csv"
df <- read_csv(filename,
               skip = 57,
               col_names = names(read_csv(filename,skip=54)))

bh <- transmute(df, time = Date_1, co2 = CO2)
bh$co2[bh$co2 < 0] <- NA

# And combining these data sets into one tibble
co2 <- full_join(ml, bh, by = "time")
co2 <- rename(co2, ml = co2.x,bh = co2.y)
co2 <- gather(co2,key = "station", value = co2, ml, bh)

#Initial plot:
ggplot(co2, aes(time,co2,color = station)) +
  geom_line() +
  geom_smooth() +
  ylim(300,420) +
  xlim(1955,2020)


# Saving the data:
write_csv(ml,file.path(write_dir,"CO2_mauna_loa.csv"))
write_csv(bh,file.path(write_dir,"CO2_bering_head.csv"))


# Ice Extent --------------------------------------------------------------

# Data is from [NSIDC](https://nsidc.org/arcticseaicenews/sea-ice-tools/) in excel format

# Reading in the data
filename = file.path(read_dir,"Sea_Ice_Index_Daily_Extent_G02135_v3.0.xlsx")
df<- readxl::read_excel(filename)
df <- rename(df,"X__1" = "...1")
df <- rename(df,"X__2" = "...2")
for (i in 1:nrow(df)) {
if(is.na(df$X__1[i])) {
df$X__1[i] <- df$X__1[i-1]
}
}

df <- df %>%
gather(key = "year", value = "extent",`1978`:`2019`) %>%
transmute(X__1,X__2,year,extent) %>%
rename(month=X__1, day=X__2)

df <- transmute(df,dttm = lubridate::mdy(paste(df$month,df$day,df$year)), year, month, extent)

df <- filter(df,!is.na(extent))


ggplot(df,aes(dttm,extent)) +
geom_point() +
geom_smooth()

#Saving the data:
write_csv(df,file.path(write_dir,"sea_ice_extent.csv"))


# Surface Temperature -----------------------------------------------------
# Data is temperature anomalies compiled by NASA and available on their [website](https://data.giss.nasa.gov/gistemp/). Also some good [FAQs](https://data.giss.nasa.gov/gistemp/faq/abs_temp.html) about how this is calculated. These anomalies are calculated relative to the baseline period 1951 -- 1980.

# Reading in the raw data

df <- read_csv(file.path(read_dir,"surftemp_NASA.csv"), skip = 1)

# Wrangling the data

df <- df %>%
gather(key = "month", value = "temp",Jan:Dec) %>%
select(Year,month,temp)

df$month <- match(df$month,month.abb)
df <- df %>%
mutate(time = Year + month/12) %>%
dplyr::arrange(time) %>%
rename(year = Year) %>%
select(time, year, month, temp)

df$temp <- as.numeric(df$temp)

# Initial plot

ggplot(df, aes(time, temp)) +
geom_line() +
geom_smooth()

#Saving the data

write_csv(df,file.path(write_dir,"nasa_surftemp.csv"))



# Paleoclimate ------------------------------------------------------------
# Data is from Antarctic ice cores and can be accessed via [CDIAC](https://cdiac.ess-dive.lbl.gov/trends/co2/ice_core_co2.html) which will take you to the [NOAA Ice Core data site](https://www.ncdc.noaa.gov/data-access/paleoclimatology-data/datasets/ice-core)

# Carbon Dioxide

# CO2 data recoverd from this (data page)[https://www.ncdc.noaa.gov/paleo-search/study/6091]. Data is a combination of records from a few cores - raw data file and site have all relevent matedata.

# Reading in raw data

a <- read_lines(file.path(read_dir,"edc-co2-2008.txt"))
li <- tail(str_which(a,"Age"),1)
df <- read_delim(file.path(read_dir,"edc-co2-2008.txt")," ",trim_ws = T, skip = li-1)

#Wrangle data
df <- rename(df,time = `Age(yrBP)`, co2 = `CO2(ppmv)`)

# Initial plot

ggplot(df, aes(time, co2)) +
geom_line()

# Write data
write_csv(df,file.path(write_dir,"paleo_co2.csv"))



# Temperature

# Temperature data found [here](https://www.ncdc.noaa.gov/paleo-search/study/6080). Temperature value is relative to the average over the past 1000 years.

# Reading in raw data
a <- read_lines(file.path(read_dir,"edc3deuttemp2007.txt"))
li <- tail(str_which(a,"Bag"),1)
df <- read_delim(file.path(read_dir,"edc3deuttemp2007.txt")," ",trim_ws = T, skip = li-1)

# Wrangle data
df <- df %>%
rename(time = Age, deut = Deuterium, temp = Temperature) %>%
select(time, deut, temp)

# Initial plot
ggplot(df, aes(time, temp)) +
geom_line()

# Write data
write_csv(df,file.path(write_dir,"paleo_temp.csv"))


# Ocean Acidification -----------------------------------------------------

## HOTS

# Data is from the Hawaii Ocean Time Series and available for download [here](http://hahana.soest.hawaii.edu/hot/hot-dogs/interface.html). NB: You have to select the data that you want.

# Reading in data
df <- read_csv(file.path(read_dir,"hots_bottle_all.csv"))

#Wrangle data
df <- mutate(df, dttm = lubridate::mdy_hms(paste(date,time)))
df <- df[-1,]
df$press <- as.numeric(df$press)
df <- mutate(df,depth = oce::swDepth(df$press,21.2))
df$ph[df$ph<0] <- NA
df$alk <- as.numeric(df$alk)
df$alk[df$alk<0] <- NA
df$dic <- as.numeric(df$dic)
df$dic[df$dic<0] <- NA
df$temp <- as.numeric(df$temp)
df$csal <- as.numeric(df$csal)
df$csal[df$csal < 0] <- NA

# ii <- is.na(df$ph) & !is.na(df$dic) & !is.na(df$alk) & !is.na(df$temp) & !is.na(df$csal)
# df$ph[ii] <- seacarbon::pH_from_alk_dic(df$alk[ii],df$dic[ii],df$temp[ii],df$csal[ii])

df <- select(df,dttm,press,depth,temp,csal,ph,alk,dic)

# Initial plot
filter(df,!is.na(ph)) %>%
ggplot(aes(dttm,depth,color = ph)) +
geom_point() +
scale_y_reverse()

filter(df, depth < 50) %>%
ggplot(aes(dttm, ph)) +
geom_point() +
geom_smooth(method="lm")

# Write data
write_csv(df,file.path(write_dir,"hots_bottle.csv"))




# Surface Ocean CO2

# A data product is available [from HOTS](http://hahana.soest.hawaii.edu/hot/products/products.html)

# Reading data
df <- read_tsv(file.path(read_dir,"HOT_surface_CO2.txt"), trim_ws = TRUE, skip = 8)

# Wrangle
df[df<0] <- NA
df <- mutate(df,dttm= lubridate::dmy(date))
df <- select(df, dttm, DIC, TA, pHcalc_insitu, pCO2calc_insitu)

# and plot
ggplot(df, aes(dttm, pHcalc_insitu)) +
geom_point()

# write data
write_csv(df,file.path(write_dir,"surface_co2.csv"))



# Ice sheet extent --------------------------------------------------------
# Data is from IMBIE (http://imbie.org/data-downloads/)

# Antarctica
# read data
filename = file.path(read_dir,"imbie_dataset-2012_11_29.xlsx")
df<- readxl::read_excel(filename)

# wrangle
df <- transmute(df,time = Year,
             ice_change = `Cumulative ice mass change (Gt)`,
             ice_change_err = `Cumulative ice mass change uncertainty (Gt)`,
             sea_level_change = `Cumulative sea level contribution (mm)`,
             sea_level_change_err = `Cumulative sea level contribution uncertainty (mm)`)

# plot
ggplot(df, aes(time,ice_change)) +
  geom_line()

# write data
write_csv(df,file.path(write_dir,"antarctic_MB.csv"))


# Greenland
df<- readxl::read_excel(filename, sheet = 2)

# wrangle
df <- transmute(df,time = Year,
                ice_change = `Cumulative ice mass change (Gt)`,
                ice_change_err = `Cumulative ice mass change uncertainty (Gt)`,
                sea_level_change = `Cumulative sea level contribution (mm)`,
                sea_level_change_err = `Cumulative sea level contribution uncertainty (mm)`)

# plot
ggplot(df, aes(time,ice_change)) +
  geom_line()

# write data
write_csv(df,file.path(write_dir,"greenland_MB.csv"))



# Sea Level Rise ----------------------------------------------------------
# Data is from CSIRO (https://www.cmar.csiro.au/sealevel/sl_data_cmar.html)

# read altimeter data
filename <- file.path(read_dir,"CSIRO_ALT_gmsl_mo_2015.csv")
df <- read_csv(filename)

# wrangle
df <- rename(df, time = Time, sea_level = `GMSL (mm)`)

# plot
ggplot(df, aes(time, sea_level)) +
  geom_line()

# read reconstructed data
filename <- file.path(read_dir,"CSIRO_Recons_gmsl_mo_2015.csv")
df2 <- read_csv(filename)

# wrangle
df2 <- rename(df2, time = Time, sea_level = `GMSL (mm)`, sea_level_err = `GMSL uncertainty (mm)`)

# plot
ggplot(df2, aes(time, sea_level)) +
  geom_line()

df_all <- right_join(df,df2,by="time")

df_all <- rename(df_all, sea_level_alt = sea_level.x,
                 sea_level_rec = sea_level.y,
                 sea_level_rec_err = sea_level_err)

ggplot(df_all) +
  geom_line(aes(time, sea_level_rec)) +
  geom_line(aes(time, sea_level_alt),color = "red")

# write data
write_csv(df_all,file.path(write_dir,"gmsl.csv"))




# Ocean Temperature -------------------------------------------------------
# Data is from NOAA (https://www.nodc.noaa.gov/OC5/3M_HEAT_CONTENT/)
filename <- file.path(read_dir,"T-dC-w0-100m.dat")
df <- read_delim(filename,delim = " ",trim_ws = TRUE)

df <- rename(df, time = YEAR)

ggplot(df, aes(time, WO)) +
  geom_line()

write_csv(df,file.path(write_dir,"ocean_temp.csv"))
