install.packages(c("readr", "readxl", "sf", "spdep", "spatialreg", "pandoc", "rstatix",
                   "tigris", "ggplot2", "dplyr", "viridis", "rgeoda", "gt", "dbscan",
                   "stargazer", "modelsummary", "rstudioapi", "GWmodel", "simex"))
install.packages("ggeffects")
install.packages("brms")

library(readxl)
library(readr)
library(sf)          # spatial data (shapefiles)
library(spdep)       # spatial weights, Moran's I, LISA
library(spatialreg)  # spatial regression models
library(tigris)      # download US county shapefiles
library(ggplot2)     # plotting
library(dplyr)       # data wrangling
library(viridis)     # colorblind-friendly color scales
library(rgeoda)
library(modelsummary)
library(broom)
library(gt)
library(purrr)
library(pandoc)
library(data.table)
library(rstatix)
library(dbscan)
library(stargazer) 
library(rstudioapi)
library(GWmodel)
library(simex)
library(car)
library(mgcv)
library(ggeffects)
library(brms)
library(ggpattern)
library(patchwork) #grid plots

#import datasets
X2025_County_Health_Rankings_Data_v4 <- read_excel("2025 County Health Rankings Data - v4.xlsx", 
                                                   +     sheet = "Select Measure Data", skip = 1)

SAE_Mammography_Prevalence_within_2_Two_Years_Age_40_Estimated_Percent_US_by_County_Female_2021_2023_2 <- 
  read_csv("SAE-Mammography Prevalence within 2 Two Years (Age 40) Estimated Percent US by County,  Female, 2021-2023-2.csv")


county_mammo <- SAE_Mammography_Prevalence_within_2_Two_Years_Age_40_Estimated_Percent_US_by_County_Female_2021_2023_2

county_mammo[["County FIPS"]] <- sprintf("%05s", as.character(county_mammo[["County FIPS"]]))
county_mammo$mammoSE <- (county_mammo$`Upper Confidence Interval` - 
                      county_mammo$`Lower Confidence Interval`) / (2*1.96)

#getting variables
short_county_health <- select(X2025_County_Health_Rankings_Data_v4, c('FIPS', 'State', 'County', 
                                                                      pcp ='Primary Care Physicians Rate', #no SE
                                                                      '% Uninsured',
                                                                      Uninsured_lowCI = '95% CI - Low...115',
                                                                      Uninsured_highCI = '95% CI - High...116',
                                                                      complete_HS = '% Completed High School',
                                                                      HS_lowCI = '95% CI - Low...175',
                                                                      HS_highCI = '95% CI - High...176',
                                                                      broadband = '% Households with Broadband Access',
                                                                      broadband_lowCI = '95% CI - Low...161',
                                                                      broadband_highCI = '95% CI - High...162',
                                                                      income_ratio_80_20 = 'Income Ratio')) # no SE

#conversion of uninsured to insured
short_county_health$insured <- 100 - short_county_health$'% Uninsured'

short_county_health$insured_SE <- (short_county_health$Uninsured_highCI- 
                                     short_county_health$Uninsured_lowCI) / (2*1.96)

short_county_health$HS_SE <- (short_county_health$HS_highCI- 
                                     short_county_health$HS_lowCI) / (2*1.96)

short_county_health$broadband_SE <- (short_county_health$broadband_highCI- 
                                short_county_health$broadband_lowCI) / (2*1.96)

summary(short_county_health)

#joining NCI SAE, County Health Rankings, and facility rate variables
County_health_w_mammo <- left_join(
  county_mammo,
  short_county_health,
 join_by('County FIPS' == 'FIPS')
)

RUCC_wfacil_per_100k <- read_csv("~/fda_census_county_output/RUCC_wfacil_per_100k.csv")


County_mammo_SDOH_RUCC_facilper100k <- left_join(County_health_w_mammo, RUCC_wfacil_per_100k,
                                                 join_by('County FIPS' == 
                                                           'FIPS'))

County_mammo_SDOH_RUCC_facilper100k$FIPS <- County_mammo_SDOH_RUCC_facilper100k$`County FIPS`
County_mammo_SDOH_RUCC_facilper100k$mammo<-County_mammo_SDOH_RUCC_facilper100k$`Estimated Percent`

#
short_County_mammo_SDOH_RUCC_facilper100k <- select(County_mammo_SDOH_RUCC_facilper100k,
                                                    c(FIPS, County, State.x, 
                                                      mammo, mammoSE, pcp, insured, 
                                                      insured_SE,
                                                      complete_HS, 
                                                      HS_SE, broadband, 
                                                      broadband_SE,
                                                      income_ratio_80_20, age40_74,
                                                      SE_40_74,
                                                      facility_count,
                                                      facil_100k, facil_rate_SE,
                                                      RUCC_2023,
                                                      black_race, black_SE,
                                                      hispanic_race, hispanic_SE))

summary(short_County_mammo_SDOH_RUCC_facilper100k)


complete_mammo_SDOH_facil <- short_County_mammo_SDOH_RUCC_facilper100k %>%
      drop_na(mammo, pcp, insured, complete_HS, broadband, income_ratio_80_20,
              facil_100k, RUCC_2023, black_race, hispanic_race)

incomplete_cases <- short_County_mammo_SDOH_RUCC_facilper100k %>%
  filter(if_any(c(mammo, pcp, insured, complete_HS, broadband, income_ratio_80_20,
                  facil_100k, RUCC_2023, black_race, hispanic_race), is.na))


#Descriptive Stats
summary(complete_mammo_SDOH_facil)
summary(incomplete_cases)

summary(complete_mammo_SDOH_facil$mammo)
sd(complete_mammo_SDOH_facil$mammo)

summary(complete_mammo_SDOH_facil$pcp)
sd(complete_mammo_SDOH_facil$pcp, na.rm=TRUE)

summary(complete_mammo_SDOH_facil$insured)
sd(complete_mammo_SDOH_facil$insured, na.rm = TRUE)

summary(complete_mammo_SDOH_facil$complete_HS)
sd(complete_mammo_SDOH_facil$complete_HS, na.rm= TRUE)

summary(complete_mammo_SDOH_facil$broadband)
sd(complete_mammo_SDOH_facil$broadband, na.rm= TRUE)

summary(complete_mammo_SDOH_facil$income_ratio_80_20)
sd(complete_mammo_SDOH_facil$income_ratio_80_20, na.rm= TRUE)

summary(complete_mammo_SDOH_facil$facil_100k)
sd(complete_mammo_SDOH_facil$facil_100k, na.rm= TRUE)

summary(complete_mammo_SDOH_facil$RUCC_2023)
sd(complete_mammo_SDOH_facil$RUCC_2023, na.rm= TRUE)

summary(complete_mammo_SDOH_facil$black_race)
sd(complete_mammo_SDOH_facil$black_race, na.rm= TRUE)

summary(complete_mammo_SDOH_facil$hispanic_race)
sd(complete_mammo_SDOH_facil$hispanic_race, na.rm= TRUE)

ggplot(complete_mammo_SDOH_facil, aes(pcp, mammo)) + geom_point()+
  geom_smooth(method = "lm") +geom_smooth(method = "loess")

ggplot(complete_mammo_SDOH_facil, aes(insured, mammo)) + geom_point()+
  geom_smooth(method = "lm") +geom_smooth(method = "loess")

ggplot(complete_mammo_SDOH_facil, aes(complete_HS, mammo)) + geom_point()+
  geom_smooth(method = "lm") +geom_smooth(method = "loess")

ggplot(complete_mammo_SDOH_facil, aes(broadband, mammo)) + geom_point()+
  geom_smooth(method = "lm") +geom_smooth(method = "loess")

ggplot(complete_mammo_SDOH_facil, aes(income_ratio_80_20, mammo)) + geom_point()+
  geom_smooth(method = "lm") +geom_smooth(method = "loess")

ggplot(complete_mammo_SDOH_facil, aes(facil_100k, mammo)) + geom_point()+
  geom_smooth(method = "lm") +geom_smooth(method = "loess")

ggplot(complete_mammo_SDOH_facil, aes(RUCC_2023, mammo)) + geom_point()+
  geom_smooth(method = "lm") +geom_smooth(method = "loess")

ggplot(complete_mammo_SDOH_facil, aes(black_race, mammo)) + geom_point()+
  geom_smooth(method = "lm") +geom_smooth(method = "loess")

ggplot(complete_mammo_SDOH_facil, aes(hispanic_race, mammo)) + geom_point()+
  geom_smooth(method = "lm") +geom_smooth(method = "loess")


#Correlation Matrix and tests
SDOH_matrix <- complete_mammo_SDOH_facil[, c("mammo", "pcp", "insured", 
                                             "complete_HS", "broadband", 
                                             "income_ratio_80_20", "facil_100k",
                                             "RUCC_2023", "black_race", "hispanic_race")]
covar <- cor(SDOH_matrix, use = "complete.obs")

covar


cor.test(complete_mammo_SDOH_facil$pcp, complete_mammo_SDOH_facil$mammo)
cor.test(complete_mammo_SDOH_facil$insured, complete_mammo_SDOH_facil$mammo)
cor.test(complete_mammo_SDOH_facil$complete_HS, complete_mammo_SDOH_facil$mammo)
cor.test(complete_mammo_SDOH_facil$broadband, complete_mammo_SDOH_facil$mammo)
cor.test(complete_mammo_SDOH_facil$income_ratio_80_20, complete_mammo_SDOH_facil$mammo)
cor.test(complete_mammo_SDOH_facil$facil_100k, complete_mammo_SDOH_facil$mammo)
cor.test(complete_mammo_SDOH_facil$RUCC_2023, complete_mammo_SDOH_facil$mammo)
cor.test(complete_mammo_SDOH_facil$black_race, complete_mammo_SDOH_facil$mammo)
cor.test(complete_mammo_SDOH_facil$hispanic_race, complete_mammo_SDOH_facil$mammo)

#summary table


datasummary(mammo + pcp + insured + complete_HS + broadband + 
              income_ratio_80_20 + facil_100k + RUCC_2023 + black_race +
              hispanic_race~ Median + Mean + SD + Histogram, complete_mammo_SDOH_facil,
            title =  "Descriptive Stats - Complete Cases")

datasummary(mammo + pcp + insured + complete_HS + broadband + 
              income_ratio_80_20 + facil_100k + RUCC_2023 + black_race +
              hispanic_race~ Median + Mean + SD + Histogram, incomplete_cases,
            title =  "Descriptive Stats - Incomplete Cases")


p_matrix <- cor_pmat(SDOH_matrix)
subset(p_matrix < 0.001)
gt(cor_mat(SDOH_matrix))
gt(cor_pmat(SDOH_matrix))

cor_mat(SDOH_matrix) %>% cor_plot(p.mat=NULL, method = "color", insignificant = "cross", label= TRUE,
                                   font.label = list(size = 1), type = "lower", significant.level = 0.001)

cor_pmat(SDOH_matrix) %>% cor_plot(p.mat=NULL, method = "color", insignificant = "cross", label= TRUE,
                         font.label = list(size = 1), type = "lower")


#output = "mammo_descr_summary.docx")


#LMs
OLS_wSE_data <- na.omit(complete_mammo_SDOH_facil)

pcp_mam_simple_lm <- lm(mammo~pcp, complete_mammo_SDOH_facil)
summary(pcp_mam_simple_lm)
AIC(pcp_mam_simple_lm)

mam_multivar_lm_9 <- lm(mammo ~ pcp + insured + complete_HS + broadband + 
                          income_ratio_80_20 + facil_100k + RUCC_2023 + black_race +
                        hispanic_race, complete_mammo_SDOH_facil, x = TRUE)

mam_inv_var <- 1/complete_mammo_SDOH_facil$mammoSE^2

OLS_var_weighted <- lm(mammo ~ pcp + insured + complete_HS + broadband + 
                          income_ratio_80_20 + facil_100k + RUCC_2023 + black_race +
                          hispanic_race, complete_mammo_SDOH_facil, weights = mam_inv_var,
                       x = TRUE)
summary(mam_multivar_lm_9)
AIC(mam_multivar_lm_9)

summary(OLS_var_weighted)
modelsummary(list(mam_multivar_lm_9, OLS_var_weighted))

vif(mam_multivar_lm_9)
vif(OLS_var_weighted)

insured_SE <- OLS_wSE_data$insured_SE
HS_SE <- OLS_wSE_data$HS_SE
broadband_SE <- OLS_wSE_data$broadband_SE
facil_SE <- OLS_wSE_data$facil_rate_SE
black_SE <- OLS_wSE_data$black_SE
hispanic_SE <- OLS_wSE_data$hispanic_SE

basic_OLSwSE <- lm(mammo ~ pcp + insured + complete_HS + broadband + 
                          income_ratio_80_20 + facil_100k + RUCC_2023 + black_race +
                          hispanic_race, OLS_wSE_data, x = TRUE)

weighted_OLSwSE <- lm(mammo ~ pcp + insured + complete_HS + broadband + 
                     income_ratio_80_20 + facil_100k + RUCC_2023 + black_race +
                     hispanic_race, OLS_wSE_data, 
                     weights = 1/OLS_wSE_data$mammoSE^2, x = TRUE)

simex_basicOLS_lin <- simex(basic_OLSwSE, 
                        SIMEXvariable = c("insured", "complete_HS", "broadband", 
                                          "facil_100k", "black_race", "hispanic_race"),
                        measurement.error = cbind(insured_SE, HS_SE, broadband_SE, 
                                                  facil_SE, black_SE, hispanic_SE),
                        B=999,
                        asymptotic = FALSE, fitting.method = "linear")


simex_basicOLS_quad <- simex(basic_OLSwSE, 
                            SIMEXvariable = c("insured", "complete_HS", "broadband", 
                                              "facil_100k", "black_race", "hispanic_race"),
                            measurement.error = cbind(insured_SE, HS_SE, broadband_SE, 
                                                      facil_SE, black_SE, hispanic_SE),
                            B=999,
                            asymptotic = FALSE, fitting.method = "quadratic")

summary(simex_basicOLS_lin)
plot(simex_basicOLS_lin)
summary(simex_basicOLS_quad)
plot(simex_basicOLS_quad)

gam_OLS <- gam(mammo ~ s(pcp) + s(insured) + s(complete_HS) + s(broadband) + 
      s(income_ratio_80_20) + s(facil_100k) + RUCC_2023 + s(black_race) +
      s(hispanic_race), data= complete_mammo_SDOH_facil)

gam_OLS_weighted <- gam(mammo ~ s(pcp) + s(insured) + s(complete_HS) + s(broadband) + 
                 s(income_ratio_80_20) + s(facil_100k) + RUCC_2023 + s(black_race) +
                 s(hispanic_race), data= complete_mammo_SDOH_facil, weights=mam_inv_var)

summary(gam_OLS)
summary(gam_OLS_weighted)



plot(ggeffects::ggpredict(gam_OLS), facets = F)
plot(ggeffects::ggpredict(gam_OLS_weighted), facets = F)


AIC(mam_multivar_lm_9)

AIC(gam_OLS)
AIC(gam_OLS_weighted)

plot(gam_OLS)

gt(tidy(gam_OLS))

simex_weightedOLS <- simex(OLS_var_weighted, 
                        SIMEXvariable = c("insured", "complete_HS", "broadband", 
                                          "facil_100k", "black_race", "hispanic_race"),
                        measurement.error = cbind(insured_SE, HS_SE, broadband_SE, 
                                                  facil_SE, black_SE, hispanic_SE),
                        B=999,
                        asymptotic = FALSE)

summary(simex_weightedOLS)
plot(simex_weightedOLS)

#display table
tidy_simex <- function(x, ...) {
  cf <- summary(x)$coefficients$jackknife
  data.frame(term = rownames(cf), estimate = cf[,1],
             std.error = cf[,2], statistic = cf[,3], p.value = cf[,4])
}
glance_simex <- function(x, ...) data.frame(nobs = nobs(x$model))

as_ms <- function(x) structure(list(tidy = tidy_simex(x), glance = glance_simex(x)),
                               class = "modelsummary_list")

modelsummary(
  list("Naive OLS"        = mam_multivar_lm_9,
       "Weighted OLS"      = OLS_var_weighted,
       "OLS (complete SE)" = basic_OLSwSE,
       "Weighted (complete SE)" = weighted_OLSwSE,
       "SIMEX"             = as_ms(simex_basicOLS_quad),
       "SIMEX (weighted)"  = as_ms(simex_weightedOLS)),
  stars = TRUE,
  statistc = "NULL"
)

#Getting the county polygons, using Albers NA
county_shapes <- counties(state = NULL, cb = TRUE, year = 2024) %>% st_transform(crs=5070)
state_shapes <- states(cb= TRUE, year = 2024) %>% st_transform(crs=5070)

county_shapes <- county_shapes %>%
  mutate(GEOID = sprintf("%05d", as.integer(GEOID)))

Mammo_Phys_Shapes<-left_join(county_shapes, complete_mammo_SDOH_facil, 
                                   join_by("GEOID" =="FIPS"))

class(Mammo_Phys_Shapes)

st_crs(Mammo_Phys_Shapes)

st_is_longlat(Mammo_Phys_Shapes)

#Descriptive Maps chloropleths
ggplot(Mammo_Phys_Shapes) +
  geom_sf(
    aes(fill = mammo),
    color = "white",
    linewidth = 0.05
  ) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey85"
  ) +
  labs(
    title = "County Mammography Screening Rates",
    fill = "Mammography\nrate"
  ) +
  theme_void()

#Transforming map into 50 states only + rearranging Alaska and Hawaii
County_health_shifted <- Mammo_Phys_Shapes %>%
  filter(!STATEFP %in% c("60", "66", "69", "72", "78")) %>%
  shift_geometry(
    geoid_column = "GEOID",
    position = "below") 

plot_mammo_chloro <- ggplot(County_health_shifted) +
  geom_sf(
    aes(fill = mammo, color = "No data"),
    linewidth = 0.03) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey85") +
  scale_color_manual(values = NA, name = "") + 
  labs(fill = "Mammography Rate") +
  theme_void()


ggplot(County_health_shifted) +
  geom_sf(
    aes(fill = pcp, color = "NA"),
    linewidth = 0.03) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey85") +
  scale_color_manual(values = NA, name = "") + 
  labs(fill = "PCP Density") +
  theme_void()


ggplot(County_health_shifted) +
  geom_sf(
    aes(fill = insured, color = "NA"),
    linewidth = 0.03) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey85") +
  scale_color_manual(values = NA, name = "") + 
  labs(fill = "Insured Rate") +
  theme_void()


ggplot(County_health_shifted) +
  geom_sf(
    aes(fill = complete_HS, color = "NA"),
    linewidth = 0.03) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey85") +
  scale_color_manual(values = NA, name = "") + 
  labs(fill = "High School Completion Rate") +
  theme_void()


ggplot(County_health_shifted) +
  geom_sf(
    aes(fill = broadband, color = "NA"),
    linewidth = 0.03) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey85") +
  scale_color_manual(values = NA, name = "") + 
  labs(fill = "Braodband Access") +
  theme_void()


ggplot(County_health_shifted) +
  geom_sf(
    aes(fill = income_ratio_80_20, color = "NA"),
    linewidth = 0.03) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey85") +
  scale_color_manual(values = NA, name = "") + 
  labs(fill = "Income Ratio") +
  theme_void()


ggplot(County_health_shifted) +
  geom_sf(
    aes(fill = RUCC_2023, color = "NA"),
    linewidth = 0.03) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey85") +
  scale_color_manual(values = NA, name = "") + 
  labs(fill = "RUCC score") +
  theme_void()


ggplot(County_health_shifted) +
  geom_sf(
    aes(fill = facil_100k, color = "NA"),
    linewidth = 0.03) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey85") +
  scale_color_manual(values = NA, name = "") + 
  labs(fill = "Facility Rate") +
  theme_void()

ggplot(County_health_shifted) +
  geom_sf(
    aes(fill = black_race, color = "NA"),
    linewidth = 0.03) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey85") +
  scale_color_manual(values = NA, name = "") + 
  labs(fill = "%Black Population") +
  theme_void()

ggplot(County_health_shifted) +
  geom_sf(
    aes(fill = hispanic_race, color = "NA"),
    linewidth = 0.03) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey85") +
  scale_color_manual(values = NA, name = "") + 
  labs(fill = "% Hispanic") +
  theme_void()

#Morans Is for mammo, pcp, insured, incomplete high school

#K based neighbor count weights w inverse weight


complete_mammo_shapes <- right_join(county_shapes, complete_mammo_SDOH_facil, 
                                   join_by("GEOID" =="FIPS"))

class(complete_mammo_shapes)
st_crs(complete_mammo_shapes)

coords <- st_coordinates(st_centroid(st_geometry(complete_mammo_shapes)))
nb_k4 <- knn2nb(knearneigh(coords, k = 4, longlat = FALSE), sym=FALSE)
plot.nb(nb_k4, coords)
summary(nb_k4)
n.comp.nb(nb_k4)

lw_k4 <- nb2listwdist(nb_k4, complete_mammo_shapes, type = "idw", longlat= FALSE, style="B")


#triangle neighbors - unused
tri_nb <- tri2nb(coords)
plot.nb(tri_nb, coords)
summary(tri_nb)

#Sphere of influence weights - unused

soi_nb <- graph2nb(soi.graph(tri_nb, coords))
plot.nb(soi_nb, coords)
summary(soi_nb)


#moran's with K based weights


moran_func <- function(x) {moran.test(complete_mammo_shapes[[x]], 
                       lw_k4, 
                       alternative = "two.sided", 
                       na.action = na.omit, 
                       zero.policy = TRUE) }
  
moran_mc_func <- function(x) {moran.mc(complete_mammo_shapes[[x]], 
                                      lw_k4, 
                                      nsim=999,
                                      alternative = "two.sided", 
                                      na.action = na.omit, 
                                      zero.policy = TRUE) }

mammo_moran <- moran_func("mammo")
mammo_moranMc <- moran_mc_func("mammo")

mammo_moran
mammo_moranMc

pcp_moran <- moran_func("pcp")
pcp_moranMc <- moran_mc_func("pcp")

pcp_moran
pcp_moranMc

insured_moran <- moran_func("insured")
insured_moran_mc <- moran_mc_func("insured")

insured_moran
insured_moran_mc

HS_moran <- moran_func("complete_HS")
HS_moran_mc <- moran_mc_func("complete_HS")

HS_moran
HS_moran_mc

broadband_moran <- moran_func("broadband")
broadband_moran_mc <- moran_mc_func("broadband")

broadband_moran
broadband_moran_mc

income_ratio_moran <- moran_func("income_ratio_80_20")
income_ratio_moran_mc <- moran_mc_func("income_ratio_80_20")

income_ratio_moran
income_ratio_moran_mc

facil_moran <- moran_func("facil_100k")
facil_moran_mc <- moran_mc_func("facil_100k")

facil_moran
facil_moran_mc

RUCC_moran <- moran_func("RUCC_2023")
RUCC_moran_mc <- moran_mc_func("RUCC_2023")

RUCC_moran
RUCC_moran_mc

black_moran <- moran_func("black_race")
black_moran_mc <- moran_mc_func("black_race")

black_moran
black_moran_mc

hisp_moran <- moran_func("hispanic_race")
hisp_moran_mc <- moran_mc_func("hispanic_race")

hisp_moran
hisp_moran_mc

# -------------------------------------

moran_list <- list(
  mammo = mammo_moran,
  PCP          = pcp_moran,
  Insured      = insured_moran,
  HS           = HS_moran,
  Broadband    = broadband_moran,
  IncomeRatio  = income_ratio_moran,
  Facilities   = facil_moran,
  RUCC         = RUCC_moran,
  Black        = black_moran,
  Hispanic     = hisp_moran
)

moran_list |>
  map_dfr(tidy, .id = "variable") |> 
  mutate(p.value = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))) |>
  rename(moran_i = estimate1, expectation = estimate2, variance = estimate3) |>
  gt() |>  tab_header(title ="Univariate Moran") |>
  fmt_number(n_sigfig = 3) 


# residuals spatial tests - moran, RS diagnostics

LM_resid_moran <- lm.morantest(mam_multivar_lm_9, lw_k4, alternative = "two.sided")
LM_wt_resid_moran <- lm.morantest(OLS_var_weighted, lw_k4, alternative = "two.sided")

LM_resid_moran
LM_wt_resid_moran


#bivariate moran's
bv_moran_func<- function(x) {moran_bv(
  complete_mammo_shapes[[x]],
  complete_mammo_shapes$mammo,
  listw = lw_k4,
  nsim = 999
)}

bv_moran_pcpxmammo <- bv_moran_func("pcp")
bv_moran_pcpxmammo

bv_moran_ins_mam <- bv_moran_func("insured")
bv_moran_ins_mam

bv_moran_HS_mam <- bv_moran_func("complete_HS")
bv_moran_HS_mam

bv_moran_broadB_mam <- bv_moran_func("broadband")
bv_moran_broadB_mam

bv_moran_incomeR_mam <- bv_moran_func("income_ratio_80_20")
bv_moran_incomeR_mam

bv_moran_facil_mam <- bv_moran_func("facil_100k")
bv_moran_facil_mam

bv_moran_rucc_mam <- bv_moran_func("RUCC_2023")
bv_moran_rucc_mam

bv_moran_black_mam <- bv_moran_func("black_race")
bv_moran_black_mam

bv_moran_hispanic_mam <- bv_moran_func("hispanic_race")
bv_moran_hispanic_mam

bv_moran_list <-  list("PCP" = bv_moran_pcpxmammo, 
                      "Insured" = bv_moran_ins_mam, 
                      "High school" = bv_moran_HS_mam, 
                      "Income Ratio" = bv_moran_incomeR_mam,
                      "Broadband" = bv_moran_broadB_mam,
                      "Facility rate" = bv_moran_facil_mam, 
                      "RUCC" = bv_moran_rucc_mam, 
                      "Race: black" = bv_moran_black_mam, 
                      "Race: hispanic" = bv_moran_hispanic_mam)
bv_moran_list

bv_moran_list |>
  map_dfr(tidy, .id = "variable") |>
  names()

bv_moran_list |>
  map_dfr(tidy, .id = "variable") |>
  gt() |>
  cols_label(
    statistic = "Estimate",
    bias = "Bias",
    std.error = "Standard Error") |>
  tab_header(title ="Bivariate Moran") |>
  fmt_number(n_sigfig = 3)

  
# LISA MAPS

knn_wt_lisa <- knn_weights(complete_mammo_shapes, k = 4, is_inverse=TRUE)

mammo_lisa = complete_mammo_shapes["mammo"]
lisa_mammo_kn <- local_moran(knn_wt_lisa, mammo_lisa)
lisa_mammo_kn

mam_colors_kn <- lisa_colors(lisa_mammo_kn)
mam_labs_kn <- lisa_labels(lisa_mammo_kn)
mam_clusters_kn <- lisa_clusters(lisa_mammo_kn, cutoff= 0.01)
mam_pvals_kn<- lisa_pvalues(lisa_mammo_kn)

map_colors_mam_kn <- mam_colors_kn[mam_clusters_kn + 1]

lisa_results_kn <- complete_mammo_shapes %>%
  sf::st_drop_geometry() %>%
  transmute(
    GEOID,
    lisa_cluster_id = as.integer(mam_clusters_kn),
    lisa_p_value = mam_pvals_kn
  )

County_health_shifted_lisa <- County_health_shifted %>%
  left_join(lisa_results_kn, by = "GEOID")

County_health_shifted_lisa <- County_health_shifted_lisa %>%
  mutate(
    lisa_cluster = factor(
      lisa_cluster_id,
      levels = seq_along(mam_labs_kn) - 1L,
      labels = mam_labs_kn
    )
  )

plot_mam_LISA <- ggplot(County_health_shifted_lisa) +
  geom_sf(
    aes(fill = lisa_cluster),
    color = "#333333",
    linewidth = 0.03
  ) +
  scale_fill_manual(
    values = setNames(mam_colors_kn, mam_labs_kn),
    drop = TRUE,
    na.value = "white"
  ) +
  labs(
    fill = "LISA cluster"
  ) +
  theme_void() 

#Figure 2

wrap_plots(plot_mammo_chloro, plot_mam_LISA, ncol=1)

#LISA FUNCTION BUILDING


LISA_func <- function(x, y, title) {
  
  lisa_var <- complete_mammo_shapes[x] 
  lisa_kn <- local_moran(knn_wt_lisa, lisa_var)
  
  # Run LISA
  lisa_kn <- local_moran(knn_wt_lisa, lisa_var)

colors_kn <- lisa_colors(lisa_kn)
labs_kn <- lisa_labels(lisa_kn)
clusters_kn <- lisa_clusters(lisa_kn, cutoff= y)
pvals_kn<- lisa_pvalues(lisa_kn)

map_colors <- colors_kn[clusters_kn + 1]

lisa_results_kn_var <- complete_mammo_shapes %>%
  sf::st_drop_geometry() %>%
  transmute(
    GEOID,
    lisa_cluster_id = as.integer(clusters_kn),
    lisa_p_value = pvals_kn
  )

County_health_shifted_lisa <- County_health_shifted %>%
  left_join(lisa_results_kn_var, by = "GEOID")

County_health_shifted_lisa <- County_health_shifted_lisa %>%
  mutate(
    lisa_cluster = factor(
      lisa_cluster_id,
      levels = seq_along(labs_kn) - 1L,
      labels = labs_kn
    )
  )

U_Lisa <- ggplot(County_health_shifted_lisa) +
  geom_sf(
    aes(fill = lisa_cluster),
    color = "#333333",
    linewidth = 0.03
  ) +
  scale_fill_manual(
    values = setNames(colors_kn, labs_kn),
    drop = TRUE,
    na.value = "white"
  ) +
  labs(
    fill = "LISA cluster"
  ) +
  coord_sf(expand = FALSE) +
  theme_void()

  #BV Lisa

BV_Lisa <- local_bimoran(knn_wt_lisa, complete_mammo_shapes[c(x, 'mammo')])

BV_colors_kn <- lisa_colors(BV_Lisa)
BV_labs_kn <- lisa_labels(BV_Lisa)
BV_clusters_kn <- lisa_clusters(BV_Lisa, cutoff= y)
BV_pvals_kn<- lisa_pvalues(BV_Lisa)

cluster_ids_kn <- as.integer(BV_clusters_kn)

map_colors_kn <- BV_colors_kn[cluster_ids_kn + 1L]

lisa_results_BV <- complete_mammo_shapes %>%
  sf::st_drop_geometry() %>%
  transmute(
    GEOID,
    lisa_cluster_id = as.integer(BV_clusters_kn),
    lisa_p_value = BV_pvals_kn
  )

#Join LISA values onto the already-shifted map
County_health_shifted_lisa <- County_health_shifted %>%
  left_join(lisa_results_BV, by = "GEOID")

County_health_shifted_lisa <- County_health_shifted_lisa %>%
  mutate(
    lisa_cluster = factor(
      lisa_cluster_id,
      levels = seq_along(BV_labs_kn) - 1L,
      labels = BV_labs_kn
    )
  )

B_Lisa <- ggplot(County_health_shifted_lisa) +
  geom_sf(
    aes(fill = lisa_cluster),
    color = "#333333",
    linewidth = 0.03
  ) +
  scale_fill_manual(
    values = setNames(BV_colors_kn, BV_labs_kn),
    drop = TRUE,
    na.value = "white"
  ) +
  labs(title = title,
    fill = "LISA cluster"
  ) +
  coord_sf(expand = FALSE) +
  theme_void()

ggsave(paste(x,"_Uni.jpg", sep = ""), plot = U_Lisa, path = "Natl_Plots/Lisas", width = NA, 
height = NA, units = "in", dpi = 500)

ggsave(paste(x,"_Bi.jpg", sep = ""), plot = B_Lisa, path = "Natl_Plots/Lisas", width = NA, 
       height = NA, units = "in", dpi = 500)

ggsave(paste(x,"_U_B_side2side.jpg", sep = ""), plot = U_Lisa + B_Lisa, 
       path ="Natl_Plots/Lisas", width = NA, height = NA, units = "in", dpi = 500)

# alternatively: pdf(file= file.path("Natl_Plots/Lisas",paste(x,"_Uni.pdf", sep = "")), width = 11, height = 8.5)
#print(U_Lisa)
#dev.off()
#pdf(file= file.path("Natl_Plots/Lisas",paste(x,"_Bi.pdf", sep = "")), width = 11, height = 8.5)
#print(B_Lisa)
#dev.off()
#pdf(file= file.path("Natl_Plots/Lisas",paste(x,"_U_B_side2side.pdf", sep = "")), width = 11, height = 8.5)
#print(U_Lisa+ B_Lisa)
#dev.off()

return(B_Lisa)

}


all_plots <- list(
  pcp               = LISA_func("pcp", 0.01, "PCP Density"),
  insured           = LISA_func("insured", 0.01, "Insurance Rate"),
  complete_HS       = LISA_func("complete_HS", 0.01, "High School Completion Rate"),
  broadband         = LISA_func("broadband", 0.01, "Broadband Access"),
  income_ratio      = LISA_func("income_ratio_80_20", 0.01, "Income Ratio"),
  facil_100k        = LISA_func("facil_100k", 0.01, "Facility Rate"),
  black_race        = LISA_func("black_race", 0.01, "Black Population"),
  hispanic_race     = LISA_func("hispanic_race", 0.01, "Hispanic Population"),
  RUCC              = LISA_func("RUCC_2023", 0.01, "RUCC Score")
)

wrap_plots(all_plots[c("pcp", "insured", "complete_HS", "RUCC", "facil_100k", "black_race", "hispanic_race")],
           nrow = 4, ncol=2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#GWR - basic first

#MGWR sendout for the GUI analysis

mgwr_gui <- complete_mammo_shapes %>% select(final_vars)

mgwr_coords <- st_coordinates(st_centroid(st_transform(mgwr_gui, 5070)))

MGWR_sendout <- mgwr_gui %>% mutate(X=mgwr_coords[,"X"],
                                    Y=mgwr_coords[,"Y"]) %>%
  st_drop_geometry()

MGWR_sendout <- MGWR_sendout[complete.cases("MGWR_sendout"),]

write.csv(MGWR_sendout, "mgwr_sendout_5070.csv", row.names=F)

MGWR_session_multi_mc_results <- read_csv("MGWR_session_multi_mc_results.csv")

MGWR_session_multi_mc_results <- MGWR_session_multi_mc_results %>%
  mutate(GEOID = sprintf("%05d", as.integer(GEOID)))

MGWR_results_geo<-left_join(County_health_shifted, MGWR_session_multi_mc_results, 
                             join_by("GEOID" =="GEOID"))


#MGWR bandwidths

# Variable                  Bandwidth      ENP_j   Adj t-val(95%)            DoD_j
#Intercept                   123.000     69.574            3.386            0.469
#pcp                         994.000      8.151            2.742            0.737
#insured                    2954.000      1.086            1.996            0.990
#complete_HS                2954.000      1.131            2.013            0.985
#broadband                  2817.000      1.703            2.180            0.933
#income_ratio_80_20         2826.000      1.716            2.183            0.932
#facil_100k                 2954.000      1.128            2.012            0.985
#RUCC_2023                  2954.000      1.155            2.022            0.982
#black_race                 2635.000      1.222            2.045            0.975
#hispanic_race              2430.000      2.138            2.268            0.905

#mapping MGWR with significant counties
mgwr_choro <- function(var, t_cutoff, title, legend) {
  
  beta_col <- paste0("beta_", var)
  t_col    <- paste0("t_", var)
  
  # Counties meeting t-stat threshold
  sig_counties <- st_simplify(MGWR_results_geo) %>%
    filter(abs(.data[[t_col]]) >= t_cutoff)
  
  ggplot(st_simplify(MGWR_results_geo)) +
    geom_sf(
      aes(fill = .data[[beta_col]], color = "NA"),
      linewidth = 0.03
    ) +
    geom_sf(
      data = (sig_counties),
      fill = NA,
      color = "black",
      linewidth = 0.2) +
    scale_fill_gradient2(
      low  = "blue",
      high = "red",
      na.value= "grey85",
      name = legend)+
    coord_sf(expand = FALSE) + 
    labs(title = title)+
    theme_void() +
    scale_color_manual(values = NA, name = "") +
    guides(
      fill   = guide_colorbar(order = 1),
      colour = guide_legend(order = 2, override.aes = list(colour = "grey85"))
    )
}

# Plot functions
pcp_mgwr <- mgwr_choro("pcp", t_cutoff = 2.742, title = "PCP Density",
           legend = "Coefficient")
insured_mgwr <- mgwr_choro("insured", t_cutoff = 1.996, title = "Insurance Rate",
           legend = "Coefficient")
HS_mgwr <- mgwr_choro("complete_HS", t_cutoff = 2.013, title = "High School Completion Rate",
           legend = "Coefficient")
broadband_mgwr <- mgwr_choro("broadband", t_cutoff = 2.180,title = "Broadband Access",
           legend = "Coefficient")
IR_mgwr <- mgwr_choro("income_ratio_80_20", t_cutoff = 2.183, title = "Income Ratio",
           legend = "Coefficient")
facil_mgwr <- mgwr_choro("facil_100k", t_cutoff = 2.012, title = "Facility Rate",
           legend = "Coefficient")
RUCC_mgwr <- mgwr_choro("RUCC_2023", t_cutoff = 2.022, title = "RUCC",
           legend = "Coefficient")
black_mgwr <- mgwr_choro("black_race", t_cutoff = 2.045, title = "Black Population",
           legend = "Coefficient")
hisp_mgwr <- mgwr_choro("hispanic_race", t_cutoff = 2.268, title = "Hispanic Population",
           legend = "Coefficient")

pcp_mgwr #keep, has significant counties
insured_mgwr #keep, has significant counties
HS_mgwr # no significant counties
broadband_mgwr # no significant counties
IR_mgwr # no significant counties
facil_mgwr # keep, has significant counties
RUCC_mgwr # no significant counties 
black_mgwr # keep, has significant counties
hisp_mgwr # keep, has significant counties

gwr_plots <- list(pcp_mgwr, insured_mgwr, facil_mgwr, black_mgwr, hisp_mgwr)
wrap_plots(gwr_plots, ncol=2)

summary(MGWR_session_multi_mc_results$local_CN)

summary(MGWR_session_multi_mc_results$local_vdp_Intercept)
summary(MGWR_session_multi_mc_results$local_vdp_pcp)
summary(MGWR_session_multi_mc_results$local_vdp_insured)
summary(MGWR_session_multi_mc_results$local_vdp_complete_HS)
summary(MGWR_session_multi_mc_results$local_vdp_broadband)
summary(MGWR_session_multi_mc_results$local_vdp_income_ratio_80_20)
summary(MGWR_session_multi_mc_results$local_vdp_facil_100k)
summary(MGWR_session_multi_mc_results$local_vdp_RUCC_2023)
summary(MGWR_session_multi_mc_results$local_vdp_black_race)
summary(MGWR_session_multi_mc_results$local_vdp_hispanic_race)

# summary(MGWR_session_multi_mc_results$local_vdp_Intercept) %>%
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000000 0.002987 0.009576 0.032403 0.036728 0.568398 

# > summary(MGWR_session_multi_mc_results$local_vdp_Intercept)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000000 0.002987 0.009576 0.032403 0.036728 0.568398 

# > summary(MGWR_session_multi_mc_results$local_vdp_pcp)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000000 0.0008821 0.0040947 0.0068607 0.0099079 0.0524235 

# > summary(MGWR_session_multi_mc_results$local_vdp_insured)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 5.300e-07 1.488e-01 2.919e-01 2.729e-01 3.714e-01 7.371e-01 

# > summary(MGWR_session_multi_mc_results$local_vdp_complete_HS)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 2.020e-06 5.676e-01 6.391e-01 6.206e-01 6.921e-01 8.647e-01 

# > summary(MGWR_session_multi_mc_results$local_vdp_broadband)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 4.420e-06 1.727e-01 3.654e-01 3.239e-01 4.493e-01 6.149e-01 

# > summary(MGWR_session_multi_mc_results$local_vdp_income_ratio_80_20)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 9.600e-07 1.451e-02 3.046e-02 3.652e-02 3.780e-02 3.524e-01 

# > summary(MGWR_session_multi_mc_results$local_vdp_facil_100k)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 9.000e-10 6.748e-04 1.453e-03 2.458e-03 3.175e-03 2.475e-02 

# > summary(MGWR_session_multi_mc_results$local_vdp_RUCC_2023)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 9.000e-08 1.008e-01 2.170e-01 1.775e-01 2.419e-01 3.187e-01 

# > summary(MGWR_session_multi_mc_results$local_vdp_black_race)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 7.000e-08 7.215e-02 9.636e-02 1.779e-01 2.362e-01 8.964e-01 

# > summary(MGWR_session_multi_mc_results$local_vdp_hispanic_race)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000000 0.005773 0.032943 0.152742 0.154296 0.900291 
