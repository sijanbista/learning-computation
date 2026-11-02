bagmati_data <- read.csv("./Data/Raw/bagmati_raw_inventory.csv")
str(bagmati_data)
names(bagmati_data)

# Assignment 1 making major AGB function 
# Above Ground Biomass = Stem Volume * Wood Specific Gravity 
# FRTC, 2025 givest the best fitted volume model, Stem Biomass and Wood specific gravity which is;
# B = a*(DBH^2 * Height * rho)^b 
# B = 0.0567 * (DBH^2 * Height * 626.8 kg/m3 )^0.98 #Oven Dry Density of stem and Branches with bark
#Then convet this AGB to the per ha and convert to mg
# AGB/ha = Sum(Bi)/1000 *10000/Plot Area (A)
# Convert biomass into carbon stock
# Total AG Carbon Stock = AGB/ha * Carbon fraction of dry matter (usually taken 0.47 for Shorea robusta)




# Volume function creation
cal_vol <- function(D, H) {
  my_vol <- D^2 * H
  return(my_vol)
}

# Biomass function creation  
cal_bm <- function(my_vol, WD, a, b) {
  AGB <- a * (my_vol * WD)^b
  return(AGB)
}

# Creation Major funtion 
calculate_forest_inventory <- function(data, dbh_col, height_col, wd_col, a, b, plot_area_m2 = 500, cf = 0.47) {
  
  # Aafno dataset bata tanne 
  D  <- data[[dbh_col]]
  H  <- data[[height_col]]
  WD <- data[[wd_col]]
  
  # Calls the cal_vol function
  tree_vol <- cal_vol(D, H)
  
  # Calls the cal_bm funtion
  tree_agb <- cal_bm(tree_vol, WD, a, b)
  
  # change to Hectare and calculation of average carbon stock per ha
  agb_ha <- (sum(tree_agb, na.rm = TRUE) / 1000) * (10000 / plot_area_m2)
  agc_ha <- agb_ha * cf
  
  # Return results  
  return(list(
    AGB_Mg_ha = round(agb_ha, 3),
    AGC_Mg_ha = round(agc_ha, 3),
    Updated_Data = transform(data, Vol_m3 = tree_vol, AGB_kg = tree_agb)
  ))
}

#####

# 1. Filter the dataset for Shorea robusta only
shorea_data <- subset(bagmati_data, Species == "Shorea robusta")

# 2. Run the major function on ONLY the Shorea robusta data
shorea_results <- calculate_forest_inventory(
  data         = shorea_data, 
  dbh_col      = "DBH_cm", 
  height_col   = "Height_m", 
  wd_col       = "Wood_density", 
  a            = 0.0567,          # Shorea robusta constant a
  b            = 0.98,            # Shorea robusta constant b
  plot_area_m2 = 500, 
  cf           = 0.47
)

# 3. View the per-hectare metrics for Shorea robusta
print(shorea_results$AGB_Mg_ha)
print(shorea_results$AGC_Mg_ha)
 