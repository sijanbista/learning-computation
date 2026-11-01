dbh <- 28
height <- 1
Tree_height <- 10.2
#numeric
tree_count <- 150L
species_name <- "Chirpine"
subtropical_species <- TRUE
timber_stress <- 3 + 4i 
forest_byte <- charToRaw("Chirpine")
typeof(Species_name)
is.integer(Timber_stress)
is.character(species_name)
as.character(tree_count)

forest_type <- c("Community", "Private", "Community", "Protected")
forest_factor <- as.factor(forest_type)
class(forest_factor)
levels(forest_factor)


plot_codes_factor <-factor(c("101", "102", "105"))
as.numeric(plot_codes_factor)
as.numeric(as.character(plot_codes_factor))

mixed_vec <-c(28.4, TRUE, "Schima")
mixed_vec


dbh <- c(15.2, 24.5, 12.1, 30.2, 20.0) # DBH in cm
height <- c(10, 20, 25, 40, 20)
dbh > 20

(dbh > 30) & (height > 10)

dbh_cm <- c(100, 20, 50, 20, 30, 40)

calculate_basal_area <- function(dbh_cm){
if(!is.numeric(dbh_cm)) {
stop("Error: DBH must be numeric (measured in cm)!")
}
if(any(dbh_cm <=0, na.rm = TRUE)) {
stop("Error: DBH must be strictly greater than 0!")
}
if(any(dbh_cm >150, na.rm = TRUE)) {
warning("Error: DBH >150 cm detected. DId you record in mm?")
}
ba_m2 <- pi * (dbh_cm^2)/40000
return(ba_m2)
}
calculate_basal_area(20)

calculate_basal_area(-10)

trees <- read.csv("./Data/Raw/unit2_starter_trees.csv")
head(trees)
trees(dbh_cm)
trees$dbh_cm
calculate_cv <- function(dbh_cm) {
  # na.rm = TRUE drops missing data points so the calculation doesn't fail
  mean_val <- mean(dbh_cm, na.rm = TRUE)
  sd_val   <- sd(dbh_cm, na.rm = TRUE)
  
  # Math formula: (Standard Deviation / Mean) * 100
  cv_percentage <- (sd_val / mean_val) * 100
  
  return(cv_percentage)
}
calculate_cv(trees$dbh_cm)
trees$ba_m2 <- calculate_basal_area(trees$dbh_cm) 
head(trees)
calculate_basal_area(trees$dbh_cm)

trees$dbh_cm[trees$dbh_cm>30]
which.max(trees$dbh_cm)
trees$size_class <- cut(
  trees$dbh_cm,
  breaks = c(10, 30, Inf),                  # The numeric boundaries
  labels = c("sapling", "pole trees"),      # The matching names for each bin
  right  = FALSE                            # Includes 10 in sapling, 30 in pole trees
)
table(trees$size_class)

hist(
  trees$dbh_cm, 
  breaks = 12,                                 # Suggested number of bar slots
  col = "forestgreen",                         # Color of the bars
  border = "white",                            # Line color between bars
  main = "Distribution of Tree Diameters",     # Main title
  xlab = "DBH (cm)",                           # X-axis label
  ylab = "Frequency"                           # Y-axis label
)

