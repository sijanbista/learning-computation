#paprt 1
inv <- read.csv("data/raw/bagmati_raw_inventory.csv")
nrow(inv)
ncol(inv)
names(inv)
str(inv)
inv$DBH_cm
class(inv$DBH_cm)
typeof(inv$DBH_cm)
class(inv$Species)
typeof(inv$Species)
print(names(inv))
 
 #part 2
missing_dbh <- sum(is.na(inv$DBH_cm))
missing_height <- sum(is.na(inv$Height_m))

cat("Missing DBH values:", missing_dbh, "\n")
cat("Missing Height values:", missing_height, "\n")

#min adn max garera garna sakinxa
 
dbh_minus99 <- sum(inv$DBH_cm == -99, na.rm = TRUE)
 
dbh_999 <- sum(inv$DBH_cm == 999, na.rm = TRUE)
 
height_minus99 <- sum(inv$Height_m == -99, na.rm = TRUE)

cat("DBH recorded as -99:", dbh_minus99, "\n")
cat("DBH recorded as 999:", dbh_999, "\n")
cat("Height recorded as -99:", height_minus99, "\n")

non_positive_dbh <- sum(inv$DBH_cm <= 0, na.rm = TRUE)

cat("Non-positive DBH records:", non_positive_dbh, "\n")

inv[inv$Plot_ID == "P17" &
    inv$Tree_ID == "T203", ]

 inv$DBH_cm[inv$Plot_ID == "P17" &
           inv$Tree_ID == "T203"] <- 29.9

inv[inv$Plot_ID == "P17" &
    inv$Tree_ID == "T203", ]

mm_error_count <- sum(inv$DBH_cm > 150 &
                      inv$DBH_cm < 900,
                      na.rm = TRUE)

cat("Trees with millimetre entry errors:",
    mm_error_count, "\n")
inv[inv$DBH_cm > 150 &
    inv$DBH_cm < 900, ]

 
mm_error <- inv$DBH_cm > 150 &
            inv$DBH_cm < 900

inv$DBH_cm[mm_error] <- inv$DBH_cm[mm_error] / 10

mm_error <- which(inv$DBH_cm > 150 & inv$DBH_cm < 900)

inv$DBH_cm[mm_error] <- inv$DBH_cm[mm_error] / 10

mm_error_count <- sum(
  inv$DBH_cm > 150 & inv$DBH_cm < 900,
  na.rm = TRUE
)

cat("Millimetre errors before correction:", mm_error_count, "\n")


remaining_errors <- sum(
  inv$DBH_cm > 150 & inv$DBH_cm < 900,
  na.rm = TRUE
)
#part 3

 inv_clean <- inv[
  !is.na(inv$DBH_cm) &
  inv$DBH_cm > 0 &
  inv$DBH_cm != -99 &
  inv$DBH_cm != 999,
]
dim(inv_clean)
head(inv_clean)

inv_clean$size_class <- cut(
  inv_clean$DBH_cm,
  breaks = c(0, 10, 30, Inf),
  labels = c("Sapling", "Pole", "Sawtimber"),
  right = TRUE
)

table(inv_clean$size_class)
size_frequency <- table(inv_clean$size_class)

print(size_frequency)
dominant_class <- names(which.max(size_frequency))

cat("Dominant diameter class:",
    dominant_class, "\n")

 
champion_row <- which.max(inv_clean$DBH_cm)

cat("Champion tree row number:",
    champion_row, "\n")
 
inv_clean[champion_row,
          c("Plot_ID", "Tree_ID", "Species",
            "DBH_cm", "Height_m")]

sal <- inv_clean[
  inv_clean$Species == "Shorea robusta", ]

sal_count <- nrow(sal)

cat("Total Shorea robusta trees:",
    sal_count, "\n")

sal_mean_dbh <- mean(sal$DBH_cm, na.rm = TRUE)

cat("Mean DBH of Shorea robusta:",
    sal_mean_dbh, "cm\n")


#part 4

calculate_basal_area <- function(dbh_cm) {


  if (any(dbh_cm <= 0, na.rm = TRUE)) {
    stop("Error: DBH must be greater than zero.")
  }


  if (any(dbh_cm > 150, na.rm = TRUE)) {
    warning("Warning: DBH exceeds 150 cm. Check the measurement.")
  }

   
  ba <- pi * (dbh_cm^2) / 40000

  return(ba)
}

inv_clean$ba_m2 <- calculate_basal_area(
  inv_clean$DBH_cm
)
total_ba <- sum(inv_clean$ba_m2, na.rm = TRUE)

cat("Total basal area:",
    round(total_ba, 4), "m2\n")

calc_cv <- function(x) {

  cv <- (sd(x, na.rm = TRUE) /
         mean(x, na.rm = TRUE)) * 100

  return(cv)
}
dbh_cv <- calc_cv(inv_clean$DBH_cm)

cat("Coefficient of variation:",
    round(dbh_cv, 2), "%\n")
exists("calculate_volume_example")

calculate_volume_example <- function(dbh_cm, height_m) {

  if (any(dbh_cm <= 0, na.rm = TRUE)) {
    stop("DBH must be greater than zero.")
  }

  if (any(height_m <= 0, na.rm = TRUE)) {
    stop("Height must be greater than zero.")
  }

  dbh_m <- dbh_cm / 100

  volume <- (pi * dbh_m^2 / 4) * height_m * 0.5

  return(volume)
}

# Tree A
calculate_volume_example(
  dbh_cm = 32.4,
  height_m = 22.1
)

# Tree B
calculate_volume_example(
  dbh_cm = 6.5,
  height_m = 4.2
)

#part 5

if (!dir.exists("Figures")) {
  dir.create("figures")
}

# Prepare records with valid DBH and height
plot_data <- inv_clean[
  !is.na(inv_clean$Height_m) &
  inv_clean$Height_m > 0 &
  inv_clean$Height_m != -99, 
]

# Save diagnostic graph
png("figures/homework_unit2_diagnostics.png",
    width = 1200,
    height = 1000,
    res = 150)

# Arrange graphs in 2 rows and 2 columns
par(mfrow = c(2, 2))

# ----------------------------------
# Graph 1: Histogram of DBH
# ----------------------------------

hist(inv_clean$DBH_cm,
     main = "Distribution of Tree DBH",
     xlab = "DBH (cm)",
     col = "lightgreen",
     border = "black")

abline(v = mean(inv_clean$DBH_cm, na.rm = TRUE),
       col = "red",
       lty = 2,
       lwd = 2)

# ----------------------------------
# Graph 2: Histogram of Height
# ----------------------------------

hist(plot_data$Height_m,
     main = "Distribution of Tree Height",
     xlab = "Height (m)",
     col = "lightblue",
     border = "black")

abline(v = median(plot_data$Height_m, na.rm = TRUE),
       col = "blue",
       lty = 2,
       lwd = 2)

# ----------------------------------
# Graph 3: DBH comparison by stand
# ----------------------------------

boxplot(DBH_cm ~ Stand_ID,
        data = inv_clean,
        main = "DBH Comparison Among Stands",
        xlab = "Stand ID",
        ylab = "DBH (cm)",
        col = "lightyellow")

# ----------------------------------
# Graph 4: DBH vs Height
# ----------------------------------

plot(plot_data$DBH_cm,
     plot_data$Height_m,
     main = "Relationship Between DBH and Height",
     xlab = "DBH (cm)",
     ylab = "Height (m)",
     pch = 19,
     col = "darkgreen")

# Add linear trendline
abline(lm(Height_m ~ DBH_cm, data = plot_data),
       col = "red",
       lwd = 2)

# Reset graphical layout
par(mfrow = c(1, 1))

# Close graphics device
dev.off()