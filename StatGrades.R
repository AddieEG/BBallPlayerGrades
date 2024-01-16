setwd("/Users/adelaidegilley/Desktop/MBB/Grades")
library(readr)
library(dplyr)

data <- read_csv("Feb25Game.csv")
data <- as.data.frame(data) 
data[data == "null"] <- 0
columns_to_convert <- c("PTS", "PPP", "FG.", "STLBLK", "EFG.", "SSQ", "PPS", "3FG.", "FT.")
for (col in columns_to_convert) {
  data[[col]][!grepl("^\\d+(\\.\\d+)?$", data[[col]])] <- NA
  data[[col]] <- ifelse(data[[col]] == "DNP", "DNP", as.numeric(data[[col]]))
}

weights <- c(
  PTS = 0.1,
  PPP = 0.125,
  FG. = 0.1,
  STLBLK = 0.1,
  EFG. = 0.125,
  SSQ = 0.125,
  PPS = 0.125,
  `3FG.` = 0.1,
  FT. = 0.1
)

pos_weights <- list(
  F = c(
    PTS = 0.2,
    PPP = 0.1,
    FG. = 0.1,
    STLBLK = 0.15,
    EFG. = 0.2,
    SSQ = 0.1,
    PPS = 0.1,
    `3FG.` = 0.1,
    FT. = 0.05
  ),
  G = c(
    PTS = 0.2,
    PPP = 0.1,
    FG. = 0.1,
    STLBLK = 0.1,
    EFG. = 0.2,
    SSQ = 0.1,
    PPS = 0.1,
    `3FG.` = 0.15,
    FT. = 0.05
  ),
  C = c(
    PTS = 0.2,
    PPP = 0.1,
    FG. = 0.15,
    STLBLK = 0.15,
    EFG. = 0.15,
    SSQ = 0.1,
    PPS = 0.1,
    `3FG.` = 0.05,
    FT. = 0.1
  )
)
calculate_grade <- function(value, scale) {
  if (is.na(value) || any(is.na(scale)) || any(is.na(scale[value >= scale]))) {
    return("")
  } else {
    if (value >= scale["A"]) {
      return("A")
    } else if (value >= scale["B"]) {
      return("B")
    } else if (value >= scale["C"]) {
      return("C")
    } else if (value >= scale["D"]) {
      return("D")
    } else {
      return("F")
    }
  }
}

scale <- list(
  PTS = c(A = 20, B = 15, C = 8, D = 3, F = 0),
  PPP = c(A = 1.2, B = 1, C = 0.8, D = 0.4, F = 0),
  FG. = c(A = 0.8, B = 0.6, C = 0.4, D = 0.2, F = 0),
  STLBLK = c(A = 5, B = 4, C = 2, D = 1, F = 0),
  EFG. = c(A = 0.7, B = 0.6, C = 0.5, D = 0.2, F = 0),
  SSQ = c(A = 1.5, B = 1.2, C = 0.9, D = 0.6, F = 0),
  PPS = c(A = 1.5, B = 1.2, C = 0.9, D = 0.6, F = 0),
  `3FG.` = c(A = 0.6, B = 0.5, C = 0.3, D = 0.2, F = 0),
  FT. = c(A = 0.8, B = 0.6, C = 0.4, D = 0.2, F = 0)
)

for (player in 1:nrow(data)) {
  position <- data$POS[player]
  for (stat in names(weights)) {
    if (!is.na(data[[stat]][player])) {
      weight <- weights[stat]
      pos_weight <- pos_weights[[position]][stat]
      value <- data[[stat]][player]
      stat_scale <- scale[[stat]]
      grade <- calculate_grade(value, stat_scale)
      weighted_grade <- round(weight * pos_weight, 2)
      data[[paste0(stat, "_GRADE")]][player] <- paste0(grade)
    }
  }
}


#install.packages("openxlsx")
library(openxlsx)
write.xlsx(data,file = '/Users/adelaidegilley/Desktop/MBB/Grades/new_file4.xlsx')

