# Detta projekt skapar demografi PPT:er för samtliga av Sveriges kommuner

# Installerar paket som saknas ----
paket <- c("tidyverse",
           "jsonlite",
           "officer",
           "mschart",
           "openxlsx")

invisible(lapply(paket, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}))

# Sätter parametrar för databas samt systemtider för att skapa en logg över hur lång tid körningen tar
databasdatum <- today()
start_time <- Sys.time()
ptm <- proc.time() 
dagens_datum <- today()

# sätt till 290 för att skapa för samtliga kommuner
antal_rapporter <- 2

# Laddar filer och kör program ----

source("R/inspect_ppt_structure.R")
source("R/get_data.R")
source("R/text_functions.R")
source("R/ppt_functions.R")
source("R/create_ppts.R")

# ---- Skriver logg -----
elapsed_time <- proc.time() - ptm
end_time <- Sys.time()
print(paste("User time: ", elapsed_time[1], "seconds"))
print(paste("System time: ", elapsed_time[2], "seconds"))
print(paste("Elapsed time: ", elapsed_time[3], "seconds"))

run_data <- data.frame(
  Date = Sys.Date(),
  Start_time = format(start_time, "%Y-%m-%d %H:%M:%S"),
  End_time = format(end_time, "%Y-%m-%d %H:%M:%S"),
  UserTime = elapsed_time[1],
  SystemTime = elapsed_time[2],
  ElapsedTime = elapsed_time[3]
)

file_name <- "output_timelog/timelog.xlsx"

if (file.exists(file_name)) {
  existing_data <- read.xlsx(file_name)
  updated_data <- rbind(existing_data, run_data)
} else {
  updated_data <- run_data
}

write.xlsx(updated_data, file_name, overwrite = TRUE)
