for (f in list.files("app/R", pattern = "\\.R$", full.names = TRUE)) source(f)
df <- read.csv("data/miscitation_reports.csv", stringsAsFactors = FALSE,
               na.strings = c("", "NA"), check.names = FALSE)
cat("Rows to restore:", nrow(df), "\n")
status <- push_db_to_github(df, "restore: recover database from local backup")
cat("GitHub:", status, "\n")
