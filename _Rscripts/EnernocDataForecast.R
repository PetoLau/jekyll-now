setwd("C:\\Users\\Peter\\Downloads\\ProjektBD\\enernoc\\csv\\")

library(data.table)

## Data Cleaning -----
files <- list.files(pattern = "*.csv")
DT <- rbindlist(lapply(files, function(x) cbind(fread(x), gsub(".csv", "", x))))
names(DT)[6] <- "ID"
DT[, dttm_utc := as.Date(DT[["dttm_utc"]], "%Y-%m-%d")]
datum <- unique(DT[, dttm_utc])

pocet <- DT[, .N, ID]
# setkey(pocet, "N")
full <- pocet[N == max(N), .(ID)]
DT <- DT[ID %in% full[, ID],]

# Transpose
oomDT <- aggregate(DT[, .(value)], by = DT[, .(ID)], FUN = as.vector, simplify = TRUE)
oomDT <- as.data.table(oomDT[1:dim(full)[1], 2])

plot(ts(colSums(oomDT), freq = 288))