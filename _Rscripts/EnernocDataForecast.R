setwd("C:\\Users\\Peter\\Downloads\\ProjektBD\\enernoc\\csv\\")

library(data.table)
library(lubridate)

## Data Cleaning -----
files <- list.files(pattern = "*.csv")
DT <- rbindlist(lapply(files, function(x) cbind(fread(x), gsub(".csv", "", x))))
str(DT)
setnames(DT, c("dttm_utc", "V2"), c("date", "ID"))

## Meta data ----
setwd("C:\\Users\\Peter\\Downloads\\ProjektBD\\enernoc\\")
meta_data <- fread("meta\\all_sites.csv")
str(meta_data)

# nice features to explore
meta_data[, .N, by = .(INDUSTRY, SUB_INDUSTRY)]
# Table of frequencies of industries and subindustries
library(ggplot2)
library(gridExtra)
# --- Graph 1 : If you want ONLY the table in your image :
# First I create an empty graph with absolutely nothing :
qplot(1:5, 1:5, geom = "blank") + theme_bw() + theme(line = element_blank(), text = element_blank()) +
  annotation_custom(grob = tableGrob(meta_data[, .N, by = .(INDUSTRY, SUB_INDUSTRY)]))

# Plot industries in map of USA
library(ggmap)
map <- get_map(location = "USA", zoom = 4) # c(lon = -125, lat = 22)
# ?get_map
mapPoints <- ggmap(map) + geom_point(aes(x = LNG, y = LAT, size = 3, color = INDUSTRY), data = meta_data, alpha = .5) +
  theme(axis.title.x = element_text(colour = "white"), axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"), axis.text.y = element_text(colour = "white"),
        legend.text = element_text(colour="black", size = 12, face = "bold"), legend.title = element_text(colour="black", size=14, face="bold"))
mapPoints

# Histogram of sqft -> square meter
set(meta_data, j = "SQ_FT", value = meta_data[["SQ_FT"]] * 0.09290304)
setnames(meta_data, "SQ_FT", "SQ_M")
# par(bg="white", mar=c(5, 4, 3, 2), oma=c(0,0,0,0), xpd=FALSE,
#     xaxs="r", yaxs="r", mgp=c(2.8,0.3,0.5), col.lab="black", col.axis="black",
#     col.main="black", cex.main=1.1, cex.axis=1.1, cex.lab=1.1,
#     font.main=7, font.lab=7, font.axis=7, lend=1, tck=0)
# hist(meta_data$SQ_M, breaks=25, col = "dodgerblue2", border = "grey90", xlab = "SQ_M", main = "Histogram of SQ_M for all consumers")
ggplot(meta_data, aes(meta_data$SQ_M)) +
  geom_histogram(bins = 32,
                 col = "grey45",
                 fill = "dodgerblue2", 
                 alpha = .75) +
  labs(title = "Histogram of SQ_M for all consumers") +
  labs(x = "SQ_M", y = "Frequency") +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"))


# By industries
ggplot(meta_data, aes(SQ_M, colour = INDUSTRY, fill = INDUSTRY)) + 
  geom_density(alpha=0.55) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))


# prepare dataset to merge with DT
meta_data[, ':='(TIME_ZONE = NULL, TZ_OFFSET = NULL)]
setnames(meta_data, "SITE_ID", "ID")
meta_data[, ID := as.character(meta_data[["ID"]])]

ID_stats <- DT[, .(Mean = mean(value),
                   Median = median(value),
                   Sum = sum(value)), .(ID)]

data_m <- merge(ID_stats, meta_data, by = "ID")
sub_sum <- data_m[, .(mean(Mean)), .(SUB_INDUSTRY)]
setkey(sub_sum, V1)

end_point <- 0.5 + nrow(sub_sum) + nrow(sub_sum) - 1
par(bg="white", mar = c(7, 4, 2, 2) + 0.2, oma=c(0,0,0,0), xpd=FALSE,
    xaxs="r", yaxs="r", mgp=c(2.8,0.3,0.5), col.lab="black", col.axis="black",
    col.main="black", cex.main=1.2, cex.axis=1.1, cex.lab=1.1,
    font.main=7, font.lab=7, font.axis=7, lend=1, tck=0)
barplot(sub_sum[,V1], names.arg = "", las = 1, cex.names = 0.8, ylab = "Mean Load (kW)", xlab = "",
        col = "dodgerblue2", space = 1, border = "grey90", main = "Mean load by subindustries")
text(seq(1.5, end_point, by = 2), par("usr")[3]-0.25, 
     srt = 60, adj = 1, xpd = TRUE,
     labels = sub_sum[,SUB_INDUSTRY], cex = 0.70, font = 7)

# Regression lines SQ_M vs Median Load
# library(MASS)
# par(bg="white", mar=c(5, 4, 3, 2), oma=c(0,0,0,0), xpd=FALSE,
#     xaxs="r", yaxs="r", mgp=c(2.8,0.3,0.5), col.lab="black", col.axis="black",
#     col.main="black", cex.main=1.1, cex.axis=1.1, cex.lab=1.1,
#     font.main=7, font.lab=7, font.axis=7, lend=1, tck=0)
# colW <- rainbow(4)[as.factor(data_m[,INDUSTRY])]
# pchW <- c(15,16,17,18)[as.factor(data_m[,INDUSTRY])]
# plot(data_m[,.(SQ_M, Median)], pch = pchW, col = colW, cex = 1.2, ylab = "Median Load (kW)")
# legend(113000, 370, unique(data_m[, INDUSTRY]), col = unique(colW), pch = unique(pchW),
#        cex=1.05, bty="n", pt.cex=1.5, text.font=7, ncol = 1)
# abline(rlm(Median ~ SQ_M, data = data_m[,.(SQ_M, Median)]), col = "yellow1", lwd = 2)
# abline(lm(Median ~ SQ_M, data = data_m[,.(SQ_M, Median)]), col = "salmon", lwd = 2)
# text(100000, 155, paste("LM"), font = 7)
# text(130000, 100, paste("Robust LM"), font = 7)

# Regression lines SQ_M vs Median Load
ggplot(data_m[, .(SQ_M, Median, INDUSTRY)], aes(x = SQ_M, y = Median)) +
  geom_point(aes(colour = INDUSTRY, shape = INDUSTRY), size = 4, alpha = 0.8) +
  geom_smooth(method = lm, color = "yellow1", se = TRUE) +
  scale_shape_manual(values = c(15,16,17,18)) +
  scale_color_manual( values=c("salmon", "dodgerblue2", "springgreen3", "plum3")) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12,face="bold"))

###########
# Date and times
DT[, date_time := ymd_hms(DT[["date"]])]
DT[, date := as.Date(DT[["date"]], "%Y-%m-%d")]
DT[, ':='(timestamp = NULL, estimated = NULL, anomaly = NULL)]
str(DT)

# Extract ID's with an whole length
count_ID <- DT[, .N, ID]
full <- count_ID[N == max(N), .(ID)]
DT <- DT[ID %in% full[, ID]]

# Extract date's with an all measurements during the day (288)
num_date <- DT[ID == 100, .N, .(date)]
unique(num_date[, N])
DT <- DT[!date %in% num_date[c(1,367), date]]
unique(DT[,ID])
ggplot(DT[ID == 99, .(value, date)], aes(date, value)) + geom_line()

# aggregate consumption to 48 measurments per day (every half hour)
DT_48 <- DT[, .(value = sum(value), date, ID, date_time), by = (seq(nrow(DT)) - 1) %/% 6]
DT_48 <- DT_48[seq(1, nrow(DT_48), by = 6)]
DT_48[, seq := NULL]

## Some examples of consumers
# Primary/Secondary School
p1 <- ggplot(DT_48[ID == 213, .(value, date)], aes(date, value)) + geom_line() +
  annotate("text", x = as.Date("2012-02-11"), y = 120, label = "ID 213", size = 6) + 
  annotate("rect", xmin = as.Date("2012-01-20"), xmax = as.Date("2012-03-01"), ymin = 112, ymax = 128, alpha = 0.25)
# Grocer/Market
p2 <- ggplot(DT_48[ID == 401, .(value, date)], aes(date, value)) + geom_line() +
  annotate("text", x = as.Date("2012-02-11"), y = 120, label = "ID 401", size = 6) + 
  annotate("rect", xmin = as.Date("2012-01-20"), xmax = as.Date("2012-03-01"), ymin = 112, ymax = 128, alpha = 0.25)
# Corporate Office
p3 <- ggplot(DT_48[ID == 9, .(value, date)], aes(date, value)) + geom_line()  +
  annotate("text", x = as.Date("2012-02-11"), y = 400, label = "ID 9", size = 6) + 
  annotate("rect", xmin = as.Date("2012-01-20"), xmax = as.Date("2012-03-01"), ymin = 380, ymax = 412, alpha = 0.25)
# Manufacturing
p4 <- ggplot(DT_48[ID == 832, .(value, date)], aes(date, value)) + geom_line() +
  annotate("text", x = as.Date("2012-02-11"), y = 0, label = "ID 832", size = 6) + 
  annotate("rect", xmin = as.Date("2012-01-20"), xmax = as.Date("2012-03-01"), ymin = -25, ymax = 25, alpha = 0.25)

multiplot(p1, p2, p3, p4, cols = 1)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# ggplot(DT_48[ID %in% c(213, 9, 401, 832)], aes(x=date, y=value, colour=ID, group=ID)) + geom_line()


# Aggregate consumption of all consumers (43)
DT_agg <- as.data.table(aggregate(DT_48[, .(value)], by = DT_48[, .(date_time)], FUN = sum, simplify = TRUE))
ggplot(DT_agg, aes(date_time, value)) + geom_line()

# Median daily profile of aggregate consumption with MAD (deviation)
Med_Mad <- DT_agg[, .(Med = median(value), Mad = mad(value)), by = (seq(nrow(DT_agg)) - 1) %% 48]
ggplot(Med_Mad, aes(x = seq, Med)) + 
  geom_line(size = 0.9) +
  geom_ribbon(data = Med_Mad, aes(ymin = Med - Mad, ymax = Med + Mad),
              fill = "firebrick2", alpha = 0.3) +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(title = "Median daily profile +- Deviation (MAD)") +
  labs(x = "Time", y = "Load (kW)")

# Median weekly profile of aggregate consumption
# DT_agg[, weekday := weekdays(date_time)]
Med_Mad_Week <- DT_agg[, .(Med = median(value), Mad = mad(value)), by = (seq(nrow(DT_agg)) - 1) %% (48*7)]
ggplot(Med_Mad_Week, aes(x = seq, Med)) + 
  geom_line(size = 0.9) + 
  geom_ribbon(data = Med_Mad_Week, aes(ymin = Med - Mad, ymax = Med + Mad),
              fill = "firebrick2", alpha = 0.3) +
  geom_vline(xintercept = c(47, 47+(48*3), 47+(48*4), 47+(48*5)), linetype = 2, size = 1) +
  theme(title = element_text(size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")) +
  labs(title = "Median weekly profile +- Deviation (MAD)") +
  labs(x = "Time", y = "Load (kW)")

# Transpose
# oomDT <- aggregate(DT[, .(value)], by = DT[, .(ID)], FUN = as.vector, simplify = TRUE)
# oomDT <- as.data.table(oomDT[1:dim(full)[1], 2])
# 
# plot(ts(colSums(oomDT), freq = 288))