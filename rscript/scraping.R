# Library
library(rvest)
library(gganimate)
library(tidyverse)


######################### season 2016 ############################
## Download data
# Create a list of the teams
list <- c("LT", "MC", "MI-POWER", "MIVER", "MO", "BAM", "PD", "BASTIA", "PC",
		  "RAV-ROB", "FR-SORA", "TN-ITAS", "VRI", "VV")

# Download Squadre giornata per giornata
y <- list()
for (i in 1:length(list)){
	y[[i]] <- read_html(paste0("http://www.legavolley.it/Statistiche.asp?TipoStat=1.2&Serie=1&AnnoInizio=2016&Fase=1&Giornata=0&Squadra=", list[i])) %>% html_table(header = TRUE, fill = TRUE)
}

# Extract the data table
for (i in 1:length(list)){
y[[i]] <- y[[i]][[5]]
}

# Rename tables
for (i in 1:length(list)){
	names(y[[i]])[3:24] <- paste0(names(y[[i]])[3:24], "_", y[[i]][1, 3:24])
}

# Remove unused columns
for (i in 1:length(list)){
	y[[i]] <- y[[i]][2:27, 1:24]
}

for (i in 1:length(list)){
	y[[i]][,1] <- gsub("RSA1 - (\\d*) (\\w+)", "\\2_\\1", y[[1]][,1])
}

xx$group  <- "a"
xx$cum <- cumsum(as.numeric(xx$X3))
xx$num <- 1:26

ggplot(data = xx, aes(x = num, y = cum, group = group, frame = cum)) +
	geom_line() + geom_point()

