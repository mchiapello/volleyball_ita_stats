# Library
library(rvest)
library(tidyverse)


######################### season 2016 ############################
## Download data
# Create a list of the teams
list <- data.frame(name = c("Latina", "Macerata", "Milano Power", "Milano Vero Volley",
							"Modena", "Molfetta", "Padova", "Perugia", "Piacenza",
							"Ravenna", "Sora", "Trento", "Verona", "Vibo Valentia"),
				   code = c("LT", "MC", "MI-POWER", "MIVER", "MO", "BAM", "PD", "BASTIA",
					 "PC", "RAV-ROB", "FR-SORA", "TN-ITAS", "VRI", "VV"))

# Download Squadre giornata per giornata
y <- list()
for (i in 1:dim(list)[1]){
	y[[i]] <- read_html(paste0("http://www.legavolley.it/Statistiche.asp?TipoStat=1.2&Serie=1&AnnoInizio=2016&Fase=1&Giornata=0&Squadra=", list[i, "code"])) %>% html_table(header = TRUE, fill = TRUE)
	# Extract the data table
	y[[i]] <- y[[i]][[5]]
	# Rename tables
	names(y[[i]])[3:24] <- paste0(names(y[[i]])[3:24], "_", y[[i]][1, 3:24])
	# Remove unused columns
	y[[i]] <- y[[i]][2:27, 1:24]
	# Rename first column
	y[[i]][,1] <- gsub("RSA1 - (\\d*) (\\w+)", "\\2_\\1", y[[1]][,1])
	# Add team name to each dataset
	y[[i]]$team <- list[i, "name"]
}

# Trasform list into dataset
x <- do.call("rbind", y)

# Giornata as factor
x$Giornata <- factor(x$Giornata, levels = c(paste0(rep("Andata_", 13), 1:13), paste0(rep("Ritorno_", 13), 1:13)))

# Save data
save(x, file = "../data/season2016.rda")
