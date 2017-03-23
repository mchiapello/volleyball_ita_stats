# Library
library(rvest)
library(tidyverse)


######################### season 2016 ############################
## Download data
# Create a list of the teams
list <- data.frame(name = c("Top Volley Latina", "Cucine Lube Civitanova", "Revivre Milano", "Gi Group Monza", "Azimut Modena", "Exprivia Molfetta", "Kioene Padova", "Sir Safety Conad Perugia", "LPR Piacenza", "Bunge Ravenna", "Biosì Indexa Sora", "Diatec Trentino", "Calzedonia Verona", "Tonno Callipo Calabria Vibo Valentia"), code = c("LT", "MC", "MI-POWER", "MIVER", "MO", "BAM", "PD", "BASTIA", "PC", "RAV-ROB", "FR-SORA", "TN-ITAS", "VRI", "VV"))

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

####################### Downlod match results #####################

######### Girone di andata
a <- list()

## URL giornata
sq <- 5837:5849

## Download tabelle girone di andata
for (i in 1:13){
	y <- read_html(paste0("http://www.legavolley.it/Risultati.asp?Anno=2016&IdCampionato=648&IdFase=1&IdGiornata=", sq[i]), encoding = "ISO-8859-1") %>% html_table(fill = TRUE)
	a[[i]] <- y[[6]][4:17,1:2]
	names(a[[i]]) <- c("squadre", "risultati")
	a[[i]]$risultati <- unlist(strsplit(a[[i]]$risultati, "-"))[rep(seq(1,2),7) + rep(seq(0,24,4),each=2)]
}

## Combine list into df
andata <- a[[1]]
for (i in 2:13){
	andata <- left_join(andata, a[[i]], by = "squadre")
}

## Rename colonne
names(andata) <- c("team", paste0(rep("Andata_", 13), 1:13))

######## Girone di ritorno
a <- list()

## URL giornata
sq <- 5850:5862

## Download tabelle girone di andata
for (i in 1:13){
	y <- read_html(paste0("http://www.legavolley.it/Risultati.asp?Anno=2016&IdCampionato=648&IdFase=2&IdGiornata=", sq[i]), encoding = "ISO-8859-1") %>% html_table(fill = TRUE)
	a[[i]] <- y[[6]][4:17,1:2]
	names(a[[i]]) <- c("squadre", "risultati")
	a[[i]]$risultati <- unlist(strsplit(a[[i]]$risultati, "-"))[rep(seq(1,2),7) + rep(seq(0,24,4),each=2)]
}

## Combine list into df
ritorno <- a[[1]]
for (i in 2:13){
	ritorno <- left_join(ritorno, a[[i]], by = "squadre")
}

## Rename colonne
names(ritorno) <- c("team", paste0(rep("Ritorno_", 13), 1:13))

#########
## Combine datasets
final <- inner_join(andata, ritorno, by = "team")

## Transform long table
final <- final %>% gather(Giornata, risultato, -team)

## Combine stat and risultati
def <- left_join(x, final, by = c("team", "Giornata"))

## Save final data table
save(def, file = "../data/season2016.rda")
