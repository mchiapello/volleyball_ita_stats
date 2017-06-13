# Library
library(rvest)
library(tidyverse)


######################### season 2016 ############################
## Download data
# Create a list of the teams
list <- data.frame(name = c("Top Volley Latina", "Cucine Lube Civitanova", "Revivre Milano", "Gi Group Monza", "Azimut Modena", "Exprivia Molfetta", "Kioene Padova", "Sir Safety Conad Perugia", "LPR Piacenza", "Bunge Ravenna", "Biosi Indexa Sora", "Diatec Trentino", "Calzedonia Verona", "Tonno Callipo Calabria Vibo Valentia"), code = c("LT", "MC", "MI-POWER", "MIVER", "MO", "BAM", "PD", "BASTIA", "PC", "RAV-ROB", "FR-SORA", "TN-ITAS", "VRI", "VV"))

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

## Change Biosi name
andata$team[andata$team == "Biosì Indexa Sora"] <- "Biosi Indexa Sora"

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

## Change Biosi name
ritorno$team[ritorno$team == "Biosì Indexa Sora"] <- "Biosi Indexa Sora"

#########
## Combine datasets
final <- inner_join(andata, ritorno, by = "team")

## Transform long table
final <- final %>% gather(Giornata, risultato, -team)

## Combine stat and risultati
def <- left_join(x, final, by = c("team", "Giornata"))
def$risultato <- as.numeric(def$risultato)
def$vit <- def$risultato >= 3
def$vit[def$vit == FALSE]  <- 0
def$vit[def$vit == TRUE]  <- 1
def <- def %>% group_by(team) %>% mutate(cum = cumsum(vit))

################## FIX TABLE ######################
# Giornata as factor
def$Giornata <- factor(def$Giornata, levels = c(paste0(rep("Andata_", 13), 1:13), paste0(rep("Ritorno_", 13), 1:13)))
	
def <- tbl_df(transform(def, SetGioc. = as.numeric(SetGioc.),
				  PUNTI_Tot = as.numeric(PUNTI_Tot),
				  PUNTI_Vin = as.numeric(PUNTI_Vin),
				  PUNTI_BP = as.numeric(PUNTI_BP),
				  BATTUTA_Tot = as.numeric(BATTUTA_Tot),
				  BATTUTA_Ace = as.numeric(BATTUTA_Ace),
				  BATTUTA_Err. = as.numeric(BATTUTA_Err.),
				  BATTUTA_AceperSet = as.numeric(gsub(",", ".", def$BATTUTA_AceperSet)),
				  BATTUTA_Effic. = as.numeric(gsub(",", ".", def$BATTUTA_Effic.)),
				  RICEZIONE_Tot = as.numeric(RICEZIONE_Tot),
				  RICEZIONE_Err. = as.numeric(RICEZIONE_Err.),
				  RICEZIONE_Neg. = as.numeric(RICEZIONE_Neg.),
				  RICEZIONE_Prf. = as.numeric(gsub(",", ".", def$RICEZIONE_Prf.)),
				  `RICEZIONE_Prf.%` = as.numeric(gsub(",", ".", def$`RICEZIONE_Prf.%`)),
				  RICEZIONE_Effic. = as.numeric(gsub(",", ".", def$RICEZIONE_Effic.)),
				  ATTACCO_Tot = as.numeric(ATTACCO_Tot),
				  ATTACCO_Err. = as.numeric(ATTACCO_Err.),
				  ATTACCO_Murati = as.numeric(ATTACCO_Murati),
				  ATTACCO_Prf. = as.numeric(ATTACCO_Prf.),
				  `ATTACCO_Prf.%` = as.numeric(gsub(",", ".", def$`ATTACCO_Prf.%`)),
				  ATTACCO_Effic. = as.numeric(gsub(",", ".", def$ATTACCO_Effic.)),
				  MURO_Prf. = as.numeric(MURO_Prf.),
				  MURO_PuntiperSet = as.numeric(gsub(",", ".", def$MURO_PuntiperSet))
				  )
)
				  

## Save final data table
save(def, file = "../data/season2016.rda")

###############################################################################
## STATISTICS per team
###############################################################################
######### Girone di andata
a <- list()

## URL giornata
sq <- 5837:5849

## Download tabelle girone di andata
for (i in 1:13){
	y <- read_html(paste0("http://www.legavolley.it/Rendimento.asp?Tipo=1&Classifica=1.1&AnnoInizio=2016&AnnoFine=2016&Serie=1&Fase=1&Giornata=", sq[i]), encoding = "ISO-8859-1") %>% html_table(fill = TRUE)
	# Extract the data table
	a[[i]] <- y[[5]]
	# Remove unused columns
	a[[i]] <- a[[i]][, c(2, 4, 5)]
	# Rename tables
	names(a[[i]])[2:3] <- paste0(names(a[[i]])[2:3], "_andata_", i)
}

## Combine list into df
andata <- a[[1]]
for (i in 2:13){
	andata <- left_join(andata, a[[i]], by = c("Club"))
}

# Reorder
andata <- andata[, c(1, seq(2, 26, 2), seq(3, 27, 2))]

######## Girone di ritorno
a <- list()

## URL giornata
sq <- 5850:5862

## Download tabelle girone di ritorno
for (i in 1:13){
	y <- read_html(paste0("http://www.legavolley.it/Rendimento.asp?Tipo=1&Classifica=1.1&AnnoInizio=2016&AnnoFine=2016&Serie=1&Fase=1&Giornata=", sq[i]), encoding = "ISO-8859-1") %>% html_table(fill = TRUE)
	# Extract the data table
	a[[i]] <- y[[5]]
	# Remove unused columns
	a[[i]] <- a[[i]][, c(2, 4, 5)]
	# Rename tables
	names(a[[i]])[2:3] <- paste0(names(a[[i]])[2:3], "_ritorno_", i)
	# Remove dot
}

## Combine list into df
ritorno <- a[[1]]
for (i in 2:13){
	ritorno <- left_join(ritorno, a[[i]], by = c("Club"))
}

# Reorder
ritorno <- ritorno[, c(1, seq(2, 26, 2), seq(3, 27, 2))]

# Remove decimals
for (i in 1:nrow(ritorno)){
	for (ii in 15:ncol(ritorno)){
		if (ritorno[i, ii] < 2){
			ritorno[i, ii] <- ritorno[i, ii] * 1000
		} else {
			ritorno[i, ii] <- ritorno[i, ii]
		}
	}
}


###############################################################################
# Combine Andata + Ritorno
x <- inner_join(andata, ritorno, by = "Club")
# Reorder
x <- x[, c(1, 2:14, 28:40, 15:27, 41:53)]

# Cumulative difference
m <- matrix(rep(0, nrow(x) * (ncol(x)-1)), nrow = 14)
for (i in 1:nrow(x)){
	m[i, 1:26] <- c(as.numeric(x[i, 2:27])[1], diff(as.numeric(x[i, 2:27])))
	m[i, 27:52] <- c(as.numeric(x[i, 28:53])[1], diff(as.numeric(x[i, 28:53])))
}

# Rename columns
colnames(m) <- c(paste0("DiffSet_andata_", 1:13), paste0("DiffSet_ritorno_", 1:13),
				 paste0("DiffPunti_andata_", 1:13), paste0("DiffPunti_ritorno_", 1:13))

# Combine datasets
x <- tbl_df(cbind(x, m))

# Save stat per team
save(x, file = "../data/stat_per_team.rda")

###############################################################################
## Stat per atleta TOTALI
###############################################################################
######### Girone di andata
a <- list()

## URL giornata
sq <- 5837:5849

## Download tabelle girone di andata
for (i in 1:13){
	y <- read_html(paste0("http://www.legavolley.it/Rendimento.asp?Tipo=2&Classifica=2.1&AnnoInizio=2016&AnnoFine=2016&Serie=1&Fase=1&Giornata=", sq[i], "&Pos=40"), encoding = "ISO-8859-1") %>% html_table(fill = TRUE)
	# Extract the data table
	a[[i]] <- y[[5]][, c(2, 4, 6:7)]
	# Rename tables
	names(a[[i]])[3:4] <- paste0(c("Set", "Punti"), "_andata_", i)
}

## Combine list into df
andata <- a[[1]]
for (i in 2:13){
	andata <- left_join(andata, a[[i]], by = c("Atleta", "Squadra"))
}

## Reorder
andata <- andata[, c(1, 2, seq(3, 27, 2), seq(4, 28, 2))]

######### Girone di ritorno
a <- list()

## URL giornata
sq <- 5850:5862

## Download tabelle girone di ritorno
for (i in 1:13){
	y <- read_html(paste0("http://www.legavolley.it/Rendimento.asp?Tipo=2&Classifica=2.1&AnnoInizio=2016&AnnoFine=2016&Serie=1&Fase=1&Giornata=", sq[i], "&Pos=40"), encoding = "ISO-8859-1") %>% html_table(fill = TRUE)
	# Extract the data table
	a[[i]] <- y[[5]][, c(2, 4, 6:7)]
	# Rename tables
	names(a[[i]])[3:4] <- paste0(c("Set", "Punti"), "_ritorno_", i)
}

## Combine list into df
ritorno <- a[[1]]
for (i in 2:13){
	ritorno <- left_join(ritorno, a[[i]], by = c("Atleta", "Squadra"))
}

## Reorder
ritorno <- ritorno[, c(1, 2, seq(3, 27, 2), seq(4, 28, 2))]

###############################################################################
# Combine andata + ritorno
x <- inner_join(andata, ritorno, by = c("Atleta", "Squadra"))

# Reorder
x <- x[, c(1, 2, 3:15, 29:41, 16:28, 42:54)]

# Sort
x <- x[order(x[, 54], decreasing = TRUE), ]

# Cumulative difference
m <- matrix(rep(0, nrow(x) * (ncol(x) - 2)), nrow = 27)
for (i in 1:nrow(x)){
	m[i, 1:26] <- c(as.numeric(x[i, 3:28])[1], diff(as.numeric(x[i, 3:28])))
	m[i, 27:52] <- c(as.numeric(x[i, 29:54])[1], diff(as.numeric(x[i, 29:54])))
}

# Rename columns
colnames(m) <- c(paste0("DiffSet_andata_", 1:13), paste0("DiffSet_ritorno_", 1:13),
				 paste0("DiffPunti_andata_", 1:13), paste0("DiffPunti_ritorno_", 1:13))

# Combine datasets
xx <- tbl_df(cbind(x, m))
xx <- xx[1:10, ]

# Save stat per team
save(xx, file = "../data/stat_per_player_tot.rda")

###############################################################################
## Stat per atleta ATTACCO
###############################################################################
######### Girone di andata
a <- list()

## URL giornata
sq <- 5837:5849

## Download tabelle girone di andata
for (i in 1:13){
	y <- read_html(paste0("http://www.legavolley.it/Rendimento.asp?Tipo=2&Classifica=2.3&AnnoInizio=2016&AnnoFine=2016&Serie=1&Fase=1&Giornata=", sq[i], "&Pos=40"), encoding = "ISO-8859-1") %>% html_table(fill = TRUE)
	# Extract the data table
	a[[i]] <- y[[5]][, c(2, 4, 6:7)]
	# Rename tables
	names(a[[i]])[3:4] <- paste0(c("Set", "Punti"), "_andata_", i)
}

## Combine list into df
andata <- a[[1]]
for (i in 2:13){
	andata <- left_join(andata, a[[i]], by = c("Atleta", "Squadra"))
}

## Reorder
andata <- andata[, c(1, 2, seq(3, 27, 2), seq(4, 28, 2))]

######### Girone di ritorno
a <- list()

## URL giornata
sq <- 5850:5862

## Download tabelle girone di ritorno
for (i in 1:13){
	y <- read_html(paste0("http://www.legavolley.it/Rendimento.asp?Tipo=2&Classifica=2.3&AnnoInizio=2016&AnnoFine=2016&Serie=1&Fase=1&Giornata=", sq[i], "&Pos=40"), encoding = "ISO-8859-1") %>% html_table(fill = TRUE)
	# Extract the data table
	a[[i]] <- y[[5]][, c(2, 4, 6:7)]
	# Rename tables
	names(a[[i]])[3:4] <- paste0(c("Set", "Punti"), "_ritorno_", i)
}

## Combine list into df
ritorno <- a[[1]]
for (i in 2:13){
	ritorno <- left_join(ritorno, a[[i]], by = c("Atleta", "Squadra"))
}

## Reorder
ritorno <- ritorno[, c(1, 2, seq(3, 27, 2), seq(4, 28, 2))]

###############################################################################
# Combine andata + ritorno
x <- inner_join(andata, ritorno, by = c("Atleta", "Squadra"))

# Reorder
x <- x[, c(1, 2, 3:15, 29:41, 16:28, 42:54)]

# Sort
x <- x[order(x[, 54], decreasing = TRUE), ]

# Cumulative difference
m <- matrix(rep(0, nrow(x) * (ncol(x) - 2)), nrow = 28)
for (i in 1:nrow(x)){
	m[i, 1:26] <- c(as.numeric(x[i, 3:28])[1], diff(as.numeric(x[i, 3:28])))
	m[i, 27:52] <- c(as.numeric(x[i, 29:54])[1], diff(as.numeric(x[i, 29:54])))
}

# Rename columns
colnames(m) <- c(paste0("DiffSet_andata_", 1:13), paste0("DiffSet_ritorno_", 1:13),
				 paste0("DiffPunti_andata_", 1:13), paste0("DiffPunti_ritorno_", 1:13))

# Combine datasets
xx <- tbl_df(cbind(x, m))
xx <- xx[1:10, ]

# Save stat per team
save(xx, file = "../data/stat_per_player_att.rda")

###############################################################################
## Stat per atleta BATTUTA
###############################################################################
a <- list()

## URL giornata
sq <- 5837:5862
gir <- rep(c("andata", "ritorno"), each = 13)
## Download tabelle girone di andata
for (i in 1:26){
	y <- read_html(paste0("http://www.legavolley.it/Rendimento.asp?Tipo=2&Classifica=2.4&AnnoInizio=2016&AnnoFine=2016&Serie=1&Fase=1&Giornata=", sq[i], "&Pos=40"), encoding = "ISO-8859-1") %>% html_table(fill = TRUE)
	# Extract the data table
	a[[i]] <- y[[5]][, c(2, 4, 6:7)]
	# Rename tables
	names(a[[i]])[3:4] <- paste0(c("Set", "ACE"), "_", gir[i], "_", rep(1:13, 2)[i])
}

## Combine list into df
x <- a[[1]]
for (i in 2:26){
	x <- left_join(x, a[[i]], by = c("Atleta", "Squadra"))
}

## Reorder
x <- x[, c(1, 2, seq(3, 53, 2), seq(4, 54, 2))]

# Sort
x <- x[order(x[, 54], decreasing = TRUE), ]
x <- x[1:10,]

# Cumulative difference
m <- matrix(rep(0, nrow(x) * (ncol(x) - 2)), nrow = 10)
for (i in 1:nrow(x)){
	m[i, 1:26] <- c(as.numeric(x[i, 3:28])[1], diff(as.numeric(x[i, 3:28])))
	m[i, 27:52] <- c(as.numeric(x[i, 29:54])[1], diff(as.numeric(x[i, 29:54])))
}

# Rename columns
colnames(m) <- c(paste0("DiffSet_andata_", 1:13), paste0("DiffSet_ritorno_", 1:13),
				 paste0("DiffAce_andata_", 1:13), paste0("DiffAce_ritorno_", 1:13))

# Combine datasets
xx <- tbl_df(cbind(x, m))

# Save stat per team
save(xx, file = "../data/stat_per_player_ace.rda")

###############################################################################
## Stat per atleta RICEZIONE
###############################################################################
a <- list()

## URL giornata
sq <- 5837:5862
gir <- rep(c("andata", "ritorno"), each = 13)
## Download tabelle girone di andata
for (i in 1:26){
	y <- read_html(paste0("http://www.legavolley.it/Rendimento.asp?Tipo=2&Classifica=2.5&AnnoInizio=2016&AnnoFine=2016&Serie=1&Fase=1&Giornata=", sq[i], "&Pos=40"), encoding = "ISO-8859-1") %>% html_table(fill = TRUE)
	# Extract the data table
	a[[i]] <- y[[5]][, c(2, 4, 6:7)]
	# Rename tables
	names(a[[i]])[3:4] <- paste0(c("Set", "Ricez"), "_", gir[i], "_", rep(1:13, 2)[i])
}

## Combine list into df
x <- a[[1]]
for (i in 2:26){
	x <- left_join(x, a[[i]], by = c("Atleta", "Squadra"))
}

## Reorder
x <- x[, c(1, 2, seq(3, 53, 2), seq(4, 54, 2))]

# Sort
x <- x[order(x[, 54], decreasing = TRUE), ]
x <- x[1:10,]

# Cumulative difference
m <- matrix(rep(0, nrow(x) * (ncol(x) - 2)), nrow = 10)
for (i in 1:nrow(x)){
	m[i, 1:26] <- c(as.numeric(x[i, 3:28])[1], diff(as.numeric(x[i, 3:28])))
	m[i, 27:52] <- c(as.numeric(x[i, 29:54])[1], diff(as.numeric(x[i, 29:54])))
}

# Rename columns
colnames(m) <- c(paste0("DiffSet_andata_", 1:13), paste0("DiffSet_ritorno_", 1:13),
				 paste0("DiffRicez_andata_", 1:13), paste0("DiffRicez_ritorno_", 1:13))

# Combine datasets
xx <- tbl_df(cbind(x, m))

# Save stat per team
save(xx, file = "../data/stat_per_player_ricez.rda")

###############################################################################
## Stat per atleta RICEZIONE
###############################################################################
a <- list()

## URL giornata
sq <- 5837:5862
gir <- rep(c("andata", "ritorno"), each = 13)
## Download tabelle girone di andata
for (i in 1:26){
	y <- read_html(paste0("http://www.legavolley.it/Rendimento.asp?Tipo=2&Classifica=2.6&AnnoInizio=2016&AnnoFine=2016&Serie=1&Fase=1&Giornata=", sq[i], "&Pos=40"), encoding = "ISO-8859-1") %>% html_table(fill = TRUE)
	# Extract the data table
	a[[i]] <- y[[5]][, c(2, 4, 6:7)]
	# Rename tables
	names(a[[i]])[3:4] <- paste0(c("Set", "Muri"), "_", gir[i], "_", rep(1:13, 2)[i])
}

## Combine list into df
x <- a[[1]]
for (i in 2:26){
	x <- left_join(x, a[[i]], by = c("Atleta", "Squadra"))
}

## Reorder
x <- x[, c(1, 2, seq(3, 53, 2), seq(4, 54, 2))]

# Sort
x <- x[order(x[, 54], decreasing = TRUE), ]
x <- x[1:10,]

# Cumulative difference
m <- matrix(rep(0, nrow(x) * (ncol(x) - 2)), nrow = 10)
for (i in 1:nrow(x)){
	m[i, 1:26] <- c(as.numeric(x[i, 3:28])[1], diff(as.numeric(x[i, 3:28])))
	m[i, 27:52] <- c(as.numeric(x[i, 29:54])[1], diff(as.numeric(x[i, 29:54])))
}

# Rename columns
colnames(m) <- c(paste0("DiffSet_andata_", 1:13), paste0("DiffSet_ritorno_", 1:13),
				 paste0("DiffMuri_andata_", 1:13), paste0("DiffMuri_ritorno_", 1:13))

# Combine datasets
xx <- tbl_df(cbind(x, m))

# Save stat per team
save(xx, file = "../data/stat_per_player_muri.rda")


