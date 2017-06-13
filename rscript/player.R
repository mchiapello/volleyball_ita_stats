#Library
library(tidyverse)
library("gplots")
library(RColorBrewer)
library(ggdendro)

# Load data
load("../data/stat_per_player_tot.rda")

####### Heatmap results
# Create df
final2 <- xx %>% gather(giornata, values, -Atleta, -Squadra)
oo <- final2 %>% filter(grepl("^Punti_", giornata)) %>% print(n = 100)
oo$giornata <- factor(oo$giornata, levels = unique(oo$giornata))
ggplot(data = oo, aes(x = giornata, y = values, group = Atleta, colour = Atleta)) + geom_point() + geom_line() + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 7)) 

oo <- final2 %>% filter(grepl("DiffPunti_", giornata)) %>% print(n = 100)

oo <- oo %>% filter(Atleta %in% c("Sabbi Giulio", "Vettori Luca", "Juantorena Osmany"))
oo$giornata <- factor(oo$giornata, levels = unique(oo$giornata))
ggplot(data = oo, aes(x = giornata, y = values, group = Atleta, colour = Atleta)) + geom_point(alpha = .5) + geom_line(alpha = .3) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 7)) + geom_smooth(method = "loess", se = FALSE)

oo <- oo %>% filter(Atleta %in% c("Sabbi Giulio", "Hernandez Ramos Fernando", "Miskevich Radzivon"))
oo$giornata <- factor(oo$giornata, levels = unique(oo$giornata))
ggplot(data = oo, aes(x = giornata, y = values, group = Atleta, colour = Atleta)) + geom_point(alpha = .5) + geom_line(alpha = .3) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 7)) + geom_smooth(method = "loess", se = FALSE)

oo <- final2 %>% filter(grepl("^Punti_", giornata)) %>% print(n = 100)

oo <- oo %>% filter(Atleta %in% c("Sabbi Giulio", "Hernandez Ramos Fernando", "Miskevich Radzivon"))
oo$giornata <- factor(oo$giornata, levels = unique(oo$giornata))
ggplot(data = oo, aes(x = giornata, y = values, group = Atleta, colour = Atleta)) + geom_point(alpha = .5) + geom_line(alpha = .3) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 7))

# Load data
load("../data/stat_per_player_ricez.rda", verbose = TRUE)

####### Heatmap results
# Create df
final2 <- xx %>% gather(giornata, values, -Atleta, -Squadra)
oo <- final2 %>% filter(grepl("^Ricez_", giornata)) %>% print(n = 100)
oo$giornata <- factor(oo$giornata, levels = unique(oo$giornata))
ggplot(data = oo, aes(x = giornata, y = values, group = Atleta, colour = Atleta)) + geom_point() + geom_line() + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 7)) 

oo <- final2 %>% filter(grepl("^DiffRicez_", giornata)) %>% print(n = 100)
oo <- oo %>% filter(Atleta %in% c("Clevenot Trevor", "Botto Iacopo", "Maruotti Gabriele"))
oo$giornata <- factor(oo$giornata, levels = unique(oo$giornata))
ggplot(data = oo, aes(x = giornata, y = values, group = Atleta, colour = Atleta)) + geom_point(alpha = .5) + geom_line(alpha = .3) + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 7)) + geom_smooth(method = "loess", se = FALSE)

