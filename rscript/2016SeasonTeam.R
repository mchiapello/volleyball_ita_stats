# Library
library(tidyverse)

# Load data
load("../data/season2016.rda")

# Plot MURO
x %>% group_by(team) %>% mutate(cum = cumsum(MURO_Prf.)) %>% ggplot(aes(x = Giornata, y = cum,
					 group = team, colour = team)) +
	geom_line() + geom_point() +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 7)) +
	ylab("MURO Perfetto") + coord_polar()

# Plot PUNTI
x %>% group_by(team) %>% mutate(cum = cumsum(PUNTI_Tot)) %>% ggplot(aes(x = Giornata, y = cum,
					 group = team, colour = team)) +
	geom_line() + geom_point() +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 7)) +
	ylab("Punti Totali")

# Plot Attacco
x$ATTACCO_Err. <- as.numeric(x$ATTACCO_Err.)
x %>% group_by(team) %>% mutate(ma = mean(ATTACCO_Err.), cumE = cumsum(as.numeric(ATTACCO_Err.))) %>% ggplot(aes(x = Giornata, y = ATTACCO_Err.,
					 group = team, colour = team)) +
	geom_line() + geom_point() + 
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 7)) +
	ylab("Attacchi Errati") + facet_wrap(~team)



ciao <- function(x, cosa, ...){
y <- x %>% group_by(team) %>% summarize(m = mean(cosa))
x %>% group_by(team) %>%
	ggplot(aes(x = Giornata, y = cosa,group = team, colour = team)) +
	geom_line() +
	geom_point() +
	geom_line(aes(x = Giornata, y = ma), colour = "black", size = .3) +
	theme_bw() +
	theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
		  legend.position = "none", axis.ticks.y = element_blank(),
		  panel.grid.major = element_blank(), plot.title = element_text(hjust=0.5, size = 15)) +
geom_text(data = y, aes(x = 2, y = 17, label = paste0("Errori medi: ",
													  round(m,2))), size = 2, colour = "black") +
	ylab("") +
	xlab("") +
	labs(title = "ATTACCHI ERRATI") +
	coord_polar() +
	facet_wrap(~team)
}

ciao(x, "ATTACCO_Err.")
