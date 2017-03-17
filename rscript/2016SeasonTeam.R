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
	ylab("MURO Perfetto")

# Plot PUNTI
x %>% group_by(team) %>% mutate(cum = cumsum(PUNTI_Tot)) %>% ggplot(aes(x = Giornata, y = cum,
					 group = team, colour = team)) +
	geom_line() + geom_point() +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 7)) +
	ylab("Punti Totali")
x$ATTACCO_Err. <- as.numeric(x$ATTACCO_Err.)
# Plot Attacco
x %>% group_by(team) %>% mutate(ma = mean(ATTACCO_Err.), cumE = cumsum(as.numeric(ATTACCO_Err.))) %>% ggplot(aes(x = Giornata, y = ATTACCO_Err.,
					 group = team, colour = team)) +
	geom_line() + geom_point() + 
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 7)) +
	ylab("Attacchi Errati") + 	facet_wrap(~team)


