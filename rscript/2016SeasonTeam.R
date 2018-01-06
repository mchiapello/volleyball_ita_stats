#Library
library(tidyverse)
library("gplots")
library(RColorBrewer)
library(ggdendro)

# Load data
load("../data/season2016.rda")

####### Heatmap results
# Create df
final2 <- def %>% select(1,25,26) %>% spread(Giornata, risultato)
# Transform it into matrix
rownames(final2) <- final2$team
final2$team <- NULL
final2 <- as.matrix(final2)
mode(final2) <- "numeric"

my_palette <- colorRampPalette(c("#0046B2", "#F0E442" ))(n = 4)

# Plot heatmap
heatmap.2(final2, Colv = NULL, dendrogram = "row",
		  main = "Cluster del numero\ndi set vinti",
          lwid = c(0.7,4),
          lhei = c(0.6,4.1),
          notecol="black",
          cellnote = final2,
          notecex = 1.1,
		  srtCol = 45,
          offsetCol = 0.5,
		  rowsep = 1:14,
		  colsep = 1:26,
		  sepcolor = "white",
		  key = FALSE,
		  trace = "none",
		  margins = c(5,17),
          cexRow = 1.1,
          cexCol = 0.8,
		  col=my_palette)

####### Heatmap punti totali
# Create df
final2 <- def %>% select(1,25,3) %>% spread(Giornata, PUNTI_Tot)

# Transform it into matrix
rownames(final2) <- final2$team
final2$team <- NULL
final2 <- as.matrix(final2)

my_palette <- colorRampPalette(c("#0046B2", "#F0E442" ))(n = 299)

# Plot heatmap
heatmap.2(final2, Colv = NULL, dendrogram = "row",
		  main = "Cluster del numero\ndi totali vinti",
          lwid = c(0.7,4),
          lhei = c(0.6,4.1),
          notecol="black",
          cellnote = final2,
          notecex = 1.1,
		  srtCol = 45,
          offsetCol = 0.5,
		  rowsep = 1:14,
		  colsep = 1:26,
		  sepcolor = "white",
		  key = FALSE,
		  trace = "none",
		  margins = c(5,17),
          cexRow = 1.1,
          cexCol = 0.8,
		  col=my_palette)

####### Heatmap ace
# Create df
final2 <- def %>% select(1,25,7) %>% spread(Giornata, BATTUTA_Ace)


# Transform it into matrix
rownames(final2) <- final2$team
final2$team <- NULL
final2 <- as.matrix(final2)

my_palette <- colorRampPalette(c("#0046B2", "#F0E442" ))(n = 15)

# Plot heatmap
heatmap.2(final2, Colv = NULL, dendrogram = "row",
		  main = "Cluster del numero\ndi battute ace",
          lwid = c(0.7,4),
          lhei = c(0.6,4.1),
          notecol="black",
          cellnote = final2,
          notecex = 1.1,
		  srtCol = 45,
          offsetCol = 0.5,
		  rowsep = 1:14,
		  colsep = 1:26,
		  sepcolor = "white",
		  key = FALSE,
		  trace = "none",
		  margins = c(5,17),
          cexRow = 1.1,
          cexCol = 0.8,
		  col=my_palette)


####### Heatmap error battuta
# Create df
final2 <- def %>% select(1,25,8) %>% spread(Giornata, BATTUTA_Err.)

# Transform it into matrix
rownames(final2) <- final2$team
final2$team <- NULL
final2 <- as.matrix(final2)


my_palette <- colorRampPalette(c("#0046B2", "#F0E442" ))(n = 31)

# Plot heatmap
heatmap.2(final2, Colv = NULL, dendrogram = "row",
		  main = "Cluster del numero\ndi errori in battuta",
          lwid = c(0.7,4),
          lhei = c(0.6,4.1),
          notecol="black",
          cellnote = final2,
          notecex = 1.1,
		  srtCol = 45,
          offsetCol = 0.5,
		  rowsep = 1:14,
		  colsep = 1:26,
		  sepcolor = "white",
		  key = FALSE,
		  trace = "none",
		  margins = c(5,17),
          cexRow = 1.1,
          cexCol = 0.8,
		  col=my_palette)



####### Dendrogram ace
# Create df
final2 <- def %>% select(1,25,7) %>% spread(team, BATTUTA_Ace)


# Transform it into matrix
rownames(final2) <- final2$Giornata
final2$Giornata <- NULL
final2 <- as.matrix(final2)

aa <- hclust(dist(t(final2)))
ggdendrogram(aa, rotate = FALSE, size = 2)
dhc <- as.dendrogram(aa)
# Rectangular lines
ddata <- dendro_data(dhc, type = "rectangle")
ddata$segments$yend[ddata$segments$yend == 0] <- 10  
ddata$labels$y <- 9
ggplot(segment(ddata)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + theme_dendro() +
  geom_text(data=label(ddata), aes(x=x, y=y, label=label, hjust=0), size=3) +
  coord_flip() + 
  scale_y_reverse(expand=c(0.35, 0))

plot(aa, xlab = "", main = "Battute punto")

# Plot MURO
def$MURO_Prf.[is.na(def$MURO_Prf.)] <- 0

def %>% group_by(Giornata, team) %>% mutate(cumM = cumsum(MURO_Prf.)) %>% ggplot(aes(x = Giornata, y = cumM, group = team, colour = team)) +
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

# Plot Attacco
x$ATTACCO_Err. <- as.numeric(x$ATTACCO_Err.)
x %>% group_by(team) %>% mutate(ma = mean(ATTACCO_Err.), cumE = cumsum(as.numeric(ATTACCO_Err.))) %>% ggplot(aes(x = Giornata, y = ATTACCO_Err.,
					 group = team, colour = team)) +
	geom_line() + geom_point() + 
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 7)) +
	ylab("Attacchi Errati") + facet_wrap(~team)


y <- x %>% group_by(team) %>% summarize(m = mean(ATTACCO_Err.))
x %>% group_by(team) %>% mutate(ma = mean(ATTACCO_Err.), cumE = cumsum(as.numeric(ATTACCO_Err.))) %>% ggplot(aes(x = Giornata, y = ATTACCO_Err.,
					 group = team, colour = team)) +
	geom_line() + geom_point() + geom_line(aes(x = Giornata, y = ma), colour = "black", size = .3) +
	theme_bw() +
	theme(axis.text.y = element_blank(), axis.text.x = element_blank(), legend.position = "none", axis.ticks.y = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(hjust=0.5, size = 15)) + geom_text(data = y, aes(x = 2, y = 17, label = paste0("Errori medi: ", round(m,2))), size = 2, colour = "black") +
	ylab("") + xlab("") + labs(title = "ATTACCHI ERRATI") + coord_polar() + facet_wrap(~team)


ggplot(data = def, aes(x = Giornata, y = vit, group = team)) + theme(axis.text.y = element_blank(), axis.text.x = element_blank(), legend.position = "none", axis.ticks.y = element_blank(), panel.grid.major = element_blank(), plot.title = element_text(hjust=0.5, size = 15)) + ylim(-.7,1) + geom_line() + geom_point() + facet_wrap(~team) + coord_polar()
