library(rnaturalearth) # for map
library(ggplot2) # for plotting
library(sf) # for wrangling spatial data
library(data.table) 
# installed package to repel labels on a sf object from github
#devtools::install_github("yutannihilation/ggsflabel")
library(ggsflabel)
theme_set(theme_bw())


reportingDat <- fread("data/reporting_data_analysis.csv")

world <- ne_countries(scale = "medium", returnclass = "sf")
world1 <- st_make_valid(world)

reportingMap <- reportingDat[, .N, by = .(ISO2, v2mebias_ord, EPI)]
reportingMap <- reportingMap[,.SD[which.max(v2mebias_ord)], by = ISO2]
mapDat<- merge(world1, reportingMap, by.x ="iso_a2", by.y = "ISO2", all = T)


ggplot(data = mapDat) + geom_sf(aes(fill = as.factor(v2mebias_ord)), 
                                size = 0.05)+
  scale_fill_manual(name = "Media bias score", 
                       labels = c("0", "1", "2", "3", "4", "No data"),
                       values = c("#F8FFE5", "#FFC43D","hotpink3", "lightblue3", "darkslategray4","white"))+ 
  geom_sf_text_repel(aes(label = EPI), size = 2.25) + 
  xlab("")+ylab("")

ggsave("./figures/Figure1.jpeg", width = 6.5, height = 5, dpi = 300)
