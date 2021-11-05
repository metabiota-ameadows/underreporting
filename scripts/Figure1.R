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
                                size = 0.1)+
  scale_fill_manual(name = "Media bias score", 
                       labels = c("0", "1", "2", "3", "4", "No data"),
                       values = c("rosybrown2", "#725663FF", "#D49464FF", "#5B8FA8FF","#ADB17DFF"))+ 
  geom_sf_text_repel(aes(label = EPI), size = 3, 
                     min.segment.length = unit(0, 'lines'), box.padding = 0.3) + 
  xlab("")+ylab("") + 
  theme(legend.position = c(0.5, 0.08),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "black",
                                         size=0.25, linetype="solid"))+
  
  guides(fill = guide_legend(nrow = 1))



ggsave("./figures/Figure1.tiff", width = 6.5, height = 4, dpi = 300)
