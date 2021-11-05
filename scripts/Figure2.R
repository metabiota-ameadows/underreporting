library(ggplot2) 
library(data.table) 
library(tidyverse)
library(effects)
library(gridExtra)
theme_set(theme_bw())

load("data/final_model_robust.rda")
load("data/final_model.rda")

reportingDat <- fread("data/reporting_data_analysis.csv")
reportingDat[, CFR := CFR *100]
ilink <- family(top_mod)$linkinv # using non-robust model to fetch family because not accessible in robust model 

# use the effect() function to show the effect of the term accounting for the other model variables
# corece to tibbe first because it does a better job at grabbing the data than data.table
EPIeffect <- effect(term = "EPI", mod = top_mod_robust) %>% as_tibble() %>% data.table()
EPIeffect[, fit_resp := ilink(fit)]
EPIeffect[, upr_resp := ilink(fit + (1.96 * se))]
EPIeffect[, lwr_resp := ilink(fit - (1.96 * se))]

p1 <- ggplot() +
  geom_point(data= reportingDat, aes(x = EPI, y = reporting_rate), alpha = 0.5)+
  geom_line(data = EPIeffect, aes(x = EPI, y = fit_resp), color = "navy")+
  geom_ribbon(data = EPIeffect, aes(x = EPI, y = fit_resp, ymin = lwr_resp, ymax = upr_resp), 
              fill = "gray", alpha = 0.25) +
  ylab("Reporting rate") 
#ggsave("./figures/EPI_fig.jpeg", width = 5.5, height = 3.5, dpi = 300)

MediaEffect <- effect(term = "v2mebias_ord", mod = top_mod_robust) %>% as_tibble() %>% data.table()
MediaEffect[, fit_resp := ilink(fit)]
MediaEffect[, upr_resp := ilink(fit + (1.96 * se))]
MediaEffect[, lwr_resp := ilink(fit - (1.96 * se))]

p2 <- ggplot() +
  geom_point(data= reportingDat, aes(x = v2mebias_ord, y = reporting_rate), alpha = 0.5)+
  geom_line(data = MediaEffect, aes(x = v2mebias_ord, y = fit_resp), color = "navy")+
  geom_ribbon(data = MediaEffect, aes(x = v2mebias_ord, y = fit_resp, ymin = lwr_resp, ymax = upr_resp), 
              fill = "gray", alpha = 0.25) + xlab("Media bias") +
  ylab("Reporting rate") 
#ggsave("./figures/Media_fig.jpeg", width = 5.5, height = 3.5, dpi = 300)

CFReffect <- effect(term = "CFR", mod = top_mod_robust) %>% as_tibble() %>% data.table()
CFReffect[, fit_resp := ilink(fit)]
CFReffect[, upr_resp := ilink(fit + (1.96 * se))]
CFReffect[, lwr_resp := ilink(fit - (1.96 * se))]

p3 <- ggplot() +
  geom_point(data= reportingDat, aes(x = CFR, y = reporting_rate), alpha = 0.5)+
  geom_line(data = CFReffect, aes(x = CFR, y = fit_resp), color = "navy")+
  geom_ribbon(data = CFReffect, aes(x = CFR, y = fit_resp, ymin = lwr_resp, ymax = upr_resp), 
              fill = "gray", alpha = 0.25) +
  ylab("Reporting rate") 
#ggsave("./figures/CFR_fig.jpeg", width = 5.5, height = 3.5, dpi = 300)


STIeffect <- effect(term = "STI", mod = top_mod_robust) %>% as_tibble() %>% data.table()
STIeffect[, fit_resp := ilink(fit)]
STIeffect[, upr_resp := ilink(fit + (1.96 * se))]
STIeffect[, lwr_resp := ilink(fit - (1.96 * se))]

p4 <- ggplot() +
  geom_point(data= reportingDat, aes(x = STI, y = reporting_rate), alpha = 0.1)+
  geom_errorbar(data = STIeffect, aes(x = STI, y = fit_resp, ymin = lwr_resp, ymax = upr_resp), 
                width = 0.15, color = "navy") +
  geom_point(data = STIeffect, aes(x = STI, y = fit_resp), color = "navy")+
  ylab("Reporting rate") 
#ggsave("./figures/STI_fig.jpeg", width = 5.5, height = 3.5, dpi = 300)

p5 <- grid.arrange(p1, p2, p3, p4, ncol = 2)
ggsave(p5, filename = "./figures/Figure2.tiff", width = 7.5, height = 7, dpi = 300)
