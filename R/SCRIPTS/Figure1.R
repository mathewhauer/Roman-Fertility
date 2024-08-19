rm(list=ls())

library(tidyverse)

RomanResults <- read_rds("./R/DATA-PROCESSED/RomanResults.rds") %>%
  dplyr::select(Code, iTFR_hat, xTFR_hat, post_mean, Q10, Q90) %>%
  pivot_longer(cols = 2:4,
               names_to = "type",
               values_to = "TFR"
               ) %>%
  mutate(Q10 = if_else(type %in% c("iTFR_hat", "xTFR_hat"), NA, Q10),
         Q90 = if_else(type %in% c("iTFR_hat", "xTFR_hat"), NA, Q90))

ggplot(data = RomanResults, aes(x = Code, y = TFR)) +
         geom_errorbar(
           aes(ymin = Q10, ymax = Q90, color = type),
           position = position_dodge(0.3)
         ) +
  geom_point(aes(color = type), position = position_dodge(0.3), size = 2) +
  theme_bw() +
  ylim(c(0,11.5)) +
  labs(color = "") +
  scale_color_manual(labels = c(expression("iTFR"^"+"), expression("bTFR"), expression("xTFR"^"+")),
                     values = c("#e41a1c", "#377eb8", "#4daf4a")) +
  labs(x = "") +
  theme(legend.position = "bottom")
       
       
       , fill = Code, color = Code,
                                ymin = Q10, ymax = Q90)) +
  geom_point() +
  geom_pointrange() +
  theme_bw() +
  labs(x = "",
       y = "TFR")