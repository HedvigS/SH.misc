source("coloured_SPLOM.R")
library(tidyverse)

df <- diamonds
df <- df[,-c(2, 3, 4)]

p <- coloured_SPLOM(df = df)

ggsave(plot = p, filename = "coloured_SPLOM_diamonds.png", height = 10, width = 10)

p_herring <- coloured_SPLOM(df = df, herringbone = TRUE)

ggsave(plot = p_herring, filename = "coloured_SPLOM_diamonds_herring.png", height = 10, width = 10)
