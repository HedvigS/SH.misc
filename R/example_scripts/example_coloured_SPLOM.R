source("coloured_SPLOM.R")
library(tidyverse)

df <- diamonds
df <- df[,-c(2, 3, 4)]

p <- coloured_SPLOM(df = df)

p_herring <- coloured_SPLOM(df = df, herringbone = TRUE)
p_herring




