# R Code for updating UserNetR2 data package

library(intergraph)
library(igraph)
load("data/Bali.rda")
iBali <- asIgraph(Bali)
summary(iBali)

Bali2 <- iBali
save(Bali2, file="data/Bali2.rda")
