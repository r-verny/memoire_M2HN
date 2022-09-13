### On lance igraph

library(igraph)

### On importe le fichier
W1 <- data.matrix(read.csv("citation_collab6.csv", header = TRUE, sep = ";", encoding = "UTF-8"))
W1 <- W1[,-1]
head(W1)

#On charge le fichier des attributs
Attrs <- as.data.frame((read.csv("cc_commu.csv", header=TRUE, sep = ";")))
table(Attrs$COMMU)

# On construit le graphe
W1g <- graph.adjacency(W1)
V(W1g)$COMMU <- Attrs$COMMU

colmap = c("orange", "lightblue", "cornflowerblue", "indianred1", "lightgreen")
commu = c("Réflexion, vulgarisation",
          "Actualité, pop culture",
          "Politique",
          "Média",
          "Lifestyle")

par(mar = c(0, 0, 0, 0))

plot(W1g,
     vertex.color=colmap[V(W1g)$COMMU],
     vertex.size = 7,
     vertex.label.cex = 0.6,
     vertex.label.color = "black",
     edge.arrow.size = 0.2,
     edge.curved = FALSE,
     layout = layout_nicely(W1g))

# Légende
legend("topleft",bty = "n",
       legend = commu,
       fill = colmap, border=NA)


# On relève les degrés
sort(degree(W1g))
degree(W1g)
deg <- degree(W1g)

sort(degree(W1g, mode = "in"))
degin <- degree(W1g, mode = "in")

sort(degree(W1g, mode = "out"))
degout <- degree(W1g, mode = "out")

# On relève les mesures de centralité d'intermédiarité
btw <- betweenness(W1g)
sort(betweenness(W1g))
mean(betweenness(W1g))

tab <- matrix(c(deg, degin, degout, btw), ncol=4, byrow=FALSE)
tab

colnames(tab) <- c("degré total", "degré entrant", "degré sortant", "centralité d'intermédiarité")
rownames(tab) <- c("Actu du Futur", "Atelier Missor", "Barbare Civilisé",
                   "BENCH CIGARS", "Boulevard Voltaire", "Bruno Le Salé", "Caljbeut",
                   "Christopher Lannes", "Consul Pazen", "Damien Rieu", "Eric Zemmour",
                   "Greg TOUSSAINT", "Julien Rochedy", "La Cartouche", "LAPIN DU FUTUR",
                   "L'Assimilé", "Le Raptor", "Les Philogynes", "Livre Noir",
                   "Marion Maréchal", "PAPACITO", "Psyhodelik", "Thaïs d'Escufon", "Valek",
                   "VA Plus", "Virginie Vota")

write.csv(tab,"measures.csv", row.names = TRUE)
