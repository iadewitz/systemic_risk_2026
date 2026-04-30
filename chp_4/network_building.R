library(vars)
library(astsa)
library(forecast)

# Costruzione reti --------------------------------------------------------























# Analisi descrittive -----------------------------------------------------
library(igraph)
net_igraph <- lapply(adj_1, function(mat) {
     graph_from_adjacency_matrix(
          adjmatrix = mat,
          mode = "directed", # La rete è diretta
          weighted = NULL, # Gli archi non sono pesati
          diag = FALSE
     )
}) # Self-loops non definiti

# A livello di rete
x_label_years <- c(as.character(seq(2008, 2022, by = 2)))
x_at_years <- NULL
for (i in 1:length(x_label_years)) {
     x_at_years[i] <- min(which(floor(stoxx600_week_cut / 10000) == as.numeric(x_label_years[i])))
}

par(mfrow = c(3, 1))
par(mar = c(2.5, 4, 3, 2))
# Densità
density <- lapply(net_igraph, function(graph) graph.density(graph))
density <- unlist(density)
plot(density,
     type = "l",
     lty = 1,
     ylab = "",
     xlab = "", main = "Densità",
     xaxt = "n"
)
axis(side = 1, at = x_at_years, labels = x_label_years)
abline(h = mean(density), col = 2, lty = 2, xpd = FALSE)

# Link reciprocity
link_reciprocity <- lapply(net_igraph, function(graph) reciprocity(graph))
link_reciprocity <- unlist(link_reciprocity)
plot(link_reciprocity,
     type = "l", lty = 1, ylab = "", xlab = "", main = "Reciprocità",
     xaxt = "n"
)
axis(side = 1, at = x_at_years, labels = x_label_years)
abline(h = mean(link_reciprocity), col = 2, lty = 2, xpd = FALSE)

# Mean shortest paths (utile per Small world - velocità di trasmissione)
mean_sp <- lapply(net_igraph, function(graph) mean_distance(graph, unconnected = TRUE))
mean_sp <- unlist(mean_sp)
plot(mean_sp,
     type = "l", lty = 1, ylab = "", xlab = "", main = "Mean shortest paths",
     xaxt = "n"
)
axis(side = 1, at = x_at_years, labels = x_label_years)
abline(h = mean(mean_sp), col = 2, lty = 2, xpd = FALSE)
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)


# Diametro
diameter <- lapply(net_igraph, function(graph) diameter(graph, unconnected = TRUE))
diameter <- unlist(diameter)
plot(diameter,
     type = "l", lty = 1, ylab = "", xlab = "", main = "Diametro",
     xaxt = "n", xlim = c(1, 900)
)
axis(side = 1, at = x_at_years, labels = x_label_years)
abline(h = mean(diameter), col = 2, lty = 2, xpd = FALSE)





# Misure sui singoli nodi - importanti per individuare i nodi più rilevanti e valutarne le connessioni
# -> individuazione canale di trasmissione del rischio entro il sistema finanziario

# Gradi in uscita e in entrata per nodo (influenza e prestigio dei nodi)
out_degree <- lapply(net_igraph, function(graph) degree(graph, mode = "out")) # Misura l'influenza dei nodi (il contributo al rischio di sistema)
out_degree_mat <- matrix(unlist(out_degree),
     byrow = TRUE,
     ncol = NCOL(stoxx600_logrend_week)
)

boxplot(out_degree_mat,
     col = c(rep(2, length(bank_ind)), rep(3, length(ins_ind)), rep(4, length(fs_ind))),
     xaxt = "n",
     main = "Grado in uscita"
)

in_degree <- lapply(net_igraph, function(graph) degree(graph, mode = "in")) # Misura il prestigio di un nodo ()
in_degree_mat <- matrix(unlist(in_degree),
     byrow = TRUE,
     ncol = NCOL(stoxx600_logrend_week)
)

boxplot(in_degree_mat,
     col = c(rep(2, length(bank_ind)), rep(3, length(ins_ind)), rep(4, length(fs_ind))),
     xaxt = "n",
     main = "Grado in entrata"
)

max_out <- max_in <- NCOL(stoxx600_logrend_week) - 1 # Niente self-loop!
min_out <- min_in <- 0

out_degree_max <- lapply(out_degree, function(num) which(num == max_out))
out_degree_min <- lapply(out_degree, function(num) which(num == min_out))

in_degree_max <- lapply(in_degree, function(num) which(num == max_in))
in_degree_min <- lapply(in_degree, function(num) which(num == min_in))



# Closeness (misura di centralità dei nodi basata su)
out_closeness <- lapply(net_igraph, function(graph) {
     closeness(graph,
          mode = "out",
          normalized = TRUE
     )
})
out_closeness_mat <- matrix(unlist(out_closeness),
     byrow = TRUE,
     ncol = NCOL(stoxx600_logrend_week)
)
out_closeness_mat[is.nan(out_closeness_mat)] <- NA

boxplot(out_closeness_mat,
     col = c(rep(2, length(bank_ind)), rep(3, length(ins_ind)), rep(4, length(fs_ind))),
     xaxt = "n",
     main = "Out-closeness"
)

# Massimo e minimo out-closeness
mean_outclosness <- apply(out_closeness_mat, 2, function(col) mean(col, na.rm = TRUE))
out_closeness_max <- which.max(mean_outclosness)
colnames(stoxx600_logrend_week)[out_closeness_max]
out_closeness_min <- which.min(mean_outclosness)
colnames(stoxx600_logrend_week)[out_closeness_min]

plot(out_closeness_mat[, out_closeness_max],
     col = 1, type = "l", ylab = "", xaxt = "n",
     ylim = c(0, 1)
)
axis(side = 1, at = x_at_years, labels = x_label_years)
points(out_closeness_mat[, out_closeness_min], col = 2, type = "l")


in_closeness <- lapply(net_igraph, function(graph) {
     closeness(graph,
          mode = "in",
          normalized = TRUE
     )
})
in_closeness_mat <- matrix(unlist(in_closeness),
     byrow = TRUE,
     ncol = NCOL(stoxx600_logrend_week)
)
in_closeness_mat[is.nan(in_closeness_mat)] <- NA

boxplot(in_closeness_mat,
     col = c(rep(2, length(bank_ind)), rep(3, length(ins_ind)), rep(4, length(fs_ind))),
     xaxt = "n",
     main = "In-closeness"
)

# Massimo e minimo in-closeness
mean_inclosness <- apply(in_closeness_mat, 2, function(col) mean(col, na.rm = TRUE))
in_closeness_max <- which.max(mean_inclosness)
colnames(stoxx600_logrend_week)[in_closeness_max]
in_closeness_min <- which.min(mean_inclosness)
colnames(stoxx600_logrend_week)[in_closeness_min]

plot(in_closeness_mat[, in_closeness_max],
     col = 1, type = "l", ylab = "", ylim = c(0, 1),
     xaxt = "n"
)
axis(side = 1, at = x_at_years, labels = x_label_years)
points(in_closeness_mat[, in_closeness_min], col = 2, type = "l")


# Betweenness (misura di centralità dei nodi basata su)
betweenness <- lapply(net_igraph, function(graph) {
     betweenness(graph,
          directed = TRUE,
          normalized = TRUE
     )
})
betweenness_mat <- matrix(unlist(betweenness),
     byrow = TRUE,
     ncol = NCOL(stoxx600_logrend_week)
)
boxplot(betweenness_mat,
     col = c(rep(2, length(bank_ind)), rep(3, length(ins_ind)), rep(4, length(fs_ind))),
     xaxt = "n",
     main = "Betweeness"
)

# Massima e minima betweenness
mean_betweenness <- apply(betweenness_mat, 2, function(col) mean(col, na.rm = TRUE))
betweenness_max <- which.max(mean_betweenness)
colnames(stoxx600_logrend_week)[betweenness_max]
betweenness_min <- which.min(mean_betweenness)
colnames(stoxx600_logrend_week)[betweenness_min]

plot(betweenness_mat[, in_closeness_max], col = 1, type = "l", ylab = "", xaxt = "n")
axis(side = 1, at = x_at_years, labels = x_label_years)
points(betweenness_mat[, in_closeness_min], col = 2, type = "l")







# Eigenvector centrality (misura di centralità dei nodi basata su )
eigenvector_centrality <- lapply(net_igraph, function(graph) {
     eigen_centrality(graph,
          directed = TRUE,
          scale = TRUE
     )$vector
}) # Normalizza ad 1 il massimo score
eigenvector_centrality_mat <- matrix(unlist(eigenvector_centrality),
     byrow = TRUE,
     ncol = NCOL(stoxx600_logrend_week)
)
boxplot(eigenvector_centrality_mat,
     col = c(rep(2, length(bank_ind)), rep(3, length(ins_ind)), rep(4, length(fs_ind))),
     xaxt = "n",
     main = "Eigenvector centrality"
)

# # Page rank (misura di centralità dei nodi basata su )
# page_rank_centrality <- lapply(net_igraph, function(graph) page_rank(graph,
#                                                                      directed = TRUE,
#                                                                      damping = 0.85)$vector)




# Fiedler vector













# Community detection
# set.seed(123)
# cluster_eb <- lapply(net_igraph, cluster_edge_betweenness)
# membership_eb <- lapply(cluster_eb, membership)
# assortativity_eb <- NULL
# for (i in 1:length(membership_eb)){
#   assortativity_eb <- c(assortativity_eb, assortativity(net_igraph[[i]], membership_eb[[i]]))
# }
# plot(assortativity_eb, type = 'l') # Comunità non significative


# Assortatività wrt settore
membership_sec <- c(rep(2, length(bank_ind)), rep(3, length(ins_ind)), rep(4, length(fs_ind)))
assortativity_sec <- lapply(net_igraph, function(graph) assortativity(graph, membership_sec))
assortativity_sec <- unlist(assortativity_sec)
plot(assortativity_sec,
     type = "l",
     ylab = "",
     main = "Assortatività (per settore)",
     xaxt = "n",
     xla = ""
) # Non c'è omofilia per settore
axis(side = 1, at = x_at_years, labels = x_label_years)
abline(h = mean(assortativity_sec), col = 2, lty = 2, xpd = FALSE)




# Scale-free property










# Rappresentazione grafica

##### Dimensione \propto grado in uscita #####

# Minima densità out-degree
min_density_id <- which.min(density)
stoxx600_week_cut[min_density_id] # Settimana con la minore densità
min(density)
set.seed(123)
V(net_igraph[[min_density_id]])$size <- out_degree[[min_density_id]] / 5
circle_layout <- layout_in_circle(net_igraph[[min_density_id]])

# Colorazione archi per settore
# Estraggo il nome del sender
sender <- lapply(strsplit(attr(E(net_igraph[[min_density_id]]), "vnames"), "|", fixed = TRUE), function(list) list[[1]])
# Controllo a che gruppo appartiene il sender
sender_bank <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[bank_ind])
sender_ins <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[ins_ind])
sender_fs <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[fs_ind])

plot(net_igraph[[min_density_id]],
     layout = circle_layout,
     edge.arrow.size = 0,
     edge.color = c(
          rep(1, length(sender_bank)),
          rep(2, length(sender_ins)),
          rep(3, length(sender_fs))
     ),
     vertex.color = c(
          rep(1, length(bank_ind)),
          rep(2, length(ins_ind)),
          rep(3, length(fs_ind))
     ),
     vertex.label.cex = 0.4,
     vertex.label.degree = c(
          rep(0, 16),
          rep(-pi / 6, 4),
          rep(pi / 6, 3),
          rep(pi, 34),
          rep(-pi / 6, 4),
          rep(0, 17)
     ),
     vertex.label.dist = c(
          rep(2, length = 16),
          seq(0, 2, length = 4),
          -seq(2, 0, length = 3),
          rep(2, length = 34),
          -seq(0, 3, length = 4),
          rep(2, length = 17)
     ),
     margin = c(-0.4, 0, -0.3, 0)
)

# Minima densità in-degree
min_density_id <- which.min(density)
stoxx600_week_cut[min_density_id] # Settimana con la minore densità
min(density)
set.seed(123)
V(net_igraph[[min_density_id]])$size <- in_degree[[min_density_id]] / 5
circle_layout <- layout_in_circle(net_igraph[[min_density_id]])

# Colorazione archi per settore
# Estraggo il nome del sender
sender <- lapply(strsplit(attr(E(net_igraph[[min_density_id]]), "vnames"), "|", fixed = TRUE), function(list) list[[1]])
# Controllo a che gruppo appartiene il sender
sender_bank <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[bank_ind])
sender_ins <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[ins_ind])
sender_fs <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[fs_ind])

plot(net_igraph[[min_density_id]],
     layout = circle_layout,
     edge.arrow.size = 0,
     edge.color = c(
          rep(1, length(sender_bank)),
          rep(2, length(sender_ins)),
          rep(3, length(sender_fs))
     ),
     vertex.color = c(
          rep(1, length(bank_ind)),
          rep(2, length(ins_ind)),
          rep(3, length(fs_ind))
     ),
     vertex.label.cex = 0.4,
     vertex.label.degree = c(
          rep(0, 16),
          rep(-pi / 6, 4),
          rep(pi / 6, 3),
          rep(pi, 34),
          rep(-pi / 6, 4),
          rep(0, 17)
     ),
     vertex.label.dist = c(
          rep(2, length = 16),
          seq(0, 2, length = 4),
          -seq(2, 0, length = 3),
          rep(2, length = 34),
          -seq(0, 3, length = 4),
          rep(2, length = 17)
     ),
     margin = c(-0.4, 0, -0.3, 0)
)


# Massima densità out-degree
max_density_id <- which.max(density)
stoxx600_week_cut[max_density_id] # Settimana con la maggiore densità
max(density)
set.seed(123)
V(net_igraph[[max_density_id]])$size <- out_degree[[max_density_id]] / 5
circle_layout <- layout_in_circle(net_igraph[[max_density_id]])

# Colorazione archi per settore
# Estraggo il nome del sender
sender <- lapply(strsplit(attr(E(net_igraph[[max_density_id]]), "vnames"), "|", fixed = TRUE), function(list) list[[1]])
# Controllo a che gruppo appartiene il sender
sender_bank <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[bank_ind])
sender_ins <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[ins_ind])
sender_fs <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[fs_ind])

plot(net_igraph[[max_density_id]],
     layout = circle_layout,
     edge.arrow.size = 0,
     edge.color = c(
          rep(1, length(sender_bank)),
          rep(2, length(sender_ins)),
          rep(3, length(sender_fs))
     ),
     vertex.color = c(
          rep(1, length(bank_ind)),
          rep(2, length(ins_ind)),
          rep(3, length(fs_ind))
     ),
     vertex.label.cex = 0.4,
     vertex.label.degree = c(
          rep(0, 16),
          rep(-pi / 6, 4),
          rep(pi / 6, 3),
          rep(pi, 34),
          rep(-pi / 6, 4),
          rep(0, 17)
     ),
     vertex.label.dist = c(
          rep(2, length = 16),
          seq(0, 2, length = 4),
          -seq(2, 0, length = 3),
          rep(2, length = 34),
          -seq(0, 3, length = 4),
          rep(2, length = 17)
     ),
     margin = c(-0.4, 0, -0.3, 0)
)


# Massima densità in-degree
max_density_id <- which.max(density)
stoxx600_week_cut[max_density_id] # Settimana con la maggiore densità
max(density)
set.seed(123)
V(net_igraph[[max_density_id]])$size <- in_degree[[max_density_id]] / 5
circle_layout <- layout_in_circle(net_igraph[[max_density_id]])

# Colorazione archi per settore
# Estraggo il nome del sender
sender <- lapply(strsplit(attr(E(net_igraph[[max_density_id]]), "vnames"), "|", fixed = TRUE), function(list) list[[1]])
# Controllo a che gruppo appartiene il sender
sender_bank <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[bank_ind])
sender_ins <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[ins_ind])
sender_fs <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[fs_ind])

plot(net_igraph[[max_density_id]],
     layout = circle_layout,
     edge.arrow.size = 0,
     edge.color = c(
          rep(1, length(sender_bank)),
          rep(2, length(sender_ins)),
          rep(3, length(sender_fs))
     ),
     vertex.color = c(
          rep(1, length(bank_ind)),
          rep(2, length(ins_ind)),
          rep(3, length(fs_ind))
     ),
     vertex.label.cex = 0.4,
     vertex.label.degree = c(
          rep(0, 16),
          rep(-pi / 6, 4),
          rep(pi / 6, 3),
          rep(pi, 34),
          rep(-pi / 6, 4),
          rep(0, 17)
     ),
     vertex.label.dist = c(
          rep(2, length = 16),
          seq(0, 2, length = 4),
          -seq(2, 0, length = 3),
          rep(2, length = 34),
          -seq(0, 3, length = 4),
          rep(2, length = 17)
     ),
     margin = c(-0.4, 0, -0.3, 0)
)





##### Dimensione \propto betweeness #####

# Minima densità
min_density_id <- which.min(density)
set.seed(123)
V(net_igraph[[min_density_id]])$size <- betweenness[[min_density_id]] * 100
circle_layout <- layout_in_circle(net_igraph[[min_density_id]])

# Colorazione archi per settore
# Estraggo il nome del sender
sender <- lapply(strsplit(attr(E(net_igraph[[min_density_id]]), "vnames"), "|", fixed = TRUE), function(list) list[[1]])
# Controllo a che gruppo appartiene il sender
sender_bank <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[bank_ind])
sender_ins <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[ins_ind])
sender_fs <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[fs_ind])

plot(net_igraph[[min_density_id]],
     layout = circle_layout,
     edge.arrow.size = 0,
     edge.color = c(
          rep(1, length(sender_bank)),
          rep(2, length(sender_ins)),
          rep(3, length(sender_fs))
     ),
     vertex.color = c(
          rep(1, length(bank_ind)),
          rep(2, length(ins_ind)),
          rep(3, length(fs_ind))
     ),
     vertex.label.cex = 0.4,
     vertex.label.degree = c(
          rep(0, 16),
          rep(-pi / 6, 4),
          rep(pi / 6, 3),
          rep(pi, 34),
          rep(-pi / 6, 4),
          rep(0, 17)
     ),
     vertex.label.dist = c(
          rep(2, length = 16),
          seq(0, 2, length = 4),
          -seq(2, 0, length = 3),
          rep(2, length = 34),
          -seq(0, 3, length = 4),
          rep(2, length = 17)
     ),
     margin = c(-0.4, 0, -0.3, 0)
)



# Massima densità
max_density_id <- which.max(density)
set.seed(123)
V(net_igraph[[max_density_id]])$size <- betweenness[[max_density_id]] * 100
circle_layout <- layout_in_circle(net_igraph[[max_density_id]])

# Colorazione archi per settore
# Estraggo il nome del sender
sender <- lapply(strsplit(attr(E(net_igraph[[max_density_id]]), "vnames"), "|", fixed = TRUE), function(list) list[[1]])
# Controllo a che gruppo appartiene il sender
sender_bank <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[bank_ind])
sender_ins <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[ins_ind])
sender_fs <- which(unlist(sender) %in% colnames(stoxx600_logrend_week)[fs_ind])

plot(net_igraph[[max_density_id]],
     layout = circle_layout,
     edge.arrow.size = 0,
     edge.color = c(
          rep(1, length(sender_bank)),
          rep(2, length(sender_ins)),
          rep(3, length(sender_fs))
     ),
     vertex.color = c(
          rep(1, length(bank_ind)),
          rep(2, length(ins_ind)),
          rep(3, length(fs_ind))
     ),
     vertex.label.cex = 0.4,
     vertex.label.degree = c(
          rep(0, 16),
          rep(-pi / 6, 4),
          rep(pi / 6, 3),
          rep(pi, 34),
          rep(-pi / 6, 4),
          rep(0, 17)
     ),
     vertex.label.dist = c(
          rep(2, length = 16),
          seq(0, 2, length = 4),
          -seq(2, 0, length = 3),
          rep(2, length = 34),
          -seq(0, 3, length = 4),
          rep(2, length = 17)
     ),
     margin = c(-0.4, 0, -0.3, 0)
)




# Analisi significatività statistiche di rete via simulazione -------------
