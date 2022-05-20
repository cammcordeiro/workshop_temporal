
# institutions' network

links <- formulario %>% 
  dplyr::filter(PELD == "Sim") %>% 
  filter(!is.na(pesquisador_responsavel)) %>% 
  dplyr::select(nome_programa, outras_instituicoes) %>% 
  unique()

links1 <- links %>%
  mutate(parcerias = strsplit(as.character(outras_instituicoes), ";")) %>%
  unnest(cols = c(parcerias)) %>%
  filter(parcerias != "") %>%
  select(-outras_instituicoes) %>% 
  mutate(parcerias = trimws(.$parcerias)) %>% 
  # bind_rows(data.frame(nome_programa = c("PELD-ILOC", "PELD-ELPA"), parcerias = c("UFF", "FURG"))) %>% 
  mutate(nome_programa = gsub("PELD-", "", nome_programa)) %>% 
  arrange(nome_programa) %>% 
  data.frame() %>% 
  bind_rows(data.frame(nome_programa = c("HCES", "HCES", "RECA", "RECA", "RLaC", "ILOC", rep("ELPA", 19)),
                       parcerias = c("Oregon State University", "UFF", "IEAPM", "ICMBio", "UFRJ", "UFF",
                                     "FURG", "UFRGS", "UFPR", "UFC", "UFPE", "UERJ", "Texas A&M University", 
                                     "University of North Texas", "The University of Adelaide", "NOAA",
                                     "Portos RS", "ICMBio", "MCTI", "MAPA", "Yaqu Pacha", "NEMA", 
                                     "KAOSA", "Costa Semi-Árida", "TAMS")))

# network
net <- graph.data.frame(links1, directed=T) %>% 
  simplify(., remove.multiple = T, remove.loops = T)
# E(net)$arrow.size <- .1
# plot(net)

# get nodes
plotcord <- data.frame(layout.fruchterman.reingold(net))
plotcord$subjects <- c(unique(links1 %>% pull(nome_programa)), unique(links1 %>% pull(parcerias))) %>% unique()

plotcord <- plotcord %>% 
  mutate(tamanho = ifelse(subjects %in% c("Abrolhos", "CCAL", "ELPA", "HCES", "ILOC", "PEBG", "RECA", "RLaC", "TAMS"), 3, 1),
         cor_node = ifelse(subjects %in% c("NEMA", "MCTI", "MAPA", "Portos RS", "ICMBio", "Embrapa", "CEPENE", "Ibama", "INPE", "Jardim Botânico do Rio de Janeiro", "IEAPM"), "govermental institution",
                           ifelse(subjects %in% c("Oregon State University", "Goethe Universität Frankfurt/Alemanha", 
                                                  "University of Kiel/Alemanha", "Universidad Nacional de Mar del Plata/Argentina",
                                                  "Universidad de la Republica/Uruguai", "Environment Agency/Austria", 
                                                  "University of Southern California", "University of South Carolina/USA",
                                                  "Oregon State University","Texas A&M University", "NOAA",
                                                  "University of North Texas", "The University of Adelaide"), "foreign institution",
                                  ifelse(subjects %in% c("Abrolhos", "CCAL", "ELPA", "HCES", "ILOC", "PEBG", "RECA", "RLaC", "TAMS"), "PELD project",
                                         ifelse(subjects %in% c("ONG Biota", "KAOSA", "Yaqu Pacha"), "NGO", 
                                                ifelse(subjects %in% c("Costa Semi-Árida", "TAMS"), "PELD project", "Brazilian university")))))
  )


# get vertices
g <- get.data.frame(net)  # get the edge information using the get.data.frame function


# add codes
plotcord <- left_join(plotcord,
                      plotcord %>% 
                        select(-X1,-X2,-tamanho) %>% 
                        filter(!subjects %in% unique(g$from)) %>% 
                        mutate(codes = ifelse(subjects %in% unique(g$from), subjects,
                                              as.factor(subjects) %>% as.numeric())) %>% 
                        mutate(codes = as.numeric(codes))) %>% 
  arrange(codes) %>% 
  mutate(codes = ifelse(subjects %in% unique(g$from), subjects, codes))


# add vertices
g$from.x <- plotcord$X1[match(g$from, plotcord$subjects)]  #  match the from locations from the node data.frame we previously connected
g$from.y <- plotcord$X2[match(g$from, plotcord$subjects)]
g$to.x <- plotcord$X1[match(g$to, plotcord$subjects)]  #  match the to locations from the node data.frame we previously connected
g$to.y <- plotcord$X2[match(g$to, plotcord$subjects)]

# plot
g %>% 
  ggplot() +
  geom_segment(alpha = 1, aes(x = from.x, xend = to.x, y = from.y, yend = to.y)) +
  geom_point(data = plotcord, 
             aes(x = X1, y = X2, colour = cor_node, size = tamanho)) +
  geom_text_repel(data = plotcord, 
                  aes(x = X1, y = X2, label = codes),
                  box.padding = 0.3) +
  theme_classic() +
  theme(legend.title = element_blank()) +
  # lims(x = c(-10, 15)) +
  scale_size(range = c(1, 5)) 



##################
V(net)
net <- simplify(net, remove.multiple = F, remove.loops = T) 

degree(net) %>% 
  data.frame() %>% 
  rownames_to_column("node") %>% 
  dplyr::rename(vertices = '.') %>% 
  arrange(-vertices)

betweenness(net) %>% 
  data.frame() %>% 
  rownames_to_column("node") %>% 
  dplyr::rename(vertices = '.') %>% 
  arrange(-vertices)

closeness(net) %>% 
  data.frame() %>% 
  rownames_to_column("node") %>% 
  dplyr::rename(vertices = '.') %>% 
  arrange(-vertices)

dyad_census(net) %>% 
  data.frame() %>% 
  rownames_to_column("node") %>% 
  dplyr::rename(vertices = '.') %>% 
  arrange(-vertices)

transitivity(net, type="global")  # net is treated as an undirected network
transitivity(as.undirected(net, mode="collapse")) # same as above
transitivity(net, type="local")
triad_census(net) # for directed networks 

diameter(net, directed=F, weights=NA)
diam <- get_diameter(net, directed=T)

vcol <- rep("gray40", vcount(net))
vcol[diam] <- "gold"
ecol <- rep("gray80", ecount(net))
ecol[E(net, path=diam)] <- "orange" 
# E(net, path=diam) finds edges along a path, here 'diam'
plot(net, vertex.color=vcol, edge.color=ecol, edge.arrow.mode=0)


deg <- degree(net, mode="all")
plot(net, vertex.size=deg*2)
hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")

deg.dist <- degree_distribution(net, cumulative=T, mode="all")
plot(x = 0:max(deg), y = 1-deg.dist, pch = 19, las = 1,
     cex=1.2, col="orange", xlab="Degree", ylab="Cumulative Frequency")

degree(net, mode="in")
centr_degree(net, mode="in", normalized=T)

##
hs <- hub_score(net, weights=NA)$vector
hs %>% 
  data.frame() %>% 
  rownames_to_column("node") %>% 
  dplyr::rename(hubs = '.') %>% 
  arrange(-hubs)

as <- authority_score(net, weights=NA)$vector
as %>% 
  data.frame() %>% 
  rownames_to_column("node") %>% 
  dplyr::rename(autor = '.') %>% 
  arrange(-autor)

par(mfrow=c(1,2))
plot(net, vertex.size=hs*50, main="Hubs")
plot(net, vertex.size=as*30, main="Authorities")

colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
kc <- coreness(as.undirected(net), mode="all")
plot(net, vertex.size=kc*6, vertex.label=kc, vertex.color=colrs[kc])

clp <- cluster_label_prop(net)
plot(clp, net)

ceb <- cluster_edge_betweenness(net) 
length(ceb) 
membership(ceb)
dendPlot(ceb, mode="hclust")
plot(ceb, net) 
crossing(ceb, net) 

net.sym <- as.undirected(net, mode= "collapse",
                         edge.attr.comb=list(weight="sum", "ignore"))
cliques(net.sym)
sapply(cliques(net.sym), length) # clique sizes
largest_cliques(net.sym) # cliques with max number of nodes
vcol <- rep("grey80", vcount(net.sym))
vcol[unlist(largest_cliques(net.sym))] <- "gold"
plot(as.undirected(net.sym), vertex.label=V(net.sym)$name, vertex.color=vcol)
