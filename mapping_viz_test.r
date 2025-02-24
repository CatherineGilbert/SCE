library(reshape2)

final_dt
final_dt2

id_cor <- cor(t(final_dt2), use = "pairwise.complete.obs") 
id_cor[lower.tri(id_cor)] <- NA
IDs <- colnames(id_cor)

id_edges <- melt(id_cor, varnames = c("Site1", "Site2"), value.name = "Corr")
id_edges <- filter(id_edges, !is.na(Corr))
id_edges <- filter(id_edges, Site1 != Site2)

id_verts <- filter(final_x, ID %in% IDs)

site_net <- graph_from_data_frame(id_edges, directed = FALSE, vertices = id_verts)

jitter_factor = 50
latlong <- select(id_verts, Longitude, Latitude) %>% 
  mutate(Longitude = jitter(.$Longitude, factor = jitter_factor), 
         Latitude = jitter(.$Latitude, factor = jitter_factor)) %>% # add jitter for overlaps
  as.matrix() %>% unname() ; head(latlong)

library(paletteer)
library(pals)
plasma(10)
library(RColorBrewer)

# Map the scaled values to colors
# Create 100 colors in the palette, and map the scaled values to these colors
hex_colors <- paletteer_c("ggthemes::Classic Orange-Blue", 100)[as.numeric(cut(E(site_net)$Corr, breaks = seq(-1, 1, length.out = 100)))]

par(mai = c(0,0,0,0))
plot(site_net, layout = latlong, asp = 0, 
     vertex.label.color = "black", vertex.size = 5, 
     vertex.label.cex = 0.5, vertex.color = NA, 
     vertex.frame.color = NA,
     edge.color = hex_colors
)
