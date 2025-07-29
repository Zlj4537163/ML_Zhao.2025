library(ape)

phylo_tree <- as.phylo(hc)

create_distinct_colors <- function(n) {
  if (n <= 8) {
    return(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", 
             "#FF7F00", "#FFFF33", "#A65628", "#F781BF")[1:n])
  } else {
    return(colorRampPalette(brewer.pal(min(12, n), "Set3"))(n))
  }
}


library(RColorBrewer)
n_groups <- length(unique(anno$group))
group_colors <- create_distinct_colors(n_groups)
names(group_colors) <- unique(anno$group)


tip_colors <- group_colors[anno$group[match(phylo_tree$tip.label, anno$ID)]]


plot(phylo_tree, 
     type = "fan",       
     tip.color = tip_colors,  
     edge.width = 1,   
     no.margin = TRUE,    
     label.offset = 1,  
     cex = 1)           


# title("Safflower Accessions Clustering", line = -1)


legend_colors <- unique(group_colors)


legend_labels <- names(group_colors)


legend("bottomleft", 
       legend = legend_labels, 
       fill = legend_colors,
       title = "Geographic Group",
       bty = "n",         
       cex = 0.8,         
       inset = c(0, 0.05))

