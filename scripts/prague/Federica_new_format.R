# Federica table for Genome size and GC content
library(tidyverse)
setwd(dir = "G:/My Drive/federica/microbiome_br_2/")

table_with_columns_edited <- read.delim(file = "FEDERICA_samples_tax_traits.txt")

# names(table_with_columns_edited)
# table_with_columns_edited[1:10,1:10]

sample_matrix <- table_with_columns_edited[,3:31]

get_genome_sizes <- function(v) {
  presence_absence_vector <- ifelse (v > 0, 1, 0)
  positions_to_get_genome_sizes <- which(as.logical(presence_absence_vector))
  v[positions_to_get_genome_sizes] <- table_with_columns_edited$size_mb[positions_to_get_genome_sizes]
  v
}

get_gc_content <- function(v) {
  presence_absence_vector <- ifelse (v > 0, 1, 0)
  positions_to_get_genome_sizes <- which(as.logical(presence_absence_vector))
  v[positions_to_get_genome_sizes] <- table_with_columns_edited$gc_percent[positions_to_get_genome_sizes]
  v
}

get_genome_sizes(table_with_columns_edited$X2a_dna[1:100])
get_gc_content(table_with_columns_edited$X2a_dna[1:100])

size_mb_full_table <- sapply(X = table_with_columns_edited[,3:31], get_genome_sizes)
gc_content_full_table <- sapply(X = table_with_columns_edited[,3:31], get_gc_content)

# size_mb_full_table[1:10,1:10]
# gc_content_full_table[1:10,1:10]


Size_names <- paste0(colnames(gc_content_full_table),rep("_size")) %>%
  str_replace("_dna","")
GC_names <- paste0(colnames(gc_content_full_table),rep("_GC")) %>%
  str_replace("_dna","")

colnames(size_mb_full_table) <- Size_names
colnames(gc_content_full_table) <- GC_names

Big_table_with_size_and_GC <- cbind(size_mb_full_table, gc_content_full_table)

# Big_table_with_size_and_GC[1:10,1:10]

Big_table_with_size_and_GC[1:3,c(1,30,2,31)]
29*2

v1 <- 1:29
v2 <- 30:58

order_of_columns <- c(rbind(v1, v2))

Big_table_with_size_and_GC <- Big_table_with_size_and_GC[,order_of_columns]

Final_table_with_size_and_GC <- data.frame(ctg_name = table_with_columns_edited[,1], 
                                           Big_table_with_size_and_GC, 
                                           BEST_TAX = table_with_columns_edited[,32])
    
# Final_table_with_size_and_GC[1:10,1:10]

write.table(x = Final_table_with_size_and_GC, file = "FEDERICA_size_and_gc_per_sample.txt", quote = F, sep = "\t", row.names = F)

# if you want to remove NAs
# Final_table_with_size_and_GC[is.na(Final_table_with_size_and_GC)] <- 0
# Final_table_with_size_and_GC <- na.omit(Final_table_with_size_and_GC)



