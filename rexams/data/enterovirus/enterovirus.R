# www.computational-genomics.net
# https://www.researchgate.net/profile/Mohamed_Mourad_Lafifi/post/How_to_download_sequences_from_Genbang_using_long_taxonomic_list/attachment/5dd4fea33843b0938391fad3/AS%3A827236278079488%401574239906326/download/Getting+Sequences+from+GenBank+using+R-packages+.pdf
# https://a-little-book-of-r-for-bioinformatics.readthedocs.io/en/latest/src/chapter5.html


library(tidyverse)
library(ape)
library(Biostrings)
library(ggseqlogo)
library(rentrez)
library(seqinr)

ntd <- "Enterovirus[Organism]"
enterovirus_search <- entrez_search(db="nuccore", term=ntd, retmax=40)
enterovirus_seqs <- entrez_fetch(db="nuccore",
                                 id=enterovirus_search$ids[c(2)],
                                 rettype="fasta")
write(enterovirus_seqs, "rexams/data/enterovirus/enterovirus-40.fasta", sep="\n")

enterovirus_seqinr_format <- read.fasta(file = "rexams/data/enterovirus/enterovirus-40.fasta",
                                        seqtype = "DNA",
                                        as.string = TRUE,
                                        forceDNAtolower = FALSE)
z <- attr(enterovirus_seqinr_format, "name")
enterovirus_ape_format <- read.GenBank(z)
attr(enterovirus_ape_format, "species")
dna <- clustal(enterovirus_ape_format)
image(dna)
D <- dist.dna(dna, model = "TN93")
table.paint(as.data.frame(as.matrix(D)), cleg = 0, clabel.row = 0.5, clabel.col = 0.5)
tre <- njs(D)
plot(tre, cex = 0.6)
title("A simple NJ tree")



ref <- "EU203593"

z <- read.GenBank(ref, as.character = T)
z1 <- Biostrings::DNAString(z[[1]] |> str_c(collapse = ""))
Biostrings::translate(z1)

z[[1]][c(3:29)] %>%
  str_c(collapse = "") %>%
  DNAString() %>%
  Biostrings::translate()

z[[1]][21492:25259] %>%
  str_c(collapse = "") %>%
  DNAString() %>%
  Biostrings::translate()

data("cynipids")

z <- lapply(cynipids, function(x) str_c(x, collapse = ""))
str_sub(z$Diplolepis_rosae, start = 1, end = 1) <- "l"

ggseqlogo(z %>% toupper() %>% substr(30, 40), seq_type = "aa")


z1 <- strsplit(c(z[[3]], z[[4]]), split = "")
index <- z1[[1]] != z1[[2]]
z2 <- lapply(z, function(x) strsplit(x, "")[[1]][index])
ggseqlogo(z2 %>% toupper(), seq_type = "aa")

# Thu Apr 06 14:15:39 2023 ------------------------------
m_genitalium <- "NC_000908"

z <- read.GenBank(m_genitalium, as.character = T)
z1 <- Biostrings::DNAString(z[[1]] %>% str_c(collapse = ""))

ntd <- "H2N3"
h2n3_search <- entrez_search(db="nuccore", term=ntd, retmax=40)
h2n3_seqs <- entrez_fetch(db="nuccore",
                          id=h2n3_search$ids[1],
                          rettype="fasta")

# Fri Apr 07 00:10:07 2023 ------------------------------
library(GEOquery)

eList <- getGEO("GSE215359")
class(eList)
length(eList)
names(eList)
eData <- eList[[1]]
eData
