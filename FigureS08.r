library(yolo)
library(picante)
library(vegan)
library(tidyverse)
library(ape)


metadata = read.delim('metadata_large_scale.tsv', row.names = 1, sep = '\t')
table = read.csv('ASVTable_largescale.csv', row.names = 1)
otutab = table[,-((ncol(table)-6):ncol(table))]
tax = table[,(ncol(table)-6):ncol(table)]

meta = metadata %>% filter(site=='HN')
meta$sample = rownames(meta) 
otu = as.matrix(otutab[,row.names(meta)])
otu = otu[which(rowSums(otu) > 0),]
tax = as.matrix(tax[rownames(otutab),])

meta_f = meta %>% filter(site=='HN')
data.f = otu[,rownames(meta_f)]

### DESeq2 ###
library(DESeq2)
data.f = converter(data.f, "core", 4)
dds = DESeqDataSetFromMatrix(countData = data.f, colData = as.matrix(meta_f), design=~habitat) 
dds1 = DESeq(dds)
res = results(dds1, contrast = c('habitat', 'Rhizoplane', 'Bulk'), pAdjustMethod = 'BH') 
deseq_res = as.data.frame(res[order(res$padj),])
# significant
deseq_res[which(deseq_res$padj < 0.05 & deseq_res$log2FoldChange <= 0),'sig'] <- 'depleted'
deseq_res[which(deseq_res$padj < 0.05 & deseq_res$log2FoldChange >= 0),'sig'] <- 'enriched'
# non-significant
deseq_res[which(deseq_res$padj >= 0.05),'sig'] <- 'nodiff'
# NA
deseq_res[which(is.na(deseq_res$padj)),'sig'] <- 'nodiff'
deseq_res[which(is.na(deseq_res$log2FoldChange)),'sig'] <- 'nodiff'
deseq_severe <- deseq_res
HN = deseq_severe %>% filter(sig=='enriched')

HN = tax[rownames(HN),'Order']
HN = table(HN)




meta = metadata %>% filter(site=='YN')
meta$sample = rownames(meta) 
otu = as.matrix(otutab[,row.names(meta)])
otu = otu[which(rowSums(otu) > 0),]
tax = as.matrix(tax[rownames(otutab),])

meta_f = meta %>% filter(site=='YN')
data.f = otu[,rownames(meta_f)]

### DESeq2 ###
library(DESeq2)
data.f = converter(data.f, "core", 4)
dds = DESeqDataSetFromMatrix(countData = data.f, colData = as.matrix(meta_f), design=~habitat) 
dds1 = DESeq(dds)
res = results(dds1, contrast = c('habitat', 'Rhizoplane', 'Bulk'), pAdjustMethod = 'BH') 
deseq_res = as.data.frame(res[order(res$padj),])
# significant
deseq_res[which(deseq_res$padj < 0.05 & deseq_res$log2FoldChange <= 0),'sig'] <- 'depleted'
deseq_res[which(deseq_res$padj < 0.05 & deseq_res$log2FoldChange >= 0),'sig'] <- 'enriched'
# non-significant
deseq_res[which(deseq_res$padj >= 0.05),'sig'] <- 'nodiff'
# NA
deseq_res[which(is.na(deseq_res$padj)),'sig'] <- 'nodiff'
deseq_res[which(is.na(deseq_res$log2FoldChange)),'sig'] <- 'nodiff'
deseq_severe <- deseq_res
YN = deseq_severe %>% filter(sig=='enriched')

YN = tax[rownames(YN),'Order']
YN = table(YN)





meta = metadata %>% filter(site=='DY')
meta$sample = rownames(meta) 
otu = as.matrix(otutab[,row.names(meta)])
otu = otu[which(rowSums(otu) > 0),]
tax = as.matrix(tax[rownames(otutab),])

meta_f = meta %>% filter(site=='DY')
data.f = otu[,rownames(meta_f)]

### DESeq2 ###
library(DESeq2)
data.f = converter(data.f, "core", 4)
dds = DESeqDataSetFromMatrix(countData = data.f, colData = as.matrix(meta_f), design=~habitat) 
dds1 = DESeq(dds)
res = results(dds1, contrast = c('habitat', 'Rhizoplane', 'Bulk'), pAdjustMethod = 'BH') 
deseq_res = as.data.frame(res[order(res$padj),])
# significant
deseq_res[which(deseq_res$padj < 0.05 & deseq_res$log2FoldChange <= 0),'sig'] <- 'depleted'
deseq_res[which(deseq_res$padj < 0.05 & deseq_res$log2FoldChange >= 0),'sig'] <- 'enriched'
# non-significant
deseq_res[which(deseq_res$padj >= 0.05),'sig'] <- 'nodiff'
# NA
deseq_res[which(is.na(deseq_res$padj)),'sig'] <- 'nodiff'
deseq_res[which(is.na(deseq_res$log2FoldChange)),'sig'] <- 'nodiff'
deseq_severe <- deseq_res
DY = deseq_severe %>% filter(sig=='enriched')

DY = tax[rownames(DY),'Order']
DY = table(DY)




##############################
HN_ = row.names(HN)
YN_ = row.names(YN)
DY_ = row.names(DY)





metadata = read.csv('metadata.csv', row.names = 'sample')
otutab = read.delim('ASVtable.txt', row.names = 1, sep = '\t')
tax = read.delim('taxon.tsv', row.names = 1, sep = '\t')
tax_gtdb = read.delim('taxon_gtdb.tsv', row.names = 1, sep = '\t')

meta = metadata %>% filter(habitat=='Rhizosphere')
meta$sample = rownames(meta)

otu = as.matrix(otutab[,row.names(meta)])
data.f = otu[which(rowSums(otu) > 0),]

tax = as.data.frame(tax[rownames(data.f),])
tax_gtdb = as.data.frame(tax_gtdb[rownames(data.f),])

### DESeq2 ###
library(DESeq2)
library(yolo)
otu_f = converter(data.f, "core", 10)
dds = DESeqDataSetFromMatrix(countData = otu_f, colData = as.matrix(meta), design=~type) 
dds1 = DESeq(dds)
res = results(dds1, contrast = c('type', 'Severe', 'Slight'), pAdjustMethod = 'BH') 
deseq_res = as.data.frame(res[order(res$padj),])
# significant
deseq_res[which(deseq_res$padj < 0.05 & deseq_res$log2FoldChange <= 0),'sig'] <- 'depleted'
deseq_res[which(deseq_res$padj < 0.05 & deseq_res$log2FoldChange >= 0),'sig'] <- 'enriched'
# non-significant
deseq_res[which(deseq_res$padj >= 0.05),'sig'] <- 'nodiff'
# NA
deseq_res[which(is.na(deseq_res$padj)),'sig'] <- 'nodiff'
deseq_res[which(is.na(deseq_res$log2FoldChange)),'sig'] <- 'nodiff'
deseq_severe <- deseq_res


sws = deseq_severe %>% filter(sig=='enriched')

sws = tax_gtdb[rownames(sws),'order']
sws = table(sws)


################################

library(ggvenn)
a <- list(`ZZ` = HN,
          `WS1` = YN,
          `DY` = DY,
          `WS2` = sws)
(ggvenn(a, c("ZZ", "WS1", "DY", "WS2"), fill_alpha = .3, stroke_color = .3, show_percentage = FALSE) -> pl)

d1 = intersect(HN, YN)
d2 = intersect(d1, DY)
d3 = intersect(d2, sws)
