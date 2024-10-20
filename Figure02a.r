

library(ggseqlogo)


data1 = read.csv("nifH.motifA.v16.LOGOmake.fasta", header = FALSE)
data2 = read.csv("nifH.motifB.v16.LOGOmake.fasta", header = FALSE)
data3 = read.csv("nifH.motifC.v16.LOGOmake.fasta", header = FALSE)
data1 = as.list(data1)
data2 = as.list(data2)
data3 = as.list(data3)

list_logo = list()
list_logo$motifA = data1$V1
list_logo$motifB = data2$V1
list_logo$motifC = data3$V1

# colors
col1<-make_col_scheme(chars = c("A","G", "T", "C"), 
                      groups = c("g1","g2", "g3","g4"),
                      cols = c("red","green","blue","yellow"))

# plots
ggseqlogo(list_logo, method="bits", facet = "wrap",ncol = 1, seq_type="aa")


# 查看字体
list_fonts(v = T)
# 查看颜色
list_col_schemes(v = T)
