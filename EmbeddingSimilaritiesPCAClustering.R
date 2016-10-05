rm(list=ls())
library(needs)
needs(data.table,kknn,igraph)
args = commandArgs(trailingOnly=TRUE)
f <- T
scriptName <- "EmbeddingSimilaritiesPCAClustering"
params <- list(algo="pcakmeans",k=100,kmeansalgo="MacQueen",nstart=100)
if(f){
  #   originalFN <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge1/data/subchallenge1/3_signal_anonym_directed_v3_wowts.txt"
  #   embeddingFN <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge1/data/subchallenge1/embeddings/submission1/3_signal_anonym_directed_v3_wowts.embedding"
  #   outputDir <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge1/Results/Final"
  originalFN <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge2/data/subchallenge2/all_wowts.txt"
  embeddingFN <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge2/data/subchallenge2/all_wowts.embeddings"
  outputDir <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge2/Results"
  clSize <- 30
  embeddingAlgo <- "DEEPWALK"
}else{
  originalFN <- args[1]
  embeddingFN <- args[2]
  outputDir <- args[3]
  clSize <- as.integer(args[4])
  embeddingAlgo <- args[6]
}
params$k <- clSize
embedding <- fread(embeddingFN,skip=1)
original <- fread(originalFN)
embeddingFileName <- unlist(strsplit(embeddingFN,"/"))
embeddingFileName <- embeddingFileName[length(embeddingFileName)]
embeddingFileName <- unlist(strsplit(embeddingFileName,"_wowts"))
embeddingFileName <- embeddingFileName[1]
outputDir <- paste(outputDir,"/",scriptName,"/",paste(names(params),params,sep="_",collapse="_"),sep="")
dir.create(outputDir,recursive = T)
outputFileName <- paste(outputDir,"/",embeddingFileName,".txt",sep="")
originalNodes <- union(original$V1,original$V2)
print(paste(length(originalNodes),params$k))
print(class(length(originalNodes)))
print(class(params$k))
print(length(originalNodes)/params$k)
print(ceiling(length(originalNodes)/params$k))
principalComponents <- prcomp(embedding[,2:ncol(embedding),with=F])
loadings <- data.frame(principalComponents$x[,1:30])
if(params$kmeansalgo=="MacQueen"){
  cluster <- kmeans(loadings,centers =ceiling(length(originalNodes)/params$k),
                       iter.max=1000,algorithm="MacQueen")
}else if(params$kmeansalgo=="Lloyd"){
  cluster <- kmeans(loadings,centers =ceiling(length(originalNodes)/params$k),
                       iter.max=1000,algorithm="Lloyd")
}else if(params$kmeansalgo=="Hartigan-Wong"){
  cluster <- kmeans(loadings,centers =ceiling(length(originalNodes)/params$k),
                       iter.max=1000,algorithm="Hartigan-Wong")
}else{
  cluster <- kmeans(loadings,centers =ceiling(length(originalNodes)/params$k),
                       iter.max=1000)
}

# if(params$algo=="kmeans"){
#   cl <- kmeans(embedding[,2:ncol(embedding),with=F],as.integer(ceiling(length(originalNodes)/params$k)),iter.max = params$maxIter,algorithm = "Lloyd",nstart = params$nstart)
# }else if(params$algo=="pam"){
#   cl <- pam(embedding[,2:ncol(embedding),with=F],k=as.integer(length(originalNodes)/params$k),cluster.only=T)
# }
save(cluster,file=paste(outputDir,"/",embeddingFileName,".rdata",sep=""))
embedding$Cluster <- cluster$cluster
clCtr <- as.integer(length(originalNodes)/params$k)
for(cl in unique(embedding$Cluster)){
  #tmp <- as.character(unlist(embedding[Cluster==c,"V1",with=F]))
  tmp <- embedding[Cluster==cl,"V1",with=F]
  clNodes <- intersect(tmp$V1,originalNodes)
  splits <- split(clNodes,ceiling(seq_along(clNodes)/params$k))
  for(x in names(splits)){
    if(x==1){
      write(c(paste(cl,"1",paste(splits[[x]],collapse = "\t"),sep="\t")),file=outputFileName,append=T)  
    }else{
      clCtr <- clCtr+1
      write(c(paste(clCtr,"1",paste(splits[[x]],collapse = "\t"),sep="\t")),file=outputFileName,append=T)  
    }
  }
  #tmp <- paste(intersect(tmp$V1,originalNodes),collapse="\t")
  #write(c(paste(cl,"1",tmp,sep="\t")),file=outputFileName,append=T)
}