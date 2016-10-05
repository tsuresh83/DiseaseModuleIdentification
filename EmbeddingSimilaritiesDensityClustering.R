rm(list=ls())
library(needs)
needs(data.table,densityClust)
args = commandArgs(trailingOnly=TRUE)
f <- F
scriptName <- "EmbeddingSimilaritiesDensityClustering"
params <- list(algo="density",k=100)
if(f){
#   originalFN <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge1/data/subchallenge1/1_ppi_anonym_v2_wowts.txt"
#   embeddingFN <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge1/data/subchallenge1/embeddings/submission1/1_ppi_anonym_v2_wowts.embedding"
#   outputDir <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge1/Results1"
#   clSize <- 100
#   embeddingAlgo <- "DEEPWALK"
  originalFN <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge2/data/subchallenge2/all_wowts.txt"
  embeddingFN <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge2/data/subchallenge2/all_wowts.embeddings"
  outputDir <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge2/Results"
  clSize <- 100
  embeddingAlgo <- "DEEPWALK"
}else{
  originalFN <- args[1]
  embeddingFN <- args[2]
  outputDir <- args[3]
  clSize <- as.integer(args[4])
  embeddingAlgo <- args[5]
}
params$k <- clSize
embedding <- fread(embeddingFN,skip=1)
distances <- dist(embedding[,(2:ncol(embedding)),with=F])
distanceCutoff <- estimateDc(distances)
densityClusterRes <- densityClust(distances,1)
clusters <- findClusters(densityClusterRes,150,1)
original <- fread(originalFN)
embeddingFileName <- unlist(strsplit(embeddingFN,"/"))
embeddingFileName <- embeddingFileName[length(embeddingFileName)]
embeddingFileName <- unlist(strsplit(embeddingFileName,"_wowts"))
embeddingFileName <- embeddingFileName[1]
outputDir <- paste(outputDir,"/",scriptName,"_",embeddingAlgo,"/",paste(names(params),params,sep="_",collapse="_"),sep="")
dir.create(outputDir,recursive = T)
outputFileName <- paste(outputDir,"/",embeddingFileName,".txt",sep="")
originalNodes <- union(original$V1,original$V2)
for(cl in unique(embedding$Cluster)){
  tmp <- embedding[Cluster==cl,"V1",with=F]
  clNodes <- intersect(tmp$V1,originalNodes)
  if(length(clNodes)>params$k){
    tmpData <- embedding[V1 %in% clNodes,]
    tmpcl <- kmeans(tmpData[,2:(ncol(tmpData)-1),with=F],centers=as.integer(ceiling(length(clNodes)/params$k)),iter.max = params$maxIter,algorithm = "MacQueen")
    tmpCenters <- tmpcl$centers
    tmpSizeRemaining <- length(clNodes)
    firstCluster <- T
    for(center in 1:nrow(tmpCenters)){
      if(tmpSizeRemaining >0){
        neighbors <- nn2(tmpData[,2:(ncol(tmpData)-1),with=F],matrix(tmpCenters[center,],nrow=1),k=min(tmpSizeRemaining,params$k))
        tmpSizeRemaining <-tmpSizeRemaining-min(tmpSizeRemaining,params$k)
        if(firstCluster){
          write(c(paste(cl,"1",paste(unlist(tmpData[neighbors$nn.idx[1,],"V1",with=F]),collapse = "\t"),sep="\t")),file=outputFileName,append=T) 
          firstCluster <- F
        }else{
          clCtr <- clCtr+1
          write(c(paste(clCtr,"1",paste(unlist(tmpData[neighbors$nn.idx[1,],"V1",with=F]),collapse = "\t"),sep="\t")),file=outputFileName,append=T) 
        }
        
        tmpData <- tmpData[-neighbors$nn.idx[1,],]
      }
    }
  }else{
    splits <- split(clNodes,ceiling(seq_along(clNodes)/params$k))
    write(c(paste(cl,"1",paste(splits[[1]],collapse = "\t"),sep="\t")),file=outputFileName,append=T)
  }
  
}
