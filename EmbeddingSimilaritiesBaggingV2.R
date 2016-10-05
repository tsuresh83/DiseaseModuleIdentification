library(needs)
needs(data.table,clue,cluster,RANN,e1071)
rm(list=ls())
args = commandArgs(trailingOnly=TRUE)
f <- F
scriptName <- "EmbeddingSimilaritiesBaggingV2"
params <- list(algo="cmeans",k=100,maxIter=1000)
if(f){
  originalFN <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge1/data/subchallenge1/1_ppi_anonym_v2_wowts.txt"
  embeddingFN <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge1/data/subchallenge1/embeddings/submission1/1_ppi_anonym_v2_wowts.embedding"
  outputDir <- "/media/3TB/DreamChallenges/DiseaseModuleIdentification/challenge1/Results1"
  clSize <- 100
  embeddingAlgo <- "DEEPWALK"
  clusterAlgo <-"cmeans"
}else{
  originalFN <- args[1]
  embeddingFN <- args[2]
  outputDir <- args[3]
  clSize <- as.integer(args[4])
  embeddingAlgo <- args[5]
  clusterAlgo <- args[6]
}
params$k <- clSize
params$algo <- clusterAlgo
embedding <- fread(embeddingFN,skip=1)
original <- fread(originalFN)
embeddingFileName <- unlist(strsplit(embeddingFN,"/"))
embeddingFileName <- embeddingFileName[length(embeddingFileName)]
embeddingFileName <- unlist(strsplit(embeddingFileName,"_wowts"))
embeddingFileName <- embeddingFileName[1]
outputDir <- paste(outputDir,"/",scriptName,"_",embeddingAlgo,"/",paste(names(params),params,sep="_",collapse="_"),sep="")
dir.create(outputDir,recursive = T)
outputFileName <- paste(outputDir,"/",embeddingFileName,".txt",sep="")
originalNodes <- union(original$V1,original$V2)
if(params$algo=="kmeans"){
  clu <- cl_bag(embedding[,2:ncol(embedding),with=F],B=5,k=as.integer(ceiling(length(originalNodes)/params$k)),algorithm="kmeans",parameters=list(iter.max=params$maxIter,algorithm="Lloyd"))
}else if(params$algo=="pam"){
  clu <- cl_bag(embedding[,2:ncol(embedding),with=F],B=10,k=as.integer(ceiling(length(originalNodes)/params$k)),algorithm="pam")
}else if(params$algo=="cmeans"){
  clu <- cl_bag(embedding[,2:ncol(embedding),with=F],B=10,k=as.integer(ceiling(length(originalNodes)/params$k)),algorithm="cmeans",parameters=list(iter.max=params$maxIter))
}
embedding$Cluster <- cl_class_ids(clu)
clCtr <- as.integer(ceiling(length(originalNodes)/params$k))
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
