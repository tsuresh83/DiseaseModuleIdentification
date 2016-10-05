library(needs)
needs(data.table,Rtsne,clue,cluster,dbscan)
rm(list=ls())
args = commandArgs(trailingOnly=TRUE)
f <- T
scriptName <- "EmbeddingSimilaritiesDbscanOptics"
params <- list(algo="optics",eps=0.7,minPts=30)
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
original <- fread(originalFN)
embeddingFileName <- unlist(strsplit(embeddingFN,"/"))
embeddingFileName <- embeddingFileName[length(embeddingFileName)]
embeddingFileName <- unlist(strsplit(embeddingFileName,"_wowts"))
embeddingFileName <- embeddingFileName[1]
outputDir <- paste(outputDir,"/",scriptName,"_",embeddingAlgo,"/",paste(names(params),params,sep="_",collapse="_"),sep="")
dir.create(outputDir,recursive = T)
outputFileName <- paste(outputDir,"/",embeddingFileName,".txt",sep="")
if(params$algo=="dbscan"){
  dbs <- dbscan(embedding[,2:ncol(embedding),with=F],eps = params$eps,minPts = params$minPts)
  embedding$Cluster <- dbs$cluster
}else if(params$algo=="optics"){
  params <- list(algo="optics",eps=0.7,minPts=40)
  opt <- optics(embedding[,2:ncol(embedding),with=F],eps=params$eps,minPts = params$minPts)
  plot(opt)
  #embedding$Cluster <- dbs$cluster
  for(i in seq(0.05,0.69,0.05)){
    
  }
}
# clCtr <- (max(dbs$cluster)+1)
# originalNodes <- union(original$V1,original$V2)
# for(cl in unique(embedding$Cluster)){
#   #tmp <- as.character(unlist(embedding[Cluster==c,"V1",with=F]))
#   tmp <- embedding[Cluster==cl,"V1",with=F]
#   clNodes <- intersect(tmp$V1,originalNodes)
#   splits <- split(clNodes,ceiling(seq_along(clNodes)/params$k))
#   for(x in names(splits)){
#     if(x==1){
#       write(c(paste(cl,"1",paste(splits[[x]],collapse = "\t"),sep="\t")),file=outputFileName,append=T)  
#     }else{
#       clCtr <- clCtr+1
#       write(c(paste(clCtr,"1",paste(splits[[x]],collapse = "\t"),sep="\t")),file=outputFileName,append=T)  
#     }
#   }
#   #tmp <- paste(intersect(tmp$V1,originalNodes),collapse="\t")
#   #write(c(paste(cl,"1",tmp,sep="\t")),file=outputFileName,append=T)
# }
# #close(fileCon)
