# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

MonoFunc=function(input,mean.trans=F,linear.trans=F){
  data(ClockData,package="MonoDNAmAge")
  MonoData=ClockData$MonoData
  probe.gold=MonoData$cg[-1]
  miss.idx=which(!probe.gold %in% rownames(input))
  if (length(miss.idx)>0){
    miss.probe=probe.gold[miss.idx]
    print(paste0(length(miss.idx)," Missing Probes: ",paste(miss.probe,collapse=" ")))
    miss.probe.est=matrix(rep(MonoData[miss.probe,]$cg.mean,each=ncol(input)), ncol=ncol(input), byrow=TRUE)
    rownames(miss.probe.est)=miss.probe
    colnames(miss.probe.est)=colnames(input)
    miss.probe.est=as.data.frame(miss.probe.est)
    input.rv=rbind(input,miss.probe.est)
  }else{
    input.rv=input
  }
  input.rv=input.rv[probe.gold,]
  ##----------------------------------------------------------------##
  stopifnot(all(rownames(input.rv)==probe.gold))
  na.num=apply(input.rv, 1, function(x) sum(is.na(x)))
  na.pro=rownames(input.rv)[which(na.num>0)]
  if(length(na.pro)>0){
    for(i in 1:length(na.pro)){
      prb=na.pro[i]
      input.rv[prb,is.na(input.rv[prb,])]=MonoData[prb,]$cg.mean
    }
  }
  ##----------------------------------------------------------------##
  if(mean.trans){
    mu1=rowMeans(input.rv)
    mu2=MonoData[-1,]$cg.mean
    input.rv1=input.rv-mu1+mu2
  }else{
    input.rv1=input.rv
  }
  methy_gold=t(input.rv1)
  stopifnot(all(colnames(methy_gold)==probe.gold))
  predi_gold=cbind(rep(1,nrow(methy_gold)),methy_gold) %*% MonoData$coef
  if(linear.trans){
    predi_gold.out=0.877764024218443*predi_gold+17.9255109926324
  }else{
    predi_gold.out=predi_gold
  }
  return(predi_gold.out[,1])
}
