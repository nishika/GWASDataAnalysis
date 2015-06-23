getraf= function(snp){
  #to yield raf
  rafstr<- gwas.catalog[getrow(snp), 27]
  raf<- as.numeric(rafstr)
  return(raf)
}