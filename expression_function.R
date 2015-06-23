#check for expression (intron, ncRNA/synonymous)
expression = function (snp){
  missense <- gwas.catalog[getrow(snp), 25]
  if (missense == "missense")
  {
    cat("SNP Context:", missense)
  }
  else if(missense == "cds-syn")
  {
    cat("SNP Context: Synonymous mutation")
  }
  else cat("SNP Context:", missense, "; SNP is not expressed."," ")}
}
