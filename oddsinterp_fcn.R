oddsinterp <- function(snp){
  #to yield OR
  orstr <- gwas.catalog[getrow(snp), 31]
  OR <- as.numeric (orstr)
  
  #to interpret and return OR
  if (OR >1){
    if(getraf(snp) > 0.5){
      OR <- 1/OR
      cat("raf > 0.5; user is advised to investigate protective allele properties. presence of protective allele is associated with", round(OR, 3), "times less chance of disease incidence.", " ")
    }
   
  }   
  else
    {cat("Odds ratio =", OR)
    }
  }
