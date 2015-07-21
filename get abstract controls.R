#method: returns number closest to the word "controls"; best suited for NCBI; cannot be used for source Europe PMC. 
#This code can be easily modified to return the numerical value closest to any string pattern. Replace "controls" with desired pattern.

url <- 'http://www.ncbi.nlm.nih.gov/pubmed/24999842'  #assign any NCBI abstract to 'url'. 
getcontrols = function(url){

  abstract <- c(exabs(url)) #make sure this matches input to getcontrols()
  startcont=regexpr("controls", abstract) #returns character number where "controls" starts
  
  startvec <- c(startcont)
  startcont <- tail(sort(startvec), 1)
  abstring <- toString(abstract)
  
  abshort <- substr (abstring, startcont-20, startcont+20) #makes a substring 20 characters around beginning letter "c" of "cases". 20 is an arbitrary designation; this number can be modified to narrow or widen the scope of search. 
  
  if(startcont == -1){
    print("Error: keyword 'controls' is not specified in abstract.")  #give an error message if keyword "controls" can't be located. 
  }else{
      
    
     startchar <- numeric(0)
    dist <- numeric(0)
    
    digitvec<- c(unique(na.omit(as.numeric(unlist(strsplit(unlist(abshort), "[^0-9]+"))))))  #makes a vector of all the numbers in the 40 character substring. 
    length_digit<- length(unique(digitvec))  
    for(i in 1:length_digit){                  #extract each value in the vector digitvec; do this for the number of values that there are. 
      digstring <- toString(digitvec[i])          #convert each numeric value from digitvec to a string.  
      
      count <- regexpr(digstring, abstring)        #find the beginning character position (numeric value) of each vector value in the original string abstring. use original 'abstring' because this is what 'startcont' is in terms of. 
      startchar <- c(startchar, count)          #add each beginning character to a vector.
      
    }
    startlength <- length(unique(startchar))      #get the length of the vector "startchar" containing all the characters. 
    for(i in 1:startlength){                   #for each value in "vector"...
      dist <- c(dist, startcont- startchar[i])   #find the distance of the value from 'startcont'; assign this distance to a vector. + is to left, - is to right. 
      
      
    }
    
    absval_dist <- abs(dist)  #because some distances are negative, take the absolute value of all values in 'dist'. 
  
    sortvec <- tail(sort(absval_dist),length(unique(dist))) #assign to 'sortvec' the following: the sorted 'absval_dist'. since sort goes least to greatest, this needs to be applied to the entire set of values there are in 'dist', so calculate the length of 'dist'.   
    valuefin <- sortvec [1]    #from 'sortvec', extract the 1st value; this will be the least number, corresponding to the least distance and thus nearest value to 'startcont'. 
    
    
    counter <- 0
    pick = ""
    stringlast = ""
    for (i in 1:length(unique(dist))){   #this is a counter function that helps determine whether 'valuefin' was originally positive or negative in 'dist'. if 'valuefin' matches a value in 'dist', this most likely means that 'valuefin'> 0 and was positive to begin with. In this case, increase "counter" by 1.
      if (dist [i] == valuefin)
        counter <- counter + 1
    }
      
    
    if (counter == 0){
      pick <- substr(abstring,startcont, startcont+valuefin+6)   #if 'counter' = 0, then valuefin did NOT match anything in 'dist', so valuefin must have originally been negative. Values with negative distances from 'startcont' occur after 'startcont'. A new substring is therefore used to search from startcont to startcont+valuefin, which is the downstream start position of the desired number. I use +6 (arbitrary length; modifiable) to accommodate for the full length of the number. 
      pickdigit <- str_extract(pick, "[0-9,.]+")   #Extract all numbers from substring "pick".
                    
  } else{
      pick<- substr(abstring, startcont-valuefin-6, startcont)  #follows the same logic as the if statement; if thing !=0, valuefin must have been originally positive. Get a string frame to the left instead of right. 
      pick
      pickdigit <- str_extract(pick, "[0-9,.]+")
      pickdigit
    }
    
    
    controls <- as.numeric(pickdigit)   #convert the string (containing the smallest distance to the "c" of "controls") into a numeric variable.
    controls      #output number of controls. 
  
    }
    
}





