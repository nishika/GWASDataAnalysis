
url <- 'http://www.ncbi.nlm.nih.gov/pubmed/24993907'
getcontrols = function(url){

  #method: returns number closest to the word "controls"; best suited for NCBI; cannot be used for source europe pmc.    
  
  v11 <- c(exabs(url)) #make sure this matches input to getcontrols()
  startcont=regexpr("controls", v11)#returns character no. where "cont" starts
  
  vector14 <- c(startcont)
  startcont <- tail(sort(vector14), 1)
  n <- toString(v11)
  
  string1 <- substr (n, startcont-20, startcont+20) #makes a substring 20 characters around beginning letter "c" of "cases".
  
  if(startcont == -1){
    print("Error: keyword 'controls' is not specified in abstract.")  #give an error message if keyword "controls" can't be located. 
  }else{
      
    
     vector <- numeric(0)
    vector2 <- numeric(0)
    
    v1<- c(unique(na.omit(as.numeric(unlist(strsplit(unlist(string1), "[^0-9]+"))))))  #makes a vector of all the numbers in the substring. 
    length1<- length(unique(v1))  
    for(i in 1:length1){                  #extract each value in the vector v1; do this for the number of values that there are. 
      string2 <- toString(v1[i])          #convert each vector value to a string.  
      
      count <- regexpr(string2, n)        #find the beginning character of each vector value in the original string n. use original n because this is what startcase is in terms of. 
      vector <- c(vector, count)          #add each beginning character to a vector.
      
    }
    length2 <- length(unique(vector))      #get the length of the vector ("vector") containing all the characters. 
    for(i in 1:length2){                   #for each value in "vector"...
      vector2 <- c(vector2, startcont- vector[i])   #find the distance of the value from startcase; assign this distance to a vector. + is to left, - is to right. 
      
      
    }
    
    vector3 <- abs(vector2)  #because some distances are negative, take absval of all values in vector2. let "vector3" contain absval distances.  
  
    vector4 <- tail(sort(vector3),length(unique(vector2))) #assign to vector4 the following: the sorted vector3. since sort goes least to greatest, this needs to be applied to the entire set of values there are in vector2, so i calculate the length of vector2.   
    valuefin <- vector4 [1]    #from vector4, extract the 1st value; this will be the least number, correspondnig to the least distance. 
    
    
    thing <- 0
    pick = ""
    stringlast = ""
    for (i in 1:length(unique(vector2))){   #this is a counter function that helps determine whether a valuefin was originally positive or negative in vector2. if valuefin matches a value in vector2, this most likely means that valuefin is a positive number that was positive to begin with. in this case, increase "thing" by 1.
      if (vector2 [i] == valuefin)
        thing <- thing + 1
    }
      
    
    if (thing == 0){
      pick<- substr(n,startcont, startcont+valuefin+6)   #if thing = 0, then valuefin did NOT match anything in vector, so valuefin must have originally been negative. negative distances from startcase occur after startcase. thus i use a new substring to search from startcase to startcase+valuefin, which is the start position of the desired number. i use +6 to accommodate for the full length of the number. 
      stringlast <- str_extract(pick, "[0-9,.]+")   #i extract all numbers from substring "pick".
                    
  } else{
      pick<- substr(n, startcont-valuefin-6, startcont)  #follows the same logic as the if statement; if thing !=0, valuefin must have been originally positive. Get a string going left instead of right. 
      pick
      stringlast <- str_extract(pick, "[0-9,.]+")
      stringlast
    }
    
    
    controls <- as.numeric(stringlast)   #convert the string (containing the smallest distance to the "c" of "controls") into a numeric variable.
    controls      #output number of controls. 
  
    }
    
}





