

# --- Day 2: Red-Nosed Reports ---
# Analyze the unusual data from the engineers. How many reports are safe?

input = readLines('../Input/input_02.txt')
head(input)
tail(input)

# processing
list.reports = sapply(input, strsplit, split=' ')
reports = lapply(list.reports, as.numeric)
check_validity = function(x){
  diffs = diff(x)
  
  if(any(diffs == 0)){
    return(0)
  }
  
  if(all(diffs > 0) | all(diffs < 0)){ # test for monotonic change
    diffs = abs(diffs)
  } else{
    # The levels are either all increasing or all decreasing
    return(99)
  }
  
  if(all(diffs >= 1) & all(diffs <= 3)){ # test for magnitute of change
    return(1)
  } else{
    # Any two adjacent levels differ by at least one and at most three.
    return(-99)
  }
}

# Number of SAFE reports
safety = sapply(reports, check_validity)
sum(safety == 1) # 213



#---------------------------------------------------------------------
# --- Part Two ---
# Update your analysis by handling situations where the Problem Dampener can remove a single level from unsafe reports. How many reports are now safe?

problem_dampener = function(x){
  safety = check_validity(x)
  if(safety == 1){
    return(safety)
  }
  
  diffs = diff(x)
  
  # Any two adjacent levels differ by ZERO
  if(safety == 0){
    ind = which(diffs == 0)
  }
  
  # The levels are either all increasing or all decreasing
  if(safety == 99){
    neg = sum(diffs < 0)
    pos = sum(diffs > 0)
    
    if(neg >= pos){
      ind = which(diffs > 0)
    } else{
      ind = which(diffs < 0)
    }
  }
  
  # Any two adjacent levels differ by at least one and at most three.
  if(safety == -99){
    diffs = abs(diffs)
    ind1 = which(diffs < 1)
    ind3 = which(diffs > 3)
    ind = sort(c(ind1, ind3))
  }
  
  # test the 2 options
  y = x[-(ind[1]+1)]
  safety = check_validity(y)
  if(safety != 0){
    y = x[-(ind[1])]
    safety = check_validity(y)
  }
  
  return(safety)
}


# Number of SAFE reports with DAMPENERS
damp = sapply(reports, problem_dampener)
sum(damp == 1) # 263




brute_force = function(x){
  safety = check_validity(x)
  if(safety == 1){
    return(safety)
  }
  
  for(i in 1:length(x)){
    y = x[-i]
    safety = check_validity(y)
    if(safety == 1) break
  }
  
  return(safety)
}
damp = sapply(reports, brute_force)
sum(damp == 1) # 285
