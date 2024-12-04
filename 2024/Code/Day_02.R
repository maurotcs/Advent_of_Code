

# --- Day 2: Red-Nosed Reports ---
# Fortunately, the first location The Historians want to search isn't a long walk from the Chief Historian's office.
# While the Red-Nosed Reindeer nuclear fusion/fission plant appears to contain no sign of the Chief Historian, the engineers there run up to you as soon as they see you. Apparently, they still talk about the time Rudolph was saved through molecular synthesis from a single electron.
# They're quick to add that - since you're already here - they'd really appreciate your help analyzing some unusual data from the Red-Nosed reactor. You turn to check if The Historians are waiting for you, but they seem to have already divided into groups that are currently searching every corner of the facility. You offer to help with the unusual data.
# The unusual data (your puzzle input) consists of many reports, one report per line. Each report is a list of numbers called levels that are separated by spaces. For example:
# 7 6 4 2 1
# 1 2 7 8 9
# 9 7 6 2 1
# 1 3 2 4 5
# 8 6 4 4 1
# 1 3 6 7 9

# This example data contains six reports each containing five levels.
# The engineers are trying to figure out which reports are safe. The Red-Nosed reactor safety systems can only tolerate levels that are either gradually increasing or gradually decreasing. So, a report only counts as safe if both of the following are true:
# The levels are either all increasing or all decreasing.
# Any two adjacent levels differ by at least one and at most three.
# In the example above, the reports can be found safe or unsafe by checking those rules:
# 7 6 4 2 1: Safe because the levels are all decreasing by 1 or 2.
# 1 2 7 8 9: Unsafe because 2 7 is an increase of 5.
# 9 7 6 2 1: Unsafe because 6 2 is a decrease of 4.
# 1 3 2 4 5: Unsafe because 1 3 is increasing but 3 2 is decreasing.
# 8 6 4 4 1: Unsafe because 4 4 is neither an increase or a decrease.
# 1 3 6 7 9: Safe because the levels are all increasing by 1, 2, or 3.

# So, in this example, 2 reports are safe.
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
# The engineers are surprised by the low number of safe reports until they realize they forgot to tell you about the Problem Dampener.
# The Problem Dampener is a reactor-mounted module that lets the reactor safety systems tolerate a single bad level in what would otherwise be a safe report. It's like the bad level never happened!
# Now, the same rules apply as before, except if removing a single level from an unsafe report would make it safe, the report instead counts as safe.
# More of the above example's reports are now safe:
# 7 6 4 2 1: Safe without removing any level.
# 1 2 7 8 9: Unsafe regardless of which level is removed.
# 9 7 6 2 1: Unsafe regardless of which level is removed.
# 1 3 2 4 5: Safe by removing the second level, 3.
# 8 6 4 4 1: Safe by removing the third level, 4.
# 1 3 6 7 9: Safe without removing any level.

# Thanks to the Problem Dampener, 4 reports are actually safe!
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
