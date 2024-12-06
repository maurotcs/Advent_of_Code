
# https://adventofcode.com/2020/day/10

#--- Day 10: Adapter Array ---
# Find a chain that uses all of your adapters to connect the charging outlet to your device's built-in adapter and count the joltage differences between the charging outlet, the adapters, and your device. What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?

input = as.numeric(readLines('Input/input_10.txt'))
head(input)
summary(input)

count_diff = function(vec){
  vec = sort(vec, decreasing = FALSE)
  vec = c(0, vec, max(vec)+3)
  diffs = diff(vec)
  out = table(diffs)
  
  return(out)
}

res = count_diff(input)
res
prod(res)
# [1] 1998

#-----------------------------------------------------------
# --- Part Two ---
# What is the total number of distinct ways you can arrange the adapters to connect the charging outlet to your device?

adapters = c(0, sort(input, decreasing = FALSE), max(input)+3)
jolts = diff(adapters)

d3 = which(jolts == 3)
jolt1 = list()
j1=1
start=1
for(i in 1:length(d3)){
  if(d3[i] > start){
    jolt1[[j1]] = as.integer(adapters[start:d3[i]])
    j1 = j1+1
  }
  start = d3[i]+1
}

permutations2 = 2^(sapply(jolt1, length)-2)
permutations2[which(permutations2==8)] = 7

prod(permutations2)





















