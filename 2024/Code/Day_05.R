# --- Day 5: Print Queue ---
# Determine which updates are already in the correct order. What do you get if you add up the middle page number from those correctly-ordered updates?

input = readLines('../Input/input_05.txt')

# processing
div = which(input == '')

# get all the updates
updates = sapply(input[(div+1):length(input)], function(x) as.integer(strsplit(x, split=',')[[1]]), USE.NAMES = FALSE)


# get all the rules
rules = sapply(input[1:(div-1)], function(x) strsplit(x, split='\\|')[[1]], USE.NAMES = FALSE)
rules.df = data.frame('first' = as.integer(rules[1, ]), 'after' = as.integer(rules[2, ]))

correctly_ordered = function(updt, damn_rules){
  # find the rules that are relevant for each update
  bool = apply(damn_rules, 1, function(x) all(x %in% updt), simplify = TRUE)
  # if there isn't one, return TRUE
  if(sum(bool) == 0){
    return(TRUE)
  }
  ind = which(bool)
  # test the update is in accordance to the rules
  for(i in seq_along(ind)){
    f = damn_rules[ind[i], 'first']
    a = damn_rules[ind[i], 'after']
    if(which(updt == f) >= which(updt == a)){
      return(FALSE)
    }
  }
  
  return(TRUE)
}

ordered_updates = sapply(updates, FUN = correctly_ordered, damn_rules = rules.df)
middle = sapply(updates[ordered_updates], function(x) x[ceiling(length(x)/2)])
sum(middle) #6041


#----------------------------------------------------------------------
# --- Part Two ---
# Find the updates which are not in the correct order. What do you get if you add up the middle page numbers after correctly ordering just those updates?


sort_updates = function(updt, damn_rules){
  # find the rules that are relevant for each update
  bool = apply(damn_rules, 1, function(x) all(x %in% updt), simplify = TRUE)
  ind = which(bool)
  # put the update in order
  correct = FALSE
  while(!correct){
    for(i in seq_along(ind)){
      ind.f = which(updt == damn_rules[ind[i], 'first'])
      ind.a = which(updt == damn_rules[ind[i], 'after'])
      if(ind.f >= ind.a){
        updt = append(updt, updt[ind.f], ind.a-1)
        updt = updt[-(ind.f+1)]
        
        correct = correctly_ordered(updt, damn_rules)
        if(correct) break
      }
    }
  }
  
  return(updt)
}


incorrect = updates[!ordered_updates]
corrected_updates = sapply(incorrect, FUN = sort_updates, damn_rules = rules.df)
all(sapply(corrected_updates, FUN = correctly_ordered, damn_rules = rules.df))
new_middle = sapply(corrected_updates, function(x) x[ceiling(length(x)/2)])
sum(new_middle) #4884
