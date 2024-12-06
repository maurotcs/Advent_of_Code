

# --- Day 11: Seating System ---
# Simulate your seating area by applying the seating rules repeatedly until no seats change state. How many seats end up occupied?


input = readLines('Input/input_11.txt')
input = sapply(input, strsplit, split='')

find_neighbours = function(ind.c, ind.l, max.c, max.l){
  ll = ind.l
  cc = ind.c
  
  if(ind.c > 1){
    cc = append(cc, ind.c-1)
  }
  if(ind.c < max.c){
    cc = append(cc, ind.c+1)
  }
  if(ind.l > 1){
    ll = append(ll, ind.l-1)
  }
  if(ind.l < max.l){
    ll = append(ll, ind.l+1)
  }
  
  nghbrs = expand.grid(ll, cc)
  nghbrs = nghbrs[!apply(nghbrs, 1, function(x) x[1]==ind.l&x[2]==ind.c), ]
  names(nghbrs) = c('lines', 'columns')
  
  return(nghbrs)
}

chair_dancing = function(layout){
  chairs = sapply(input, function(x) which(x == 'L'), USE.NAMES = FALSE)
  max.l = length(input)
  max.c = length(input[[1]])
  
  while(1){
    changes = data.frame('ll'=c(), 'cc'=c(), 'new.value'=c())
    count = 0
    for(i in 1:length(chairs)){
      for(j in chairs[[i]]){
        neigh = find_neighbours(ind.l = i, ind.c = j, max.l=max.l, max.c=max.c)
        # If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
        if(layout[[i]][j]=='L' &)
        # If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty. Otherwise, the seat's state does not change.
        
      }
    }
    if(count == 0){
      break
    } else{
      
    }
  }
  
  
  return(layout)
}