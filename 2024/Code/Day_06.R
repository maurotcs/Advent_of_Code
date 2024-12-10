


# --- Day 6: Guard Gallivant ---
# Predict the path of the guard. How many distinct positions will the guard visit before leaving the mapped area?

input = sapply(readLines('../Input/input_06.txt'), strsplit, split = '')
length(input)
length(input[[1]])

# find all the obstacles
hashs = sapply(input, function(x) which(x == '#'))
linhas = colunas = c()
for(i in seq_along(hashs)){
  # find the initial position of the guard
  guard = which(input[[i]] == '^')
  if(length(guard) > 0){
    guard.c = guard[1]
    guard.l = i
  }
  
  # is there an obstacle in this line?
  if(length(hashs[[i]]) == 0) next
  
  # get the index of the obstacles
  linhas = append(linhas, rep(i, length(hashs[[i]])))
  colunas = append(colunas, hashs[[i]])
}
# make it into a data frame
obstacles = data.frame(linhas, colunas)

follow_the_guard = function(ind.c, ind.l, obst, map, direction){
  
  while(direction != 'exit'){
    # find path
    if(direction == 'front'){
      ll = (obst[['linhas']] == ind.l)
      cc = sort(obst[ll, 'colunas'], decreasing = FALSE)
      foo = which(cc > ind.c)
      if(length(foo) > 0){
        new.ll = ind.l
        new.cc = cc[foo[1]]-1
        direction = 'down'
      } else{
        new.ll = ind.l
        new.cc = length(map[[1]])
        direction = 'exit'
      }
    } else if(direction == 'back'){
      ll = (obst[['linhas']] == ind.l)
      cc = sort(obst[ll, 'colunas'], decreasing = TRUE)
      foo = which(cc < ind.c)
      if(length(foo) > 0){
        new.ll = ind.l
        new.cc = cc[foo[1]]+1
        direction = 'up'
      } else{
        new.ll = ind.l
        new.cc = 1
        direction = 'exit'
      }
    } else if(direction == 'up'){
      cc = (obst[['colunas']] == ind.c)
      ll = sort(obst[cc, 'linhas'], decreasing = TRUE)
      foo = which(ll < ind.l)
      if(length(foo) > 0){
        new.ll = ll[foo[1]]+1
        new.cc = ind.c
        direction = 'front'
      } else{
        new.ll = 1
        new.cc = ind.c
        direction = 'exit'
      }
    } else if(direction == 'down'){
      cc = (obst[['colunas']] == ind.c)
      ll = sort(obst[cc, 'linhas'], decreasing = FALSE)
      foo = which(ll > ind.l)
      if(length(foo) > 0){
        new.ll = ll[foo[1]]-1
        new.cc = ind.c
        direction = 'back'
      } else{
        new.ll = length(map)
        new.cc = ind.c
        direction = 'exit'
      }
    }
    
    update = data.frame(ll = ind.l:new.ll, cc = ind.c:new.cc)
    for(i in 1:nrow(update)){
      map[[update[i, 'll']]][update[i, 'cc']] = 'X'
    }
    
    ind.c = new.cc
    ind.l = new.ll
    
  }
  
  
  return(map)
}

res = follow_the_guard(ind.c = guard.c, ind.l = guard.l, obst = obstacles, map = input, direction = 'up')
sum(sapply(res, function(x) x=='X')) # 4602



#--------------------------------------------------------------------------
# --- Part Two ---


find_the_loop = function(ind.c, ind.l, obst, direction, n.repeat = 32){
  
  rep = 0
  path = list()
  count = 1
  while(direction != 'exit'){
    # find path
    if(direction == 'front'){
      ll = (obst[['linhas']] == ind.l)
      cc = sort(obst[ll, 'colunas'], decreasing = FALSE)
      foo = which(cc > ind.c)
      if(length(foo) > 0){
        new.ll = ind.l
        new.cc = cc[foo[1]]-1
        direction = 'down'
      } else{
        direction = 'exit'
      }
    } else if(direction == 'back'){
      ll = (obst[['linhas']] == ind.l)
      cc = sort(obst[ll, 'colunas'], decreasing = TRUE)
      foo = which(cc < ind.c)
      if(length(foo) > 0){
        new.ll = ind.l
        new.cc = cc[foo[1]]+1
        direction = 'up'
      } else{
        direction = 'exit'
      }
    } else if(direction == 'up'){
      cc = (obst[['colunas']] == ind.c)
      ll = sort(obst[cc, 'linhas'], decreasing = TRUE)
      foo = which(ll < ind.l)
      if(length(foo) > 0){
        new.ll = ll[foo[1]]+1
        new.cc = ind.c
        direction = 'front'
      } else{
        direction = 'exit'
      }
    } else if(direction == 'down'){
      cc = (obst[['colunas']] == ind.c)
      ll = sort(obst[cc, 'linhas'], decreasing = FALSE)
      foo = which(ll > ind.l)
      if(length(foo) > 0){
        new.ll = ll[foo[1]]-1
        new.cc = ind.c
        direction = 'back'
      } else{
        direction = 'exit'
      }
    }
    
    update = data.frame(ll = ind.l:new.ll, cc = ind.c:new.cc)
    ind.c = new.cc
    ind.l = new.ll
    
    if(any(sapply(path, function(x) identical(x, update)))){
      rep = rep+1
      if(rep >= n.repeat) break
    } else{
      rep = 0
    }
    path[[count]] = update
    count = count+1
  }
  
  
  
  if(direction == 'exit'){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

perfect_loops = list()
obst = obstacles
for(ll in 1:length(input)){
  # cat('\n\nlinha: ', ll, ':\n')
  perfect_loops[[ll]] = logical(length(input[[ll]]))
  for(cc in 1:length(input[[ll]])){
    # cat(cc, '-\t')
    # if it already has an obstacle, goes to the next!
    if(input[[ll]][cc] == '#' | input[[ll]][cc] == '^'){
      # cat('SKIP\t')
      bool = FALSE
    } else{
      obst[nrow(obstacles)+1, ] = c(ll, cc)
      
      bool = find_the_loop(ind.c = guard.c, ind.l = guard.l, obst = obst, direction = 'up', n.repeat = 32)
    }
    
    perfect_loops[[ll]][cc] = bool
    
    # cat(bool, '\t')
  }
}


perfect_loops
sum(sapply(perfect_loops, sum))




