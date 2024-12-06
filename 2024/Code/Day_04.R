

# --- Day 4: Ceres Search ---
# Take a look at the little Elf's word search. How many times does XMAS appear?

input = readLines('../Input/input_04.txt')

# processing
lines = sapply(input, strsplit, split='')

# find the beginings of Xmas!!
allX = sapply(lines, function(x) which(x=='X'))
# initialize the vectors of indices
linhas = colunas = c()
# loop to find and store the indices of all the Xs
for(i in 1:length(lines)){
  if(is.na(allX[[i]][[1]])) next
  linhas = append(linhas, rep(i, length(allX[[i]])))
  colunas = append(colunas, allX[[i]])
}
# creates a data frame of the indices
ind = data.frame(linhas, colunas)

# series of functions to solve the problem!!
find_all_Xmas = function(reference, indices, max.c, max.l){
  default = 'XMAS'
  res = 0
  for(i in 1:nrow(indices)){
    # get the indices
    ind.l = indices[i, 'linhas']
    ind.c = indices[i, 'colunas']
    
    # make all the comparisons
    foward = (ind.c+3 <= max.c)
    back = (ind.c-3 >= 1)
    up = (ind.l-3 >= 1)
    down = (ind.l+3 <= max.l)
    
    if(foward){
      w = walk_foward(ind.l, ind.c, reference)
      res = res+(w == default)
    }
    if(back){
      w = walk_back(ind.l, ind.c, reference)
      res = res+(w == default)
    }
    if(up){
      w = walk_up(ind.l, ind.c, reference)
      res = res+(w == default)
    }
    if(down){
      w = walk_down(ind.l, ind.c, reference)
      res = res+(w == default)
    }
    if(foward & up){ # first quadrant 0(+,+)
      w = walk_QI(ind.l, ind.c, reference)
      res = res+(w == default)
    }
    if(back & up){ # second quadrant 0(-,+)
      w = walk_QII(ind.l, ind.c, reference)
      res = res+(w == default)
    }
    if(back & down){ # third quadrant 0(-,-)
      w = walk_QIII(ind.l, ind.c, reference)
      res = res+(w == default)
    }
    if(foward & down){ # fourth quadrant 0(+,-)
      w = walk_QIV(ind.l, ind.c, reference)
      res = res+(w == default)
    }
  }
  
  return(res)
}
walk_foward = function(ll, cc, ref){
  letters = ref[[ll]][cc:(cc+3)]
  word = paste0(letters, collapse='')
  return(word)
}
walk_back = function(ll, cc, ref){
  letters = ref[[ll]][cc:(cc-3)]
  word = paste0(letters, collapse='')
  return(word)
}
walk_up = function(ll, cc, ref){
  letters = c(ref[[ll]][cc], ref[[ll-1]][cc],
              ref[[ll-2]][cc], ref[[ll-3]][cc])
  word = paste0(letters, collapse='')
  return(word)
}
walk_down = function(ll, cc, ref){
  letters = c(ref[[ll]][cc], ref[[ll+1]][cc],
              ref[[ll+2]][cc], ref[[ll+3]][cc])
  word = paste0(letters, collapse='')
  return(word)
}
walk_QI = function(ll, cc, ref){ # first quadrant 0(+,+)
  letters = c(ref[[ll]][cc], ref[[ll-1]][cc+1],
              ref[[ll-2]][cc+2], ref[[ll-3]][cc+3])
  word = paste0(letters, collapse='')
  return(word)
}
walk_QII = function(ll, cc, ref){ # second quadrant 0(-,+)
  letters = c(ref[[ll]][cc], ref[[ll-1]][cc-1],
              ref[[ll-2]][cc-2], ref[[ll-3]][cc-3])
  word = paste0(letters, collapse='')
  return(word)
}
walk_QIII = function(ll, cc, ref){ # third quadrant 0(-,-)
  letters = c(ref[[ll]][cc], ref[[ll+1]][cc-1],
              ref[[ll+2]][cc-2], ref[[ll+3]][cc-3])
  word = paste0(letters, collapse='')
  return(word)
}
walk_QIV = function(ll, cc, ref){ # fourth quadrant 0(+,-)
  letters = c(ref[[ll]][cc], ref[[ll+1]][cc+1],
              ref[[ll+2]][cc+2], ref[[ll+3]][cc+3])
  word = paste0(letters, collapse='')
  return(word)
}

find_all_Xmas(reference = lines, indices = ind,
              max.c = length(lines[[1]]), max.l = length(lines))
# 2567



#------------------------------------------------------------------
# --- Part Two ---
# Flip the word search from the instructions back over to the word search side and try again. How many times does an X-MAS appear?

# find the As!!
allA = sapply(lines, function(x) which(x=='A'))
# initialize the vectors of indices
linhasA = colunasA = c()
# loop to find and store the indices of all the As
for(i in 1:length(lines)){
  if(is.na(allA[[i]][[1]])) next
  linhasA = append(linhasA, rep(i, length(allA[[i]])))
  colunasA = append(colunasA, allA[[i]])
}
# creates a data frame of the indices
indA = data.frame('linhas'=linhasA, 'colunas'=colunasA)

# THE function to solve the problem!!
find_all_X_MAS = function(reference, indices, max.c, max.l){
  defaults = c('MS', 'SM')
  res = 0
  for(i in 1:nrow(indices)){
    # get the indices
    ind.l = indices[i, 'linhas']
    ind.c = indices[i, 'colunas']
    
    #cat('Indices--\tcol:', ind.c, '\trow:', ind.l, '\n')
    
    # make the comparisons
    teto = ind.l>1
    chao = ind.l<max.l
    esquerda = ind.c>1
    direita = ind.c<max.c
    
    if(all(teto, chao, esquerda, direita)){
      # the diagonal 0(+,+)
      QI = c(reference[[ind.l+1]][ind.c-1], reference[[ind.l-1]][ind.c+1])
      testQI = (paste0(QI, collapse='') %in% defaults)
      # diagonal 0(-,+)
      QII = c(reference[[ind.l-1]][ind.c-1], reference[[ind.l+1]][ind.c+1])
      testQII = (paste0(QII, collapse='') %in% defaults)
      
      #cat('TEXT--\tQI:', paste0(QI, collapse=''), '\tQII:', paste0(QII, collapse=''), '\tACCEPTED:', all(testQI, testQII), '\n\n')
      
      res = res+all(testQI, testQII)
    }
  }
  
  return(res)
}


find_all_X_MAS(reference = lines, indices = indA,
               max.c = length(lines[[1]]), max.l = length(lines))
# 2029
