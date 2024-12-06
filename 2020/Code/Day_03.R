
# https://adventofcode.com/2020/day/3



# --- Day 3: Toboggan Trajectory ---
#   Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?

input = readLines('Input/input_03.txt')
head(input)
str(input)
tail(input)

Ntrees = function(landscape, right, down){
  # fixed par
  height = length(landscape)
  width = length(strsplit(landscape[1], split = '')[[1]])
  
  # initialize arg
  path = ''
  row = 1 + down
  column = 1 + right
  
  while(row <= height){
    this_row = strsplit(landscape[row], split = '')[[1]]
    rest = column %% width
    this_square = this_row[ifelse(rest == 0, width, rest)]
    
    # path
    path = append(x = path, values = this_square)
    
    # walk
    row = row + down
    column = column + right
  }
  
  trees = sum(path == '#')
  
  return(trees)
}

Ntrees(landscape = input, right = 3, down = 1)
# [1] 292



#--------------------------------
#--- Part Two ---
#   What do you get if you multiply together the number of trees encountered on each of the listed slopes?
slopes = data.frame(right = c(1, 3, 5, 7, 1),
                    down = c(rep(1, 4), 2) )
slopes

all_trees = sapply(X = 1:nrow(slopes), FUN = function(i)
                   Ntrees(landscape = input,
                          right = slopes[i, 1], down = slopes[i, 2])
                   )
all_trees
# [1]  81 292  89 101  44
prod(all_trees)
# [1] 9354744432

# 