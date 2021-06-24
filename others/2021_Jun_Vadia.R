

dat <- data.frame(x=seq.Date(as.Date("2020-11-01"), as.Date("2020-11-22"), "day"),
                  y=c(0,0,0,0,2,5,0,18,0,0,0,0,0,0,7,21,8,0,0,0,0,0),
                  z=c(1,1,1,1,1,1,1,1,3,7,8,8,8,8,8,8,8,9,10,13,14,14))

test = c(0,0,12,23,42,0,0,1,23,0,2,1,10,0,0,0,0)


# Primeiramente umas definicoes:
# Em y, eu defino como um "evento" todos os valores entre 3 zeros consecutivos. No exemplo tem 2 eventos: "2, 5, 0, 18" e "7, 21, 8".
#---- Review 'find_events' function ----
# Shameless' function
find_events <- function(x){
  
  zeros <- which(x  == 0)
  
  event_start <- zeros[which(diff(zeros)>2)]+1
  
  event_length <- vector()
  
  for(i in event_start){
    
    counter <-0
    
    while(!(x[i+counter]==0 & x[i+counter+1]==0 & x[i+counter+2]==0)){
      
      counter = counter + 1
      
    }
    
    event_length <- c(event_length, counter)
    
  }
  
  return(cbind(event_start, event_length))
  
}
# works perfectly for Vadia's dataset
find_events(dat$y)
# but not in general
test = c(0,0,12,23,42,0,0,1,23,0,2,1,10,0,0,0,0)
find_events(test) # there's NO valid sequence
#       event_start event_length
# [1,]           3           11
# [2,]           8            6
# [3,]          11            3

# that's bc the 3rd command, contrary to the intention, does not find the start of a valid sequence
# 'which(diff(zeros)>2)' selects any sequence of at least 2 consecutive non-zeros surrounded by at least 1 zero

#---- New 'find_events' function ----
# I made my own 'find_events' function
# the MAIN function, which returns a list of 2 obj:
#       - indexes, a data.frame with first and last index (from the input) for all valid sequences
#       - sequences, a list with all the corresponding valid sequences
find_events = function(input){
  # let's build the skeleton of the output object
  # data.frame with the indexes of valid sequences
  indexes = data.frame(first = numeric(),
                       last = numeric())
  # list with the valid sequences extracted
  sequences = list()
  # they'll be combined later
  
  zeros = find_zeros(input) # finds all the sequences of at least 3 subsequent zeros
  nr = nrow(zeros) # get the number of rows in 'zeros'; ie, number of "valid sequences of zeros" -> our markers
  if(nr < 2){ # i.e., there's no sequence surrounded by 3+ ZEROs!
    warning("We found NO valid sequences (i.e., there's no sequence surrounded by 3+ ZEROs)")
  } else{ # we got something!! YAY
    for(i in 2:nr){
      ind = (i-1)
      indexes[ind, 'first'] = zeros[ind, 'last']+1
      indexes[ind, 'last'] = zeros[i, 'first']-1
      sequences[[ind]] = input[c(indexes$first[ind]:indexes$last[ind])]
    }
  }
  
  out = list('indexes' = indexes, 'sequences' = sequences)
  return(out)
}
# auxiliary function that finds all sequences with at least 3 Zeros
find_zeros = function(x){ # find sequences of at least 3 zeros
  # our indexes for the zero sequences
  zeros = data.frame(first = numeric(),
                     last = numeric())
  # variables for the loop
  ll = length(x) # stop condidition
  bool = (x == 0) # faster than checking every entry individually
  n0 = 0 # initialize the counter of subsequent 0s
  for(i in 1:ll){
    if(bool[i]){ # if the entry is a ZERO
      n0 = n0+1
    } else{
      if(n0 >= 3){ # do we have enough ZEROs (ie, at least 3)??
        # if YES, then store the indexes of the sequence of ZEROs!
        zeros[nrow(zeros)+1, ] = c(i - n0, i - 1)
      }
      # restart the counter variable
      n0 = 0
    }
  }
  # we have to check the last row!
  if(n0 >= 3){ # do we have enough ZEROs (ie, at least 3)??
    # if YES, then store the indexes of the sequence of ZEROs!
    zeros[nrow(zeros)+1, ] = c(i - n0 + 1, i)
  }
  
  return(zeros)
}

# they seem to work...
find_events(test)
find_events(c(0,0,1,2,33,9,0,9,7,0,32,0,0,0,12,0,0,0,23,1,0))
find_events(dat$y)
#----
# Em z, eu considero um aumento quando um numero eh pelo menos 10% maior que a media dos ultimos 3 dias.
#---- 'aumentou_uai' function ----
# MAIN function, returns a Boolean vector with the same length as input; TRUE for "AUMENTO", FALSE, otherwise
#       - nDays, controls the number of previous days included in the estimation of the mean; default: nDays = 3
#       - na.rm, should we remove NAs when estimating the mean? Default = FALSE
#       - cutoff, the difference considered valid; default: cutoff = 110
aumentou_uai = function(input, nDays = 3, na.rm = FALSE, cutoff = 110){
  ll = length(input)
  out = logical(ll) # initialize everything as FALSE
  for(i in (nDays+1):ll){ # we cannot calculate a "valid mean" for the first 3 (or 'nDays') entries
    this.mean = mean( c(input[(i-nDays):(i-1)]) , na.rm = na.rm)
    if(this.mean == 0){
      warning("There's a mean with value ZERO")
    } else{
      estimate = (input[i]*100) / this.mean # estimate the difference to the mean
      if(estimate >= cutoff){ # ie, 'today' is at least 10% higher (or 'cutoff') than the mean
        out[i] = TRUE
      }
    }
  }
  
  
  return(out)
}
dat$z
dat$z[aumentou_uai(dat$z)]
test
test[aumentou_uai(test)]

#----
#---- Let's get down to business!! The 'Bitch' function ----
# A couple of auxiliary functions
getB = function(x){ # solve item b
  #       b) numero de dias entre o comeco do evento em y e o primeiro aumento em z.
  aux = which(dat$aumentou[-c(1:x)])
  out = ifelse(length(aux) == 0, NA, aux[1])
  out
}
getC = function(){ # solve item c
  #       c) numero de dias entre o MAIOR valor dentro dos eventos em y e o maior valor em z ANTES do proximo evento.
  maior = sapply(sequences, which.max) + indexes$first - 1
  out = numeric()
  for(i in 1:length(maior)){
    if(i < length(maior)){
      final = ( indexes[i+1, 'first'] - 1 )
    } else{ # do the same for the last row
      # considers there'd an event happenning the day after the last sample
      final = nrow(dat)
    }
    aux = dat$z[(maior[i]+1):final]
    out[i] = which.max(aux) # considers ONLY the first entry with maximum value
  }
  
  
  return(out)
}
getD = function(){ # solve item d
  #       d)diferenca entre o valore maximo em z depois do evento e a media de z nos 3 dias antes do evento
  meanZ = apply(indexes, MARGIN = 1, FUN = function(vec){
    if(vec[1] < 3){
      return(NA) 
    } else{
      return(mean(dat$z[(vec[1]-3):(vec[1]-1)]) )
    }
  })
  maxZ = sapply(indexes$last, FUN = function(x) which.max(dat$z[-c(1:x+1)]))
  
  out = ( maxZ - as.numeric(meanZ) )
  return(out)
}
# So NOW we can make the MASTER function that'll output the desired data.frame
Bitch = function(dat, # expects something EXACTLY like the example data.frame
                 nDays = 3, na.rm = FALSE, cutoff = 110 # for the 'aumentou_uai' function
                 ){
  # initialize the object with the output
  bitching = data.frame(event_begin = as.Date('1666-01-01'),
                        a = -999, b = -999, c = -999, d = -999)
  
  # adds a boolean column to the input dataset, with 'z' values that pass the cutoff
  dat$aumentou = aumentou_uai(dat$z, nDays = nDays, na.rm = na.rm, cutoff = cutoff)
  
  # finds all valid sequences
  aux = find_events(dat$y)
  indexes = aux$indexes
  sequences = aux$sequences
  if(nrow(indexes) == 0){ # ie, there's NO valid sequences
    warning("We found NO valid sequences (i.e., there's no sequence surrounded by 3+ ZEROs")
    # exits the function
    return(bitching)
  }
  # otherwise, we store the data on the events
  bitching[1:nrow(indexes), 'event_begin'] = dat[indexes$first, 'x']
  
  
#       a) soma dos valores do evento
  bitching[ , 'a'] = sapply(sequences, FUN = sum)
  
#       b) numero de dias entre o comeco do evento em y e o primeiro aumento em z.
  bitching[ , 'b'] = sapply(indexes$first, FUN = getB)

#       c) numero de dias entre o MAIOR valor dentro dos eventos em y e o maior valor em z ANTES do proximo evento.
  bitching[ , 'c'] = getC()

#       d)diferenca entre o valore maximo em z depois do evento e a media de z nos 3 dias antes do evento
  bitching[ , 'd'] = getD()


  return(bitching)
}


res = Bitch(dat)
res
# No caso, preciso me retorne o seguinte
#     event_begin   a   b   c   d
# 1   2020-11-05    25  4   3   7
# 2   2020-11-15    36  5   4   6


