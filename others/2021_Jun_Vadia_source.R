
find_events = function(input){
  # returns a list of 2 obj:
  #       - indexes, a data.frame with first and last index (from the input) for all valid sequences
  #       - sequences, a list with all the corresponding valid sequences
  
  # builds the skeleton of the output object
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

aumentou_uai = function(input, nDays = 3, na.rm = FALSE, cutoff = 110){
  # returns a Boolean vector with the same length as input; TRUE for "AUMENTO", FALSE, otherwise
  #       - nDays, controls the number of previous days included in the estimation of the mean; default: nDays = 3
  #       - na.rm, should we remove NAs when estimating the mean? Default = FALSE
  #       - cutoff, the difference considered valid; default: cutoff = 110
  
  
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

Bitch = function(dat, # expects something EXACTLY like the example data.frame
                 nDays = 3, na.rm = FALSE, cutoff = 110 # for the 'aumentou_uai' function
                 ){
  # initialize the object with the output
  bitching = data.frame(event_begin = as.Date('1666-01-01'),
                        a = -999, b = -999, c = -999, d = -999)
  
  # creates a boolean vector to the input dataset, with 'z' values that pass the cutoff
  aumentou = aumentou_uai(dat$z, nDays = nDays, na.rm = na.rm, cutoff = cutoff)
  
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
  getB = function(x){ # solve item b
    #       b) numero de dias entre o comeco do evento em y e o primeiro aumento em z.
    aux = which(aumentou[-c(1:x)])
    out = ifelse(length(aumentou) == 0, NA, aux[1])
    out
  }
  bitching[ , 'b'] = sapply(indexes$first, FUN = getB)

#       c) numero de dias entre o MAIOR valor dentro dos eventos em y e o maior valor em z ANTES do proximo evento.
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
  bitching[ , 'c'] = getC()

#       d)diferenca entre o valore maximo em z depois do evento e a media de z nos 3 dias antes do evento
  getD = function(){ # solve item d
    #       d)diferenca entre o valore maximo em z depois do evento e a media de z nos 3 dias antes do evento
    meanZ = sapply(indexes$first, FUN = function(x){
      if(x < 3){
        return(NA) 
      } else{
        return(mean(dat$z[(x-3):(x-1)]) )
      }
    })
    maxZ = sapply(indexes$last, FUN = function(x) max(dat$z[-c(1:x)]) )
    
    out = ( maxZ - as.numeric(meanZ) )
    return(out)
  }
  bitching[ , 'd'] = getD()


  return(bitching)
}
