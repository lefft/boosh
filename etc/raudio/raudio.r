# lol u can make lil wave tracks                   
library("audio")
for (x in     seq_along(xs <- rep(1, times=4))){
  for (y in   seq_along(ys <- c(3,6))){
    for (z in seq_along(zs <- c(22050,44100,88200))){
      # play a tone and print progress message for each z
      play(exp(exp(rnorm(10000, mean=ys[y]))), rate=zs[z]) # ~~<(o_@)^
      print(paste0("x==",x, ", y==",y,", and z==",z," rn waowwie :o"))
    }
    Sys.sleep(.25)
    play((rnorm(10000, mean=xs[x])), rate=zs[z]) #     ~~*(^-^)*
    Sys.sleep(.75) 
  }
}

### ABOVE HERE IS SCREENSHOT 


# make a wrapper for the first song for quicker dev
make_song <- function(tones, yvals=c(3,6), zvals=c(22050,44100,88200), 
                      sleeps=c(.25,.75), reps=2){
  # where:
  #   - tones  -- a list of numeric vectors e.g. a df          [NOT IMPLEMENTED]
  #   - yvals  -- vary mean on first tone
  #   - zvals  -- vary sample rate on both tones
  #   - sleeps -- a length-two numeric vector giving pause durs
  #   - reps   -- vary mean on second tone upper-bound top-level iteration
  xs <- rep(1, times=reps)
  ys <- yvals
  zs <- zvals
  
  for (x in seq_along(xs)){
    for (y in seq_along(ys)){
      for (z in seq_along(zs)){
        play(exp(exp(rnorm(10000, mean=ys[y]))), rate=zs[z])
        print(paste0("x==", x, ", y==", y, ", and z==", z, " rn waowwie :o"))
      }
      Sys.sleep(sleeps[1])
      play((rnorm(10000, mean=xs[x])), rate=zs[z])
      Sys.sleep(sleeps[2]) 
    }
  }
}


# kinda intuitive at end of middle loop but conceptually weird
# print(paste0("iteration ", x, y, z))
# print(paste0("iteration ", x, y, z))

# print(paste0("all uppin z for the ", zcounter, "th time ", 
#              "on x's ", x, "th val waowwie :o"))

ls("package:audio")
record()
play()
play(log(tan(1:10000/20)))

play(1:100000)
pause()
y=4

