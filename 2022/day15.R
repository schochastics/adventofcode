library(tidyverse)
input <- as_tibble(readLines("input15.txt"))
beacons <- input |>
  extract(value,
          c("sx","sy","bx","by"),
          "Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)",
          convert = TRUE
          ) |> 
  mutate(dist = abs(sx-bx) + abs(sy-by))

ty <- 2000000
check_fct <- function(x){
  if(ty>=(x["sy"]-x["dist"]) & ty <= (x["sy"]+x["dist"])){
    deltay <- abs(ty - x["sy"])
    remain <- x["dist"] - deltay
    horiz <- (x["sx"]-remain):(x["sx"]+remain)
    if(x["by"]==ty){
      horiz <- horiz[horiz!=ty]
    }
    return(horiz)
  }
  return(NULL)
}
length(unique(unlist(apply(beacons,1,check_fct))))
