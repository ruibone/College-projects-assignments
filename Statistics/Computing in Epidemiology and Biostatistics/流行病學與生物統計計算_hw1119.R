###recursive function###

tower <- function(x){
  if (x == 1) {
    return(1)
    
  }else{
    return(2*tower(x-1)+1)
  }
}

sprintf("最少需要%s步。", tower(20))


###apply family###

x <- matrix(c(3600, 5000, 12000, NA, 1000, 2000, 600, 7500, 1800, 9000,
              3600, 4500, 10000, 8500, 3000, 10000, 1000, NA, 1200, 10000,
              3800, 5500, 9000, 6000, 6600, 3000, 9600, 6500, 8200, 8000,
              5000, 6600, 13000, 4500, 5000, NA, 10600, 9500, 7600, 6000,
              6600, 8000, 17000, 3000, 7000, 1000, 12600, 8500, 6000, NA),5,10, byrow = TRUE) 

apply(x, 1, median, na.rm = T)
apply(x, 1, max, na.rm = T)
apply(x, 1, min, na.rm = T)

apply(x, 2, median, na.rm = T)
apply(x, 2, max, na.rm = T)
apply(x, 2, min, na.rm = T)


###equation###

A <- matrix(c(1,1,1,-3,-2,-1,1,3,1), 3, 3)
b <- c(4,6,4)
x <- solve(A, b)

sprintf("x = %f; y = %f, z = %f", x[1], x[2], x[3])
