team <- 3

c <- 1/(team + 1);c

one <- (c/2)*(team^2 - (-1)^2);one
two <- (c/3)*(team^3 - (-1)^3);two
thr <- (c/4)*(team^4 - (-1)^4);thr
fou <- (c/5)*(team^5 - (-1)^5);fou

mtwo <- two - one^2;mtwo
mfou <- fou - 4*thr*one + 6*two*one^2 - 3*one^4;mfou

beta <- mfou/sqrt(mtwo)^4;beta
