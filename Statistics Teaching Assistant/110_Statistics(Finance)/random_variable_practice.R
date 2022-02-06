##### Practice 1 #####
# 1.
dbinom(2, 3, 4/10)
dbinom(1, 3, 6/10)

# 2.
dhyper(2, 4, 6, 3)
dhyper(1, 6, 4, 3)

# 3.
1 - phyper(1, 4, 6, 3)
phyper(1, 6, 4, 3)


##### Practice 2 #####
# 1.
pgeom(2, 0.6)

# 2.
dnbinom(1, 2, 0.6)

# 3.
1 - pnbinom(2, 3, 0.6)


##### Practice 4 #####
# 1.
pnorm(69, 66, 3) - pnorm(63, 66, 3)

# 1.
pnorm(72, 66, 3) - pnorm(60, 66, 3)

# 1.
pnorm(75, 66, 3) - pnorm(57, 66, 3)

# 可以觀察到以上3個小題之機率分別為正負1, 2, 3個標準差之涵蓋範圍。
