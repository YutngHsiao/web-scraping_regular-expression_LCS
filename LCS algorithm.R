############### LCS algorithm
library(stringi)
library(stringdist)

LCS <- data.frame()

for (j in 1:10){
  b <- sp1_c$att[j]
  sb <- stri_sub(b, 1, 1:nchar(b))
  sstr <- na.omit(stri_extract_all_coll(a, sb, simplify=TRUE))
  LCS <- rbind(LCS, sstr[which.max(nchar(sstr))][1])
}

order(..., na.last = TRUE, decreasing = FALSE)

sort.list(x, partial = NULL, na.last = TRUE, decreasing = FALSE,
          method = c("shell", "quick", "radix"))


