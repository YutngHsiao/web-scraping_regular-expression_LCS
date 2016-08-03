u <- read.csv("spot.csv", stringsAsFactors = F, header = F)[,-1]
###
library(rvest)

get_cat <- function(html){
  r <- html_nodes(html, ".p66n_prop_v9 a") %>% as.character()
  r1 <- gsub("<(.*?)>", "", r)
  r1 <- gsub("\\s&amp;\\s", ";", r1)
  r1 <- gsub("^[\n]|[\n]$", "", r1)
  r1 <- gsub("[\n]", ";", r1)
  return(r1)
}


get_n <- function(html){
  n <- html_nodes(html, ".property_index p") %>% html_text()
  n1 <- gsub("\\n{2,}.*\\n|\\n", "", n)
  n2 <- gsub("\\(\\d+\\)", NA, n1)
  if (is.element(NA, n2) == T){
    n2 <- n2[ - which(n2 %in% NA)]
  }
  return(n2)
}

get_rev <- function(html){
  alt <- html_nodes(html, "div.more.rating") %>%  as.character()
  alt1 <- (gsub(".*alt=[\\\"]", "",alt))
  alt1 <- as.numeric(gsub("\\sof.*$", "",alt1))
  return(alt1)
}

get_rew <- function(html){
  r <- html_nodes(html, "span.ect a") %>% as.character()
  r1 <- gsub(".*REVIEWS|\\n|[[:punct:]]|\\sreviews.*$|\\sreviewa.*$", "", r)
  return(r1)
}

get_com <- function(html){
  re <- html_nodes(html, ".c2_review_items a") %>% as.character()
  re1 <- gsub(".*ShowUserReviews|\\n|\\/a|[[:punct:]]", "", re)
  return(re1)
}

##################### site1
spot <- data.frame()
spot_t <- data.frame()

for (i in u[84:101]){
  s1 <- sub("^(.*Act_num_)(.*)$", '\\1', i)
  s2 <- sub("^(.*Act_sum)(.*)$", '\\2', i)
  tav <- i
  html <- read_html(tav)
  num <- getnum(i)
  attr <- get_n(html)
  l <- length(attr)
  alt <- get_rev(html)
  length(alt) <- l
  rew <- get_rew(html)
  length(rew) <- l
  com <- get_com(html)
  length(com) <- l*2
  com <- split(com, 1:length(com) %% 2 == 0)
  cat <- get_cat(html)
  length(cat) <- l
  city <- gsub("^.*param\\-|\\_.*html|toa.*?-", "", i)
  spot_t <- data.frame(city = unlist(city), att = unlist(attr), rate = unlist(alt), review = unlist(rew), c1 <- com[1], c2 <- com[2], c3 <- cat)
  spot <- rbind(spot, spot_t)
  if (num > 5){
    for (j in 1:5){
      i5 <- paste(s1, "gas", as.character(j*30), "-", s2, sep = "")
      tav5 <- i5
      html5 <- read_html(tav5)
      attr5 <- get_n(html5)
      l5 <- length(attr5)
      alt5 <- get_rev(html5)
      length(alt5) <- l5
      rew5 <- get_rew(html5)
      length(rew5) <- l5
      com5 <- get_com(html5)
      length(com5) <- l5*2
      com5 <- split(com5, 1:length(com5) %% 2 == 0)
      cat <- get_cat(html5)
      length(cat) <- l5
      city5 <- gsub("^.*flows\\-|\\_.*html|xca.*?-", "", i5)
      spot_t5 <- data.frame(city = unlist(city5), att = unlist(attr5), rate = unlist(alt5), review = unlist(rew5), c1 <- com5[1], c2 <- com5[2], c3 <- cat)
      spot <- rbind(spot, spot_t5)
    }
    
  }
  
}  


##################### site2

traffic <- data.frame()
for(j in tuscany$spot[9:10]){
  for(i in tuscany$spot){
    if(is.na(match(i,j))) {
      k <- gsub("\\s|\\'", "-", j)
      ki <- gsub("\\s|\\'", "-", i)
      traffic_stats <- data.frame()
      from_url <- paste(r2r, k, sep="")
      to_url <- paste(from_url, "/", ki, sep="")
      print(to_url)
      html <- read_html(to_url)
      t1 <- html_nodes(html, ".ini-nerary-subject") %>% html_text()
      t1 <- t1[1:(length(t1)-3)]
      t2 <- html_nodes(html, ".ini-nerary-area") %>% html_text()
      t2 <- gsub("\\'", " ", t2)
      x <- grep(i, t2)
      x <- c(0, x)
      t3 <- html_nodes(html, ".ini-nerary-details") %>% html_text()
      t3 <- t3[1:(length(t3)-3)]
      t4 <- html_nodes(html, ".ini-nerary-price") %>% html_text() %>% iconv("UTF-8", "UTF-8")
      t4 <- t4[1:(length(t4)-3)]
      t5 <- html_nodes(html, ".route-sumini-nerary p") %>% html_text()
      t6 <- html_nodes(html, ".route-sumini-nerary span") %>% html_text() %>% iconv("UTF-8", "UTF-8")
      t6 <- t6[t6!=""]
      x <- grep(i, t2)
      x <- c(0, x)
      y <- grep(j, t2)
      y <- c(y-1, length(t2))
      if(length(grep(i, t2)) == length(t1)){
        r <- unlist(lapply(seq(x)[1:(length(x)-1)], function(i) x[i+1]-x[i]))
      } else {
        r <- unlist(lapply(seq(y)[1:(length(y)-1)], function(i) y[i+1]-y[i]))
      }
      
      traffic_stats <- data.frame()
      tt1 <- unlist(lapply(seq(r), function(i) rep(t1[i],r[i])))
      tt5 <- unlist(lapply(seq(r), function(i) rep(t5[i],r[i])))
      tt6 <- unlist(lapply(seq(r), function(i) rep(t6[i],r[i])))
      sq <- unlist(lapply(seq(r), function(i) rep(i,r[i])))
      tfx <- cbind(tt1, t2, c(t2[-1], NA), t3, t4, tt5, tt6, sq)
      tfx <- tfx[-x[-1],]
      tfx <- as.data.frame(tfx, stringsAsFactors = F)
      tfx$from <- j
      tfx$to <- i
      traffic <- rbind(traffic, tfx)
      Sys.sleep(1)
    }
  }
}

############### site3

for(i in 1:length(cities)){
  tav <- paste0("http://wiki____.org/en/", cities[i])
  html <- read_html(tav)
  r <- html_nodes(html, "p")[2] %>% html_text()
  r <- gsub("\n", "", r)
  intro <- c(intro, r)
  print(i)
  print(r)
}
