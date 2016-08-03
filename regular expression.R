################ Regex

x1 <- gsub("^(.*?);\\s|[^=;]+or|[).]|hours|euro|&|mins", "", x0)
x1 <- gsub("\\(|\\,", ";", x1)
x2 <- strsplit(x1,";")

y <- traffic$status
y1 <- gsub("^(.*?);\\s|^(.*?)can\\s|[).]|hours|euro|&|mins", "", y)

y2 <- gsub("\\([k]", "; k", y1)
y2 <- gsub("\\([S]", "; S", y2)
y2 <- gsub("\\([RUB]", "; R", y2)
y2 <- gsub("\\(\\D+\\s|£", "", y2)
y2 <- gsub("\\;\\sor|\\(|\\,|or\\s|\\;\\,", ";", y2)
y3 <- strsplit(y2,";")

gsub("^\\s+|\\s+$", "", x)
grep('^([^to])*$', d$`1`, perl=TRUE, value=FALSE)
grep('^([\\d])*$', d$`1`, perl=TRUE, value=FALSE)