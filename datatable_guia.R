library(data.table)
setwd("C:/Users/mporcar/Desktop/Primers codis")
###################################################
##### Definir data table y utilizar parametros ####
###################################################

DF = data.frame(x=rep(c("a","b","c"),each=3), y=c(1,3,6), v=1:9)
DT = data.table(x=rep(c("a","b","c"),each=3), y=c(1,3,6), v=1:9)
DF
DT
identical(dim(DT),dim(DF)) # TRUE
identical(DF$a, DT$a) # TRUE
is.list(DF) # TRUE
is.list(DT) # TRUE
is.data.frame(DT) # TRUE
tables()
DT[2] # 2nd row
DT[,v] # v column (as vector)
DT[,list(v)] # v column (as data.table)
DT[2:3,sum(v)] # sum(v) over rows 2 and 3
DT[2:5,cat(v)] # just for j's side effect
DT[c(TRUE,FALSE)] # even rows (usual recycling)
DT[,2,with=FALSE] # 2nd column
colNum = 2
DT[,colNum,with=FALSE] # same
setkey(DT,x) # set a 1-column key. No quotes, for convenience.
setkeyv(DT,"x") # same (v in setkeyv stands for vector)
v="x"
setkeyv(DT,v) # same
# key(DT)<-"x" # copies whole table, please use set* functions instead
DT["a"] # binary search (fast)
DT[x=="a"] # same; i.e. binary search (fast)
DT[,sum(v),by=x] # keyed by
DT[,sum(v),by=key(DT)] # same
DT[,sum(v),by=y] # ad hoc by
DT["a",sum(v)] # j for one group
DT[c("a","b"),sum(v),by=.EACHI] # j for two groups
X = data.table(c("b","c"),foo=c(4,2))
X
DT
DT[X] # join
DT[X,sum(v),by=.EACHI] # join and eval j for each row in i
DT[X,mult="first"] # first row of each group
DT[X,mult="last"] # last row of each group
DT[X,sum(v)*foo,by=.EACHI] # join inherited scope
setkey(DT,x,y) # 2-column key
setkeyv(DT,c("x","y")) # same
DT["a"] # join to 1st column of key
DT[.("a")] # same, .() is an alias for list()
DT[list("a")] # same
DT[.("a",3)] # join to 2 columns
DT[.("a",3:6)] # join 4 rows (2 missing)
DT[.("a",3:6),nomatch=0] # remove missing
DT[.("a",3:6),roll=TRUE] # rolling join (locf)
DT[,sum(v),by=.(y%%2)] # by expression
DT[,.SD[2],by=x] # 2nd row of each group
DT[,tail(.SD,2),by=x] # last 2 rows of each group
DT[,lapply(.SD,sum),by=x] # apply through columns by group
DT[,list(MySum=sum(v),
         MyMin=min(v),
         MyMax=max(v)),
   by=.(x,y%%2)] # by 2 expressions
DT[,sum(v),x][V1<20] # compound query
DT[,sum(v),x][order(V1)] # ordering results
print(DT[,z:=42L]) # add new column by reference
print(DT[,z:=NULL]) # remove column by reference
print(DT["a",v:=42L]) # subassign to existing v column by reference
print(DT["b",v2:=84L]) # subassign to new column by reference (NA padded)
DT[,m:=mean(v),by=x][] # add new column by reference by group
# NB: postfix [] is shortcut to print()
DT[,.SD[which.min(v)],by=x][] # nested query by group
DT[!.("a")] # not join
DT[!"a"] # same
DT[!2:4] # all rows other than 2:4
DT[x!="b" | y!=3] # not yet optimized, currently vector scans
DT[!.("b",3)] # same result but much faster
# new feature: 'on' argument, from v1.9.6+
DT1 = data.table(x=c("c", "a", "b", "a", "b"), a=1:5)
DT2 = data.table(x=c("d", "c", "b"), mul=6:8)
DT1[DT2, on=c(x="x")] # join on columns 'x'
DT1[DT2, on="x"] # same as above
DT1[DT2, .(sum(a) * mul), by=.EACHI, on="x"] # using by=.EACHI
DT1[DT2, .(sum(a) * mul), by=.EACHI, on="x", nomatch=0L] # using by=.EACHI
# Follow r-help posting guide, support is here (*not* r-help) :
# http://stackoverflow.com/questions/tagged/data.table
# or
# datatable-help@lists.r-forge.r-project.org
## Not run:
vignette("datatable-intro")
vignette("datatable-faq")





######################################
#### := definiendo por referencia ####
######################################


DT = data.table(a=LETTERS[c(1,1:3)],b=4:7,key="a")
DT[,c:=8] # add a numeric column, 8 for all rows
DT[,d:=9L] # add an integer column, 9L for all rows
DT[,c:=NULL] # remove column c
DT[2,d:=10L] # subassign by reference to column d
DT # DT changed by reference
DT[b>4,b:=d*2L] # subassign to b using d, where b>4
DT["A",b:=0L] # binary search for group "A" and set column b
DT[,e:=mean(d),by=a] # add new column by group by reference
DT["B",f:=mean(d)] # subassign to new column, NA initialized
## Not run:
# Speed example ...
m = matrix(1,nrow=100000,ncol=100)
DF = as.data.frame(m)
DT = as.data.table(m)
system.time(for (i in 1:1000) DF[i,1] <- i)
# 591 seconds
system.time(for (i in 1:1000) DT[i,V1:=i])
# 2.4 seconds ( 246 times faster, 2.4 is overhead in [.data.table )
system.time(for (i in 1:1000) set(DT,i,1L,i))
# 0.03 seconds ( 19700 times faster, overhead of [.data.table is avoided )
# However, normally, we call [.data.table *once* on *large* data, not many times on small data.
# The above is to demonstrate overhead, not to recommend looping in this way. But the option
# of set() is there if you need it.
## End(Not run)

#########################
#### dcast important ####
#########################

names(ChickWeight) <- tolower(names(ChickWeight))
DT <- melt(as.data.table(ChickWeight), id=2:4) # calls melt.data.table
?melt
# dcast is a S3 method in data.table from v1.9.6
dcast(DT, time ~ variable, fun=mean)
dcast(DT, diet ~ variable, fun=mean)
dcast(DT, diet+chick ~ time, drop=FALSE)
dcast(DT, diet+chick ~ time, drop=FALSE, fill=0)
# using subset
dcast(DT, chick ~ time, fun=mean, subset=.(time < 10 & chick < 20))
## Not run:
# benchmark against reshape2's dcast, minimum of 3 runs
set.seed(45)
DT <- data.table(aa=sample(1e4, 1e6, TRUE),
                 bb=sample(1e3, 1e6, TRUE),
                 cc = sample(letters, 1e6, TRUE), dd=runif(1e6))
system.time(dcast(DT, aa ~ cc, fun=sum)) # 0.12 seconds
system.time(dcast(DT, bb ~ cc, fun=mean)) # 0.04 seconds
# reshape2::dcast takes 31 seconds
system.time(dcast(DT, aa + bb ~ cc, fun=sum)) # 1.2 seconds
## End(Not run)
# NEW FEATURE - multiple value.var and multiple fun.aggregate
dt = data.table(x=sample(5,20,TRUE), y=sample(2,20,TRUE),
                z=sample(letters[1:2], 20,TRUE), d1 = runif(20), d2=1L)
# multiple value.var
dcast(dt, x + y ~ z, fun=sum, value.var=c("d1","d2"))
# multiple fun.aggregate
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var="d1")
# multiple fun.agg and value.var (all combinations)
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var=c("d1", "d2"))
# multiple fun.agg and value.var (one-to-one)
dcast(dt, x + y ~ z, fun=list(sum, mean), value.var=list("d1", "d2"))


###########################
#### Duplicated/unique ####
###########################

DT <- data.table(A = rep(1:3, each=4), B = rep(1:4, each=3), C = rep(1:2, 6), key = "A,B")
duplicated(DT)
unique(DT)
duplicated(DT, by="B")
unique(DT, by="B")
duplicated(DT, by=c("A", "C"))
unique(DT, by=c("A", "C"))
DT = data.table(a=c(2L,1L,2L), b=c(1L,2L,1L)) # no key
unique(DT) # rows 1 and 2 (row 3 is a duplicate of row 1)
DT = data.table(a=c(3.142, 4.2, 4.2, 3.142, 1.223, 1.223), b=rep(1,6))
unique(DT) # rows 1,2 and 5
DT = data.table(a=tan(pi*(1/4 + 1:10)), b=rep(1,10)) # example from ?all.equal
length(unique(DT$a)) # 10 strictly unique floating point values
all.equal(DT$a,rep(1,10)) # TRUE, all within tolerance of 1.0
DT[,which.min(a)] # row 10, the strictly smallest floating point value
identical(unique(DT),DT[1]) # TRUE, stable within tolerance
identical(unique(DT),DT[10]) # FALSE
# fromLast=TRUE
DT <- data.table(A = rep(1:3, each=4), B = rep(1:4, each=3), C = rep(1:2, 6), key = "A,B")
duplicated(DT, by="B", fromLast=TRUE)
unique(DT, by="B", fromLast=TRUE)
# anyDuplicated
anyDuplicated(DT, by=c("A", "B")) # 3L
any(duplicated(DT, by=c("A", "B"))) # TRUE
# uniqueN, unique rows on key columns
uniqueN(DT)
# uniqueN, unique rows on all all columns
uniqueN(DT, by=NULL)
# uniqueN while grouped by "A"
DT[, .(uN=uniqueN(.SD)), by=A]

source("https://bioconductor.org/biocLite.R")
biocLite("IRanges")
browseVignettes("IRanges")


###################
#### foverlaps ####
###################

?foverlaps

## simple example:
x = data.table(start=c(5,31,22,16), end=c(8,50,25,18), val2 = 7:10)
y = data.table(start=c(10, 20, 30), end=c(15, 35, 45), val1 = 1:3)
setkey(y, start, end)
foverlaps(x, y, type="any", which=TRUE) ## return overlap indices
foverlaps(x, y, type="any") ## return overlap join
foverlaps(x, y, type="any", mult="first") ## returns only first match
foverlaps(x, y, type="within") ## matches iff 'x' is within 'y'
## with extra identifiers (ex: in genomics)
x = data.table(chr=c("Chr1", "Chr1", "Chr2", "Chr2", "Chr2"),
               start=c(5,10, 1, 25, 50), end=c(11,20,4,52,60))
y = data.table(chr=c("Chr1", "Chr1", "Chr2"), start=c(1, 15,1),
               end=c(4, 18, 55), geneid=letters[1:3])
setkey(y, chr, start, end)
foverlaps(x, y, type="any", which=TRUE)
foverlaps(x, y, type="any")
foverlaps(x, y, type="any", nomatch=0L)
foverlaps(x, y, type="within", which=TRUE)
foverlaps(x, y, type="within")
foverlaps(x, y, type="start")
## x and y have different column names - specify by.x
x = data.table(seq=c("Chr1", "Chr1", "Chr2", "Chr2", "Chr2"),
               start=c(5,10, 1, 25, 50), end=c(11,20,4,52,60))
y = data.table(chr=c("Chr1", "Chr1", "Chr2"), start=c(1, 15,1),
               end=c(4, 18, 55), geneid=letters[1:3])
setkey(y, chr, start, end)
foverlaps(x, y, by.x=c("seq", "start", "end"),
          type="any", which=TRUE)




################
#### frank #####
################
?frank
# on vectors
x = c(4, 1, 4, NA, 1, NA, 4)
# NAs are considered identical (unlike base R)
# default is average
frankv(x) # na.last=TRUE
frankv(x, na.last=FALSE)
# ties.method = min
frankv(x, ties.method="min")
# ties.method = dense
frankv(x, ties.method="dense")
# on data.table
DT = data.table(x, y=c(1, 1, 1, 0, NA, 0, 2))
frankv(DT, cols="x") # same as frankv(x) from before
frankv(DT, cols="x", na.last="keep")
frankv(DT, cols="x", ties.method="dense", na.last=NA)
frank(DT, x, ties.method="dense", na.last=NA) # equivalent of above using frank
# on both columns
frankv(DT, ties.method="first", na.last="keep")
frank(DT, ties.method="first", na.last="keep") # equivalent of above using frank
# order argument
frank(DT, x, -y, ties.method="first")
# equivalent of above using frankv
frankv(DT, order=c(1L, -1L), ties.method="first")


###############
#### fread ####
###############

# Demo speedup
n=1e6
DT = data.table( a=sample(1:1000,n,replace=TRUE),
                 b=sample(1:1000,n,replace=TRUE),
                 c=rnorm(n),
                 d=sample(c("foo","bar","baz","qux","quux"),n,replace=TRUE),
                 e=rnorm(n),
                 f=sample(1:1000,n,replace=TRUE) )
head(DT)
#estem definint el tipos de variables?
DT[2,b:=NA_integer_]
DT[4,c:=NA_real_]
DT[3,d:=NA_character_]
DT[5,d:=""]
DT[2,e:=+Inf]
DT[3,e:=-Inf]
write.table(DT,"test.csv",sep=",",row.names=FALSE,quote=FALSE)
cat("File size (MB):", round(file.info("test.csv")$size/1024^2),"\n")
# 50 MB (1e6 rows x 6 columns)
system.time(DF1 <-read.csv("test.csv",stringsAsFactors=FALSE))
# 60 sec (first time in fresh R session)
system.time(DF1 <- read.csv("test.csv",stringsAsFactors=FALSE))
# 30 sec (immediate repeat is faster, varies)
system.time(DF2 <- read.table("test.csv",header=TRUE,sep=",",quote="",
                              stringsAsFactors=FALSE,comment.char="",nrows=n,
                              colClasses=c("integer","integer","numeric",
                                           "character","numeric","integer")))
# 10 sec (consistently). All known tricks and known nrows, see references.
require(data.table)
system.time(DT <- fread("test.csv"))
# 3 sec (faster and friendlier)
require(sqldf)
system.time(SQLDF <- read.csv.sql("test.csv",dbname=NULL))
# 20 sec (friendly too, good defaults)
require(ff)
system.time(FFDF <- read.csv.ffdf(file="test.csv",nrows=n))
# 20 sec (friendly too, good defaults)
identical(DF1,DF2)
all.equal(as.data.table(DF1), DT)
identical(DF1,within(SQLDF,{b<-as.integer(b);c<-as.numeric(c)}))
identical(DF1,within(as.data.frame(FFDF),d<-as.character(d)))
# Scaling up ...
l = vector("list",10)
for (i in 1:10) l[[i]] = DT
DTbig = rbindlist(l)
tables()
write.table(DTbig,"testbig.csv",sep=",",row.names=FALSE,quote=FALSE)
# 500MB (10 million rows x 6 columns)
system.time(DF <- read.table("testbig.csv",header=TRUE,sep=",",
                             quote="",stringsAsFactors=FALSE,comment.char="",nrows=1e7,
                             colClasses=c("integer","integer","numeric",
                                          "character","numeric","integer")))
# 100-200 sec (varies)
system.time(DT <- fread("testbig.csv"))
# 30-40 sec
all(mapply(all.equal, DF, DT))
# Real data example (Airline data)
# http://stat-computing.org/dataexpo/2009/the-data.html
download.file("http://stat-computing.org/dataexpo/2009/2008.csv.bz2",
              destfile="2008.csv.bz2")
# 109MB (compressed)
system("bunzip2 2008.csv.bz2")
# 658MB (7,009,728 rows x 29 columns)
colClasses = sapply(read.csv("2008.csv",nrows=100),class)
# 4 character, 24 integer, 1 logical. Incorrect.
colClasses = sapply(read.csv("2008.csv",nrows=200),class)
# 5 character, 24 integer. Correct. Might have missed data only using 100 rows
# since read.table assumes colClasses is correct.
system.time(DF <- read.table("2008.csv", header=TRUE, sep=",",
                             quote="",stringsAsFactors=FALSE,comment.char="",nrows=7009730,
                             colClasses=colClasses)
# 360 secs
system.time(DT <- fread("2008.csv"))
            # 40 secs
            table(sapply(DT,class))
            # 5 character and 24 integer columns. Correct without needing to worry about colClasses
            # issue above.
            # Reads URLs directly :
            fread("http://www.stats.ox.ac.uk/pub/datasets/csb/ch11b.dat")
            ## End(Not run)
            # Reads text input directly :
            fread("A,B\n1,2\n3,4")
            # Reads pasted input directly :
            fread("A,B
                  1,2
                  3,4
                  ")
            # Finds the first data line automatically :
            fread("
                  This is perhaps a banner line or two or ten.
                  A,B
                  1,2
                  3,4
                  ")
            # Detects whether column names are present automatically :
            fread("
                  1,2
                  3,4
                  ")
            # Numerical precision :
            DT = fread("A\n1.010203040506070809010203040506\n") # silent loss of precision
            DT[,sprintf("%.15E",A)] # stored accurately as far as double precision allows
            DT = fread("A\n1.46761e-313\n") # detailed warning about ERANGE; read as 'numeric'
            DT[,sprintf("%.15E",A)] # beyond what double precision can store accurately to 15 digits
            # For greater accuracy use colClasses to read as character, then package Rmpfr.
            # colClasses
            data = "A,B,C,D\n1,3,5,7\n2,4,6,8\n"
            fread(data, colClasses=c(B="character",C="character",D="character")) # as read.csv
            fread(data, colClasses=list(character=c("B","C","D"))) # saves typing
            fread(data, colClasses=list(character=2:4)) # same using column numbers
            # drop
            fread(data, colClasses=c("B"="NULL","C"="NULL")) # as read.csv
            fread(data, colClasses=list(NULL=c("B","C"))) #
            fread(data, drop=c("B","C")) # same but less typing, easier to read
            fread(data, drop=2:3) # same using column numbers
            # select
# (in read.csv you need to work out which to drop)
fread(data, select=c("A","D")) # less typing, easier to read
fread(data, select=c(1,4)) # same using column numbers
            
            
####################
#### IDateTime #####
####################
# create IDate:
(d <- as.IDate("2001-01-01"))
# S4 coercion also works
identical(as.IDate("2001-01-01"), as("2001-01-01", "IDate"))
# create ITime:
(t <- as.ITime("10:45"))
# S4 coercion also works
identical(as.ITime("10:45"), as("10:45", "ITime"))
(t <- as.ITime("10:45:04"))
(t <- as.ITime("10:45:04", format = "%H:%M:%S"))
as.POSIXct("2001-01-01") + as.ITime("10:45")
datetime <- seq(as.POSIXct("2001-01-01"), as.POSIXct("2001-01-03"), by = "5 hour")
(af <- data.table(IDateTime(datetime), a = rep(1:2, 5), key = "a,idate,itime"))
af[, mean(a), by = "itime"]
af[, mean(a), by = list(hour = hour(itime))]
af[, mean(a), by = list(wday = factor(weekdays(idate)))]
af[, mean(a), by = list(wday = wday(idate))]
as.POSIXct(af$idate)
as.POSIXct(af$idate, time = af$itime)
as.POSIXct(af$idate, af$itime)
as.POSIXct(af$idate, time = af$itime, tz = "GMT")
as.POSIXct(af$itime, af$idate)
as.POSIXct(af$itime) # uses today's date
(seqdates <- seq(as.IDate("2001-01-01"), as.IDate("2001-08-03"), by = "3 weeks"))
round(seqdates, "months")
if (require(chron)) {
  as.chron(as.IDate("2000-01-01"))
  as.chron(as.ITime("10:45"))
  as.chron(as.IDate("2000-01-01"), as.ITime("10:45"))
  as.chron(as.ITime("10:45"), as.IDate("2000-01-01"))
  as.ITime(chron(times = "11:01:01"))
  IDateTime(chron("12/31/98","10:45:00"))
}
class(as.IDate("2000-01-01")-as.IDate("2001-08-03"))
difftime(as.IDate("2000-01-01"),as.IDate("2001-08-03"),units = "secs")


#####################
#####   J/CJ   ######
#####################


DT = data.table(A=5:1,B=letters[5:1])
setkey(DT,B) # re-orders table and marks it sorted.
DT[J("b")] # returns the 2nd row
DT[.("b")] # same. Style of package plyr.
DT[list("b")] # same
# CJ usage examples
CJ(c(5,NA,1), c(1,3,2)) # sorted and keyed data.table
do.call(CJ, list(c(5,NA,1), c(1,3,2))) # same as above
CJ(c(5,NA,1), c(1,3,2), sorted=FALSE) # same order as input, unkeyed
# use for 'unique=' argument
x = c(1,1,2)
y = c(4,6,4)
CJ(x, y, unique=TRUE) # unique(x) and unique(y) are computed automatically


#####################
#####   melt   ######
#####################

set.seed(45)
require(data.table)
DT <- data.table(
  i_1 = c(1:5, NA),
  i_2 = c(NA,6,7,8,9,10),
  f_1 = factor(sample(c(letters[1:3], NA), 6, TRUE)),
  f_2 = factor(c("z", "a", "x", "c", "x", "x"), ordered=TRUE),
  c_1 = sample(c(letters[1:3], NA), 6, TRUE),
  d_1 = as.Date(c(1:3,NA,4:5), origin="2013-09-01"),
  d_2 = as.Date(6:1, origin="2012-01-01"))
# add a couple of list cols
DT[, l_1 := DT[, list(c=list(rep(i_1, sample(5,1)))), by = i_1]$c]
DT[, l_2 := DT[, list(c=list(rep(c_1, sample(5,1)))), by = i_1]$c]
# id, measure as character/integer/numeric vectors
melt(DT, id=1:2, measure="f_1")
melt(DT, id=c("i_1", "i_2"), measure=3) # same as above
melt(DT, id=1:2, measure=3L, value.factor=TRUE) # same, but 'value' is factor
melt(DT, id=1:2, measure=3:4, value.factor=TRUE) # 'value' is *ordered* factor
# preserves attribute when types are identical, ex: Date
melt(DT, id=3:4, measure=c("d_1", "d_2"))
melt(DT, id=3:4, measure=c("i_1", "d_1")) # attribute not preserved
# on list
melt(DT, id=1, measure=c("l_1", "l_2")) # value is a list
melt(DT, id=1, measure=c("c_1", "l_1")) # c1 coerced to list
# on character
melt(DT, id=1, measure=c("c_1", "f_1")) # value is char
melt(DT, id=1, measure=c("c_1", "i_2")) # i2 coerced to char
# on na.rm=TRUE. NAs are removed efficiently, from within C
melt(DT, id=1, measure=c("c_1", "i_2"), na.rm=TRUE) # remove NA
# NEW FEATURE: measure.vars can be a list
# melt "f_1,f_2" and "d_1,d_2" simultaneously, retain 'factor' attribute
# convenient way using internal function patterns()
melt(DT, id=1:2, measure=patterns("^f_", "^d_"), value.factor=TRUE)
# same as above, but provide list of columns directly by column names or indices
melt(DT, id=1:2, measure=list(3:4, c("d_1", "d_2")), value.factor=TRUE)
# na.rm=TRUE removes rows with NAs in any 'value' columns
melt(DT, id=1:2, measure=patterns("f_", "d_"), value.factor=TRUE, na.rm=TRUE)
# return 'NA' for missing columns, 'na.rm=TRUE' ignored due to list column
melt(DT, id=1:2, measure=patterns("l_", "c_"), na.rm=TRUE)




#################
#### merge ######
#################

(dt1 <- data.table(A = letters[1:10], X = 1:10, key = "A"))
(dt2 <- data.table(A = letters[5:14], Y = 1:10, key = "A"))
merge(dt1, dt2)
merge(dt1, dt2, all = TRUE)
(dt1 <- data.table(A = letters[rep(1:3, 2)], X = 1:6, key = "A"))
(dt2 <- data.table(A = letters[rep(2:4, 2)], Y = 6:1, key = "A"))
merge(dt1, dt2, allow.cartesian=TRUE)
(dt1 <- data.table(A = c(rep(1L, 5), 2L), B = letters[rep(1:3, 2)], X = 1:6, key = "A,B"))
(dt2 <- data.table(A = c(rep(1L, 5), 2L), B = letters[rep(2:4, 2)], Y = 6:1, key = "A,B"))
merge(dt1, dt2)
merge(dt1, dt2, by="B", allow.cartesian=TRUE)
# test it more:
d1 <- data.table(a=rep(1:2,each=3), b=1:6, key="a,b")
d2 <- data.table(a=0:1, bb=10:11, key="a")
d3 <- data.table(a=0:1, key="a")
d4 <- data.table(a=0:1, b=0:1, key="a,b")
merge(d1, d2)
merge(d2, d1)
merge(d1, d2, all=TRUE)
merge(d2, d1, all=TRUE)
merge(d3, d1)
merge(d1, d3)
merge(d1, d3, all=TRUE)
merge(d3, d1, all=TRUE)
merge(d1, d4)
merge(d1, d4, by="a", suffixes=c(".d1", ".d4"))
merge(d4, d1)
merge(d1, d4, all=TRUE)
merge(d4, d1, all=TRUE)
# new feature, no need to set keys anymore
set.seed(1L)
d1 <- data.table(a=sample(rep(1:3,each=2)), z=1:6)
d2 <- data.table(a=2:0, z=10:12)
merge(d1, d2, by="a")
merge(d1, d2, by="a", all=TRUE)
# new feature, using by.x and by.y arguments
setnames(d2, "a", "b")
merge(d1, d2, by.x="a", by.y="b")
merge(d1, d2, by.x="a", by.y="b", all=TRUE)
merge(d2, d1, by.x="b", by.y="a")


#################
#### na.omit ####
#################


DT = data.table(x=c(1,NaN,NA,3), y=c(NA_integer_, 1:3), z=c("a", NA_character_, "b", "c"))
# default behaviour
na.omit(DT)
# omit rows where 'x' has a missing value
na.omit(DT, cols="x")
# omit rows where either 'x' or 'y' have missing values
na.omit(DT, cols=c("x", "y"))
## Not run:
# Timings on relatively large data
set.seed(1L)
DT = data.table(x = sample(c(1:100, NA_integer_), 5e7L, TRUE),
                y = sample(c(rnorm(100), NA), 5e7L, TRUE))
system.time(ans1 <- na.omit(DT)) ## 2.6 seconds
system.time(ans2 <- stats:::na.omit.data.frame(DT)) ## 29 seconds
# identical? check each column separately, as ans2 will have additional attribute
all(sapply(1:2, function(i) identical(ans1[[i]], ans2[[i]]))) ## TRUE
## End(Not run)


#######################
##### patterns ########
#######################

# makes sense only in the context of melt at the moment
dt = data.table(x1 = 1:5, x2 = 6:10, y1 = letters[1:5], y2 = letters[6:10])
# melt all columns that begin with 'x' & 'y', respectively, into separate columns
melt(dt, measure.vars = patterns("^x", "^y"))


#######################
##### rbindlist #######
#######################
?rbindlist
# default case
DT1 = data.table(A=1:3,B=letters[1:3])
DT2 = data.table(A=4:5,B=letters[4:5])
l = list(DT1,DT2)
rbindlist(l)
# bind correctly by names
DT1 = data.table(A=1:3,B=letters[1:3])
DT2 = data.table(B=letters[4:5],A=4:5)
l = list(DT1,DT2)
rbindlist(l, use.names=TRUE)
# fill missing columns, and match by col names
DT1 = data.table(A=1:3,B=letters[1:3])
DT2 = data.table(B=letters[4:5],C=factor(1:2))
l = list(DT1,DT2)
rbindlist(l, use.names=TRUE, fill=TRUE)
# generate index column, auto generates indices
rbindlist(l, use.names=TRUE, fill=TRUE, idcol=TRUE)
# let's name the list
setattr(l, 'names', c("a", "b"))
rbindlist(l, use.names=TRUE, fill=TRUE, idcol="ID")


######################
######## rleid #######
######################
?rleid
DT = data.table(grp=rep(c("A", "B", "C", "A", "B"), c(2,2,3,1,2)), value=1:10)
rleid(DT$grp) # get run-length ids
rleidv(DT, "grp") # same as above
# get sum of value over run-length groups
DT[, sum(value), by=.(grp, rleid(grp))]

#####################
#### setcolorder ####
#####################

set.seed(45L)
DT = data.table(A=sample(3, 10, TRUE),
                B=sample(letters[1:3], 10, TRUE), C=sample(10))
setcolorder(DT, c("C", "A", "B"))

#####################
###### setDF  #######
#####################

X = data.table(x=1:5, y=6:10)
## convert 'X' to data.frame, without any copy.
setDF(X)
X = data.table(x=1:5, y=6:10)
## idem, assigning row names
setDF(X, rownames = LETTERS[1:5])
X = list(x=1:5, y=6:10)
# X is converted to a data.frame without any copy.
setDF(X)

#####################
###### setDT  #######
#####################

set.seed(45L)
X = data.frame(A=sample(3, 10, TRUE),
               B=sample(letters[1:3], 10, TRUE),
               C=sample(10), stringsAsFactors=FALSE)
# Convert X to data.table by reference and
# get the frequency of each "A,B" combination
setDT(X)[, .N, by=.(A,B)]
# convert list to data.table
# autofill names
X = list(1:4, letters[1:4])
setDT(X)
# don't provide names
X = list(a=1:4, letters[1:4])
setDT(X, FALSE)
# setkey directly
X = list(a = 4:1, b=runif(4))
setDT(X, key="a")[]
# check.names argument
X = list(a=1:5, a=6:10)
setDT(X, check.names=TRUE)[]

#####################
##### setkey  #######
#####################

# Type 'example(setkey)' to run these at prompt and browse output
DT = data.table(A=5:1,B=letters[5:1])
DT # before
setkey(DT,B) # re-orders table and marks it sorted.
DT # after
tables() # KEY column reports the key'd columns
key(DT)
keycols = c("A","B")
setkeyv(DT,keycols) # rather than key(DT)<-keycols (which copies entire table)
DT = data.table(A=5:1,B=letters[5:1])
DT2 = DT # does not copy
setkey(DT2,B) # does not copy-on-write to DT2
identical(DT,DT2) # TRUE. DT and DT2 are two names for the same keyed table
DT = data.table(A=5:1,B=letters[5:1])
DT2 = copy(DT) # explicit copy() needed to copy a data.table
setkey(DT2,B) # now just changes DT2
identical(DT,DT2) # FALSE. DT and DT2 are now different tables



############################
#### setNumericRounding ####
############################

DT = data.table(a=seq(0,1,by=0.2),b=1:2, key="a")
DT
setNumericRounding(0) # turn off rounding
DT[.(0.4)] # works
DT[.(0.6)] # no match, confusing since 0.6 is clearly there in DT
setNumericRounding(2) # restore default
DT[.(0.6)] # works as expected
# using type 'numeric' for integers > 2^31 (typically ids)
DT = data.table(id = c(1234567890123, 1234567890124, 1234567890125), val=1:3)
print(DT, digits=15)
DT[,.N,by=id] # 1 row
setNumericRounding(0)
DT[,.N,by=id] # 3 rows
# better to use bit64::integer64 for such ids
setNumericRounding(2)

#####################
##### setorder  #####
#####################

set.seed(45L)
DT = data.table(A=sample(3, 10, TRUE),
                B=sample(letters[1:3], 10, TRUE), C=sample(10))
# setorder
setorder(DT, A, -B)
# same as above, but using setorderv
setorderv(DT, c("A", "B"), c(1, -1))

#####################
##### shift     #####
#####################

# on vectors, returns a vector as long as length(n) == 1, #1127
x = 1:5
x = c(1,1.2,3.3,4.4,3)
# lag with period=1 and pad with NA (returns vector)
shift(x, n=1, fill=NA, type="lag")
# lag with period=1 and 2, and pad with 0 (returns list)
shift(x, n=1:2, fill=0, type="lag")
# on data.tables
DT = data.table(year=2010:2014, v1=runif(5), v2=1:5, v3=letters[1:5])
# lag columns 'v1,v2,v3' DT by 1 and fill with 0
cols = c("v1","v2","v3")
anscols = paste("lead", cols, sep="_")
DT[, (anscols) := shift(.SD, 1, 0, "lead"), .SDcols=cols]
# return a new data.table instead of updating
# with names automatically set
DT = data.table(year=2010:2014, v1=runif(5), v2=1:5, v3=letters[1:5])
DT[, shift(.SD, 1:2, NA, "lead", TRUE), .SDcols=2:4]
# lag/lead in the right order
DT = data.table(year=2010:2014, v1=runif(5), v2=1:5, v3=letters[1:5])
DT = DT[sample(nrow(DT))]
# add lag=1 for columns 'v1,v2,v3' in increasing order of 'year'
cols = c("v1","v2","v3")
anscols = paste("lag", cols, sep="_")
DT[order(year), (cols) := shift(.SD, 1, type="lag"), .SDcols=cols]
DT[order(year)]
# while grouping
DT = data.table(year=rep(2010:2011, each=3), v1=1:6)
DT[, c("lag1", "lag2") := shift(.SD, 1:2), by=year]
# on lists
ll = list(1:3, letters[4:1], runif(2))
shift(ll, 1, type="lead")
shift(ll, 1, type="lead", give.names=TRUE)
shift(ll, 1:2, type="lead")


##############################
##### subset.data.table ######
##############################

dt <- data.table(a=sample(c('a', 'b', 'c'), 20, replace=TRUE),
                 b=sample(c('a', 'b', 'c'), 20, replace=TRUE),
                 c=sample(20), key=c('a', 'b'))
sub <- subset(dt, a == 'a')
all.equal(key(sub), key(dt))

#################################
##### transform // within  ######
#################################

DT <- data.table(a=rep(1:3, each=2), b=1:6)
DT2 <- transform(DT, c = a^2)
DT[, c:=a^2]
identical(DT,DT2)
DT2 <- within(DT, {
  b <- rev(b)
  c <- a*2
  rm(a)
})
DT[,`:=`(b = rev(b),
         c = a*2,
         a = NULL)]
identical(DT,DT2)
DT$d = ave(DT$b, DT$c, FUN=max) # copies entire DT, even if it is 10GB in RAM

DT = DT[, transform(.SD, d=max(b)), by="c"] # same, but even worse as .SD is copied for each group
DT[, d:=max(b), by="c"] # same result, but much faster, shorter and scales
# Multiple update by group. Convenient, fast, scales and easy to read.
DT[, `:=`(minb = min(b),
          meanb = mean(b),
          bplusd = sum(b+d)), by=c%/%5]

DT

#################################
##### transpose.data.table ######
#################################

ll = list(1:5, 6:8)
transpose(ll)
setDT(transpose(ll, fill=0))[]
dt = data.table(x=1:5, y=6:10)
transpose(dt)

##################################
#### truelength // alloc.col  ####
##################################

DT = data.table(a=1:3,b=4:6)
length(DT) # 2 column pointer slots used
truelength(DT) # 100 column pointer slots allocated
alloc.col(DT,200)
length(DT) # 2 used
?alloc.col
truelength(DT) # 200 allocated, 198 free
DT[,c:=7L] # add new column by assigning to spare slot
truelength(DT)-length(DT) # 197 slots spare


##################################
####        tstrsplit        #####
##################################


x = c("abcde", "ghij", "klmnopq")
strsplit(x, "", fixed=TRUE)
tstrsplit(x, "", fixed=TRUE)
tstrsplit(x, "", fixed=TRUE, fill="<NA>")
DT = data.table(x=c("A/B", "A", "B"), y=1:3)
DT[, c("c1", "c2") := tstrsplit(x, "/", fixed=TRUE)][]
