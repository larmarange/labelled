pkgname <- "questionr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('questionr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("addNAstr")
### * addNAstr

flush(stderr()); flush(stdout())

### Name: addNAstr
### Title: Transform missing values of a factor to an extra level
### Aliases: addNAstr

### ** Examples

f <- as.factor(c("a","b",NA,"a","b"))
f
addNAstr(f)
addNAstr(f, value="missing")
addNAstr(f, value=NULL)



cleanEx()
nameEx("chisq.residuals")
### * chisq.residuals

flush(stderr()); flush(stdout())

### Name: chisq.residuals
### Title: Return the chi-squared residuals of a two-way frequency table.
### Aliases: chisq.residuals residus

### ** Examples

## Sample table
data(Titanic)
tab <- apply(Titanic, c(1,4), sum)
## Pearson residuals
chisq.residuals(tab)
## Standardized residuals
chisq.residuals(tab, std = TRUE)
## Raw residuals
chisq.residuals(tab, raw = TRUE)



cleanEx()
nameEx("clipcopy")
### * clipcopy

flush(stderr()); flush(stdout())

### Name: clipcopy
### Title: Transform an object into HTML and copy it for export
### Aliases: clipcopy copie copie.default clipcopy.default clipcopy.default
###   copie.proptab clipcopy.proptab copie.proptab
### Keywords: connection

### ** Examples

data(iris)
tab <- table(cut(iris$Sepal.Length,8),cut(iris$Sepal.Width,4))
## Not run: copie(tab)
ptab <- rprop(tab, percent=TRUE)
## Not run: clipcopy(ptab)



cleanEx()
nameEx("cprop")
### * cprop

flush(stderr()); flush(stdout())

### Name: cprop
### Title: Column percentages of a two-way frequency table.
### Aliases: cprop cprop.table cprop.data.frame cprop.matrix cprop.tabyl

### ** Examples

## Sample table
data(Titanic)
tab <- apply(Titanic, c(4,1), sum)
## Column percentages
cprop(tab)
## Column percentages with custom display
cprop(tab, digits=2, percent=TRUE, total=FALSE)



cleanEx()
nameEx("cramer.v")
### * cramer.v

flush(stderr()); flush(stdout())

### Name: cramer.v
### Title: Compute Cramer's V of a two-way frequency table
### Aliases: cramer.v
### Keywords: univar

### ** Examples

data(Titanic)
tab <- apply(Titanic, c(4,1), sum)
#' print(tab)
cramer.v(tab)



cleanEx()
nameEx("cross.multi.table")
### * cross.multi.table

flush(stderr()); flush(stdout())

### Name: cross.multi.table
### Title: Two-way frequency table between a multiple choices question and
###   a factor
### Aliases: cross.multi.table

### ** Examples

## Sample data frame
set.seed(1337)
sex <- sample(c("Man","Woman"),100,replace=TRUE)
jazz <- sample(c(0,1),100,replace=TRUE)
rock <- sample(c(TRUE, FALSE),100,replace=TRUE)
electronic <- sample(c("Y","N"),100,replace=TRUE)
weights <- runif(100)*2
df <- data.frame(sex,jazz,rock,electronic,weights)
## Two-way frequency table on 'music' variables by sex
cross.multi.table(df[,c("jazz", "rock","electronic")], df$sex, true.codes=list("Y"))
## Column percentages based on respondants
cross.multi.table(df[,c("jazz", "rock","electronic")], df$sex, true.codes=list("Y"), freq=TRUE)
## Row percentages based on respondants
cross.multi.table(df[,c("jazz", "rock","electronic")], 
                  df$sex, true.codes=list("Y"), freq=TRUE, tfreq="row", n=TRUE)



cleanEx()
nameEx("describe")
### * describe

flush(stderr()); flush(stdout())

### Name: describe
### Title: Describe the variables of a data.frame
### Aliases: describe describe.factor describe.numeric describe.character
###   describe.default describe.labelled describe.data.frame
###   print.description

### ** Examples

data(hdv2003)
describe(hdv2003$sexe)
describe(hdv2003$age)
data(fecondite)
describe(femmes$milieu)
describe(hdv2003)
describe(hdv2003, "cuisine", "heures.tv")
describe(hdv2003, "trav*")
describe(hdv2003, "trav|lecture")
describe(hdv2003, "trav", "lecture")
describe(femmes)
describe(femmes, "ident")



cleanEx()
nameEx("duplicated2")
### * duplicated2

flush(stderr()); flush(stdout())

### Name: duplicated2
### Title: Determine all duplicate elements
### Aliases: duplicated2

### ** Examples

df <- data.frame(x=c("a","b","c","b","d","c"),y=c(1,2,3,2,4,3))
df
duplicated(df)
duplicated2(df)



cleanEx()
nameEx("fecondite")
### * fecondite

flush(stderr()); flush(stdout())

### Name: fecondite
### Title: A fertility survey
### Aliases: fecondite
### Keywords: datasets

### ** Examples

data(fecondite)
describe(menages)
describe(femmes)
describe(enfants)



cleanEx()
nameEx("fertility")
### * fertility

flush(stderr()); flush(stdout())

### Name: fertility
### Title: A fertility survey
### Aliases: fertility
### Keywords: datasets

### ** Examples

data(fertility)
describe(households)
describe(women)
describe(children)



cleanEx()
nameEx("freq")
### * freq

flush(stderr()); flush(stdout())

### Name: freq
### Title: Generate frequency tables.
### Aliases: freq

### ** Examples

# factor
data(hdv2003)
freq(hdv2003$qualif)
freq(hdv2003$qualif, cum = TRUE, total = TRUE)
freq(hdv2003$qualif, cum = TRUE, total = TRUE, sort ="dec")

# labelled data
data(fecondite)
freq(femmes$region)
freq(femmes$region, levels = "l")
freq(femmes$region, levels = "v")



cleanEx()
nameEx("freq.na")
### * freq.na

flush(stderr()); flush(stdout())

### Name: freq.na
### Title: Generate frequency table of missing values.
### Aliases: freq.na

### ** Examples

data(hdv2003)
## Examine a single vector.
freq.na(hdv2003$qualif)
## Examine a data frame.
freq.na(hdv2003)
## Examine several variables.
freq.na(hdv2003, "nivetud", "trav.satisf")
## To see only variables with the most number of missing values
head(freq.na(hdv2003))



cleanEx()
nameEx("icut")
### * icut

flush(stderr()); flush(stdout())

### Name: icut
### Title: Interactive conversion from numeric to factor
### Aliases: icut

### ** Examples

## Not run: 
##D data(hdv2003)
##D icut(hdv2003, "age")
##D irec(hdv2003, heures.tv) ## this also works
## End(Not run)




cleanEx()
nameEx("iorder")
### * iorder

flush(stderr()); flush(stdout())

### Name: iorder
### Title: Interactive reordering of factor levels
### Aliases: iorder

### ** Examples

## Not run: 
##D data(hdv2003)
##D iorder(hdv2003, "qualif")
## End(Not run)



cleanEx()
nameEx("irec")
### * irec

flush(stderr()); flush(stdout())

### Name: irec
### Title: Interactive recoding
### Aliases: irec

### ** Examples

## Not run: 
##D data(hdv2003)
##D irec()
##D v <- sample(c("Red","Green","Blue"), 50, replace=TRUE)
##D irec(v)
##D irec(hdv2003, "qualif")
##D irec(hdv2003, sexe) ## this also works
## End(Not run)



cleanEx()
nameEx("lookfor")
### * lookfor

flush(stderr()); flush(stdout())

### Name: lookfor
### Title: Look for keywords variable names and descriptions
### Aliases: lookfor

### ** Examples

lookfor(iris)
# Look for a single keyword.
lookfor(iris, "petal")
lookfor(iris, "s")
# Look for with a regular expression
lookfor(iris, "petal|species")
lookfor(iris, "s$")
# Look for with several keywords
lookfor(iris, "pet", "sp")
lookfor(iris, "pet", "sp", "width")
# Load memisc package and example data.
## Not run: 
##D require(memisc)
##D nes1948.por <- UnZip("anes/NES1948.ZIP","NES1948.POR", package="memisc")
##D nes1948 <- spss.portable.file(nes1948.por)
##D # Look for a vector of keywords.
##D lookfor(nes1948, c("Truman", "Dewey"))
##D # Look for a regular expression.
##D lookfor(nes1948, "truman|dewey")
##D # Look for a phrase.
##D lookfor(nes1948, "personal attribute")
## End(Not run)
# Labelled data
data(fecondite)
lookfor(femmes)
lookfor(femmes, "date")
# Display details
lookfor(femmes, details = TRUE)



cleanEx()
nameEx("ltabs")
### * ltabs

flush(stderr()); flush(stdout())

### Name: ltabs
### Title: Cross tabulation with labelled variables
### Aliases: ltabs

### ** Examples

data(fecondite)
ltabs(~radio, femmes)
ltabs(~radio+tv, femmes)
ltabs(~radio+tv, femmes, "l")
ltabs(~radio+tv, femmes, "v")
ltabs(~radio+tv+journal, femmes)
ltabs(~radio+tv, femmes, variable_label = FALSE)



cleanEx()
nameEx("multi.split")
### * multi.split

flush(stderr()); flush(stdout())

### Name: multi.split
### Title: Split a multiple choices variable in a series of binary
###   variables
### Aliases: multi.split

### ** Examples

v <- c("red/blue","green","red/green","blue/red")
multi.split(v)
## One-way frequency table of the result
multi.table(multi.split(v))



cleanEx()
nameEx("multi.table")
### * multi.table

flush(stderr()); flush(stdout())

### Name: multi.table
### Title: One-way frequency table for multiple choices question
### Aliases: multi.table

### ** Examples

## Sample data frame
set.seed(1337)
sex <- sample(c("Man","Woman"),100,replace=TRUE)
jazz <- sample(c(0,1),100,replace=TRUE)
rock <- sample(c(TRUE, FALSE),100,replace=TRUE)
electronic <- sample(c("Y","N"),100,replace=TRUE)
weights <- runif(100)*2
df <- data.frame(sex,jazz,rock,electronic,weights)
## Frequency table on 'music' variables
multi.table(df[,c("jazz", "rock","electronic")], true.codes=list("Y"))
## Weighted frequency table on 'music' variables
multi.table(df[,c("jazz", "rock","electronic")], true.codes=list("Y"), weights=df$weights)
## No percentages
multi.table(df[,c("jazz", "rock","electronic")], true.codes=list("Y"), freq=FALSE)



cleanEx()
nameEx("na.rm")
### * na.rm

flush(stderr()); flush(stdout())

### Name: na.rm
### Title: Remove observations with missing values
### Aliases: na.rm

### ** Examples

df <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA), z= c("a",NA,"b"))
df
na.omit(df)
na.rm(df)
na.rm(df, c("x","y"))
na.rm(df, "z")



cleanEx()
nameEx("odds.ratio")
### * odds.ratio

flush(stderr()); flush(stdout())

### Name: odds.ratio
### Title: Odds Ratio
### Aliases: odds.ratio odds.ratio.glm odds.ratio.multinom
###   odds.ratio.factor odds.ratio.table odds.ratio.matrix
###   odds.ratio.numeric print.odds.ratio

### ** Examples

data(hdv2003)
reg <- glm(cinema ~ sexe + age, data=hdv2003, family=binomial)
odds.ratio(reg)
odds.ratio(hdv2003$sport, hdv2003$cuisine)
odds.ratio(table(hdv2003$sport, hdv2003$cuisine))
M <- matrix(c(759, 360, 518, 363), ncol = 2)
odds.ratio(M)
odds.ratio(0.26, 0.42)



cleanEx()
nameEx("prop")
### * prop

flush(stderr()); flush(stdout())

### Name: prop
### Title: Global percentages of a two-way frequency table.
### Aliases: prop prop_table prop.data.frame prop.matrix prop.tabyl

### ** Examples

## Sample table
data(Titanic)
tab <- apply(Titanic, c(1,4), sum)
## Percentages
prop(tab)
## Percentages with custom display
prop(tab, digits=2, percent=TRUE, total=FALSE, n=TRUE)



cleanEx()
nameEx("qload")
### * qload

flush(stderr()); flush(stdout())

### Name: qload
### Title: Load one or more packages, installing them first if necessary
### Aliases: qload

### ** Examples

qload("questionr")
qload("questionr", silent = FALSE)



cleanEx()
nameEx("qscan")
### * qscan

flush(stderr()); flush(stdout())

### Name: qscan
### Title: Scan R scripts and load/install all detected packages
### Aliases: qscan

### ** Examples

## Scan the working directory.
## Not run: qscan()



cleanEx()
nameEx("quant.cut")
### * quant.cut

flush(stderr()); flush(stdout())

### Name: quant.cut
### Title: Transform a quantitative variable into a qualitative variable
### Aliases: quant.cut

### ** Examples

data(iris)
sepal.width3cl <- quant.cut(iris$Sepal.Width,3)
table(sepal.width3cl)



cleanEx()
nameEx("recode.na")
### * recode.na

flush(stderr()); flush(stdout())

### Name: recode.na
### Title: Recode values of a variable to missing values, using exact or
###   regular expression matching.
### Aliases: recode.na

### ** Examples

data(hdv2003)
## With exact string matches.
hdv2003$nivetud = recode.na(hdv2003$nivetud, "Inconnu")
## With regular expressions.
hdv2003$relig = recode.na(hdv2003$relig, "[A|a]ppartenance", "Rejet|NSP")
## Showing missing values. 
hdv2003$clso = recode.na(hdv2003$clso, "Ne sait pas", verbose = TRUE)
## Test results with freq.
freq(recode.na(hdv2003$trav.satisf, "Equilibre"))
## Truncate a count variable (recommends numeric conversion).
freq(recode.na(hdv2003$freres.soeurs, 5:22))



cleanEx()
nameEx("rename.variable")
### * rename.variable

flush(stderr()); flush(stdout())

### Name: rename.variable
### Title: Rename a data frame column
### Aliases: rename.variable renomme.variable
### Keywords: manip

### ** Examples

data(iris)
str(iris)
iris <- rename.variable(iris, "Species", "especes")
str(iris)



cleanEx()
nameEx("rm.unused.levels")
### * rm.unused.levels

flush(stderr()); flush(stdout())

### Name: rm.unused.levels
### Title: Remove unused levels
### Aliases: rm.unused.levels

### ** Examples

df <- data.frame(v1=c("a","b","a","b"),v2=c("x","x","y","y"))
df$v1 <- factor(df$v1,c("a","b","c"))
df$v2 <- factor(df$v2,c("x","y","z"))
df
str(df)
str(rm.unused.levels(df))
str(rm.unused.levels(df,"v1"))



cleanEx()
nameEx("rprop")
### * rprop

flush(stderr()); flush(stdout())

### Name: rprop
### Title: Row percentages of a two-way frequency table.
### Aliases: rprop lprop rprop.table rprop.data.frame rprop.matrix
###   rprop.tabyl

### ** Examples

## Sample table
data(Titanic)
tab <- apply(Titanic, c(1,4), sum)
## Column percentages
rprop(tab)
## Column percentages with custom display
rprop(tab, digits=2, percent=TRUE, total=FALSE)



cleanEx()
nameEx("tabs")
### * tabs

flush(stderr()); flush(stdout())

### Name: tabs
### Title: Weighted Crossresult
### Aliases: tabs

### ** Examples

data(hdv2003) 
tabs(hdv2003, x = "relig", y = c("qualif", "trav.imp"), weight = "poids")
result <- tabs(hdv2003, x = "relig", y = c("qualif", "trav.imp"), type = "counts")
format(result, digits = 3)
# library(knitr)
# xt <- tabs(hdv2003, x = "relig", y = c("qualif", "trav.imp"), weight = "poids")
# kable(format(xt))                        # to use with RMarkdown...




cleanEx()
nameEx("wtd.mean")
### * wtd.mean

flush(stderr()); flush(stdout())

### Name: wtd.mean
### Title: Weighted mean and variance of a vector
### Aliases: wtd.mean wtd.var

### ** Examples

data(hdv2003)
mean(hdv2003$age)
wtd.mean(hdv2003$age, weights=hdv2003$poids)
var(hdv2003$age)
wtd.var(hdv2003$age, weights=hdv2003$poids)



cleanEx()
nameEx("wtd.table")
### * wtd.table

flush(stderr()); flush(stdout())

### Name: wtd.table
### Title: Weighted one-way and two-way frequency tables.
### Aliases: wtd.table

### ** Examples

data(hdv2003)
wtd.table(hdv2003$sexe, weights=hdv2003$poids)
wtd.table(hdv2003$sexe, weights=hdv2003$poids, normwt=TRUE)
table(hdv2003$sexe, hdv2003$hard.rock)
wtd.table(hdv2003$sexe, hdv2003$hard.rock, weights=hdv2003$poids)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
