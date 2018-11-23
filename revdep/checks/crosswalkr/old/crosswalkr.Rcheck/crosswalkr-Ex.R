pkgname <- "crosswalkr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('crosswalkr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("encodefrom")
### * encodefrom

flush(stderr()); flush(stdout())

### Name: encodefrom
### Title: Encode data frame column using external crosswalk file.
### Aliases: encodefrom encodefrom_

### ** Examples

df <- data.frame(state = c('Kentucky','Tennessee','Virginia'),
                 stfips = c(21,47,51),
                 cenregnm = c('South','South','South'))

df_tbl <- tibble::as_data_frame(df)

cw <- get(data(stcrosswalk))

df$state2 <- encodefrom(df, state, cw, stname, stfips, stabbr)
df_tbl$state2 <- encodefrom(df_tbl, state, cw, stname, stfips, stabbr)
df_tbl$state3 <- encodefrom(df_tbl, state, cw, stname, stfips, stabbr,
                            ignore_tibble = TRUE)

haven::as_factor(df_tbl)
haven::zap_labels(df_tbl)



cleanEx()
nameEx("renamefrom")
### * renamefrom

flush(stderr()); flush(stdout())

### Name: renamefrom
### Title: Rename data frame columns using external crosswalk file.
### Aliases: renamefrom renamefrom_

### ** Examples

df <- data.frame(state = c('Kentucky','Tennessee','Virginia'),
                 fips = c(21,47,51),
                 region = c('South','South','South'))

cw <- data.frame(old_name = c('state','fips'),
                 new_name = c('stname','stfips'),
                 label = c('Full state name', 'FIPS code'))

df1 <- renamefrom(df, cw, old_name, new_name, label)
df2 <- renamefrom(df, cw, old_name, new_name, name_label = TRUE)
df3 <- renamefrom(df, cw, old_name, new_name, drop_extra = FALSE)




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
