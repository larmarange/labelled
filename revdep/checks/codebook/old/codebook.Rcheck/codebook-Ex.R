pkgname <- "codebook"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('codebook')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("aggregate_and_document_scale")
### * aggregate_and_document_scale

flush(stderr()); flush(stdout())

### Name: aggregate_and_document_scale
### Title: Aggregate variables and remember which variables this were
### Aliases: aggregate_and_document_scale

### ** Examples

testdf <- data.frame(bfi_neuro_1 = rnorm(20), bfi_neuro_2 = rnorm(20),
                    bfi_neuro_3R = rnorm(20), age = rpois(20, 30))
item_names <- c('bfi_neuro_1', 'bfi_neuro_2', 'bfi_neuro_3R')
testdf$bfi_neuro <- aggregate_and_document_scale(testdf[, item_names])
testdf$bfi_neuro



cleanEx()
nameEx("asis_knit_child")
### * asis_knit_child

flush(stderr()); flush(stdout())

### Name: asis_knit_child
### Title: Knit a child document and output as is (render markup)
### Aliases: asis_knit_child

### ** Examples

## Not run: 
##D # an example of a wrapper function that calls asis_knit_child with an argument
##D # ensures distinct paths for cache and figures, so that these calls can be looped in parallel
##D regression_summary <- function(model) {
##D    hash <- digest::digest(model)
##D    options <- list(
##D        fig.path = paste0(knitr::opts_chunk$get("fig.path"), hash, "-"),
##D        cache.path = paste0(knitr::opts_chunk$get("cache.path"), hash, "-"))
##D    asis_knit_child("_regression_summary.Rmd", options = options)
##D }
## End(Not run)



cleanEx()
nameEx("codebook")
### * codebook

flush(stderr()); flush(stdout())

### Name: codebook
### Title: Generate rmarkdown codebook
### Aliases: codebook

### ** Examples

# will generate figures in a temporary directory
old_base_dir <- knitr::opts_knit$get("base.dir")
knitr::opts_knit$set(base.dir = tempdir())
on.exit(knitr::opts_knit$set(base.dir = old_base_dir))
data("bfi")
bfi <- bfi[, c("BFIK_open_1", "BFIK_open_1")]
md <- codebook(bfi, survey_repetition = "single", metadata_table = FALSE)



cleanEx()
nameEx("codebook_component_scale")
### * codebook_component_scale

flush(stderr()); flush(stdout())

### Name: codebook_component_scale
### Title: Codebook component for scales
### Aliases: codebook_component_scale

### ** Examples

# will generate figures in a temporary directory
old_base_dir <- knitr::opts_knit$get("base.dir")
knitr::opts_knit$set(base.dir = tempdir())
on.exit(knitr::opts_knit$set(base.dir = old_base_dir))
data("bfi")
bfi <- bfi[,c("BFIK_open", paste0("BFIK_open_", 1:4))]
codebook_component_scale(bfi[,1], "BFIK_open", bfi[,-1],
   reliabilities = list(BFIK_open = psych::alpha(bfi[,-1])))



cleanEx()
nameEx("codebook_component_single_item")
### * codebook_component_single_item

flush(stderr()); flush(stdout())

### Name: codebook_component_single_item
### Title: Codebook component for single items
### Aliases: codebook_component_single_item

### ** Examples

# will generate figure in a temporary directory
old_base_dir <- knitr::opts_knit$get("base.dir")
knitr::opts_knit$set(base.dir = tempdir())
on.exit(knitr::opts_knit$set(base.dir = old_base_dir))
data("bfi")
codebook_component_single_item(bfi$BFIK_open_1, "BFIK_open_1")



cleanEx()
nameEx("codebook_items")
### * codebook_items

flush(stderr()); flush(stdout())

### Name: codebook_items
### Title: Tabular codebook
### Aliases: codebook_items

### ** Examples

data("bfi")
## Not run: 
##D # doesn't show interactively, because a html widget needs to be registered
##D codebook_items(bfi)
## End(Not run)



cleanEx()
nameEx("codebook_missingness")
### * codebook_missingness

flush(stderr()); flush(stdout())

### Name: codebook_missingness
### Title: Codebook missingness
### Aliases: codebook_missingness

### ** Examples

data("bfi")
codebook_missingness(bfi)



cleanEx()
nameEx("codebook_survey_overview")
### * codebook_survey_overview

flush(stderr()); flush(stdout())

### Name: codebook_survey_overview
### Title: Codebook survey overview
### Aliases: codebook_survey_overview

### ** Examples

# will generate figures in a figure/ subdirectory
data("bfi")
codebook_survey_overview(bfi)



cleanEx()
nameEx("codebook_table")
### * codebook_table

flush(stderr()); flush(stdout())

### Name: codebook_table
### Title: Codebook metadata table
### Aliases: codebook_table

### ** Examples

data("bfi")
codebook_table(bfi)



cleanEx()
nameEx("compute_reliabilities")
### * compute_reliabilities

flush(stderr()); flush(stdout())

### Name: compute_reliabilities
### Title: Compute reliabilities
### Aliases: compute_reliabilities

### ** Examples

data("bfi", package = "codebook")
reliabilities <- compute_reliabilities(bfi)



cleanEx()
nameEx("detect_scales")
### * detect_scales

flush(stderr()); flush(stdout())

### Name: detect_scales
### Title: Detect item scales
### Aliases: detect_scales

### ** Examples

bfi <- data.frame(matrix(data = rnorm(500), ncol = 5))
names(bfi) <- c("bfi_e1", "bfi_e2R", "bfi_e3", "bfi_n1", "bfi_n2")
bfi$bfi_e <- rowMeans(bfi[, c("bfi_e1", "bfi_e2R", "bfi_e3")])
bfi <- detect_scales(bfi)
bfi$bfi_e



cleanEx()
nameEx("ended")
### * ended

flush(stderr()); flush(stdout())

### Name: ended
### Title: How many surveys were ended?
### Aliases: ended

### ** Examples

survey <- data.frame(ended = c("2016-05-28 10:11:00", NA, "2016-05-30 11:18:28"))
ended(survey = survey)



cleanEx()
nameEx("expired")
### * expired

flush(stderr()); flush(stdout())

### Name: expired
### Title: How many surveys were expired?
### Aliases: expired

### ** Examples

survey <- data.frame(expired = c(NA, "2016-05-29 10:11:00", NA))
expired(survey = survey)



cleanEx()
nameEx("has_label")
### * has_label

flush(stderr()); flush(stdout())

### Name: has_label
### Title: Has label
### Aliases: has_label

### ** Examples

example("labelled", "haven")
has_label(x)



cleanEx()
nameEx("has_labels")
### * has_labels

flush(stderr()); flush(stdout())

### Name: has_labels
### Title: Has labels
### Aliases: has_labels

### ** Examples

example("labelled", "haven")
has_labels(x)



cleanEx()
nameEx("knit_print.alpha")
### * knit_print.alpha

flush(stderr()); flush(stdout())

### Name: knit_print.alpha
### Title: Pretty-print a Cronbach's alpha object
### Aliases: knit_print.alpha

### ** Examples

example("alpha", "psych")
knitr::knit_print(a4)




cleanEx()
nameEx("knit_print.htest")
### * knit_print.htest

flush(stderr()); flush(stdout())

### Name: knit_print.htest
### Title: Print a 'stats::cor.test()' object for knitr
### Aliases: knit_print.htest

### ** Examples

knitr::knit_print(cor.test(rnorm(100), rnorm(100)))




cleanEx()
nameEx("knit_print.multilevel")
### * knit_print.multilevel

flush(stderr()); flush(stdout())

### Name: knit_print.multilevel
### Title: Print a 'psych::multilevel.reliability()' object for knitr
### Aliases: knit_print.multilevel

### ** Examples

example("mlr", "psych")
knitr::knit_print(mg)




cleanEx()
nameEx("label_browser_static")
### * label_browser_static

flush(stderr()); flush(stdout())

### Name: label_browser_static
### Title: Browse and search variable and value labels
### Aliases: label_browser_static

### ** Examples

label_browser_static(bfi)





cleanEx()
nameEx("likert_from_items")
### * likert_from_items

flush(stderr()); flush(stdout())

### Name: likert_from_items
### Title: Derive a likert object from items
### Aliases: likert_from_items

### ** Examples

data("bfi", package = "codebook")
open_items <- paste0("BFIK_open_",1:4)
graphics::plot(likert_from_items(bfi[, open_items]))



cleanEx()
nameEx("md_pattern")
### * md_pattern

flush(stderr()); flush(stdout())

### Name: md_pattern
### Title: Missing data patterns
### Aliases: md_pattern

### ** Examples

data("nhanes", package = "mice")
md_pattern(nhanes)
md_pattern(nhanes, only_vars_with_missings = FALSE, min_freq = 0.2)



cleanEx()
nameEx("metadata_jsonld")
### * metadata_jsonld

flush(stderr()); flush(stdout())

### Name: metadata_jsonld
### Title: Metadata as JSON-LD
### Aliases: metadata_jsonld

### ** Examples

data("bfi")
metadata_jsonld(bfi)



cleanEx()
nameEx("metadata_list")
### * metadata_list

flush(stderr()); flush(stdout())

### Name: metadata_list
### Title: Metadata from dataframe
### Aliases: metadata_list

### ** Examples

data("bfi")
md_list <- metadata_list(bfi)
md_list$variableMeasured[[20]]



cleanEx()
nameEx("modified")
### * modified

flush(stderr()); flush(stdout())

### Name: modified
### Title: How many surveys were modified?
### Aliases: modified

### ** Examples

survey <- data.frame(modified = c(NA, "2016-05-29 10:11:00", NA))
modified(survey = survey)



cleanEx()
nameEx("paste.knit_asis")
### * paste.knit_asis

flush(stderr()); flush(stdout())

### Name: paste.knit_asis
### Title: Paste and output as is (render markup)
### Aliases: paste.knit_asis

### ** Examples

paste.knit_asis("# Headline 1", "## Headline 2")



cleanEx()
nameEx("plot_labelled")
### * plot_labelled

flush(stderr()); flush(stdout())

### Name: plot_labelled
### Title: Plot labelled vector
### Aliases: plot_labelled

### ** Examples

data("bfi", package = "codebook")
plot_labelled(bfi$BFIK_open_1)



cleanEx()
nameEx("reverse_labelled_values")
### * reverse_labelled_values

flush(stderr()); flush(stdout())

### Name: reverse_labelled_values
### Title: Reverse labelled values reverse the underlying valus for a
###   numeric 'haven::labelled()' vector while keeping the labels correct
### Aliases: reverse_labelled_values

### ** Examples

x <- haven::labelled(rep(1:3, each = 3), c(Bad = 1, Good = 5))
x
reverse_labelled_values(x)



cleanEx()
nameEx("summary.labelled")
### * summary.labelled

flush(stderr()); flush(stdout())

### Name: summary.labelled
### Title: Summary function for labelled vector
### Aliases: summary.labelled

### ** Examples

example("labelled", "haven")
summary(x)




cleanEx()
nameEx("summary.labelled_spss")
### * summary.labelled_spss

flush(stderr()); flush(stdout())

### Name: summary.labelled_spss
### Title: Summary function for labelled_spss vector
### Aliases: summary.labelled_spss

### ** Examples

example("labelled", "haven")
summary(x)




cleanEx()
nameEx("zap_attributes")
### * zap_attributes

flush(stderr()); flush(stdout())

### Name: zap_attributes
### Title: Zap attributes
### Aliases: zap_attributes

### ** Examples

bfi <- data.frame(matrix(data = rnorm(300), ncol = 3))
names(bfi) <- c("bfi_e1", "bfi_e2R", "bfi_e3")
attributes(bfi$bfi_e1)$label <- "I am outgoing."
attributes(bfi$bfi_e2R)$label <- "I prefer books to people."
attributes(bfi$bfi_e3)$label <- "I love to party."
bfi$bfi_e <- rowMeans(bfi[, c("bfi_e1", "bfi_e2R", "bfi_e3")])
bfi <- detect_scales(bfi, quiet = TRUE) # create attributes
str(zap_attributes(bfi, "label"))
zap_attributes(bfi$bfi_e)



cleanEx()
nameEx("zap_label")
### * zap_label

flush(stderr()); flush(stdout())

### Name: zap_label
### Title: Zap variable label
### Aliases: zap_label

### ** Examples

x <- haven::labelled(rep(1:5, each = 1), c(Bad = 1, Good = 5))
zap_label(x)



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
