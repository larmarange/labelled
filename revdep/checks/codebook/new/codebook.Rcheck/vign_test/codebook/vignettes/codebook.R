## ------------------------------------------------------------------------
knit_by_pkgdown <- !is.null(knitr::opts_chunk$get("fig.retina"))
knitr::opts_chunk$set(warning = TRUE, message = TRUE, error = TRUE, echo = FALSE)
pander::panderOptions("table.split.table", Inf)
ggplot2::theme_set(ggplot2::theme_bw())

library(codebook)
data("bfi", package = 'codebook')
bfi$age <- rpois(nrow(bfi), 30)
library(labelled)
var_label(bfi$age) <- "Alter"

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (exists("testing")) {
	indent = '#' # ugly hack so _regression_summary can be "spun" (variables included via `r ` have to be available)
	results = data.frame()
	survey_repetition = 'single'
	reliabilities = list()
}

## ----repeated------------------------------------------------------------
if (survey_repetition != "single") {
	overview = results %>% dplyr::group_by(session) %>% 
		dplyr::summarise(
			n = sum(!is.na(session)),
			expired = sum(!is.na(expired)),
			ended = sum(!is.na(ended))
		) %>% 
		tidyr::gather(key, value, -session)
	if (length(unique(dplyr::filter(overview, key == "expired")$value)) == 1) {
		overview = dplyr::filter(overview, key != "expired")
	}
	print(
		ggplot2::ggplot(overview, ggplot2::aes(value, ..count..)) + ggplot2::geom_bar() + ggplot2::facet_wrap(~ key, nrow = 1)
	)
}

## ----starting_time-------------------------------------------------------
ggplot2::qplot(results$created) + ggplot2::scale_x_datetime("Date/time when survey was started")

## ----duration------------------------------------------------------------
if (low_vals == 0) {
  warning("Durations below 0 detected.")
}
ggplot2::qplot(duration$duration, binwidth = 0.5) + ggplot2::scale_x_continuous(paste("Duration (in minutes), excluding", high_vals, "values above median + 4*MAD"), limits = c(lower_limit, upper_limit))

## ----setup,eval=TRUE,echo=FALSE------------------------------------------
if (!exists("indent")) {
	indent <- '#'
}
if (exists("testing")) {
  scale_name <- safe_name <- "bla"
	scale <- 1:10
	reliabilities <- list()
	items <- list()
	scale_info <- list(scale_item_names = c("item1", "item2", "item3R"))
}
html_scale_name <- recursive_escape(scale_name)
names(items) <- recursive_escape(names(items))
scale_info <- recursive_escape(attributes(scale))

## ----likert_setup--------------------------------------------------------
old_height <- knitr::opts_chunk$get("fig.height")
new_height <- length(scale_info$scale_item_names)
new_height <- ifelse(new_height > 20, 20, new_height)
new_height <- ifelse(new_height < 1, 1, new_height)
new_height <- ifelse(is.na(new_height) | is.nan(new_height), 
                     old_height, new_height)
knitr::opts_chunk$set(fig.height = new_height)

