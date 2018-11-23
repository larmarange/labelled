context('encodefrom')

## -----------------------------------------------------------------------------
## PRELIM
## -----------------------------------------------------------------------------

## set up data
df <- tibble::data_frame(state = c('Texas','Louisiana','Massachusetts',
                                   'Florida','Oregon','Montana',
                                   'Washington','Arkansas','Tennessee',
                                   'Mississippi','Utah','Alabama',
                                   'Kentucky','Maine','Michigan',
                                   'California','Illinois','Delaware',
                                   'Idaho','North Carolina','Nebraska',
                                   'Maryland','Virginia','Pennsylvania',
                                   'Rhode Island','South Carolina','Iowa',
                                   'New Hampshire','Kansas','Nevada',
                                   'South Dakota','Colorado','Arizona',
                                   'Alaska','New York','Oklahoma',
                                   'Wisconsin','Wyoming','Connecticut',
                                   'Missouri','Minnesota','Ohio',
                                   'Georgia','New Jersey','Vermont',
                                   'District of Columbia','North Dakota',
                                   'New Mexico','West Virginia','Indiana',
                                   'Hawaii'),
                         y = seq(1:51))

crosswalk_file <- system.file('extdata', 'stcrosswalk.rda',
                              package = 'crosswalkr')

## encode vector
vec <- encodefrom(df, state, crosswalk_file, stname, stfips, stabbr)

## -----------------------------------------------------------------------------
## TESTS
## -----------------------------------------------------------------------------

test_that('Not proper labelled class', {

    expect_is(vec, 'labelled')

})

test_that('Failed to properly label vector', {

    lab_names <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI',
                   'ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN',
                   'MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH',
                   'OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA',
                   'WV','WI','WY')
    lab_vals <- as.character(c(1:2,4:6,8:13,15:42,44:51,53:56))
    names(lab_vals) <- lab_names

    expect_equal(names(labelled::val_labels(vec)), lab_names)
    expect_equal(labelled::val_labels(vec), lab_vals)

})

test_that('Failed to assign proper values', {

    act_vals <- c('48','22','25','12','41','30','53','5','47','28','49','1',
                  '21','23','26','6','17','10','16','37','31','24','51','42',
                  '44','45','19','33','20','32','46','8','4','2','36','40',
                  '55','56','9','29','27','39','13','34','50','11','38','35',
                  '54','18','15')

    expect_equal(haven::zap_labels(vec), act_vals)

})

## encode vector (factor)
vec <- encodefrom(df, state, crosswalk_file, stname, stfips,
                  stabbr, ignore_tibble = TRUE)

test_that('Not proper factor class', {

    expect_is(vec, 'factor')

})

test_that('Failed to properly label factor', {

    lab_names <- c('AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI',
                   'ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN',
                   'MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH',
                   'OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA',
                   'WV','WI','WY')
    lab_vals <- as.character(c(1:2,4:6,8:13,15:42,44:51,53:56))
    names(lab_vals) <- lab_names

    expect_equal(levels(vec), lab_names)
    expect_equal(labels(vec), as.character(1:51))

})

test_that('Failed to assign proper values', {

    act_vals <- c('TX','LA','MA','FL','OR','MT','WA','AR','TN','MS','UT',
                  'AL','KY','ME','MI','CA','IL','DE','ID','NC','NE','MD',
                  'VA','PA','RI','SC','IA','NH','KS','NV','SD','CO','AZ',
                  'AK','NY','OK','WI','WY','CT','MO','MN','OH','GA','NJ',
                  'VT','DC','ND','NM','WV','IN','HI')

    expect_equal(as.character(vec), act_vals)

})


