context('recodefrom')

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

## set up crosswalk
crosswalk_file <- tibble::data_frame(raw_ = c('state', 'y'),
                                     clean_ = c('stname', 'new_y'),
                                     label_ = c('Full Name', 'New Y'))

## create new cleaned data
new_df <- renamefrom(df, crosswalk_file, raw_, clean_, label_)

## -----------------------------------------------------------------------------
## TESTS
## -----------------------------------------------------------------------------

test_that('Does not output data.frame/tbl/tbl_df', {

    expect_is(new_df, 'data.frame')

})

test_that('Failed to properly rename columns', {

    expect_equal(names(new_df), c('stname','new_y'))

})

test_that('Failed to label columns correctly', {

    expect_equal(unlist(labelled::var_label(new_df), use.names = FALSE),
                        c('Full Name', 'New Y'))

})

