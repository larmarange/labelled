context('confirm_col')

## get crosswalk
cw <- get_cw_file('./testdata/cw.csv')

test_that('Failed to confirm actual columns', {

    expect_error(confirm_col(cw, 'a', 'm1'), NA)
    expect_error(confirm_col(cw, 'b', 'm1'), NA)

})

test_that('Failed to throw error for missing columns', {

    expect_error(confirm_col(cw, 'z', 'm1'))
    expect_error(confirm_col(cw, 'z', 'm2'))

})

