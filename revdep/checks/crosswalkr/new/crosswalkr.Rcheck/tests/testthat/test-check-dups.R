context('check_dups')

## get crosswalk
cw <- get_cw_file('./testdata/cw_dup.csv')

test_that('Failed to catch duplicate values in column', {

    expect_error(check_dups(cw, 'a', 'm1'))
    expect_error(check_dups(cw, 'b', 'm1'))
    expect_error(check_dups(cw, 'a', 'm2'))
    expect_error(check_dups(cw, 'b', 'm2'))

})

