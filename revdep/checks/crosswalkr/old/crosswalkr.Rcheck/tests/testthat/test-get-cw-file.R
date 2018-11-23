context('get_cw_file')

## delimited (pipe)
test_that('Failed to read crosswalk file of type: delimited (pipe)', {

    cw <- get_cw_file('./testdata/cw.txt', delimiter = '|')
    expect_is(cw, 'data.frame')
    expect_identical(names(cw), c('a','b'))
    expect_identical(unlist(cw[1,], use.names = FALSE), c('1','2'))

})

## delimited (CSV)
test_that('Failed to read crosswalk file of type: delimited (CSV)', {

    cw <- get_cw_file('./testdata/cw.csv')
    expect_is(cw, 'data.frame')
    expect_identical(names(cw), c('a','b'))
    expect_identical(unlist(cw[1,], use.names = FALSE), c('1','2'))

})

## delimited (TSV)
test_that('Failed to read crosswalk file of type: delimited (TSV)', {

    cw <- get_cw_file('./testdata/cw.tsv')
    expect_is(cw, 'data.frame')
    expect_identical(names(cw), c('a','b'))
    expect_identical(unlist(cw[1,], use.names = FALSE), c('1','2'))

})

## Excel (XLS)
test_that('Failed to read crosswalk file of type: Excel (XLS)', {

    cw <- get_cw_file('./testdata/cw.xls')
    expect_is(cw, 'data.frame')
    expect_identical(names(cw), c('a','b'))
    expect_identical(unlist(cw[1,], use.names = FALSE), c('1','2'))

})

## Excel (XLSX)
test_that('Failed to read crosswalk file of type: Excel (XLSX)', {

    cw <- get_cw_file('./testdata/cw.xlsx')
    expect_is(cw, 'data.frame')
    expect_identical(names(cw), c('a','b'))
    expect_identical(unlist(cw[1,], use.names = FALSE), c('1','2'))

})

## Stata
test_that('Failed to read crosswalk file of type: Stata (DTA)', {

    cw <- get_cw_file('./testdata/cw.dta')
    expect_is(cw, 'data.frame')
    expect_identical(names(cw), c('a','b'))
    expect_identical(unlist(cw[1,], use.names = FALSE), c('1','2'))

})


