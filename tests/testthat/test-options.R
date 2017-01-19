
context('options')

number <- OptionNumber$new(
    'number',
    min=0,
    max=1)

number$value <- 7

test_that('OptionNumber works', {
    number$value <- 0.5
    expect_silent(number$check())
    number$value <- 5
    expect_error(number$check(), 'number must be between 0 and 1 (is 5)', fixed=TRUE)
})

