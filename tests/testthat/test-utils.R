
context('utils')

test_that('extractRegexMatches() works', {
    content <- 'fred:jim && bob:will'
    matches <- gregexpr('[A-Za-z][A-Za-z0-9]*:[A-Za-z][A-Za-z0-9]*', content)
    expect_equal(extractRegexMatches(content, matches), c('fred:jim', 'bob:will'))
})

test_that('replaceRegexMatches() works', {
    content <- '(fred:jim && bob:will) || fred:glen'
    matches <- gregexpr('[A-Za-z][A-Za-z0-9]*:[A-Za-z][A-Za-z0-9]*', content)
    replaced <- replaceRegexMatches(content, matches, c('pow', 'wow', 'woo'))
    expect_equal(replaced, '(pow && wow) || woo')
})

test_that('htmlToText() works', {
    html <- '<h1>bruce</h1>'
    text <- htmlToText(html)
    expect_equal(text, 'bruce')

    html <- '<h1>bruce</h1>fred'
    text <- htmlToText(html)
    expect_equal(text, 'bruce\n\nfred')

    html <- '<p>p1</p><p>p2</p>'
    text <- htmlToText(html)
    expect_equal(text, 'p1\n\np2')

    html <- '&alpha; = 3'
    text <- htmlToText(html)
    expect_equal(text, '\u03B1 = 3')
})
