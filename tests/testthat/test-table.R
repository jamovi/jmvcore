
context('table')

test_that('Table works', {

    table <- Table$new()
    table$addColumn(
        name='1',
        title='Column 1',
        type='text')
    table$addColumn(
        name='2',
        title='Column 2',
        type='text')
    table$addColumn(
        name='3',
        title='Column 3',
        type='text')

    table$addRow(rowKey=1)
    table$addRow(rowKey=2)
    table$addRow(rowKey=3)

    table$setRow(rowKey=1, values=list(`1`='x', `2`='y', `3`='z'))
    table$setRow(rowKey=2, values=list(`1`='a', `2`='b'))
    table$setRow(rowKey=3, values=list(`1`='c'))

    expect_equal(table$isFilled(), FALSE)

    # columns
    expect_equal(table$isFilled(col=1), TRUE)
    expect_equal(table$isFilled(col=2), FALSE)
    expect_equal(table$isFilled(col=3), FALSE)
    expect_equal(table$isFilled(col='1'), TRUE)
    expect_equal(table$isFilled(col='2'), FALSE)
    expect_equal(table$isFilled(col='3'), FALSE)

    # rows
    expect_equal(table$isFilled(rowNo=1), TRUE)
    expect_equal(table$isFilled(rowNo=2), FALSE)
    expect_equal(table$isFilled(rowNo=3), FALSE)
    expect_equal(table$isFilled(rowKey=1), TRUE)
    expect_equal(table$isFilled(rowKey=2), FALSE)
    expect_equal(table$isFilled(rowKey=3), FALSE)

    # cells
    expect_equal(table$isFilled(rowNo=1, col=1), TRUE)
    expect_equal(table$isFilled(rowNo=2, col=1), TRUE)
    expect_equal(table$isFilled(rowNo=3, col=1), TRUE)
    expect_equal(table$isFilled(rowNo=1, col=2), TRUE)
    expect_equal(table$isFilled(rowNo=2, col=2), TRUE)
    expect_equal(table$isFilled(rowNo=3, col=2), FALSE)
    expect_equal(table$isFilled(rowNo=1, col=3), TRUE)
    expect_equal(table$isFilled(rowNo=2, col=3), FALSE)
    expect_equal(table$isFilled(rowNo=3, col=3), FALSE)

    expect_equal(table$isFilled(rowKey=1, col=1), TRUE)
    expect_equal(table$isFilled(rowKey=2, col=1), TRUE)
    expect_equal(table$isFilled(rowKey=3, col=1), TRUE)
    expect_equal(table$isFilled(rowKey=1, col=2), TRUE)
    expect_equal(table$isFilled(rowKey=2, col=2), TRUE)
    expect_equal(table$isFilled(rowKey=3, col=2), FALSE)
    expect_equal(table$isFilled(rowKey=1, col=3), TRUE)
    expect_equal(table$isFilled(rowKey=2, col=3), FALSE)
    expect_equal(table$isFilled(rowKey=3, col=3), FALSE)

})
