
test_that("Retornar um forecast pack (y)", {

  fp <- get_y('consumo_sc')

  expect_type(fp, 'list')

})

test_that("Baixar todos os forecasts packs", {

  expect_null(get_y(y = 'all'))

})

test_that("Recebendo uma lista como parâmetro", {

  expect_error(get_y(list('com_co', 'com_n')))

})

test_that("Recebendo valores não válidos", {

  expect_error(get_y('forecast_com_co'))
  expect_error(get_y('pack_com_co'))
  expect_error(get_y('commercial midwest'))
  expect_error(get_y(data.frame(y = 'com_co')))

})
