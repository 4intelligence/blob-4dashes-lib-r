
test_that("Baixa somente um aquivo", {

  expect_null(get_blob('forecast_consumo_sc'))

})

test_that("Baixa mais de um arquivo", {

  expect_null(get_blob('^forecast'))

})

test_that("Nome inválido", {

  expect_error(get_blob('blob_name'))

})

test_that("Recebendo valores não válidos", {

  expect_error(get_blob('forecast_com_co'))
  expect_error(get_blob('pack_com_co'))
  expect_error(get_blob('commercial midwest'))
  expect_error(get_blob(data.frame(y = 'com_co')))

})
