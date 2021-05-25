test_that('Passando parametros',{
  expect_error(files_on_container(forecast))

})

test_that('Verificando se é um data frame',{

  file <- files_on_container()
  expect_type(file, 'list')

})

