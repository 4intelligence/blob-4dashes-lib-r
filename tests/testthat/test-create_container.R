
test_that("Criando container", {

  expect_message(create_container(), "The container 'teste-lib' has been created!")

})

test_that("Container já criado", {

  expect_error(create_container())

})

test_that("Parâmetros para a função", {

  expect_error(create_container("nome-do-container"))

})
