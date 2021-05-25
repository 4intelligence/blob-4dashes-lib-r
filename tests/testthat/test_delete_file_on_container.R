test_that('Não passando o nome do forecast',{

  expect_error(delete_file_on_container())

})
test_that('Passando o nome do forecast invalido',{

  expect_message(delete_file_on_container('nome do forecast'),"Invalid file name.")

})
test_that('Passando o nome  numerico',{

  expect_message(delete_file_on_container(23),"Invalid paramter 'name'! It must be a class 'character'.")

})
# test_that('Passando um nome valido',{
#
#   expect_message(delete_file_on_container('com_co'),"The file 'com_co' has been delete!")
#
# })
