#Ok
test_that('Sem parametros na função',{

  expect_error(generate_r_environ())

})
#Ok
test_that('Apenas um parametro',{

  expect_error(generate_r_environ('a'))

})
#Ok
test_that('Generate environ',{

  expect_error(generate_r_environ('a','b'))

})
#Falha
test_that('Todos os parametro',{

  expect_error(generate_r_environ('a','b','c'))

})
