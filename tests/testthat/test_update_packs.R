test_that('Baixando todos os forecast',{

  expect_message(update_packs(),'Uploaded all files in `packs/` folder.')

})
test_that('passando o pack numerico',{

  expect_message(update_packs(pack=1),'Invalid blob name!')

})
test_that('passando o path numerico',{

  expect_message(update_packs(path=2),'Invalid path!')

})
