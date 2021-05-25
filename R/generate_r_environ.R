# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

#' @title Gera um arquivo para armazenar as variáveis de autenticação
#' @name generate_r_environ
#'
#' @description Função para criar ou adicionar linhas (caso já exista) no arquivo **.Renviron** com os dados fornecidos
#'
#' @param endpoint String com a url base de acesso ao servidor
#' @param access_key String com a senha de acesso do usuário
#' @param  project_name String com o nome do container
#'
#' @author Samuel Souza
#'
#' @details O arquivo será gerado com uma única seção, login, que é por padrão a que todas funções neste pacote utilizam.
#' O local de criação será o mesmo de sua R Session, e irá conter o nome **.Renviron** .
#'
#' Para acessar o valor das credenciais, utilize a função `Sys.getenv()` e passe como parâmetro a variável que deseja.
#'
#' Os valores disponíveis são:
#' `Sys.getenv(AZURE_URL)`
#' `Sys.getenv(ACCESS_KEY)`
#' `Sys.getenv(PROJECT_NAME)`
#'
#' @return Não existe nenhum retorno para a função.
#'
#' @examples
#' generate_r_environ("https://4intelligence.com.br/", "example@@4i.com.br", "dash-de")
#'
#' @export
generate_r_environ <- function(endpoint, access_key,  project_name){

  # Create lines to write in .Renviron file
  lines <- base::paste0("\n# Variables from Azure\n",
                        "AZURE_URL=", endpoint,'\n',
                        "ACCESS_KEY=", access_key, '\n',
                        "PROJECT_NAME=",  project_name)

  # Write/append .Renviron file
  base::write(x = lines, file = '.Renviron', append = TRUE)

  # Restart R session (to validate environment variables)
  .rs.restartR()

}
