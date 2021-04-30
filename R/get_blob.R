#' @tilte Realiza o download de algum arquivo armazenado no blob storage.
#'
#' @name get_blob
#'
#' @author 4intelligence
#'
#' @details Realiza o download de um(ns) arquivo(s) em específico, dado um nome ou uma regex.
#'
#' @param blob_name nome do arquivo ou regex para buscar vários arquivos.
#' @param dest pasta a ser armazenado os arquivos baixados. Deve ser um caminho para uma pasta.
#'
#' @return Não existe retorno para esta função
#'
#' @example
#' \dontrun {
#' # Baixando um arquivo em específico
#' get_blob('com_co')
#'
#' # Baixando vários arquivos, utilizando uma expressão regular
#' get_blob('^com_')    # Arquivos que começam com o prefixo `com_`.
#' get_blob('.rds$')    # Arquivos em formato `.rds`.
#' get_blob('forecast') # Arquivos que contém `forecast` em seu nome.
#' }
#'
#' @export
get_blob <- function(blob_name, dest = paste0(getwd(), '/download_data/')){

  if(missing(blob_name) || class(blob_name) != 'character')
    stop('Invalid blob name.')

  # Padroniza o nome do blob
  blob_name <- base::tolower(blob_name)

  # Endpoint
  endp <- AzureStor::storage_endpoint(Sys.getenv('AZURE_URL'),
                                      key = Sys.getenv('ACCESS_KEY'))

  # Container
  container <- AzureStor::list_storage_containers(endp)

  if(!Sys.getenv("PROJECT_NAME") %in% names(container))
    stop('Project not found.')

  container <- container[[Sys.getenv("PROJECT_NAME")]]

  # Arquivos no container
  files_on_container <- AzureStor::list_storage_files(container) %>%
    dplyr::select(-isdir, -blobtype, -size)

  # Busca pelo nome do blob no container
  files <- files_on_container %>%
    dplyr::filter(stringr::str_detect(name, blob_name)) %>%
    dplyr::mutate(destino = paste0(dest, name))

  # Caso mais de um arquivo foi encontrado, baixa todos
  if( nrow(files) > 1 ) {
    all_files_found <- paste(files$name, collapse = "\n")

    message("Many files has found. Downloading all files:\n" , all_files_found)

    AzureStor::multidownload_blob(container,
                                  dest = files$destino,
                                  src  = files$name,
                                  overwrite = TRUE)

    message('All files has been downloaded.')

    # return()
  }

  # Caso nenhum arquivo foi encontrado
  if( nrow(files) == 0 ){
    stop('File `', blob_name, '` not found. Try call `files_on_container()` to get',
         ' all available files to download.')
  }

  # Caso encontre apenas um arquivo
  if( nrow(files) == 1 ){

    # Verifica se o arquivo existe
    if( ! file.exists(files$destino[1]) ){
      # Se não existir faz o download
      AzureStor::download_blob(container = container,
                               dest = files$destino[1],
                               src  = files$name[1],
                               overwrite = FALSE)

    }

    # # Retorna o arquivo
    # path <- paste0(files$destino[1])
    # blob <- readRDS(path)
    #
    # return(blob)
  }

  return()

}
