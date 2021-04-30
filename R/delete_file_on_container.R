#' Utilize essa função para a exclusão de um arquivo no Azure Blob Storage.
#'
#'
#' @param name nome do arquivo que deve ser excluido
#'   Exemplo: `dataset_flex.rds`
#' @export

delete_file_on_container <- function(name){

  # Criando o endpoint
  endpoint <- AzureStor::storage_endpoint(Sys.getenv('AZURE_URL'), key = Sys.getenv('ACCESS_KEY'))

   # Listando todos os containers
  container_list <- AzureStor::list_storage_containers(endpoint)

  project_name <- Sys.getenv('PROJECT_NAME')

   # Check if container exists
  if(!project_name %in% container_list)
    stop('Project not found.')

  # Pegando o container do dash
  container <- container_list[[project_name]]

  #Listando os arquivos do container
  files_on_container <- AzureStor::list_storage_files(container)

  # Preparing the variables
  try({
    name <- tolower(name)
  }, silent = TRUE)

  if(class(name) != 'character')
    stop("Invalid paramter 'name'! It must be a class 'character'.")

  if(! name %in% files_on_container)
    stop("Invalid file name.")


  AzureStor::delete_blob(container, name, confirm = T)

  message(paste0("The file '", name, "' has been delete!"))
}
