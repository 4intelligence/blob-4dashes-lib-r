#' Utilize essa função para a criação de um novo container no Azure Blob Storage.
#' O nome do container será o nome do projeto, utilizando a variavel de ambiente PROJECT_NAME,
#' definida durante a execução da função generate_r_envirion.R
#'
#' É recomendável a criação de dois containers, um para DEV e outro
#' para PROD Porém é opcional.
#'
#'
#' Package necessário: AzureStor
#' @export
create_container <- function(){

    #getting the name of container to be create
    name <- Sys.getenv("PROJECT_NAME")

    # Preparing the variables
    try({
        name <- tolower(name)
    }, silent = TRUE)

    if(class(name) != 'character')
        stop("Invalid paramter 'name'! It must be a class 'character'.")

    # Create container based on 'type' value
    # Endpoint
    endpoint <- AzureStor::storage_endpoint(Sys.getenv('AZURE_URL'), key = Sys.getenv('ACCESS_KEY'))

    # List all containers
    container_list <- names(AzureStor::list_storage_containers(endpoint))

    # Check if container exists
    if(name %in% container_list)
        stop(paste0("The container '", name, "' already exists"))

    # Create container, if not exists
    else
        AzureStor::create_blob_container(endpoint, name)

    # Check if container has created
    container_list <- names(AzureStor::list_storage_containers(endpoint)) # Update container list

    if(name %in% container_list)
        message(paste0("The container '", name, "' has been created!"))   # Done


}
