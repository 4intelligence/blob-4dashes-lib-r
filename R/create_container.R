#' Utilize essa função para a criação de um novo container no Azure Blob Storage.
#' 
#' É recomendável a criação de dois containers, um para DEV e outro
#' para PROD Porém é opcional.
#' 
#' @param name nome do container que deve ser o nome do dash mais o tipo, separado por hífen. 
#'   Exemplo: `dash-de-dev`
#' @param type tipo do container, se é de `DEV`, `PROD` ou vazio. 
create_container <- function(name, type = 'dev'){
    
    # Dependencies
    require(AzureStor)
    
    # Preparing the variables
    try({
        name <- tolower(name)
        type <- tolower(type)
    }, silent = TRUE)
    
    if(class(name) != 'character')
        stop("Invalid paramter 'name'! It must be a class 'character'.")
    
    if(! type %in% c('dev', 'prod', ''))
        stop("Invalid 'type' specified! 'type' must be a 'dev' or 'prod'.")
    
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