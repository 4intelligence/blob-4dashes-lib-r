#' Função utilizada para a listagem de todos os arquivos contidos no
#' Blob Storage do projeto.
#' 
#' @return dataframe com o nome, tamanho e variável (y) dos arquivos
files_on_container <- function(){
    
    require(AzureStor)
    require(stringr)
    require(dplyr)
    require(tibble)
    
    # Endpoint
    endp <- AzureStor::storage_endpoint(Sys.getenv('AZURE_URL'), key = Sys.getenv('ACCESS_KEY'))
    
    # Container
    container <- list_storage_containers(endp)
    
    if(!PROJECT_NAME %in% names(container))
        stop('Project not found. Try specify an type (dev) or (prod).')
    
    container <- container[[PROJECT_NAME]]
    
    # Arquivos no container
    files_on_container <- AzureStor::list_storage_files(container)
    
    # Dataframe com os arquivos no container ----
    # Transformando o tamanho em MB
    file_size <- files_on_container %>% 
        dplyr::select(size) %>% 
        dplyr::pull() %>% 
        {( . / 1024 ) / 1024} %>% 
        round(2) 
    
    files_on_container$size <- file_size
    
    files_on_container <- files_on_container %>% 
        dplyr::rename("size_mb" = size) %>% 
        dplyr::select(-isdir) %>% 
        dplyr::mutate(y = stringr::str_remove(name, "pack_")) %>% 
        dplyr::mutate(is_pack = ! stringr::str_detect(y, '.rds')) %>% 
        dplyr::mutate(y = ifelse(is_pack, y, NA)) %>% 
        dplyr::select(-is_pack, -blobtype) %>% 
        tibble::as_tibble()
    
    return(files_on_container)
    
}
