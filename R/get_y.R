
#' Baixa um ou vários arquivos no Azure Storage
#' @param y recebe uma string com o nome do arquivo ou NULL para baixar todos os arquivos.
get_y <- function(y = NULL, .first = "com_co", dest = "packs/"){
    
    require(AzureStor)
    require(stringr)
    require(dplyr)
    
    if( is.null(y) || class(y) != 'character' )
        stop('Invalid y parameter. Use "all" for download all variables or type specific name variable.')
    
    # Passando o parametro para minusculo
    y <- stringr::str_to_lower(y)
    # Nome da variável no azure
    y_name <- paste0("pack_", y)
    
    if(str_detect(y, ".rds"))
        y_name <- y
    
    # Endpoint
    endp <- AzureStor::storage_endpoint(Sys.getenv('AZURE_URL'), key = Sys.getenv('ACCESS_KEY'))
    
    # Container
    container <- list_storage_containers(endp)
    
    if(!PROJECT_NAME %in% names(container))
        stop('Project not found. Try specify an type (dev) or (prod).')
    
    container <- container[[PROJECT_NAME]]
    
    # Arquivos no container
    files_on_container <- AzureStor::list_storage_files(container)
    
    files_on_container <- files_on_container %>% 
        dplyr::select(-isdir, -blobtype) %>% 
        dplyr::mutate(y = stringr::str_remove(name, "pack_"))
    
    # Caso o y seja `all`, baixa todos os packs disponíveis
    if( y == 'all' ){
        
        # Baixa primeiro o y especificado no parâmetro `.first`
        path_file_first <- paste0(dest, .first)
        
        if( ! file.exists(path_file_first) )
            AzureStor::download_blob(container = container, 
                                     dest = path_file_first, 
                                     src  = .first,
                                     overwrite = FALSE)
        
        files <- files_on_container %>% 
            dplyr::mutate(destino = paste0(dest, name)) %>% 
            dplyr::mutate(exists = file.exists(destino)) %>% 
            dplyr::filter(exists == FALSE) %>% 
            dplyr::filter(!stringr::str_detect(y, ".rds"))
        
        if( nrow(files) > 0 )
            AzureStor::multidownload_blob(container,
                                          dest = files$destino,
                                          src  = files$name,
                                          overwrite = TRUE)
        else
            warning('Todos os arquivos já estão baixados.')
        
        return()
    }
    
    # Caso seja diferente, baixa somente o especificado
    else {
        
        if( ! y %in% files_on_container$y )
            stop('Invalid y value.')
        
        # Faz o download dos arquivos
        if( ! file.exists(paste0(dest, y_name)) )
            AzureStor::download_blob(container = container, 
                                     dest = paste0(dest, y_name), 
                                     src  = y_name,
                                     overwrite = FALSE)
        else
        {
            warning("File already exists.")
        }
        
        pack_y <- readRDS(paste0(dest, y_name))
        
        return(pack_y)
    }
}
