
#' Baixa um ou vários arquivos no Azure Storage
#' @param y recebe uma string com o nome do arquivo ou NULL para baixar todos os arquivos.
get_y <- function(y = 'all', dest = "forecast_packs/"){

    if( missing(y) || class(y) != 'character' )
        stop('Invalid y parameter. Use "all" for download all forecast packs or type specific name variable.')

    # Passando o parametro para minusculo
    y <- stringr::str_to_lower(y)

    # Nome da variável no azure
    y_name <- y

    if(!stringr::str_detect(y, "^forecast_")) # Verificando se começa com 'forecast_'
        y_name <- base::paste0("forecast_", y_name)
    if(!stringr::str_detect(y, ".rds$"))      # Verificando se termina com '.rds'
        y_name <- base::paste0(y_name, ".rds")

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
        dplyr::select(-isdir, -blobtype, -size) %>%
        dplyr::filter(stringr::str_detect(name, "^forecast_")) %>%
        dplyr::mutate(y = stringr::str_remove_all(name, "^forecast_|.rds$"))

    # Caso o y seja `all`, baixa todos os packs disponíveis
    if( y == 'all' ){

        first <- files_on_container$name[1]

        path_file_first <- paste0(dest, first)

        if( ! file.exists(path_file_first) )
            AzureStor::download_blob(container = container,
                                     dest = path_file_first,
                                     src  = first,
                                     overwrite = FALSE)

        files <- files_on_container %>%
            dplyr::mutate(destino = paste0(dest, name)) %>%
            dplyr::mutate(exists = file.exists(destino)) %>%
            dplyr::filter(exists == FALSE) %>%
            dplyr::filter(name != first)

        if( nrow(files) > 0 )
            AzureStor::multidownload_blob(container,
                                          dest = files$destino,
                                          src  = files$name,
                                          overwrite = FALSE)
        else
            message('All files already downloaded.')

        return()
    }

    # Caso seja diferente, baixa somente o especificado
    else {

        if( ! y %in% files_on_container$y )
            stop('Invalid y value. `', y, '` is not stored.')

        # Faz o download dos arquivos
        if( ! file.exists(paste0(dest, y_name)) )
            AzureStor::download_blob(container = container,
                                     dest = paste0(dest, y_name),
                                     src  = y_name,
                                     overwrite = FALSE)
        else
            message("File already exists.")

        pack_y <- readRDS(paste0(dest, y_name))

        return(pack_y)
    }
}
