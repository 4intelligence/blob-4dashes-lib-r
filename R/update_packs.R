#' @title Realiza o upload de um forecast pack ou todos os forecast packs no AzureStor
#'
#' @name update_packs
#'
#' @author 4intelligence
#'
#' @details Realiza o upload do forecast pack de cada variavel y no banco AzureStor.
#'
#' @param pack nome do arquivo a se fazer o upload.
#'
#'  `NULL` faz o upload de todos os arquivos do caminho passado em `path`.
#' @param path pasta ou caminho do arquivo utilizado no parâmetro `pack`.
#'
#' @examples
#' \dontrun{
#' # Subindo todos os arquivos da pasta pack para o azuere
#'  update_packs()
#'
#'  #Subindo um arquivo da pasta pack para o azuere
#'  update_packs('com_co')
#' }
#' @export
update_packs <- function(pack = NULL, path = 'packs/'){

    if(!is.character(path))
        stop('Invalid path!')

    if(!is.null(pack))
    {
        if(!is.character(pack))
            stop('Invalid blob name!')

        if( stringr::str_ends(pack, '.rds') )
            formatted_name <- stringr::str_remove(pack, ".rds")
        else
            formatted_name <- pack
    }

    # Verifica se contém a barra da pasta
    if(! stringr::str_ends(path, '/') )
        path <- paste0(path, '/')

    # endpoint
    endp <- AzureStor::storage_endpoint(Sys.getenv('AZURE_URL'), key = Sys.getenv('ACCESS_KEY'))

    # Container
    container <- AzureStor::list_storage_containers(endp)

    if(!Sys.getenv('PROJECT_NAME') %in% names(container))
        stop('Project not found. Try specify an type (dev) or (prod).')

    container <- container[[Sys.getenv('PROJECT_NAME')]]

    # Faz o upload de todos os forecast_packs
    if(is.null(pack))
    {
        files.sources <- list.files(path, full.names = TRUE)  # path
        files.names   <- list.files(path, full.names = FALSE) # name

        AzureStor::multiupload_blob(container = container,
                                    src = files.sources,
                                    dest = files.names)

        message('Uploaded all files in `', path, '` folder.') %>%
            return()
    }
    else
    {
        AzureStor::upload_blob(container = container,
                               # Path to file
                               src  = paste0(path, pack),
                               # Name on azure
                               dest = formatted_name)

        message('Uploaded ', formatted_name) %>%
            return()
    }
}
