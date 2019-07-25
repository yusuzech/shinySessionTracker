require(jsonlite)
sessionTracker <- R6Class(
    classname = "sessionTracker",
    public = list(
        # object data ----
        events = data.frame(
            eventName = character(0),
            type = character(0),
            info = character(0),
            timestamp = as.POSIXct(as.POSIXct(character(0)))
        ),
        sessionID = NULL,
        sessionStart = NA,
        sessionStop = NA,
        defaultPath = NA,
        initialize = function(sessionID = NULL,defaultPath = "sessionInfo.json"){
            # generate session ID
            if(!is.null(sessionID)){
                stopifnot(is.character(sessionID))
                self$sessionID <- sessionID
            } else {
                self$sessionID <- private$genSeesionId()
            }
            
            # set default path for sessionTracker file
            stopifnot(is.character(defaultPath))
            self$defaultPath <- defaultPath
            # session start time
            self$sessionStart = Sys.time()
            
            invisible(self)
        },
        # add an event to events dataframe ----
        addEvent = function(
            eventName,
            type = NA,
            info = NA
        ){
            stopifnot(is.character(eventName))
            record <- data.frame(
                eventName = eventName,
                type = type,
                info = info,
                timestamp = as.POSIXct(as.POSIXct(character(0)))
            )
            self$events <- rbind(self$events,record)
            invisible(self)
        },
        # add time stopped for a event
        # save session info ----
        saveSessionInfo = function(
            path = self$defaultPath,
            merge = FALSE 
        ){
            # validate input
            pattern <- "json$|rds$"
            if(!grepl(pattern,path)){
                stop("File extension should only be one of json/rds/csv")
            } else {
                m <- regexpr(pattern, path)
                extension <- regmatches(path, m)
            }
            stopifnot(merge == TRUE | merge == FALSE)
            # retrieve session info
            self$sessionStop <- Sys.time()
            sessionInfo <- self$getSessionInfo()
            
            if(extension == "json"){
                if(file.exists(path)){
                    if(merge){
                        existing_data <- fromJSON(path)
                        sessionInfo <- append(sessionInfo,existing_data)
                        writeLines(paste0("Appending to ", path))
                    } else{
                        writeLines(paste0("Overwriting ", path))
                    }
                } 
                
                
                exportJSON <- jsonlite::toJSON(
                    sessionInfo,
                    dataframe = "rows",
                    auto_unbox = TRUE
                )
                
                write(exportJSON,path,append = FALSE)
            } else if(extension == "rds"){
                if(file.exists(path)){
                    if(merge){
                        existing_data <- readRDS(path)
                        sessionInfo <- append(sessionInfo,existing_data)
                        writeLines(paste0("Appending to ", path))
                    } else{
                        writeLines(paste0("Overwriting ", path))
                    }
                }
                
                saveRDS(object = sessionInfo,file = path)
            } 
            
            
            invisible(self)
        },
        # get session info as a list ----
        getSessionInfo = function(){
            sessionMeta <- list(
                sessionStart = self$sessionStart,
                sessionStop = self$sessionStop,
                sessionDuration = private$getDuration(
                    self$sessionStop,
                    self$sessionStart
                ),
                durationUnit = "second"
            )
            sessionDetail <- self$events
            
            out <- list(
                list(
                    meta = sessionMeta,
                    detail = sessionDetail
                )
            )
            names(out) <- self$sessionID
            return(out)
            
        }
    ),
    # create random session ID ----
    private = list(
        genSeesionId = function(){
            pool <- c(letters,LETTERS)
            key1 <- paste0(sample(pool,4,replace = TRUE),collapse = "")
            key2 <- format(Sys.time(),"%Y%m%d%H%M%S%Z")
            paste0(key1,key2)
        },
        getDuration = function(t1,t2){
            return(
                as.numeric(
                    difftime(
                        time1 = self$sessionStop,
                        time2 = self$sessionStart,
                        units = "secs"
                    )
                )
            )
        }
    ),
    active = list(
        
    ),
    lock_objects = FALSE
)
