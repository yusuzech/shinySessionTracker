require(jsonlite)
require(aws.s3)
library(R6)
library(shinyjs)
sessionTracker <- R6Class(
    classname = "sessionTracker",
    public = list(
        # object data ----
        events = list(),
        sessionID = NULL,
        sessionStart = NA,
        sessionStop = NA,
        eventListening = c(),
        initialize = function(
            sessionID = NULL,
            add_columns = NULL
        ){
            # generate session ID
            if(!is.null(sessionID)){
                stopifnot(is.character(sessionID))
                self$sessionID <- sessionID
            } else {
                self$sessionID <- private$genSeesionId()
            }
            
            # session start time
            self$sessionStart = Sys.time()
            
            
            # add new columns(add in future)
            
            invisible(self)
        },
        # add an event to events list automatically ----
        # used when track events automatically(track all events)
        addEventAuto = function(
            eventName,
            type = NA,
            info = NA,
            ...
        ){
            stopifnot(is.character(eventName))
            record <- list(
                eventName = eventName,
                collectType = "auto",
                type = type,
                info = info,
                timestamp = Sys.time()
            )
            self$events <- append(self$events,list(record))
            invisible(self)
        },
        # add an event to events list manually ----
        # used when track events manually(track a specified event)
        addEventManual = function(
            eventName,
            type = NA,
            info = NA
        ){
            stopifnot(is.character(eventName))
            record <- list(
                eventName = eventName,
                collectType = "manual",
                type = type,
                info = info,
                timestamp = Sys.time()
            )
            self$events <- append(self$events,list(record))
            invisible(self)
        },
        startListening = function(eventTypes = c("button.click")){
            self$eventListening <- unique(append(self$eventListening,eventTypes))
            if("button.click" %in% self$eventListening){
                js$trackButtonClick(eventObserverID = "eventObserver",
                                    infoLevel = "partial")
            } else if(1 == 1){
                
            }
            invisible(self)
        },
        # add time stopped for a event
        # save session info ----
        saveSessionInfo = function(
            fname = "default", # do not specify path with / or \
            storageType = "local", # options: local, aws.s3
            saveParams = list(folder_path = "")
        ){
            # write sessionStop timestamp
            self$sessionStop <- Sys.time()
            stopifnot(is.character(fname))
            stopifnot(!grepl("\\\\|/",fname))
            # default name sessionID.json
            if(fname == "default"){
                fname <- paste0(self$sessionID,".json")
            }
            # write file to tempfile with correct extension
            tmpfpath <- private$selectiveWrite(x = self$getSessionInfo(),
                                            fname = fname)
            
            if( storageType == "local"){
                # save file to a tempfile
                folder_path <- saveParams[["folder_path"]]
                private$validateFolderPath(folder_path)
                # move file to folder_path
                file.copy(tmpfpath,paste0(folder_path,fname))
            } else if (storageType == "aws.s3"){
                
                object <- saveParams[["object"]]
                bucket <- saveParams[["bucket "]]
                stopifnot(all(is.character(object),is.character(bucket)))
                put_object(
                    file = fname,
                    object = object,
                    bucket = bucket
                )
            } else {
                stop("only support storage type of local, aws.s3")
            }
            invisible(self)
        },
        # get session info as a list ----
        getSessionInfo = function(){
            sessionMeta <- list(
                sessionStart = self$sessionStart,
                sessionStop = Sys.time(),
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
                        time1 = t1,
                        time2 = t2,
                        units = "secs"
                    )
                )
            )
        },
        # write file to a tempfile with provided extension
        selectiveWrite = function(x,fname,supportExtension = c("json","rds")){
            # extract file extension
            extension <- regmatches(fname,
                                    regexpr("(?<=\\.).+$",fname,perl = TRUE))
            stopifnot(extension %in% supportExtension)
            if(extension == "json"){
                tmp <- tempfile(fileext = ".json")
                exportJSON <- jsonlite::toJSON(x = x,auto_unbox = TRUE)
                write(exportJSON,tmp)
            } else if (extension == "rds"){
                tmp <- tempfile(fileext = ".rds")
                saveRDS(x,tmp)
            } else {
                cust_message <- paste0(supportExtension,collapse = ",")
                stop("File extension should be one of ",cust_message)
            }
            # return file name
            return(tmp)
        },
        validateFolderPath = function(folderPath){
            stopifnot(is.character(folderPath))
            # empty sting ""
            condition1 <- folderPath == ""
            # current folder ".", "./", " "
            condition2 <- grepl("^ $|^\\.$|^./$",folderPath,perl = TRUE)
            # child folder with / or \\ at the end
            condition3 <- grepl("^.+/|^.+\\\\",folderPath)
            stopifnot(any(condition1,condition2,condition3))
        }
    ),
    active = list(
        
    ),
    lock_objects = FALSE
)
