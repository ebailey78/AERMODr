ControlOptions <- setClass(Class = "ControlOptions", 
         slots = list(TITLEONE = "character",
                      TITLETWO = "character",
                      MODELOPT = "list",
                      AVERTIME = "list",
                      URBANOPT = "list",
                      POLLUTID = "character",
                      HALFLIFE = "numeric",
                      DCAYCOEF = "numeric",
                      FLAGPOLE = "numeric",
                      EVENTFIL = "list",
                      SAVEFILE = "list",
                      INITFILE = "character",
                      MULTYEAR = "list",
                      DEBUGOPT = "list",
                      ERRORFIL = "character",
                      RUNORNOT = "logical"),
         prototype = list(TITLEONE = "AERMODr Run",
                          TITLETWO = "",
                          MODELOPT = list("DFAULT"),
                          AVERTIME = list("24"),
                          URBANOPT = list(),
                          POLLUTID = "OTHER",
                          HALFLIFE = as.numeric(NA),
                          DCAYCOEF = as.numeric(NA),
                          FLAGPOLE = as.numeric(NA),
                          EVENTFIL = list(),
                          SAVEFILE = list(),
                          INITFILE = "",
                          MULTYEAR = list(),
                          DEBUGOPT = list(),
                          ERRORFIL = "",
                          RUNORNOT = TRUE),
         validity = function(object) {
            op <- "\nThe following problems were found with your control input:\n"
            ok <- TRUE
            
            # TITLEONE CHECKS
            if(nchar(object@TITLEONE) < 1) {
              ok <- FALSE
              op <- paste0(op, "\t* TITLEONE is not optional.\n")
            } else if(nchar(object@TITLEONE) > 68) {
              ok <- FALSE
              op <- paste0(op, "\t* TITLEONE cannot be more than 68 characters long.\n")
            }
            
            # TITLETWO CHECKS
            if(nchar(object@TITLETWO) > 68) {
              ok <- FALSE
              op <- paste0(op, "\t* TITLETWO cannot be more than 68 characters long.\n")
            }
                        
            if(!ok) {
              ok <- op
            }
            
            return(ok)
            
        }          
)

setMethod(f="show",
          signature="ControlOptions",
          definition = function(object) {
            op <- "CO STARTING\n"
            op <- paste0(op, "   TITLEONE ", object@TITLEONE, "\n")
            if(nchar(object@TITLETWO) > 0) {
              op <- paste0(op, "   TITLETWO ", object@TITLETWO, "\n")
            }
            op <- paste0(op, "   MODELOPT ", paste(object@MODELOPT, collapse = " "), "\n")
            op <- paste0(op, "   AVERTIME ", paste(object@AVERTIME, collapse = " "), "\n")
            if(length(object@URBANOPT) > 0) {
              op <- paste0(op, "   URBANOPT ", paste(object@URBANOPT, collapse = " "), "\n")
            }
            op <- paste0(op, "   POLLUTID ", object@POLLUTID, "\n")
            if(!is.na(object@HALFLIFE)) {
              op <- paste0(op, "   HALFLIFE ", object@HALFLIFE, "\n")
            }
            if(!is.na(object@DCAYCOEF)) {
              op <- paste0(op, "   DCAYCOEF ", object@DCAYCOEF, "\n")
            }            
            if(!is.na(object@FLAGPOLE)) {
              op <- paste0(op, "   FLAGPOLE ", object@FLAGPOLE, "\n")
            }
            
            
            if(object@RUNORNOT) {
              op <- paste0(op, "   RUNORNOT RUN\n")
            } else {
              op <- paste0(op, "   RUNORNOT NOT\n")
            }
            
            cat(op)
            
          })