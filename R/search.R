##    This program is free software: you can redistribute it and/or modify
##    it under the terms of the GNU General Public License as published by
##    the Free Software Foundation, either version 3 of the License, or
##    (at your option) any later version.
##
##    This program is distributed in the hope that it will be useful,
##    but WITHOUT ANY WARRANTY; without even the implied warranty of
##    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##    GNU General Public License for more details.
##
##    You should have received a copy of the GNU General Public License
##    along with this program.  If not, see <http://www.gnu.org/licenses/>.

##################################################################
## file history
##################################################################
## 2013-06-24: matteo redaelli: first release
## 2013-11-25: matteo redaelli: switching to mysql
##

##################################################################
## TODO
##################################################################
## 1) managing options since,until, lang
##
##

library(twitteR)

.twLogin <- function(config.twitter) {
    loginfo("connecting to twitter APIs...")
    setup_twitter_oauth(consumer_key    = config.twitter$consumer_key,
                        consumer_secret = config.twitter$consumer_secret,
                        access_token    = config.twitter$access_token,
                        access_secret   = config.twitter$access_secret,
                        credentials_file=NULL)
   }

.getFilenameTweetsIDs <- function(dir) {
    file.path(dir, "tweets-ids.csv")
}

.getFilenameSinceID <- function(dir) {
    file.path(dir, ".sinceID")
}

.getFilenameDump <- function(dir) {
    file.path(dir, "search.Rdata")
}

## ###############################################################
## twSearch
## ###############################################################
twSearch <- function(config.twitter, q, out.dir=".", geocode=NULL, lang=NULL) {

    if( !is.null(geocode) && (is.na(geocode) || geocode=='')) geocode <- NULL
    if( !is.null(lang)    && (is.na(lang)    || lang==''))    lang <- NULL

    file.ids <- .getFilenameTweetsIDs(out.dir)
    file.sinceID <- getFilenameSinceID(out.dir)

    .twLogin(config.twitter)

    sinceID=0

    if(file.exists(file.sinceID)) {
       logwarn(sprintf("Loading sinceID from file %s", file.sinceID))
       sinceID <- read.table(file.sinceID)[1,1]
    }

    logwarn(sprintf("Searching for q=%s, sinceID=%s", q, sinceID))
    t<- searchTwitter(q, n=1500, sinceID=sinceID, geocode=geocode, lang=lang)

    if( length(t) == 0) {
        logwarn(sprintf("No tweets found searching for q=%s, sinceID=%s", q, sinceID))
    } else {
        t.ids <- unlist(lapply(t, function(x) x$id))
        write.table(t.ids, file=file.ids, append=TRUE, row.names=FALSE, col.names=FALSE, quote=FALSE)
        sinceID=tail(t.ids, n=1)
        cat(sprintf("%s\n", sinceID), file=file.sinceID, append=FALSE)
    }
}

## ###############################################################
## twSearchDump
## ###############################################################
twSearchDump <- function(config.twitter, q, out.dir=".", rename.csv=TRUE) {
    
    file.ids  <- .getFilenameTweetsIDs(out.dir)
    file.dump <- .getFilenameDump(out.dir)

    if(not file.exists(file.ids)) {
       logwarn("Missing tweets IDs file: dumping is not possible" )
       return(1)
    }
    tweets.ids <- read.csv(file.ids, header=FALSE)
    .twLogin(config.twitter)
}
