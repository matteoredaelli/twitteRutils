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
library(RSQLite)
library(logging)

.twLogin <- function(twitter.creds) {
    ##loginfo("connecting to twitter APIs...")
    ##setup_twitter_oauth(consumer_key    = config.twitter$consumer_key,
    ##                    consumer_secret = config.twitter$consumer_secret,
    ##                    access_token    = config.twitter$access_token,
    ##                    access_secret   = config.twitter$access_secret,
    ##                    credentials_file=NULL)
    load_twitter_oauth(twitter.creds)
   }

.getFilenameSearchDB <- function(dir) {
    file.path(dir, "search.db")
}

.getFilenameSinceID <- function(dir) {
  file.path(dir, "sinceID.RData")
}
.getFilenameSearchDump <- function(dir) {
    file.path(dir, "search.RData")
}

.loadSinceID <- function(dir) {
  file.sinceID <- .getFilenameSinceID(dir)
  if(file.exists(file.sinceID)) {
    load(file.sinceID)
  } else {
    sinceID <- 0
  }
  return(sinceID)
}

.saveSinceID <- function(sinceID, dir) {
  file.sinceID <- .getFilenameSinceID(dir)
  save(sinceID, file=file.sinceID)
}

.twDbConnect <- function(dir) {
    file.db <- .getFilenameSearchDB(dir)
    conn <- dbConnect(SQLite(), dbname = file.db)
    if(! dbExistsTable(conn, "tweets")) {
        ## create table tweets

        sql <- "CREATE TABLE tweets (
  text text,
  favorited BOOLEAN DEFAULT NULL,
  favoriteCount float DEFAULT NULL,
  replyToSN varchar(50),
  created datetime,
  truncated tinyint(4) DEFAULT NULL,
  replyToSID varchar(30) DEFAULT NULL,
  id varchar(30) not NULL,
  replyToUID varchar(30) DEFAULT NULL,
  statusSource varchar(300),
  screenName varchar(50),
  retweetCount float DEFAULT NULL,
  isRetweet BOOLEAN DEFAULT NULL,
  retweeted BOOLEAN DEFAULT NULL,
  longitude float,
  latitude float,
  PRIMARY KEY (id)
)"
        rs <- dbSendQuery(conn, sql)
        dbClearResult(rs)
        
    }
    return(conn)
}

.twDbDisconnect <- function(conn) {
    dbDisconnect(conn)
}
    
## ###############################################################
## twIncrementalSearch
## ###############################################################
twIncrementalSearch <- function(twitter.creds, q, out.dir=".", geocode=NULL, lang=NULL) {

    if( !is.null(geocode) && (is.na(geocode) || geocode=='')) geocode <- NULL
    if( !is.null(lang)    && (is.na(lang)    || lang==''))    lang <- NULL

    .twLogin(twitter.creds)  
    sinceID <- .loadSinceID(out.dir)
    logwarn(sprintf("Searching for q=%s, sinceID=%s", q, sinceID))
    tweets <- searchTwitter(q, n=1500, sinceID=sinceID, geocode=geocode, lang=lang)
    len <- length(tweets)
    logwarn(sprintf("Found %d tweets searching for q=%s, sinceID=%s", 
                    len, 
                    q, 
                    as.character(sinceID)))
    if( len == 0) {
        logwarn("No tweets no save to DB")
    } else {
        ## db setup
        logwarn("..Connecting to database..")
        conn <- .twDbConnect(out.dir)
        tweets.df <- twListToDF(tweets)
        tweets.df$text <- twUTF8FixText(tweets.df$text)
        logwarn("..Saving tweets to DB table tweets..")
        dbWriteTable(conn, "tweets", tweets.df, row.names=FALSE, append=TRUE)
        .twDbDisconnect(conn)
        logwarn("..Updating sinceID file")
        sinceID=max(tweets.df$id)
        .saveSinceID(sinceID, out.dir)
    }

}

