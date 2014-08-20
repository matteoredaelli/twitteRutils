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

library(lattice)
library(stringr)
library(ggplot2)
library(reshape2)
library(igraph)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(cluster)
library(FactoMineR)

AnalyzeTweets <- function(tweets.df, top=10, stopwords=stopwords("english"), tz="Europe/Rome", output.dir=".", chart.color="red", chart.width=1000, chart.height=1000) {
  
  df <- twNormalizeDate(tweets.df, tz)
  
  #twHistTweets(df, breaks="30 mins", width=chart.width, height=chart.height, color=chart.color)
  
  twChartAgents(df, width=chart.width, height=chart.height, color=chart.color, top=top,
                    output.dir=output.dir)
  twChartAuthors(df, width=chart.width, height=chart.height, color=chart.color, top=top,
                     output.dir=output.dir)
  twChartAuthorsWithRetweets(df, width=chart.width, height=chart.height, color=chart.color, top=top,
                                 output.dir=output.dir)
  twChartAuthorsWithReplies(df, width=chart.width, height=chart.height, color=chart.color, top=top,
                                output.dir=output.dir)
  twChartInfluencers(df, width=chart.width, height=chart.height,
                         color=chart.color, top=top,
                         from=1, 
                         output.dir=output.dir, 
                         output.file="influencers.png")
  twChartInfluencers(df, width=chart.width, height=chart.height,
                         color=chart.color, top=top,
                         from=2, 
                         output.dir=output.dir, 
                         output.file="influencers-excluding-topscores-1.png")
  text = df$text
  text <- twCleanText(text)
  tdm.matrix <- twBuildTDMMatrix(text, stopwords=stopwords)
  
  twChartWordcloud(table=twTopWords(text, top=20),
                       width=chart.width, height=chart.height, 
                       output.dir=output.dir,
                       output.file="wordcloud-words.png")
  twChartWordcloud(table=twTopHashtags(df$text, top=20),
                       width=chart.width, height=chart.height, 
                       output.dir=output.dir,
                       output.file="wordcloud-hashtags.png")
  twChartGivenTopics(tdm.matrix=tdm.matrix, width=chart.width, height=chart.height,
                         output.dir=output.dir)
  twChartWhoRetweetsWhom(df, width=chart.width, height=chart.height,
                             output.dir=output.dir)
  twChartDendrogram(tdm.matrix=tdm.matrix, width=chart.width, height=chart.height,
                        output.dir=output.dir)
}
