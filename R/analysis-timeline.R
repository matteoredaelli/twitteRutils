AnalyzeTimeline <- function(tweets.df, top=10, stopwords=stopwords("english"), tz="Europe/Rome", output.dir=".", chart.color="red", chart.width=1000, chart.height=1000) {
  
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
                       output.file="wordcloud-hashtags.png")
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