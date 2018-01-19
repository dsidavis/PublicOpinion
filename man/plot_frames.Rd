\name{plot_frames}
\alias{plot_frames}
\title{Plot Time Series of Frames}
\description{
}
\usage{
plot_frames(df, main = deparse(subtitute(df)), df_polls = NULL, events = NULL,
            eventHover = NULL, frame_names = computeFrameNames(),
            interval = df$Week_start,
            polls = !missing(df_polls), span = 0.1)
}
\arguments{
  \item{df}{ \code{data.frame} with the article frame data, created by \code{readTopicData}
}
  \item{df_polls}{ \code{data.frame} of the polling data. Optional.
}
  \item{main}{ \code{character} the main title for the plot. Optional.
}
  \item{events}{ a vector of class \code{Date} of events. Optional.
}
  \item{eventHover}{ a \code{character} vector of hover text for events
}
  \item{frame_names}{ vector of labels for the frame names, e.g., p1 = ...
}
  \item{interval}{ a vector of dates to aggregate over. By default
	aggregates by week.
}
  \item{polls}{
}
  \item{span}{
}
}
\details{

}
\value{
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Matt Espe
}

\seealso{
}
\examples{
#
if(!is.null(getOption("PublicOpinionDataDir"))){
gc.articles = readTopicData("guncontrol")
gc.poll = readPollData("guncontrol")
plot_frames(df = gc.articles, main = "Gun Control", df_polls = gc.poll)


data("GunViolenceEvents")
plot_frames(df = gc.articles, df_poll = gc.poll, main = "Gun Control",
            events = GunViolenceEvents$date, eventHover = GunViolenceEvents$location)


# Subset articles - only those from the Washington Post
gc.wapo = gc.articles[grep("washington", gc.articles$Source),]
plot_frames(df = gc.wapo, main = "WaPo articles on gun control")

# Save the plot
# browseURL() to visit the file in a browser.
tmp = plot_frames(df = gc.wapo, outfile = "example_frame_plot.html",
            main = "WaPo articles on gun control")

browseURL("example_frame_plot.html")

# Using your own event data
# assumes you have loaded data for this purpose named
# example_data
# e.g., example_data =
# read.csv("_official_same-sex_timeline_through_2017_shayla.csv",
#           stringsAsFactors = FALSE)

if(exists("example_data")){
# Required that the events are dates
example_data$date = as.Date(paste0(example_data$Year, "-01-01"))
ss.articles = readTopicData("ssm")

plot_frames(df = ss.articles, main = "Same Sex Marriage",
            events = example_data$date, eventHover = example_data$Description)
}
} else {
stop("Please set the 'PublicOpinionDataDir' option before running example")
}
\keyword{}
\keyword{}
