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
  \item{df}{
}
  \item{df_polls}{
}
  \item{main}{
}
  \item{events}{
}
  \item{eventHover}{
}
  \item{frame_names}{
}
  \item{interval}{
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
# Add to examples writing to a file.
# browseURL() to visit the file in a browser.

gc.articles = readTopicData("guncontrol")
gc.poll = readPollData("guncontrol")
plot_frames(gc.articles, "Gun Control", gc.poll)


data("GunViolenceEvents")
plot_frames(gc.articles, gc.poll, "Gun Control", events = GunViolenceEvents)


# Subset events - only those in California
#  or those between 2010 & 2017
}
\keyword{}
\keyword{}
