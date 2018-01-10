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
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
gc.articles = readTopicData("guncontrol")
gc.poll = readPollData("guncontrol")
plot_frames(gc.articles, gc.poll, "Gun Control")


data("GunViolenceEvents")
plot_frames(gc.articles, gc.poll, "Gun Control", events = GunViolenceEvents)
}
\keyword{}
\keyword{}
