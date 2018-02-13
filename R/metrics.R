#
# How dominant is the most dominant frame
#
# Identify intervals of interest.
#
# surge - x articles over a period.

# frames is a list with a data frame for each issue.

#
# Run Rscripts/setup.R to get data frames with dates expanded and also pro/con in the tone variable, and the top frame in top_frame.
# If we are interested in Pro, use
#  gc = frames[[1]]
#  pro.gc = gc[gc$tone == "Pro", ]
#
# Each row is an article, and we have its frame and date.
# So for a surge
# r = range(pro.gc$date)
# wks = seq(r[1], r[2], by = "week")
#

Delta = c("weeks" = 7, "days" = 1, years = 365)

surgeInfo =
    #
    # Take a single vector of values for pro versus con, in our example, 
    # Break into bins of time (weeks by default)
    #
function(ts, bw = 2, r = range(ts), timeUnit = "weeks", d = cut(ts, timeUnit))
{
    counts = table(d)
    ans = sapply(seq(along = counts), function(i)  sum(counts[ min(i + bw, length(counts)) ]))
    d = as.Date(names(counts))
    delta = bw * Delta[timeUnit]
    data.frame(counts = ans, startDates = d, endDates = d + delta)
}

if(FALSE) {

    s = surgeInfo(pro.gc$date, 4)
    w = s$counts > quantile(s$counts, .9)
    plot(s$startDates[w], s$counts[w])

    # Can do run-length-encoding to determine regions that are
    # above threshold.
}




getChangePoints =
function(ts, changeValue = 0, var = 1)
{
    w = ts[[var]] > changeValue
    r = rle(w)
    g = rep(1:length(r$lengths), r$lengths)
    split(ts, g)
}


if(FALSE) {
    ts = c(rnorm(20, .5, .1), rnorm(10, -.5, .2), rnorm(30, .4, .1), rnorm(10, -.2, .05))
    y = seq(as.Date("2017/1/1"), as.Date("2017/12/31"), 1)
    date = sort(sample(y, length(ts)))
    tt = data.frame(vals = ts, dates = date)
    with(tt, plot(dates, vals, type = "l"))
    abline(h = 0, col = "red", lty = 3)
    cp = getChangePoints(tt)
}
