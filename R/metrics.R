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


isBlockSequenceInteresting =
function(blocks, fun, ...)
{
    pos = cbind()
    mapply(fun,
            blocks[1:(length(blocks)-1)], blocks[2:length(blocks)], MoreArgs = list(blocks, ...))
}


#############

totalBlockRatio =
    # b1 b2 are data frames coming from the ts_utils computation defining a time series.
    # But the rows in b1 correspond to all of the time units before the change point
    #   (NOT TO THE TIME INTERVAL FOR AGGREGATION)
    # and after the change point in b2.
function(b1, b2, lag = 0, stat = median, var = "val")
{
   stat(b1[[var]])/stat(b2[[var]][1])
}

blockRatio =
    # b1 b2 are data frames coming from the ts_utils computation defining a time series.
    # But the rows in b1 correspond to all of the time units before the change point
    #   (NOT TO THE TIME INTERVAL FOR AGGREGATION)
    # and after the change point in b2.
function(b1, b2, lag = 0, stat = median, ...)
{
   stat(b1[nrow(b1)], ...)/stat(b2$val[1], ...)
}

#########

   # These are more general, flexible versions of the specific ones above.
   # They allows us to take differences and ratios and include points to the left and right
   # of the break/change point in the calculations.

   blockDiff =
    # b1 b2 are data frames coming from the ts_utils computation defining a time series.
    # But the rows in b1 correspond to all of the time units before the change point
    #   (NOT TO THE TIME INTERVAL FOR AGGREGATION)
    # and after the change point in b2.
function(b1, b2, lag = 0, stat = median, op = `-`)
{
    #assumes each of b1 and b2 are ordered by date.
    if(length(lag) == 0)
      return(op(b1$val[nrow(b1)], b2$val[1]))

    lag = rep(lag, length = 2)
    op( stat(b1$val[seq(max(1, (nrow(b1) - lag[1] + 1)), nrow(b1))]),
             stat(b2$val[seq(1, min(nrow(b2), 1 + lag[2]))]))
}

totalBlockDiff =
    # b1 b2 are data frames coming from the ts_utils computation defining a time series.
    # But the rows in b1 correspond to all of the time units before the change point
    #   (NOT TO THE TIME INTERVAL FOR AGGREGATION)
    # and after the change point in b2.
function(b1, b2, lag = 0, stat = median, op = `-`)
{
 op(stat(b1$val), stat(b2$val[1]))
}




if(FALSE) {
    ts = c(rnorm(20, .5, .1), rnorm(10, -.5, .2), rnorm(30, .4, .1), rnorm(10, -.2, .05))
    y = seq(as.Date("2017/1/1"), as.Date("2017/12/31"), 1)
    date = sort(sample(y, length(ts)))
    tt = data.frame(vals = ts, dates = date)
    cp = getChangePoints(tt)
}
