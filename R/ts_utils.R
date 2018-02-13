

getSurges = function(df, threshold)
    # Not used - maybe move to metrics.R
{
    by(df, list(df$top_frame, df$tone), function(x, threshold){
        x = x[order(x$interval),]
        d = abs(x$Count) >= threshold
        # browser()
        x[d,]
        }, threshold = threshold)
}


byInterval = function(df, interval)
    # Interval is just a vector of the time you want to collapse by
{
    byX = aggregate(Pro ~ interval + tone + top_frame, data = df, length)
    byX = byX[order(byX$interval),]
    
    colnames(byX)[4] = "Count"
    
    byX$Count[byX$tone == "Anti"] = byX$Count[byX$tone == "Anti"] * -1
    byX
}

aggregateArticles = function(df, interval = "week", sepFrames = TRUE, fun = articleAggFun)
    #
    # Basic function to aggregate the framing data
    # sepFrames is whether the results should be separated by individual frames
    #   or if the data from different frames should be aggregated together
    # 
{
    if(!interval %in% c("day", "week", "month", "year"))
        stop("Interval must be one of: 'day', 'week', 'month', or 'year'")

    ans = by(df, cut(df$date, interval), function(dd, sep) {
        if(sep) {
            do.call(rbind,
                    by(dd, dd$top_frame, fun))
        } else {
            fun(dd)
        }
    }, sep = sepFrames)

    do.call(rbind, ans)
}

articleAggFun = function(x)
{
    data.frame(startDate = min(x$date),
               endDate = max(x$date),
               nTotal = nrow(x),               
               frame = paste(unique(x$top_frame), collapse = ";"),
               avgPro = mean(x$Pro[x$tone == "Pro"], na.rm = TRUE),
               nPro = sum(x$tone == "Pro"),
               avgNeut = mean(x$Neutral[x$tone == "Neutral"], na.rm = TRUE),
               nNeut = sum(x$tone == "Neutral"),
               avgAnti = mean(x$Anti[x$tone == "Anti"], na.rm = TRUE),
               nAnti = sum(x$tone == "Anti"),
               nSource = length(unique(x$Source))
               )
}


proConRatio =
function(x, stat = mean, ...)
{
    data.frame(startDate = min(x$date),
               endDate = max(x$date),
               nTotal = nrow(x),
               val = stat(x$Pro/x$Anti, ...)
               )
}

proConDiff =
function(x, stat = mean, ...)
{
    data.frame(startDate = min(x$date),
               endDate = max(x$date),
               nTotal = nrow(x),
               val = stat(x$Pro - x$Anti, ...))

}

topFrameDominance =
    # If both then two columns separately
    # otherwise combine across pro and con and neutral.    
function(x, bothProCon = TRUE, neutral = TRUE)
{
    
    ans = data.frame(startDate = min(x$date),
        endDate = max(x$date))

    if(bothProCon) {
        ans[c("Anti", "Pro", "Neutral")] = by(x$top_frame, x$tone, computeMaxFrameToNext) # Offer to remove the neutral
    } else
        ans$val = computeMaxFrameToNext( x$top_frame)
}

computeMaxFrameToNext =
function(frame)
{
    tt = sort(table(frame), decreasing = TRUE)
    tt[1] - tt[2]
}

topFrameDominanceEntropy =
    function(x)
{
    p = table(x$top_frame)/nrow(x)

    data.frame(startDate = min(x$date),
               endDate = max(x$date),
               nTotal = nrow(x),
               entropy =  sum(-p * log(p))
               )
}

getChangePointStart = function(cp)
{
    # Hack to avoid converting to numeric and then back to date
    do.call(c, lapply(cp, function(x) min(x$startDate)))
}
