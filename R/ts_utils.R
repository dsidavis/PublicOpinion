

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

aggregateArticles = function(df, interval = "week")
    #
    #
    # 
{
    if(!interval %in% c("day", "week", "month", "year"))
        stop("Interval must be one of: 'day', 'week', 'month', or 'year'")

    ans = by(df, cut(df$date, interval), function(x) {
        data.frame(startDate = min(x$date),
                   endDate = max(x$date),
                   avgPro = mean(x$Pro[x$tone == "Pro"], na.rm = TRUE),
                   nPro = sum(x$tone == "Pro"),
                   avgNeut = mean(x$Neutral[x$tone == "Neutral"], na.rm = TRUE),
                   nPro = sum(x$tone == "Neutral"),
                   avgAnti = mean(x$Anti[x$tone == "Anti"], na.rm = TRUE),
                   nAnti = sum(x$tone == "Anti"),
                   nSource = length(unique(x$Source)),
                   nTotal = nrow(x))
    })
    do.call(rbind, ans)
}


