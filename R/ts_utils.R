

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

aggregateArticles = function(df, interval = "week", sepFrames = TRUE)
    #
    # Basic function to aggregate the framing data
    # sepFrames is whether the results should be separated by individual frames
    #   or if the data from different frames should be aggregated together
    # 
{
    if(!interval %in% c("day", "week", "month", "year"))
        stop("Interval must be one of: 'day', 'week', 'month', or 'year'")

    ans = by(df, cut(df$date, interval), function(dd, sep) {
        if(sep){
            do.call(rbind,
                    by(dd, dd$top_frame, articleAggFun))
        }else{
            articleAggFun(dd)
        }
    }, sep = sepFrames)

    do.call(rbind, ans)
}

articleAggFun = function(x)
{
    data.frame(startDate = min(x$date),
               endDate = max(x$date),
               frame = paste(unique(x$top_frame), collapse = ";"),
               avgPro = mean(x$Pro[x$tone == "Pro"], na.rm = TRUE),
               nPro = sum(x$tone == "Pro"),
               avgNeut = mean(x$Neutral[x$tone == "Neutral"], na.rm = TRUE),
               nPro = sum(x$tone == "Neutral"),
               avgAnti = mean(x$Anti[x$tone == "Anti"], na.rm = TRUE),
               nAnti = sum(x$tone == "Anti"),
               nSource = length(unique(x$Source)),
               nTotal = nrow(x))
}

