

getSurges = function(df, threshold)
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
