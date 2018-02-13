# Functions to aid plotting the Media Framing dataset

plot_frames = function(df, main = deparse(substitute(df)),
                       outfile = character(),
                       df_polls = NULL, 
                       events = NULL, eventHover = NULL,
                       frame_names = computeFrameNames(),
                       interval = "week",
                       polls = !missing(df_polls), span = 0.1)
{
    df.agg = aggregateArticles(df, interval)
    df.agg$nAnti = df.agg$nAnti * -1
    
    a = ggplot(df.agg,
               aes(x = startDate, y = nPro, color = frame)) +
        geom_line() +
        geom_line(aes(x = startDate, y = nAnti))+ 
        theme_bw() +
        xlab("Date (week start)") +
        xlim(as.Date(c(min(df.agg$startDate), "2017-01-01"))) + 
        scale_y_continuous(sec.axis = sec_axis(~., name = "Public polling"))+
        ggtitle(main) +
        theme_bw()

    a = ggplotly(a, dynamicTicks = TRUE)
    
    if(!is.null(events)){
        a = a %>% add_segments(x = ~events, y = max(df.agg$nPro),
                               xend = ~events, yend = min(df.agg$nAnti),
                               inherit = FALSE, name = "Events",
                               hoverinfo = "text", text = ~ eventHover,
                               line = list(dash = "dot", width = 1,
                                           color = "rgba(67,67,67,1)"))
    }
    if(polls){
        b = ggplot(df_polls, aes(x = Date, y = Index, color = House, size = N)) +
            geom_point() +
            geom_smooth(color = "gray", se = FALSE, span = 0.5) +
            geom_hline(yintercept=50, show.legend = FALSE,
                       linetype = "dashed", color = "black") +
            theme_bw() +
            guides(color = guide_legend(title="Polls"),
                   size = FALSE)
        # browser()
        b = ggplotly(b, dynamicTicks = TRUE)
        c = subplot(a, b, nrows = 2, shareX = TRUE, heights = c(0.8, 0.2))
    } else {
        c = ggplotly(a, dynamicTicks = TRUE)
    }

    if(length(outfile))
       saveWidget(c, file = outfile)

    c
}

plot_sources = function(df, main = deparse(substitute(df)))
{
    # bySource =  aggregate(Pro ~ paste(Month, Year) + Source, data = df, mean)
    # bySource$start_date = as.Date(paste("1", bySource$'paste(Month, Year)'),
                                  # format = "%d %m %Y")
    bySource =  aggregate(Pro ~ Week_start + Source, data = df, mean)
    
    bias = tapply(bySource$Pro, bySource$Source, median)
    bySource$Source = factor(bySource$Source, levels = names(bias)[order(bias)])
    
    c = ggplot(bySource, aes(x = Week_start, y = Pro, color = Source)) +
        geom_point(size = 0.5, alpha = 0.35) +
        geom_smooth(se = FALSE, span = 0.5) +
        theme_bw() +
        geom_hline(yintercept = 0.5, color = "black", linetype = "dashed") + 
        facet_wrap(~Source) +
        ggtitle(paste0("Median position over time: ", main))
    
    ggplotly(c)

}



