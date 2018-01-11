
findData =
function(file, 
         dataDir = getOption("PublicOpinionDataDir", "../MediaFramingData/public_opinion_analysis/data"), poll = FALSE)      
{
    dataDir = path.expand(dataDir)
    
    if(file.exists(file))
        return(file)

    f = file.path(dataDir, file)
    
    if(file.exists(f))
        return(f)

    
    if(poll)
       f = file.path(dataDir, paste0(file, "_polls.csv"))
    else
        f = grep(sprintf("%s_with_metadata.*\\.csv", f), list.files(dataDir, full = TRUE), value = TRUE)

    if(length(f) == 0 || !file.exists(f))
        stop("cannot find ", file)

    f
}


readPollData =
function(file = "ssm_polls.csv", expand = TRUE,
         dataDir = getOption("PublicOpinionDataDir", "../MediaFramingData/public_opinion_analysis/data")             )
{
    f = findData(file, dataDir, poll = TRUE)    
    
    ans = read.csv(f, stringsAsFactors = FALSE)
    if(expand) {
        if(!("House" %in% names(ans)))
            ans$House = ""

        if("Date" %in% names(ans))
           ans$Date = as.Date(ans$Date, format = "%m/%d/%y")
        else
           warning("No Date column in the polling date in ", f)
    }
    ans
}


findBadYears =
function(x, vals = as.integer(format(x[[var]], "%Y")),
         var = "Date", max.year = as.integer(format(Sys.Date(), "%Y")))
{
    vals > max.year  
}

readTopicData =
function(file = "ssm_with_metadata_2017_05_25.csv",
         expand = TRUE,
         dataDir = getOption("PublicOpinionDataDir", "../MediaFramingData/public_opinion_analysis/data")             
             )
{
    f = findData(file, dataDir)

    ans = read.csv(f, stringsAsFactors = FALSE)
    if(expand)
        ans = expandDateFrameTone(ans)

    ans
}
