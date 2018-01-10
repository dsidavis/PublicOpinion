
findData =
function(file, 
         dataDir = getOption("PublicOpinionDataDir", "../MediaFramingData/public_opinion_analysis/data"), poll = FALSE)      
{
    if(file.exists(file))
        return(f)

    f = path.file(dataDir, file)
    if(file.exists(file))
        return(f)

    
    if(poll)
       f = path.file(paste0(file, "_polls.csv"), dataDir)
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
    findData(file, dataDir)    
    
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
function(x, vals = x[[var]], var = "Date", max.year = as.integer(format(Sys.Date(), "%Y")))
{
    vals > 2017  
}

readTopicData =
function(file = "ssm_with_metadata_2017_05_25.csv",
         expand = TRUE,
         dataDir = getOption("PublicOpinionDataDir", "../MediaFramingData/public_opinion_analysis/data")             
             )
{
    findData(file, dataDir)

    ans = read.csv(f, stringAsFactors = FALSE)
    if(expand)
        ans = expandDateFrameTone(ans)

    ans
}
