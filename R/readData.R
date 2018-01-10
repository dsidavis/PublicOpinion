
readPollData =
function(file = "ssm_polls.csv", expand = TRUE,
         dataDir = getOption("PublicOpinionDataDir", "../MediaFramingData/public_opinion_analysis/data")             )
{
    if(!file.exists(file))
        f = path.file(dataDir, file)
    else
        f = file
    
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
function(x, vals = x[[var]], var = "Date")
{
  
}

readTopicData =
function(file = "ssm_with_metadata_2017_05_25.csv",
         expand = TRUE,
         dataDir = getOption("PublicOpinionDataDir", "../MediaFramingData/public_opinion_analysis/data")             
             )
{
    if(!file.exists(file))
        f = path.file(dataDir, file)
    else
        f = file

    ans = read.csv(f, stringAsFactors = FALSE)
    if(expand)
        ans = expandDateFrameTone(ans)

    ans
}
