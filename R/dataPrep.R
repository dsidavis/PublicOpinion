week_start = function(x){
    as.Date(format(x, "%Y-%W-1"), format = "%Y-%W-%u")
}


expandDateFrameTone =
# All frames have FullDate - it is just pasted %Y%m%d
# Convert date, get the top frame and tone    
function(x, frame_names = computeFrameNames())
{
    x$date = as.Date(as.character(x$FullDate), format = "%Y%m%d")
    x$Week_start = week_start(x$date)
    # Get top frame
    ps = grep("^p[0-9]{1,2}$", colnames(x))

    x$top_frame =  factor(apply(x[,ps], 1, which.max),
                          levels = 1:15, labels = frame_names)
    x$tone = factor(apply(x[,c("Pro","Neutral","Anti")], 1, which.max),
                    labels = c("Pro","Neutral","Anti"))

    x
}



computeFrameNames =
function(file = "frames.xlsx",
         dataDir = getOption("PublicOpinionDataDir", "../MediaFramingData/public_opinion_analysis/data"))
{
    if(!file.exists(file))
        f = path.file(dataDir, file)
    else
        f = file
    
    as.data.frame(read_excel(f, col_names = FALSE))$X__3
}


fixSource =
function(df, vals = df[[var]], var = "Source")
{
    mvals = missing(vals)
    ovals = vals
    vals = gsub(" blogs.*$| \\(.*\\)$", "", vals)
    if(!missing(df) & mvals) {
      df[[ paste0(var, ".orig")]] = ovals
      df[[var]] = vals
      df
    } else
      vals
}
