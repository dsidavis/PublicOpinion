week_start = function(x){
    as.Date(format(x, "%Y-%W-1"), format = "%Y-%W-%u")
}

mkDate = function(x)
{
    if(!is.null(x$FullDate)){
        date = as.Date(as.character(x$FullDate), format = "%Y%m%d")
    }else{
        date = as.Date(paste(x$Year, x$Month, x$Day, sep = "-"), format = "%Y-%m-%d")
    }
    
    date
}

frameNames = c("Irrelevant", "Capacity.and.Resources", "Crime.and.Punishment", 
                "Cultural.Identity", "Economic", "External.Regulation.and.Reputation", 
                "Fairness.and.Equality", "Health.and.Safety", "Legality..Constitutionality..Jurisdiction", 
                "Morality", "Other", "Policy.Prescription.and.Evaluation", "Political", 
                "Public.Sentiment", "Quality.of.Life", "Security.and.Defense"
                )

expandDateFrameTone =
# All frames have FullDate - it is just pasted %Y%m%d
# Convert date, get the top frame and tone    
function(x, frame_names = frameNames)
{
    x$date = mkDate(x)

    x$Week_start = week_start(x$date)
    # Get top frame
    if(!all(frame_names %in% names(x)))
        stop("Frame names not found in column names")
    
    ps = match(frame_names, colnames(x))

    x$top_frame =  factor(apply(x[,ps], 1, which.max),
                          levels = seq(along= frame_names),
                          labels = gsub("\\.", " ", frame_names))
    
    x$tone = factor(apply(x[,c("Pro","Neutral","Anti")], 1, which.max),
                    levels = 1:3, 
                    labels = c("Pro","Neutral","Anti"))

    x
}



computeFrameNames =
function(file = "frames.xlsx",
         dataDir = getOption("PublicOpinionDataDir", "../MediaFramingData/public_opinion_analysis/data"))
{
    if(!file.exists(file))
        f = file.path(dataDir, file)
    else
        f = file
    
    as.data.frame(read_excel(f, col_names = FALSE))[,3]
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
