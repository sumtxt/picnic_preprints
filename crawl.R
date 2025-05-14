library(httr)
library(jsonlite)

source("fun.R")

now <- Sys.time()
dates <- as.character(seq(as.Date(now) - 7, as.Date(now) - 1, by="days"))

past_ids <- read.csv("./memory/osf_ids.csv")

# Crawl OSF 
out <- list()
for(date in dates){
    cat(date, "\n")
    total_pages <- get_osf_total_pages(call_osf_api(date=date))
    out_tmp <- list()
    for(page in 1:total_pages){
        if(page==0) break
        out_tmp[[page]] <- get_osf_articles(call_osf_api(page=page, date=date))
        }
    out <- c(out, out_tmp)
    }

out <- do.call(rbind, out)
if(is.null(out)) quit(save="no")

# Code ID/Version
out$id_version <- basename(out$url)
out$version <- as.integer(sub(".*_v([0-9]+)$", "\\1", out$id_version))
out$id <- sub("_v[0-9]+$", "", out$id_version)
out$id_version <- NULL 

# If multiple versions, keep the latest
out <- out[ave(out$version, out$id, FUN=function(x) x == max(x)) == TRUE, ]

# Remove past papers
out <- out[!(out$id %in% past_ids$id), ]

# Cleanup data
out$abstract <- strip_whitespace(out$abstract)
out$title <- strip_whitespace(out$title)
out$doi <- extract_doi_id(out$url)

out_subjects <-aggregate(subjects ~ doi , out, function(x) paste0(x, collapse=", "))
colnames(out_subjects)[2] <- "subjects_osf"
out_subjects$subjects_osf[out_subjects$subjects_osf==""] <- NA

# Classification 
out$subjects <- ifelse(out$subjects=="", 
    "(Unspecified)", 
    out$subjects)
out$subjects <- ifelse(out$subjects=="Political Science", 
    "Politics",
    out$subjects)

out$subjects <- ifelse(out$subjects %in% 
    c("Politics", "Economics", "Sociology", "(Unspecified)"), 
    out$subjects, 
    "(Other)")

out$subjects <- factor(out$subjects, levels=c("Politics", "Economics", 
    "Sociology", "(Other)", "(Unspecified)"))

# Collapse 
out <- unique(out)
out <- out[order(out$subjects), ]
out <- aggregate(subjects ~ ., out, function(x) {
    if( sum(x == "(Other)")==1 & length(x)>1 ) x <- x[x!="(Other)"]
    return(list(as.character(x)))
    } )

out <- merge(out, out_subjects, by="doi")

# Flag to hide 
out$hidden <- flag_in_list(out$subjects, c("(Other)", "(Unspecified)"))

politics_flag <- flag_in_list(out$subjects, c("Politics"))
economics_flag <- flag_in_list(out$subjects, c("Economics"))
sociology_flag <- flag_in_list(out$subjects, c("Sociology"))
other_flag <- flag_in_list(out$subjects, c("(Other)"))
unspecified_flag <- flag_in_list(out$subjects, c("(Unspecified)"))

out <- rbind(out[politics_flag, ],
        out[!politics_flag & economics_flag, ], 
        out[!politics_flag & !economics_flag & sociology_flag, ], 
        out[!politics_flag & !economics_flag & !sociology_flag & unspecified_flag, ], 
        out[!politics_flag & !economics_flag & !sociology_flag & !unspecified_flag, ])

# Output JSON
out_json <- render_json(out, date=as.Date(now)) 
write(out_json, paste0("./output/osf.json"))

# Update past urls
write.table(out[,"id"], 
    file=paste0("./memory/osf_ids.csv"), 
    na="", 
    sep=";", 
    append=TRUE, 
    quote=FALSE, 
    col.names=FALSE,
    row.names=FALSE)

# Provider list 
lst <- get_osf_providers(call_osf_api_providers())
lst <- list(
    "main"=subset(lst, name %in% c("Open Science Framework", "SocArXiv")),
    "other"=subset(lst, !(name %in% c("SocArXiv", "Open Science Framework")))
    )
out_json <- toJSON(lst, pretty=TRUE, auto_unbox=TRUE) 
write(out_json, paste0("./output/osf_proviers.json"))

