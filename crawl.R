library(httr)
library(jsonlite)

source("fun.R")

now <- Sys.time()
dates <- as.character(seq(as.Date(now) - 7, as.Date(now) - 1, by="days"))

# Crawl OSF 
out <- list()
for(date in dates){
    cat(date, "\n")
    total_pages <- get_osf_total_pages(call_osf_api(date=date))
    out_tmp <- list()
    for(page in 1:total_pages){
        out_tmp[[page]] <- get_osf_articles(call_osf_api(page=page, date=date))
        }
    out <- c(out, out_tmp)
    }

out <- do.call(rbind, out)

# Remove past papers
if(is.null(out)) quit(save="no")

# Cleanup data
out$abstract <- strip_whitespace(out$abstract)
out$title <- strip_whitespace(out$title)
out$doi <- extract_doi_id(out$url)

# Classification 
out$politics <- as.numeric(grepl("Political Science", out$subjects))
out$economics <- as.numeric(grepl("Economics", out$subjects))
out$sociology <- as.numeric(grepl("Sociology", out$subjects))
out$interdisciplinary <- as.numeric((out$politics + out$sociology + out$economics)>1)
out$notclassified <- as.numeric(out$subjects=="")

# Output JSON
out_json <- render_json(out, date=as.Date(now)) 
write(out_json, paste0("./output/osf.json"))

# Update past urls
write.table(out[,"url"], 
    file=paste0("./memory/osf_urls.csv"), 
    na="", 
    sep=";", 
    append=TRUE, 
    quote=FALSE, 
    col.names=FALSE,
    row.names=FALSE)

