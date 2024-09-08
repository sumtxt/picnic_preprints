# Main 
########

# Level 1: Social and Behavioral Sciences
# https://digitalcommons.bepress.com/cgi/viewcontent.cgi?referer=&httpsredir=1&article=1008&context=reference

call_osf_api <- function(date, page=1){
    endpoint <- "https://api.osf.io/v2/preprints/"
    param <- list(
#        "filter[provider]"="socarxiv",
        "filter[subjects]"="5a8c80f7c698300375c76d84", 
        "filter[date_created]"=date,
        "fields[preprints]"="title,description,date_created,contributors,subjects",
        "embed"="contributors",
        "fields[users]"="full_name", 
        "format"="jsonapi", 
        "page"=page 
    )
    res = GET(endpoint,query=param)
    return(content(res, type="application/json"))
    }


# Filter 
#########


# Helpers 
##########

render_json <- function(df,date){
    df_hidden <- subset(df, hidden==TRUE, select=-hidden)
    df <- subset(df, hidden==FALSE, select=-hidden)
    df <- list(
            "articles"=df, 
            "articles_hidden"=df_hidden)
    to_json <- list("update"=date, "content"=df)
    json <- toJSON(to_json, pretty=TRUE, auto_unbox=TRUE) 
    return(json)
    }

extract_doi_id <- function(url){
    return(gsub("http(|s)://doi.org/", "", url))
    }

strip_html <- function(str) {
   if(is.null(str)) return(NA)
   else {
    str <- gsub("<.*?>", " ", str)
    str <- gsub("\\s+", " ", str)
    str <- trimws(str)
    return(str)
   }
}

strip_whitespace <- function(str) {
   if(is.null(str)) return(NA)
   else {
    str <- gsub("\\s+", " ", str)
    return(trimws(str))
   }
}

file_empty <- function(file){
    length(readLines(file))==0
    }

read.csv2_check <- function(file, ...){
    if(!file_empty(file)){ 
        return(read.csv2(file, ...))
    } else { 
        return(NULL)
    }
}

# Crossref 

call_osf_api_next <- function(url){
    res = GET(url)
    return(res)
}

get_osf_articles <- function(data){
    ll <- lapply(data$data, get_osf_article_info)
    ll <- do.call(rbind, ll)
    return(ll)
}

get_osf_article_info <- function(item){

    return(data.frame(
        title=get_osf_title(item),
        authors=get_osf_authors(item),
        created=get_osf_date(item, "date_created"),
        abstract=get_osf_abstract(item), 
        url = get_osf_url(item), 
        subjects = get_osf_subjects2(item)
    ))
}

get_osf_title <- function(item){
    if(is.null(item$attributes$title)) return(NA)
    else return(item$attributes$title)
}

get_osf_abstract <- function(item){
    if(is.null(item$attributes$description)) return(NA)
    else return(item$attributes$description)
}   

get_osf_url <- function(item){
    if(is.null(item$links$preprint_doi)) return(NA)
    else return(item$links$preprint_doi)
}

get_osf_date <- function(item, name){
    if(is.null(item$attributes[[name]])) return(NA)
    else return(as.Date(as.POSIXct(item$attributes[[name]])))
}

get_osf_authors <- function(item){
    return(paste0(lapply(item$embeds$contributors$data, get_osf_author), collapse=", "))
}

get_osf_tags <- function(item){
    if(is.null(item$attributes$tags)) return(NA)
    else return(paste0(item$attributes$tags, collapse=", "))
}   

get_osf_author <- function(item){
    if(is.null(item$embeds$users$data$attributes$full_name)) return(NA)
    return(item$embeds$users$data$attributes$full_name)
}

get_osf_next_url <- function(data){
    if(is.null(data$links$`next`)) return(NA)
    else return(data$links$`next`)
    }

get_osf_total_pages <- function(data){
    total <- data$links$meta$total
    per_page <- data$links$meta$per_page
    return(ceiling(total/per_page))
}

get_osf_subjects2 <- function(item, name){
    out <- lapply(item$attributes$subjects, get_osf_subject2)
    out <- unique(unlist(out))
    if(is.null(out)) return("")
    else return(out)
}

get_osf_subject2 <- function(item){
    if(length(item)<2) return(NULL)
    if(is.null(item[[2]]$text)) return(NULL)
    else return(item[[2]]$text)
}




# Open AI 
call_openai_api <- function(system_prompt, user_prompt, model){
    endpoint <- "https://api.openai.com/v1/chat/completions"
    body <- list(
        model = model,
        messages = list(
            list(role="system", content=system_prompt),
            list(role="user", content=user_prompt)
            )
        )
    body <- toJSON(body, auto_unbox=TRUE)
    res <- POST(endpoint, 
        body=body, 
        encode='raw', 
        content_type_json(), 
        add_headers(Authorization = paste("Bearer", openai_apikey, sep = " ")))
    
    return(content(res))
    }

get_openai_response <- function(response){
    return(response$choices[[1]]$message$content)
}

get_openai_finish_reason <- function(response){
    return(response$choices[[1]]$finish_reason)
}

get_openai_usage <- function(response){
    return(unlist(response$usage$total_tokens))
}


# OSF Providers 

call_osf_api_providers <- function(){
    endpoint <- "https://api.osf.io/v2/providers/preprints/"    
    res = GET(endpoint)
    return(content(res, type="application/json"))
    }

get_osf_providers <- function(data){
    ll <- lapply(data$data, get_osf_provider_info)
    ll <- do.call(rbind, ll)
    return(ll)
}

get_osf_provider_info <- function(item){
    return(data.frame(
        name=get_osf_provider_name(item),
        link=get_osf_provider_link(item)
    ))
}

get_osf_provider_name <- function(item){
    return(item$attribute$name)
    }

get_osf_provider_type <- function(item){
    return(item$type)
    }

get_osf_provider_link <- function(item){
    return(item$links$iri)
    }

 