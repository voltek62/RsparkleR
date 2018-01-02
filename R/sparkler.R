sparkler_tag <- "0.2.0"

#' Create an Instance with Sparkler
#'
#' @param client The name of the resource, provided by the client when initially creating the resource
#' @param regionVM The name of datacenter: SBG3, BHS3, WAW1, UK1, DE1, GRA3
#' @param typeVM The range of your cloud server ( s1-2, s1-4: https://www.ovh.co.uk/public-cloud/instances/prices/)
#' @param sshPubKeyPath The path of public SSH key
#' @param sshPrivKeyPath The path of private SSH key
#'
#' @details
#'
#' Make sure the instance is big enough to handle what you need,
#'   for instance the default \code{s1-4} will hang the instance when trying to install large R libraries.
#'
#' @section Creation logic:
#'
#' You need these parameters defined to call the right function for creation.  Check the function definitions for more details.
#'
#' If the Instance name exists but is not running, it starts the Instance and return the Instance object
#' If the Instance doesn't exist, it creates the Instance object by using the createDocker function.
#' If the Instance is running, it will return the Instance object
#'
#'
#' @return A \code{instance} object
#'
#' @examples
#'
#' \dontrun{
#'
#' library(RsparkleR)
#'
#' ovh <- import_ovh()
#' client <- load_client(ovh, endpoint, application_key, application_secret, consumer_key)
#'
#' sshPubKeyPath  <- 'C:/Users/vterrasi/.ssh/id_rsa.pub'
#' sshPrivKeyPath <- 'C:/Users/vterrasi/.ssh/id_rsa'
#'
#' vm <- sparkler.create(client, regionVM="UK1", typeVM="s1-4", sshPubKeyPath, sshPrivKeyPath)
#'
#' }
#'
#' @export
sparkler.create <- function(client, regionVM, typeVM, sshPubKeyPath, sshPrivKeyPath) {

  projects <- get_projects(client)

  # check if sparkler project exists
  active_project <- FALSE
  active_project_id <- ""

  if (length(projects)>0) {
    for (i in 1:length(projects)) {
      project <- get_project_info(client, projects[i])
      description <- project$description
      status <- project$status
      project_id <- project$project_id

      if (status=="ok" && description=="Sparkler") {
        active_project <- TRUE
        active_project_id <-project_id
        break
      }
    }
  }

  # if doesn't exist , create and get ID
  if (active_project==FALSE) {
    myMessage('Sparkler create :: ', level = 3)
    active_project_id <- create_project(client,"Sparkler")$project

  }
  else {
    myMessage('Sparkler exists :: ',active_project_id,level = 3)
  }

  # check id active VM exits
  instances <- get_intances(client,active_project_id)

  active_instance <- FALSE
  active_instance_ip <- ""
  active_instance_id <- ""

  if (length(instances)>0) {
    for (i in 1:length(instances)) {

      instance <- instances[i][[1]]
      name <- instance$name
      status <- instance$status
      id <- instance$id
      ip <- instance$ipAddresses[[1]]$ip

      if (status=="ACTIVE" && name=="Sparkler1") {
        active_instance <- TRUE
        active_instance_ip <- ip
        active_instance_id <- id
        break
      }
    }
  }

  # FIX : wait : credit ??
  # activate the project by buyig Public Cloud Credits
  if (active_instance==FALSE && active_project_id=="") {
    myMessage('Please you need to activate the project by buyig Public Cloud Credits', level = 3)
    myMessage(paste('https://www.ovh.com/manager/cloud/index.html#/iaas/pci/project/',active_project_id,sep=''), level = 3)
    myMessage(paste('Find some instructions here : https://docs.ovh.com/gb/en/public-cloud/getting_started_with_public_cloud_logging_in_and_creating_a_project/',sep=''), level = 3)
    myMessage(paste('Merci de consulter l\'aide : https://docs.ovh.com/fr/public-cloud/debuter-avec-public-cloud-premiere-connexion/',sep=''), level = 3)
  }


  # if doesn't exist , create and get ID
  if (active_instance==FALSE) {
    myMessage('Docker VM Sparkler1 creating :: ', level = 3)

    keys <- get_keys(client, active_project_id)

    # control SSH keys
    sshKeyId <- ""

    # read current public ssh key
    con <- file(sshPubKeyPath,open="r")
    myKey <-  readLines(con)
    close(con)

    if (length(keys)>0) {
      for (i in 1:length(keys)) {

        key <- keys[i][[1]]

        sshPublicKey <- key$publicKey

        if (sshPublicKey==myKey) {
          sshKeyId <- key$id
          break
        }
      }
    }

    if (sshKeyId=="") {
      myMessage('No SSH key found :: SSH Key uploading', level = 3)
      ssh_new <- create_key(client, active_project_id,"test key", sshPubKeyPath)
      sshKeyId <- ssh_new$id
    }

    docker <- create_docker(client, active_project_id,"Sparkler1", regionVM, typeVM, sshKeyId)
    #TODO : get IP
    dockerId <- docker$id
    #while no IP : wait
    myMessage('Docker VM checking Sparkler 1 IP :: ',dockerId, level = 3)
    instance <- get_intance(client,active_project_id,dockerId)

    while(length(instance$ipAddresses)==0) {
      Sys.sleep(5)
      myMessage('Docker VM Sparkler1 creating Wait 5s :: ', level = 3)
      instance <- get_intance(client,active_project_id,dockerId)
    }

    active_instance_ip <- instance$ipAddresses[[1]]$ip
    active_instance_id <- instance$id

    active_instance <- TRUE
  }
  else {
    myMessage('Sparkler VM exists getIP :: ',active_instance_ip,level = 3)
  }


  while(is_port_open(active_instance_ip)==FALSE) {
    Sys.sleep(5)
    myMessage('Docker VM Sparkler1 port 22 is closed Wait 5s :: ', level = 3)
  }

  myMessage('Docker VM Sparkler1 port 22 is open ! let\'s rock :: ', level = 3)

  instance <- Instance(name = "Docker"
                       ,machineType = typeVM
                       ,region = regionVM
                       ,description = ""
                       ,projectId = active_project_id
                       ,instanceId = active_instance_id
                       ,ip = active_instance_ip)

  instance$ssh <- list(
    username = "debian",
    key.pub = sshPubKeyPath,
    key.private = sshPrivKeyPath
  )

  instance
}



#' Start a docker with Sparkler
#'
#'
#' @param vm The Instance object
#' @param debug If TRUE, will see debug messages.
#'
#' @details
#' Check if Docker exists and running
#'    If not, we create the docker with Sparkler
#'    If exists, we restart it
#'
#' @examples
#'
#' \dontrun{
#'
#' library(RsparkleR)
#' ovh <- import_ovh()
#' client <- load_client(ovh,endpoint,application_key,application_secret,consumer_key)
#'
#' sshPubKeyPath  <- 'C:/Users/vterrasi/.ssh/id_rsa.pub'
#' sshPrivKeyPath <- 'C:/Users/vterrasi/.ssh/id_rsa'
#'
#' vm <- sparkler.create(client, regionVM="UK1", typeVM="s1-4", sshPubKeyPath, sshPrivKeyPath)
#'
#' sparkler.start(vm, debug=FALSE)
#'
#' }
#'
#' @export
sparkler.start <-function(vm, debug=FALSE) {

  # FIX : no return
  # we need to launch a process to keep the instance alive
  myMessage("******************************************************************************************", level = 3)
  myMessage("********* installing sparkler, be patient for first time ( 5 to 10 min ) *****************", level = 3)
  myMessage("******************************************************************************************", level = 3)

  dockername <- "sparkler_instance"

  dockeractive <- tryCatch({

    test <- docker_inspect(vm, dockername)

    if (!grepl("sparkler_instance",test[[1]]$Name) || !grepl("running",test[[1]]$State$Status)) {
      myMessage("Docker false", level = 3)
     FALSE
    }

    myMessage("Docker true", level = 3)
    TRUE

  }, error = function(ex) {

    #myMessage("Docker error", level = 3)
    FALSE

  })

  # TODO : multiple docker
  if (dockeractive==FALSE) {

    myMessage("Docker sparkler_instance doesn't exist - Docker run", level = 3)

    docker_cmd(vm,
               ,"run"
               ,args = c("-t","-d","-p","8983:8983","-p","4041:4040","--user","sparkler","--name",dockername,paste0("uscdatascience/sparkler:",sparkler_tag),"/bin/bash")
               ,capture_text = debug
    )

  }
  else {
    myMessage("Docker Sparkler exists", level = 3)
  }

  docker_cmd(vm,
             ,"exec"
             ,args = c(dockername,"/data/solr/bin/solr","restart","-force")
             ,capture_text = debug
  )

  myMessage("running solr admin , http://",vm$ip,":8983/solr/#/", level = 3)
  myMessage("extract solr data , http://",vm$ip,":8983/solr/crawldb/query?q=*:*", level = 3)
  myMessage("running jobs spark , http://",vm$ip,":4041/jobs/", level = 3)
  myMessage("banana , http://",vm$ip,":8983/banana/#/dashboard", level = 3)

}


#' Launch a crawl
#'
#' @param vm The Instance object
#' @param url URL website to crawl
#' @param topUrls Number of URLs in each website
#' @param topGroups Number of hosts to fetch in parallel.
#' @param maxIter Number of iterations to run.
#' @param debug If TRUE, will see debug messages.
#' @param mode Choose your delays (default:1000ms,fast:500ms,turbo:100ms) between two fetch requests for the same host
#'
#' @details
#'
#' Check if Docker exists and running
#' - If not, we create the docker with Sparkler with the "docker run" command
#' - If exists, we restart it
#' Next, we use "sparkler crawl" to inject URL parameters in Sparkler and launch a crawl.
#'
#' Very important: Sparkler developers slow down the crawl to avoid getting blocked from the websites.
#' Top groups = number of hosts to fetch in parallel.
#' Top N = number of URLs in each website.
#' By default, it tries for 256 groups and 1000 URLs in each group
#'
#'
#' @return The crawl Id
#'
#' @examples
#'
#' \dontrun{
#'
#' library(RsparkleR)
#'
#' ovh <- import_ovh()
#' client <- load_client(ovh, endpoint, application_key, application_secret, consumer_key)
#'
#' sshPubKeyPath  <- 'C:/Users/vterrasi/.ssh/id_rsa.pub'
#' sshPrivKeyPath <- 'C:/Users/vterrasi/.ssh/id_rsa'
#'
#' vm <- sparkler.create(client, regionVM="UK1", typeVM="s1-4", sshPubKeyPath, sshPrivKeyPath)
#'
#' sparkler.start(vm, debug)
#' url <- "https://www.YOUR WEBSITE.com"
#' pattern <- "www.YOUR WEBSITE.com"
#'
#' topN <- 1000
#' maxIter <- 100;
#' topGroups <- 2
#'
#' crawlid <- sparkler.crawl(vm, url, topN, topGroups, maxIter, debug=FALSE, mode="fast")
#'
#' }
#'
#' @export
sparkler.crawl <-function(vm, url, topUrls, topGroups, maxIter, debug=FALSE, mode="default") {

  dockername <- "sparkler_instance"

  crawlid <- format(Sys.time(), "%Y%m%d-%H%M%S")

  docker_cmd(vm,
             ,"exec"
             ,args = c(dockername,"/data/sparkler/bin/sparkler.sh","inject","-id",crawlid,"-su",url)
             ,capture_text = debug
  )

  docker_cmd(vm,
             ,"exec"
             ,args = c(dockername,"/data/sparkler/bin/sparkler.sh","crawl","-id",crawlid,"-tn",topUrls,"-tg",topGroups,"-i",maxIter)
             ,capture_text = FALSE
  )

  if (mode=="turbo") {
    # 100 ms
    path <- get_sparkler_file("sparkler-turbo")
    cloud_ssh_upload(vm, path, "/home/debian/sparkler-turbo.yaml")
    docker_cmd(vm, cmd = "cp", args = c("./sparkler-turbo.yaml", "sparkler_instance:/data/sparkler/conf/sparkler-default.yaml"))
    myMessage("MODE TURBO ON", level = 3)
  }
  else if (mode=="fast") {
    # 500 ms
    path <- get_sparkler_file("sparkler-fast")
    cloud_ssh_upload(vm, path, "/home/debian/sparkler-fast.yaml")
    docker_cmd(vm, cmd = "cp", args = c("./sparkler-fast.yaml", "sparkler_instance:/data/sparkler/conf/sparkler-default.yaml"))
    myMessage("MODE FAST ON", level = 3)
  }
  else {
    # default mode : 1000s
    path <- get_sparkler_file("sparkler-default")
    cloud_ssh_upload(vm, path, "/home/debian/sparkler-default.yaml")
    docker_cmd(vm, cmd = "cp", args = c("./sparkler-default.yaml", "sparkler_instance:/data/sparkler/conf/sparkler-default.yaml"))
    myMessage("MODE DEFAULT ON", level = 3)
  }

  # TODO : test if connection ; also msg error
  myMessage("*****************************************************", level = 3 )
  myMessage("*****************************************************", level = 3 )

  myMessage("running solr admin , http://",vm$ip,":8983/solr/#/", level = 3)

  myMessage("extract solr data , http://",vm$ip,":8983/solr/crawldb/query?q=*:*", level = 3)

  myMessage("running jobs spark , http://",vm$ip,":4041/jobs/", level = 3)

  myMessage("banana , http://",vm$ip,":8983/banana/#/dashboard", level = 3)

  if (debug==TRUE) {

    myMessage("debug : ",paste("http://",vm$ip,":8983/solr/crawldb/query?q=*:*&facet=true&facet.field=status&facet.field=depth&facet.field=group",sep=""), level = 3)

    myMessage("debug : ",paste("http://",vm$ip,":8983/solr/crawldb/query?q=*:*&fq=crawl_id:",crawlid,"&fl=url,hostname,status&rows=1000",sep=""), level = 3)

    myMessage("debug : ",paste("http://",vm$ip,":8983/solr/crawldb/query?q=*:*&fq=crawl_id:",crawlid,"&fq=status:FETCHED&fl=url,hostname,status&rows=1000",sep=""), level = 3)

  }

  myMessage("*****************************************************", level = 3 )
  myMessage("*****************************************************", level = 3 )

  return(crawlid)
}


#' Check State
#'
#' Print the state of crawl
#'
#' @param vm The Instance object
#' @param pattern The hostname.
#' @param crawlid The Id of crawl.
#' @param maxUrls The max URLs for fetching.
#'
#' @details
#'
#' Display progress bar
#' Return TRUE When UNFECTCHED count becomes zero
#'
#' @return Boolean
#'
#' @examples
#'
#' \dontrun{
#'
#' library(RsparkleR)
#' ovh <- import_ovh()
#' client <- load_client(ovh, endpoint, application_key, application_secret, consumer_key)
#'
#' sshPubKeyPath  <- 'C:/Users/vterrasi/.ssh/id_rsa.pub'
#' sshPrivKeyPath <- 'C:/Users/vterrasi/.ssh/id_rsa'
#'
#' vm <- sparkler.create(client, regionVM="UK1", typeVM="s1-4", sshPubKeyPath, sshPrivKeyPath)
#'
#' url <- "https://www.YOUR WEBSITE.com"
#' pattern <- "www.YOUR WEBSITE.com"
#' topN <- 1000
#  maxIter <- 100;
#  topGroups <- 2
#'
#' crawlid <- sparkler.crawl(vm, url, topN, topGroups, maxIter, debug, mode="fast")
#'
#' checkAll <- sparkler.check(vm, pattern, crawlid, topN)
#'
#' }
#'
#' @import data.table
#' @export
sparkler.check <- function(vm, pattern, crawlid, maxUrls) {

  crawlDF <- sparkler.read.csv(vm, pattern, crawlid, maxUrls)

  #Use data.table in spite of dplyr
  crawlDT <- data.table(crawlDF)
  crawlDFGroup <- crawlDT[, list(length(url)),
                          by = crawlDT$status]
  colnames(crawlDFGroup) <- c("status","count")

  total <- sum(crawlDFGroup$count)
  totalUnfetched <- 0
  totalFetched <- 0
  complete <- 0

  if (total>1) {
    if (any(crawlDFGroup$status=="FETCHED"))
      totalFetched <- crawlDFGroup[crawlDFGroup$status=="FETCHED",]$count

    if (any(crawlDFGroup$status=="ERROR"))
      totalFetched <- totalFetched + crawlDFGroup[crawlDFGroup$status=="ERROR",]$count

    complete <- round((as.numeric(totalFetched/total)*100),2)

    totalUnfetched <- crawlDFGroup[crawlDFGroup$status=="UNFETCHED",]$count
  }

  myMessage("Progress Bar :",complete,"%", level = 3)

  #When UNFECTCHED count becomes zero, it is the end of crawl, return TRUE
  if (totalUnfetched==0) return(TRUE)
  else return(FALSE)

}



#' Shutdown Solr and remove the container
#'
#' @param vm The Instance object
#' @param debug If TRUE, will see debug messages.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' library(RsparkleR)
#'
#' ovh <- import_ovh()
#' client <- load_client(ovh, endpoint, application_key, application_secret, consumer_key)
#'
#' sshPubKeyPath  <- 'C:/Users/vterrasi/.ssh/id_rsa.pub'
#' sshPrivKeyPath <- 'C:/Users/vterrasi/.ssh/id_rsa'
#'
#' vm <- sparkler.create(client, regionVM="UK1", typeVM="s1-4", sshPubKeyPath, sshPrivKeyPath)
#'
#' sparkler.stop(vm, debug=FALSE)
#'
#' }
#'
#' @export
sparkler.stop <-function(vm, debug=FALSE) {

  dockername <- "sparkler_instance"

  docker_cmd(vm,
             ,"exec"
             ,args = c(dockername,"/data/solr/bin/solr","stop")
             ,capture_text = debug
  )

  docker_cmd(vm,
             ,"stop"
             ,args = c(dockername)
             ,capture_text = debug
  )

  docker_cmd(vm,
              ,"rm"
              ,args = c(dockername)
              ,capture_text = debug
  )

}


#' Read Solr CSV
#'
#' @param vm The Instance object
#' @param pattern The hostname
#' @param crawl_id The Id of crawl
#' @param limit The max URLs you want
#' @param extracted It TRUE, will get extracted text
#'
#' @details
#'
#' You can find a SolR tutorial here : https://lucene.apache.org/solr/guide/7_2/solr-tutorial.html
#'
#' @return A dataframe
#'
#' @examples
#'
#' \dontrun{
#'
#' library(RsparkleR)
#'
#' ovh <- import_ovh()
#' client <- load_client(ovh,endpoint,application_key,application_secret,consumer_key)
#'
#' sshPubKeyPath  <- 'C:/Users/vterrasi/.ssh/id_rsa.pub'
#' sshPrivKeyPath <- 'C:/Users/vterrasi/.ssh/id_rsa'
#'
#' vm <- sparkler.create(client, regionVM="UK1", typeVM="s1-4", sshPubKeyPath, sshPrivKeyPath)
#'
#' topN <- 1000
#  maxIter <- 100;
#  topGroups <- 2
#'
#' crawlid <- sparkler.crawl(vm, url, topN, topGroups, maxIter, debug=FALSE, mode="fast")
#'
#' crawlDF <- sparkler.read.csv(vm, pattern, crawlid, topN, extracted=FALSE)
#'
#' }
#'
#' @import data.table
#' @export
sparkler.read.csv <- function(vm, pattern, crawl_id, limit, extracted=FALSE) {

  url_solr <- paste("http://",vm$ip,":8983/solr/crawldb/query?q=*:*"
                    ,"&rows=",limit,"&fl=crawl_id,url,group,discover_depth,score,status,"
                    ,"hostname,parent,dedup_id,modified_time,indexed_at,last_update_at,"
                    ,"fetch_status_code,fetch_depth,fetch_timestamp,content_type,signature,"
                    ,"outlinks,"
                    ,"og:title_t_md,dc:title_t_md,"
                    ,"title_t_md,description_t_md,"
                    ,"server_t_hd,content-type_t_hd",
                    sep="")

  # get extracted txt
  if (extracted==TRUE)
    url_solr <- paste(url_solr,",extracted_text",sep="")

  url_solr <- paste(url_solr,"&fq=hostname:",pattern,sep="")
  url_solr <- paste(url_solr,"&fq=crawl_id:",crawl_id,sep="")
  url_solr <- paste(url_solr,"&wt=csv",sep="")

  myMessage(paste0("Read: ",url_solr), level = 3)

  res <- tryCatch({

    data.table::fread(url_solr,
             header=TRUE,
             encoding="UTF-8",
             stringsAsFactors=FALSE)


  }, error = function(ex) {
    myMessage("Error : no solr", level = 3)
  })


  res

}


#' Show the sparkler-config template files
#'
#' @param template
#'
#' This returns the file location of template files for use Docker
#'
#' @return file location
#'
#' @export
get_sparkler_file <- function(template){
  system.file("sparklerfiles", paste0(template,".yaml"), package = "RsparkleR")
}



