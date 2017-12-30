

#' Remotely execute ssh code, upload & download files.
#'
#' Taken via https://github.com/cloudyr/googleComputeEngineR/blob/master/R/ssh.R
#'
#' @description
#' Assumes that you have ssh & scp installed.  If on Windows see website and examples for workarounds.
#'
#' @details
#'
#' Only works connecting to linux based instances.
#'
#' On Windows you will need to install an ssh command line client - see examples for an example using RStudio's built in client.
#'
#' You will need to generate a new SSH key-pair if you have not connected to the instance before via say the gcloud SDK.
#'
#' To customise SSH connection see \link{sparkler.create}
#'
#' \code{capture_text} is passed to \code{stdout} and \code{stderr} of \link{system2}
#'
#' Otherwise, instructions for generating SSH keys can be found here: \url{https://www.ovh.com/us/publiccloud/guides/g1769.creating_ssh_keys}.
#'
#' Uploads and downloads are recursive, so if you specify a directory,
#' everything inside the directory will also be downloaded.
#'
#' @param instance The Instance object
#' @param ... List of parameters
#' @param key.pub The public SSH key
#' @param key.private The private SSH key
#' @param username The useranme
#' @param local,remote Local and remote paths.
#' @param overwrite If TRUE, will overwrite the local file if exists.
#' @param wait Whether then SSH output should be waited for or run it asynchronously.
#' @param capture_text Possible values are "", to the R console (the default), NULL or FALSE (discard output), TRUE (capture the output in a character vector) or a character string naming a file.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#'   # you need to instance a client for creating VM
#'
#'   ## if running on Windows, use the RStudio default SSH client
#'   ## add C:\Program Files\RStudio\bin\msys-ssh-1000-18 to your PATH
#'   ## AND
#'   ## add C:\Program Files\RStudio\bin\msys-ssh-1000-18 to your RSTUDIO_SSH_PATH
#'   ## then run:
#'
#'   regionVM <- "UK1"
#'   typeVM <- "s1-4"
#'   sshPubKeyPath  <- './.ssh/id_rsa.pub'
#'   sshPrivKeyPath <- './.ssh/id_rsa'
#'   vm <- createSparkler(client,regionVM,typeVM,sshPubKeyPath,sshPrivKeyPath)
#'
#'   ## if you have already logged in via gcloud, the default keys will be used
#'   ## no need to run cloud_ssh_addkeys
#'   ## run command on instance
#'   cloud_ssh(vm, "pwd")
#'   #> /user/debian
#'
#'
#' }
#' @export
#' @family ssh functions
cloud_ssh <- function(instance,
                    ...,
                    key.pub = NULL,
                    key.private = NULL,
                    wait = TRUE,
                    capture_text = "",
                    username = "") {


  username <- instance$ssh$username

  lines <- paste(c(...), collapse = " \\\n&& ")
  if (lines == "") stop("Provide commands", call. = FALSE)

  sargs <- c(
    ssh_options(instance),
    paste0(username, "@", instance$ip),
    shQuote(lines)
  )

  do_system(instance, cmd = "ssh", sargs = sargs, wait = wait, capture = capture_text)

}

#' @export
#' @rdname cloud_ssh
cloud_ssh_upload <- function(instance,
                           local,
                           remote,
                           wait = TRUE) {

  username <- instance$ssh$username

  # PATCH : if you don't use the RStudio default SSH client
  if (grepl("Windows",Sys.info()["sysname"])) {

    local <- paste0("/",(sub(":/","/",local)))

    sargs <- c(
      scp_options_windows(instance),
      local,
      paste0(username, "@", instance$ip, ":", remote))
  }
  else {
    sargs <- c(
      ssh_options(instance),
      local,
      paste0(username, "@", instance$ip, ":", remote))
  }

  do_system(instance, cmd = "scp", sargs = sargs, wait = wait)

}

#' @export
#' @rdname cloud_ssh
cloud_ssh_download <- function(instance,
                             remote,
                             local,
                             overwrite = FALSE,
                             wait = TRUE) {

  username <- instance$ssh$username

  # PATCH : if you don't use the RStudio default SSH client
  if (grepl("Windows",Sys.info()["sysname"])) {
    sargs <- c(
      "-r ", scp_options_windows(instance),
      paste0(username, "@", instance$ip, ":", remote),
      local)
  }
  else {
    sargs <- c(
      "-r ", ssh_options(instance),
      paste0(username, "@", instance$ip, ":", remote),
      local)
  }

  do_system(instance, cmd = "scp", sargs = sargs, wait = wait)
}


do_system <- function(instance,
                      cmd = "ssh",
                      sargs = character(),
                      wait = TRUE,
                      capture = ""
) {

  #stopifnot(is.ovh_instance(instance))

  ## check ssh/scp installed
  cli_tools()

  external_ip <- instance$ip

  # check to make sure port 22 open, otherwise ssh commands will fail
  if (!is_port_open(external_ip, 22)) {
    stop("port 22 is not open for ", external_ip, call. = FALSE)
  }

  ## do the command
  myMessage(cmd, " ", paste(sargs, collapse = " "), level = 2)

  status <- system2(cmd, args = sargs, wait = wait, stdout = capture, stderr = capture)

  if(capture == TRUE){
    ## return the command text to local R

    ## maybe a warning available in attr(status, "status) or attr(status, "errmsg)
    if(!is.null(attr(status, "status"))){
      myMessage("Remote error: ", attr(status, "status"), attr(status, "errmsg"), level = 3)
    }

    ## status is the output text
    ## parse out the connection warning
    host_warn <- status[grepl("^Warning: Permanently added .* to the list of known hosts", status)]
    myMessage(host_warn, level = 3)
    status <- status[!grepl("^Warning: Permanently added .* to the list of known hosts", status)]
    out <- status

  } else {
    ## status if error code (0 for success)
    if (status == 127) {
      stop("ssh failed\n", cmd, paste(sargs, collapse = " "), call. = FALSE)
    }

    ## output may be written to file if capture = "filepath"
    if(is.character(capture) && file.exists(capture)){
      myMessage("Wrote output to ", capture, level = 2)
    }

    myMessage("status: ", status, " wait: ", wait, level = 2)
    out <-   invisible(TRUE)
  }


  out

}

ssh_options <- function(instance) {
  opts <- c(
    BatchMode = "yes",
    StrictHostKeyChecking = "no",
    UserKnownHostsFile = paste0("'",file.path(tempdir(), "hosts"),"'")
  )

  if(exists("ssh", instance)){
    private_key <- instance$ssh$key.private
  }

  if(!file.exists(private_key)) stop("Couldn't find private key")

  c(paste0("-o ", names(opts), "=", opts, collapse = " "),
    " -i ",
    paste0("'",private_key,"'"))
}

scp_options_windows <- function(instance) {
  opts <- c(
    BatchMode = "yes",
    StrictHostKeyChecking = "no",
    UserKnownHostsFile = paste0("'",file.path(tempdir(), "hosts"),"'")
  )

  if(exists("ssh", instance)){
    private_key <- instance$ssh$key.private
  }

  if(!file.exists(private_key)) stop("Couldn't find private key")

  path_ssh_windows <- Sys.getenv('RSTUDIO_SSH_PATH')
  if(path_ssh_windows=="") stop("See ?cloud_ssh for workarounds, including using the RStudio native SSH client.")

  c(paste0("-S ", paste0("'",path_ssh_windows,"\\ssh'"), collapse = ""),
    paste0("-o ", names(opts), "=", opts, collapse = " "),
    " -i ",
    paste0("'",private_key,"'"))
}

#' Add SSH details to a cloud_instance
#'
#'
#' @param instance The cloud_instance
#' @param username SSH username to login with
#' @param key.pub filepath to public SSH key
#' @param key.private filepath to the private SSK key
#' @param overwrite Overwrite existing SSH details if they exist
#'
#' @details
#'
#' You will only need to run this yourself if you save your SSH keys somewhere other
#'   than \code{$HOME/.ssh/google_compute_engine.pub} or use a different username than
#'   your local username as found in \code{Sys.info[["user"]]}, otherwise it will configure
#'   itself automatically the first time you use \link{cloud_ssh} in an R session.
#'
#' If key.pub is NULL then will look for default Google credentials at
#'   \code{file.path(Sys.getenv("HOME"), ".ssh", "google_compute_engine.pub")}
#'
#' @return The instance with SSH details included in $ssh
#'
#' @family ssh functions
#' @export
cloud_ssh_addkeys <- function(instance,
                            key.pub = NULL,
                            key.private = NULL,
                            username = Sys.info()[["user"]],
                            overwrite = FALSE){

  #stopifnot(is.ovh_instance(instance))

  if(exists("ssh", instance)){
    if(!overwrite){
      myMessage("SSH keys already set", level = 1)
      return(instance)
    } else {
      myMessage("Overwriting SSH keys data on local instance object", level = 3)
    }
  }

  if(!is.null(key.pub) & !is.null(key.private)){
    myMessage("Using ssh-key files given as ",
              key.pub," / ", key.private,
              level = 3)

    stopifnot(file.exists(key.pub))
    stopifnot(file.exists(key.private))

    key.private <- normalizePath(key.private)
    key.pub.content  <- readChar(key.pub, 10000)
  }

  instance$ssh <- list(
    username = username,
    key.pub = key.pub.content,
    key.private = key.private
  )

  instance

}


#' Test to see if a TCP port is open
#'
#' Taken via https://github.com/sckott/analogsea/blob/e728772013ad286750e0e89dc261a36fa31d4647/R/is_port_open.R
#'
#' @param host ip or host name to connect to
#' @param port port to connect to
#' @param timeout how many secs to let it try
#' @noRd
#' @author Bob Rudis \email{bob@@rudis.net}
#' @examples \dontrun{
#' is_port_open("httpbin.org", 80)
#' is_port_open("httpbin.org", 22)
#' }
is_port_open <- function(host, port=22, timeout=1) {

  WARN <- getOption("warn")
  options(warn = -1)

  con <- try(socketConnection(host, port, blocking = TRUE, timeout = timeout),
             silent = TRUE)

  if (!inherits(con, "try-error")) {
    close(con)
    options(warn = WARN)
    TRUE
  } else {
    options(warn = WARN)
    FALSE
  }

}

#' See if ssh or scp is installed
#' From https://github.com/sckott/analogsea/blob/master/R/zzz.R
#' @keywords internal
cli_tools <- function(){
  tmp <- Sys.which(c("ssh","scp"))
  if (any(tmp == "")) {
    nf <- paste0(names(tmp)[tmp == ""], collapse = ", ")
    if(.Platform$OS.type == "windows"){
      stop(sprintf("\n%s not found on your computer\nInstall the missing tool(s) and try again. See ?cloud_ssh for workarounds, including using the RStudio native SSH client.", nf))
    } else {
      stop(sprintf("\n%s not found on your computer\nInstall the missing tool(s) and try again", nf))
    }

  }
}


