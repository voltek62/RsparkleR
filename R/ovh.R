#' Instance Object
#'
#' @details
#' An Instance resource.
#'
#' @param name The name of the resource, provided by the client when initially creating the resource
#' @param machineType The type of your server ( s1-2, s1-4 )
#' @param region The name of datacenter : SBG3, BHS3, WAW1, UK1, DE1, GRA3
#' @param description ( not used )
#' @param projectId The id of the cloud project
#' @param instanceId The id of the instance
#' @param ip The IP of your instance ( format IPV4 )
#'
#' @details
#' About regions :
#' SBG3 Datacenter is in France
#' BHS3 Datacenter is in Canada
#' WAW1 Datacenter is in Poland
#' UK1  Datacenter is in UK
#' DE1  Datacenter is in Deutch
#' GRA3 Datacenter is in France
#'
#' About range of cloud servers
#' https://www.ovh.co.uk/public-cloud/instances/prices/
#'
#' @return Instance object
#'
#' @family Instance functions
#' @keywords internal
Instance <- function(name = NULL,
                     machineType = NULL,
                     region = NULL,
                     description = NULL,
                     projectId = NULL,
                     instanceId = NULL,
                     ip = NULL) {

  structure(list(
    name = name,
    machineType = machineType,
    region = region,
    description = description,
    projectId = projectId,
    instanceId = instanceId,
    ip = ip),
    class = c("ovh_instance", "list")
  )
}


#' Lightweight wrapper around OVH's APIs. Handles all the hard work including credential creation and requests signing.
#' Import Python Library : https://github.com/ovh/python-ovh
#'
#' @details
#' An Instance resource.
#' 1. you need to install Python
#' 2. execute this command : pip install ovh
#'
#' @import reticulate
#' @export
import_ovh <- function() {

  ovh <- tryCatch({

    ovh <- import("ovh")

    myMessage("Library OVH ready", level = 3)

    ovh

  }, error = function(ex) {
    myMessage("Error : Please install : install python 64 bits and pip install ovh", level = 3)
  })

  ovh

}

#' Create a client using configuration
#'
#' @param ovh The OVH lib
#' @param endpoint e.g ; ovh-eu
#' @param appkey Application key
#' @param appsecret Secret app key
#' @param consumerkey Consumer key
#'
#' @details
#' You need to get appkey, appsecret, consumerkey
#' The four last can be took directly from their page createApp by filling fields. It is easy.
#' https://api.ovh.com/createToken/index.cgi?GET=/*&POST=/*&PUT=/*&DELETE=/*
#'
#' @return client
#'
#' @export
load_client <- function(ovh,endpoint,appkey,appsecret,consumerkey) {

  # TODO : if not exists
  client <- tryCatch({

    client <- ovh$Client(
      endpoint=endpoint,
      application_key=appkey,
      application_secret=appsecret,
      consumer_key=consumerkey
    )

    myMessage("OVH Client ready", level = 3)

    client

  }, error = function(ex) {
    myMessage("Error : Please create your key : https://api.ovh.com/createToken/index.cgi?GET=/*&POST=/*&PUT=/*&DELETE=/*", level = 3)
    myMessage("Error loadClient : ",myAPIError(ex), level = 3)
  })

  client

}


#' Get Project Info
#'
#' Get this object properties
#'
#' \url{https://api.ovh.com/console/#/cloud}
#'
#' @param client The client previously loaded
#' @param projectId The Id of the project
#'
#' @return JSON
#'
#' @export
get_project_info <- function(client,projectId) {

  api_project <- paste('/cloud/project/',projectId,sep="")

  project <- tryCatch({

    client$get(api_project)

  }, error = function(ex) {

    myMessage("Error getProjectInfo : ",myAPIError(ex), level = 3)

  })

  project
}



#' Get Projects
#'
#' List available services
#'
#' \url{https://api.ovh.com/console/#/cloud}
#'
#' @param client The client previously loaded
#'
#' @return JSON
#'
#' @export
get_projects <- function(client) {

  projects <- tryCatch({

    client$get('/cloud/project')

  }, error = function(ex) {

    myMessage("Error getProjects : ",myAPIError(ex), level = 3)
  })

  projects
}

#' Create Cloud Project
#'
#' Start a new cloud project
#'
#' \url{https://api.ovh.com/console/#/cloud}
#'
#' @param client The client previously loaded
#' @param name The name of the project
#'
#' @return JSON
#'
#' @export
create_project <- function(client,name) {

  project <- tryCatch({

    myMessage('Creating new project', level = 3)

    project <- client$post('/cloud/createProject', description=name)

    myMessage('New project created', level = 3)

    project

  }, error = function(ex) {

    myMessage("Error createProject : ",myAPIError(ex), level = 3)

  })

  project


}

#' Delete an instance
#'
#' \url{https://api.ovh.com/console/#/cloud}
#'
#' @param client The client previously loaded
#' @param projectId The Id of the project
#' @param instanceId The Id of the instance
#'
#' @export
delete_instance <- function(client,projectId,instanceId) {

  api_instance <- paste('/cloud/project/',projectId,"/instance/",instanceId,sep="")

  project <- tryCatch({
    myMessage('Deleting project', level = 3)
    project <- client$delete(api_instance)
    myMessage('Project deleted', level = 3)
    project

  }, error = function(ex) {
    myMessage("Error deleteProject : ",myAPIError(ex), level = 3)
  })

  project

}


#' Get instances from a specific project
#'
#' \url{https://api.ovh.com/console/#/cloud}
#'
#' @param client The client previously loaded
#' @param projectId The Id of the project
#'
#' @return JSON
#'
#' @export
get_intances <- function(client,projectId) {

  api_instance <- paste("/cloud/project/",projectId,"/instance",sep="")
  instances <- tryCatch({
    myMessage('Get instances', level = 3)
    client$get(api_instance)
  }, error = function(ex) {
    myMessage("Error getInstances : ",myAPIError(ex), level = 3)
  })

  instances

}


#' Get instance Detail from a specific project
#'
#' \url{https://api.ovh.com/console/#/cloud}
#'
#' @param client The client previously loaded
#' @param projectId The id of the project
#' @param instanceId The id of the instance
#'
#' @return JSON
#'
#' @export
get_intance <- function(client,projectId,instanceId) {

  api_instance <- paste("/cloud/project/",projectId,"/instance/",instanceId,sep="")

  instance <- tryCatch({
    myMessage('Get instance detail', level = 3)
    client$get(api_instance)

  }, error = function(ex) {
    myMessage("Error getInstance : ",myAPIError(ex), level = 3)
  })

  instance

}


#' Get Flavors
#'
#' Get all flavors
#'
#' \url{https://api.ovh.com/console/#/cloud}
#'
#' @param client The client previously loaded
#' @param projectId The id of the project
#'
#' @details
#' In OpenStack, a flavor defines the compute, memory, and storage capacity of a virtual server, also known as an instance.
#'
#' @return Data.frame
#'
#' @export
get_flavors <- function(client, projectId) {

  api_flavor <- paste("/cloud/project/",projectId,"/flavor",sep="")

  flavors <- tryCatch({

    myMessage('Get flavors', level = 3)
    flavorsList <- client$get(api_flavor)
    flavorsSelect <- sapply( flavorsList, function(m) m[c("name","region","available","ram","osType","disk","id")] )
    as.data.frame(t(flavorsSelect))

  }, error = function(ex) {
    myMessage("Error getFlavors : ",myAPIError(ex), level = 3)
  })

  flavors

}


#' Get Images
#'
#' Get all images
#'
#' \url{https://api.ovh.com/console/#/cloud}
#'
#' @param client The client previously loaded
#' @param projectId The id of the instance
#'
#' @details
#' A virtual machine image is a single file that contains a virtual disk that has a bootable operating system installed on it.
#' Images are used to create virtual machine instances within the cloud.
#'
#' @return Data.frame
#'
#' @export
get_images <- function(client, projectId) {

  api_image <- paste("/cloud/project/",projectId,"/image",sep="")

  images <- tryCatch({

    myMessage('Get Images', level = 3)
    imagesList <- client$get(api_image)
    imagesSelect <- sapply( imagesList, function(m) m[c("status","name","region","id","user","visibility")] )
    as.data.frame(t(imagesSelect))

  }, error = function(ex) {
    myMessage("Error getImages : ",myAPIError(ex), level = 3)
  })

  images

}

#' Create new Instance
#'
#' \url{https://api.ovh.com/console/#/cloud}
#'
#' @param client The client previously loaded
#' @param projectId The id of your cloud project
#' @param flavorId The id of your flavor
#' @param imageId The id of your distribution
#' @param name The name of your instance
#' @param region The name of your datacenter
#' @param sshKeyId The Id of your SSH Key
#'
#' @return JSON
#'
#' @export
create_instance <- function(client, projectId, flavorId, imageId, name, region, sshKeyId) {

  api_instance <- paste("/cloud/project/",projectId,"/instance",sep="")

  res <- tryCatch({

    myMessage('Creating Instance', level = 3)

    res <- client$post(api_instance,
                flavorId = flavorId,
                imageId = imageId,
                name = name,
                region = region,
                sshKeyId = sshKeyId
    )

    myMessage('Instance created', level = 3)

    res

  }, error = function(ex) {

    myMessage("Error createVM : ",myAPIError(ex), level = 3)

  })

  res

}

#' Create an Instance with Docker
#'
#' @param client The client previously loaded
#' @param projectId The Id of the project
#' @param name The name of the instance
#' @param regionVM The name of datacenter: SBG3, BHS3, WAW1, UK1, DE1, GRA3
#' @param typeVM The range of your cloud server ( s1-2, s1-4: https://www.ovh.co.uk/public-cloud/instances/prices/)
#' @param sshKeyId The Id of ssh key
#'
#' @details
#' Logic Workflow
#' 1. get the available flavor where the name matching with the specific region
#' 2. get the active image where the name matching with "Docker"
#' 3. create the instance
#'
#' @return JSON
#'
#' @import dplyr
#' @export
create_docker <- function(client, projectId, name, regionVM, typeVM, sshKeyId) {

  flavors <- get_flavors(client, projectId)
  flavors <- select(filter(flavors,grepl(regionVM,flavors$region) & flavors$available==TRUE & flavors$name==typeVM),id)
  flavorId <- toString(flavors[[1]])

  images <- get_images(client, projectId)
  images <- select(filter(images,grepl(regionVM,images$region) & images$status=="active" & grepl("Docker",images$name)),id)
  imageId <- toString(images[[1]])

  api_instance <- paste("/cloud/project/",projectId,"/instance",sep="")

  res <- tryCatch({

    myMessage('Creating an Instance with Docker', level = 3)

    res <- client$post(api_instance,
                flavorId = flavorId,
                imageId = imageId,
                name = name,
                region = regionVM,
                sshKeyId = sshKeyId
    )

    myMessage('Instance with Docker created', level = 3)

    res

  }, error = function(ex) {

    myMessage("Error createVM : ",myAPIError(ex), level = 3)

  })

  res

}

#' Get SSH Keys
#'
#' \url{https://api.ovh.com/console/#/cloud}
#'
#' @param client The client previously loaded
#' @param projectId The Id of the project
#'
#' @return JSON
#'
#' @export
get_keys <- function(client, projectId) {

  api_key <- paste("/cloud/project/",projectId,"/sshkey",sep="")

  res <- tryCatch({

    myMessage('get SSH keys', level = 3)

    client$get(api_key)

  }, error = function(ex) {

    myMessage("Error get SSH Keys : ",myAPIError(ex), level = 3)

  })

  res

}

#' Create SSH Key
#'
#' \url{https://api.ovh.com/console/#/cloud}
#'
#' @param client The client previously loaded
#' @param projectId The id of the cloud project
#' @param name The name of the SSH Key
#' @param pathPublicKey The path of your public SSH Key
#'
#' @return JSON
#'
#' @export
create_key <- function(client, projectId, name, pathPublicKey) {

  api_key <- paste("/cloud/project/",projectId,"/sshkey",sep="")

  res <- tryCatch({

    myMessage('Creating SSH Key', level = 3)

    con <- file(pathPublicKey,open="r")
    publicKey <-  readLines(con)
    close(con)

    res <- client$post(api_key,
                serviceName = projectId,
                publicKey = publicKey,
                name = name
    )

    myMessage('SSH Key created', level = 3)

    res

  }, error = function(ex) {

    myMessage("Error create_key : ",myAPIError(ex), level = 3)

  })

  res

}


#' Show the docker template files
#'
#' @param path The file name you want
#'
#' This returns the file location of template files for use Docker
#'
#' @return file location
#'
#' @export
get_docker_file <- function(path){
  system.file("dockerfiles", path, package = "RsparkleR")
}
