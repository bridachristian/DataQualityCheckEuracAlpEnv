mail_config_parsing_new = function(mail_config_file){
  
  list_inpt = xmlToList(mail_config_file)
  if(any(names(list_inpt) == "comment")){
    list_inpt = list_inpt[-which(names(list_inpt) == "comment")]
  }
  
  
  sender = list_inpt$sender
  my_smtp = list(host.name = list_inpt$host.name, port = list_inpt$port, user.name = list_inpt$user.name, passwd = list_inpt$passwd, ssl = list_inpt$ssl)
  
  reciver = strsplit(list_inpt$reciver,",")[[1]]

  url_webservice =  list_inpt$url_webservice
  
  output = list(sender, reciver, my_smtp, url_webservice)
  names(output) = c("sender", "reciver","my_smtp","url_webservice")
  return(output)
}
