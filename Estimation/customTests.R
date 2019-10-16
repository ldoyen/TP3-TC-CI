# Put custom tests in this file.

# Uncommenting the following line of code will disable
# auto-detection of new variables and thus prevent swirl from
# executing every command twice, which can slow things down.

# AUTO_DETECT_NEWVAR <- FALSE

# However, this means that you should detect user-created
# variables when appropriate. The answer test, creates_new_var()
# can be used for for the purpose, but it also re-evaluates the
# expression which the user entered, so care must be taken.

# Get the swirl state
getState <- function(){
  # Whenever swirl is running, its callback is at the top of its call stack.
  # Swirl's state, named e, is stored in the environment of the callback.
  environment(sys.function(1))$e
}

# Retrieve the log from swirl's state
getLog <- function(){
  getState()$log
}

Choix_sujet_etudiant<-function(num_etud,nb_sujet=5){
  #return(floor((num_etud-floor(num_etud/100)*100)/20))
  set.seed(num_etud)
  #return(sample(1:nb_sujet,size=2)[2])
  #Pour garantir que les redoublants n'ont pas le meme sujet que l annee derniere
  #L'annee prochaine reprendre l'instruction precedente pouir le garantir
  sujet_prec<-sample(1:nb_sujet,size=2)[2]
  sujet_indice<-sample(1:(nb_sujet-1),1)
  sujet_possibles<-setdiff(1:nb_sujet,sujet_prec)
  return(sujet_possibles[sujet_indice])
}

genere_data<-function(vs){
  data<-replicate(vs$m2, mean(rbinom(vs$n,1,vs$p0)))
  iddata<-sample(1:vs$m1,6,replace=FALSE)


  data[iddata[1]]<-max((floor(vs$pinf*vs$n)-1)/vs$n,0)
  data[iddata[2]]<-max((floor(vs$pinf*vs$n)-4)/vs$n,0)
  data[iddata[3]]<-(ceiling(vs$pinf*vs$n)+1)/vs$n
  data[iddata[4]]<-(floor(vs$psup*vs$n)-1)/vs$n
  data[iddata[5]]<-min((ceiling(vs$psup*vs$n)+1)/vs$n,1)
  data[iddata[6]]<-min((ceiling(vs$psup*vs$n)+4)/vs$n,1)
  return(data)
}

num_etud<-function(){
  ###Les differents sujets
  variable_sujet<-list(
   prop=c(1,0,1,0,1,0),
   n=c(100,100,200,200,300,300),
   niv_confiance=c(85,85,85,80,80,80),
   Y=c("Y^A","Y^B","Y^C","Y^D","Y^X","Y^Z"),
   y=c("y^A","y^B","y^C","y^D","y^X","y^Z"),
   nom_data=c("yA","y_B","y_C_obs","yD","y_X","y_Z_obs"),
   Y_OQ=c("Y^Z","Y^X","Y^D","Y^C","Y^B","Y^A"),
   y_OQ=c("y^Z","y^X","y^D","y^C","y^B","y^A"),
   yy_OQ=c("yy_Z","yyX","yy_D","yyC","yy_B","yyA"),
   m_OQ=c(5000,6000,7000,5000,6000,7000),
   n_OQ=c(150,200,250,300,350,400),
   mu_OQ=c("muEst_Z","mu_Est_X","muEstD","muEst_C","mu_Est_B","muEstA"),
   delta_OQ=c("delta_Z","deltaX","delta_D","deltaC","delta_B","deltaA"),
   i_OQ=2:7,
   j_OQ=7:2,
   pageWebsujet=c("https://toltex.imag.fr/VAM/TP3/sujet.html","https://toltex.imag.fr/VAM/TP3/sujet_tp.html","https://toltex.imag.fr/VAM/TP3/sujet__tp.html","https://toltex.imag.fr/VAM/TP3/sujet3.html","https://toltex.imag.fr/VAM/TP3/sujet_tp3.html","https://toltex.imag.fr/VAM/TP3/sujet__tp3.html")
  )
  ####

  e <- get("e", parent.frame())
  num_etud <- as.integer(e$val)
  res<-TRUE
  if (is.na(num_etud)|(num_etud<0)){
    res<-FALSE
    } else {
    confirmation<-paste("Vous confirmez que votre num\xE9ro d'\xE9tudiant est", num_etud, "?",sep=" ")
    Encoding(confirmation)<- "latin1"
    message(confirmation)
    res<-readline("Tapez 1, si c'est bien le cas, sinon appuyez sur n'importe quelle autre touche. Puis validez.")==1
    if (res){
      e$num_etud<-num_etud
      e$num_sujet <- Choix_sujet_etudiant(num_etud,length(variable_sujet[[1]]))
      set.seed(num_etud)
      vs<-variable_sujet
      for (i in 1:length(vs)){
         vs[[i]]<-vs[[i]][e$num_sujet]
      }
      e$vs<-vs

      n<-vs$n_OQ
      m<-vs$m_OQ
      y<-round((1-rbeta(n*m,2,4))*100,digits=0)
      yy<-matrix(data=y,nrow=m,ncol=n)
      assign(vs$yy_OQ,yy,.GlobalEnv)
      e$log$skipped<-c()#pour corriger un bugg swirl: quand on fait deux leçons d'affile, il y a FALSE à l'initialisation de skipped, alors que ce n'est pas le cas pour la première leçon ???
      e$log$mon_skip<-e$log$skipped
    }
  }
  return(res)
}

submit_log <- function(){
  e <- get("e", parent.frame())

  res<-FALSE
  selection <- getState()$val
if(selection %in% 1:5){
  res<-TRUE

  nom_etud <- readline("Quel est votre nom de famille ? ")
  demande_prenom<-"Quel est votre pr\xE9nom ? "
  Encoding(demande_prenom) <- "latin1"
  prenom_etud <- readline(demande_prenom)

  # Please edit the link below
  pre_fill_link1 <- "https://docs.google.com/forms/d/e/1FAIpQLSe0vu3khlVduduY6VOb7bRKwlJ-suMTkHa3BHFQ2gkF-3vcdA/viewform?usp=pp_url&entry.2090562688="
  pre_fill_link2 <- "https://docs.google.com/forms/d/e/1FAIpQLSfeJPzm2QmCWIeHekmH0NWkDmgdo8gG_ElDHR_f5IMdAGdH8w/viewform?usp=pp_url&entry.1874543433="
  pre_fill_link3 <- "https://docs.google.com/forms/d/e/1FAIpQLSfy4qN-m-bEt2Ppw5s39hSr-Ur3fVLOKbp42srLKwWD-bSkNg/viewform?usp=pp_url&entry.1243347104="
  pre_fill_link4 <- "https://docs.google.com/forms/d/e/1FAIpQLSd-CYVKRMjXDdDlxctH1RQ1oeUXJLnl3r-trT4Pr2TN5u8TnQ/viewform?usp=pp_url&entry.108469289="
  pre_fill_link5 <- "https://docs.google.com/forms/d/e/1FAIpQLSe8Wlj-6QfeI6mcOZPC6UqugH7HtM09Bj7qcJbt7d2hASZupw/viewform?usp=pp_url&entry.1000315833="

  pre_fill_link <- switch(selection,
    pre_fill_link1,
    pre_fill_link2,
    pre_fill_link3,
    pre_fill_link4,
    pre_fill_link5
  )

  # Do not edit the code below
  if(!grepl("=$", pre_fill_link)){
    pre_fill_link <- paste0(pre_fill_link, "=")
  }

  p <- function(x, p, f, l = length(x)){if(l < p){x <- c(x, rep(f, p - l))};x}
  e$log$skipped[1:length(e$log$mon_skip)]<-e$log$mon_skip

  temp <- tempfile()
  log_ <- getLog()
  nrow_ <- max(unlist(lapply(log_, length)))
  log_tbl <- data.frame( p(log_$question_number, nrow_, NA),
                         p(log_$correct, nrow_, NA),
                         p(log_$attempt, nrow_, NA),
                         p(log_$skipped, nrow_, NA),
                         p(log_$datetime, nrow_, NA),
                        stringsAsFactors = FALSE)
  names(log_tbl) <- c(e$num_etud, nom_etud, prenom_etud,log_$lesson_name,e$num_sujet)
  write.csv(log_tbl, file = temp, row.names = FALSE)
  encoded_log <- base64encode(temp)
  e <- get("e", parent.frame())
  e$encoded_log<-encoded_log
  e$log_tbl<-log_tbl
  e$url_googleForm<-paste0(pre_fill_link, encoded_log)
  #browseURL(paste0(pre_fill_link, encoded_log)
  readline("Swirl va maintenant ouvrir un Google Form dans votre navigateur web. Tapez sur la touche Entrée.")
  browseURL(e$url_googleForm)

  e <- get("e", parent.frame())
    if(selection %in% c(3,4,5)) e$adresse_email<-"laurent.doyen@iut2.univ-grenoble-alpes.fr" else e$adresse_email<-"marie-jose.martinez@iut2.univ-grenoble-alpes.fr"
    e$sujet_email<-paste0("**TP3-TC-CI**"," G",selection,", ",log_$lesson_name,", ", nom_etud,collapse="")
    e$corp_email<-encoded_log
  }
  return(res)
}

submit_log_alt <- function(){
  e <- get("e", parent.frame())

  res<-FALSE
  selection <- getState()$val
#if(selection %in% 1:5){
  res<-TRUE

  nom_etud <- readline("Quel est votre nom de famille ? ")
  demande_prenom<-"Quel est votre pr\xE9nom ? "
  Encoding(demande_prenom) <- "latin1"
  prenom_etud <- readline(demande_prenom)

  # Please edit the link below
  #pre_fill_link1 <- "https://docs.google.com/forms/d/e/1FAIpQLSeWzSmQyQa5YE-MUL_0DxzD5RShhaKbWBS63Bu0AmdbxwmI2w/viewform?usp=pp_url&entry.1536247898="
  #pre_fill_link2 <- "https://docs.google.com/forms/d/e/1FAIpQLSfgztQT4bQTcgAuTlpMtVD5vQfAcLz5TWXqdS-D24Ctog4TFg/viewform?usp=pp_url&entry.1449157816="
  #pre_fill_link3 <- "https://docs.google.com/forms/d/e/1FAIpQLSc-MLNgzzLzS6znCGlIMnSpBwbfqsbmJYGItyOxL0ucInW3YQ/viewform?usp=pp_url&entry.947620631="
  #pre_fill_link4 <- "https://docs.google.com/forms/d/e/1FAIpQLSdHFMGd0kZ0K3n3wWX85Ka1FMonKLm1dF409NbjgIL0U0kMKA/viewform?usp=pp_url&entry.1829019151="
  #pre_fill_link5 <- "https://docs.google.com/forms/d/e/1FAIpQLSdXGObsIGsQlhgQ4UwxknYANU2EAlm8cbakMVxpNFD9kmsmgg/viewform?usp=pp_url&entry.958732492="

  #pre_fill_link <- switch(selection,
  #  pre_fill_link1,
  #  pre_fill_link2,
  #  pre_fill_link3,
  #  pre_fill_link4,
  #  pre_fill_link5
  #)

  pre_fill_link<-"https://docs.google.com/forms/d/e/1FAIpQLSd3Myo0EalPM3qCfmMQw2xpITKslfNrnjFiHBBu4_Uo4BIVYg/viewform?usp=pp_url&entry.1066847456="

  # Do not edit the code below
  if(!grepl("=$", pre_fill_link)){
    pre_fill_link <- paste0(pre_fill_link, "=")
  }

  p <- function(x, p, f, l = length(x)){if(l < p){x <- c(x, rep(f, p - l))};x}

  e$log$skipped[1:length(e$log$mon_skip)]<-e$log$mon_skip

  temp <- tempfile()
  log_ <- getLog()
  nrow_ <- max(unlist(lapply(log_, length)))

  log_tbl <- data.frame( p(log_$question_number, nrow_, NA),
                         p(log_$correct, nrow_, NA),
                         p(log_$attempt, nrow_, NA),
                         p(log_$skipped, nrow_, NA),
                         p(log_$datetime, nrow_, NA),
                        stringsAsFactors = FALSE)
  names(log_tbl) <- c(e$num_etud, nom_etud, prenom_etud,log_$lesson_name,e$num_sujet)
  write.csv(log_tbl, file = temp, row.names = FALSE)
  encoded_log <- base64encode(temp)
  e <- get("e", parent.frame())
  e$encoded_log<-encoded_log
  e$log_tbl<-log_tbl
  e$url_googleForm<-paste0(pre_fill_link, encoded_log)
  #browseURL(paste0(pre_fill_link, encoded_log)
  readline("Swirl va maintenant ouvrir un Google Form dans votre navigateur web. Tapez sur la touche Entrée.")
  browseURL(e$url_googleForm)

  e <- get("e", parent.frame())
    #if(selection %in% c(1,2,3)) e$adresse_email<-"laurent.doyen@iut2.univ-grenoble-alpes.fr" else e$adresse_email<-"marie-jose.martinez@iut2.univ-grenoble-alpes.fr"
    e$adresse_email<-"laurent.doyen@iut2.univ-grenoble-alpes.fr"
    e$sujet_email<-paste0("**TP3-TC-CI** Alt, ",log_$lesson_name,", ", nom_etud,collapse="")
    e$corp_email<-encoded_log
  #}
  return(res)
}

googleForm_log<-function(){
  e <- get("e", parent.frame())
  if(regexpr("Google Form",e$val)!=-1){
    res<-FALSE
    browseURL(e$url_googleForm)
  } else {
    res<-TRUE
   readline("Swirl va maintenant ouvrir un email dans votre logiciel de messagerie. Tapez sur la touche Entrée.")
    email(e$adresse_email,e$sujet_email,e$corp_email)
  }
  return(res)
}


email_log<-function(){
  e <- get("e", parent.frame())
  res<-TRUE
  if(regexpr("email",e$val)!=-1){
    res<-FALSE
    email(e$adresse_email,e$sujet_email,e$corp_email)
  }
  return(res)
}

sauve_log<-function(){
  demande<-"Appuyez sur Entr\xE9, puis choississez un r\xE9pertoire dans lequel sauver votre cl\xE9. Attention, dans les salles machine de l'IUT, choississez un r\xE9pertoire personnel."
  Encoding(demande) <- "latin1"
  rep <- readline(demande)
  path <- choose_dir()
  if(length(path)==0){
    return(FALSE)
  } else {
    setwd(path)
    e <- get("e", parent.frame())
    encoded_log<-e$encoded_log
    log_tbl<-e$log_tbl
    log_ <- getLog()
    e$fichier<-paste0("TP2",log_$lesson_name,".R")

    save(log_tbl,encoded_log,file=e$fichier)
    demande<-paste0("Votre cl\xE9, est sauv\xE9 dans le fichier ",e$fichier," Tapez sur la touche Entr\xE9e pour continuer.")
    Encoding(demande) <- "latin1"
    rep <- readline(demande)
    return(TRUE)
  }
}

qsauve_log<-function(){
e <- get("e", parent.frame())
if(e$val=="Oui"){
  return(TRUE)
} else {
  demande<-"Appuyez sur Entr\xE9, puis choississez un r\xE9pertoire dans lequel sauver votre cl\xE9. Attention, dans les salles machine de l'IUT, choississez un r\xE9pertoire personnel."
  Encoding(demande) <- "latin1"
  rep <- readline(demande)
  path <- choose_dir()
  if(length(path)==0){
    return(FALSE)
  } else {
    setwd(path)
    e <- get("e", parent.frame())
    encoded_log<-e$encoded_log
    log_tbl<-e$log_tbl

    save(log_tbl,encoded_log,file=e$fichier)
    demande<-paste0("Votre cl\xE9, est sauv\xE9 dans le fichier ",e$fichier," Tapez sur la touche Entr\xE9e pour continuer.")
    Encoding(demande) <- "latin1"
    rep <- readline(demande)
    return(FALSE)
  }
}
}

#answear test to known if the value of the answear is between b_inf and b_sup
test_between <- function(b_inf,b_sup){
  n<-length(b_inf)
  res<-TRUE
  e <- get("e", parent.frame())
  e<-e$val
  for(i in 1:n){
    res<-res&(e[i] >= b_inf[i])&(e[i] <= b_sup[i])
  }
  return(res)
}

ouvrir_sujet_TP<-function(){
  e <- get("e", parent.frame())
  selection <- getState()$val
  res<-FALSE
  if(selection == "Oui"){
    browseURL(e$vs$pageWebsujet)
    res<-TRUE
  }
  return(res)
}

test_passer<-function(){
  e <- get("e", parent.frame())
  res<-(e$expr=="passer()")
  if(length(e$log$mon_skip)>0) e$log$skipped[1:length(e$log$mon_skip)]<-e$log$mon_skip
  e$log$mon_skip<-e$log$skipped
  e$log$mon_skip[length(e$log$mon_skip)+1]<-res
  return(res)
}
