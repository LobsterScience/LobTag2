
#' @title  lobster.letters
#' @description  create reward letters for fishers who report tags, letters contain map and text.
#' @import ROracle DBI stringi lubridate dplyr sf svDialogs rmarkdown
#' @export
lobster.letters = function(people = NULL, db = "Oracle", only.unrewarded = T, output.location = NULL,
                           oracle.user = oracle.personal.user,
                           oracle.password = oracle.personal.password,
                           oracle.dbname = oracle.personal.server){

  ## let user select output file location for maps
  if(is.null(output.location)){
    dlg_message("In the following window, choose the directory where you want to keep your letters.")
    output.location <- dlg_dir(filter = dlg_filters["csv",])$res
  }

  dir.create(paste0(output.location,"/maps"), showWarnings = F)
  dir.create(paste0(output.location,"/letters"), showWarnings = F)
  letter_path = paste0(output.location,"/letters/")
  markdownfilepath = system.file("data","knit_rewards.Rmd", package = "LobTag2")


  ## letter building function pulled from LobTag
  #' @title rewards.letter.fill
  #' @description Letter text for rewards markdown
  #' @export
  rewards.letter.fill = function(){

    lettertxt = "<PARAGRAPH addresslabel>
  <PARAGRAPH A>Lobster Ecology & Assessment Team
  DFO Science
  Dartmouth, NS</PARAGRAPH A>
  <PARAGRAPH mytagcapturedbutihavenoreturns>  <name>,
  \\newline
  Thank you for participating in the lobster tagging program. I thought you would be interested to know that a lobster you had previously released was captured again and information sent in. This information is shown in a chart provided. Recaptures such as these will help us better track the movement of lobsters across the Scotian shelf.</PARAGRAPH mytagcapturedbutihavenoreturns>
  \\newline
  <PARAGRAPH B><name>,
  \\newline
  \\newline
  Thanks for reporting the lobster <tag/tags> caught last season. A chart showing the release and all recapture positions for the <tag/tags> is included. The information provided by tagging recaptures is helpful in determining the movement of lobster throughout the Scotian Shelf.</PARAGRAPH B>
  \\newline
  <PARAGRAPH info>
  \\newline
  \\newline
  The tagged lobster you caught <was/were/wereall> tagged in the <yeartagged/yearstagged/season/seasons>.</PARAGRAPH info>
  <PARAGRAPH capturedbefore>
  \\newline
  <onebefore/somebefore> of the tagged lobster you caught <wasb/wereb> captured before and released.</PARAGRAPH capturedbefore>
  <PARAGRAPH capturedafter>
  \\newline
  \\newline
  <oneafter/someafter> of the tagged lobster you caught and released in the past <wasa/werea> captured this season.</PARAGRAPH capturedafter>
  <PARAGRAPH capturedbeforeandafter>
  \\newline
  <onebefore/somebefore> of the tagged lobster you caught <wasb/wereb> captured before and released. As well, <oneafter/someafter> of the tagged lobster you caught and released in the past <wasa/werea> captured this season.</PARAGRAPH capturedbeforeandafter>
  <PARAGRAPH consider>
  \\newline
  \\newline
  Letting us know if tagged lobsters are released or retained is helpful; additional knowledge will be gained by tracking subsequent recaptures of individual lobsters over their lifespan.</PARAGRAPH consider>
  <PARAGRAPH pickup>
  \\newline
  \\newline
  <community>
  </PARAGRAPH pickup>
  <PARAGRAPH notreleased>If you are interested in participating in tagging lobsters at the close of the commercial season in your area, please send an email to lobtags@gmail.com. </PARAGRAPH notreleased>
  <PARAGRAPH released>If you are interested in participating in tagging lobsters at the close of the commercial season in your area, please send an email to lobtags@gmail.com. </PARAGRAPH released>
  <PARAGRAPH mixedrelret>If you are interested in participating in tagging lobsters at the close of the commercial season in your area, please send an email to lobtags@gmail.com. </PARAGRAPH mixedrelret>
  <PARAGRAPH unknownrel>If you are interested in participating in tagging lobsters at the close of the commercial season in your area, please send an email to lobtags@gmail.com. </PARAGRAPH unknownrel>
  \\newline
  \\newline
  <PARAGRAPH final>
  \\newline
  \\newline
  I have included a one page information sheet on our tagging program. On the reverse side of this sheet is a form which can easily be used to record all required information on any tagged lobster you may catch in the future. This entire form can be sent to DFO Science at the end of the lobster season. </PARAGRAPH final>
  <PARAGRAPH end>
  \\newline
  \\newline
  Thanks for your help.
  \\newline
  \\newline
  Ben Zisserson
  \\newline
  (902) 222-5211
  \\newline
  Ben.Zisserson@dfo-mpo.gc.ca</PARAGRAPH end>
  <PARAGRAPH mapdisclaimer>
  * Lobster positions very near the coast may appear to be on land due to map resolution.</PARAGRAPH mapdisclaimer>"

    return(lettertxt)
  }

  ## letter building function pulled from LobTag
  getBetween = function(text = "", pattern = ""){
    text = substr(text, gregexpr(pattern, text)[[1]][1], gregexpr(pattern, text)[[1]][2]-1)
    text = substr(text, regexpr(">", text)[1]+1, regexpr("</", text)[1]-1)
    return(text)
  }


  # Letter text and formatting, be careful when changing text to be aware of return line formatting
  lettertxt = rewards.letter.fill()

  ### open db connection
  if(db %in% "Oracle"){
    tryCatch({
      drv <- DBI::dbDriver("Oracle")
      con <- ROracle::dbConnect(drv, username = oracle.user, password = oracle.password, dbname = oracle.dbname)
    }, warning = function(w) {
    }, error = function(e) {
      return(toJSON("Connection failed"))
    }, finally = {
    })
  }else{
    con <- dbConnect(RSQLite::SQLite(), "C:/LOBTAG/LOBTAG.db")
  }


  #initialize lobster databases
  reldb = "LBT_RELEASES"
  captdb = "LBT_RECAPTURES"
  peopdb = "LBT_PEOPLE"
  #pathdb = paste("LOBSTER",".","LBT_PATH", sep = "")

  query = paste("SELECT * FROM",captdb)
  capt.quer <- ROracle::dbSendQuery(con, query)
  captures <- ROracle::fetch(capt.quer)

  ## filter for only those that haven't yet received their reward
  if(only.unrewarded){
    captures <- captures %>% filter(REWARDED %in% "no")
  }

  ## for selective letter generation
  if(!is.null(people)){captures <- captures %>% filter(PERSON  %in% people)}

  cap.tags <- paste0("('",paste(captures$TAG_ID, collapse ="','"),"')")
  cap.names <- paste0("('",paste(captures$PERSON, collapse ="','"),"')")

  query = paste("SELECT * FROM",peopdb, "WHERE NAME IN",cap.names)
  peep.quer <- ROracle::dbSendQuery(con,query)
  peopda <- ROracle::fetch(peep.quer)

  query = paste("SELECT * FROM",reldb, "WHERE TAG_ID IN",cap.tags)
  rel.quer <- ROracle::dbSendQuery(con,query)
  relda <- ROracle::fetch(rel.quer)


  ROracle::dbDisconnect(con)

  ##### check which LFAs recaptures fall within by converting to SF objects
  cap_sf <- st_as_sf(captures, coords = c("LON_DD","LAT_DD"))
  st_crs(cap_sf) <- 4326

  #### For full polygons:
  LFA <- read.csv("C:/bio.data/bio.lobster/data/maps/LFAPolys.csv")
  LFA <- LFA %>% mutate(PID = ifelse(PID  %in% 311, "31A",
                                     ifelse(PID %in% 312, "31B",PID)))

  lfa_poly_list <- NULL
  for (k in unique(LFA$PID)){
    lfa <- LFA %>% filter(PID %in% k)
    sub.box_list <- NULL
    for(l in 1:max(lfa$SID)){
      if(nrow(lfa %>% filter(SID %in% l)>0)){
        sub.box <- lfa %>% filter(SID %in% l)
        sub.box <- arrange(sub.box,POS) %>% dplyr::select(-PID,-SID,-POS)
        sub.box <- rbind(sub.box,sub.box[1,])
        sub.box_list[[as.character(l)]] <- st_polygon(list(as.matrix(sub.box)))
      }
    }
    multipoly <- st_multipolygon(sub.box_list)
    lfa_poly_list[[k]] <- multipoly
  }


  lfa_sf <- st_sfc(lfa_poly_list, crs = 4326)
  lfa_sf <- st_sf(lfa_sf, LFA = names(lfa_sf))
  st_crs(lfa_sf) <- 4326
  lfa_sf <- lfa_sf %>% filter(!(LFA %in% 41))

  ## join points and polygons to check their association
  captures_sf <- st_join(cap_sf, lfa_sf, join = st_within)
  captures_sf <- captures_sf %>% mutate(MANAGEMENT_AREA = LFA)
  captures <- captures_sf
  ######

  captures <- captures %>% dplyr::select(TAG_ID,REC_DATE,MANAGEMENT_AREA,YEAR,RELEASED,REWARDED,PERSON) %>% rename(NAME = PERSON)
  peopda <- peopda %>% dplyr::select(NAME,EMAIL,CIVIC,TOWN,PROV,POST,LICENSE_AREA)
  relda <- relda %>% dplyr::select(TAG_ID,YEAR) %>% rename(YEAR1 = YEAR)

  da <- left_join(captures,peopda)
  da <- left_join(da,relda)

  ## LFA locations for captures are used to fill in missing LICENSE_AREA info for hat distribution (if we don't know where they're licensed to fish, we use where they've been fishing)
  da <- da %>% mutate(LICENSE_AREA = ifelse(LICENSE_AREA %in% c(NA,"NA"),MANAGEMENT_AREA,LICENSE_AREA))
  ## if any of the fishers still have NA for license area, end function and tell user they need to fix this to run letters
  no.area <- da %>% filter(LICENSE_AREA %in% c(NA,"NA"))
  if(nrow(no.area)>0){
    return(print(paste("Error: There are fishers in LBT_PEOPLE with no associated LICENSE_AREA and their captures can't be localized within an area. Update LICENSE_AREA for:",paste(no.area$NAME,collapse = ", "),"and try again.")))
  }

  tid = da$TAG_ID[which(da$REWARDED == 'no')] #Get tag ids of unrewarded returns
  da = da[which(da$TAG_ID %in% tid),] #Get all required data for plotting tag histories of unrewarded

  perlist = list() #set up list to hold relevant data
  persplit = split(da, da$NAME)

  for(i in 1:length(persplit)){
    per = list()
    perlist = list()
    per$data = persplit[[i]]
    per$name = per$data$NAME[1]
    per$email = per$data$EMAIL

          per$addresslabel = getBetween(lettertxt, "PARAGRAPH addresslabel" )
          per$paraA = getBetween(lettertxt, "PARAGRAPH A")
          per$paraB = getBetween(lettertxt, "PARAGRAPH B")
          per$paraB = sub("<name>", per$name, per$paraB)
          #new paragraph for hat pickup locations
          per$pickup = getBetween(lettertxt, "PARAGRAPH pickup")
          per$paraB = gsub("<tag/tags>", "tags", per$paraB)
          per$info = getBetween(lettertxt, "PARAGRAPH info" )
          if(length(unique(per$data$TAG_ID))>1){
            if(length(unique(per$data$YEAR1))>1){
              per$info = gsub("<was/were/wereall>", "were", per$info)
              per$info = gsub("<yeartagged/yearstagged/season/seasons>", paste(stri_reverse(sub(",", "dna ", stri_reverse(paste0(sort(unique(per$data$YEAR1)), collapse = ", ")))), " seasons", sep = ""), per$info)
            }else{
              per$info = gsub("<was/were/wereall>", "were all", per$info)
              per$info = gsub("<yeartagged/yearstagged/season/seasons>", paste(sort(unique(per$data$YEAR1)), " season", sep = ""), per$info)
            }
          }else{
            per$info = gsub("<was/were/wereall>", "was", per$info)
            per$info = gsub("<yeartagged/yearstagged/season/seasons>", paste(sort(unique(per$data$YEAR1)), " season", sep = ""), per$info)
          }

          capbefore = F
          capbeforegto = F
          capafter = F
          capaftergto = F
          cb = 0
          ca = 0
          utid = unique(per$data$TAG_ID)
          #k = 1
          for(k in 1:length(utid)){
            cb = max(cb, length(which(per$matcheddata$REC_DATE[which(per$matcheddata$TAG_ID == utid[k])] <  per$data$REC_DATE[which(per$data$TAG_ID == utid[k])])))
            if(cb > 0) capbefore = T
            ca = max(ca, length(which(per$matcheddata$REC_DATE[which(per$matcheddata$TAG_ID == utid[k])] >  per$data$REC_DATE[which(per$data$TAG_ID == utid[k])])))
            if(ca > 0) capafter = T
          }
          if(capbefore & !capafter){
            per$capbefore = getBetween(lettertxt, "PARAGRAPH capturedbefore" )
            if(cb == 1){
              per$capbefore = gsub("<onebefore/somebefore>", "One", per$capbefore)
              per$capbefore = gsub("<wasb/wereb>", "was", per$capbefore)
            }
            if(cb > 1){
              per$capbefore = gsub("<onebefore/somebefore>", "Some", per$capbefore)
              per$capbefore = gsub("<wasb/wereb>", "were", per$capbefore)
            }
          }else{
            per$capbefore = ""
          }
          if(!capbefore & capafter){
            per$capafter = getBetween(lettertxt, "PARAGRAPH capturedafter" )
            if(ca == 1){
              per$capafter = gsub("<oneafter/someafter>", "One", per$capafter)
              per$capafter = gsub("<wasa/werea>", "was", per$capafter)
            }
            if(ca > 1){
              per$capafter = gsub("<oneafter/someafter>", "Some", per$capafter)
              per$capafter = gsub("<wasa/werea>", "were", per$capafter)
            }
          }else{
            per$capafter = ""
          }
          if(capbefore & capafter){
            per$capturedbeforeandafter = getBetween(lettertxt, "PARAGRAPH capturedbeforeandafter" )
            if(ca == 1){
              per$capturedbeforeandafter = gsub("<oneafter/someafter>", "one", per$capturedbeforeandafter)
              per$capturedbeforeandafter = gsub("<wasa/werea>", "was", per$capturedbeforeandafter)
            }
            if(ca > 1){
              per$capturedbeforeandafter = gsub("<oneafter/someafter>", "some", per$capturedbeforeandafter)
              per$capturedbeforeandafter = gsub("<wasa/werea>", "were", per$capturedbeforeandafter)
            }
            if(cb == 1){
              per$capturedbeforeandafter = gsub("<onebefore/somebefore>", "One", per$capturedbeforeandafter)
              per$capturedbeforeandafter = gsub("<wasb/wereb>", "was", per$capturedbeforeandafter)
            }
            if(cb > 1){
              per$capturedbeforeandafter = gsub("<onebefore/somebefore>", "Some", per$capturedbeforeandafter)
              per$capturedbeforeandaftere = gsub("<wasb/wereb>", "were", per$capturedbeforeandafter)
            }
          }else{
            per$capturedbeforeandafter = ""
          }
          if(all(per$data$RELEASED  %in%  c("1","yes"))){
            per$released = getBetween(lettertxt, "PARAGRAPH released" )
          }else{
            per$released = ""
          }
          if(all(per$data$RELEASED %in% c("2","no"))){
            per$notreleased = getBetween(lettertxt, "PARAGRAPH notreleased" )
          }else{
            per$notreleased = ""
          }
          if(all(per$data$RELEASED  %in% c("3","unknown"))){
            per$unknownrel = getBetween(lettertxt, "PARAGRAPH unknownrel" )
          }else{
            #per$notreleased = ""
            per$unknownrel = ""
          }
          #if( per$released == "" & per$notreleased == "" & per$notreleased == ""){
          if(per$released == "" & per$notreleased == "" & per$unknownrel == ""){
            per$mixedrelret = getBetween(lettertxt, "PARAGRAPH mixedrelret" )
          }else{
            per$mixedrelret = ""
          }

          #if(is.null(per$data$LICENSE_AREA)){}
          if(all(per$data$LICENSE_AREA == "27")){
            per$pickup = gsub("<community>", "As a token of our appreciation for your participation in the lobster tagging program, we have a Lobster Science hat for you. It can be picked up at the office for the Cape Breton Fish Harvesters at your convenience.", per$pickup)
          } else if(all(per$data$LICENSE_AREA %in% c("LFA28", "LFA29", "LFA30","28","29","30"))){
            per$pickup = gsub("<community>", "As a token of our appreciation for your participation in the lobster tagging program, we have a Lobster Science hat for you. It can be picked up at the office for the Richmond County Inshore Fishermen's Association at your convenience.", per$pickup)
          } else if(all(per$data$LICENSE_AREA %in% c("LFA31A","31A"))){
            per$pickup = gsub("<community>", "As a token of our appreciation for your participation in the lobster tagging program, we have a Lobster Science hat for you. It can be picked up at the office for the Guysborough County Inshore Fishermen's Association at your convenience. ", per$pickup)
          } else if(all(per$data$LICENSE_AREA  %in% c("LFA31B","31B"))){
            per$pickup = gsub("<community>", "As a token of our appreciation for your participation in the lobster tagging program, we have a Lobster Science hat for you. It can be picked up at the office for your respective association (Eastern Shore Fisherman's Protective Association or Guysborough County Inshore Fishermen's Association) at your convenience.", per$pickup)
          } else if(all(per$data$LICENSE_AREA %in% c("LFA32","32"))){
            per$pickup = gsub("<community>", "As a token of our appreciation for your participation in the lobster tagging program, we have a Lobster Science hat for you. It can be picked up at the office for the Eastern Shore Fisherman's Protective Association at your convenience.", per$pickup)
          }


        per$consider = getBetween(lettertxt, "PARAGRAPH consider")
        per$final = getBetween(lettertxt, "PARAGRAPH final" )
        per$end = getBetween(lettertxt, "PARAGRAPH end" )
        #per$mapdisclaimer = getBetween(lettertxt, "PARAGRAPH mapdisclaimer")   ### not needed anymore once we got better maps


        generate_maps(people = per$name, only.unrewarded = T, output.location = paste0(output.location,"/maps"), db = db, inset.option = F)

        charts <- list.files(path = paste0(output.location,"/maps"), pattern = per$name)
        c <- c()
        for(j in 1:length(charts)){
        c <- c(c,paste0(output.location,"/maps/",charts[j]))
        }
        per$charts <- c
          #perlist[[length(perlist)+1]] = per
        perlist[[i]] = per

        pdf_file_name = paste(letter_path, per$name, ".pdf", sep = "")

          #render function needs TinyTex installed to run properly. Check if installed and install if not
          if(tinytex::is_tinytex() %in% FALSE){tinytex::install_tinytex(force = TRUE)}

          rmarkdown::render(input = markdownfilepath,
                            output_format = 'pdf_document',
                            output_file = pdf_file_name,
                            params = list(i = i, perlist = perlist))

          print("done")


  }

  #   library(ROracle)
  #   library(DBI)
  #   library(stringi)
  #   library(lubridate)
  #   library(dplyr)
  #   library(sf)
  #   library(svDialogs)
  # library(rmarkdown)



  ####SCRAP
  # for(k in 1:length(perlist)){
  #
  #
  #   pdf_file_name = paste(letter_path, perlist[[k]]$name, ".pdf", sep = "")
  #
  #   #render function needs TinyTex installed to run properly. Check if installed and install if not
  #   if(tinytex::is_tinytex() %in% FALSE){tinytex::install_tinytex(force = TRUE)}
  #
  #   rmarkdown::render(input = markdownfilepath,
  #                     output_format = 'pdf_document',
  #                     output_file = pdf_file_name,
  #                     params = list(i = k, perlist = perlist))
  #
  #   print("done")
  #   #return(perlist[i])
  # }


  }











