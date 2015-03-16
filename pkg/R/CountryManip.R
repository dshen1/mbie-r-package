OneChina <- function(x){
  x <- rename.levels(x, 
                     orig=c("Macau (Special Administrative Region)", "Macao", "Hong Kong", 
                     "Hong Kong (Special Administrative Region)"),
                     new=rep("China, People's Republic of", 4))
}

##============ start of CountryGroup function =============
##==========================================================
CountryGroup <- function(x, shorten=TRUE, type="IVSweights", OneChina_first=FALSE){
  # takes a detailed vector of countries and returns them grouped into 13 categories as used in IVS2
    # create all the various groupings used for UK, Rest of Europe, Rest of Asia, etc 
  
  # for debugging
  # x <- IVStrips$COPRDetail
  # type <- "TNZ2013"
  # OneChina_first <- FALSE
  # shorten <- TRUE
  if (!type %in% c("IVSweights", "TNZ2013", "IVSweights_old")) {
    stop("type must be 'IVSweights' or 'TNZ2013' or 'IVSweights_old'")
  }
  
  ##=========General cleanup================
  
  if(is.character(x)){x <- factor(x)}
  if(!is.factor(x)){stop("x must be a vector of countries, of class character or factor")}
  
  if(OneChina_first){
    x <- OneChina(x)
  }
  
  x <- rename.levels(x,
                     orig=c("AUSTRALIA", "CANADA", "GREECE", "ITALY"),
                     new = c("Australia", "Canada", "Greece", "Italy"))
  
  x <- rename.levels(x,
                     orig = c("Bosnia And Herzegovina", "Isle Of Man", "Trinidad And Tobago", 
                              "Turks And Caicos Islands", "United States Of America", "Wallis And Futuna"),
                     new = c("Bosnia and Herzegovina", "Isle of Man", "Trinidad and Tobago", 
                             "Turks and Caicos Islands", "United States of America", "Wallis and Futuna"))
  
  
  x2 <- as.character(x)
  x2[grep("d'Ivoire", x2)] <- "Cote d'Ivoire"
  x <- factor(x2)
  
  x <- rename.levels(x, 
						 orig=c("Norfolk Island", "Unknown", "Cocos (Keeling) Islands", "Christmas Island"),
						 new=rep("Australia", 4))
  
  x <- rename.levels(x, 
						 orig=c("UK", "England", "Scotland", "Wales", "Northern Ireland", "United Kingdom (not further defined)", 
                           "Channel Islands", "Isle of Man", "Pitcairn", "British Indian Ocean Territory"),
						 new=rep("United Kingdom",10))

  x <- rename.levels(x,
                     orig="St Maarten (Dutch Part)",
                     new="Netherlands")
  
  x <- rename.levels(x, 
                     orig="China",
                     new="China, People's Republic of")
  
  x <- rename.levels(x, 
                     orig=c("United States", "Virgin Islands, U.S.", "Virgin Islands, United States", "United States Minor Outlying Islands"),
                     new=rep("United States of America", 4))
  
  
  
  x <- rename.levels(x,
                     orig = "Burma (Myanmar)",
                     new = "Myanmar")
  
  ##============ IVSweights =============
  
  if(type=="IVSweights"){

    x <- rename.levels(x, 
                     orig = c("Not Stated"), 
                     new = rep("Australia", 1))
  
    x <- rename.levels(x, 
  						 orig=c("Netherlands", "France", "Switzerland", "Sweden", "Italy", "Denmark", "Belgium", "Spain", 
  								"Austria", "Ireland", "Hungary", "Portugal", "Russia", "Andorra", "Greece", "Cyprus", "Lithuania", "Norway", 
  								"Romania", "Luxembourg", "Finland", "Czech Republic", "Poland", "Iceland", "Czechoslovakia",
  								"Montenegro", "Croatia", "Slovenia", "Ukraine", "Gibraltar", "Latvia",
  								"Estonia", "Greenland", "Liechtenstein", "Serbia", "Bulgaria", "Latvia", "Monaco", "Serbia and Montenegro",
  								"Moldova", "Malta", "Bosnia and Herzegovina", "Slovakia", "Belarus", 
  						        "Faeroe Islands", "Vatican City State", "San Marino", "Albania",
  						        "Former Yugoslav Republic of Macedonia (F", "Moldova, Republic of", "Russian Federation", 
  						        "Holy See (Vatican City State)", "Macedonia, the former Yugoslav Republic of", 
  						        "United Nations Interim Administration in Kosovo", "Germany, Democratic Republic of", "Kosovo",
                          "Union of Soviet Socialist Republics", "Yugoslavia/Serbia and Montenegro", "Former Yugoslav Republic of Macedonia (FYROM)"),
  						 new=rep("Rest of Europe", 59))
      
    x <- rename.levels(x, 
  						 orig=c("Singapore", "Malaysia", "Thailand", "India", "Hong Kong (Special Administrative Region)", 
  								"Taiwan", "Philippines", "Sri Lanka", "Pakistan", "Indonesia", "Viet Nam", "Mongolia", 
  								"Timor-Leste", "Bhutan", "Nepal", "Cambodia", "Laos", "Bangladesh", 
  								"Uzbekistan", "Kyrgyzstan", "Kazakhstan", 
  								"Korea, Democratic People's Republic of", "Myanmar", "Macau (Special Administrative Region)",
                  "Macao", "Hong Kong", "Taiwan, Province of China", "Tajikistan", "Turkmenistan", 
                  "Lao People's Democratic Republic", "Azerbaijan", "Georgia", "Brunei Darussalam", "Afghanistan", "Maldives", "Armenia"),
  						 new=rep("Rest of Asia", 36))
    
    x <- rename.levels(x, 
  						 orig=c("NZ", "New Caledonia", "Tonga", "Fiji", "Cook Islands", "Samoa", "Niue", 
  								"French Polynesia", "Vanuatu", "Papua New Guinea", "Solomon Islands", "Guam", 
  								"Tokelau", "Samoa, American", "Antarctica", "Kiribati", "Palau", "Tuvalu", "Micronesia, Federated States of",
  								"Marshall Islands", "Rest of Oceania", "Northern Mariana Islands", "Nauru", "Wallis and Futuna", "Not Stated", 
                          "French Southern Territories", "South Georgia and the South Sandwich Islands"),
  						 new=rep("Rest of Oceania", 27))
    
    x <- rename.levels(x, 
  						 orig=c("South Africa", "United Arab Emirates", "Oman", "Turkey", "Bahrain", "Kenya", "Reunion",     
  								"Botswana", "Saudi Arabia", "Kuwait", "Israel", "Ghana", "Qatar", "Zambia", "Iraq", "Zimbabwe", "Nigeria", 
                        "Swaziland", "Sudan", "Cameroon", "Lebanon", "Namibia",
  								"Mauritius", "Congo", "Malawi", "Madagascar", "Libya", "Iran", "Lesotho", "Tunisia",
  								"Angoloa", "Somalia", "Tanzania", "Morocco", "Yemen", "Mauritania", "Liberia", "Sierra Leone", "Jordan",
  								"Cape Verde", "Uganda", "Egypt", "Mali", "Cote d'Ivoire", "Gabon", "Eritrea", "Angola", "Mozambique",
  								"Seychelles", "Ethiopia", "Algeria", "Western Sahara", "Gaza Strip/Palestine/West Bank", "Syria", "Benin", "Burkina Faso", 
  						        "Central African Republic", "Chad", "Congo, the Democratic Republic of the", "Equatorial Guinea", 
  						        "Gambia", "Guinea-Bissau", "Niger", "Sao Tome and Principe", "Senegal", "Togo", "Burundi", 
  						        "Comoros", "Djibouti", "Mayotte", "Syrian Arab Republic", "Tanzania, United Republic of", 
  						        "Palestinian Territories, Occupied", "Cote d'Ivoire", "Middle East", "Rest of Africa",
                      "Guinea", "Rwanda", "St Helena", "Cabo Verde", "South Sudan", "Yemen, Democratic"),
  						 new=rep("Africa and Middle East", 82))
    
    x <- rename.levels(x, 
  						 orig=c("Argentina", "Chile", "Brazil", "Mexico", "Ecuador", "Costa Rica", "Jamaica", "Panama", "Cayman Islands",
  								"Colombia", "Uruguay", "Peru", "Paraguay", "Bermuda", "Barbados", "St Lucia",
  								"Antigua and Barbuda", "Guatemala", "Dominican Republic", "Venezuela", "Puerto Rico", "Bolivia",
  								"Bahamas", "Martinique", "St Vincent and the Grenadines", "Turks and Caicos Islands",
  								"Guadeloupe", "Guyana", "Dominica", "Belize", "Cuba", "Aruba", "Virgin Islands, British",
  								"Netherlands Antilles", "Trinidad and Tobago", "Honduras", "St Pierre and Miquelon", "French Guiana",
                      "Suriname", "El Salvador", "Nicaragua", "Grenada", "Haiti", "Montserrat", "St Kitts and Nevis",
                      "Anguilla", "Saint Vincent and the Grenadines", "Curacao", "Saint Kitts and Nevis",
                      "Bolivia, Plurinational State of", "Saint Lucia", "Venezuela, Bolivarian Republic of", "Falkland Islands"),
  						 new=rep("Rest of Americas", 53))
    
    Countries <- c("Australia", "China, People's Republic of", "Germany", "United States of America", 
                   "Japan", "Korea, Republic of", "Canada", "United Kingdom", "Rest of Europe", "Rest of Asia", "Rest of Oceania", 
                   "Africa and Middle East", "Rest of Americas")
  
  } 
##============ IVSweights_old =============

  if (type == "IVSweights_old") {
        ##
        ## note that the old COPR14 incorrectly classifies Hungary	Poland	Russia	Lithuania	Armenia	Ukraine	Estonia	Latvia	Slovakia
        ## as other countries, they should be other Europe
        ## note that the old COPR14 incorrectly classifies Uzbekistan, Kyrgyzstan, Kazakhstan, they should be other Asia
        
   x <- rename.levels(x, 
                  orig = c("Not Stated"), 
                  new = rep("Other Countries", 1))
                  

   x <- rename.levels(x, 
                  orig = c("Falkland Islands"), 
                  new = rep("United Kingdom", 1))
                  
   x <- rename.levels(x, 
                  orig = c("Netherlands", "France", "Switzerland", "Sweden", "Italy", "Denmark", "Belgium", "Czechoslovakia",
                           "Spain", "Austria", "Ireland",  "Portugal", "Andorra", "Greece", "Cyprus",  
                           "Norway", "Romania", "Luxembourg", "Finland", "Czech Republic", "Iceland", "Montenegro", "Croatia", "Slovenia", 
                           "Gibraltar","Greenland", "Liechtenstein", "Serbia", "Bulgaria", "Monaco", "Serbia and Montenegro", "Moldova", 
                           "Malta", "Bosnia and Herzegovina",  "Belarus", "Faeroe Islands", "Vatican City State", "San Marino", 
                           "Albania", "Former Yugoslav Republic of Macedonia (F", "Moldova, Republic of", "Russian Federation", "Holy See (Vatican City State)", 
                           "Macedonia, the former Yugoslav Republic of", "United Nations Interim Administration in Kosovo",
                           "Germany, Democratic Republic of", "Kosovo", "Union of Soviet Socialist Republics", "Yugoslavia/Serbia and Montenegro",
                           "Former Yugoslav Republic of Macedonia (FYROM)"), 
                  new = rep("Other Europe", 50))
                  
   x <- rename.levels(x, 
                  orig = c("Malaysia", "Thailand", "India", "Philippines", "Sri Lanka", "Pakistan", "Indonesia", "Viet Nam", "Mongolia", "Timor-Leste", 
                           "Bhutan", "Nepal", "Cambodia", "Laos", "Bangladesh", "Korea, Democratic People's Republic of", "Myanmar", "Macau (Special Administrative Region)", 
                           "Macao", "Hong Kong", "Taiwan, Province of China", "Tajikistan", "Turkmenistan", "Lao People's Democratic Republic", "Azerbaijan", "Georgia",
                          "Brunei Darussalam", "Maldives", "Afghanistan" ), 
                  new = rep("Other Asia", 29))
                  
   x <- rename.levels(x, 
                  orig = c("NZ", "New Caledonia", "Tonga", "Fiji", "Cook Islands", "Samoa", "Niue", "French Polynesia", "Vanuatu", "Papua New Guinea", 
                           "Solomon Islands", "Guam", "Tokelau", "Samoa, American", "Antarctica", "Kiribati", "Palau", "Tuvalu", "Micronesia, Federated States of", 
                           "Marshall Islands", "Rest of Oceania", "Northern Mariana Islands", "Nauru", "Wallis and Futuna", "Not Stated",
                           "French Southern Territories", "South Georgia and the South Sandwich Islands",
                           # Incorrect European countries
                           "Hungary", "Lithuania", "Russia", "Latvia", "Slovakia", "Armenia", "Ukraine", "Poland", "Estonia","Kazakhstan", "Kyrgyzstan", "Uzbekistan"), 
                  new = rep("Other Countries", 38))
                  
   x <- rename.levels(x, 
                     orig = c("South Africa", "United Arab Emirates", "Oman", "Turkey", "Bahrain", "Kenya", "Reunion", "Botswana", "Saudi Arabia", "Kuwait", 
                                 "Israel", "Ghana", "Qatar", "Zambia", "Iraq", "Zimbabwe", "Nigeria", "Swaziland", "Sudan", "Cameroon", "Lebanon", "Namibia", "Mauritius", "Congo", 
                                 "Malawi", "Madagascar", "Libya", "Iran", "Lesotho", "Tunisia", "Angoloa", "Somalia", "Tanzania", "Morocco", "Yemen", "Mauritania", "Liberia", "Sierra Leone", 
                                 "Jordan", "Cape Verde", "Uganda", "Egypt", "Mali", "Cote d'Ivoire", "Gabon", "Eritrea", "Angola", "Mozambique", "Seychelles", "Ethiopia", "Algeria", "Western Sahara", 
                                 "Gaza Strip/Palestine/West Bank", "Syria", "Benin", "Burkina Faso", "Central African Republic", "Chad", "Congo, the Democratic Republic of the", "Equatorial Guinea", 
                                 "Gambia", "Guinea-Bissau", "Niger", "Sao Tome and Principe", "Senegal", "Togo", "Burundi", "Comoros", "Djibouti", "Mayotte", "Syrian Arab Republic", 
                                 "Tanzania, United Republic of", "Palestinian Territories, Occupied", "Cote d'Ivoire", "Middle East", 
                              "Rest of Africa", "Guinea", "Rwanda", "Cabo Verde", "South Sudan", "Yemen, Democratic"), 
                     new = rep("Other Countries", 81))
                     
   x <- rename.levels(x, 
                     orig = c("Argentina", "Chile", "Brazil", "Mexico", "Ecuador", "Costa Rica", "Jamaica", "Panama", "Cayman Islands", "Colombia", "Uruguay", "Peru", "Paraguay", 
                              "Bermuda", "Barbados", "St Lucia", "Antigua and Barbuda", "Guatemala", "Dominican Republic", "Venezuela", "Puerto Rico", "Bolivia", "Bahamas", "Martinique", 
                              "St Vincent and the Grenadines", "Turks and Caicos Islands", "Guadeloupe", "Guyana", "Dominica", "St Helena", "Belize", "Cuba", "Aruba", "Virgin Islands, British", 
                              "Netherlands Antilles", "Trinidad and Tobago", "Honduras", "St Pierre and Miquelon", "French Guiana", "Suriname", "El Salvador", "Nicaragua", "Grenada", "Haiti", "Montserrat", 
                              "St Kitts and Nevis", "Anguilla", "Saint Vincent and the Grenadines", "Curacao", "Saint Kitts and Nevis", "Bolivia, Plurinational State of", "Saint Lucia", 
                              "Venezuela, Bolivarian Republic of"), 
                     new = rep("Other Countries", 53))
                     
   Countries <- c("Australia", "China, People's Republic of", "Germany", "United States of America", "Hong Kong (Special Administrative Region)", 
               "Japan", "Singapore", "Taiwan", "Korea, Republic of", "Canada", "United Kingdom", "Other Europe", "Other Asia", "Other Countries")
    }
   
    

    
    ##============ TNZ2013 =============
    
    
    if(type=="TNZ2013"){  
 
    x<- rename.levels(x, 
                orig=c("Mexico", "Argentina", "Brazil", "Chile"),
                new=rep("Latin America", 4))

    
    x<- rename.levels(x, 
                orig=c("Singapore", "Thailand", "Malaysia"),
                new=rep("Pen. South East Asia", 3))

    Countries <- c("Australia", "China, People's Republic of", "Germany", "United States of America", 
                     "Japan", "Korea, Republic of", "Canada", "United Kingdom", "Indonesia","India",
                      "Latin America", "Pen. South East Asia", "France")
   }
   
   
   
   ##==================final processing==========================
  x <- (as.factor(ifelse(x %in% Countries, as.character(x), "Other")))

  if(shorten){
    x<- rename.levels(x, 
                      orig=c("China, People's Republic of", "United States of America", "United Kingdom"),
                      new =c("China", "USA", "UK"))
    }
  
  return(x)
  
}





##============ end of CountryGroup function =============
##==========================================================