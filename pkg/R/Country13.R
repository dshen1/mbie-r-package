Country13 <- function(x, shorten=TRUE){
  # takes a detailed vector of countries and returns them grouped into 13 categories as used in IVS2
  
  # create all the various groupings used for UK, Rest of Europe, Rest of Asia, etc 
    
  # for debugging
  # x <- IVStrips$COPRDetail
  if(is.character(x)){x <- factor(x)}
  if(!is.factor(x)){stop("x must be a vector of countries, of class character or factor")}
  
  x2 <- as.character(x)
  x2[grep("d'Ivoire", x2)] <- "Cote d'Ivoire"
  x <- factor(x2)
  
  x <- rename.levels(x, 
						 orig=c("Norfolk Island", "Not Stated", "Unknown"),
						 new=rep("Australia", 3))
  
  x <- rename.levels(x, 
						 orig=c("UK", "England", "Scotland", "Wales", "Northern Ireland", "United Kingdom (not further defined)", "Channel Islands", "Isle of Man", "Falkland Islands"),
						 new=rep("United Kingdom",9))

  x <- rename.levels(x, 
						 orig=c("Netherlands", "France", "Switzerland", "Sweden", "Italy", "Denmark", "Belgium", "Spain", 
								"Austria", "Ireland", "Hungary", "Portugal", "Russia", "Andorra", "Greece", "Cyprus", "Lithuania", "Norway", 
								"Romania", "Luxembourg", "Finland", "Czech Republic", "Poland", "Iceland", 
								"Montenegro", "Croatia", "Slovenia", "Armenia", "Ukraine", "Gibraltar", "Latvia",
								"Estonia", "Greenland", "Liechtenstein", "Serbia", "Bulgaria", "Latvia", "Monaco", "Serbia and Montenegro",
								"Moldova", "Malta", "Bosnia and Herzegovina", "Slovakia", "Belarus", 
						        "Faeroe Islands", "Vatican City State", "San Marino", "Albania",
						        "Former Yugoslav Republic of Macedonia (F", "Moldova, Republic of", "Russian Federation", 
						        "Holy See (Vatican City State)", "Macedonia, the former Yugoslav Republic of", 
						        "United Nations Interim Administration in Kosovo"),
						 new=rep("Rest of Europe", 54))
    
  x <- rename.levels(x, 
						 orig=c("Singapore", "Malaysia", "Thailand", "India", "Hong Kong (Special Administrative Region)", 
								"Taiwan", "Philippines", "Sri Lanka", "Pakistan", "Indonesia", "Viet Nam", "Mongolia", 
								"Timor-Leste", "Bhutan", "Nepal", "Cambodia", "Laos", "Bangladesh", 
								"Uzbekistan", "Kyrgyzstan", "Kazakhstan", 
								"Korea, Democratic People's Republic of", "Myanmar", "Macau (Special Administrative Region)",
                "Macao", "Hong Kong", "Taiwan, Province of China", "Tajikistan", "Turkmenistan", 
                "Lao People's Democratic Republic", "Azerbaijan", "Georgia"),
						 new=rep("Rest of Asia", 32))
  
  x <- rename.levels(x, 
						 orig=c("NZ", "New Caledonia", "Tonga", "Fiji", "Cook Islands", "Samoa", "Niue", 
								"French Polynesia", "Vanuatu", "Papua New Guinea", "Solomon Islands", "Guam", 
								"Tokelau", "Samoa, American", "Antarctica", "Kiribati", "Palau", "Tuvalu", "Micronesia, Federated States of",
								"Marshall Islands", "Rest of Oceania", "Northern Mariana Islands", "Nauru", "Wallis and Futuna", "Not Stated"),
						 new=rep("Rest of Oceania", 25))
  
  x <- rename.levels(x, 
						 orig=c("South Africa", "United Arab Emirates", "Afghanistan", "Oman", "Turkey", "Bahrain", "Kenya", "Reunion",     
								"Botswana", "Saudi Arabia", "Kuwait", "Israel", "Ghana", "Qatar", "Maldives", "Brunei Darussalam",
								"Zambia", "Iraq", "Zimbabwe", "Nigeria", "Swaziland", "Sudan", "Cameroon", "Lebanon", "Namibia",
								"Mauritius", "Congo", "Malawi", "Madagascar", "Libya", "Iran", "Cayman Islands", "Lesotho", "Tunisia",
								"Angoloa", "Somalia", "Tanzania", "Morocco", "Yemen", "Mauritania", "Liberia", "Sierra Leone", "Jordan",
								"Cape Verde", "Uganda", "Egypt", "Mali", "Cote d'Ivoire", "Gabon", "Eritrea", "Angola", "Mozambique",
								"Seychelles", "Trinidad and Tobago", "Ethiopia", "Algeria", "Western Sahara", "Gaza Strip/Palestine/West Bank", "Syria", "Benin", "Burkina Faso", 
						        "Central African Republic", "Chad", "Congo, the Democratic Republic of the", "Equatorial Guinea", 
						        "Gambia", "Guinea-Bissau", "Niger", "Sao Tome and Principe", "Senegal", "Togo", "Burundi", 
						        "Comoros", "Djibouti", "Mayotte", "Syrian Arab Republic", "Tanzania, United Republic of", 
						        "Palestinian Territories, Occupied", "Cote d'Ivoire", "Middle East", "Rest of Africa",
                    "Guinea", "Rwanda"),
						 new=rep("Africa and Middle East", 83))
  
  x <- rename.levels(x, 
						 orig=c("Argentina", "Chile", "Brazil", "Mexico", "Ecuador", "Costa Rica", "Jamaica", "Panama", 
								"Colombia", "Uruguay", "Peru", "Paraguay", "Bermuda", "Barbados", "St Lucia",
								"Antigua and Barbuda", "Guatemala", "Dominican Republic", "Venezuela", "Puerto Rico", "Bolivia",
								"Bahamas", "Martinique", "St Vincent and the Grenadines", "Turks and Caicos Islands",
								"Guadeloupe", "Guyana", "Dominica", "St Helena", "Belize", "Cuba", "Aruba", "Virgin Islands, British",
								"Netherlands Antilles", "Trinidad and Tobago", "Honduras", "St Pierre and Miquelon", "French Guiana",
                    "Suriname", "El Salvador", "Nicaragua", "Grenada", "Haiti", "Montserrat", "St Kitts and Nevis",
                    "Anguilla", "Saint Vincent and the Grenadines", "Curacao", "Saint Kitts and Nevis",
                    "Bolivia, Plurinational State of", "Saint Lucia", "Venezuela, Bolivarian Republic of"),
						 new=rep("Rest of Americas", 52))
  
  x <- rename.levels(x, 
                     orig="China",
                     new="China, People's Republic of")
  
  x <- rename.levels(x, 
                     orig=c("United States", "Virgin Islands, U.S.", "Virgin Islands, United States"),
                     new=rep("United States of America", 3))
  
  Countries <- c("Australia", "China, People's Republic of", "Germany", "United States of America", 
                 "Japan", "Korea, Republic of", "Canada", "United Kingdom", "Rest of Europe", "Rest of Asia", "Rest of Oceania", 
                 "Africa and Middle East", "Rest of Americas")
  
  x <- (as.factor(ifelse(x %in% Countries, as.character(x), "Other")))
  if(shorten){
    x<- rename.levels(x, 
                      orig=c("China, People's Republic of", "United States of America", "United Kingdom"),
                      new =c("China", "USA", "UK"))
    }
  
  return(x)
  
}