# Quarantine periods and income status

ResidenceCases$high_income=3

ResidenceCases$quarantine = 0
ResidenceCases$quarantine[ResidenceCases$comuna.x=="Santiago" & 
                            ResidenceCases$day>=10 &
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Santiago"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Nunoa" & 
                            ResidenceCases$day>=10 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="Nunoa"] = 1

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Independencia" & 
                            ((ResidenceCases$day>=10 & 
                                ResidenceCases$day<=18) | ResidenceCases$day>=39)] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Independencia"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Las Condes" & 
                            ResidenceCases$day>=10 &
                            ResidenceCases$day<=32] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Las Condes"] = 1

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Vitacura" & 
                            ResidenceCases$day>=10 &
                            ResidenceCases$day<=29] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Vitacura"] = 1

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Lo Barnechea" & 
                            ResidenceCases$day>=10 &
                            ResidenceCases$day<=29] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Lo Barnechea"] = 1

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Providencia" & 
                            ResidenceCases$day>=10 &
                            ResidenceCases$day<=29] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Providencia"] = 1

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Puente Alto" & 
                            ResidenceCases$day>=23 &
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Puente Alto"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="El Bosque" & 
                            ResidenceCases$day>=32 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="El Bosque"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="San Bernardo" & 
                            ResidenceCases$day>=32 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="San Bernardo"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Quinta Normal" & 
                            ResidenceCases$day>=39 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Quinta Normal"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Pedro Aguirre Cerda" & 
                            ResidenceCases$day>=39 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Pedro Aguirre Cerda"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="La Pintana" & 
                            ResidenceCases$day>=46 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="La Pintana"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.y=="San Ramon" & 
                            ResidenceCases$day>=46 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="San Ramon"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Estacion Central" & 
                            ResidenceCases$day>=46 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="Estacion Central"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Cerrillos" & 
                            ResidenceCases$day>=51 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Cerrillos"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Quilicura" & 
                            ResidenceCases$day>=51 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Quilicura"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Recoleta" & 
                            ResidenceCases$day>=51 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Recoleta"] = 0

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Tortel" & 
                            ResidenceCases$day>=0 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Tortel"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Puerto Williams" & 
                            ResidenceCases$day>=10 & 
                            ResidenceCases$day<=22] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Puerto Williams"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Temuco" & 
                            ResidenceCases$day>=13 & 
                            ResidenceCases$day<=46] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Temuco"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Padre Las Casas" & 
                            ResidenceCases$day>=13 & 
                            ResidenceCases$day<=32] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Padre Las Casas"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Chillan" & 
                            ResidenceCases$day>=16 & 
                            ResidenceCases$day<=38] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="Chillan"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Chillan Viejo" & 
                            ResidenceCases$day>=16 & 
                            ResidenceCases$day<=38] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="Chillan Viejo"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Osorno" & 
                            ResidenceCases$day>=16 & 
                            ResidenceCases$day<=46] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Osorno"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Punta Arenas" & 
                            ResidenceCases$day>=17 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Punta Arenas"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.x=="San Pedro de la Paz" & 
                            ResidenceCases$day>=22 & 
                            ResidenceCases$day<=32] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="San Pedro de la Paz"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Hualpen" & 
                            ResidenceCases$day>=22 & 
                            ResidenceCases$day<=32] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="Hualpen"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.x=="Arica" & 
                            ResidenceCases$day>=32 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Arica"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Angol" & 
                            ResidenceCases$day>=46 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="Angol"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Victoria" & 
                            ResidenceCases$day>=46 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.y=="Victoria"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Antofagasta" & 
                            ResidenceCases$day>=51 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Antofagasta"] = 2

ResidenceCases$quarantine[ResidenceCases$comuna.y=="Mejillones" & 
                            ResidenceCases$day>=51 & 
                            ResidenceCases$day<=54] = 1
ResidenceCases$high_income[ResidenceCases$comuna.x=="Mejillones"] = 2