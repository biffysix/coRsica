
###########################################################################################################################################################################################
################### Corsica ###############################################################################################################################################################
###########################################################################################################################################################################################

# Corsica: An All-Purpose R "Package" for Hockey Analysis
# Last edit: 8-13-2016
# Manny

# Required packages
require(dplyr)
require(RSQLite)
require(doMC)
require(ggplot2)
require(Kmisc)
library(rvest)
library(RCurl)
library(rjson)
load(url("http://fenwicka.com/shiny/xG_Regressions.RData"))
load(url("http://fenwicka.com/shiny/coeffs.RData"))

### VARIABLES ###

c(20001:21230,
  30111:30117, 30121:30127, 30131:30137, 30141:30147, 30151:30157, 30161:30167, 30171:30177, 30181:30187,
  30211:30217, 30221:30227, 30231:30237, 30241:30247,
  30311:30317, 30321:30327,
  30411:30417
  ) %>%
  as.character() ->
  all_games

palette <- function(n) {
  
  c("dodgerblue",
    "red1",
    "limegreen",
    "darkorchid",
    "darkorange"
    )[1:n]
  
}

#################

### META FUNCTIONS ###

code <- function(a, b, c) {
  sorted <- sort(c(first(a), first(b), first(c)), decreasing = FALSE)
  p1 <- sorted[1]
  p2 <- sorted[2]
  p3 <- sorted[3]
  return(paste(p1, p2, p3, sep = "-"))
}

sum1p.home <- function(x) {
  summarise(x,
            Venue = "Home", Team = first(Home.Team), TOI = round(sum(na.omit(as.numeric(as.character(Event.Length))))/60, 2),
            CF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
            FF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
            SF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")),
            GF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")),
            xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))),
            ACF = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            AFF = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            ASF = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            AGF = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
            AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))),
            MCF = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            MFF = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            MSF = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            MGF = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
            MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))),
            SCF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09), SCA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09),
            ASCF = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), ASCA = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
            MSCF = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), MSCA = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
            OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def"}),
            DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off"}),
            NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
            OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
            DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
            NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
            FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team),
            HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team),
            GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Home.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Home.Team),
            PENT = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
            PEND = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
            iPENT = sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
            iPEND = sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
            iCF = sum(as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK") & as.character(p1) == as.character(Player)),
            iFF = sum(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(p1) == as.character(Player)),
            iSF = sum(as.character(Event) %in% c("SHOT", "GOAL") & as.character(p1) == as.character(Player)),
            ixG = sum(na.omit(xG*(as.character(p1) == as.character(Player)))),
            iSCF = sum(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(p1) == as.character(Player) & xG >= 0.09),
            iFOW = sum(as.character(Event) %in% c("FAC") & as.character(p2) == as.character(Player) & as.character(ev.team) == Home.Team),
            iFOL = sum(as.character(Event) %in% c("FAC") & as.character(p2) == as.character(Player) & as.character(ev.team) == Away.Team),
            iHF = sum(as.character(Event) %in% c("HIT") & as.character(p1) == as.character(Player)),
            iHA = sum(as.character(Event) %in% c("HIT") & as.character(p2) == as.character(Player)),
            iGVA = sum(as.character(Event) %in% c("GIVE") & as.character(p1) == as.character(Player)), iTKA = sum(as.character(Event) %in% c("TAKE") & as.character(p1) == as.character(Player)),
            iBLK = sum(as.character(Event) %in% c("BLOCK") & as.character(p2) == as.character(Player)),
            OTF = sum(as.character(Event) %in% c("ON") & as.character(p1) == as.character(Player) & FOS < 1),
            DIST = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(p1) == as.character(Player)))),
            RBF = sum(as.character(ev.team) == Home.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Away.Team & is.Rebound == 1),
            RSF = sum(as.character(ev.team) == Home.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Away.Team & is.Rush == 1),
            iRB = sum(is.Rebound == 1 & as.character(p1) == as.character(Player)),
            iRS = sum(is.Rush == 1 & as.character(p1) == as.character(Player)),
            POSTF = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Home.Team & grepl("Goalpost", as.character(Detail) == TRUE)),
            POSTA = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Away.Team & grepl("Goalpost", as.character(Detail) == TRUE)),
            iPOSTF = sum(as.character(Event) %in% c("MISS") & as.character(p1) == as.character(Player) & grepl("Goalpost", as.character(Detail) == TRUE))
  ) %>% data.frame()
}

sum1p.away <- function(x) {
  summarise(x,
            Venue = "Away", Team = first(Away.Team), TOI = round(sum(na.omit(as.numeric(as.character(Event.Length))))/60, 2),
            CF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
            FF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
            SF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")),
            GF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")),
            xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))),
            ACF = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            AFF = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            ASF = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            AGF = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
            AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))),
            MCF = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            MFF = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            MSF = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            MGF = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
            MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))),
            SCF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09), SCA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09),
            ASCF = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), ASCA = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
            MSCF = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), MSCA = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
            OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def"}),
            DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off"}),
            NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
            OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
            DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
            NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
            FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team),
            HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team),
            GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Away.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Away.Team),
            PENT = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
            PEND = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
            iPENT = sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
            iPEND = sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
            iCF = sum(as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK") & as.character(p1) == as.character(Player)),
            iFF = sum(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(p1) == as.character(Player)),
            iSF = sum(as.character(Event) %in% c("SHOT", "GOAL") & as.character(p1) == as.character(Player)),
            ixG = sum(na.omit(xG*(as.character(p1) == as.character(Player)))),
            iSCF = sum(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(p1) == as.character(Player) & xG >= 0.09),
            iFOW = sum(as.character(Event) %in% c("FAC") & as.character(p1) == as.character(Player) & as.character(ev.team) == Away.Team),
            iFOL = sum(as.character(Event) %in% c("FAC") & as.character(p1) == as.character(Player) & as.character(ev.team) == Home.Team),
            iHF = sum(as.character(Event) %in% c("HIT") & as.character(p1) == as.character(Player)),
            iHA = sum(as.character(Event) %in% c("HIT") & as.character(p2) == as.character(Player)),
            iGVA = sum(as.character(Event) %in% c("GIVE") & as.character(p1) == as.character(Player)), iTKA = sum(as.character(Event) %in% c("TAKE") & as.character(p1) == as.character(Player)),
            iBLK = sum(as.character(Event) %in% c("BLOCK") & as.character(p2) == as.character(Player)),
            OTF = sum(as.character(Event) %in% c("ON") & as.character(p1) == as.character(Player) & FOS < 1),
            DIST = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(p1) == as.character(Player)))),
            RBF = sum(as.character(ev.team) == Away.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Home.Team & is.Rebound == 1),
            RSF = sum(as.character(ev.team) == Away.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Home.Team & is.Rush == 1),
            iRB = sum(is.Rebound == 1 & as.character(p1) == as.character(Player)),
            iRS = sum(is.Rush == 1 & as.character(p1) == as.character(Player)),
            POSTF = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Away.Team & grepl("Goalpost", as.character(Detail) == TRUE)),
            POSTA = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Home.Team & grepl("Goalpost", as.character(Detail) == TRUE)),
            iPOSTF = sum(as.character(Event) %in% c("MISS") & as.character(p1) == as.character(Player) & grepl("Goalpost", as.character(Detail) == TRUE))
  ) %>% data.frame()
}

sum2p.home <- function(x) {
  summarise(x, P3 = "X",
            Venue = "Home", Team = first(Home.Team), TOI = round(sum(na.omit(as.numeric(as.character(Event.Length))))/60, 2),
            CF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
            FF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
            SF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")),
            GF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")),
            xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))),
            ACF = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            AFF = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            ASF = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            AGF = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
            AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))),
            MCF = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            MFF = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            MSF = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            MGF = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
            MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))),
            SCF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09), SCA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09),
            ASCF = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), ASCA = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
            MSCF = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), MSCA = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
            OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def"}),
            DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off"}),
            NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
            OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
            DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
            NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
            FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team),
            HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team),
            GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Home.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Home.Team),
            PENT = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
            PEND = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
            RBF = sum(as.character(ev.team) == Home.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Away.Team & is.Rebound == 1),
            RSF = sum(as.character(ev.team) == Home.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Away.Team & is.Rush == 1),
            P1.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P1)), P1.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P1)),
            P1.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P1)), P2.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P2)),
            P2.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P2)), P2.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P2)),
            P3.G = NA, P3.A1 = NA, P3.A2 = NA,
            P1.A1.P2 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P1) & as.character(p1) == as.character(P2)), 
            P1.A2.P2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P1) & as.character(p1) == as.character(P2)),
            P2.A1.P1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P2) & as.character(p1) == as.character(P1)), 
            P2.A2.P1 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P2) & as.character(p1) == as.character(P1)),
            P1.A1.P3 = NA, P1.A2.P3 = NA, P2.A1.P3 = NA, P2.A2.P3 = NA,
            P3.A1.P1 = NA, P3.A2.P1 = NA, P3.A1.P2 = NA, P3.A2.P2 = NA,
            POSTF = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Home.Team & grepl("Goalpost", as.character(Detail) == TRUE)),
            POSTA = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Away.Team & grepl("Goalpost", as.character(Detail) == TRUE))
  ) %>% data.frame()
}

sum2p.away <- function(x) {
  summarise(x, P3 = "X",
            Venue = "Away", Team = first(Away.Team), TOI = round(sum(na.omit(as.numeric(as.character(Event.Length))))/60, 2),
            CF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
            FF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
            SF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")),
            GF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")),
            xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))),
            ACF = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            AFF = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            ASF = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            AGF = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
            AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))),
            MCF = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            MFF = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            MSF = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            MGF = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
            MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))),
            SCF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09), SCA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09),
            ASCF = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), ASCA = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
            MSCF = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), MSCA = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
            OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def"}),
            DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off"}),
            NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
            OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
            DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
            NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
            FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team),
            HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team),
            GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Away.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Away.Team),
            PENT = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
            PEND = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
            RBF = sum(as.character(ev.team) == Away.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Home.Team & is.Rebound == 1),
            RSF = sum(as.character(ev.team) == Away.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Home.Team & is.Rush == 1),
            P1.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P1)), P1.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P1)),
            P1.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P1)), P2.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P2)),
            P2.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P2)), P2.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P2)),
            P3.G = NA, P3.A1 = NA, P3.A2 = NA,
            P1.A1.P2 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P1) & as.character(p1) == as.character(P2)), 
            P1.A2.P2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P1) & as.character(p1) == as.character(P2)),
            P2.A1.P1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P2) & as.character(p1) == as.character(P1)), 
            P2.A2.P1 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P2) & as.character(p1) == as.character(P1)),
            P1.A1.P3 = NA, P1.A2.P3 = NA, P2.A1.P3 = NA, P2.A2.P3 = NA,
            P3.A1.P1 = NA, P3.A2.P1 = NA, P3.A1.P2 = NA, P3.A2.P2 = NA,
            POSTF = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Away.Team & grepl("Goalpost", as.character(Detail) == TRUE)),
            POSTA = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Home.Team & grepl("Goalpost", as.character(Detail) == TRUE))
  ) %>% data.frame()
}

sum3p.home <- function(x) {
  summarise(x,
            Venue = "Home", Team = first(Home.Team), TOI = round(sum(na.omit(as.numeric(as.character(Event.Length))))/60, 2),
            CF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
            FF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
            SF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")),
            GF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")),
            xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))),
            ACF = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            AFF = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            ASF = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            AGF = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
            AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))),
            MCF = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            MFF = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            MSF = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            MGF = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
            MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))),
            SCF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09), SCA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09),
            ASCF = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), ASCA = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
            MSCF = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), MSCA = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
            OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def"}),
            DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off"}),
            NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
            OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
            DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
            NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
            FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team),
            HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team),
            GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Home.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Home.Team),
            PENT = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
            PEND = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
            RBF = sum(as.character(ev.team) == Home.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Away.Team & is.Rebound == 1),
            RSF = sum(as.character(ev.team) == Home.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Away.Team & is.Rush == 1),
            P1.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P1)), P1.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P1)),
            P1.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P1)), P2.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P2)),
            P2.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P2)), P2.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P2)),
            P3.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P3)), P3.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P3)),
            P3.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P3)),
            POSTF = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Home.Team & grepl("Goalpost", as.character(Detail) == TRUE)),
            POSTA = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Away.Team & grepl("Goalpost", as.character(Detail) == TRUE))
  ) %>% data.frame()
}

sum3p.away <- function(x) {
  summarise(x,
            Venue = "Away", Team = first(Away.Team), TOI = round(sum(na.omit(as.numeric(as.character(Event.Length))))/60, 2),
            CF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
            FF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
            SF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")),
            GF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")),
            xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))),
            ACF = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            AFF = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            ASF = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            AGF = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
            AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))),
            MCF = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
            MFF = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
            MSF = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
            MGF = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
            MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))),
            SCF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09), SCA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09),
            ASCF = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), ASCA = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
            MSCF = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), MSCA = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
            OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def"}),
            DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off"}),
            NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
            OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
            DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
            NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
            FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team),
            HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team),
            GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Away.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Away.Team),
            PENT = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
            PEND = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
            RBF = sum(as.character(ev.team) == Away.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Home.Team & is.Rebound == 1),
            RSF = sum(as.character(ev.team) == Away.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Home.Team & is.Rush == 1),
            P1.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P1)), P1.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P1)),
            P1.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P1)), P2.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P2)),
            P2.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P2)), P2.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P2)),
            P3.G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(P3)), P3.A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(P3)),
            P3.A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(P3)),
            POSTF = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Away.Team & grepl("Goalpost", as.character(Detail) == TRUE)),
            POSTA = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Home.Team & grepl("Goalpost", as.character(Detail) == TRUE))
  ) %>% data.frame()
}

######################

### FUNCTIONS ###

# Bang!
# A forward operator for saving objects while supplying additional information.
'%!%' <- function(a, b) {
  
  system.time({
  
    cat("Object of class: ",
        class(a),
        " assigned to variable: ",
        b,
        " at ",
        as.character(Sys.time()),
        "\n",
        sep = "")
    
    assign(b, 
           a, 
           envir = globalenv()
           )
  
  })
  
}

# Bang?
# Takes user input to coerce object to desired class.
'%?%' <- function(a, b) {
  
  system.time({
    
    choice <- readline(prompt = paste("Save to class: ",
                                      "(d)ata.frame",
                                      "(c)haracter",
                                      "(n)umeric",
                                      "(i)nteger",
                                      "(f)actor",
                                      "(D)ate",
                                      "(l)ist",
                                      "",
                                      sep = "\n"
                                      )
                       )
    
    if(choice == "d") {a <- as.data.frame(a)}
    if(choice == "c") {a <- as.character(a)}
    if(choice == "n") {a <- as.numeric(as.character(a))}
    if(choice == "i") {a <- as.integer(a)}
    if(choice == "f") {a <- as.factor(a)}
    if(choice == "D") {a <- as.Date(a)}
    if(choice == "l") {a <- as.list(a)}
    
    cat("Object of class: ",
        class(a),
        " assigned to variable: ",
        b,
        " at ",
        as.character(Sys.time()),
        "\n",
        dim(as.data.frame(a))[1],
        " rows by ",
        dim(as.data.frame(a))[2],
        " columns",
        "\n",
        sep = "")
    
    assign(b, 
           a, 
           envir = globalenv()
    )
    
  })
  
}

# Absolute Numeric
# Coerce object to numeric class.
nabs <- function(x) {
  
  x %>%
    as.character() %>%
    as.numeric() %>%
    return()
  
}

# Parallel Apply
# Use lapply on on chunks of original object in parallel.
parapply <- function(x, fun, cores, ...) {
  
  registerDoMC(cores)
  
  x %>% split(cut(1:length(x), cores)) -> chunks
  
  foreach(i = 1:cores, .combine = c) %dopar% {
    
    chunks[[i]] %>%
    lapply(fun, ...) 
    
  } ->
  mat
  
  return(mat)
  
}

scrape <- function(season, start, end, names = TRUE, pause) {
  
  pbp.list <- NULL
  roster.list <- NULL
  
  agents <- c("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36",
              "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36",
              "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.5.2171.95 Safari/537.36",
              "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.86 Safari/537.36")
  
  for (j in 1:((end - start) + 1)) {
    
    ########################################################################################################################################################################################################
    ########################## SCRAPE PLAY-BY-PLAY / ACQUÉRIR RÉSUMÉ DU MATCH ##############################################################################################################################
    ########################################################################################################################################################################################################
    
    # Define URL / Definir URL
    ID <- as.character((start - 1) + j)
    cat(ID)
    url <- paste("http://www.nhl.com/scores/htmlreports/", season, "/PL0", ID, ".HTM", sep = "")
    
    url.text <- try(getURL(url, header = FALSE,
                           .opts = curlOptions(
                             referer = 'nhl.com',
                             verbose = TRUE,
                             header = TRUE,
                             followLocation = TRUE,
                             useragent = agents[sample(1:4, 1)]))
    )
    
    if(class(url.text) == "try-error") {
      url.text <- getURL(url, header = FALSE,
                         .opts = curlOptions(
                           referer = 'nhl.com',
                           verbose = TRUE,
                           header = TRUE,
                           followLocation = TRUE,
                           useragent = agents[sample(1:4, 1)]))
    }
    
    # Create HTML object / Créer objet HTML
    html <- read_html(url.text)
    
    # Scrape text / Acquérir texte 
    all <- html_nodes(html, "td")
    body <- html_nodes(html, ".bborder")
    full.text <- html_text(all)
    body.text <- html_text(body)
    
    # Skip game if file is broken / Proceder au prochain match si le fichier est incomplet 
    if (length(full.text) < 500) {next}
    
    pbp.raw <- matrix(body.text, byrow = TRUE, ncol = 8) %>% data.frame() %>% filter(X2 != "Per")
    
    # Team list / Liste d'équipes 
    teamlist <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ",
                  "CGY", "CHI", "COL", "DAL", "DET", "EDM",
                  "FLA", "L.A", "MIN", "MTL", "N.J", "NSH",
                  "NYI", "NYR", "OTT", "PHI", "PIT", "S.J",
                  "STL", "T.B", "TOR", "VAN", "WPG", "WSH",
                  "PHX", "ATL")
    
    # Get teams / Obtenir équipes
    hometeam <- gsub(" On Ice", "", body.text[8])
    awayteam <- gsub(" On Ice", "", body.text[7])
    
    hometeam[which(hometeam == "PHX")] <- "ARI"; awayteam[which(awayteam == "PHX")] <- "ARI"
    
    teams <- c(awayteam, hometeam)
    
    # Date, game and etc. data / Date, match, etc.
    date <- gsub("^[a-zA-Z]*, ", "", full.text[grep("^[a-zA-Z]*, ", full.text)]) %>% as.Date(format = "%B %d, %Y") %>% first() %>% as.character()
    Date <- rep(date, time = length(pbp.raw$X1))
    
    Game.ID <- rep(ID, times = length(pbp.raw$X1))
    
    Home.Team <- rep(hometeam, times = length(pbp.raw$X1))
    Away.Team <- rep(awayteam, times = length(pbp.raw$X1))
    
    Duration <- rep(NA, times = length(pbp.raw$X1))
    
    # Parse time / Traiter temps
    timemat <- data.frame(matrix(as.numeric(unlist(strsplit(as.character(pbp.raw$X4), ":"))), byrow = TRUE, ncol = 3))
    
    Seconds <- 1200*(as.numeric(pbp.raw$X2) - 1) + timemat$X1*60 + (timemat$X3 > 0)*(60 - timemat$X3)
    Seconds[which(as.numeric(pbp.raw$X2) == 5)] <- 3900.001
    
    ## Parse on-ice / Traiter joueurs sur glace
    stretch <- function(x) {
      t <- as.character(unlist(x))
      t2 <- list(c(t, rep(c(0, NA), times = (12 - (length(t)/2)))))
      return(t2)
    }
    
    # Away / étrangère 
    a.match <- regmatches(as.character(pbp.raw$X7), gregexpr("[0-9|A-Z]+", as.character(pbp.raw$X7)))
    a.new <- lapply(a.match, stretch)
    Away.On <- data.frame(matrix(unlist(a.new), byrow = TRUE, ncol = 24))
    colnames(Away.On) <- c("a1.num", "a1.pos", "a2.num", "a2.pos", "a3.num", "a3.pos", "a4.num", "a4.pos", "a5.num", "a5.pos", "a6.num", "a6.pos",
                           "a7.num", "a7.pos", "a8.num", "a8.pos", "a9.num", "a9.pos", "a10.num", "a10.pos", "a11.num", "a11.pos", "a12.num", "a12.pos")
    
    # Home / Domicile 
    h.match <- regmatches(as.character(pbp.raw$X8), gregexpr("[0-9|A-Z]+", as.character(pbp.raw$X8)))
    h.new <- lapply(h.match, stretch)
    Home.On <- data.frame(matrix(unlist(h.new), byrow = TRUE, ncol = 24))
    colnames(Home.On) <- c("h1.num", "h1.pos", "h2.num", "h2.pos", "h3.num", "h3.pos", "h4.num", "h4.pos", "h5.num", "h5.pos", "h6.num", "h6.pos", 
                           "h7.num", "h7.pos", "h8.num", "h8.pos", "h9.num", "h9.pos", "h10.num", "h10.pos","h11.num", "h11.pos", "h12.num", "h12.pos")
    
    ## Parse description / Traiter déscription 
    clean.nums <- function(x) {
      t <- gsub("#|ONGOAL - ", "", as.character(unlist(x)))
      t2 <- list(c(t, rep(NA, times = (3 - length(t)))))
      return(t2)
    }
    
    dummy.team <- function(x) {
      if (length(unlist(x)) > 0) {
        t <- x
      } else {
        t <- NA
      }
      return(t)
    }
    
    dummy.zone <- function(x) {
      if (length(unlist(x)) > 0) {
        t <- x
      } else {
        t <- NA
      }
      return(t)
    }
    
    dummy.detail <- function(x) {
      if (length(unlist(x)) > 0) {
        t <- paste(unlist(x), collapse = "")
      } else {
        t <- NA
      }
      return(t)
    }
    
    # Event team / équipe du jeu
    t.match <- regmatches(as.character(pbp.raw$X6), gregexpr(paste("(^", paste(teamlist, collapse = "|^"), ")", sep = ""), as.character(pbp.raw$X6)))
    t.new <- lapply(t.match, dummy.team)
    ev.team <- gsub(" ", "", as.character(unlist(t.new)))
    ev.team[which(ev.team == "PHX")] <- "ARI"
    
    # Event players / Joueurs du jeu
    d.match <- regmatches(as.character(pbp.raw$X6), gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(pbp.raw$X6)))
    d.new <- lapply(d.match, clean.nums)
    ev.players <- data.frame(matrix(unlist(d.new), byrow = TRUE, ncol = 3))
    colnames(ev.players) <- c("p1", "p2", "p3")
    
    # Event zone / Zone du jeu
    z.match <- regmatches(as.character(pbp.raw$X6), gregexpr("[a-zA-Z]{3}. [zZ]one", as.character(pbp.raw$X6)))
    z.new <- lapply(z.match, dummy.zone)
    ev.zone <- gsub(". [zZ]one", "", as.character(unlist(z.new)))
    
    # Event details / Détails du jeu
    e.match <- regmatches(as.character(pbp.raw$X6), gregexpr(", [a-zA-Z|-]+,|[A-Z] .+[(].{4,}[)],|[A-Z] .+[(][a-zA-Z]{3,}[)],", as.character(pbp.raw$X6)))
    e.new <- lapply(e.match, dummy.detail)
    Detail <- gsub(",|, |[A-Z]+ |#[0-9]+ |[A-Z]{2,}.", "", as.character(unlist(e.new)))
    
    # On-ice goalies / Gardiens sur glace
    Home.Goalie <- (Home.On$h12.pos == "G" & !is.na(Home.On$h12.pos))*as.numeric(as.character(Home.On$h12.num)) + (Home.On$h11.pos == "G" & !is.na(Home.On$h11.pos))*as.numeric(as.character(Home.On$h11.num)) +
      (Home.On$h10.pos == "G" & !is.na(Home.On$h10.pos))*as.numeric(as.character(Home.On$h10.num)) + (Home.On$h9.pos == "G" & !is.na(Home.On$h9.pos))*as.numeric(as.character(Home.On$h9.num)) +
      (Home.On$h8.pos == "G" & !is.na(Home.On$h8.pos))*as.numeric(as.character(Home.On$h8.num)) + (Home.On$h7.pos == "G" & !is.na(Home.On$h7.pos))*as.numeric(as.character(Home.On$h7.num)) +
      (Home.On$h6.pos == "G" & !is.na(Home.On$h6.pos))*as.numeric(as.character(Home.On$h6.num)) + (Home.On$h5.pos == "G" & !is.na(Home.On$h5.pos))*as.numeric(as.character(Home.On$h5.num)) +
      (Home.On$h4.pos == "G" & !is.na(Home.On$h4.pos))*as.numeric(as.character(Home.On$h4.num)) + (Home.On$h3.pos == "G" & !is.na(Home.On$h3.pos))*as.numeric(as.character(Home.On$h3.num)) +
      (Home.On$h2.pos == "G" & !is.na(Home.On$h2.pos))*as.numeric(as.character(Home.On$h2.num)) + (Home.On$h1.pos == "G" & !is.na(Home.On$h1.pos))*as.numeric(as.character(Home.On$h1.num))
    
    Away.Goalie <- (Away.On$a12.pos == "G" & !is.na(Away.On$a12.pos))*as.numeric(as.character(Away.On$a12.num)) + (Away.On$a11.pos == "G" & !is.na(Away.On$a11.pos))*as.numeric(as.character(Away.On$a11.num)) +
      (Away.On$a10.pos == "G" & !is.na(Away.On$a10.pos))*as.numeric(as.character(Away.On$a10.num)) + (Away.On$a9.pos == "G" & !is.na(Away.On$a9.pos))*as.numeric(as.character(Away.On$a9.num)) +
      (Away.On$a8.pos == "G" & !is.na(Away.On$a8.pos))*as.numeric(as.character(Away.On$a8.num)) + (Away.On$a7.pos == "G" & !is.na(Away.On$a7.pos))*as.numeric(as.character(Away.On$a7.num)) +
      (Away.On$a6.pos == "G" & !is.na(Away.On$a6.pos))*as.numeric(as.character(Away.On$a6.num)) + (Away.On$a5.pos == "G" & !is.na(Away.On$a5.pos))*as.numeric(as.character(Away.On$a5.num)) +
      (Away.On$a4.pos == "G" & !is.na(Away.On$a4.pos))*as.numeric(as.character(Away.On$a4.num)) + (Away.On$a3.pos == "G" & !is.na(Away.On$a3.pos))*as.numeric(as.character(Away.On$a3.num)) +
      (Away.On$a2.pos == "G" & !is.na(Away.On$a2.pos))*as.numeric(as.character(Away.On$a2.num)) + (Away.On$a1.pos == "G" & !is.na(Away.On$a1.pos))*as.numeric(as.character(Away.On$a1.num))
    
    # Create PBP / Créer résumé
    pbp.new <- pbp.raw %>% select(-c(X1, X3, X4, X7, X8)) %>% cbind(Duration, Date, Game.ID, ev.team, ev.players, ev.zone, Detail, Seconds, Away.On[, 1:12], Home.On[, 1:12], Away.Team, Home.Team, Away.Goalie, Home.Goalie)
    
    ## Replace with teamnum ID / Remplacer avec code équipenum
    pbp.new <- rbind_list(
      filter(pbp.new, X5 == "FAC") %>% 
        mutate(p1 = paste(awayteam, p1, sep = ""), p2 = paste(hometeam, p2, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "HIT") %>% group_by(ev.team) %>%
        mutate(p1 = paste(first(ev.team), p1, sep = ""), p2 = paste(teams[which(teams != first(ev.team))], p2, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "SHOT") %>%
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "GIVE") %>%
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "MISS") %>%
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "GOAL") %>% 
        mutate(p1 = paste(ev.team, p1, sep = ""), 
               p2 = gsub(paste(paste(teamlist, collapse = "NA|"), "NA", sep = ""), NA, paste(ev.team, p2, sep = "")),
               p3 = gsub(paste(paste(teamlist, collapse = "NA|"), "NA", sep = ""), NA, paste(ev.team, p3, sep = ""))) %>% data.frame(),
      filter(pbp.new, X5 == "BLOCK") %>% group_by(ev.team) %>%
        mutate(p1 = paste(first(ev.team), p1, sep = ""), p2 = paste(teams[which(teams != first(ev.team))], p2, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "PENL") %>% group_by(ev.team) %>%
        mutate(p1 = paste(first(ev.team), p1, sep = ""), 
               p2 = gsub(paste(paste(teamlist, collapse = "NA|"), "NA", sep = ""), NA, paste(teams[which(teams != first(ev.team))], p2, sep = ""))) %>% data.frame(),
      filter(pbp.new, X5 == "TAKE") %>% 
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 %in% c("FAC", "HIT", "SHOT", "GIVE", "MISS", "GOAL", "BLOCK", "PENL", "TAKE") == FALSE) %>% data.frame()
    ) %>% data.frame() %>% 
      mutate(a1.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a1.num, sep = "")),
             a2.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a2.num, sep = "")),
             a3.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a3.num, sep = "")),
             a4.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a4.num, sep = "")),
             a5.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a5.num, sep = "")),
             a6.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a6.num, sep = "")),
             h1.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h1.num, sep = "")),
             h2.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h2.num, sep = "")),
             h3.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h3.num, sep = "")),
             h4.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h4.num, sep = "")),
             h5.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h5.num, sep = "")),
             h6.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h6.num, sep = "")),
             Home.Goalie = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, Home.Goalie, sep = "")),
             Away.Goalie = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, Away.Goalie, sep = "")),
             Home.Skaters = 6 - (is.na(h1.num)) - (is.na(h2.num)) - (is.na(h3.num)) - (is.na(h4.num)) - (is.na(h5.num)) - (is.na(h6.num)) - (!is.na(Home.Goalie)),
             Away.Skaters = 6 - (is.na(a1.num)) - (is.na(a2.num)) - (is.na(a3.num)) - (is.na(a4.num)) - (is.na(a5.num)) - (is.na(a6.num)) - (!is.na(Away.Goalie)),
             Seconds = Seconds - 0.01*(X5 %in% c("STOP", "PENL", "GOAL", "PEND")) + 0.01*(X5 == "FAC")) %>%
      rename(Period = X2, Event = X5, Description = X6) %>% arrange(Seconds) %>%
      mutate(Home.Score = cumsum(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Home.Team)) - 1*(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Home.Team)),
             Away.Score = cumsum(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Away.Team)) - 1*(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Away.Team))) %>%
      data.frame()
    
    # Re-assign event zone for blocked shots to perspective of shooting team / Re-attribuer zone du jeu pour tirs bloqués au point de vue de l'équipe tireur
    pbp.new$ev.zone[which(pbp.new$Event == "BLOCK" & pbp.new$ev.zone == "Def")] <- "Off"
    
    # Append strength and score states / Attacher états de forces et de score
    pbp.new$Strength.State <- paste(pbp.new$Home.Skaters, pbp.new$Away.Skaters, sep = "v"); pbp.new$Score.State <- paste(pbp.new$Home.Score, pbp.new$Away.Score, sep = "-")
    pbp.new$Score.Cat <- 1*(pbp.new$Home.Score - pbp.new$Away.Score == 1) + 2*(pbp.new$Home.Score - pbp.new$Away.Score == 2) + 3*(pbp.new$Home.Score - pbp.new$Away.Score >= 3) -
      1*(pbp.new$Home.Score - pbp.new$Away.Score == -1) - 2*(pbp.new$Home.Score - pbp.new$Away.Score == -2) - 3*(pbp.new$Home.Score - pbp.new$Away.Score <= -3)
    
    ########################################################################################################################################################################################################
    ########################## SCRAPE SHIFT REPORTS / ACQUÉRIR RAPPORTS DE PRÉSENCES #######################################################################################################################
    ########################################################################################################################################################################################################
    
    # Define URLs / Définir URLs
    url1 <- paste("http://www.nhl.com/scores/htmlreports/", season, "/TH0", ID, ".HTM", sep = "") # Home / Domicile
    url2 <- paste("http://www.nhl.com/scores/htmlreports/", season, "/TV0", ID, ".HTM", sep = "") # Away / Étrangère
    
    url1.text <- try(getURL(url1, header = FALSE,
                            .opts = curlOptions(
                              referer = 'nhl.com',
                              verbose = TRUE,
                              header = TRUE,
                              followLocation = TRUE,
                              useragent = agents[sample(1:4, 1)]))
    )
    
    url2.text <- try(getURL(url2, header = FALSE,
                            .opts = curlOptions(
                              referer = 'nhl.com',
                              verbose = TRUE,
                              header = TRUE,
                              followLocation = TRUE,
                              useragent = agents[sample(1:4, 1)]))
    )
    
    if(class(url1.text) == "try-error" | class(url2.text) == "try-error") {
      url1.text <- getURL(url1, header = FALSE,
                          .opts = curlOptions(
                            referer = 'nhl.com',
                            verbose = TRUE,
                            header = TRUE,
                            followLocation = TRUE,
                            useragent = agents[sample(1:4, 1)]))
      
      url2.text <- getURL(url2, header = FALSE,
                          .opts = curlOptions(
                            referer = 'nhl.com',
                            verbose = TRUE,
                            header = TRUE,
                            followLocation = TRUE,
                            useragent = agents[sample(1:4, 1)]))
    }
    
    # Create HTML objects / Créer objets HTML
    html1 <- read_html(url1.text) # Home / Domicile
    html2 <- read_html(url2.text) # Away / Étrangère
    
    # Scrape tables / Acquérir tables
    home.text.1 <- html_nodes(html1, ".border")
    away.text.1 <- html_nodes(html2, ".border")
    home.text.2 <- html_nodes(html1, ".bborder")
    away.text.2 <- html_nodes(html2, ".bborder")
    
    home.outer <- html_text(home.text.1)
    away.outer <- html_text(away.text.1)
    home.inner <- html_text(home.text.2)
    away.inner <- html_text(away.text.2)
    
    # Skip game if file is broken / Proceder au prochain match si le fichier est incomplet 
    if (length(home.inner) < 1 | length(away.inner) < 1) {next}
    
    hometeam.full <- home.outer[1]
    home.players <- home.outer[-1]
    home.players <- home.players[which(grepl("^[0-9]+", home.players) == TRUE)] # FIX FOR 20132014-20934 / SOLUTION POUR 20132014-20934
    awayteam.full <- away.outer[1]
    away.players <- away.outer[-1]
    away.players <- away.players[which(grepl("^[0-9]+", away.players) == TRUE)] # FIX FOR 20132014-20934 / SOLUTION POUR 20132014-20934
    
    # Create roster table / Créer table de formation
    roster <- rbind_list(cbind(rep(hometeam, times = length(home.players)), home.players) %>% data.frame() %>% rename(Num.Last.First = home.players),
                         cbind(rep(awayteam, times = length(away.players)), away.players) %>% data.frame() %>% rename(Num.Last.First = away.players)) %>%
      data.frame()
    
    namemat <- data.frame(matrix(as.character(unlist(strsplit(gsub("^[0-9]+ ", "", roster$Num.Last.First), ", "))), byrow = T, ncol = 2))
    
    roster$Game.ID <- rep(ID, times = length(roster$Num.Last.First))
    roster$Date <- rep(date, times = length(roster$Num.Last.First))
    roster$Number <- unlist(regmatches(as.character(roster$Num.Last.First), gregexpr("^[0-9]+", as.character(roster$Num.Last.First))))
    roster$Last.Name <- namemat$X1
    roster$First.Name <- namemat$X2
    
    posmatch <- rbind_list(group_by(pbp.new, a1.num) %>% rename(player = a1.num) %>% 
                             summarise(C = sum(a1.pos == "C"), L = sum(a1.pos == "L"), R = sum(a1.pos == "R"), D = sum(a1.pos == "D"), G = sum(a1.pos == "G"), N = sum(a1.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a2.num) %>% rename(player = a2.num) %>% 
                             summarise(C = sum(a2.pos == "C"), L = sum(a2.pos == "L"), R = sum(a2.pos == "R"), D = sum(a2.pos == "D"), G = sum(a2.pos == "G"), N = sum(a2.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a3.num) %>% rename(player = a3.num) %>% 
                             summarise(C = sum(a3.pos == "C"), L = sum(a3.pos == "L"), R = sum(a3.pos == "R"), D = sum(a3.pos == "D"), G = sum(a3.pos == "G"), N = sum(a3.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a4.num) %>% rename(player = a4.num) %>% 
                             summarise(C = sum(a4.pos == "C"), L = sum(a4.pos == "L"), R = sum(a4.pos == "R"), D = sum(a4.pos == "D"), G = sum(a4.pos == "G"), N = sum(a4.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a5.num) %>% rename(player = a5.num) %>% 
                             summarise(C = sum(a5.pos == "C"), L = sum(a5.pos == "L"), R = sum(a5.pos == "R"), D = sum(a5.pos == "D"), G = sum(a5.pos == "G"), N = sum(a5.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a6.num) %>% rename(player = a6.num) %>% 
                             summarise(C = sum(a6.pos == "C"), L = sum(a6.pos == "L"), R = sum(a6.pos == "R"), D = sum(a6.pos == "D"), G = sum(a6.pos == "G"), N = sum(a6.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h1.num) %>% rename(player = h1.num) %>% 
                             summarise(C = sum(h1.pos == "C"), L = sum(h1.pos == "L"), R = sum(h1.pos == "R"), D = sum(h1.pos == "D"), G = sum(h1.pos == "G"), N = sum(h1.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h2.num) %>% rename(player = h2.num) %>% 
                             summarise(C = sum(h2.pos == "C"), L = sum(h2.pos == "L"), R = sum(h2.pos == "R"), D = sum(h2.pos == "D"), G = sum(h2.pos == "G"), N = sum(h2.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h3.num) %>% rename(player = h3.num) %>% 
                             summarise(C = sum(h3.pos == "C"), L = sum(h3.pos == "L"), R = sum(h3.pos == "R"), D = sum(h3.pos == "D"), G = sum(h3.pos == "G"), N = sum(h3.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h4.num) %>% rename(player = h4.num) %>% 
                             summarise(C = sum(h4.pos == "C"), L = sum(h4.pos == "L"), R = sum(h4.pos == "R"), D = sum(h4.pos == "D"), G = sum(h4.pos == "G"), N = sum(h4.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h5.num) %>% rename(player = h5.num) %>% 
                             summarise(C = sum(h5.pos == "C"), L = sum(h5.pos == "L"), R = sum(h5.pos == "R"), D = sum(h5.pos == "D"), G = sum(h5.pos == "G"), N = sum(h5.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h6.num) %>% rename(player = h6.num) %>% 
                             summarise(C = sum(h6.pos == "C"), L = sum(h6.pos == "L"), R = sum(h6.pos == "R"), D = sum(h6.pos == "D"), G = sum(h6.pos == "G"), N = sum(h6.pos %in% c("C", "L", "R", "D", "G") == F))) %>%
      data.frame() %>% group_by(player) %>%
      summarise(C = sum(C), L = sum(L), R = sum(R), D = sum(D), G = sum(G), N = sum(N)) %>% 
      mutate(Pos.Num = 1*(C > L & C > R & C > D & C > G & C > N) +
               2*(L > C & L > R & L > D & L > G & L > N) +
               3*(R > L & R > C & R > D & R > G & R > N) +
               4*(D > L & D > R & D > C & D > G & D > N) +
               5*(G > C & G > L & G > R & G > D & G > N) +
               6*(N > C & N > L & N > R & N > D & N > G)) %>%
      data.frame()
    
    posmatch$Pos <- colnames(posmatch)[-1][posmatch$Pos.Num[1:nrow(posmatch)]]
    
    roster <- roster %>% mutate(Team.Num = paste(V1, Number, sep = ""),
                                Full.Name = paste(First.Name, Last.Name, sep = "."),
                                Position = posmatch$Pos[match(Team.Num, posmatch$player)]) %>%
      rename(Team = V1) %>% data.frame()
    
    # Create shift tables / Créer tables de présences
    shiftlist.home <- NULL
    shiftlist.away <- NULL
    
    for (i in 1:(length(home.outer)-1)) {
      shiftlist.home[[i]] <- home.inner[which(home.inner == "Shift #" | home.inner == "Présence #Shift #")[i]:(which(home.inner == "SHF" | home.inner == "PR/SHF")[i]-3)]
    }
    
    for (i in 1:(length(away.outer)-1)) {
      shiftlist.away[[i]] <- away.inner[which(away.inner == "Shift #" | away.inner == "Présence #Shift #")[i]:(which(away.inner == "SHF" | away.inner == "PR/SHF")[i]-3)]
    }
    
    htoi.raw <- matrix(unlist(shiftlist.home), byrow = TRUE, ncol = 6) %>% data.frame()
    atoi.raw <- matrix(unlist(shiftlist.away), byrow = TRUE, ncol = 6) %>% data.frame()
    
    htoi.raw$p.match <- cumsum(htoi.raw$X2 == "Per")
    htoi.raw$Player <- home.players[htoi.raw$p.match[1:nrow(htoi.raw)]]
    htoi.raw <- filter(htoi.raw, X2 != "Per")
    
    atoi.raw$p.match <- cumsum(atoi.raw$X2 == "Per")
    atoi.raw$Player <- away.players[atoi.raw$p.match[1:nrow(atoi.raw)]]
    atoi.raw <- filter(atoi.raw, X2 != "Per")
    
    startmat.home <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(htoi.raw$X3), " ")), ":"))), byrow = TRUE, ncol = 5))
    endmat.home <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(htoi.raw$X4), " ")), ":"))), byrow = TRUE, ncol = 5))
    startmat.away <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(atoi.raw$X3), " ")), ":"))), byrow = TRUE, ncol = 5))
    endmat.away <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(atoi.raw$X4), " ")), ":"))), byrow = TRUE, ncol = 5))
    
    startsec.home <- 1200*(as.numeric(htoi.raw$X2) - 1) + startmat.home$X1*60 + startmat.home$X2
    endsec.home <- 1200*(as.numeric(htoi.raw$X2) - 1) + endmat.home$X1*60 + endmat.home$X2
    startsec.away <- 1200*(as.numeric(atoi.raw$X2) - 1) + startmat.away$X1*60 + startmat.away$X2
    endsec.away <- 1200*(as.numeric(atoi.raw$X2) - 1) + endmat.away$X1*60 + endmat.away$X2
    
    htoi.new <- htoi.raw %>% select(-c(X1, X3:X6, p.match)) %>% cbind(roster[match(htoi.raw$Player, roster$Num.Last.First), c(3,4,1,5,8,9)], startsec.home, endsec.home) %>%
      mutate(Duration = endsec.home - startsec.home) %>% data.frame()
    atoi.new <- atoi.raw %>% select(-c(X1, X3:X6, p.match)) %>% cbind(roster[match(atoi.raw$Player, roster$Num.Last.First), c(3,4,1,5,8,9)], startsec.away, endsec.away) %>% 
      mutate(Duration = endsec.away - startsec.away) %>% data.frame()
    
    colnames(htoi.new) <- c("Period", "Num.Last.First", "Game.ID", "Date", "Team", "Num", "Team.Num", "Full.Name", "Start.Seconds", "End.Seconds", "Duration")
    colnames(atoi.new) <- c("Period", "Num.Last.First", "Game.ID", "Date", "Team", "Num", "Team.Num", "Full.Name", "Start.Seconds", "End.Seconds", "Duration")
    
    shift.on <- rbind_list(htoi.new %>% select(Period, Start.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = Start.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                             mutate(Event = "ON", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                    Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA),
                           atoi.new %>% select(Period, Start.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = Start.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                             mutate(Event = "ON", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                    Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA)) %>% data.frame()
    
    shift.off <- rbind_list(htoi.new %>% select(Period, End.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = End.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                              mutate(Event = "OFF", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                     Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA),
                            atoi.new %>% select(Period, End.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = End.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                              mutate(Event = "OFF", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                     Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA)) %>% data.frame()
    
    who.on.1 <- function(x) {
      n <- htoi.new$Team.Num[which(as.numeric(htoi.new$Start.Seconds) <= as.numeric(x) & as.numeric(htoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      on.home <- c(n2, p)
      return(on.home)
    }
    
    who.off.1 <- function(x) {
      n <- htoi.new$Team.Num[which(as.numeric(htoi.new$Start.Seconds) < as.numeric(x) & as.numeric(htoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      off.home <- c(n2, p)
      return(off.home)
    }
    
    who.on.2 <- function(x) {
      n <- atoi.new$Team.Num[which(as.numeric(atoi.new$Start.Seconds) <= as.numeric(x) & as.numeric(atoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      on.away <- c(n2, p)
      return(on.away)
    }
    
    who.off.2 <- function(x) {
      n <- atoi.new$Team.Num[which(as.numeric(atoi.new$Start.Seconds) < as.numeric(x) & as.numeric(atoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      off.away <- c(n2, p)
      return(off.away)
    }
    
    on.home <- lapply(shift.on$Seconds, who.on.1) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(h1.num = X1, h2.num = X2, h3.num = X3, h4.num = X4, h5.num = X5, h6.num = X6, h1.pos = X13, h2.pos = X14, h3.pos = X15, h4.pos = X16, h5.pos = X17, h6.pos = X18)
    off.home <- lapply(shift.off$Seconds, who.off.1) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(h1.num = X1, h2.num = X2, h3.num = X3, h4.num = X4, h5.num = X5, h6.num = X6, h1.pos = X13, h2.pos = X14, h3.pos = X15, h4.pos = X16, h5.pos = X17, h6.pos = X18)
    on.away <- lapply(shift.on$Seconds, who.on.2) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(a1.num = X1, a2.num = X2, a3.num = X3, a4.num = X4, a5.num = X5, a6.num = X6, a1.pos = X13, a2.pos = X14, a3.pos = X15, a4.pos = X16, a5.pos = X17, a6.pos = X18)
    off.away <- lapply(shift.off$Seconds, who.off.2) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(a1.num = X1, a2.num = X2, a3.num = X3, a4.num = X4, a5.num = X5, a6.num = X6, a1.pos = X13, a2.pos = X14, a3.pos = X15, a4.pos = X16, a5.pos = X17, a6.pos = X18)
    
    shift.on <- cbind(shift.on, on.home[,c(1:6, 13:18)], on.away[,c(1:6, 13:18)]) %>% data.frame()
    shift.off <- cbind(shift.off, off.home[,c(1:6, 13:18)], off.away[,c(1:6, 13:18)]) %>% data.frame()
    
    check <- pbp.new %>% filter(Event == "FAC") %>% mutate(Event = "CHECK", Seconds = Seconds - 0.011, Description = "Checkpoint") %>% data.frame()
    
    pbp.new <- rbind_list(pbp.new, shift.on, shift.off, check) %>% arrange(Seconds) %>% 
      mutate(event.ref = cumsum(Event %in% c("ON", "OFF") == F)) %>% group_by(event.ref) %>%
      mutate(Away.Team = first(Away.Team), Home.Team = first(Home.Team), Away.Goalie = first(Away.Goalie), Home.Goalie = first(Home.Goalie),
             Home.Skaters = first(Home.Skaters), Away.Skaters = first(Away.Skaters), Strength.State = first(Strength.State), 
             Home.Score = first(Home.Score), Away.Score = first(Away.Score), Score.State = first(Score.State), Score.Cat = first(Score.Cat),
             Seconds = Seconds - 0.001*(Event == "OFF") + 0.001*(Event == "ON")) %>% 
      filter(Event != "CHECK") %>% arrange(Seconds) %>% data.frame() %>% select(-c(event.ref)) %>% data.frame() 
    
    pbp.new$Event.Length <- c((pbp.new$Seconds[2:nrow(pbp.new)] - pbp.new$Seconds[1:(nrow(pbp.new) - 1)]), 0)
    
    ########################################################################################################################################################################################################
    ########################## SCRAPE HIGHLIGHTS JSON / ACQUÉRIR JSON DE FAITS-SAILLANTS ###################################################################################################################
    ########################################################################################################################################################################################################
    
    # Define URL / Définir URL
    year <- substr(season, start = 1, stop = 4)
    url3 <- paste("http://live.nhle.com/GameData/", season, "/", year, "0", ID, "/gc/gcgm.jsonp", sep = "")
    
    full.text.3 <- try(getURL(url3, header = FALSE,
                              .opts = curlOptions(
                                referer = 'nhl.com',
                                verbose = TRUE,
                                header = TRUE,
                                followLocation = TRUE,
                                useragent = agents[sample(1:4, 1)]))
    )
    
    if (class(full.text.3) == "try-error") {
      full.text.3 <- getURL(url3, header = FALSE,
                            .opts = curlOptions(
                              referer = 'nhl.com',
                              verbose = TRUE,
                              header = TRUE,
                              followLocation = TRUE,
                              useragent = agents[sample(1:4, 1)]))
    }
    
    text.3 <- unlist(strsplit(full.text.3, ","))
    
    hl.presecs <- gsub("sip[\":]*", "", unlist(regmatches(text.3, gregexpr("sip[\":]*[0-9]*", text.3))))
    hl.period <- gsub("[^a-z]+p[\":]+", "", unlist(regmatches(text.3, gregexpr("[^a-z]+p[\":]+[0-9]*", text.3))))
    hl.Team1 <- gsub("[^a-z]+t1[\":]+", "", unlist(regmatches(text.3, gregexpr("[^a-z]+t1[\":]+[A-Z]*", text.3))))
    hl.Team2 <- gsub("[^a-z]+t2[\":]+", "", unlist(regmatches(text.3, gregexpr("[^a-z]+t2[\":]+[A-Z]*", text.3))))
    hl.seconds <- 1200*(as.numeric(hl.period) - 1) + as.numeric(hl.presecs)
    if(as.numeric(season) >= 20152016) {
      
      urls <- paste("https://www.nhl.com/video/c-", gsub("neulionId\\\":", "", unlist(regmatches(text.3, gregexpr("neulionId\\\":[0-9]+", text.3)))), sep = "")
      
    } else {
      
      urls <- NA
      
    }
    
    hl.mat <- cbind(hl.seconds, urls) %>% data.frame()
    
    pbp.new$URL <- hl.mat$urls[match((pbp.new$Event %in% c("SHOT", "GOAL"))*round(as.numeric(as.character(pbp.new$Seconds)), 0), hl.mat$hl.seconds)]
    pbp.new$Highlight <- 1*(!is.na(pbp.new$URL))
    
    ########################################################################################################################################################################################################
    ########################## SCRAPE SPORTSNET / ACQUÉRIR DONÉES SPORTSNET ################################################################################################################################
    ########################################################################################################################################################################################################
    
    if (as.numeric(season) >= 20152016) {
      
      # Provide date / Fournir date
      day <- date
      
      # Scrape main page / Acquérir page primaire 
      url <- paste0("http://www.sportsnet.ca/hockey/nhl/scores/?datepicker-date=", day)
      glist <- try(getURL(url, header = FALSE,
                          .opts = curlOptions(
                            referer = 'sportsnet.ca',
                            verbose = TRUE,
                            header = TRUE,
                            followLocation = TRUE,
                            useragent = agents[sample(1:4, 1)]))
      )
      
      if (class(glist) == "try-error") {
        glist <- getURL(url, header = FALSE,
                        .opts = curlOptions(
                          referer = 'sportsnet.ca',
                          verbose = TRUE,
                          header = TRUE,
                          followLocation = TRUE,
                          useragent = agents[sample(1:4, 1)]))
      }
      
      gameids <- gsub("window.open[(][']", "", unique(unlist(regmatches (glist, gregexpr("window.open[(][']http://www.sportsnet.ca/hockey/nhl/livetracker/game/[0-9]+", glist)))))
      teamcity <- gsub("<span class=\"scores-team-city\">|</span>", "", unlist(regmatches (glist, gregexpr("<span class=\"scores-team-city\">([a-zA-Z]|[.]|[-]|[ ])+</span>", glist))))
      teamname <- gsub("<span class=\"scores-team-name\">|</span>", "", unlist(regmatches (glist, gregexpr("<span class=\"scores-team-name\">([a-zA-Z]|[.]|[-]|[ ])+</span>", glist))))
      teams <- paste(teamcity, teamname)
      
      # Standardize team names / Standardiser noms d'équipes
      teamlist <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ",
                    "CGY", "CHI", "COL", "DAL", "DET", "EDM",
                    "FLA", "L.A", "MIN", "MTL", "N.J", "NSH",
                    "NYI", "NYR", "OTT", "PHI", "PIT", "S.J",
                    "STL", "T.B", "TOR", "VAN", "WPG", "WSH",
                    "ARI")
      
      fullnames <- c("Anaheim Ducks", "Arizona Coyotes", "Boston Bruins", "Buffalo Sabres", "Carolina Hurricanes", "Columbus Blue Jackets",
                     "Calgary Flames", "Chicago Blackhawks", "Colorado Avalanche", "Dallas Stars", "Detroit Red Wings", "Edmonton Oilers",
                     "Florida Panthers", "Los Angeles Kings", "Minnesota Wild", "Montreal Canadiens", "New Jersey Devils", "Nashville Predators",
                     "New York Islanders", "New York Rangers", "Ottawa Senators", "Philadelphia Flyers", "Pittsburgh Penguins", "San Jose Sharks",
                     "St. Louis Blues", "Tampa Bay Lightning", "Toronto Maple Leafs", "Vancouver Canucks", "Winnipeg Jets", "Washington Capitals",
                     "Phoenix Coyotes")
      
      team.match <- cbind(teamlist, fullnames) %>% data.frame()
      
      teams <- team.match$teamlist[match(teams, team.match$fullnames)]
      
      teammat <- matrix(teams, byrow = TRUE, ncol = 2) %>% data.frame()
      
      # Create URL directory / Créer annuaire de URLs
      url.match <- cbind(gameids, teammat) %>% data.frame() %>% rename(awayteam = X1, hometeam = X2)
      
      # Match URL / Associer URL
      urlt <- first(url.match$gameids[which(url.match$awayteam == awayteam | url.match$hometeam == awayteam)])
      
      ########################################################################################################################################################################################################
      ########################################################################################################################################################################################################
      
      # Scrape game page / Acquérir page du match
      gamepage <- try(getURL(urlt, header = FALSE,
                             .opts = curlOptions(
                               referer = 'sportsnet.ca',
                               verbose = TRUE,
                               header = TRUE,
                               followLocation = TRUE,
                               useragent = agents[sample(1:4, 1)]))
      )
      
      if (class(gamepage) == "try-error") {
        gamepage <- getURL(urlt, header = FALSE,
                           .opts = curlOptions(
                             referer = 'sportsnet.ca',
                             verbose = TRUE,
                             header = TRUE,
                             followLocation = TRUE,
                             useragent = agents[sample(1:4, 1)]))
      }
      
      events <- unlist(regmatches (gamepage, gregexpr("[{]\\\"id\\\":[0-9]+,\\\"loc.*?momentum", gamepage)))
      time <- gsub("elapsed\\\":\\\"", "", unlist(regmatches (events, gregexpr("elapsed\\\":\\\"[0-9:]+", events))))
      period <- gsub("period\\\":|,", "", unlist(regmatches (events, gregexpr("period\\\":[0-9],", events))))
      type <- gsub("event\\\":\\\"", "", unlist(regmatches (events, gregexpr("event\\\":\\\"[a-zA-Z -]+", events))))
      location <- gsub("location\\\":|[[]|[]]|", "", unlist(regmatches (events, gregexpr("location\\\":[[][0-9a-zA-Z,-]+[]]", events))))
      
      # Rename event types / Renommer types de jeu
      type[type == "hit"] <- "HIT"; type[type == "score"] <- "GOAL"; type[type == "penalty"] <- "PENL"
      type[type == "shot-on-goal"] <- "SHOT"; type[type == "shot-missed"] <- "MISS"; type[type == "shot-blocked"] <- "BLOCK"
      
      # Parse time / Traiter temps
      timemat <- data.frame(matrix(as.numeric(unlist(strsplit(as.character(time), ":"))), byrow = TRUE, ncol = 2))
      seconds <- 1200*(as.numeric(period) - 1) + timemat$X1*60 + timemat$X2
      
      # Parse coordinates / Traiter coordinées
      locmat <- data.frame(matrix(as.numeric(unlist(strsplit(as.character(location), ","))), byrow = TRUE, ncol = 2))
      
      sn.table <- cbind(seconds, type, locmat) %>% data.frame() %>% rename(xc = X1, yc = X2)
      
      # Match with PBP / Associer au résumé
      pbp.new <- group_by(pbp.new, Seconds, Event) %>% 
        mutate(XC = first(sn.table$xc[which(sn.table$seconds == round(as.numeric(as.character(Seconds)), 0) & as.character(sn.table$type) == as.character(Event))]),
               YC = first(sn.table$yc[which(sn.table$seconds == round(as.numeric(as.character(Seconds)), 0) & as.character(sn.table$type) == as.character(Event))]))
      
    } else {
      
      # Provide date / Fournir date
      day <- gsub("-", "", as.character(date))
      
      # Scrape main page / Acquérir page primaire 
      url <- paste("http://scores.espn.go.com/nhl/scoreboard?date=", day, sep = "")
      glist <- try(getURL(url, header = FALSE,
                          .opts = curlOptions(
                            referer = 'sports.espn.go.com',
                            verbose = TRUE,
                            header = TRUE,
                            followLocation = TRUE,
                            useragent = agents[sample(1:4, 1)]))
      )
      
      if (class(glist) == "try-error") {
        glist <- getURL(url, header = FALSE,
                        .opts = curlOptions(
                          referer = 'sports.espn.go.com',
                          verbose = TRUE,
                          header = TRUE,
                          followLocation = TRUE,
                          useragent = agents[sample(1:4, 1)]))
      }
      
      gameids <- unique(unlist(regmatches(glist, gregexpr("gameId=[0-9]+", glist))))
      teams <- toupper(gsub("team/_/name/|>|</div>", "", unique(unlist(regmatches(glist, gregexpr("team/_/name/[a-zA-Z]+|>(Coyotes|Thrashers)</div>", glist)))))) # FIX FOR PRE-20112012 / SOLUTION POUR AVANT 20112012
      
      # Format team names / Changer noms d'équipes 
      teams[which(teams == "PHX")] <- "ARI"
      teams[which(teams == "TB")] <- "T.B"
      teams[which(teams == "NJ")] <- "N.J"
      teams[which(teams == "SJ")] <- "S.J"
      teams[which(teams == "LA")] <- "L.A"
      teams[which(teams == "COYOTES")] <- "ARI" # FIX FOR PRE-20112012 / SOLUTION POUR AVANT 20112012
      teams[which(teams == "THRASHERS")] <- "ATL" # FIX FOR PRE-20112012 / SOLUTION POUR AVANT 20112012
      
      if (as.numeric(season) < 20110000) {
        teams[which(teams == "WPG")] <- "ATL"
      } # FIX FOR PRE-20112012 / SOLUTION POUR AVANT 20112012
      
      teammat <- matrix(unique(teams), byrow = TRUE, ncol = 2) %>% data.frame()
      
      # Create URL directory / Créer annuaire de URLs
      url.match <- cbind(gameids, teammat) %>% data.frame() %>% rename(awayteam = X1, hometeam = X2)
      
      # Match URL / Associer URL
      urlt <- first(as.character(url.match$gameids[which(as.character(url.match$awayteam) == as.character(awayteam) | as.character(url.match$hometeam) == as.character(awayteam))]))
      
      ####################################################################################################################################################################################
      ####################################################################################################################################################################################
      
      # Scrape game page / Acquérir page du match
      url2 <- paste("http://sports.espn.go.com/nhl/gamecast/data/masterFeed?lang=en&isAll=true&rand=0&", urlt, sep = "")
      gamepage <- try(getURL(url2, header = FALSE,
                             .opts = curlOptions(
                               referer = 'sports.espn.go.com',
                               verbose = TRUE,
                               header = TRUE,
                               followLocation = TRUE,
                               useragent = agents[sample(1:4, 1)]))
      )
      
      if (class(gamepage) == "try-error") {
        gamepage <- getURL(url2, header = FALSE,
                           .opts = curlOptions(
                             referer = 'sports.espn.go.com',
                             verbose = TRUE,
                             header = TRUE,
                             followLocation = TRUE,
                             useragent = agents[sample(1:4, 1)]))
      }
      
      enames = c("FAC", "HIT", "GvTk", "GOAL", "SHOT", "MISS", "BLOCK", "PENL",
                 "STOP", "PRDY", "PSTR", "PEND", "PERD", "SOC", "GEnd", "SOut",
                 "error", "TAKE", "GIVE", "early intermission", "nothing", "nothing")
      ecodes = as.character(c(502, 503, 504, 505, 506, 507, 508, 509,
                              516, 517, 518, 519, 520, 521, 522, 0, 
                              9999, 1401, 1402, -2147483648, 1, 5))
      
      etext <- unlist(regmatches(gamepage, gregexpr("<Play.*?/Play>", gamepage)))
      
      if (length(etext) > 1) {
        esplit <- t(do.call(cbind, strsplit(etext, "[\\[~]")))
        esplit <- esplit[,c(5,3,4,6,7,11)]
        colnames(esplit) <- c("etype","xc","yc","time","period","event.description")
        esplit <- esplit[,1:5] %>% as.data.frame(stringsAsFactors = FALSE)
        
        esplit$etype <- enames[match(esplit$etype, ecodes)]
        
        timesplits <- do.call(rbind, strsplit(esplit$time, ":"))
        seconds <- 1200*(as.numeric(esplit$period) - 1) + as.numeric(timesplits[,1])*60 + as.numeric(timesplits[,2])
        esplit$seconds <- seconds
        
        # Match with PBP / Associer au résumé
        pbp.new <- group_by(pbp.new, Seconds, Event) %>% 
          mutate(XC = first(esplit$xc[which(esplit$seconds == round(as.numeric(as.character(Seconds)), 0) & as.character(esplit$etype) == as.character(Event))]),
                 YC = first(esplit$yc[which(esplit$seconds == round(as.numeric(as.character(Seconds)), 0) & as.character(esplit$etype) == as.character(Event))]))
      } else {
        
        pbp.new$XC <- NA; pbp.new$YC <- NA
        
      }
    }
    
    ########################################################################################################################################################################################################
    ########################################################################################################################################################################################################
    ########################################################################################################################################################################################################
    
    # Fix duplicate names / Réparer noms doubles
    roster$Full.Name[which(roster$Full.Name == "ERIK.KARLSSON" & roster$Team == "CAR")] <- "ERIK.KARLSSON.2"
    roster$Full.Name[which(roster$Full.Name == "ERIK.GUSTAFSSON" & roster$Team == "PHI")] <- "ERIK.GUSTAFSSON.2"
    roster$Full.Name[which(roster$Full.Name == "PK.SUBBAN" | roster$Full.Name == "P.K.SUBBAN")] <- "P.K..SUBBAN"
    roster$Full.Name[which(roster$Full.Name == "TJ.OSHIE" | roster$Full.Name == "T.J.OSHIE")] <- "T.J..OSHIE"
    roster$Full.Name[which(roster$Full.Name == "BJ.CROMBEEN" | roster$Full.Name == "B.J.CROMBEEN" | roster$Full.Name == "BRANDON.CROMBEEN")] <- "B.J..CROMBEEN"
    roster$Full.Name[which(roster$Full.Name == "ILJA.BRYZGALOV")] <- "ILYA.BRYZGALOV"
    roster$Full.Name[which(roster$Full.Name == "CAMERON.BARKER")] <- "CAM.BARKER"
    roster$Full.Name[which(roster$Full.Name == "CHRIS.VANDE VELDE")] <- "CHRIS.VANDEVELDE"
    roster$Full.Name[which(roster$Full.Name == "DANIEL.CARCILLO")] <- "DAN.CARCILLO"
    roster$Full.Name[which(roster$Full.Name == "DANIEL.CLEARY")] <- "DAN.CLEARY"
    roster$Full.Name[which(roster$Full.Name == "DAVID JOHNNY.ODUYA")] <- "JOHNNY.ODUYA"
    roster$Full.Name[which(roster$Full.Name == "DAVID.BOLLAND")] <- "DAVE.BOLLAND"
    roster$Full.Name[which(roster$Full.Name == "DWAYNE.KING")] <- "DJ.KING"
    roster$Full.Name[which(roster$Full.Name == "EVGENII.DADONOV")] <- "EVGENY.DADONOV"
    roster$Full.Name[which(roster$Full.Name == "FREDDY.MODIN")] <- "FREDRIK.MODIN"
    roster$Full.Name[which(roster$Full.Name == "HARRISON.ZOLNIERCZYK")] <- "HARRY.ZOLNIERCZYK"
    roster$Full.Name[which(roster$Full.Name == "J P.DUMONT" | roster$Full.Name == "JEAN-PIERRE.DUMONT")] <- "J-P.DUMONT"
    roster$Full.Name[which(roster$Full.Name == "JEAN-FRANCOIS.JACQUES")] <- "J-F.JACQUES"
    roster$Full.Name[which(roster$Full.Name == "JONATHAN.AUDY-MARCHESSAULT")] <- "JONATHAN.MARCHESSAULT"
    roster$Full.Name[which(roster$Full.Name == "JOSHUA.HENNESSY")] <- "JOSH.HENNESSY"
    roster$Full.Name[which(roster$Full.Name == "KRISTOPHER.LETANG")] <- "KRIS.LETANG"
    roster$Full.Name[which(roster$Full.Name == "KRYSTOFER.BARCH")] <- "KRYS.BARCH"
    roster$Full.Name[which(roster$Full.Name == "MARTIN.ST LOUIS")] <- "MARTIN.ST. LOUIS"
    roster$Full.Name[which(roster$Full.Name == "MATTHEW.CARLE")] <- "MATT.CARLE"
    roster$Full.Name[which(roster$Full.Name == "MATTHEW.DUMBA")] <- "MATT.DUMBA"
    roster$Full.Name[which(roster$Full.Name == "JOSEPH.CORVO")] <- "JOE.CORVO"
    roster$Full.Name[which(roster$Full.Name == "TOBY.ENSTROM")] <- "TOBIAS.ENSTROM"
    roster$Full.Name[which(roster$Full.Name == "MICHAEL.SANTORELLI")] <- "MIKE.SANTORELLI"
    roster$Full.Name[which(roster$Full.Name == "MICHAEL.CAMMALLERI")] <- "MIKE.CAMMALLERI"
    roster$Full.Name[which(roster$Full.Name == "PIERRE.PARENTEAU" | roster$Full.Name == "PIERRE-ALEX.PARENTEAU")] <- "PA.PARENTEAU"
    roster$Full.Name <- gsub("ALEXANDER.|ALEXANDRE.", "ALEX.", roster$Full.Name)
    roster$Full.Name <- gsub("CHRISTOPHER.", "CHRIS.", roster$Full.Name)
    roster$Full.Name[which(roster$Full.Name == "NICOLAS.PETAN")] <- "NIC.PETAN"
    roster$Full.Name[which(roster$Full.Name == "NIKOLAI.KULEMIN")] <- "NIKOLAY.KULEMIN"
    
    # Add roster match code / Ajouter code d'association
    roster$Player.Code <- gsub("[^A-Za-z]", "", roster$Full.Name)
    
    # Replace teamnum / Remplacer code équipenum
    if(names == TRUE) {
      pbp.new$p1 <- roster$Full.Name[match(pbp.new$p1, roster$Team.Num)]; pbp.new$p2 <- roster$Full.Name[match(pbp.new$p2, roster$Team.Num)]; pbp.new$p3 <- roster$Full.Name[match(pbp.new$p3, roster$Team.Num)]
      pbp.new$a1.num <- roster$Full.Name[match(pbp.new$a1.num, roster$Team.Num)]; pbp.new$a2.num <- roster$Full.Name[match(pbp.new$a2.num, roster$Team.Num)]; pbp.new$a3.num <- roster$Full.Name[match(pbp.new$a3.num, roster$Team.Num)]
      pbp.new$a4.num <- roster$Full.Name[match(pbp.new$a4.num, roster$Team.Num)]; pbp.new$a5.num <- roster$Full.Name[match(pbp.new$a5.num, roster$Team.Num)]; pbp.new$a6.num <- roster$Full.Name[match(pbp.new$a6.num, roster$Team.Num)]
      pbp.new$h1.num <- roster$Full.Name[match(pbp.new$h1.num, roster$Team.Num)]; pbp.new$h2.num <- roster$Full.Name[match(pbp.new$h2.num, roster$Team.Num)]; pbp.new$h3.num <- roster$Full.Name[match(pbp.new$h3.num, roster$Team.Num)]
      pbp.new$h4.num <- roster$Full.Name[match(pbp.new$h4.num, roster$Team.Num)]; pbp.new$h5.num <- roster$Full.Name[match(pbp.new$h5.num, roster$Team.Num)]; pbp.new$h6.num <- roster$Full.Name[match(pbp.new$h6.num, roster$Team.Num)]
      pbp.new$Away.Goalie <- roster$Full.Name[match(pbp.new$Away.Goalie, roster$Team.Num)]; pbp.new$Home.Goalie <- roster$Full.Name[match(pbp.new$Home.Goalie, roster$Team.Num)]
    }
    
    # Populate lists / Peupler listes
    pbp.list[[j]] <- c(t(pbp.new))
    cnames1 <- colnames(pbp.new)
    
    roster.list[[j]] <- c(t(roster))
    cnames2 <- colnames(roster)
    
    cat(paste("Pausing", pause, "seconds...\n"))
    Sys.sleep(pause)
    
  }
  
  # Unlist into tables / Remplir tables
  pbp.full <- matrix(unlist(pbp.list), byrow = T, ncol = 53) %>% as.data.frame(stringsAsFactors = FALSE)
  colnames(pbp.full) <- cnames1
  pbp.full$Season <- season
  pbp.full$Season.Type <- "Regular"
  pbp.full$Season.Type[which(as.numeric(as.character(pbp.full$Game.ID)) >= 30000)] <- "Playoffs"
  
  roster.full <- matrix(unlist(roster.list), byrow = T, ncol = 11) %>% data.frame()
  colnames(roster.full) <- cnames2
  roster.full$Season <- season
  roster.full$Season.Type <- "Regular"
  roster.full$Season.Type[which(as.numeric(as.character(roster.full$Game.ID)) >= 30000)] <- "Playoffs"
  
  pbp.full <<- pbp.full
  roster.full <<- roster.full
  
  # Print record of missing games / Imprimer un record de matchs manquants
  games <- start:end
  missing <- games[which(games %in% unique(pbp.full$Game.ID) == FALSE)]
  
  if (length(missing) > 0) {
    cat(paste("Oops! Games missing:", paste(missing, collapse = ", "), "\n"))
  } else {
    cat("Success! All games were scraped.\n")
  }
  
  return(TRUE)
  
}

bios <- function(teams) {
  
  bio.list <- NULL
  
  agents <- c("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36",
              "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36",
              "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.5.2171.95 Safari/537.36",
              "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.86 Safari/537.36")
  
  # Generate list of teams / Créer liste d'équipes
  teams.full <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ",
                  "CGY", "CHI", "COL", "DAL", "DET", "EDM",
                  "FLA", "LAK", "MIN", "MTL", "NJD", "NSH",
                  "NYI", "NYR", "OTT", "PHI", "PIT", "SJS",
                  "STL", "TBL", "TOR", "VAN", "WPG", "WSH",
                  "PHX")
  
  if (tolower(first(teams)) == "all") {
    teamlist <- teams.full
  } else {
    teamlist <- toupper(teams)[which(toupper(teams) %in% teams.full)]
  }
  
  not.teams <- teams[which(toupper(teams) %in% teams.full == FALSE & tolower(teams) != "all")]
  
  # Begin loop / Commencer cycle
  for (i in 1:length(unique(teamlist))) {
    
    team <- unique(teamlist)[i]
    
    # Scrape team roster page / Acquérir données sur formation d'équipe
    url <- paste("http://nhlwc.cdnak.neulion.com/fs1/nhl/league/teamroster/", team, "/iphone/clubroster.json", sep = "")
    url.text <- try(getURL(url, header = FALSE,
                           .opts = curlOptions(
                             referer = 'nhl.com',
                             verbose = TRUE,
                             header = TRUE,
                             followLocation = TRUE,
                             useragent = agents[sample(1:4, 1)]))
    )
    
    if (class(url.text) == "try-error") {
      url.text <- getURL(url, header = FALSE,
                         .opts = curlOptions(
                           referer = 'nhl.com',
                           verbose = TRUE,
                           header = TRUE,
                           followLocation = TRUE,
                           useragent = agents[sample(1:4, 1)]))
    }
    
    # Parse bio data / Traiter information biographique
    split <- unlist(strsplit(url.text, "\\},\\{|:\\[\\{"))
    
    timestamp <- unlist(regmatches(split[1], gregexpr("[A-Za-z ]+ [0-9 :]+ [A-Z]+ [0-9]{4,}", split[1])))
    
    name.raw <- unlist(regmatches(split[-1], gregexpr("name\\\":.+?,", split[-1])))
    name <- gsub("name\\\":\\\"|\\\",", "", name.raw)
    
    weight.raw <- unlist(regmatches(split[-1], gregexpr("weight\\\":.+?,", split[-1])))
    weight <- gsub("weight\\\":|,", "", weight.raw)
    
    height.raw <- unlist(regmatches(split[-1], gregexpr("height\\\":\\\".+?,", split[-1])))
    height <- gsub("height\\\":\\\"|\\\\\\\"\\\",| ", "", height.raw)
    
    bd.raw <- unlist(regmatches(split[-1], gregexpr("birthdate\\\":\\\".+?\\\"", split[-1])))
    bd <- gsub("birthdate\\\":\\\"|\\\"", "", bd.raw)
    
    age.raw <- unlist(regmatches(split[-1], gregexpr("age\\\":.+?,", split[-1])))
    age <- gsub("age\\\":|,", "", age.raw)
    
    piclink.raw <- unlist(regmatches(split[-1], gregexpr("imageUrl\\\":\\\".+?\\\"", split[-1])))
    piclink <- gsub("imageUrl\\\":\\\"|\\\"", "", piclink.raw)
    
    # Build table / Construire table
    bio <- cbind(name, weight, height, bd, age, piclink) %>% data.frame() %>%
      mutate(Player.Code = toupper(gsub("[^A-Za-z]", "", name)), Timestamp = timestamp) %>% data.frame()
    
    # Populate lists / Peupler listes
    bio.list[[i]] <- c(t(bio))
    cnames1 <- colnames(bio)
    
  }
  
  # Unlist into tables / Remplir tables
  bio.full <- matrix(unlist(bio.list), byrow = T, ncol = 8) %>% as.data.frame(stringsAsFactors = FALSE)
  colnames(bio.full) <- cnames1
  
  bio.full <<- bio.full
  
  # Print record of missing teams / Imprimer un record d'équipes manquants
  if (length(not.teams) > 0) {
    cat(paste("Oops! The following are not properly formatted team names:", paste(not.teams, collapse = ", "), "\n"))
  } else {
    cat("Success!\n")
  }
  
}

quick_scrape <- function(season, games, names = TRUE, pause) {
  
  pbp.list <- NULL
  roster.list <- NULL
  
  agents <- c("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36",
              "Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36",
              "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.5.2171.95 Safari/537.36",
              "Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.86 Safari/537.36")
  
  for (i in 1:length(games)) {
    
    ########################################################################################################################################################################################################
    ########################## SCRAPE PLAY-BY-PLAY / ACQUÉRIR RÉSUMÉ DU MATCH ##############################################################################################################################
    ########################################################################################################################################################################################################
    
    # Define URL / Definir URL
    ID <- as.character(games[i])
    cat(ID)
    url <- paste("http://www.nhl.com/scores/htmlreports/", season, "/PL0", ID, ".HTM", sep = "")
    
    url.text <- try(getURL(url, header = FALSE,
                           .opts = curlOptions(
                             referer = 'nhl.com',
                             verbose = TRUE,
                             header = TRUE,
                             followLocation = TRUE,
                             useragent = agents[sample(1:4, 1)]))
    )
    
    if(class(url.text) == "try-error") {
      url.text <- getURL(url, header = FALSE,
                         .opts = curlOptions(
                           referer = 'nhl.com',
                           verbose = TRUE,
                           header = TRUE,
                           followLocation = TRUE,
                           useragent = agents[sample(1:4, 1)]))
    }
    
    # Create HTML object / Créer objet HTML
    html <- read_html(url.text)
    
    # Scrape text / Acquérir texte 
    all <- html_nodes(html, "td")
    body <- html_nodes(html, ".bborder")
    full.text <- html_text(all)
    body.text <- html_text(body)
    
    # Skip game if file is broken / Proceder au prochain match si le fichier est incomplet 
    if (length(full.text) < 500) {next}
    
    pbp.raw <- matrix(body.text, byrow = TRUE, ncol = 8) %>% data.frame() %>% filter(X2 != "Per")
    
    # Team list / Liste d'équipes 
    teamlist <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ",
                  "CGY", "CHI", "COL", "DAL", "DET", "EDM",
                  "FLA", "L.A", "MIN", "MTL", "N.J", "NSH",
                  "NYI", "NYR", "OTT", "PHI", "PIT", "S.J",
                  "STL", "T.B", "TOR", "VAN", "WPG", "WSH",
                  "PHX", "ATL")
    
    # Get teams / Obtenir équipes
    hometeam <- gsub(" On Ice", "", body.text[8])
    awayteam <- gsub(" On Ice", "", body.text[7])
    
    hometeam[which(hometeam == "PHX")] <- "ARI"; awayteam[which(awayteam == "PHX")] <- "ARI"
    
    teams <- c(awayteam, hometeam)
    
    # Date, game and etc. data / Date, match, etc.
    date <- gsub("^[a-zA-Z]*, ", "", full.text[grep("^[a-zA-Z]*, ", full.text)]) %>% as.Date(format = "%B %d, %Y") %>% first() %>% as.character()
    Date <- rep(date, time = length(pbp.raw$X1))
    
    Game.ID <- rep(ID, times = length(pbp.raw$X1))
    
    Home.Team <- rep(hometeam, times = length(pbp.raw$X1))
    Away.Team <- rep(awayteam, times = length(pbp.raw$X1))
    
    Duration <- rep(NA, times = length(pbp.raw$X1))
    
    # Parse time / Traiter temps
    timemat <- data.frame(matrix(as.numeric(unlist(strsplit(as.character(pbp.raw$X4), ":"))), byrow = TRUE, ncol = 3))
    
    Seconds <- 1200*(as.numeric(pbp.raw$X2) - 1) + timemat$X1*60 + (timemat$X3 > 0)*(60 - timemat$X3)
    Seconds[which(as.numeric(pbp.raw$X2) == 5)] <- 3900.001
    
    ## Parse on-ice / Traiter joueurs sur glace
    stretch <- function(x) {
      t <- as.character(unlist(x))
      t2 <- list(c(t, rep(c(0, NA), times = (12 - (length(t)/2)))))
      return(t2)
    }
    
    # Away / étrangère 
    a.match <- regmatches(as.character(pbp.raw$X7), gregexpr("[0-9|A-Z]+", as.character(pbp.raw$X7)))
    a.new <- lapply(a.match, stretch)
    Away.On <- data.frame(matrix(unlist(a.new), byrow = TRUE, ncol = 24))
    colnames(Away.On) <- c("a1.num", "a1.pos", "a2.num", "a2.pos", "a3.num", "a3.pos", "a4.num", "a4.pos", "a5.num", "a5.pos", "a6.num", "a6.pos",
                           "a7.num", "a7.pos", "a8.num", "a8.pos", "a9.num", "a9.pos", "a10.num", "a10.pos", "a11.num", "a11.pos", "a12.num", "a12.pos")
    
    # Home / Domicile 
    h.match <- regmatches(as.character(pbp.raw$X8), gregexpr("[0-9|A-Z]+", as.character(pbp.raw$X8)))
    h.new <- lapply(h.match, stretch)
    Home.On <- data.frame(matrix(unlist(h.new), byrow = TRUE, ncol = 24))
    colnames(Home.On) <- c("h1.num", "h1.pos", "h2.num", "h2.pos", "h3.num", "h3.pos", "h4.num", "h4.pos", "h5.num", "h5.pos", "h6.num", "h6.pos", 
                           "h7.num", "h7.pos", "h8.num", "h8.pos", "h9.num", "h9.pos", "h10.num", "h10.pos","h11.num", "h11.pos", "h12.num", "h12.pos")
    
    ## Parse description / Traiter déscription 
    clean.nums <- function(x) {
      t <- gsub("#|ONGOAL - ", "", as.character(unlist(x)))
      t2 <- list(c(t, rep(NA, times = (3 - length(t)))))
      return(t2)
    }
    
    dummy.team <- function(x) {
      if (length(unlist(x)) > 0) {
        t <- x
      } else {
        t <- NA
      }
      return(t)
    }
    
    dummy.zone <- function(x) {
      if (length(unlist(x)) > 0) {
        t <- x
      } else {
        t <- NA
      }
      return(t)
    }
    
    dummy.detail <- function(x) {
      if (length(unlist(x)) > 0) {
        t <- paste(unlist(x), collapse = "")
      } else {
        t <- NA
      }
      return(t)
    }
    
    # Event team / équipe du jeu
    t.match <- regmatches(as.character(pbp.raw$X6), gregexpr(paste("(^", paste(teamlist, collapse = "|^"), ")", sep = ""), as.character(pbp.raw$X6)))
    t.new <- lapply(t.match, dummy.team)
    ev.team <- gsub(" ", "", as.character(unlist(t.new)))
    ev.team[which(ev.team == "PHX")] <- "ARI"
    
    # Event players / Joueurs du jeu
    d.match <- regmatches(as.character(pbp.raw$X6), gregexpr("#[0-9]+|ONGOAL - [0-9]+", as.character(pbp.raw$X6)))
    d.new <- lapply(d.match, clean.nums)
    ev.players <- data.frame(matrix(unlist(d.new), byrow = TRUE, ncol = 3))
    colnames(ev.players) <- c("p1", "p2", "p3")
    
    # Event zone / Zone du jeu
    z.match <- regmatches(as.character(pbp.raw$X6), gregexpr("[a-zA-Z]{3}. [zZ]one", as.character(pbp.raw$X6)))
    z.new <- lapply(z.match, dummy.zone)
    ev.zone <- gsub(". [zZ]one", "", as.character(unlist(z.new)))
    
    # Event details / Détails du jeu
    e.match <- regmatches(as.character(pbp.raw$X6), gregexpr(", [a-zA-Z|-]+,|[A-Z] .+[(].{4,}[)],|[A-Z] .+[(][a-zA-Z]{3,}[)],", as.character(pbp.raw$X6)))
    e.new <- lapply(e.match, dummy.detail)
    Detail <- gsub(",|, |[A-Z]+ |#[0-9]+ |[A-Z]{2,}.", "", as.character(unlist(e.new)))
    
    # On-ice goalies / Gardiens sur glace
    Home.Goalie <- (Home.On$h12.pos == "G" & !is.na(Home.On$h12.pos))*as.numeric(as.character(Home.On$h12.num)) + (Home.On$h11.pos == "G" & !is.na(Home.On$h11.pos))*as.numeric(as.character(Home.On$h11.num)) +
      (Home.On$h10.pos == "G" & !is.na(Home.On$h10.pos))*as.numeric(as.character(Home.On$h10.num)) + (Home.On$h9.pos == "G" & !is.na(Home.On$h9.pos))*as.numeric(as.character(Home.On$h9.num)) +
      (Home.On$h8.pos == "G" & !is.na(Home.On$h8.pos))*as.numeric(as.character(Home.On$h8.num)) + (Home.On$h7.pos == "G" & !is.na(Home.On$h7.pos))*as.numeric(as.character(Home.On$h7.num)) +
      (Home.On$h6.pos == "G" & !is.na(Home.On$h6.pos))*as.numeric(as.character(Home.On$h6.num)) + (Home.On$h5.pos == "G" & !is.na(Home.On$h5.pos))*as.numeric(as.character(Home.On$h5.num)) +
      (Home.On$h4.pos == "G" & !is.na(Home.On$h4.pos))*as.numeric(as.character(Home.On$h4.num)) + (Home.On$h3.pos == "G" & !is.na(Home.On$h3.pos))*as.numeric(as.character(Home.On$h3.num)) +
      (Home.On$h2.pos == "G" & !is.na(Home.On$h2.pos))*as.numeric(as.character(Home.On$h2.num)) + (Home.On$h1.pos == "G" & !is.na(Home.On$h1.pos))*as.numeric(as.character(Home.On$h1.num))
    
    Away.Goalie <- (Away.On$a12.pos == "G" & !is.na(Away.On$a12.pos))*as.numeric(as.character(Away.On$a12.num)) + (Away.On$a11.pos == "G" & !is.na(Away.On$a11.pos))*as.numeric(as.character(Away.On$a11.num)) +
      (Away.On$a10.pos == "G" & !is.na(Away.On$a10.pos))*as.numeric(as.character(Away.On$a10.num)) + (Away.On$a9.pos == "G" & !is.na(Away.On$a9.pos))*as.numeric(as.character(Away.On$a9.num)) +
      (Away.On$a8.pos == "G" & !is.na(Away.On$a8.pos))*as.numeric(as.character(Away.On$a8.num)) + (Away.On$a7.pos == "G" & !is.na(Away.On$a7.pos))*as.numeric(as.character(Away.On$a7.num)) +
      (Away.On$a6.pos == "G" & !is.na(Away.On$a6.pos))*as.numeric(as.character(Away.On$a6.num)) + (Away.On$a5.pos == "G" & !is.na(Away.On$a5.pos))*as.numeric(as.character(Away.On$a5.num)) +
      (Away.On$a4.pos == "G" & !is.na(Away.On$a4.pos))*as.numeric(as.character(Away.On$a4.num)) + (Away.On$a3.pos == "G" & !is.na(Away.On$a3.pos))*as.numeric(as.character(Away.On$a3.num)) +
      (Away.On$a2.pos == "G" & !is.na(Away.On$a2.pos))*as.numeric(as.character(Away.On$a2.num)) + (Away.On$a1.pos == "G" & !is.na(Away.On$a1.pos))*as.numeric(as.character(Away.On$a1.num))
    
    # Create PBP / Créer résumé
    pbp.new <- pbp.raw %>% select(-c(X1, X3, X4, X7, X8)) %>% cbind(Duration, Date, Game.ID, ev.team, ev.players, ev.zone, Detail, Seconds, Away.On[, 1:12], Home.On[, 1:12], Away.Team, Home.Team, Away.Goalie, Home.Goalie)
    
    ## Replace with teamnum ID / Remplacer avec code équipenum
    pbp.new <- rbind_list(
      filter(pbp.new, X5 == "FAC") %>% 
        mutate(p1 = paste(awayteam, p1, sep = ""), p2 = paste(hometeam, p2, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "HIT") %>% group_by(ev.team) %>%
        mutate(p1 = paste(first(ev.team), p1, sep = ""), p2 = paste(teams[which(teams != first(ev.team))], p2, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "SHOT") %>%
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "GIVE") %>%
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "MISS") %>%
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "GOAL") %>% 
        mutate(p1 = paste(ev.team, p1, sep = ""), 
               p2 = gsub(paste(paste(teamlist, collapse = "NA|"), "NA", sep = ""), NA, paste(ev.team, p2, sep = "")),
               p3 = gsub(paste(paste(teamlist, collapse = "NA|"), "NA", sep = ""), NA, paste(ev.team, p3, sep = ""))) %>% data.frame(),
      filter(pbp.new, X5 == "BLOCK") %>% group_by(ev.team) %>%
        mutate(p1 = paste(first(ev.team), p1, sep = ""), p2 = paste(teams[which(teams != first(ev.team))], p2, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 == "PENL") %>% group_by(ev.team) %>%
        mutate(p1 = paste(first(ev.team), p1, sep = ""), 
               p2 = gsub(paste(paste(teamlist, collapse = "NA|"), "NA", sep = ""), NA, paste(teams[which(teams != first(ev.team))], p2, sep = ""))) %>% data.frame(),
      filter(pbp.new, X5 == "TAKE") %>% 
        mutate(p1 = paste(ev.team, p1, sep = "")) %>% data.frame(),
      filter(pbp.new, X5 %in% c("FAC", "HIT", "SHOT", "GIVE", "MISS", "GOAL", "BLOCK", "PENL", "TAKE") == FALSE) %>% data.frame()
    ) %>% data.frame() %>% 
      mutate(a1.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a1.num, sep = "")),
             a2.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a2.num, sep = "")),
             a3.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a3.num, sep = "")),
             a4.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a4.num, sep = "")),
             a5.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a5.num, sep = "")),
             a6.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, a6.num, sep = "")),
             h1.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h1.num, sep = "")),
             h2.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h2.num, sep = "")),
             h3.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h3.num, sep = "")),
             h4.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h4.num, sep = "")),
             h5.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h5.num, sep = "")),
             h6.num = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, h6.num, sep = "")),
             Home.Goalie = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(hometeam, Home.Goalie, sep = "")),
             Away.Goalie = gsub(paste(paste(teamlist, collapse = "0|"), "0", sep = ""), NA, paste(awayteam, Away.Goalie, sep = "")),
             Home.Skaters = 6 - (is.na(h1.num)) - (is.na(h2.num)) - (is.na(h3.num)) - (is.na(h4.num)) - (is.na(h5.num)) - (is.na(h6.num)) - (!is.na(Home.Goalie)),
             Away.Skaters = 6 - (is.na(a1.num)) - (is.na(a2.num)) - (is.na(a3.num)) - (is.na(a4.num)) - (is.na(a5.num)) - (is.na(a6.num)) - (!is.na(Away.Goalie)),
             Seconds = Seconds - 0.01*(X5 %in% c("STOP", "PENL", "GOAL", "PEND")) + 0.01*(X5 == "FAC")) %>%
      rename(Period = X2, Event = X5, Description = X6) %>% arrange(Seconds) %>%
      mutate(Home.Score = cumsum(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Home.Team)) - 1*(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Home.Team)),
             Away.Score = cumsum(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Away.Team)) - 1*(as.character(Event) == "GOAL" & as.character(ev.team) == as.character(Away.Team))) %>%
      data.frame()
    
    # Re-assign event zone for blocked shots to perspective of shooting team / Re-attribuer zone du jeu pour tirs bloqués au point de vue de l'équipe tireur
    pbp.new$ev.zone[which(pbp.new$Event == "BLOCK" & pbp.new$ev.zone == "Def")] <- "Off"
    
    # Append strength and score states / Attacher états de forces et de score
    pbp.new$Strength.State <- paste(pbp.new$Home.Skaters, pbp.new$Away.Skaters, sep = "v"); pbp.new$Score.State <- paste(pbp.new$Home.Score, pbp.new$Away.Score, sep = "-")
    pbp.new$Score.Cat <- 1*(pbp.new$Home.Score - pbp.new$Away.Score == 1) + 2*(pbp.new$Home.Score - pbp.new$Away.Score == 2) + 3*(pbp.new$Home.Score - pbp.new$Away.Score >= 3) -
      1*(pbp.new$Home.Score - pbp.new$Away.Score == -1) - 2*(pbp.new$Home.Score - pbp.new$Away.Score == -2) - 3*(pbp.new$Home.Score - pbp.new$Away.Score <= -3)
    
    ########################################################################################################################################################################################################
    ########################## SCRAPE SHIFT REPORTS / ACQUÉRIR RAPPORTS DE PRÉSENCES #######################################################################################################################
    ########################################################################################################################################################################################################
    
    # Define URLs / Définir URLs
    url1 <- paste("http://www.nhl.com/scores/htmlreports/", season, "/TH0", ID, ".HTM", sep = "") # Home / Domicile
    url2 <- paste("http://www.nhl.com/scores/htmlreports/", season, "/TV0", ID, ".HTM", sep = "") # Away / Étrangère
    
    url1.text <- try(getURL(url1, header = FALSE,
                            .opts = curlOptions(
                              referer = 'nhl.com',
                              verbose = TRUE,
                              header = TRUE,
                              followLocation = TRUE,
                              useragent = agents[sample(1:4, 1)]))
    )
    
    url2.text <- try(getURL(url2, header = FALSE,
                            .opts = curlOptions(
                              referer = 'nhl.com',
                              verbose = TRUE,
                              header = TRUE,
                              followLocation = TRUE,
                              useragent = agents[sample(1:4, 1)]))
    )
    
    if(class(url1.text) == "try-error" | class(url2.text) == "try-error") {
      url1.text <- getURL(url1, header = FALSE,
                          .opts = curlOptions(
                            referer = 'nhl.com',
                            verbose = TRUE,
                            header = TRUE,
                            followLocation = TRUE,
                            useragent = agents[sample(1:4, 1)]))
      
      url2.text <- getURL(url2, header = FALSE,
                          .opts = curlOptions(
                            referer = 'nhl.com',
                            verbose = TRUE,
                            header = TRUE,
                            followLocation = TRUE,
                            useragent = agents[sample(1:4, 1)]))
    }
    
    # Create HTML objects / Créer objets HTML
    html1 <- read_html(url1.text) # Home / Domicile
    html2 <- read_html(url2.text) # Away / Étrangère
    
    # Scrape tables / Acquérir tables
    home.text.1 <- html_nodes(html1, ".border")
    away.text.1 <- html_nodes(html2, ".border")
    home.text.2 <- html_nodes(html1, ".bborder")
    away.text.2 <- html_nodes(html2, ".bborder")
    
    home.outer <- html_text(home.text.1)
    away.outer <- html_text(away.text.1)
    home.inner <- html_text(home.text.2)
    away.inner <- html_text(away.text.2)
    
    # Skip game if file is broken / Proceder au prochain match si le fichier est incomplet 
    if (length(home.inner) < 1 | length(away.inner) < 1) {next}
    
    hometeam.full <- home.outer[1]
    home.players <- home.outer[-1]
    home.players <- home.players[which(grepl("^[0-9]+", home.players) == TRUE)] # FIX FOR 20132014-20934 / SOLUTION POUR 20132014-20934
    awayteam.full <- away.outer[1]
    away.players <- away.outer[-1]
    away.players <- away.players[which(grepl("^[0-9]+", away.players) == TRUE)] # FIX FOR 20132014-20934 / SOLUTION POUR 20132014-20934
    
    # Create roster table / Créer table de formation
    roster <- rbind_list(cbind(rep(hometeam, times = length(home.players)), home.players) %>% data.frame() %>% rename(Num.Last.First = home.players),
                         cbind(rep(awayteam, times = length(away.players)), away.players) %>% data.frame() %>% rename(Num.Last.First = away.players)) %>%
      data.frame()
    
    namemat <- data.frame(matrix(as.character(unlist(strsplit(gsub("^[0-9]+ ", "", roster$Num.Last.First), ", "))), byrow = T, ncol = 2))
    
    roster$Game.ID <- rep(ID, times = length(roster$Num.Last.First))
    roster$Date <- rep(date, times = length(roster$Num.Last.First))
    roster$Number <- unlist(regmatches(as.character(roster$Num.Last.First), gregexpr("^[0-9]+", as.character(roster$Num.Last.First))))
    roster$Last.Name <- namemat$X1
    roster$First.Name <- namemat$X2
    
    posmatch <- rbind_list(group_by(pbp.new, a1.num) %>% rename(player = a1.num) %>% 
                             summarise(C = sum(a1.pos == "C"), L = sum(a1.pos == "L"), R = sum(a1.pos == "R"), D = sum(a1.pos == "D"), G = sum(a1.pos == "G"), N = sum(a1.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a2.num) %>% rename(player = a2.num) %>% 
                             summarise(C = sum(a2.pos == "C"), L = sum(a2.pos == "L"), R = sum(a2.pos == "R"), D = sum(a2.pos == "D"), G = sum(a2.pos == "G"), N = sum(a2.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a3.num) %>% rename(player = a3.num) %>% 
                             summarise(C = sum(a3.pos == "C"), L = sum(a3.pos == "L"), R = sum(a3.pos == "R"), D = sum(a3.pos == "D"), G = sum(a3.pos == "G"), N = sum(a3.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a4.num) %>% rename(player = a4.num) %>% 
                             summarise(C = sum(a4.pos == "C"), L = sum(a4.pos == "L"), R = sum(a4.pos == "R"), D = sum(a4.pos == "D"), G = sum(a4.pos == "G"), N = sum(a4.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a5.num) %>% rename(player = a5.num) %>% 
                             summarise(C = sum(a5.pos == "C"), L = sum(a5.pos == "L"), R = sum(a5.pos == "R"), D = sum(a5.pos == "D"), G = sum(a5.pos == "G"), N = sum(a5.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, a6.num) %>% rename(player = a6.num) %>% 
                             summarise(C = sum(a6.pos == "C"), L = sum(a6.pos == "L"), R = sum(a6.pos == "R"), D = sum(a6.pos == "D"), G = sum(a6.pos == "G"), N = sum(a6.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h1.num) %>% rename(player = h1.num) %>% 
                             summarise(C = sum(h1.pos == "C"), L = sum(h1.pos == "L"), R = sum(h1.pos == "R"), D = sum(h1.pos == "D"), G = sum(h1.pos == "G"), N = sum(h1.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h2.num) %>% rename(player = h2.num) %>% 
                             summarise(C = sum(h2.pos == "C"), L = sum(h2.pos == "L"), R = sum(h2.pos == "R"), D = sum(h2.pos == "D"), G = sum(h2.pos == "G"), N = sum(h2.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h3.num) %>% rename(player = h3.num) %>% 
                             summarise(C = sum(h3.pos == "C"), L = sum(h3.pos == "L"), R = sum(h3.pos == "R"), D = sum(h3.pos == "D"), G = sum(h3.pos == "G"), N = sum(h3.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h4.num) %>% rename(player = h4.num) %>% 
                             summarise(C = sum(h4.pos == "C"), L = sum(h4.pos == "L"), R = sum(h4.pos == "R"), D = sum(h4.pos == "D"), G = sum(h4.pos == "G"), N = sum(h4.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h5.num) %>% rename(player = h5.num) %>% 
                             summarise(C = sum(h5.pos == "C"), L = sum(h5.pos == "L"), R = sum(h5.pos == "R"), D = sum(h5.pos == "D"), G = sum(h5.pos == "G"), N = sum(h5.pos %in% c("C", "L", "R", "D", "G") == F)),
                           group_by(pbp.new, h6.num) %>% rename(player = h6.num) %>% 
                             summarise(C = sum(h6.pos == "C"), L = sum(h6.pos == "L"), R = sum(h6.pos == "R"), D = sum(h6.pos == "D"), G = sum(h6.pos == "G"), N = sum(h6.pos %in% c("C", "L", "R", "D", "G") == F))) %>%
      data.frame() %>% group_by(player) %>%
      summarise(C = sum(C), L = sum(L), R = sum(R), D = sum(D), G = sum(G), N = sum(N)) %>% 
      mutate(Pos.Num = 1*(C > L & C > R & C > D & C > G & C > N) +
               2*(L > C & L > R & L > D & L > G & L > N) +
               3*(R > L & R > C & R > D & R > G & R > N) +
               4*(D > L & D > R & D > C & D > G & D > N) +
               5*(G > C & G > L & G > R & G > D & G > N) +
               6*(N > C & N > L & N > R & N > D & N > G)) %>%
      data.frame()
    
    posmatch$Pos <- colnames(posmatch)[-1][posmatch$Pos.Num[1:nrow(posmatch)]]
    
    roster <- roster %>% mutate(Team.Num = paste(V1, Number, sep = ""),
                                Full.Name = paste(First.Name, Last.Name, sep = "."),
                                Position = posmatch$Pos[match(Team.Num, posmatch$player)]) %>%
      rename(Team = V1) %>% data.frame()
    
    # Create shift tables / Créer tables de présences
    shiftlist.home <- NULL
    shiftlist.away <- NULL
    
    for (j in 1:(length(home.outer)-1)) {
      shiftlist.home[[j]] <- home.inner[which(home.inner == "Shift #" | home.inner == "Présence #Shift #")[j]:(which(home.inner == "SHF" | home.inner == "PR/SHF")[j]-3)]
    }
    
    for (j in 1:(length(away.outer)-1)) {
      shiftlist.away[[j]] <- away.inner[which(away.inner == "Shift #" | away.inner == "Présence #Shift #")[j]:(which(away.inner == "SHF" | away.inner == "PR/SHF")[j]-3)]
    }
    
    htoi.raw <- matrix(unlist(shiftlist.home), byrow = TRUE, ncol = 6) %>% data.frame()
    atoi.raw <- matrix(unlist(shiftlist.away), byrow = TRUE, ncol = 6) %>% data.frame()
    
    htoi.raw$p.match <- cumsum(htoi.raw$X2 == "Per")
    htoi.raw$Player <- home.players[htoi.raw$p.match[1:nrow(htoi.raw)]]
    htoi.raw <- filter(htoi.raw, X2 != "Per")
    
    atoi.raw$p.match <- cumsum(atoi.raw$X2 == "Per")
    atoi.raw$Player <- away.players[atoi.raw$p.match[1:nrow(atoi.raw)]]
    atoi.raw <- filter(atoi.raw, X2 != "Per")
    
    startmat.home <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(htoi.raw$X3), " ")), ":"))), byrow = TRUE, ncol = 5))
    endmat.home <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(htoi.raw$X4), " ")), ":"))), byrow = TRUE, ncol = 5))
    startmat.away <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(atoi.raw$X3), " ")), ":"))), byrow = TRUE, ncol = 5))
    endmat.away <- data.frame(matrix(as.numeric(unlist(strsplit(unlist(strsplit(as.character(atoi.raw$X4), " ")), ":"))), byrow = TRUE, ncol = 5))
    
    startsec.home <- 1200*(as.numeric(htoi.raw$X2) - 1) + startmat.home$X1*60 + startmat.home$X2
    endsec.home <- 1200*(as.numeric(htoi.raw$X2) - 1) + endmat.home$X1*60 + endmat.home$X2
    startsec.away <- 1200*(as.numeric(atoi.raw$X2) - 1) + startmat.away$X1*60 + startmat.away$X2
    endsec.away <- 1200*(as.numeric(atoi.raw$X2) - 1) + endmat.away$X1*60 + endmat.away$X2
    
    htoi.new <- htoi.raw %>% select(-c(X1, X3:X6, p.match)) %>% cbind(roster[match(htoi.raw$Player, roster$Num.Last.First), c(3,4,1,5,8,9)], startsec.home, endsec.home) %>%
      mutate(Duration = endsec.home - startsec.home) %>% data.frame()
    atoi.new <- atoi.raw %>% select(-c(X1, X3:X6, p.match)) %>% cbind(roster[match(atoi.raw$Player, roster$Num.Last.First), c(3,4,1,5,8,9)], startsec.away, endsec.away) %>% 
      mutate(Duration = endsec.away - startsec.away) %>% data.frame()
    
    colnames(htoi.new) <- c("Period", "Num.Last.First", "Game.ID", "Date", "Team", "Num", "Team.Num", "Full.Name", "Start.Seconds", "End.Seconds", "Duration")
    colnames(atoi.new) <- c("Period", "Num.Last.First", "Game.ID", "Date", "Team", "Num", "Team.Num", "Full.Name", "Start.Seconds", "End.Seconds", "Duration")
    
    shift.on <- rbind_list(htoi.new %>% select(Period, Start.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = Start.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                             mutate(Event = "ON", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                    Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA),
                           atoi.new %>% select(Period, Start.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = Start.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                             mutate(Event = "ON", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                    Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA)) %>% data.frame()
    
    shift.off <- rbind_list(htoi.new %>% select(Period, End.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = End.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                              mutate(Event = "OFF", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                     Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA),
                            atoi.new %>% select(Period, End.Seconds, Game.ID, Date, Team.Num, Duration, Team) %>% rename(Seconds = End.Seconds, p1 = Team.Num, ev.team = Team) %>% 
                              mutate(Event = "OFF", Description = NA, p2 = NA, p3 = NA, ev.zone = NA, Detail = NA, Away.Team = NA, Home.Team = NA,
                                     Away.Goalie = NA, Home.Goalie = NA, Strength.State = NA, Score.State = NA)) %>% data.frame()
    
    who.on.1 <- function(x) {
      n <- htoi.new$Team.Num[which(as.numeric(htoi.new$Start.Seconds) <= as.numeric(x) & as.numeric(htoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      on.home <- c(n2, p)
      return(on.home)
    }
    
    who.off.1 <- function(x) {
      n <- htoi.new$Team.Num[which(as.numeric(htoi.new$Start.Seconds) < as.numeric(x) & as.numeric(htoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      off.home <- c(n2, p)
      return(off.home)
    }
    
    who.on.2 <- function(x) {
      n <- atoi.new$Team.Num[which(as.numeric(atoi.new$Start.Seconds) <= as.numeric(x) & as.numeric(atoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      on.away <- c(n2, p)
      return(on.away)
    }
    
    who.off.2 <- function(x) {
      n <- atoi.new$Team.Num[which(as.numeric(atoi.new$Start.Seconds) < as.numeric(x) & as.numeric(atoi.new$End.Seconds) > as.numeric(x))]
      n2 <- c(n, rep(NA, times = (12 - length(n))))
      p <- roster$Position[match(n2, roster$Team.Num)]
      off.away <- c(n2, p)
      return(off.away)
    }
    
    on.home <- lapply(shift.on$Seconds, who.on.1) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(h1.num = X1, h2.num = X2, h3.num = X3, h4.num = X4, h5.num = X5, h6.num = X6, h1.pos = X13, h2.pos = X14, h3.pos = X15, h4.pos = X16, h5.pos = X17, h6.pos = X18)
    off.home <- lapply(shift.off$Seconds, who.off.1) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(h1.num = X1, h2.num = X2, h3.num = X3, h4.num = X4, h5.num = X5, h6.num = X6, h1.pos = X13, h2.pos = X14, h3.pos = X15, h4.pos = X16, h5.pos = X17, h6.pos = X18)
    on.away <- lapply(shift.on$Seconds, who.on.2) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(a1.num = X1, a2.num = X2, a3.num = X3, a4.num = X4, a5.num = X5, a6.num = X6, a1.pos = X13, a2.pos = X14, a3.pos = X15, a4.pos = X16, a5.pos = X17, a6.pos = X18)
    off.away <- lapply(shift.off$Seconds, who.off.2) %>% unlist() %>% matrix(byrow = TRUE, ncol = 24) %>% data.frame() %>% 
      rename(a1.num = X1, a2.num = X2, a3.num = X3, a4.num = X4, a5.num = X5, a6.num = X6, a1.pos = X13, a2.pos = X14, a3.pos = X15, a4.pos = X16, a5.pos = X17, a6.pos = X18)
    
    shift.on <- cbind(shift.on, on.home[,c(1:6, 13:18)], on.away[,c(1:6, 13:18)]) %>% data.frame()
    shift.off <- cbind(shift.off, off.home[,c(1:6, 13:18)], off.away[,c(1:6, 13:18)]) %>% data.frame()
    
    check <- pbp.new %>% filter(Event == "FAC") %>% mutate(Event = "CHECK", Seconds = Seconds - 0.011, Description = "Checkpoint") %>% data.frame()
    
    pbp.new <- rbind_list(pbp.new, shift.on, shift.off, check) %>% arrange(Seconds) %>% 
      mutate(event.ref = cumsum(Event %in% c("ON", "OFF") == F)) %>% group_by(event.ref) %>%
      mutate(Away.Team = first(Away.Team), Home.Team = first(Home.Team), Away.Goalie = first(Away.Goalie), Home.Goalie = first(Home.Goalie),
             Home.Skaters = first(Home.Skaters), Away.Skaters = first(Away.Skaters), Strength.State = first(Strength.State), 
             Home.Score = first(Home.Score), Away.Score = first(Away.Score), Score.State = first(Score.State), Score.Cat = first(Score.Cat),
             Seconds = Seconds - 0.001*(Event == "OFF") + 0.001*(Event == "ON")) %>% 
      filter(Event != "CHECK") %>% arrange(Seconds) %>% data.frame() %>% select(-c(event.ref)) %>% data.frame() 
    
    pbp.new$Event.Length <- c((pbp.new$Seconds[2:nrow(pbp.new)] - pbp.new$Seconds[1:(nrow(pbp.new) - 1)]), 0)
    
    ########################################################################################################################################################################################################
    ########################## SCRAPE HIGHLIGHTS JSON / ACQUÉRIR JSON DE FAITS-SAILLANTS ###################################################################################################################
    ########################################################################################################################################################################################################
    
    # Define URL / Définir URL
    year <- substr(season, start = 1, stop = 4)
    url3 <- paste("http://live.nhle.com/GameData/", season, "/", year, "0", ID, "/gc/gcgm.jsonp", sep = "")
    
    full.text.3 <- try(getURL(url3, header = FALSE,
                              .opts = curlOptions(
                                referer = 'nhl.com',
                                verbose = TRUE,
                                header = TRUE,
                                followLocation = TRUE,
                                useragent = agents[sample(1:4, 1)]))
    )
    
    if (class(full.text.3) == "try-error") {
      full.text.3 <- getURL(url3, header = FALSE,
                            .opts = curlOptions(
                              referer = 'nhl.com',
                              verbose = TRUE,
                              header = TRUE,
                              followLocation = TRUE,
                              useragent = agents[sample(1:4, 1)]))
    }
    
    text.3 <- unlist(strsplit(full.text.3, ","))
    
    hl.presecs <- gsub("sip[\":]*", "", unlist(regmatches(text.3, gregexpr("sip[\":]*[0-9]*", text.3))))
    hl.period <- gsub("[^a-z]+p[\":]+", "", unlist(regmatches(text.3, gregexpr("[^a-z]+p[\":]+[0-9]*", text.3))))
    hl.Team1 <- gsub("[^a-z]+t1[\":]+", "", unlist(regmatches(text.3, gregexpr("[^a-z]+t1[\":]+[A-Z]*", text.3))))
    hl.Team2 <- gsub("[^a-z]+t2[\":]+", "", unlist(regmatches(text.3, gregexpr("[^a-z]+t2[\":]+[A-Z]*", text.3))))
    hl.seconds <- 1200*(as.numeric(hl.period) - 1) + as.numeric(hl.presecs)
    if(as.numeric(season) >= 20152016) {
      
      urls <- paste("https://www.nhl.com/video/c-", gsub("neulionId\\\":", "", unlist(regmatches(text.3, gregexpr("neulionId\\\":[0-9]+", text.3)))), sep = "")
      
    } else {
      
      urls <- NA
      
    }
    
    hl.mat <- cbind(hl.seconds, urls) %>% data.frame()
    
    pbp.new$URL <- hl.mat$urls[match((pbp.new$Event %in% c("SHOT", "GOAL"))*round(as.numeric(as.character(pbp.new$Seconds)), 0), hl.mat$hl.seconds)]
    pbp.new$Highlight <- 1*(!is.na(pbp.new$URL))
    
    ########################################################################################################################################################################################################
    ########################## SCRAPE SPORTSNET / ACQUÉRIR DONÉES SPORTSNET ################################################################################################################################
    ########################################################################################################################################################################################################
    
    if (as.numeric(season) >= 20152016) {
      
      # Provide date / Fournir date
      day <- date
      
      # Scrape main page / Acquérir page primaire 
      url <- paste0("http://www.sportsnet.ca/hockey/nhl/scores/?datepicker-date=", day)
      glist <- try(getURL(url, header = FALSE,
                          .opts = curlOptions(
                            referer = 'sportsnet.ca',
                            verbose = TRUE,
                            header = TRUE,
                            followLocation = TRUE,
                            useragent = agents[sample(1:4, 1)]))
      )
      
      if (class(glist) == "try-error") {
        glist <- getURL(url, header = FALSE,
                        .opts = curlOptions(
                          referer = 'sportsnet.ca',
                          verbose = TRUE,
                          header = TRUE,
                          followLocation = TRUE,
                          useragent = agents[sample(1:4, 1)]))
      }
      
      gameids <- gsub("window.open[(][']", "", unique(unlist(regmatches (glist, gregexpr("window.open[(][']http://www.sportsnet.ca/hockey/nhl/livetracker/game/[0-9]+", glist)))))
      teamcity <- gsub("<span class=\"scores-team-city\">|</span>", "", unlist(regmatches (glist, gregexpr("<span class=\"scores-team-city\">([a-zA-Z]|[.]|[-]|[ ])+</span>", glist))))
      teamname <- gsub("<span class=\"scores-team-name\">|</span>", "", unlist(regmatches (glist, gregexpr("<span class=\"scores-team-name\">([a-zA-Z]|[.]|[-]|[ ])+</span>", glist))))
      teams <- paste(teamcity, teamname)
      
      # Standardize team names / Standardiser noms d'équipes
      teamlist <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ",
                    "CGY", "CHI", "COL", "DAL", "DET", "EDM",
                    "FLA", "L.A", "MIN", "MTL", "N.J", "NSH",
                    "NYI", "NYR", "OTT", "PHI", "PIT", "S.J",
                    "STL", "T.B", "TOR", "VAN", "WPG", "WSH",
                    "ARI")
      
      fullnames <- c("Anaheim Ducks", "Arizona Coyotes", "Boston Bruins", "Buffalo Sabres", "Carolina Hurricanes", "Columbus Blue Jackets",
                     "Calgary Flames", "Chicago Blackhawks", "Colorado Avalanche", "Dallas Stars", "Detroit Red Wings", "Edmonton Oilers",
                     "Florida Panthers", "Los Angeles Kings", "Minnesota Wild", "Montreal Canadiens", "New Jersey Devils", "Nashville Predators",
                     "New York Islanders", "New York Rangers", "Ottawa Senators", "Philadelphia Flyers", "Pittsburgh Penguins", "San Jose Sharks",
                     "St. Louis Blues", "Tampa Bay Lightning", "Toronto Maple Leafs", "Vancouver Canucks", "Winnipeg Jets", "Washington Capitals",
                     "Phoenix Coyotes")
      
      team.match <- cbind(teamlist, fullnames) %>% data.frame()
      
      teams <- team.match$teamlist[match(teams, team.match$fullnames)]
      
      teammat <- matrix(teams, byrow = TRUE, ncol = 2) %>% data.frame()
      
      # Create URL directory / Créer annuaire de URLs
      url.match <- cbind(gameids, teammat) %>% data.frame() %>% rename(awayteam = X1, hometeam = X2)
      
      # Match URL / Associer URL
      urlt <- first(url.match$gameids[which(url.match$awayteam == awayteam | url.match$hometeam == awayteam)])
      
      ########################################################################################################################################################################################################
      ########################################################################################################################################################################################################
      
      # Scrape game page / Acquérir page du match
      gamepage <- try(getURL(urlt, header = FALSE,
                             .opts = curlOptions(
                               referer = 'sportsnet.ca',
                               verbose = TRUE,
                               header = TRUE,
                               followLocation = TRUE,
                               useragent = agents[sample(1:4, 1)]))
      )
      
      if (class(gamepage) == "try-error") {
        gamepage <- getURL(urlt, header = FALSE,
                           .opts = curlOptions(
                             referer = 'sportsnet.ca',
                             verbose = TRUE,
                             header = TRUE,
                             followLocation = TRUE,
                             useragent = agents[sample(1:4, 1)]))
      }
      
      events <- unlist(regmatches (gamepage, gregexpr("[{]\\\"id\\\":[0-9]+,\\\"loc.*?momentum", gamepage)))
      time <- gsub("elapsed\\\":\\\"", "", unlist(regmatches (events, gregexpr("elapsed\\\":\\\"[0-9:]+", events))))
      period <- gsub("period\\\":|,", "", unlist(regmatches (events, gregexpr("period\\\":[0-9],", events))))
      type <- gsub("event\\\":\\\"", "", unlist(regmatches (events, gregexpr("event\\\":\\\"[a-zA-Z -]+", events))))
      location <- gsub("location\\\":|[[]|[]]|", "", unlist(regmatches (events, gregexpr("location\\\":[[][0-9a-zA-Z,-]+[]]", events))))
      
      # Rename event types / Renommer types de jeu
      type[type == "hit"] <- "HIT"; type[type == "score"] <- "GOAL"; type[type == "penalty"] <- "PENL"
      type[type == "shot-on-goal"] <- "SHOT"; type[type == "shot-missed"] <- "MISS"; type[type == "shot-blocked"] <- "BLOCK"
      
      # Parse time / Traiter temps
      timemat <- data.frame(matrix(as.numeric(unlist(strsplit(as.character(time), ":"))), byrow = TRUE, ncol = 2))
      seconds <- 1200*(as.numeric(period) - 1) + timemat$X1*60 + timemat$X2
      
      # Parse coordinates / Traiter coordinées
      locmat <- data.frame(matrix(as.numeric(unlist(strsplit(as.character(location), ","))), byrow = TRUE, ncol = 2))
      
      sn.table <- cbind(seconds, type, locmat) %>% data.frame() %>% rename(xc = X1, yc = X2)
      
      # Match with PBP / Associer au résumé
      pbp.new <- group_by(pbp.new, Seconds, Event) %>% 
        mutate(XC = first(sn.table$xc[which(sn.table$seconds == round(as.numeric(as.character(Seconds)), 0) & as.character(sn.table$type) == as.character(Event))]),
               YC = first(sn.table$yc[which(sn.table$seconds == round(as.numeric(as.character(Seconds)), 0) & as.character(sn.table$type) == as.character(Event))]))
      
    } else {
      
      # Provide date / Fournir date
      day <- gsub("-", "", as.character(date))
      
      # Scrape main page / Acquérir page primaire 
      url <- paste("http://scores.espn.go.com/nhl/scoreboard?date=", day, sep = "")
      glist <- try(getURL(url, header = FALSE,
                          .opts = curlOptions(
                            referer = 'sports.espn.go.com',
                            verbose = TRUE,
                            header = TRUE,
                            followLocation = TRUE,
                            useragent = agents[sample(1:4, 1)]))
      )
      
      if (class(glist) == "try-error") {
        glist <- getURL(url, header = FALSE,
                        .opts = curlOptions(
                          referer = 'sports.espn.go.com',
                          verbose = TRUE,
                          header = TRUE,
                          followLocation = TRUE,
                          useragent = agents[sample(1:4, 1)]))
      }
      
      gameids <- unique(unlist(regmatches(glist, gregexpr("gameId=[0-9]+", glist))))
      teams <- toupper(gsub("team/_/name/|>|</div>", "", unique(unlist(regmatches(glist, gregexpr("team/_/name/[a-zA-Z]+|>(Coyotes|Thrashers)</div>", glist)))))) # FIX FOR PRE-20112012 / SOLUTION POUR AVANT 20112012
      
      # Format team names / Changer noms d'équipes 
      teams[which(teams == "PHX")] <- "ARI"
      teams[which(teams == "TB")] <- "T.B"
      teams[which(teams == "NJ")] <- "N.J"
      teams[which(teams == "SJ")] <- "S.J"
      teams[which(teams == "LA")] <- "L.A"
      teams[which(teams == "COYOTES")] <- "ARI" # FIX FOR PRE-20112012 / SOLUTION POUR AVANT 20112012
      teams[which(teams == "THRASHERS")] <- "ATL" # FIX FOR PRE-20112012 / SOLUTION POUR AVANT 20112012
      
      if (as.numeric(season) < 20110000) {
        teams[which(teams == "WPG")] <- "ATL"
      } # FIX FOR PRE-20112012 / SOLUTION POUR AVANT 20112012
      
      teammat <- matrix(unique(teams), byrow = TRUE, ncol = 2) %>% data.frame()
      
      # Create URL directory / Créer annuaire de URLs
      url.match <- cbind(gameids, teammat) %>% data.frame() %>% rename(awayteam = X1, hometeam = X2)
      
      # Match URL / Associer URL
      urlt <- first(as.character(url.match$gameids[which(as.character(url.match$awayteam) == as.character(awayteam) | as.character(url.match$hometeam) == as.character(awayteam))]))
      
      ####################################################################################################################################################################################
      ####################################################################################################################################################################################
      
      # Scrape game page / Acquérir page du match
      url2 <- paste("http://sports.espn.go.com/nhl/gamecast/data/masterFeed?lang=en&isAll=true&rand=0&", urlt, sep = "")
      gamepage <- try(getURL(url2, header = FALSE,
                             .opts = curlOptions(
                               referer = 'sports.espn.go.com',
                               verbose = TRUE,
                               header = TRUE,
                               followLocation = TRUE,
                               useragent = agents[sample(1:4, 1)]))
      )
      
      if (class(gamepage) == "try-error") {
        gamepage <- getURL(url2, header = FALSE,
                           .opts = curlOptions(
                             referer = 'sports.espn.go.com',
                             verbose = TRUE,
                             header = TRUE,
                             followLocation = TRUE,
                             useragent = agents[sample(1:4, 1)]))
      }
      
      enames = c("FAC", "HIT", "GvTk", "GOAL", "SHOT", "MISS", "BLOCK", "PENL",
                 "STOP", "PRDY", "PSTR", "PEND", "PERD", "SOC", "GEnd", "SOut",
                 "error", "TAKE", "GIVE", "early intermission", "nothing", "nothing")
      ecodes = as.character(c(502, 503, 504, 505, 506, 507, 508, 509,
                              516, 517, 518, 519, 520, 521, 522, 0, 
                              9999, 1401, 1402, -2147483648, 1, 5))
      
      etext <- unlist(regmatches(gamepage, gregexpr("<Play.*?/Play>", gamepage)))
      
      if (length(etext) > 1) {
        esplit <- t(do.call(cbind, strsplit(etext, "[\\[~]")))
        esplit <- esplit[,c(5,3,4,6,7,11)]
        colnames(esplit) <- c("etype","xc","yc","time","period","event.description")
        esplit <- esplit[,1:5] %>% as.data.frame(stringsAsFactors = FALSE)
        
        esplit$etype <- enames[match(esplit$etype, ecodes)]
        
        timesplits <- do.call(rbind, strsplit(esplit$time, ":"))
        seconds <- 1200*(as.numeric(esplit$period) - 1) + as.numeric(timesplits[,1])*60 + as.numeric(timesplits[,2])
        esplit$seconds <- seconds
        
        # Match with PBP / Associer au résumé
        pbp.new <- group_by(pbp.new, Seconds, Event) %>% 
          mutate(XC = first(esplit$xc[which(esplit$seconds == round(as.numeric(as.character(Seconds)), 0) & as.character(esplit$etype) == as.character(Event))]),
                 YC = first(esplit$yc[which(esplit$seconds == round(as.numeric(as.character(Seconds)), 0) & as.character(esplit$etype) == as.character(Event))]))
      } else {
        
        pbp.new$XC <- NA; pbp.new$YC <- NA
        
      }
    }
    
    ########################################################################################################################################################################################################
    ########################################################################################################################################################################################################
    ########################################################################################################################################################################################################
    
    # Fix duplicate names / Réparer noms doubles
    roster$Full.Name[which(roster$Full.Name == "ERIK.KARLSSON" & roster$Team == "CAR")] <- "ERIK.KARLSSON.2"
    roster$Full.Name[which(roster$Full.Name == "ERIK.GUSTAFSSON" & roster$Team == "PHI")] <- "ERIK.GUSTAFSSON.2"
    roster$Full.Name[which(roster$Full.Name == "PK.SUBBAN" | roster$Full.Name == "P.K.SUBBAN")] <- "P.K..SUBBAN"
    roster$Full.Name[which(roster$Full.Name == "TJ.OSHIE" | roster$Full.Name == "T.J.OSHIE")] <- "T.J..OSHIE"
    roster$Full.Name[which(roster$Full.Name == "BJ.CROMBEEN" | roster$Full.Name == "B.J.CROMBEEN" | roster$Full.Name == "BRANDON.CROMBEEN")] <- "B.J..CROMBEEN"
    roster$Full.Name[which(roster$Full.Name == "ILJA.BRYZGALOV")] <- "ILYA.BRYZGALOV"
    roster$Full.Name[which(roster$Full.Name == "CAMERON.BARKER")] <- "CAM.BARKER"
    roster$Full.Name[which(roster$Full.Name == "CHRIS.VANDE VELDE")] <- "CHRIS.VANDEVELDE"
    roster$Full.Name[which(roster$Full.Name == "DANIEL.CARCILLO")] <- "DAN.CARCILLO"
    roster$Full.Name[which(roster$Full.Name == "DANIEL.CLEARY")] <- "DAN.CLEARY"
    roster$Full.Name[which(roster$Full.Name == "DAVID JOHNNY.ODUYA")] <- "JOHNNY.ODUYA"
    roster$Full.Name[which(roster$Full.Name == "DAVID.BOLLAND")] <- "DAVE.BOLLAND"
    roster$Full.Name[which(roster$Full.Name == "DWAYNE.KING")] <- "DJ.KING"
    roster$Full.Name[which(roster$Full.Name == "EVGENII.DADONOV")] <- "EVGENY.DADONOV"
    roster$Full.Name[which(roster$Full.Name == "FREDDY.MODIN")] <- "FREDRIK.MODIN"
    roster$Full.Name[which(roster$Full.Name == "HARRISON.ZOLNIERCZYK")] <- "HARRY.ZOLNIERCZYK"
    roster$Full.Name[which(roster$Full.Name == "J P.DUMONT" | roster$Full.Name == "JEAN-PIERRE.DUMONT")] <- "J-P.DUMONT"
    roster$Full.Name[which(roster$Full.Name == "JEAN-FRANCOIS.JACQUES")] <- "J-F.JACQUES"
    roster$Full.Name[which(roster$Full.Name == "JONATHAN.AUDY-MARCHESSAULT")] <- "JONATHAN.MARCHESSAULT"
    roster$Full.Name[which(roster$Full.Name == "JOSHUA.HENNESSY")] <- "JOSH.HENNESSY"
    roster$Full.Name[which(roster$Full.Name == "KRISTOPHER.LETANG")] <- "KRIS.LETANG"
    roster$Full.Name[which(roster$Full.Name == "KRYSTOFER.BARCH")] <- "KRYS.BARCH"
    roster$Full.Name[which(roster$Full.Name == "MARTIN.ST LOUIS")] <- "MARTIN.ST. LOUIS"
    roster$Full.Name[which(roster$Full.Name == "MATTHEW.CARLE")] <- "MATT.CARLE"
    roster$Full.Name[which(roster$Full.Name == "MATTHEW.DUMBA")] <- "MATT.DUMBA"
    roster$Full.Name[which(roster$Full.Name == "JOSEPH.CORVO")] <- "JOE.CORVO"
    roster$Full.Name[which(roster$Full.Name == "TOBY.ENSTROM")] <- "TOBIAS.ENSTROM"
    roster$Full.Name[which(roster$Full.Name == "MICHAEL.SANTORELLI")] <- "MIKE.SANTORELLI"
    roster$Full.Name[which(roster$Full.Name == "MICHAEL.CAMMALLERI")] <- "MIKE.CAMMALLERI"
    roster$Full.Name[which(roster$Full.Name == "PIERRE.PARENTEAU" | roster$Full.Name == "PIERRE-ALEX.PARENTEAU")] <- "PA.PARENTEAU"
    roster$Full.Name <- gsub("ALEXANDER.|ALEXANDRE.", "ALEX.", roster$Full.Name)
    roster$Full.Name <- gsub("CHRISTOPHER.", "CHRIS.", roster$Full.Name)
    roster$Full.Name[which(roster$Full.Name == "NICOLAS.PETAN")] <- "NIC.PETAN"
    roster$Full.Name[which(roster$Full.Name == "NIKOLAI.KULEMIN")] <- "NIKOLAY.KULEMIN"
    
    # Add roster match code / Ajouter code d'association
    roster$Player.Code <- gsub("[^A-Za-z]", "", roster$Full.Name)
    
    # Replace teamnum / Remplacer code équipenum
    if(names == TRUE) {
      pbp.new$p1 <- roster$Full.Name[match(pbp.new$p1, roster$Team.Num)]; pbp.new$p2 <- roster$Full.Name[match(pbp.new$p2, roster$Team.Num)]; pbp.new$p3 <- roster$Full.Name[match(pbp.new$p3, roster$Team.Num)]
      pbp.new$a1.num <- roster$Full.Name[match(pbp.new$a1.num, roster$Team.Num)]; pbp.new$a2.num <- roster$Full.Name[match(pbp.new$a2.num, roster$Team.Num)]; pbp.new$a3.num <- roster$Full.Name[match(pbp.new$a3.num, roster$Team.Num)]
      pbp.new$a4.num <- roster$Full.Name[match(pbp.new$a4.num, roster$Team.Num)]; pbp.new$a5.num <- roster$Full.Name[match(pbp.new$a5.num, roster$Team.Num)]; pbp.new$a6.num <- roster$Full.Name[match(pbp.new$a6.num, roster$Team.Num)]
      pbp.new$h1.num <- roster$Full.Name[match(pbp.new$h1.num, roster$Team.Num)]; pbp.new$h2.num <- roster$Full.Name[match(pbp.new$h2.num, roster$Team.Num)]; pbp.new$h3.num <- roster$Full.Name[match(pbp.new$h3.num, roster$Team.Num)]
      pbp.new$h4.num <- roster$Full.Name[match(pbp.new$h4.num, roster$Team.Num)]; pbp.new$h5.num <- roster$Full.Name[match(pbp.new$h5.num, roster$Team.Num)]; pbp.new$h6.num <- roster$Full.Name[match(pbp.new$h6.num, roster$Team.Num)]
      pbp.new$Away.Goalie <- roster$Full.Name[match(pbp.new$Away.Goalie, roster$Team.Num)]; pbp.new$Home.Goalie <- roster$Full.Name[match(pbp.new$Home.Goalie, roster$Team.Num)]
    }
    
    # Populate lists / Peupler listes
    pbp.list[[i]] <- c(t(pbp.new))
    cnames1 <- colnames(pbp.new)
    
    roster.list[[i]] <- c(t(roster))
    cnames2 <- colnames(roster)
    
    cat(paste("Pausing", pause, "seconds...\n"))
    Sys.sleep(pause)
    
  }
  
  # Unlist into tables / Remplir tables
  pbp.full <- matrix(unlist(pbp.list), byrow = T, ncol = 53) %>% as.data.frame(stringsAsFactors = FALSE)
  colnames(pbp.full) <- cnames1
  pbp.full$Season <- season
  pbp.full$Season.Type <- "Regular"
  pbp.full$Season.Type[which(as.numeric(as.character(pbp.full$Game.ID)) >= 30000)] <- "Playoffs"
  
  roster.full <- matrix(unlist(roster.list), byrow = T, ncol = 11) %>% data.frame()
  colnames(roster.full) <- cnames2
  roster.full$Season <- season
  roster.full$Season.Type <- "Regular"
  roster.full$Season.Type[which(as.numeric(as.character(roster.full$Game.ID)) >= 30000)] <- "Playoffs"
  
  pbp.full <<- pbp.full
  roster.full <<- roster.full
  
  # Print record of missing games / Imprimer un record de matchs manquants
  missing <- games[which(games %in% unique(pbp.full$Game.ID) == FALSE)]
  
  if (length(missing) > 0) {
    cat(paste("Oops! Games missing:", paste(missing, collapse = ", "), "\n"))
  } else {
    cat("Success! All games were scraped.\n")
  }
  
  return(TRUE)
  
}

schedule <- function(start, end) { # 2016-10-12
  
  registerDoMC(cores = 2)
  
  url <- paste("https://statsapi.web.nhl.com/api/v1/schedule?startDate=",
               start,
               "&endDate=",
               end,
               "&expand=schedule.teams,schedule.linescore,schedule.broadcasts.all,schedule.ticket,schedule.game.content.media.epg,schedule.game.seriesSummary,seriesSummary.series&leaderCategories=&leaderGameTypes=R&site=en_nhl&teamId=",
               sep = ""
  )
  
  json <- getURL(url,
                 .opts = curlOptions(
                   referer = 'nhl.com',
                   header = FALSE,
                   verbose = TRUE,
                   followLocation = FALSE,
                   useragent = 'Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.130 Safari/537.36')
  )
  
  parsed <- try(fromJSON(json))
  
  if(class(parsed) == "try-error") {
    
    json <- getURL(url,
                   .opts = curlOptions(
                     referer = 'nhl.com',
                     header = FALSE,
                     verbose = TRUE,
                     followLocation = FALSE,
                     useragent = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_2) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.111 Safari/537.36')
    )
    
    parsed <- try(fromJSON(json))
    
    if(class(parsed) == "try-error") {
      
      json <- getURL(url,
                     .opts = curlOptions(
                       referer = 'nhl.com',
                       header = FALSE,
                       verbose = TRUE,
                       followLocation = FALSE,
                       useragent = 'Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/46.0.2490.86 Safari/537.36')
      )
      
      parsed <- fromJSON(json)
      
    }
    
  }
  
  meat <- parsed[[4]]
  
  full.vector <- foreach(i = 1:length(meat), .combine = "c") %dopar% {
    
    date <- meat[[i]]
    
    vector <- foreach(j = 1:length(date$games), .combine = c) %do% {
      
      ID <- date$games[[j]]$gamePk
      Season <- date$games[[j]]$season
      Session <- date$games[[j]]$gameType
      Status <- date$games[[j]]$status$detailedState
      Periods <- date$games[[j]]$linescore$currentPeriod
      Date.Time <- date$games[[j]]$gameDate
      Home.Team <- date$games[[j]]$teams$home$team$abbreviation
      Home.Division <- date$games[[j]]$teams$home$team$division$name
      Home.Conference <- date$games[[j]]$teams$home$team$conference$name
      Home.Score <- date$games[[j]]$teams$home$score
      Away.Team <- date$games[[j]]$teams$away$team$abbreviation
      Away.Division <- date$games[[j]]$teams$away$team$division$name
      Away.Conference <- date$games[[j]]$teams$away$team$conference$name
      Away.Score <- date$games[[j]]$teams$away$score
      Timezone <- date$games[[j]]$teams$home$team$venue$timeZone$id
      Home.Shots <- date$games[[j]]$linescore$teams$home$shotsOnGoal
      Away.Shots <- date$games[[j]]$linescore$teams$away$shotsOnGoal
      Home.Goals <- date$games[[j]]$linescore$teams$home$goals
      Away.Goals <- date$games[[j]]$linescore$teams$away$goals
      
      row <- c(ID, Season, Session, Status, Periods, Date.Time, Home.Team, Home.Division, Home.Conference, Home.Score, Away.Team, Away.Division, Away.Conference, Away.Score, Timezone, Home.Shots, Away.Shots, Home.Goals, Away.Goals)
      
      if(length(row) == 19) {row}
      
    }
    
    unlist(vector)
    
  }
  
  mat <- matrix(full.vector, ncol = 19, byrow = TRUE) %>% data.frame()
  colnames(mat) <- c("ID", "Season", "Session", "Status", "Periods", "Date.Time", "Home.Team", "Home.Division", "Home.Conference", "Home.Score", "Away.Team",
                     "Away.Division", "Away.Conference", "Away.Score", "Timezone", "Home.Shots", "Away.Shots", "Home.Goals", "Away.Goals")
  
  mat
  
}

compile <- function(key, to.db = TRUE) {
  
  if(key != TRUE) {
    cat("Use scrape() or quick_scrape() before compile()!\n")
  } else {
    cat("Compiling...\n")
  }
  
  # Enhanced PBP / Résumé de match amélioré
  pbp.full$Home.Zone <- pbp.full$ev.zone
  pbp.full$Home.Zone[which(as.character(pbp.full$ev.team) == as.character(pbp.full$Away.Team) & as.character(pbp.full$ev.zone) == "Off")] <- "Def"
  pbp.full$Home.Zone[which(as.character(pbp.full$ev.team) == as.character(pbp.full$Away.Team) & as.character(pbp.full$ev.zone) == "Def")] <- "Off"
  
  pbp.full <- mutate(pbp.full, Newcode = paste(Season, Game.ID, sep = "."), Round.Seconds = round(as.numeric(as.character(Seconds)), 0)) %>% group_by(Game.ID, Round.Seconds) %>% 
    mutate(FOS = sum(Event %in% c("FAC")), ZF = first(as.character(Home.Zone[which(as.character(Event) %in% c("FAC"))]))) %>% data.frame() %>% 
    mutate(Distance = sqrt((89 - abs(as.numeric(as.character(XC))))^2 + as.numeric(as.character(YC))^2), 
           Angle = abs(atan(as.numeric(as.character(YC))/(89 - abs(as.numeric(as.character(XC)))))*(180/pi)),
           is.NZ = 1*(abs(as.numeric(as.character(XC))) <= 22), 
           is.PP = 1*({as.character(ev.team) == as.character(Home.Team) & as.character(Strength.State) %in% c("5v4", "5v3", "4v3")} | {as.character(ev.team) == as.character(Away.Team) & as.character(Strength.State) %in% c("4v5", "3v5", "3v4")}),
           ref = cumsum(Event %in% c("FAC"))) %>% group_by(ref) %>%
    mutate(Since = as.numeric(as.character(Round.Seconds)) - min(as.numeric(as.character(Round.Seconds))), Zone.Start = first(Home.Zone), Since.Cat = 1*(Since > 25), Score.Cat = as.numeric(as.character(Score.Cat)),
           Category1 = (as.numeric(as.character(Score.Cat)) + 160)*(1*(as.character(ev.team) == as.character(Home.Team)) + 13*(as.character(ev.team) == as.character(Away.Team)))*(as.numeric(as.character(Since.Cat))*3 + 1)*(1*(Zone.Start == "Off") + 2*(Zone.Start == "Neu") + 3*(Zone.Start == "Def")),
           Category2 = as.numeric((1*(as.character(ev.team) == as.character(Away.Team)) + 20*(as.character(ev.team) == as.character(Home.Team)))*(Score.Cat + 4))) %>%
    data.frame()
  
  # Fix empty-net states / Réparer états de filet désert 
  pbp.full$Strength.State[which(is.na(pbp.full$Home.Goalie) == TRUE | is.na(pbp.full$Away.Goalie) == TRUE)] <- "EvE"
  pbp.full$Strength.State[which(as.numeric(as.character(pbp.full$Period)) > 4 & as.numeric(as.character(pbp.full$Game.ID)) < 30000)] <- "0v0"
  
  # Prevent NA event team / Prévenir équipe du jeu NA
  pbp.full$ev.team[which(is.na(as.character(pbp.full$ev.team)) == TRUE)] <- "UKN"
  
  # DUMMY CATEGORY FOR NON-5V5 OR NA MATCHING TO 1 / CATÉGORIE FAUSSE POUR NA OU ÉTATS NON-5V5 POUR ASSOCIER A 1
  pbp.full$Category1[which(as.character(pbp.full$Strength.State) != "5v5" | pbp.full$Category1 == 0)] <- NA
  pbp.full$Category2[which(as.character(pbp.full$Strength.State) != "5v5" | pbp.full$Category2 == 0)] <- NA
  
  pbp.full <- mutate(pbp.full, cweight1 = coeffs1$corsi[match(Category1, coeffs1$category)], fweight1 = coeffs1$fenwick[match(Category1, coeffs1$category)],
                     sweight1 = coeffs1$shot[match(Category1, coeffs1$category)], gweight1 = coeffs1$goal[match(Category1, coeffs1$category)],
                     cweight2 = coeffs2$corsi[match(Category2, coeffs2$category)], fweight2 = coeffs2$fenwick[match(Category2, coeffs2$category)],
                     sweight2 = coeffs2$shot[match(Category2, coeffs2$category)], gweight2 = coeffs2$goal[match(Category2, coeffs2$category)]) %>% data.frame()
  
  pbp.full$a1.num[which(as.numeric(as.character(pbp.full$Period)) >= 5 & as.character(pbp.full$ev.team) == as.character(pbp.full$Away.Team))] <- pbp.full$p1[which(as.numeric(as.character(pbp.full$Period)) >= 5 & as.character(pbp.full$ev.team) == as.character(pbp.full$Away.Team))]
  pbp.full$h1.num[which(as.numeric(as.character(pbp.full$Period)) >= 5 & as.character(pbp.full$ev.team) == as.character(pbp.full$Home.Team))] <- pbp.full$p1[which(as.numeric(as.character(pbp.full$Period)) >= 5 & as.character(pbp.full$ev.team) == as.character(pbp.full$Home.Team))]
  
  pbp.full$is.Rebound <- c(0, 1*(pbp.full$Event[-1] %in% c("GOAL", "SHOT", "MISS", "BLOCK") & pbp.full$Event[-nrow(pbp.full)] %in% c("GOAL", "SHOT", "MISS", "BLOCK") & pbp.full$Period[-nrow(pbp.full)] == pbp.full$Period[-1] &
                                   (as.numeric(as.character(pbp.full$Seconds[-1])) - as.numeric(as.character(pbp.full$Seconds[-nrow(pbp.full)]))) <= 2))
  pbp.full$is.Rush <- c(0, 1*(pbp.full$Event[-1] %in% c("GOAL", "SHOT", "MISS", "BLOCK") & pbp.full$Period[-nrow(pbp.full)] == pbp.full$Period[-1] &
                                (as.numeric(as.character(pbp.full$Seconds[-1])) - as.numeric(as.character(pbp.full$Seconds[-nrow(pbp.full)]))) <= 4 & 
                                {pbp.full$Event[-nrow(pbp.full)] %in% c("GIVE", "TAKE") | {{pbp.full$ev.team[-nrow(pbp.full)] == pbp.full$ev.team[-1] & pbp.full$ev.zone[-nrow(pbp.full)] == "Def"} |
                                {pbp.full$ev.team[-nrow(pbp.full)] != pbp.full$ev.team[-1] & pbp.full$ev.zone[-nrow(pbp.full)] == "Off"}}}))
  
  pbp.full$is.Rush[which(is.na(pbp.full$is.Rush) == T)] <- 0
  pbp.full$is.Rebound[which(is.na(pbp.full$is.Rebound) == T)] <- 0
  
  pbp.full <- rbind_list(filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & is.NZ == 1) %>% mutate(xG = 0.00648),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & is.na(is.NZ) == T) %>% mutate(xG = 0.06639),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Wrist" & is.NZ == 0 & is.Rebound == 0) %>% xG1(),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Backhand" & is.NZ == 0 & is.Rebound == 0) %>% xG2(),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Slap" & is.NZ == 0 & is.Rebound == 0) %>% xG3(),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) %in% c("Tip-In", "Deflected") & is.NZ == 0 & is.Rebound == 0) %>% xG4(),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Snap" & is.NZ == 0 & is.Rebound == 0) %>% xG5(),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Wrap-around" & is.NZ == 0 & is.Rebound == 0) %>% xG6(),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Wrist" & is.NZ == 0 & is.Rebound == 1) %>% xG7(),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Backhand" & is.NZ == 0 & is.Rebound == 1) %>% xG8(),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Slap" & is.NZ == 0 & is.Rebound == 1) %>% xG9(),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) %in% c("Tip-In", "Deflected") & is.NZ == 0 & is.Rebound == 1) %>% xG10(),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Snap" & is.NZ == 0 & is.Rebound == 1) %>% xG11(),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) == "Wrap-around" & is.NZ == 0 & is.Rebound == 1) %>% xG12(),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") & as.character(Detail) %in% c("Wrist", "Backhand", "Slap", "Tip-In", "Deflected", "Snap", "Wrap-around") == F),
                         filter(pbp.full, as.character(Event) %in% c("GOAL", "SHOT", "MISS") == F)) %>% data.frame() %>% 
    group_by(Game.ID) %>% arrange(Seconds) %>% data.frame()
  
  # Replace xG for shots missing coordinates / Remplacer xG pour tirs sans coordinées
  pbp.full$xG[which(is.na(pbp.full$xG) == TRUE & as.character(pbp.full$Event) %in% c("GOAL", "SHOT", "MISS"))] <- 0.06639
  
  # Append unique game codes to roster table / Attacher codes de match uniques a la table de formation
  roster.full$Newcode <- paste(roster.full$Season, roster.full$Game.ID, sep = ".")
  
  # Create team table / Créer table d'équipes
  team.bygame <- rbind_list(group_by(pbp.full, Season, Date, Game.ID, Home.Team, Score.Cat, Strength.State, Season.Type) %>% rename(Team = Home.Team) %>%
                              summarise(Venue = "Home", TOI = round(sum(na.omit(as.numeric(as.character(Event.Length))))/60, 2),
                                        CF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
                                        FF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
                                        SF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")),
                                        GF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")),
                                        xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))),
                                        ACF = sum(cweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                        AFF = sum(fweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                        ASF = sum(sweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                        AGF = sum(gweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
                                        AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == Team))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))),
                                        MCF = sum(cweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                        MFF = sum(fweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                        MSF = sum(sweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                        MGF = sum(gweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
                                        MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == Team))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))),
                                        SCF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09), SCA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09),
                                        ASCF = sum(fweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), ASCA = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
                                        MSCF = sum(fweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), MSCA = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
                                        OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def"}),
                                        DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off"}),
                                        NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
                                        OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
                                        DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
                                        NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
                                        FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team),
                                        HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team),
                                        GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Team),
                                        PENT = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Team & grepl("10 min", as.character(Detail)) == F),
                                        PEND = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
                                        DISTF = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(ev.team) == Team))),
                                        DISTA = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(ev.team) == Away.Team))),
                                        RBF = sum(as.character(ev.team) == Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Away.Team & is.Rebound == 1),
                                        RSF = sum(as.character(ev.team) == Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Away.Team & is.Rush == 1),
                                        POSTF = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Team & grepl("Goalpost", as.character(Detail) == TRUE)),
                                        POSTA = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Away.Team & grepl("Goalpost", as.character(Detail) == TRUE))),
                            group_by(pbp.full, Season, Date, Game.ID, Away.Team, Score.Cat, Strength.State, Season.Type) %>% rename(Team = Away.Team) %>%
                              summarise(Venue = "Away", TOI = round(sum(na.omit(as.numeric(as.character(Event.Length))))/60, 2),
                                        CF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
                                        FF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
                                        SF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")),
                                        GF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")),
                                        xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))),
                                        ACF = sum(cweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                        AFF = sum(fweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                        ASF = sum(sweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                        AGF = sum(gweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
                                        AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == Team))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))),
                                        MCF = sum(cweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                        MFF = sum(fweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                        MSF = sum(sweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                        MGF = sum(gweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
                                        MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == Team))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))),
                                        SCF = sum(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09), SCA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09),
                                        ASCF = sum(fweight1*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), ASCA = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
                                        MSCF = sum(fweight2*(as.character(ev.team) == Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)), MSCA = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS") & xG >= 0.09)),
                                        OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def"}),
                                        DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off"}),
                                        NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
                                        OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
                                        DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
                                        NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
                                        FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team),
                                        HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team),
                                        GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Team),
                                        PENT = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Team & grepl("10 min", as.character(Detail)) == F),
                                        PEND = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
                                        DISTF = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(ev.team) == Team))),
                                        DISTA = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(ev.team) == Home.Team))),
                                        RBF = sum(as.character(ev.team) == Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Home.Team & is.Rebound == 1),
                                        RSF = sum(as.character(ev.team) == Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Home.Team & is.Rush == 1),
                                        POSTF = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Team & grepl("Goalpost", as.character(Detail) == TRUE)),
                                        POSTA = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Home.Team & grepl("Goalpost", as.character(Detail) == TRUE)))) %>%
    mutate(Newcode = paste(Season, Game.ID, sep = ".")) %>% data.frame()
  # ADD OFFSIDES/ICINGS?
  
  # Create goalie table / Créer table de gardiens
  goalie.bygame <- rbind_list(group_by(pbp.full, Season, Date, Game.ID, Home.Goalie, Score.Cat, Strength.State, Season.Type) %>% rename(Player = Home.Goalie) %>%
                                summarise(Venue = "Home", Team = first(Home.Team), TOI = round(sum(na.omit(as.numeric(as.character(Event.Length))))/60, 2),
                                          CF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
                                          FF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
                                          SF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")),
                                          GF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")),
                                          xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))),
                                          ACF = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                          AFF = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                          ASF = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                          AGF = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
                                          AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))),
                                          MCF = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                          MFF = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                          MSF = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                          MGF = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))),
                                          MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))),
                                          HDSF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL") & xG >= 0.09), HDSA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL") & xG >= 0.09),
                                          MDSF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL") & xG >= 0.03 & xG < 0.09), MDSA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL") & xG >= 0.03 & xG < 0.09),
                                          LDSF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL") & xG < 0.03), LDSA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL") & xG < 0.03),
                                          HDGF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL") & xG >= 0.09), HDGA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL") & xG >= 0.09),
                                          MDGF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL") & xG >= 0.03 & xG < 0.09), MDGA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL") & xG >= 0.03 & xG < 0.09),
                                          LDGF = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL") & xG < 0.03), LDGA = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL") & xG < 0.03),
                                          OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def"}),
                                          DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off"}),
                                          NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
                                          OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
                                          DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
                                          NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
                                          FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team),
                                          HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team),
                                          GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Home.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Home.Team),
                                          PENT = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
                                          PEND = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
                                          G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(Player)),
                                          A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(Player)),
                                          A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(Player)),
                                          iPENT = sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
                                          iPEND = sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
                                          DISTA = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(ev.team) == Away.Team))),
                                          RBF = sum(as.character(ev.team) == Home.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Away.Team & is.Rebound == 1),
                                          RSF = sum(as.character(ev.team) == Home.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Away.Team & is.Rush == 1),
                                          POSTF = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Home.Team & grepl("Goalpost", as.character(Detail) == TRUE)),
                                          POSTA = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Away.Team & grepl("Goalpost", as.character(Detail) == TRUE))),
                              group_by(pbp.full, Season, Date, Game.ID, Away.Goalie, Score.Cat, Strength.State, Season.Type) %>% rename(Player = Away.Goalie) %>%
                                summarise(Venue = "Away", Team = first(Away.Team), TOI = round(sum(na.omit(as.numeric(as.character(Event.Length))))/60, 2),
                                          CF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")), CA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK")),
                                          FF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")), FA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS")),
                                          SF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL")), SA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL")),
                                          GF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL")), GA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL")),
                                          xGF = sum(na.omit(xG*(as.character(ev.team) == as.character(Away.Team)))), xGA = sum(na.omit(xG*(as.character(ev.team) == as.character(Home.Team)))),
                                          ACF = sum(cweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), ACA = sum(cweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                          AFF = sum(fweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), AFA = sum(fweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                          ASF = sum(sweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), ASA = sum(sweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                          AGF = sum(gweight1*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), AGA = sum(gweight1*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
                                          AxGF = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Away.Team)))), AxGA = sum(na.omit(fweight1*xG*(as.character(ev.team) == as.character(Home.Team)))),
                                          MCF = sum(cweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))), MCA = sum(cweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS", "BLOCK"))),
                                          MFF = sum(fweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))), MFA = sum(fweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL", "MISS"))),
                                          MSF = sum(sweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL"))), MSA = sum(sweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL"))),
                                          MGF = sum(gweight2*(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL"))), MGA = sum(gweight2*(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL"))),
                                          MxGF = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Away.Team)))), MxGA = sum(na.omit(fweight2*xG*(as.character(ev.team) == as.character(Home.Team)))),
                                          HDSF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL") & xG >= 0.09), HDSA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL") & xG >= 0.09),
                                          MDSF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL") & xG >= 0.03 & xG < 0.09), MDSA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL") & xG >= 0.03 & xG < 0.09),
                                          LDSF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("SHOT", "GOAL") & xG < 0.03), LDSA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("SHOT", "GOAL") & xG < 0.03),
                                          HDGF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL") & xG >= 0.09), HDGA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL") & xG >= 0.09),
                                          MDGF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL") & xG >= 0.03 & xG < 0.09), MDGA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL") & xG >= 0.03 & xG < 0.09),
                                          LDGF = sum(as.character(ev.team) == Away.Team & as.character(Event) %in% c("GOAL") & xG < 0.03), LDGA = sum(as.character(ev.team) == Home.Team & as.character(Event) %in% c("GOAL") & xG < 0.03),
                                          OZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Off" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Def"}),
                                          DZS = sum(as.character(Event) %in% c("FAC") & {as.character(ev.team) == Away.Team & as.character(ev.zone) == "Def" | as.character(ev.team) == Home.Team & as.character(ev.zone) == "Off"}),
                                          NZS = sum(as.character(Event) %in% c("FAC") & as.character(ev.zone) == "Neu"),
                                          OZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Def")),
                                          DZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Off")),
                                          NZF = sum(na.omit(as.character(Event) %in% c("STOP", "PENL", "GOAL") & as.character(ZF) == "Neu")),
                                          FOW = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Away.Team), FOL = sum(as.character(Event) %in% c("FAC") & as.character(ev.team) == Home.Team),
                                          HF = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Away.Team), HA = sum(as.character(Event) %in% c("HIT") & as.character(ev.team) == Home.Team),
                                          GVA = sum(as.character(Event) %in% c("GIVE") & as.character(ev.team) == Away.Team), TKA = sum(as.character(Event) %in% c("TAKE") & as.character(ev.team) == Away.Team),
                                          PENT = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Away.Team & grepl("10 min", as.character(Detail)) == F),
                                          PEND = sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(ev.team) == Home.Team & grepl("10 min", as.character(Detail)) == F),
                                          G = sum(as.character(Event) %in% c("GOAL") & as.character(p1) == as.character(Player)),
                                          A1 = sum(as.character(Event) %in% c("GOAL") & as.character(p2) == as.character(Player)),
                                          A2 = sum(as.character(Event) %in% c("GOAL") & as.character(p3) == as.character(Player)),
                                          iPENT = sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p1) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
                                          iPEND = sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("4 min", as.character(Detail)) == T) + sum(as.character(Event) %in% c("PENL") & as.character(p2) == as.character(Player) & grepl("10 min", as.character(Detail)) == F),
                                          DISTA = sum(na.omit(Distance*(as.character(Event) %in% c("SHOT", "GOAL", "MISS") & as.character(ev.team) == Away.Team))),
                                          RBF = sum(as.character(ev.team) == Away.Team & is.Rebound == 1), RBA = sum(as.character(ev.team) == Home.Team & is.Rebound == 1),
                                          RSF = sum(as.character(ev.team) == Away.Team & is.Rush == 1), RSA = sum(as.character(ev.team) == Home.Team & is.Rush == 1),
                                          POSTF = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Away.Team & grepl("Goalpost", as.character(Detail) == TRUE)),
                                          POSTA = sum(as.character(Event) %in% c("MISS") & as.character(ev.team) == Home.Team & grepl("Goalpost", as.character(Detail) == TRUE)))) %>%
    mutate(Newcode = paste(Season, Game.ID, sep = ".")) %>% data.frame() %>% filter(!is.na(Player)) %>% data.frame()
  
  player.points <- rbind_list(group_by(pbp.full, Season, Date, Game.ID, p1, Score.Cat, Strength.State, Season.Type) %>% rename(Player = p1) %>%
                                summarise(G = sum(Event %in% c("GOAL")), A1 = 0, A2 = 0),
                              group_by(pbp.full, Season, Date, Game.ID, p2, Score.Cat, Strength.State, Season.Type) %>% rename(Player = p2) %>%
                                summarise(G = 0, A1 = sum(Event %in% c("GOAL")), A2 = 0),
                              group_by(pbp.full, Season, Date, Game.ID, p3, Score.Cat, Strength.State, Season.Type) %>% rename(Player = p3) %>%
                                summarise(G = 0, A1 = 0, A2 = sum(Event %in% c("GOAL")))
  ) %>%
    data.frame() %>% filter(!is.na(Player)) %>% data.frame() %>%
    group_by(Season, Date, Game.ID, Player, Score.Cat, Strength.State, Season.Type) %>%
    summarise(G = sum(G), A1 = sum(A1), A2 = sum(A2)) %>% data.frame()
  
  player.bygame <- rbind_list(group_by(pbp.full, Season, Date, Game.ID, h1.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = h1.num) %>%
                                sum1p.home(),
                              group_by(pbp.full, Season, Date, Game.ID, h2.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = h2.num) %>%
                                sum1p.home(),
                              group_by(pbp.full, Season, Date, Game.ID, h3.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = h3.num) %>%
                                sum1p.home(),
                              group_by(pbp.full, Season, Date, Game.ID, h4.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = h4.num) %>%
                                sum1p.home(),
                              group_by(pbp.full, Season, Date, Game.ID, h5.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = h5.num) %>%
                                sum1p.home(),
                              group_by(pbp.full, Season, Date, Game.ID, h6.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = h6.num) %>%
                                sum1p.home(),
                              group_by(pbp.full, Season, Date, Game.ID, a1.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = a1.num) %>%
                                sum1p.away(),
                              group_by(pbp.full, Season, Date, Game.ID, a2.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = a2.num) %>%
                                sum1p.away(),
                              group_by(pbp.full, Season, Date, Game.ID, a3.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = a3.num) %>%
                                sum1p.away(),
                              group_by(pbp.full, Season, Date, Game.ID, a4.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = a4.num) %>%
                                sum1p.away(),
                              group_by(pbp.full, Season, Date, Game.ID, a5.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = a5.num) %>%
                                sum1p.away(),
                              group_by(pbp.full, Season, Date, Game.ID, a6.num, Score.Cat, Strength.State, Season.Type) %>% rename(Player = a6.num) %>%
                                sum1p.away()) %>%
    data.frame() %>% filter(!is.na(Player)) %>% data.frame() %>%
    group_by(Season, Date, Game.ID, Player, Score.Cat, Strength.State, Season.Type) %>%
    summarise(Position = first(roster.full$Position[match(Player, roster.full$Full.Name)]), Venue = first(Venue), Team = first(Team), TOI = sum(TOI),
              CF = sum(CF), CA = sum(CA), iCF = sum(iCF), 
              FF = sum(FF), FA = sum(FA), iFF = sum(iFF),
              SF = sum(SF), SA = sum(SA), iSF = sum(iSF),
              GF = sum(GF), GA = sum(GA), 
              xGF = sum(xGF), xGA = sum(xGA), ixG = sum(ixG),
              ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA),
              ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA),
              AxGF = sum(AxGF), AxGA = sum(AxGA),
              MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
              MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA),
              MxGF = sum(MxGF), MxGA = sum(MxGA),
              SCF = sum(SCF), SCA = sum(SCA),
              MSCF = sum(MSCF), MSCA = sum(MSCA),
              ASCF = sum(ASCF), ASCA = sum(ASCA),
              OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF),
              OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF),
              FOW = sum(FOW), FOL = sum(FOL), iFOW = sum(iFOW), iFOL = sum(iFOL),
              HF = sum(HF), HA = sum(HA), iHF = sum(iHF), iHA = sum(iHA),
              GVA = sum(GVA), TKA = sum(TKA), iGVA = sum(iGVA), iTKA = sum(iTKA),
              iBLK = sum(iBLK), iSCF = sum(iSCF),
              PENT = sum(PENT), PEND = sum(PEND), iPENT = sum(iPENT), iPEND = sum(iPEND),
              iDIST = sum(DIST), POSTF = sum(POSTF), POSTA = sum(POSTA), iPOSTF = sum(iPOSTF),
              RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA),
              iRB = sum(iRB), iRS = sum(iRS)) %>% data.frame() %>%
    merge(player.points, by.x = c("Season", "Game.ID", "Strength.State", "Score.Cat", "Player", "Date", "Season.Type"), by.y = c("Season", "Game.ID", "Strength.State", "Score.Cat", "Player", "Date", "Season.Type")) %>%
    data.frame() %>%
    merge(team.bygame %>% select(c(Season, Game.ID, Venue, Strength.State, Score.Cat, TOI, CF, CA, FF, FA, SF, SA, GF, GA, xGF, xGA, ACF, ACA, AFF, AFA, ASF, ASA, AGF, AGA, AxGF, AxGA, MCF, MCA, MFF, MFA, MSF, MSA, MGF, MGA, MxGF, MxGA, SCF, SCA, MSCF, MSCA, ASCF, ASCA, OZS, DZS, NZS)) %>% 
            rename(tTOI = TOI, tCF = CF, tCA = CA, tFF = FF, tFA = FA, tSF = SF, tSA = SA, tGF = GF, tGA = GA, txGF = xGF, txGA = xGA, 
                   tACF = ACF, tACA = ACA, tAFF = AFF, tAFA = AFA, tASF = ASF, tASA = ASA, tAGF = AGF, tAGA = AGA, tAxGF = AxGF, tAxGA = AxGA, 
                   tMCF = MCF, tMCA = MCA, tMFF = MFF, tMFA = MFA, tMSF = MSF, tMSA = MSA, tMGF = MGF, tMGA = MGA, tMxGF = MxGF, tMxGA = MxGA,
                   tSCF = SCF, tSCA = SCA, tMSCF = MSCF, tMSCA = MSCA, tASCF = ASCF, tASCA = ASCA,
                   tOZS = OZS, tDZS = DZS, tNZS = NZS) %>% data.frame(),
          by.x = c("Season", "Game.ID", "Venue", "Strength.State", "Score.Cat"), by.y = c("Season", "Game.ID", "Venue", "Strength.State", "Score.Cat")) %>% filter(Position != "G") %>% 
    mutate(Newcode = paste(Season, Game.ID, sep = ".")) %>% data.frame()
  
  grouped2 <- rbind_list(
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a2.num) %>% rename(P1 = a1.num, P2 = a2.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a3.num) %>% rename(P1 = a1.num, P2 = a3.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a4.num) %>% rename(P1 = a1.num, P2 = a4.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a5.num) %>% rename(P1 = a1.num, P2 = a5.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a3.num) %>% rename(P1 = a2.num, P2 = a3.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a4.num) %>% rename(P1 = a2.num, P2 = a4.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a5.num) %>% rename(P1 = a2.num, P2 = a5.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a3.num, a4.num) %>% rename(P1 = a3.num, P2 = a4.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a3.num, a5.num) %>% rename(P1 = a3.num, P2 = a5.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a4.num, a5.num) %>% rename(P1 = a4.num, P2 = a5.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a6.num) %>% rename(P1 = a1.num, P2 = a6.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a6.num) %>% rename(P1 = a2.num, P2 = a6.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a3.num, a6.num) %>% rename(P1 = a3.num, P2 = a6.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a4.num, a6.num) %>% rename(P1 = a4.num, P2 = a6.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a5.num, a6.num) %>% rename(P1 = a5.num, P2 = a6.num) %>% 
      sum2p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h2.num) %>% rename(P1 = h1.num, P2 = h2.num) %>% 
      sum2p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h3.num) %>% rename(P1 = h1.num, P2 = h3.num) %>% 
      sum2p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h4.num) %>% rename(P1 = h1.num, P2 = h4.num) %>% 
      sum2p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h5.num) %>% rename(P1 = h1.num, P2 = h5.num) %>% 
      sum2p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h3.num) %>% rename(P1 = h2.num, P2 = h3.num) %>% 
      sum2p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h4.num) %>% rename(P1 = h2.num, P2 = h4.num) %>% 
      sum2p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h5.num) %>% rename(P1 = h2.num, P2 = h5.num) %>% 
      sum2p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h3.num, h4.num) %>% rename(P1 = h3.num, P2 = h4.num) %>% 
      sum2p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h3.num, h5.num) %>% rename(P1 = h3.num, P2 = h5.num) %>% 
      sum2p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h4.num, h5.num) %>% rename(P1 = h4.num, P2 = h5.num) %>% 
      sum2p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h6.num) %>% rename(P1 = h1.num, P2 = h6.num) %>% 
      sum2p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h6.num) %>% rename(P1 = h2.num, P2 = h6.num) %>% 
      sum2p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h3.num, h6.num) %>% rename(P1 = h3.num, P2 = h6.num) %>% 
      sum2p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h4.num, h6.num) %>% rename(P1 = h4.num, P2 = h6.num) %>% 
      sum2p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h5.num, h6.num) %>% rename(P1 = h5.num, P2 = h6.num) %>% 
      sum2p.home()
  ) %>% group_by(Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, P1, P2, P3) %>% mutate(Combo.Code = code(P1, P2, P3), Newcode = paste(Season, Game.ID, sep = ".")) %>% filter(!is.na(P1) & !is.na(P2)) %>% data.frame()
  
  # Three-player combos / Combinaisons de trois joueurs
  grouped3 <- rbind_list(
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a2.num, a3.num) %>% rename(P1 = a1.num, P2 = a2.num, P3 = a3.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a3.num, a4.num) %>% rename(P1 = a1.num, P2 = a3.num, P3 = a4.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a4.num, a5.num) %>% rename(P1 = a1.num, P2 = a4.num, P3 = a5.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a3.num, a4.num) %>% rename(P1 = a2.num, P2 = a3.num, P3 = a4.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a4.num, a5.num) %>% rename(P1 = a2.num, P2 = a4.num, P3 = a5.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a2.num, a4.num) %>% rename(P1 = a1.num, P2 = a2.num, P3 = a4.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a2.num, a5.num) %>% rename(P1 = a1.num, P2 = a2.num, P3 = a5.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a3.num, a5.num) %>% rename(P1 = a1.num, P2 = a3.num, P3 = a5.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a3.num, a5.num) %>% rename(P1 = a2.num, P2 = a3.num, P3 = a5.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a3.num, a4.num, a5.num) %>% rename(P1 = a3.num, P2 = a4.num, P3 = a5.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a2.num, a6.num) %>% rename(P1 = a1.num, P2 = a2.num, P3 = a6.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a3.num, a6.num) %>% rename(P1 = a1.num, P2 = a3.num, P3 = a6.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a4.num, a6.num) %>% rename(P1 = a1.num, P2 = a4.num, P3 = a6.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a1.num, a5.num, a6.num) %>% rename(P1 = a1.num, P2 = a5.num, P3 = a6.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a3.num, a6.num) %>% rename(P1 = a2.num, P2 = a3.num, P3 = a6.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a4.num, a6.num) %>% rename(P1 = a2.num, P2 = a4.num, P3 = a6.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a2.num, a5.num, a6.num) %>% rename(P1 = a2.num, P2 = a5.num, P3 = a6.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a3.num, a4.num, a6.num) %>% rename(P1 = a3.num, P2 = a4.num, P3 = a6.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a3.num, a5.num, a6.num) %>% rename(P1 = a3.num, P2 = a5.num, P3 = a6.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, a4.num, a5.num, a6.num) %>% rename(P1 = a4.num, P2 = a5.num, P3 = a6.num) %>% 
      sum3p.away(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h2.num, h3.num) %>% rename(P1 = h1.num, P2 = h2.num, P3 = h3.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h3.num, h4.num) %>% rename(P1 = h1.num, P2 = h3.num, P3 = h4.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h4.num, h5.num) %>% rename(P1 = h1.num, P2 = h4.num, P3 = h5.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h3.num, h4.num) %>% rename(P1 = h2.num, P2 = h3.num, P3 = h4.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h4.num, h5.num) %>% rename(P1 = h2.num, P2 = h4.num, P3 = h5.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h2.num, h4.num) %>% rename(P1 = h1.num, P2 = h2.num, P3 = h4.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h2.num, h5.num) %>% rename(P1 = h1.num, P2 = h2.num, P3 = h5.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h3.num, h5.num) %>% rename(P1 = h1.num, P2 = h3.num, P3 = h5.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h3.num, h5.num) %>% rename(P1 = h2.num, P2 = h3.num, P3 = h5.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h3.num, h4.num, h5.num) %>% rename(P1 = h3.num, P2 = h4.num, P3 = h5.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h2.num, h6.num) %>% rename(P1 = h1.num, P2 = h2.num, P3 = h6.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h3.num, h6.num) %>% rename(P1 = h1.num, P2 = h3.num, P3 = h6.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h4.num, h6.num) %>% rename(P1 = h1.num, P2 = h4.num, P3 = h6.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h1.num, h5.num, h6.num) %>% rename(P1 = h1.num, P2 = h5.num, P3 = h6.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h3.num, h6.num) %>% rename(P1 = h2.num, P2 = h3.num, P3 = h6.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h4.num, h6.num) %>% rename(P1 = h2.num, P2 = h4.num, P3 = h6.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h2.num, h5.num, h6.num) %>% rename(P1 = h2.num, P2 = h5.num, P3 = h6.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h3.num, h4.num, h6.num) %>% rename(P1 = h3.num, P2 = h4.num, P3 = h6.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h3.num, h5.num, h6.num) %>% rename(P1 = h3.num, P2 = h5.num, P3 = h6.num) %>% 
      sum3p.home(),
    group_by(pbp.full, Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, h4.num, h5.num, h6.num) %>% rename(P1 = h4.num, P2 = h5.num, P3 = h6.num) %>% 
      sum3p.home()
  ) %>% group_by(Season, Date, Game.ID, Score.Cat, Strength.State, Season.Type, P1, P2, P3) %>% mutate(Combo.Code = code(P1, P2, P3), Newcode = paste(Season, Game.ID, sep = ".")) %>% filter(!is.na(P1) & !is.na(P2) & !is.na(P3)) %>% data.frame()
  
  # Combine / Combiner
  grouped <- rbind_list(grouped2, grouped3)
  
  combos.bygame <- merge(group_by(grouped, Season, Date, Game.ID, Newcode, Combo.Code, Venue, Score.Cat, Strength.State, Season.Type) %>%
                           summarise(Team = first(Team), TOI = sum(TOI),
                                     P1 = first(P1), P1.POS = first(roster.full$Position[match(P1, roster.full$Full.Name)]),
                                     P2 = first(P2), P2.POS = first(roster.full$Position[match(P2, roster.full$Full.Name)]),
                                     P3 = first(P3), P3.POS = first(roster.full$Position[match(P3, roster.full$Full.Name)]),
                                     P1.G = sum(P1.G), P1.A1 = sum(P1.A1), P1.A2 = sum(P1.A2),
                                     P2.G = sum(P2.G), P2.A1 = sum(P2.A1), P2.A2 = sum(P2.A2),
                                     P3.G = sum(P3.G), P3.A1 = sum(P3.A1), P3.A2 = sum(P3.A2),
                                     P1.A1.P2 = sum(P1.A1.P2), P1.A2.P2 = sum(P1.A2.P2),
                                     P2.A1.P1 = sum(P2.A1.P1), P2.A2.P1 = sum(P2.A2.P1),
                                     P1.A1.P3 = sum(P1.A1.P3), P1.A2.P3 = sum(P1.A2.P3),
                                     P2.A1.P3 = sum(P2.A1.P3), P2.A2.P3 = sum(P2.A2.P3),
                                     P3.A1.P1 = sum(P3.A1.P1), P3.A2.P1 = sum(P3.A2.P1),
                                     P3.A1.P2 = sum(P3.A1.P2), P3.A2.P2 = sum(P3.A2.P2),
                                     CF = sum(CF), CA = sum(CA),
                                     FF = sum(FF), FA = sum(FA),
                                     SF = sum(SF), SA = sum(SA),
                                     GF = sum(GF), GA = sum(GA), 
                                     xGF = sum(xGF), xGA = sum(xGA),
                                     ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA),
                                     ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA),
                                     AxGF = sum(AxGF), AxGA = sum(AxGA),
                                     MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
                                     MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA),
                                     MxGF = sum(MxGF), MxGA = sum(MxGA),
                                     SCF = sum(SCF), SCA = sum(SCA),
                                     MSCF = sum(MSCF), MSCA = sum(MSCA),
                                     ASCF = sum(ASCF), ASCA = sum(ASCA),
                                     OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS),
                                     OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF),
                                     FOW = sum(FOW), FOL = sum(FOL),
                                     HF = sum(HF), HA = sum(HA),
                                     GVA = sum(GVA), TKA = sum(TKA),
                                     PENT = sum(PENT), PEND = sum(PEND),
                                     POSTF = sum(POSTF), POSTA = sum(POSTA),
                                     RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA)) %>% data.frame(),
                         team.bygame %>% select(c(Season, Game.ID, Venue, Strength.State, Score.Cat, TOI, CF, CA, FF, FA, SF, SA, GF, GA, xGF, xGA, ACF, ACA, AFF, AFA, ASF, ASA, AGF, AGA, AxGF, AxGA, MCF, MCA, MFF, MFA, MSF, MSA, MGF, MGA, MxGF, MxGA, SCF, SCA, MSCF, MSCA, ASCF, ASCA, OZS, DZS, NZS)) %>% 
                           rename(tTOI = TOI, tCF = CF, tCA = CA, tFF = FF, tFA = FA, tSF = SF, tSA = SA, tGF = GF, tGA = GA, txGF = xGF, txGA = xGA, 
                                  tACF = ACF, tACA = ACA, tAFF = AFF, tAFA = AFA, tASF = ASF, tASA = ASA, tAGF = AGF, tAGA = AGA, tAxGF = AxGF, tAxGA = AxGA, 
                                  tMCF = MCF, tMCA = MCA, tMFF = MFF, tMFA = MFA, tMSF = MSF, tMSA = MSA, tMGF = MGF, tMGA = MGA, tMxGF = MxGF, tMxGA = MxGA,
                                  tSCF = SCF, tSCA = SCA, tMSCF = MSCF, tMSCA = MSCA, tASCF = ASCF, tASCA = ASCA,
                                  tOZS = OZS, tDZS = DZS, tNZS = NZS) %>% data.frame(),
                         by.x = c("Season", "Game.ID", "Venue", "Strength.State", "Score.Cat"), by.y = c("Season", "Game.ID", "Venue", "Strength.State", "Score.Cat")) %>%
    filter(as.numeric(as.character(TOI)) > 0 & {is.na(P1.POS) | as.character(P1.POS) != "G"} & {is.na(P2.POS) | as.character(P2.POS) != "G"} & {is.na(P3.POS) | as.character(P3.POS) != "G"}) %>% data.frame()
  
  # Assists
  ### GROUP BY P1, P2, P3 INSTEAD OF COMBO.CODE ###
  assist.bygame <- filter(grouped, P3 == "X") %>% group_by(Season, Date, Game.ID, Season.Type, Venue, Team, P1, P2) %>%
    summarise(Combo.Code = first(Combo.Code),
              P1.A1.P2 = sum(na.omit(P1.A1.P2)), P1.A2.P2 = sum(na.omit(P1.A2.P2)),
              P2.A1.P1 = sum(na.omit(P2.A1.P1)), P2.A2.P1 = sum(na.omit(P2.A2.P1))) %>%
    mutate(Newcode = paste(Season, Game.ID, sep = ".")) %>% data.frame()
  
  # Trim PBP / Réduire résumé
  pbp.full <- select(pbp.full, -c(FOS, ZF, is.NZ, is.PP, ref, Since, Zone.Start, Since.Cat, Category1, Category2)) %>% data.frame()
  
  # Scrape bio data / Acquérir données bio
  bios(teams = "all")
  
  if(to.db == TRUE) {
  
    ## WRITE TO DATABASE / AJOUTER A LA BASE DE DONNÉES
    
    # Link to database / Connecter a la base de données
    link <- "~/corsica.sqlite"
    newcon <- dbConnect(SQLite(), link)
    
    # List games already in database / Chercher matchs déja présents dans la base de données
    db.games <- try(unique(sqliteQuickColumn(newcon, "roster", "Newcode")))
    
    if(class(db.games) == "try-error") {db.games <- NULL}
    
    # Replace old data / Remplacer anciennes données
    bio <- try(dbReadTable(newcon, "bio"))
    
    if(class(bio) == "try-error") {
      newbio <- bio.full
    } else {
      newbio <- rbind_list(
        filter(bio, Player.Code %in% unique(bio.full$Player.Code) == FALSE) %>% data.frame(),
        bio.full
      ) %>% data.frame()
    }
    
    # Remove overlapping games / Éliminer matchs extras
    pbp <- filter(pbp.full, Newcode %in% db.games == FALSE)
    roster <- filter(roster.full, Newcode %in% db.games == FALSE)
    team <- filter(team.bygame, Newcode %in% db.games == FALSE)
    goalie <- filter(goalie.bygame, Newcode %in% db.games == FALSE)
    player <- filter(player.bygame, Newcode %in% db.games == FALSE)
    combo <- filter(combos.bygame, Newcode %in% db.games == FALSE)
    assist <- filter(assist.bygame, Newcode %in% db.games == FALSE)
    
    # Write tables / Ajouter les tables
    dbWriteTable(newcon, "pbp", pbp, overwrite = FALSE, append = TRUE)
    dbWriteTable(newcon, "roster", roster, overwrite = FALSE, append = TRUE)
    dbWriteTable(newcon, "team", team, overwrite = FALSE, append = TRUE)
    dbWriteTable(newcon, "goalie", goalie, overwrite = FALSE, append = TRUE)
    dbWriteTable(newcon, "player", player, overwrite = FALSE, append = TRUE)
    dbWriteTable(newcon, "combo", combo, overwrite = FALSE, append = TRUE)
    dbWriteTable(newcon, "assist", assist, overwrite = FALSE, append = TRUE)
    dbWriteTable(newcon, "bio", newbio, overwrite = TRUE)
    
    dbDisconnect(newcon)
    
  } else {
    
    pbp.full %!% "pbp.full"
    roster.full %!% "roster.full"
    team.bygame %!% "team.bygame"
    goalie.bygame %!% "goalie.bygame"
    player.bygame %!% "skater.bygame"
    combos.bygame %!% "combo.bygame"
    assist.bygame %!% "assists"
    
  }
  
  return(TRUE)
  
}

push <- function(key = TRUE, season) {
  
  if(key == TRUE) {cat("Pushing...\n")}
  
  ## Load DB tables / Charger tables de la base de données
  # Link to database / Connecter a la base de données
  link1 <- "~/corsica.sqlite"
  con1 <- dbConnect(SQLite(), link1)
  
  # Read tables / Lire les tables 
  roster <- dbReadTable(con1, "roster")
  
  bio <- dbReadTable(con1, "bio")
  
  pbpquery <- dbSendQuery(con1, paste("SELECT * FROM pbp WHERE Season = ", season, sep = ""))
  pbp <- fetch(pbpquery, -1)
  
  teamquery <- dbSendQuery(con1, paste("SELECT * FROM team WHERE Season = ", season, sep = ""))
  team <- fetch(teamquery, -1)
  
  goaliequery <- dbSendQuery(con1, paste("SELECT * FROM goalie WHERE Season = ", season, sep = ""))
  goalie <- fetch(goaliequery, -1)
  
  playerquery <- dbSendQuery(con1, paste("SELECT * FROM player WHERE Season = ", season, sep = ""))
  player <- fetch(playerquery, -1)
  
  comboquery <- dbSendQuery(con1, paste("SELECT * FROM combo WHERE Season = ", season, sep = ""))
  combo <- fetch(comboquery, -1)
  
  assistquery <- dbSendQuery(con1, paste("SELECT * FROM assist WHERE Season = ", season, sep = ""))
  assist <- fetch(assistquery, -1)
  
  dbDisconnect(con1)
  
  ## Load current files / Charger fichiers 
  # Link to database / Connecter a la base de données
  link2 <- "~/fenwicka.sqlite"
  con2 <- dbConnect(SQLite(), link2)
  
  teamseason <- try(dbReadTable(con2, "teamseason"))
  goalieseason <- try(dbReadTable(con2, "goalieseason"))
  playerseason <- try(dbReadTable(con2, "playerseason"))
  pairseason <- try(dbReadTable(con2, "pairseason"))
  lineseason <- try(dbReadTable(con2, "lineseason"))
  assistseason <- try(dbReadTable(con2, "assistseason"))
  coords <- try(dbReadTable(con2, "coords"))
  highlights <- try(dbReadTable(con2, "highlights"))
  schedule_ <- try(dbReadTable(con2, "schedule"))
  standings <- try(dbReadTable(con2, "standings"))
  
  # Remove current season / Éliminer saison actuelle
  if(class(teamseason) != "try-error") {team.s.hist <- filter(teamseason, Season != season) %>% data.frame()} else {team.s.hist <- NULL}
  if(class(goalieseason) != "try-error") {goalie.s.hist <- filter(goalieseason, Season != season) %>% data.frame()} else {goalie.s.hist <- NULL}
  if(class(playerseason) != "try-error") {player.s.hist <- filter(playerseason, Season != season) %>% data.frame()} else {player.s.hist <- NULL}
  if(class(pairseason) != "try-error") {pair.s.hist <- filter(pairseason, Season != season) %>% data.frame()} else {pair.s.hist <- NULL}
  if(class(lineseason) != "try-error") {line.s.hist <- filter(lineseason, Season != season) %>% data.frame()} else {line.s.hist <- NULL}
  if(class(assistseason) != "try-error") {assist.s.hist <- filter(assistseason, Season != season) %>% data.frame()} else {assist.s.hist <- NULL}
  if(class(coords) != "try-error") {coord.s.hist <- filter(coords, Season != season) %>% data.frame()} else {coord.s.hist <- NULL}
  if(class(highlights) != "try-error") {highlights.s.hist <- filter(highlights, Season != season) %>% data.frame()} else {highlights.s.hist <- NULL}
  if(class(schedule_) != "try-error") {schedule.s.hist <- filter(schedule_, Season != season) %>% data.frame()} else {schedule.s.hist <- NULL}
  if(class(standings) != "try-error") {standings.s.hist <- filter(standings, Season != season) %>% data.frame()} else {standings.s.hist <- NULL}
  
  ## Aggregate stats by season / Agréger les statistiques par saison
  
  # Roster / Formation
  roster <- group_by(roster, Full.Name, Player.Code, Season, Season.Type) %>% 
    summarise(Team = paste(unique(Team), collapse = "/"), Number = paste(unique(Number), collapse = "/"), Team.Num = paste(unique(Team.Num), collapse = "/"), 
              Position = paste(unique(Position), collapse = "/"), Last.Name = first(Last.Name), First.Name = first(First.Name)) %>%
    data.frame() %>%
    merge(bio, by.x = "Player.Code", by.y = "Player.Code", all.x = TRUE) %>%
    data.frame()
  
  # QoC/QoT stuff
  pbp.qual <- select(pbp, c(Season, Date, Game.ID, h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, a1.num, a2.num, a3.num, a4.num, a5.num, a6.num, Home.Team, Away.Team, Score.Cat, Strength.State, Season.Type, Event.Length)) %>% data.frame()
  
  pbp.qual$h1.num[which(pbp.qual$h1.num %in% unique(player$Player) == FALSE)] <- "GOALIE"; pbp.qual$h2.num[which(pbp.qual$h2.num %in% unique(player$Player) == FALSE)] <- "GOALIE"
  pbp.qual$h3.num[which(pbp.qual$h3.num %in% unique(player$Player) == FALSE)] <- "GOALIE"; pbp.qual$h4.num[which(pbp.qual$h4.num %in% unique(player$Player) == FALSE)] <- "GOALIE"
  pbp.qual$h5.num[which(pbp.qual$h5.num %in% unique(player$Player) == FALSE)] <- "GOALIE"; pbp.qual$h6.num[which(pbp.qual$h6.num %in% unique(player$Player) == FALSE)] <- "GOALIE"
  pbp.qual$a1.num[which(pbp.qual$a1.num %in% unique(player$Player) == FALSE)] <- "GOALIE"; pbp.qual$a2.num[which(pbp.qual$a2.num %in% unique(player$Player) == FALSE)] <- "GOALIE"
  pbp.qual$a3.num[which(pbp.qual$a3.num %in% unique(player$Player) == FALSE)] <- "GOALIE"; pbp.qual$a4.num[which(pbp.qual$a4.num %in% unique(player$Player) == FALSE)] <- "GOALIE"
  pbp.qual$a5.num[which(pbp.qual$a5.num %in% unique(player$Player) == FALSE)] <- "GOALIE"; pbp.qual$a6.num[which(pbp.qual$a6.num %in% unique(player$Player) == FALSE)] <- "GOALIE"
  
  # Coordinates
  coords <- filter(pbp, !is.na(XC) & !is.na(YC)) %>% 
    select(c(Season, Game.ID, Period,
             Event, Detail, ev.team, ev.zone, p1, p2, p3, 
             h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, 
             a1.num, a2.num, a3.num, a4.num, a5.num, a6.num,
             Home.Team, Away.Team, Home.Goalie, Away.Goalie,
             Strength.State, XC, YC, xG)) %>%
    data.frame()
  
  # Write
  coords <- rbind_list(coord.s.hist, coords) %>% data.frame()
  dbWriteTable(con2, "coords", coords, overwrite = TRUE)
  
  # Highlights
  hl <- {
    pbp %>%
      filter(!is.na(URL) & URL != "https://www.nhl.com/video/c-")
  }
  
  # Write
  hl <- rbind_list(highlights.s.hist, hl) %>% data.frame()
  dbWriteTable(con2, "highlights", hl, overwrite = TRUE)
  
  # Team / Équipes
  teamgp <- group_by(team, Team, Season, Season.Type) %>% summarise(GP = length(unique(Newcode))) %>% data.frame() %>%
    mutate(Code = paste(Team, Season, Season.Type, sep = ".")) %>% data.frame()
  
  # Group leftover strength states / Combiner états de forces mineurs 
  team$Strength.State[which(team$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "4v3", "3v4", "0v0") == FALSE)] <- "XvX" #EvE?
  
  teamseason <- filter(team, Strength.State != "0v0") %>% group_by(Team, Season, Venue, Strength.State, Score.Cat, Season.Type) %>%
    summarise(GP = teamgp$GP[match(paste(first(Team), first(Season), first(Season.Type), sep = "."), teamgp$Code)], 
              TOI = sum(TOI), CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
              xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
              AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
              MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
              SCF = sum(SCF), SCA = sum(SCA), ASCF = sum(ASCF), ASCA = sum(ASCA), MSCF = sum(MSCF), MSCA = sum(MSCA),
              OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
              DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
              PENT = sum(PENT), PEND = sum(PEND), DISTF = sum(DISTF), DISTA = sum(DISTA),
              RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA), POSTF = sum(POSTF), POSTA = sum(POSTA)) %>% data.frame()
  
  # Write
  teamseason <- rbind_list(team.s.hist, teamseason) %>% data.frame()
  dbWriteTable(con2, "teamseason", teamseason, overwrite = TRUE)
  
  # Goalie / Gardiens
  goaliegp <- group_by(goalie, Player, Season, Season.Type) %>% summarise(GP = length(unique(Newcode))) %>% data.frame() %>%
    mutate(Code = paste(Player, Season, Season.Type, sep = ".")) %>% data.frame()
  
  # Group leftover strength states / Combiner états de forces mineurs 
  goalie$Strength.State[which(goalie$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "4v3", "3v4", "0v0") == FALSE)] <- "XvX" #EvE?
  
  goalieseason <- filter(goalie, Strength.State != "0v0") %>% group_by(Player, Season, Team, Venue, Strength.State, Score.Cat, Season.Type) %>%
    summarise(GP = goaliegp$GP[match(paste(first(Player), first(Season), first(Season.Type), sep = "."), goaliegp$Code)], 
              TOI = sum(TOI), CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), 
              GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
              AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
              MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
              HDSF = sum(HDSF), HDSA = sum(HDSA), MDSF = sum(MDSF), MDSA = sum(MDSA), LDSF = sum(LDSF), LDSA = sum(LDSA),
              HDGF = sum(HDGF), HDGA = sum(HDGA), MDGF = sum(MDGF), MDGA = sum(MDGA), LDGF = sum(LDGF), LDGA = sum(LDGA),
              OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
              DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA), GVA = sum(GVA), TKA = sum(TKA), 
              PENT = sum(PENT), PEND = sum(PEND), DISTA = sum(DISTA), G = sum(G), A1 = sum(na.omit(A1)), A2 = sum(na.omit(A2)),
              iPENT = sum(iPENT), iPEND = sum(na.omit(iPEND)),
              RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA), POSTF = sum(POSTF), POSTA = sum(POSTA)) %>% data.frame()
  
  # Write
  goalieseason <- rbind_list(goalie.s.hist, goalieseason) %>% data.frame()
  dbWriteTable(con2, "goalieseason", goalieseason, overwrite = TRUE)
  
  # Player / Joueurs 
  playergp <- group_by(player, Player, Season, Season.Type) %>% summarise(GP = length(unique(Newcode))) %>% data.frame() %>%
    mutate(Code = paste(Player, Season, Season.Type, sep = ".")) %>% data.frame()
  
  # Group leftover strength states / Combiner états de forces mineurs 
  player$Strength.State[which(player$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "0v0") == FALSE)] <- "XvX" #EvE?
  
  # Group score states / Combiner états de score
  player$Score.Cat[which(player$Score.Cat < 0)] <- -1
  player$Score.Cat[which(player$Score.Cat > 0)] <- 1
  
  playerseason <- filter(player, Strength.State != "0v0") %>% group_by(Player, Season, Team, Venue, Strength.State, Score.Cat, Season.Type) %>%
    summarise(GP = playergp$GP[match(paste(first(Player), first(Season), first(Season.Type), sep = "."), playergp$Code)], 
              Position = roster$Position[match(first(Player), roster$Full.Name)],
              TOI = sum(TOI), CF = sum(CF), CA = sum(CA), iCF = sum(iCF), FF = sum(FF), FA = sum(FA), iFF = sum(iFF), 
              SF = sum(SF), SA = sum(SA), iSF = sum(iSF), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA), ixG = sum(ixG), 
              ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
              AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
              MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
              SCF = sum(SCF), SCA = sum(SCA), ASCF = sum(ASCF), ASCA = sum(ASCA), MSCF = sum(MSCF), MSCA = sum(MSCA),
              OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), 
              FOW = sum(FOW), FOL = sum(FOL), iFOW = sum(iFOW), iFOL = sum(iFOL), HF = sum(HF), HA = sum(HA), iHF = sum(iHF), iHA = sum(iHA), 
              GVA = sum(GVA), TKA = sum(TKA), iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK), PENT = sum(PENT), PEND = sum(PEND),
              iSCF = sum(iSCF), iPOSTF = sum(iPOSTF), POSTF = sum(POSTF), POSTA = sum(POSTA),
              iDIST = sum(iDIST), G = sum(G), A1 = sum(na.omit(A1)), A2 = sum(na.omit(A2)), iPENT = sum(iPENT), iPEND = sum(na.omit(iPEND)),
              tTOI = sum(tTOI), tCF = sum(tCF), tCA = sum(tCA), tFF = sum(tFF), tFA = sum(tFA), tSF = sum(tSF), tSA = sum(tSA), 
              tGF = sum(tGF), tGA = sum(tGA), txGF = sum(txGF), txGA = sum(txGA), tACF = sum(tACF), tACA = sum(tACA), tAFF = sum(tAFF), tAFA = sum(tAFA), 
              tASF = sum(tASF), tASA = sum(tASA), tAGF = sum(tAGF), tAGA = sum(tAGA), tAxGF = sum(tAxGF), tAxGA = sum(tAxGA), tMCF = sum(tMCF), tMCA = sum(tMCA), 
              tMFF = sum(tMFF), tMFA = sum(tMFA), tMSF = sum(tMSF), tMSA = sum(tMSA), tMGF = sum(tMGF), tMGA = sum(tMGA), tMxGF = sum(tMxGF), tMxGA = sum(tMxGA),
              tSCF = sum(tSCF), tSCA = sum(tSCA), tMSCF = sum(tMSCF), tMSCA = sum(tMSCA), tASCF = sum(tASCF), tASCA = sum(tASCA),
              tOZS = sum(tOZS), tDZS = sum(tDZS), tNZS = sum(tNZS),
              RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA),
              iRB = sum(iRB), iRS = sum(iRS)) %>% data.frame() %>%
    mutate(OCF = tCF - CF, OCA = tCA - CA,
           OFF = tFF - FF, OFA = tFA - FA,
           OSF = tSF - SF, OSA = tSA - SA,
           OGF = tGF - GF, OGA = tGA - GA,
           OxGF = txGF - xGF, OxGA = txGA - xGA, 
           OACF = tACF - ACF, OACA = tACA - ACA,
           OAFF = tAFF - AFF, OAFA = tAFA - AFA,
           OASF = tASF - ASF, OASA = tASA - ASA,
           OAGF = tAGF - AGF, OAGA = tAGA - AGA,
           OAxGF = tAxGF - AxGF, OAxGA = tAxGA - AxGA, 
           OMCF = tMCF - MCF, OMCA = tMCA - MCA,
           OMFF = tMFF - MFF, OMFA = tMFA - MFA, 
           OMSF = tMSF - MSF, OMSA = tMSA - MSA,
           OMGF = tMGF - MGF, OMGA = tMGA - MGA,
           OMxGF = tMxGF - MxGF, OMxGA = tMxGA - MxGA,
           OSCF = tSCF - SCF, OSCA = tSCA - SCA,
           OMSCF = tMSCF - MSCF, OMSCA = tMSCA - MSCA,
           OASCF = tASCF - ASCF, OASCA = tASCA - ASCA,
           OOZS = tOZS - OZS, ODZS = tDZS - DZS, ONZS = tNZS - NZS) %>% data.frame() %>%
    select(-c(tCF:tNZS)) %>%
    data.frame()
  
  # Combos / Combinaisons
  
  # Group leftover strength states / Combiner états de forces mineurs 
  combo$Strength.State[which(combo$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "0v0") == FALSE)] <- "XvX" # EXCLUDE SHOOTOUT
  
  # List regular combinations / Chercher combinaisons communs
  regcombo <- group_by(combo, Combo.Code, Season, Season.Type) %>% summarise(TOI = sum(TOI)) %>% mutate(Newcode = paste(Combo.Code, Season, Season.Type, sep = "-")) %>% filter(TOI >= 25) %>% data.frame()
  
  lineseason <- filter(combo, grepl("C|L|R", as.character(P3.POS)) == TRUE & grepl("C|L|R", as.character(P2.POS)) == TRUE & grepl("C|L|R", as.character(P1.POS)) == TRUE & Strength.State != "0v0") %>% 
    group_by(Combo.Code, Season, Strength.State, Season.Type, Venue) %>%
    summarise(Team = paste(unique(Team), collapse = "/"), TOI = sum(TOI), P1 = first(P1), P1.POS = first(P1.POS), 
              P2 = first(P2), P2.POS = first(P2.POS), P3 = first(P3), P3.POS = first(P3.POS),
              CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
              SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
              ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
              AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
              MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
              SCF = sum(SCF), SCA = sum(SCA), ASCF = sum(ASCF), ASCA = sum(ASCA), MSCF = sum(MSCF), MSCA = sum(MSCA),
              OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), 
              FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA),
              GVA = sum(GVA), TKA = sum(TKA), PENT = sum(PENT), PEND = sum(PEND),
              RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA), POSTF = sum(POSTF), POSTA = sum(POSTA),
              P1.G = sum(P1.G), P1.A1 = sum(na.omit(P1.A1)), P1.A2 = sum(na.omit(P1.A2)),
              P2.G = sum(P2.G), P2.A1 = sum(na.omit(P2.A1)), P2.A2 = sum(na.omit(P2.A2)),
              P3.G = sum(P3.G), P3.A1 = sum(na.omit(P3.A1)), P3.A2 = sum(na.omit(P3.A2)),
              tTOI = sum(tTOI), tCF = sum(tCF), tCA = sum(tCA), tFF = sum(tFF), tFA = sum(tFA), tSF = sum(tSF), tSA = sum(tSA), 
              tGF = sum(tGF), tGA = sum(tGA), txGF = sum(txGF), txGA = sum(txGA), tACF = sum(tACF), tACA = sum(tACA), tAFF = sum(tAFF), tAFA = sum(tAFA), 
              tASF = sum(tASF), tASA = sum(tASA), tAGF = sum(tAGF), tAGA = sum(tAGA), tAxGF = sum(tAxGF), tAxGA = sum(tAxGA), tMCF = sum(tMCF), tMCA = sum(tMCA), 
              tMFF = sum(tMFF), tMFA = sum(tMFA), tMSF = sum(tMSF), tMSA = sum(tMSA), tMGF = sum(tMGF), tMGA = sum(tMGA), tMxGF = sum(tMxGF), tMxGA = sum(tMxGA),
              tSCF = sum(tSCF), tSCA = sum(tSCA), tMSCF = sum(tMSCF), tMSCA = sum(tMSCA), tASCF = sum(tASCF), tASCA = sum(tASCA),
              tOZS = sum(tOZS), tDZS = sum(DZS), tNZS = sum(tNZS)) %>% data.frame() %>%
    mutate(OCF = tCF - CF, OCA = tCA - CA,
           OFF = tFF - FF, OFA = tFA - FA,
           OSF = tSF - SF, OSA = tSA - SA,
           OGF = tGF - GF, OGA = tGA - GA,
           OxGF = txGF - xGF, OxGA = txGA - xGA, 
           OACF = tACF - ACF, OACA = tACA - ACA,
           OAFF = tAFF - AFF, OAFA = tAFA - AFA,
           OASF = tASF - ASF, OASA = tASA - ASA,
           OAGF = tAGF - AGF, OAGA = tAGA - AGA,
           OAxGF = tAxGF - AxGF, OAxGA = tAxGA - AxGA, 
           OMCF = tMCF - MCF, OMCA = tMCA - MCA,
           OMFF = tMFF - MFF, OMFA = tMFA - MFA, 
           OMSF = tMSF - MSF, OMSA = tMSA - MSA,
           OMGF = tMGF - MGF, OMGA = tMGA - MGA,
           OMxGF = tMxGF - MxGF, OMxGA = tMxGA - MxGA,
           OSCF = tSCF - SCF, OSCA = tSCA - SCA,
           OMSCF = tMSCF - MSCF, OMSCA = tMSCA - MSCA,
           OASCF = tASCF - ASCF, OASCA = tASCA - ASCA,
           OOZS = tOZS - OZS, ODZS = tDZS - DZS, ONZS = tNZS - NZS,
           Newcode = paste(Combo.Code, Season, Season.Type, sep = "-")) %>%
    select(-c(tCF:tASCA)) %>% data.frame() %>% 
    filter(Newcode %in% regcombo$Newcode) %>% select(-c(Newcode)) %>% data.frame()
  
  pairseason <- filter(combo, as.character(P3) == "X" & grepl("D", as.character(P2.POS)) == TRUE & grepl("D", as.character(P1.POS)) == TRUE & Strength.State != "0v0") %>% 
    group_by(Combo.Code, Season, Strength.State, Season.Type, Venue) %>%
    summarise(Team = paste(unique(Team), collapse = "/"), TOI = sum(TOI), 
              P1 = first(P1), P1.POS = first(P1.POS), P2 = first(P2), P2.POS = first(P2.POS),
              CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
              SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
              ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
              AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
              MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
              SCF = sum(SCF), SCA = sum(SCA), ASCF = sum(ASCF), ASCA = sum(ASCA), MSCF = sum(MSCF), MSCA = sum(MSCA),
              OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), 
              FOW = sum(FOW), FOL = sum(FOL), HF = sum(HF), HA = sum(HA),
              GVA = sum(GVA), TKA = sum(TKA), PENT = sum(PENT), PEND = sum(PEND), 
              RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA), POSTF = sum(POSTF), POSTA = sum(POSTA),
              P1.G = sum(P1.G), P1.A1 = sum(na.omit(P1.A1)), P1.A2 = sum(na.omit(P1.A2)),
              P2.G = sum(P2.G), P2.A1 = sum(na.omit(P2.A1)), P2.A2 = sum(na.omit(P2.A2)),
              tTOI = sum(tTOI), tCF = sum(tCF), tCA = sum(tCA), tFF = sum(tFF), tFA = sum(tFA), tSF = sum(tSF), tSA = sum(tSA), 
              tGF = sum(tGF), tGA = sum(tGA), txGF = sum(txGF), txGA = sum(txGA), tACF = sum(tACF), tACA = sum(tACA), tAFF = sum(tAFF), tAFA = sum(tAFA), 
              tASF = sum(tASF), tASA = sum(tASA), tAGF = sum(tAGF), tAGA = sum(tAGA), tAxGF = sum(tAxGF), tAxGA = sum(tAxGA), tMCF = sum(tMCF), tMCA = sum(tMCA), 
              tMFF = sum(tMFF), tMFA = sum(tMFA), tMSF = sum(tMSF), tMSA = sum(tMSA), tMGF = sum(tMGF), tMGA = sum(tMGA), tMxGF = sum(tMxGF), tMxGA = sum(tMxGA),
              tSCF = sum(tSCF), tSCA = sum(tSCA), tMSCF = sum(tMSCF), tMSCA = sum(tMSCA), tASCF = sum(tASCF), tASCA = sum(tASCA),
              tOZS = sum(tOZS), tDZS = sum(DZS), tNZS = sum(tNZS)) %>% data.frame() %>%
    mutate(OCF = tCF - CF, OCA = tCA - CA,
           OFF = tFF - FF, OFA = tFA - FA,
           OSF = tSF - SF, OSA = tSA - SA,
           OGF = tGF - GF, OGA = tGA - GA,
           OxGF = txGF - xGF, OxGA = txGA - xGA, 
           OACF = tACF - ACF, OACA = tACA - ACA,
           OAFF = tAFF - AFF, OAFA = tAFA - AFA,
           OASF = tASF - ASF, OASA = tASA - ASA,
           OAGF = tAGF - AGF, OAGA = tAGA - AGA,
           OAxGF = tAxGF - AxGF, OAxGA = tAxGA - AxGA, 
           OMCF = tMCF - MCF, OMCA = tMCA - MCA,
           OMFF = tMFF - MFF, OMFA = tMFA - MFA, 
           OMSF = tMSF - MSF, OMSA = tMSA - MSA,
           OMGF = tMGF - MGF, OMGA = tMGA - MGA,
           OMxGF = tMxGF - MxGF, OMxGA = tMxGA - MxGA,
           OSCF = tSCF - SCF, OSCA = tSCA - SCA,
           OMSCF = tMSCF - MSCF, OMSCA = tMSCA - MSCA,
           OASCF = tASCF - ASCF, OASCA = tASCA - ASCA,
           OOZS = tOZS - OZS, ODZS = tDZS - DZS, ONZS = tNZS - NZS,
           Newcode = paste(Combo.Code, Season, Season.Type, sep = "-")) %>%
    select(-c(tCF:tASCA)) %>% data.frame() %>%
    filter(Newcode %in% regcombo$Newcode) %>% select(-c(Newcode)) %>% data.frame()
  
  # Write
  pairseason <- rbind_list(pair.s.hist, pairseason) %>% data.frame()
  lineseason <- rbind_list(line.s.hist, lineseason) %>% data.frame()
  dbWriteTable(con2, "pairseason", pairseason, overwrite = TRUE)
  dbWriteTable(con2, "lineseason", lineseason, overwrite = TRUE)
  
  # Assists
  assistseason <- group_by(assist, P1, P2, Season, Team) %>%
    summarise(Combo.Code = first(Combo.Code),
              P1.A1.P2 = sum(na.omit(P1.A1.P2)), P1.A2.P2 = sum(na.omit(P1.A2.P2)),
              P2.A1.P1 = sum(na.omit(P2.A1.P1)), P2.A2.P1 = sum(na.omit(P2.A2.P1))) %>%
    data.frame()
  
  # Write
  assistseason <- rbind_list(assist.s.hist, assistseason) %>% data.frame()
  dbWriteTable(con2, "assistseason", assistseason, overwrite = TRUE)
  
  # QoC, QoT
  
  # Build player network / Construire réseau de joueurs
  plist <- c(unique(player$Player), "GOALIE")
  
  pmatrix <- cbind(rep(plist, each = length(plist)), rep(plist, times = length(plist))) %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  colnames(pmatrix) <- c("Player", "Without")
  
  pmatrix <- group_by(pmatrix, Player, Without) %>% mutate(Dummy = "X", Season = season, Combo.Code = code(first(Player), first(Without), first(Dummy))) %>% data.frame()
  
  # Condense player and combo tables / Condenser tables de joueurs et combinaisons
  player.ref <- filter(player, Strength.State == "5v5") %>% 
    group_by(Player, Season) %>%
    summarise(TOI = sum(TOI),
              tTOI = sum(tTOI),
              CF = sum(CF), CA = sum(CA),
              xGF = sum(xGF), xGA = sum(xGA)) %>%
    mutate(TOI. = TOI/tTOI) %>%
    data.frame()
  
  combo.ref <- filter(combo, Strength.State == "5v5" & P3 == "X") %>% 
    group_by(Combo.Code, Season) %>%
    summarise(CF = sum(CF), CA = sum(CA),
              xGF = sum(xGF), xGA = sum(xGA)) %>%
    data.frame()
  
  # Clear
  rm(list = c("player"))
  
  # Compile player-without stats / Calculer statistiques pour chaque joueur sans l'autre
  player.without <- rbind_list(
    filter(pmatrix, pmatrix$Combo.Code %in% combo.ref$Combo.Code) %>% group_by(Player, Without, Season) %>% 
      mutate(Player.TOI. = first(player.ref$TOI.[match(Player, player.ref$Player)]),
             Player.CF = first(player.ref$CF[match(Player, player.ref$Player)]),
             Player.CA = first(player.ref$CA[match(Player, player.ref$Player)]),
             Player.xGF = first(player.ref$xGF[match(Player, player.ref$Player)]),
             Player.xGA = first(player.ref$xGA[match(Player, player.ref$Player)]),
             Combo.CF = first(combo.ref$CF[match(Combo.Code, combo.ref$Combo.Code)]),
             Combo.CA = first(combo.ref$CA[match(Combo.Code, combo.ref$Combo.Code)]),
             Combo.xGF = first(combo.ref$xGF[match(Combo.Code, combo.ref$Combo.Code)]),
             Combo.xGA = first(combo.ref$xGA[match(Combo.Code, combo.ref$Combo.Code)]),
             Without.CF = Player.CF - Combo.CF,
             Without.CA = Player.CA - Combo.CA,
             Without.CF. = Without.CF/(Without.CF + Without.CA),
             Without.xGF = Player.xGF - Combo.xGF,
             Without.xGA = Player.xGA - Combo.xGA,
             Without.xGF. = Without.xGF/(Without.xGF + Without.xGA)),
    filter(pmatrix, pmatrix$Combo.Code %in% combo.ref$Combo.Code == FALSE) %>% group_by(Player, Without, Season) %>% 
      mutate(Player.TOI. = first(player.ref$TOI.[match(Player, player.ref$Player)]),
             Player.CF = first(player.ref$CF[match(Player, player.ref$Player)]),
             Player.CA = first(player.ref$CA[match(Player, player.ref$Player)]),
             Player.xGF = first(player.ref$xGF[match(Player, player.ref$Player)]),
             Player.xGA = first(player.ref$xGA[match(Player, player.ref$Player)]),
             Combo.CF = 0,
             Combo.CA = 0,
             Combo.xGF = 0,
             Combo.xGA = 0,
             Without.CF = Player.CF - Combo.CF,
             Without.CA = Player.CA - Combo.CA,
             Without.CF. = Without.CF/(Without.CF + Without.CA),
             Without.xGF = Player.xGF - Combo.xGF,
             Without.xGA = Player.xGA - Combo.xGA,
             Without.xGF. = Without.xGF/(Without.xGF + Without.xGA))
  ) %>% data.frame()
  
  # Cool down
  Sys.sleep(60)
  
  # Calculate TOI-weighted average strength of units / Calculer la force moyenne des groupes
  group1 <- rbind_list(
    group_by(pbp.qual, h1.num, a1.num, a2.num, a3.num, a4.num, a5.num, a6.num, Season, Home.Team, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
      rename(Player = h1.num, Team = Home.Team, OP1 = a1.num, OP2 = a2.num, OP3 = a3.num, OP4 = a4.num, OP5 = a5.num, OP6 = a6.num) %>%
      summarise(Venue = "Home", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
    group_by(pbp.qual, h2.num, a1.num, a2.num, a3.num, a4.num, a5.num, a6.num, Season, Home.Team, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
      rename(Player = h2.num, Team = Home.Team, OP1 = a1.num, OP2 = a2.num, OP3 = a3.num, OP4 = a4.num, OP5 = a5.num, OP6 = a6.num) %>%
      summarise(Venue = "Home", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
    group_by(pbp.qual, h3.num, a1.num, a2.num, a3.num, a4.num, a5.num, a6.num, Season, Home.Team, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
      rename(Player = h3.num, Team = Home.Team, OP1 = a1.num, OP2 = a2.num, OP3 = a3.num, OP4 = a4.num, OP5 = a5.num, OP6 = a6.num) %>%
      summarise(Venue = "Home", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
    group_by(pbp.qual, h4.num, a1.num, a2.num, a3.num, a4.num, a5.num, a6.num, Season, Home.Team, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
      rename(Player = h4.num, Team = Home.Team, OP1 = a1.num, OP2 = a2.num, OP3 = a3.num, OP4 = a4.num, OP5 = a5.num, OP6 = a6.num) %>%
      summarise(Venue = "Home", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
    group_by(pbp.qual, h5.num, a1.num, a2.num, a3.num, a4.num, a5.num, a6.num, Season, Home.Team, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
      rename(Player = h5.num, Team = Home.Team, OP1 = a1.num, OP2 = a2.num, OP3 = a3.num, OP4 = a4.num, OP5 = a5.num, OP6 = a6.num) %>%
      summarise(Venue = "Home", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
    group_by(pbp.qual, h6.num, a1.num, a2.num, a3.num, a4.num, a5.num, a6.num, Season, Home.Team, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
      rename(Player = h6.num, Team = Home.Team, OP1 = a1.num, OP2 = a2.num, OP3 = a3.num, OP4 = a4.num, OP5 = a5.num, OP6 = a6.num) %>%
      summarise(Venue = "Home", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
    group_by(pbp.qual, a1.num, h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, Season, Away.Team, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
      rename(Player = a1.num, Team = Away.Team, OP1 = h1.num, OP2 = h2.num, OP3 = h3.num, OP4 = h4.num, OP5 = h5.num, OP6 = h6.num) %>%
      summarise(Venue = "Away", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
    group_by(pbp.qual, a2.num, h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, Season, Away.Team, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
      rename(Player = a2.num, Team = Away.Team, OP1 = h1.num, OP2 = h2.num, OP3 = h3.num, OP4 = h4.num, OP5 = h5.num, OP6 = h6.num) %>%
      summarise(Venue = "Away", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
    group_by(pbp.qual, a3.num, h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, Season, Away.Team, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
      rename(Player = a3.num, Team = Away.Team, OP1 = h1.num, OP2 = h2.num, OP3 = h3.num, OP4 = h4.num, OP5 = h5.num, OP6 = h6.num) %>%
      summarise(Venue = "Away", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
    group_by(pbp.qual, a4.num, h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, Season, Away.Team, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
      rename(Player = a4.num, Team = Away.Team, OP1 = h1.num, OP2 = h2.num, OP3 = h3.num, OP4 = h4.num, OP5 = h5.num, OP6 = h6.num) %>%
      summarise(Venue = "Away", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
    group_by(pbp.qual, a5.num, h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, Season, Away.Team, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
      rename(Player = a5.num, Team = Away.Team, OP1 = h1.num, OP2 = h2.num, OP3 = h3.num, OP4 = h4.num, OP5 = h5.num, OP6 = h6.num) %>%
      summarise(Venue = "Away", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2)),
    group_by(pbp.qual, a6.num, h1.num, h2.num, h3.num, h4.num, h5.num, h6.num, Season, Away.Team, Season.Type, Date, Game.ID, Strength.State, Score.Cat) %>% 
      rename(Player = a6.num, Team = Away.Team, OP1 = h1.num, OP2 = h2.num, OP3 = h3.num, OP4 = h4.num, OP5 = h5.num, OP6 = h6.num) %>%
      summarise(Venue = "Away", TOI = round(sum(as.numeric(as.character(Event.Length)))/60, 2))
  ) %>% data.frame()
  
  group2 <- rbind_list(
    group_by(group1, Player, Team, OP1, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(OP = OP1) %>%
      summarise(TOI = sum(TOI)),
    group_by(group1, Player, Team, OP2, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(OP = OP2) %>%
      summarise(TOI = sum(TOI)),
    group_by(group1, Player, Team, OP3, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(OP = OP3) %>%
      summarise(TOI = sum(TOI)),
    group_by(group1, Player, Team, OP4, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(OP = OP4) %>%
      summarise(TOI = sum(TOI)),
    group_by(group1, Player, Team, OP5, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(OP = OP5) %>%
      summarise(TOI = sum(TOI)),
    group_by(group1, Player, Team, OP6, Season, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(OP = OP6) %>%
      summarise(TOI = sum(TOI))
  ) %>% filter(Player != "GOALIE" & OP != "GOALIE") %>% data.frame()
  
  QoC <- merge(group2, select(player.without, c(Without, Player, Player.TOI., Without.CF., Without.xGF.)), by.x = c("Player", "OP"), by.y = c("Without", "Player")) %>%
    mutate(W.TOI. = TOI*Player.TOI., W.CF. = TOI*Without.CF., W.xGF. = TOI*Without.xGF.) %>%
    group_by(Player, Season, Team, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>%
    summarise(S.TOI = sum(TOI), S.TOI. = sum(W.TOI.), S.CF. = sum(W.CF.), S.xGF. = sum(W.xGF.)) %>%
    data.frame()
  
  QoT <- rbind_list(
    filter(combo, P3 == "X") %>% 
      group_by(P1, P2, Combo.Code, Season, Team, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(Player = P1, TM = P2) %>%
      summarise(TOI = sum(TOI)) %>% data.frame(),
    filter(combo, P3 == "X") %>% 
      group_by(P1, P2, Combo.Code, Season, Team, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>% rename(Player = P2, TM = P1) %>%
      summarise(TOI = sum(TOI)) %>% data.frame()
  ) %>% data.frame() %>%
    merge(select(player.without, c(Without, Player, Player.TOI., Without.CF., Without.xGF.)), by.x = c("Player", "TM"), by.y = c("Without", "Player")) %>%
    mutate(W.TOI. = TOI*Player.TOI., W.CF. = TOI*Without.CF., W.xGF. = TOI*Without.xGF.) %>%
    group_by(Player, Season, Team, Season.Type, Date, Game.ID, Venue, Strength.State, Score.Cat) %>%
    summarise(S.TOI = sum(TOI), S.TOI. = sum(W.TOI.), S.CF. = sum(W.CF.), S.xGF. = sum(W.xGF.)) %>%
    data.frame()
  
  playerseason <- merge(playerseason, 
                        group_by(QoT, Player, Season, Team, Venue, Strength.State, Score.Cat, Season.Type) %>% 
                          summarise(S.TOIT = sum(na.omit(S.TOI)), S.TOI.T = sum(na.omit(S.TOI.)), S.CF.T = sum(na.omit(S.CF.)), S.xGF.T = sum(na.omit(S.xGF.))) %>%
                          select(c(Player, Season, Team, Venue, Strength.State, Score.Cat, Season.Type, S.TOIT, S.TOI.T, S.CF.T, S.xGF.T)) %>% data.frame(),
                        by.x = c("Player", "Season", "Team", "Venue", "Strength.State", "Score.Cat", "Season.Type"), by.y = c("Player", "Season", "Team", "Venue", "Strength.State", "Score.Cat", "Season.Type"),
                        all.x = TRUE) %>%
    data.frame() %>% merge(
      group_by(QoC, Player, Season, Team, Venue, Strength.State, Score.Cat, Season.Type) %>% 
        summarise(S.TOIC = sum(na.omit(S.TOI)), S.TOI.C = sum(na.omit(S.TOI.)), S.CF.C = sum(na.omit(S.CF.)), S.xGF.C = sum(na.omit(S.xGF.))) %>%
        select(c(Player, Season, Team, Venue, Strength.State, Score.Cat, Season.Type, S.TOIC, S.TOI.C, S.CF.C, S.xGF.C)) %>% data.frame(),
      by.x = c("Player", "Season", "Team", "Venue", "Strength.State", "Score.Cat", "Season.Type"), by.y = c("Player", "Season", "Team", "Venue", "Strength.State", "Score.Cat", "Season.Type"),
      all.x = TRUE) %>%
    data.frame()
  
  # Write
  playerseason <- rbind_list(player.s.hist, playerseason) %>% data.frame()
  dbWriteTable(con2, "playerseason", playerseason, overwrite = TRUE)
  
  ## Load DB tables / Charger tables de la base de données
  # Link to database / Connecter a la base de données
  link1 <- "~/corsica.sqlite"
  con1 <- dbConnect(SQLite(), link1)
  
  teamquery <- dbSendQuery(con1, paste("SELECT * FROM team WHERE Season = ", season, sep = ""))
  team <- fetch(teamquery, -1)
  
  goaliequery <- dbSendQuery(con1, paste("SELECT * FROM goalie WHERE Season = ", season, sep = ""))
  goalie <- fetch(goaliequery, -1)
  
  playerquery <- dbSendQuery(con1, paste("SELECT * FROM player WHERE Season = ", season, sep = ""))
  player <- fetch(playerquery, -1)
  
  dbDisconnect(con1)
  
  ## Load current files / Charger fichiers 
  # Link to database / Connecter a la base de données
  link2 <- "~/fenwicka.sqlite"
  con2 <- dbConnect(SQLite(), link2)
  
  teamgame <- try(dbReadTable(con2, "teamgame"))
  goaliegame <- try(dbReadTable(con2, "goaliegame"))
  playergame <- try(dbReadTable(con2, "playergame"))
  combogame <- try(dbReadTable(con2, "combogame"))
  
  # Remove current season / Éliminer saison actuelle
  if(class(teamgame) != "try-error") {team.g.hist <- filter(teamgame, Season != season) %>% data.frame()} else {team.g.hist <- NULL}
  if(class(goaliegame) != "try-error") {goalie.g.hist <- filter(goaliegame, Season != season) %>% data.frame()} else {goalie.g.hist <- NULL}
  if(class(playergame) != "try-error") {player.g.hist <- filter(playergame, Season != season) %>% data.frame()} else {player.g.hist <- NULL}
  if(class(combogame) != "try-error") {combo.g.hist <- filter(combogame, Season != season) %>% data.frame()} else {combo.g.hist <- NULL}
  
  ## Aggregate stats by game / Agréger les statistiques par match
  
  # Team / Équipes
  team$Strength.State[which(team$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "0v0") == FALSE)] <- "XvX" #EvE?
  
  # Group score states / Combiner états de score
  team$Score.Cat[which(team$Score.Cat < 0)] <- -1
  team$Score.Cat[which(team$Score.Cat > 0)] <- 1
  
  teamgame <- filter(team, Strength.State != "0v0") %>% group_by(Team, Season, Venue, Date, Game.ID, Strength.State, Score.Cat, Season.Type) %>%
    summarise(TOI = sum(TOI), CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA),
              xGF = sum(xGF), xGA = sum(xGA), ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA),
              AGF = sum(AGF), AGA = sum(AGA), AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
              MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
              SCF = sum(SCF), SCA = sum(SCA), ASCF = sum(ASCF), ASCA = sum(ASCA), MSCF = sum(MSCF), MSCA = sum(MSCA),
              OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
              DZF = sum(DZF), NZF = sum(NZF), FOW = sum(FOW), FOL = sum(FOL), GVA = sum(GVA), TKA = sum(TKA), 
              PENT = sum(PENT), PEND = sum(PEND), RBF = sum(RBF), RBA = sum(RBA), RSF = sum(RSF), RSA = sum(RSA),
              POSTF = sum(POSTF), POSTA = sum(POSTA)) %>% data.frame()
  
  # Write
  teamgame <- rbind_list(team.g.hist, teamgame) %>% data.frame()
  dbWriteTable(con2, "teamgame", teamgame, overwrite = TRUE)
  
  # Goalies / Gardiens
  goalie$Strength.State[which(goalie$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "0v0") == FALSE)] <- "XvX" #EvE?
  
  # Group score states / Combiner états de score
  goalie$Score.Cat[which(goalie$Score.Cat < 0)] <- -1
  goalie$Score.Cat[which(goalie$Score.Cat > 0)] <- 1
  
  goaliegame <- filter(goalie, Strength.State != "0v0") %>% group_by(Player, Season, Venue, Date, Game.ID, Strength.State, Score.Cat, Season.Type) %>%
    summarise(Team = first(Team), TOI = sum(TOI), CA = sum(CA), FA = sum(FA), SA = sum(SA), GA = sum(GA),
              xGA = sum(xGA), 
              HDSA = sum(HDSA), MDSA = sum(MDSA), LDSA = sum(LDSA),
              HDGA = sum(HDGA), MDGA = sum(MDGA), LDGA = sum(LDGA), POSTA = sum(POSTA),
              OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF),
              DZF = sum(DZF), NZF = sum(NZF), PENT = sum(PENT), PEND = sum(PEND), RBA = sum(RBA), RSA = sum(RSA)) %>% data.frame()
  
  # Write
  goaliegame <- rbind_list(goalie.g.hist, goaliegame) %>% data.frame()
  dbWriteTable(con2, "goaliegame", goaliegame, overwrite = TRUE)
  
  # Players / Joueurs
  player$Strength.State[which(player$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "0v0") == FALSE)] <- "XvX"
  
  # Group score states / Combiner états de score
  player$Score.Cat[which(player$Score.Cat < 0)] <- -1
  player$Score.Cat[which(player$Score.Cat > 0)] <- 1
  
  playergame <- filter(player, Strength.State != "0v0") %>% group_by(Player, Season, Date, Game.ID, Venue, Strength.State, Score.Cat, Season.Type) %>%
    summarise(Position = roster$Position[match(first(Player), roster$Full.Name)],
              Team = first(Team), TOI = sum(TOI), CF = sum(CF), CA = sum(CA), iCF = sum(iCF), FF = sum(FF), FA = sum(FA), iFF = sum(iFF), 
              SF = sum(SF), SA = sum(SA), iSF = sum(iSF), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA), ixG = sum(ixG), 
              ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
              AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
              MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
              SCF = sum(SCF), SCA = sum(SCA), ASCF = sum(ASCF), ASCA = sum(ASCA), MSCF = sum(MSCF), MSCA = sum(MSCA),
              OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OTF = sum(OTF), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF), 
              iFOW = sum(iFOW), iFOL = sum(iFOL), iHF = sum(iHF), iHA = sum(iHA), 
              iGVA = sum(iGVA), iTKA = sum(iTKA), iBLK = sum(iBLK), iSCF = sum(iSCF),
              iPOSTF = sum(iPOSTF), POSTF = sum(POSTF), POSTA = sum(POSTA),
              G = sum(G), A1 = sum(na.omit(A1)), A2 = sum(na.omit(A2)), iPENT = sum(iPENT), iPEND = sum(na.omit(iPEND)),
              tTOI = sum(tTOI), tCF = sum(tCF), tCA = sum(tCA), tFF = sum(tFF), tFA = sum(tFA), tSF = sum(tSF), tSA = sum(tSA), 
              tGF = sum(tGF), tGA = sum(tGA), txGF = sum(txGF), txGA = sum(txGA), tACF = sum(tACF), tACA = sum(tACA), tAFF = sum(tAFF), tAFA = sum(tAFA), 
              tASF = sum(tASF), tASA = sum(tASA), tAGF = sum(tAGF), tAGA = sum(tAGA), tAxGF = sum(tAxGF), tAxGA = sum(tAxGA), tMCF = sum(tMCF), tMCA = sum(tMCA), 
              tMFF = sum(tMFF), tMFA = sum(tMFA), tMSF = sum(tMSF), tMSA = sum(tMSA), tMGF = sum(tMGF), tMGA = sum(tMGA), tMxGF = sum(tMxGF), tMxGA = sum(tMxGA),
              tSCF = sum(tSCF), tSCA = sum(tSCA), tMSCF = sum(tMSCF), tMSCA = sum(tMSCA), tASCF = sum(tASCF), tASCA = sum(tASCA),
              iRB = sum(iRB), iRS = sum(iRS)) %>% data.frame() %>%
    mutate(OCF = tCF - CF, OCA = tCA - CA,
           OFF = tFF - FF, OFA = tFA - FA,
           OSF = tSF - SF, OSA = tSA - SA,
           OGF = tGF - GF, OGA = tGA - GA,
           OxGF = txGF - xGF, OxGA = txGA - xGA, 
           OACF = tACF - ACF, OACA = tACA - ACA,
           OAFF = tAFF - AFF, OAFA = tAFA - AFA,
           OASF = tASF - ASF, OASA = tASA - ASA,
           OAGF = tAGF - AGF, OAGA = tAGA - AGA,
           OAxGF = tAxGF - AxGF, OAxGA = tAxGA - AxGA, 
           OMCF = tMCF - MCF, OMCA = tMCA - MCA,
           OMFF = tMFF - MFF, OMFA = tMFA - MFA, 
           OMSF = tMSF - MSF, OMSA = tMSA - MSA,
           OMGF = tMGF - MGF, OMGA = tMGA - MGA,
           OMxGF = tMxGF - MxGF, OMxGA = tMxGA - MxGA,
           OSCF = tSCF - SCF, OSCA = tSCA - SCA,
           OMSCF = tMSCF - MSCF, OMSCA = tMSCA - MSCA,
           OASCF = tASCF - ASCF, OASCA = tASCA - ASCA
    ) %>% data.frame() %>%
    select(-c(tCF:tASCA)) %>%
    data.frame()
  
  # Write
  playergame <- rbind_list(player.g.hist, playergame) %>% data.frame()
  dbWriteTable(con2, "playergame", playergame, overwrite = TRUE)
  
  # Combos / Combinaisons
  combo$Strength.State[which(combo$Strength.State %in% c("5v5", "5v4", "4v5", "4v4", "5v3", "3v5", "3v3", "0v0") == FALSE)] <- "XvX"
  
  combogame <- filter(combo, as.character(P3) == "X" & Strength.State != "0v0") %>% 
    group_by(Combo.Code, Season, Date, Game.ID, Strength.State, Venue) %>%
    summarise(Team = first(Team), TOI = sum(TOI), 
              P1 = first(P1), P2 = first(P2),
              CF = sum(CF), CA = sum(CA), FF = sum(FF), FA = sum(FA), 
              SF = sum(SF), SA = sum(SA), GF = sum(GF), GA = sum(GA), xGF = sum(xGF), xGA = sum(xGA),
              ACF = sum(ACF), ACA = sum(ACA), AFF = sum(AFF), AFA = sum(AFA), ASF = sum(ASF), ASA = sum(ASA), AGF = sum(AGF), AGA = sum(AGA), 
              AxGF = sum(AxGF), AxGA = sum(AxGA), MCF = sum(MCF), MCA = sum(MCA), MFF = sum(MFF), MFA = sum(MFA),
              MSF = sum(MSF), MSA = sum(MSA), MGF = sum(MGF), MGA = sum(MGA), MxGF = sum(MxGF), MxGA = sum(MxGA), 
              SCF = sum(SCF), SCA = sum(SCA), ASCF = sum(ASCF), ASCA = sum(ASCA), MSCF = sum(MSCF), MSCA = sum(MSCA),
              OZS = sum(OZS), DZS = sum(DZS), NZS = sum(NZS), OZF = sum(OZF), DZF = sum(DZF), NZF = sum(NZF),
              P1.G = sum(P1.G), P1.A1 = sum(na.omit(P1.A1)), P1.A2 = sum(na.omit(P1.A2)),
              P2.G = sum(P2.G), P2.A1 = sum(na.omit(P2.A1)), P2.A2 = sum(na.omit(P2.A2)),
              tTOI = sum(tTOI), tCF = sum(tCF), tCA = sum(tCA), tFF = sum(tFF), tFA = sum(tFA), tSF = sum(tSF), tSA = sum(tSA), 
              tGF = sum(tGF), tGA = sum(tGA), txGF = sum(txGF), txGA = sum(txGA), tACF = sum(tACF), tACA = sum(tACA), tAFF = sum(tAFF), tAFA = sum(tAFA), 
              tASF = sum(tASF), tASA = sum(tASA), tAGF = sum(tAGF), tAGA = sum(tAGA), tAxGF = sum(tAxGF), tAxGA = sum(tAxGA), tMCF = sum(tMCF), tMCA = sum(tMCA), 
              tMFF = sum(tMFF), tMFA = sum(tMFA), tMSF = sum(tMSF), tMSA = sum(tMSA), tMGF = sum(tMGF), tMGA = sum(tMGA), tMxGF = sum(tMxGF), tMxGA = sum(tMxGA),
              tSCF = sum(tSCF), tSCA = sum(tSCA), tMSCF = sum(tMSCF), tMSCA = sum(tMSCA), tASCF = sum(tASCF), tASCA = sum(tASCA)
    ) %>% data.frame() %>%
    mutate(OCF = tCF - CF, OCA = tCA - CA,
           OFF = tFF - FF, OFA = tFA - FA,
           OSF = tSF - SF, OSA = tSA - SA,
           OGF = tGF - GF, OGA = tGA - GA,
           OxGF = txGF - xGF, OxGA = txGA - xGA, 
           OACF = tACF - ACF, OACA = tACA - ACA,
           OAFF = tAFF - AFF, OAFA = tAFA - AFA,
           OASF = tASF - ASF, OASA = tASA - ASA,
           OAGF = tAGF - AGF, OAGA = tAGA - AGA,
           OAxGF = tAxGF - AxGF, OAxGA = tAxGA - AxGA, 
           OMCF = tMCF - MCF, OMCA = tMCA - MCA,
           OMFF = tMFF - MFF, OMFA = tMFA - MFA, 
           OMSF = tMSF - MSF, OMSA = tMSA - MSA,
           OMGF = tMGF - MGF, OMGA = tMGA - MGA,
           OMxGF = tMxGF - MxGF, OMxGA = tMxGA - MxGA,
           OSCF = tSCF - SCF, OSCA = tSCA - SCA,
           OMSCF = tMSCF - MSCF, OMSCA = tMSCA - MSCA,
           OASCF = tASCF - ASCF, OASCA = tASCA - ASCA
    ) %>%
    select(-c(tCF:tASCA)) %>% data.frame()
  
  # Write
  combogame <- rbind_list(combo.g.hist, combogame) %>% data.frame()
  dbWriteTable(con2, "combogame", combogame, overwrite = TRUE)
  
  # Schedule
  start_date <- paste(substr(season, 0, 4), "10", "01", sep = "-")
  end_date <- paste(substr(season, 5, 8), "07", "15", sep = "-")
  
  sched <- try(schedule(start_date, end_date))
  
  if(class(sched) == "try-error") {Sys.sleep(120); sched <- try(schedule(start_date, as.Date(end_date)-1))}
  if(class(sched) == "try-error") {Sys.sleep(120); sched <- try(schedule(start_date, as.Date(end_date)-2))}
  if(class(sched) == "try-error") {Sys.sleep(120); sched <- try(schedule(start_date, as.Date(end_date)-3))}
  if(class(sched) == "try-error") {Sys.sleep(120); sched <- try(schedule(start_date, as.Date(end_date)-4))}
  
  dt.mat <- unlist(strsplit(as.character(sched$Date.Time), split = "T")) %>% matrix(ncol = 2, byrow = TRUE)
  sched$Date <- as.Date(dt.mat[, 1])
  sched$Time <- dt.mat[, 2]
  
  registerDoMC(2)
  
  newcols <- foreach(i = 1:length(sched$Home.Team)) %dopar% {
    
    row = sched[i, ]
    
    Home.Last <- max(as.Date(sched$Date[which({sched$Home.Team == row$Home.Team | sched$Away.Team == row$Home.Team} & as.Date(sched$Date) < as.Date(row$Date))]))
    Away.Last <- max(as.Date(sched$Date[which({sched$Home.Team == row$Away.Team | sched$Away.Team == row$Away.Team} & as.Date(sched$Date) < as.Date(row$Date))]))
    
    add <- cbind(Home.Last, Away.Last) %>% data.frame()
    
    add
    
  }
  
  ncols <- unlist(newcols) %>% matrix(ncol = 2, byrow = TRUE) %>% data.frame()
  
  newsched <- cbind(sched, ncols)
  newsched$Home.Last <- as.Date(newsched$X1, origin = "1970-01-01"); newsched$Away.Last <- as.Date(newsched$X2, origin = "1970-01-01")
  newsched$Home.Rest <- as.Date(newsched$Date) - as.Date(newsched$Home.Last); newsched$Away.Rest <- as.Date(newsched$Date) - as.Date(newsched$Away.Last)
  
  # Standings
  standings <- {
    rbind_list(
      newsched %>%
        rename(Team = Home.Team, Opponent = Away.Team) %>%
        mutate(Venue = "Home") %>%
        select(Team, Season, Session, Date, Venue, Status, Periods, Home.Score, Away.Score, Opponent) %>%
        data.frame(),
      
      newsched %>%
        rename(Team = Away.Team, Opponent = Home.Team) %>%
        mutate(Venue = "Away") %>%
        select(Team, Season, Session, Date, Venue, Status, Periods, Home.Score, Away.Score, Opponent) %>%
        data.frame()
    ) %>%
      data.frame() %>%
      group_by(Team, Season, Session) %>%
      arrange(as.Date(Date)) %>%
      mutate(
        HROW = cumsum(Venue == "Home" & as.numeric(as.character(Home.Score)) > as.numeric(as.character(Away.Score)) & Status == "Final" & {Session == "P" | as.numeric(as.character(Periods)) < 5}),
        AROW = cumsum(Venue == "Away" & as.numeric(as.character(Home.Score)) < as.numeric(as.character(Away.Score)) & Status == "Final" & {Session == "P" | as.numeric(as.character(Periods)) < 5}),
        HSOW = cumsum(Venue == "Home" & as.numeric(as.character(Home.Score)) > as.numeric(as.character(Away.Score)) & Status == "Final" & Session != "P" & as.numeric(as.character(Periods)) > 4),
        ASOW = cumsum(Venue == "Away" & as.numeric(as.character(Home.Score)) < as.numeric(as.character(Away.Score)) & Status == "Final" & Session != "P" & as.numeric(as.character(Periods)) > 4),
        HL = cumsum(Venue == "Home" & as.numeric(as.character(Home.Score)) < as.numeric(as.character(Away.Score)) & Status == "Final" & as.numeric(as.character(Periods)) == 3),
        AL = cumsum(Venue == "Away" & as.numeric(as.character(Home.Score)) > as.numeric(as.character(Away.Score)) & Status == "Final" & as.numeric(as.character(Periods)) == 3),
        HOTL = cumsum(Venue == "Home" & as.numeric(as.character(Home.Score)) < as.numeric(as.character(Away.Score)) & Status == "Final" & as.numeric(as.character(Periods)) > 3),
        AOTL = cumsum(Venue == "Away" & as.numeric(as.character(Home.Score)) > as.numeric(as.character(Away.Score)) & Status == "Final" & as.numeric(as.character(Periods)) > 3),
        HGP = cumsum(Venue == "Home" & Status == "Final"),
        AGP = cumsum(Venue == "Away" & Status == "Final"),
        ROW = HROW + AROW,
        SOW = HSOW + ASOW,
        W = ROW + SOW,
        L = HL + AL,
        OTL = HOTL + AOTL,
        Points = 2*W + OTL
      ) %>%
      data.frame()
  }
  
  # Write
  newsched$Date <- as.character(newsched$Date) # For DB schema
  newsched$Home.Last <- as.character(newsched$Home.Last) # For DB schema
  newsched$Away.Last <- as.character(newsched$Away.Last) # For DB schema
  standings$Date <- as.character(standings$Date) # For DB schema
  newsched <- rbind_list(newsched, schedule.s.hist) %>% data.frame() %>% arrange(desc(as.Date(Date)))
  standings <- rbind_list(standings, standings.s.hist) %>% data.frame() %>% arrange(as.Date(Date))
  dbWriteTable(con2, "schedule", newsched, overwrite = TRUE)
  dbWriteTable(con2, "standings", standings, overwrite = TRUE)
  
  dbDisconnect(con1)
  dbDisconnect(con2)
  
}

csv <- function(key, folder = paste("~/Desktop/Scrape_", Sys.Date(), sep = "")) {
  
  dir.create(folder)
  
  if(exists("pbp.full")) {
    write.csv(pbp.full, file = paste(folder, "/pbp.csv", sep = ""))
    cat(paste("Saved PBP to", folder, "\n"))
  } else {cat("Oops! Could not find object pbp.full!\n")}
  
  if(exists("roster.full")) {
    write.csv(roster.full, file = paste(folder, "/roster.csv", sep = ""))
    cat(paste("Saved Roster to", folder, "\n"))
  } else {cat("Oops! Could not find object roster.full!\n")}
  
  if(exists("team.bygame")) {
    write.csv(team.bygame, file = paste(folder, "/team_bygame.csv", sep = ""))
    cat(paste("Saved team.bygame to", folder, "\n"))
  } else {cat("Oops! Could not find object team.bygame!\n")}
  
  if(exists("goalie.bygame")) {
    write.csv(goalie.bygame, file = paste(folder, "/goalie_bygame.csv", sep = ""))
    cat(paste("Saved goalie.bygame to", folder, "\n"))
  } else {cat("Oops! Could not find object goalie.bygame!\n")}
  
  if(exists("skater.bygame")) {
    write.csv(skater.bygame, file = paste(folder, "/skater_bygame.csv", sep = ""))
    cat(paste("Saved skater.bygame to", folder, "\n"))
  } else {cat("Oops! Could not find object skater.bygame!\n")}
  
  if(exists("combo.bygame")) {
    write.csv(combo.bygame, file = paste(folder, "/combo_bygame.csv", sep = ""))
    cat(paste("Saved combo.bygame to", folder, "\n"))
  } else {cat("Oops! Could not find object combo.bygame!\n")}
  
  if(exists("assists")) {
    write.csv(assists, file = paste(folder, "/assists.csv", sep = ""))
    cat(paste("Saved assists to", folder, "\n"))
  } else {cat("Oops! Could not find object assists!\n")}
  
}

read <- function(db, table, directory = "~/") {
  
  paste(directory, 
        tolower(db), 
        ".sqlite",
        sep = ""
        ) -> 
  path
  
  if(tolower(db) == "corsica") {
    
    paste("The table: \"",
          table,
          "\" does not exist.\n",
          "Select one of: pbp, roster, team, goalie, player, combo, assist or bio.",
          sep = ""
          ) ->
    msg
    
  } else if(tolower(db) == "fenwicka") {
    
    paste("The table: \"",
          table,
          "\" does not exist.\n",
          "Select one of: teamseason, teamgame, goalieseason, goaliegame, playerseason, playergame, comboseason, combogame, lineseason, pairseason, assistseason, coords, highlights, schedule, standings.",
          sep = ""
          ) ->
    msg
    
  } else {
    
    paste(db,
          " is not a valid database name.\n",
          "Select one of: corsica, fenwicka.",
          sep = ""
          ) ->
    msg
    
  }
  
  con <- dbConnect(SQLite(), path)
  
  check <- try({dbReadTable(con, tolower(table)) %!% tolower(table)})
  
  if(class(check) == "try-error") {
    
    cat(msg)
    
  }
  
}

#################

### TESTING ###



###############

### DESCRIPTIONS ###



####################

### NOTES ###

# Translate whole scrape-push flow into functions (scrape %>% compile %>% push pipeline)
# Convert to CSV functions (or general save options)
# Complete set of absolute functions
# Y2Y correlation function
# Split-half correlation function
# Custom ggplotly helper functions
# Info and describe functions (function %>% describe or info() to directory)
# _ondate functions (return stats on a given date)
# dbReadTable helpers
# Player/team search (return summary of grep matches)
# Compile to.db argument (default TRUE, but FALSE allows pipe to csv())

#############