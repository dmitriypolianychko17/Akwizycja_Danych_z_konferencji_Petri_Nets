#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(pdftools)
library(stringr)
library(solrium)
library(xml2)
library(tidyRSS)
library(tm)
library(httr)
library(XML)
library(wordcloud2)
library(stringi)
library(tm)
library(memoise)
library(SnowballC)

minArtykul <- 4;
maxArtykul <- 26;
aktualnyArtykul <- 4;

ui <- dashboardPage(skin = "purple",
                    
    dashboardHeader(title="Aplikacja do akwizycji i analizy danych z artykułów z konferencji Petri Nets and Software Engineering 2016", titleWidth=1000),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("O aplikacji", tabName = "about", icon = icon("clipboard")),
            menuItem("Pobranie artykulow", tabName = "artykuly", icon = icon("newspaper")),
            menuItem("Baza Solr", tabName = "data", icon = icon("database"), badgeLabel = "nowa", badgeColor = "green"),
            menuItem("Chmura i klastry", tabName = "chmura", icon = icon("chart-pie")),
            menuItem("Link do kodu aplikacji", href = "https://github.com/dmitriypolianychko17/Akwizycja_Danych_z_konrerencji_Petri_Nets", icon = icon("code"))
        )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "about", h4("Projekt wykonany przez Dmytro Polianychko Informatyka III rok 29.12.2020")), 
          
        tabItem(tabName = "artykuly",
          fluidRow(box(title = "Parametry artykulow",
                       actionButton("losowySolr", "Pobierz losowy artykul"),
                       actionButton("poprzedniSolr", "Poprzedni Artykul"),
                       actionButton("nastepnySolr", "Nastepny Artykul"),
                       sliderInput("sliderSolr", "Artykul:", min = minArtykul, max = maxArtykul, value = aktualnyArtykul),
                       textInput("wpiszArtykul","Wpisz numer artykulu: "),
                       actionButton("wczytajSolr", "Wczytaj artykul"),
                       actionButton("dodajSolr", "Dodaj do bazy"), width = 6, solidHeader = T, status = "success", collapsible = T)),
          fluidRow(box(title = "Wybrane artykuly", dataTableOutput("tableSolr"), width = 12, solidHeader = T, status = "warning"))),
        tabItem(tabName = "data",
                fluidRow(box(title = "Operacje na bazie danych",
                             actionButton("odczytBazy", "Odczytaj dane z bazy"),
                             actionButton("usuwanieBazy", "Usun wszystkie artykuly z bazy"),
                             actionButton("usuwanieTabeli", "Usun dane z tabeli"), width = 5, solidHeader = T, status = "success", collapsible = T)),
                fluidRow(box(title = "Artykuly z bazy", dataTableOutput("solr"), width = 12, solidHeader = T, status = "warning"))),
        tabItem(tabName = "chmura",
                fluidRow(box(title = "Chmura slow",
                             actionButton("add_chmura", "Stworz chmure"),
                             selectInput("ksztaltSelect", label = h4("Wybierz ksztalt chmury"),
                                         choices = list("Star","Diamond","Pentagon","Cardioid","Triangle","Triangle-forward"),
                                         selected = 1),
                             selectInput("kolorSelect", label = h4("Kolor chmury"),
                                         choices = list("Random-light","Random-dark","White","Blue","Red","Green","Yellow","Aqua","Lime"),
                                         selected = 1),
                             numericInput("wielkoscSelect", label = h4("Wielkosc chmury"),
                                          value = 0.5,
                                          step = 0.2), width = 4, solidHeader = T, status = "success")),
                fluidRow(box(title = "Chmura slow", wordcloud2Output("wordcloud2", height = 600), width = 12, status = "warning", solidHeader = T, collapsible = T)),
                fluidRow(box(title =" Najczestsze slowa", plotOutput("slowa", height = 600),width = 12, status = "warning", solidHeader = T, collapsible = T)),
                fluidRow(box(title = "K-means", plotOutput("kmeans", height = 600), width = 12, status = "warning", solidHeader = T, collapsible = T)))
      )
    )
)



  


server <- function(input, output, session) {
  
#Wybranie losowego artykulu
  observeEvent(input$losowySolr,
               {
                 aktualnyArtykul <<- sample(minArtykul:maxArtykul, 1)
                 updateSliderInput(session, "sliderSolr", value = aktualnyArtykul)
                 #======POBRANIE ZE STRONY
                 output$wynikWczytywania<-renderText(isolate("Poprawnie wczytano artykul"));
                 strona<-read_html(x="http://ceur-ws.org/Vol-1591/", encoding = "UTF-8")
                 #Tytuly
                 tytuly<-xml_find_all(x=strona, xpath = "//span[@class='CEURTITLE']")
                 teksty<-xml_text(tytuly)
                 #Autorzy
                 autorzy<-xml_find_all(x=strona, xpath = "//span[@class='CEURAUTHOR']")
                 autor<-xml_text(autorzy)
                 numery <- c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26);
                 nazwa = "http://ceur-ws.org/Vol-1591/paper";
                 reszta = ".pdf"
                 pobierz <- (paste(nazwa,numery[aktualnyArtykul],reszta,sep = ""))
                 tekst<-pdf_text(pdf=pobierz);
                 tekst2<-str_replace_all(string=tekst, pattern="[\r\n]",replacement = " "); # jeden ze znakow jak wystapi to zastepujemy
                 tekst3<-str_squish(string=tekst2); #usuwa nadmiarowe spacje itp
                 write.table(tekst3, file = "dane.txt", sep = "\t", row.names = FALSE)
                 my_data <- read.delim("dane.txt")
                 dokumenty<-data.frame(matrix(ncol=4, nrow=1));
                 colnames(dokumenty)[1]<-"id";
                 colnames(dokumenty)[2]<-"tytul";
                 colnames(dokumenty)[3]<-"autor";
                 colnames(dokumenty)[4]<-"kontent";
                 dokumenty$id<-c(aktualnyArtykul);
                 dokumenty$tytul<-c(teksty[aktualnyArtykul]);
                 dokumenty$autor<-c(autorzy[aktualnyArtykul]);
                 dokumenty$kontent<-c(my_data);
                 output$tableSolr <- renderDataTable(dokumenty);
               });
  
  # handler dla slajdera
  observeEvent(input$sliderSolr,
               {
                 aktualnyArtykul <<- as.numeric(input$sliderSolr)
                 strona<-read_html(x="http://ceur-ws.org/Vol-1591/", encoding = "UTF-8")
                 #Tytuly
                 tytuly<-xml_find_all(x=strona, xpath = "//span[@class='CEURTITLE']")
                 teksty<-xml_text(tytuly)
                 #Autorzy
                 autorzy<-xml_find_all(x=strona, xpath = "//span[@class='CEURAUTHOR']")
                 autor<-xml_text(autorzy)
                 numery <- c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26);
                 nazwa = "http://ceur-ws.org/Vol-1591/paper";
                 reszta = ".pdf"
                 pobierz <- (paste(nazwa,numery[aktualnyArtykul],reszta,sep = ""))
                 tekst<-pdf_text(pdf=pobierz);
                 tekst2<-str_replace_all(string=tekst, pattern="[\r\n]",replacement = " "); # jeden ze znakow jak wystapi to zastepujemy
                 tekst3<-str_squish(string=tekst2); #usuwa nadmiarowe spacje itp
                 write.table(tekst3, file = "dane.txt", sep = "\t", row.names = FALSE)
                 my_data <- read.delim("dane.txt")
                 dokumenty<-data.frame(matrix(ncol=4, nrow=1));
                 colnames(dokumenty)[1]<-"id";
                 colnames(dokumenty)[2]<-"tytul";
                 colnames(dokumenty)[3]<-"autor";
                 colnames(dokumenty)[4]<-"kontent";
                 dokumenty$id<-c(aktualnyArtykul);
                 dokumenty$tytul<-c(teksty[aktualnyArtykul]);
                 dokumenty$autor<-c(autor[aktualnyArtykul]);
                 dokumenty$kontent<-c(my_data);
                 output$tableSolr <- renderDataTable(dokumenty);
               });

  observeEvent(input$wczytajSolr,
               {
                 wybranyNumer = as.numeric(input$wpiszArtykul)
                 if(wybranyNumer < minArtykul || wybranyNumer > maxArtykul){
                   tresc <- paste("Wybrany artykul nie istneje. Wybierz jeden z przedzialu od: ", minArtykul, " do: ", maxArtykul, ".", sep="");
                 } else {
                   aktualnyArtykul <<- wybranyNumer
                   updateSliderInput(session, "sliderSolr", value = aktualnyArtykul)
                   strona<-read_html(x="http://ceur-ws.org/Vol-1591/", encoding = "UTF-8")
                   #Tytuly
                   tytuly<-xml_find_all(x=strona, xpath = "//span[@class='CEURTITLE']")
                   teksty<-xml_text(tytuly)
                   #Autorzy
                   autorzy<-xml_find_all(x=strona, xpath = "//span[@class='CEURAUTHOR']")
                   autor<-xml_text(autorzy)
                   numery <- c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26);
                   nazwa = "http://ceur-ws.org/Vol-1591/paper";
                   reszta = ".pdf"
                   pobierz <- (paste(nazwa,numery[aktualnyArtykul],reszta,sep = ""))
                   tekst<-pdf_text(pdf=pobierz);
                   tekst2<-str_replace_all(string=tekst, pattern="[\r\n]",replacement = " "); # jeden ze znakow jak wystapi to zastepujemy
                   tekst3<-str_squish(string=tekst2); #usuwa nadmiarowe spacje itp
                   write.table(tekst3, file = "dane.txt", sep = "\t", row.names = FALSE)
                   my_data <- read.delim("dane.txt")
                   dokumenty<-data.frame(matrix(ncol=4, nrow=1));
                   colnames(dokumenty)[1]<-"id";
                   colnames(dokumenty)[2]<-"tytul";
                   colnames(dokumenty)[3]<-"autor";
                   colnames(dokumenty)[4]<-"kontent";
                   dokumenty$id<-c(aktualnyArtykul);
                   dokumenty$tytul<-c(teksty[aktualnyArtykul]);
                   dokumenty$autor<-c(autor[aktualnyArtykul]);
                   dokumenty$kontent<-c(my_data);
                   output$tableSolr <- renderDataTable(dokumenty);
                 }
               });
  
  #przelaczenie do nastepnego artykulu
  observeEvent(input$nastepnySolr,
               {
                 aktualnyArtykul <<- aktualnyArtykul + 1;
                 if(aktualnyArtykul > maxArtykul){
                   aktualnyArtykul <<- minArtykul;
                 }
                 updateSliderInput(session, "sliderSolr", value = aktualnyArtykul)
                 strona<-read_html(x="http://ceur-ws.org/Vol-1591/", encoding = "UTF-8")
                 #Tytuly
                 tytuly<-xml_find_all(x=strona, xpath = "//span[@class='CEURTITLE']")
                 teksty<-xml_text(tytuly)
                 #Autorzy
                 autorzy<-xml_find_all(x=strona, xpath = "//span[@class='CEURAUTHOR']")
                 autor<-xml_text(autorzy)
                 numery <- c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26);
                 nazwa = "http://ceur-ws.org/Vol-1591/paper";
                 reszta = ".pdf"
                 pobierz <- (paste(nazwa,numery[aktualnyArtykul],reszta,sep = ""))
                 tekst<-pdf_text(pdf=pobierz);
                 tekst2<-str_replace_all(string=tekst, pattern="[\r\n]",replacement = " "); # jeden ze znakow jak wystapi to zastepujemy
                 tekst3<-str_squish(string=tekst2); #usuwa nadmiarowe spacje itp
                 write.table(tekst3, file = "dane.txt", sep = "\t", row.names = FALSE)
                 my_data <- read.delim("dane.txt")
                 dokumenty<-data.frame(matrix(ncol=4, nrow=1));
                 colnames(dokumenty)[1]<-"id";
                 colnames(dokumenty)[2]<-"tytul";
                 colnames(dokumenty)[3]<-"autor";
                 colnames(dokumenty)[4]<-"kontent";
                 dokumenty$id<-c(aktualnyArtykul);
                 dokumenty$tytul<-c(teksty[aktualnyArtykul]);
                 dokumenty$autor<-c(autor[aktualnyArtykul]);
                 dokumenty$kontent<-c(my_data);
                 output$tableSolr <- renderDataTable(dokumenty);
                 
               });
  
  #przelaczenie do poprzedniego artykulu
  observeEvent(input$poprzedniSolr,
               {
                 aktualnyArtykul <<- aktualnyArtykul - 1;
                 if(aktualnyArtykul > maxArtykul){
                   aktualnyArtykul <<- minArtykul;
                 }
                 updateSliderInput(session, "sliderSolr", value = aktualnyArtykul)
                 strona<-read_html(x="http://ceur-ws.org/Vol-1591/", encoding = "UTF-8")
                 #Tytuly
                 tytuly<-xml_find_all(x=strona, xpath = "//span[@class='CEURTITLE']")
                 teksty<-xml_text(tytuly)
                 #Autorzy
                 autorzy<-xml_find_all(x=strona, xpath = "//span[@class='CEURAUTHOR']")
                 autor<-xml_text(autorzy)
                 numery <- c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26);
                 nazwa = "http://ceur-ws.org/Vol-1591/paper";
                 reszta = ".pdf"
                 pobierz <- (paste(nazwa,numery[aktualnyArtykul],reszta,sep = ""))
                 tekst<-pdf_text(pdf=pobierz);
                 tekst2<-str_replace_all(string=tekst, pattern="[\r\n]",replacement = " "); # jeden ze znakow jak wystapi to zastepujemy
                 tekst3<-str_squish(string=tekst2); #usuwa nadmiarowe spacje itp
                 write.table(tekst3, file = "dane.txt", sep = "\t", row.names = FALSE)
                 my_data <- read.delim("dane.txt")
                 dokumenty<-data.frame(matrix(ncol=4, nrow=1));
                 colnames(dokumenty)[1]<-"id";
                 colnames(dokumenty)[2]<-"tytul";
                 colnames(dokumenty)[3]<-"autor";
                 colnames(dokumenty)[4]<-"kontent";
                 dokumenty$id<-c(aktualnyArtykul);
                 dokumenty$tytul<-c(teksty[aktualnyArtykul]);
                 dokumenty$autor<-c(autor[aktualnyArtykul]);
                 dokumenty$kontent<-c(my_data);
                 output$tableSolr <- renderDataTable(dokumenty);
               });
  
  #dodanie wybranego artykulu do Apache Solr
  observeEvent(input$dodajSolr,
               {
                 wybranyNumer = as.numeric(input$wpiszArtykul)
                 if(wybranyNumer < minArtykul || wybranyNumer > maxArtykul ){
                   tresc <- paste("Wybrany artykul nie istneje. Wybierz jeden z przedzialu od: ", minArtykul, " do: ", maxArtykul, ".", sep="");
                 } else {
                   aktualnyArtykul <<- wybranyNumer
                   updateSliderInput(session, "sliderSolr", value = aktualnyArtykul)
                   strona<-read_html(x="http://ceur-ws.org/Vol-1591/", encoding = "UTF-8")
                   #Tytuly
                   tytuly<-xml_find_all(x=strona, xpath = "//span[@class='CEURTITLE']")
                   teksty<-xml_text(tytuly)
                   #Autorzy
                   autorzy<-xml_find_all(x=strona, xpath = "//span[@class='CEURAUTHOR']")
                   autor<-xml_text(autorzy)
                   numery <- c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,26);
                   nazwa = "http://ceur-ws.org/Vol-1591/paper";
                   reszta = ".pdf"
                   pobierz <- (paste(nazwa,numery[aktualnyArtykul],reszta,sep = ""))
                   tekst<-pdf_text(pdf=pobierz);
                   tekst2<-str_replace_all(string=tekst, pattern="[\r\n]",replacement = " "); # jeden ze znakow jak wystapi to zastepujemy
                   tekst3<-str_squish(string=tekst2); #usuwa nadmiarowe spacje itp
                   
                   write.table(tekst3, file = "dane.txt", sep = "\t",row.names = FALSE)
                   my_data <- read.delim("dane.txt")
                   
                   polaczenie<-SolrClient$new(host="127.0.0.1", port=8983, path="/solr/petri_nets/select");
                   dodaj_artykul=strtoi(input$dodaj_artykul)
                   dokumenty<-data.frame(matrix(ncol=4, nrow=1));
                   colnames(dokumenty)[1]<-"id";
                   colnames(dokumenty)[2]<-"title";
                   colnames(dokumenty)[3]<-"autor";
                   colnames(dokumenty)[4]<-"content";
                   dokumenty$id<-c(aktualnyArtykul);
                   dokumenty$title<-c(teksty[aktualnyArtykul]);
                   dokumenty$autor<-c(autor[aktualnyArtykul]);
                   dokumenty$content<-c(my_data);
                   solrium::add(x=dokumenty, conn=polaczenie, name="petri_nets");
                 }
               });
  
  
  
  
  #usuwanie wszystkiego z bazy
  observeEvent(input$usuwanieBazy,
               {
                 polaczenie<-SolrClient$new(host="127.0.0.1", port=8983, path="/solr/petri_nets/select");
                 solrium::delete_by_query(conn=polaczenie, q="*", name = "petri_nets")
               });
  
  #odczyt artykulow z bazy
  observeEvent(input$odczytBazy,
               {
                 polaczenie<-SolrClient$new(host="127.0.0.1", port=8983, path="/solr/petri_nets/select");
                 dokumenty_solr<-solr_search(conn=polaczenie, params=list(q="*:*",rows=-1))
                 output$solr <- renderDataTable(dokumenty_solr)
               });
  
  #usuwanie artykukow z tabeli
  observeEvent(input$usuwanieTabeli,
               {
                 polaczenie<-SolrClient$new(host="127.0.0.1", port=8983, path="/solr/petri_nets/select");
                 dokumenty_solr<-solr_search(conn=polaczenie, params=list(q="id:xxxx",rows=-1))
                 output$solr <- renderDataTable(dokumenty_solr)
               });
  
  #chmura slow
  observeEvent(input$add_chmura,
               {
                 stop<-as.vector(unlist(read.csv(file="stop_words_pl.txt", header = FALSE, sep = ",", fileEncoding = "UTF-8")));
                 
                 polaczenie<-SolrClient$new(host="127.0.0.1", port=8983, path="/solr/petri_nets/select");
                 dokumenty1<-solr_search(conn=polaczenie, params=list(q="*:*",rows=-1));
                 text_corpus <- Corpus(VectorSource(dokumenty1))
                 inspect(text_corpus)
                 delete.words <- function(x) gsub("[–”„]", "", x);
                 corpus_clean <- tm_map(text_corpus, delete.words);
                 corpus_clean <- tm_map(corpus_clean, removePunctuation);
                 corpus_clean <- tm_map(corpus_clean, stripWhitespace);
                 corpus_clean <- tm_map(corpus_clean, removeNumbers);
                 corpus_clean <- tm_map(corpus_clean, content_transformer(tolower));
                 corpus_clean <- tm_map(corpus_clean, removeWords, stop);
                 
                 set.seed(1234);
                 #Generowanie chmury
                 tdm<-TermDocumentMatrix(corpus_clean);
                 m1<-as.matrix(tdm);
                 v<-sort(rowSums(m1), decreasing=TRUE);
                 d<-data.frame(word=names(v), freq=v);
                 
                 wordcloud_rep <- repeatable(wordcloud2)
                 output$wordcloud2 <- renderWordcloud2({
                   wordcloud2(data=d, color = tolower(input$kolorSelect), size = input$wielkoscSelect, shape = tolower(input$ksztaltSelect), backgroundColor = "grey")
                 })
                 
                 #Najczestsze wystepowanie slow
                 findFreqTerms(tdm, lowfreq = 4)
                 findAssocs(tdm, terms = "freedom", corlimit = 0.3)
                 head(d, 10)
                 output$slowa <- renderPlot({
                   barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word, col ="yellow", main ="Najczestsze slowa", ylab = "Czestotliwosci slow")
                 })
                 
                 #Dendrogram
                 myterms <- c("engine", "software", "petri");
                 findAssocs(tdm, myterms, corlimit=0.3);
                 lim <- 0.99;
                 tdmss <-removeSparseTerms(tdm, lim);
                 d1 <- dist(t(tdmss), method = "euclidian")
                 fit <- hclust(d=d1, method = "ward.D")
                 output$kmeans <- renderPlot({
                   plot(fit, main="Dendrogram", hang = -1)
                   })
               });
}

shinyApp(ui = ui, server = server)
