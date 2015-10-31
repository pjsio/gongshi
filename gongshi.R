InstallCandidates <- c("data.table","stringr","ggplot2","httr","rvest","XML","RSelenium","KoNLP")
# check if pkgs are already present
toInstall <- InstallCandidates[!InstallCandidates %in% library()$results[,1]]
if(length(toInstall)!=0) {install.packages(toInstall, repos = "http://cran.r-project.org")}
# load pkgs
lapply(InstallCandidates, library, character.only = TRUE)

RSelenium::startServer( invisible = TRUE)
eCap <- list(phantomjs.binary.path = "C:/Users/parkets/Documents/phantomjs-2.0.0-windows/bin/phantomjs.exe")
remDr <- remoteDriver(browserName = "phantomjs", extraCapabilities = eCap)
remDr$open()
uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"

remDr$close()
remDr$maxWindowSize()
remDr$screenshot(display=T)

## ���� ������

qreport<-function(url){
remDr$maxWindowSize()
url<-paste0("https://dart.fss.or.kr",url)
     remDr$navigate(url)
Sys.sleep(1)

test<-(remDr$getPageSource()[[1]])
te<-html(test)
testurl<-html_attr(html_nodes(te, 'iframe'), 'src')

url<-paste0("https://dart.fss.or.kr",testurl)
tt<-html(url)
title<-html_text(html_nodes(tt,'p a'))
title<-gsub(" ","",title)

date<-html_text(html_nodes(tt,'tr td'))
date<-date[9]
date<-gsub(" ","",date)

comname<-html_text(html_nodes(tt,'tr td'))
comname<-comname[21]

webElems <- remDr$findElements(using = "xpath", '//*[@id="ext-gen10"]/div/li/div')
Sys.sleep(0.5)

sectiontexts<-unlist(lapply(webElems, function(x){x$getElementText()}))
sectiontexts<-sectiontexts[nchar(sectiontexts)>1]
t<-grep("�繫",sectiontexts)
xpath<-paste0('//*[@id="ext-gen10"]/div/li[',t,']/ul/li[1]/div/a/span')
webElem <- remDr$findElement("xpath", xpath)

webElem$clickElement()
Sys.sleep(1)

test<-(remDr$getPageSource()[[1]])
te<-html(test)
testurl<-html_attr(html_nodes(te, 'iframe'), 'src')

url<-paste0("https://dart.fss.or.kr",testurl)
tt<-html(url)

##colnames<-html_text(html_nodes(tt,'tr th'))
##colnames<-colnames[6:7]

contents<-html_text(html_nodes(tt,'tr td'))
Encoding(contents)<-"UTF-8"

forscale<-html_text(html_nodes(tt,'body'))
forscale<-strsplit(forscale,"\n")
forscale<-forscale[[1]][grep("����",forscale[[1]])]
forscale<-forscale[1]
forscale<-str_match(forscale,'����.*\\)')
forscale<-gsub(" ","",forscale)
forscale<-unlist(strsplit(forscale,""))
forscale<-paste0(forscale[is.hangul(forscale)],collapse="")
forscale<-substr(forscale,3,nchar(forscale))

contents<-gsub(" ","",contents)
jasanloc<-grep("�ڻ��Ѱ�",contents)
jasanloc<-jasanloc[length(jasanloc)]
buchaeloc<-grep("��ä�Ѱ�",contents)
buchaeloc<-buchaeloc[length(buchaeloc)]
jabonloc<-grep("�ں��Ѱ�",contents)
jabonloc<-jabonloc[length(jabonloc)]
jasan<-c(contents[jasanloc+1],contents[jasanloc+2])
buchae<-c(contents[buchaeloc+1],contents[buchaeloc+2])
jabon<-c(contents[jabonloc+1],contents[jabonloc+2])
maejulloc<-grep("�����",contents)
maejulloc<-maejulloc[length(maejulloc)]
if(identical(maejulloc,as.integer(NA))|identical(maejulloc,integer(0))){
maejulloc<-grep("��������",contents)
maejulloc<-maejulloc[length(maejulloc)]
}
sunloc<-grep("����",contents)
sunloc<-sunloc[length(sunloc)]
maejul<-c(contents[maejulloc+1],contents[maejulloc+2])
sun<-c(contents[sunloc+1],contents[sunloc+2])

data<-c(jasan,buchae,jabon,maejul,sun)
minusloc<-grep(")",data)
data<-gsub(")","",data)
data[minusloc]<-substr(data[minusloc],2,nchar(data[minusloc]))
data<-as.numeric(str_trim(as.character(gsub(",","",data))))

if(forscale=="��"){
data<-round(data/1000000)
data[minusloc]<-data[minusloc]*(-1)
}
if(forscale=="õ��"){
data<-round(data/1000)
data[minusloc]<-data[minusloc]*(-1)
}
if(forscale=="�鸸��"){
data<-round(data)
data[minusloc]<-data[minusloc]*(-1)
}

data<-as.data.frame(matrix(data,nrow=2))
names(data)<-c("jasan","buchae","jabon","maejul","sun")

if(data[1,1]>data[2,1]){jasantxt<-"����"}else{jasantxt<-"����"}
if(data[1,2]>data[2,2]){buchaetxt<-"����"}else{buchaetxt<-"����"}
if(data[1,3]>data[2,3]){jabontxt<-"����"}else{jabontxt<-"����"}
if(data[1,4]>data[2,4]){maejultxt<-"����"}else{maejultxt<-"����"}
if(data[1,5]>data[2,5]){suntxt<-"����"}else{suntxt<-"����"}
sunproper<-round(data[1,5]/data[1,4],digits = 1)-round(data[2,5]/data[2,4],digits = 1)
buchaeper<-round(data[1,2]/data[1,3],digits = 1)-round(data[2,2]/data[2,3],digits = 1)
if(sunproper>0){sunprotxt<-"����"}else{sunprotxt<-"����"}
if(buchaeper>0){bupertxt<-"����"}else{bupertxt<-"����"}

twtext<<-paste0(comname,"��(��) ",date,"�� ",title,"��(��) �����Ͽ����ϴ�.",
" �������� ������ ",abs(data[1,5]-data[2,5]),"�鸸����ŭ ",suntxt,"�Ͽ���, ������� ",
abs(data[1,4]-data[2,4]),"�鸸����ŭ ",maejultxt,"�Ͽ����ϴ�. ���ڻ��� ",abs(data[1,1]-data[2,1]),
"�鸸����ŭ ",jasantxt,"�Ͽ����ϴ�. ���� ���������� ",round(data[1,5]/data[1,4],digits = 1),
"%�̰� ��ä������ ",round(data[1,2]/data[1,3],digits = 1),"%�Դϴ�. �̰��� ������ ���� ",
abs(sunproper),"%p",sunprotxt,", ",abs(buchaeper),"%p",bopertxt,"�� ���Դϴ�.")
}


## ���⺸���� �׽�Ʈ �� url ����
##url<-df[grep("�б⺸����",df$title),3][3]



## link ����
url<-"https://dart.fss.or.kr/dsac001/mainAll.do"

     remDr$navigate(url)
webElem <- remDr$findElement("xpath", '//*[@id="listContents"]/div[2]/p')
listnum<-unlist(webElem$getElementText())
listnum<-unlist(strsplit(listnum,""))
alnf<-grep("/",listnum)+1
alne<-grep("]",listnum)[1]-1
pagenum<-paste0(listnum[alnf:alne],collapse="")
pagenum<-as.numeric(pagenum)
allconf<-grep(']',listnum)[1]+5
allcone<-grep("]",listnum)[2]-3
allcon<-listnum[allconf:allcone]
allcon<-paste0(allcon,collapse="")

df<-data.frame(com="test", title="test", url="test")
df<-df[df$com=="2agasdc",]
for(i in 1:pagenum){

xpath<-paste0('//*[@id="listContents"]/div[2]/input[',i,']')

webElem <- remDr$findElement("xpath", xpath)
Sys.sleep(0.5)
webElem$clickElement()
Sys.sleep(1)
test<-(remDr$getPageSource()[[1]])
class(test)

listnum<- html(test) %>%
  html_nodes('div p.page_info') %>%
  html_text
Encoding(listnum)<-"UTF-8"
listnum<-gsub("\n","",listnum)
listnum<-gsub("\r","",listnum)
listnum<-gsub("\t","",listnum)
listnum<-gsub("  ","",listnum)
listnum
listnum<-unlist(strsplit(listnum,""))
cnf<-grep("/",listnum)-1
cupage<-(listnum[cnf])

url_list<- html(test) %>%
  html_nodes('a') %>%
  html_attr('href')
url_list<-url_list[grep("dsaf001",url_list)]

com<- html(test) %>%
  html_nodes('td span a') %>%
  html_text
Encoding(com)<-"UTF-8"
com<-gsub("\r","",com)
com<-gsub("\t","",com)
com<-gsub("\n","",com)
com<-gsub("  ","",com)
comname<-com[nchar(com)>1]

contents<-html(test) %>%
  html_nodes('td a') %>%
  html_text
Encoding(contents)<-"UTF-8"
contents<-gsub("\r","",contents)
contents<-gsub("\t","",contents)
contents<-gsub("\n","",contents)
contents<-gsub("  ","",contents)
contents<-contents[nchar(contents)>1]

if(i<pagenum){
k<-200}else{
k<-2*(as.numeric(allcon)%%100)}

contents<-contents[1:k]
even_indexes<-seq(2,k,2)
contents<-contents[even_indexes]

df.temp<-data.frame(com=comname, title=contents, url=url_list)
df<-rbind(df,df.temp)

}

foriend<-nrow(df)
foriend

for(i in foriend:1){
i<-foriend
grep("����ȭ����ȸ��",df[i,2])

if(!identical(grep("�б⺸����",df[i,2]),integer(0))){

if(identical(grep("����ȭ����ȸ��",df[i,2]),integer(0))){

tw<-qreport(df[i,3])

} ## if "����ȭ����ȸ��" end

} ## if '�б⺸����' end

} ## for i end


url<-"https://dart.fss.or.kr/dsaf001/main.do?rcpNo=20151030000510"


url<-paste0("https://dart.fss.or.kr",url_list[3])
     remDr$navigate(url)
test<-(remDr$getPageSource()[[1]])
te<-html(test)
testurl<-html_attr(html_nodes(te, 'iframe'), 'src')
testurl

remDr$screenshot(display=T)

url<-paste0("https://dart.fss.or.kr",testurl)
     remDr$navigate(url)
tt<-html(url)
title<-html_text(html_nodes(tt,'p a'))
Encoding(title)<-"UTF-8"
title

con1<-html_text(html_nodes(tt,'table.nb tbody tr td'))
Encoding(con1)<-"UTF-8"
con1
