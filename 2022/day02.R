library(data.table)
sum(fread("input02.txt",header = FALSE)[,
  `:=`(V1 = match(V1,LETTERS), V2 = match(V2,LETTERS)-23)][
    ,wdl:=fcase(V1-V2==0,3,V1-V2==-1,6,V1-V2==2,6,default=0)][
      ,score:=V2+wdl][["score"]])

sum(fread("input02.txt",header = FALSE)[
  ,`:=`(V1 = match(V1,LETTERS),wdl=fcase(V2=="X",0,V2=="Y",3,V2=="Z",6))][
    ,V2:=fcase(wdl==3,V1,(wdl==6 & V1<3),V1+1L,(wdl==6 & V1==3),1L,(wdl==0 & V1>1),V1-1L,default=3L)][
      ,score:=V2+wdl][["score"]])



