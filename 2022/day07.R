library(igraph)
input <- c(readLines("input07.txt"),"$")

traverse_graph <- function(input){
  cmds <- grep("^\\$",input)[-1]
  g <- make_empty_graph(n = 1, directed = TRUE)
  V(g)$name[1] <- "home"
  V(g)$fsize <- NA_integer_
  current_node <- 1
  n <- 1
  for(i in seq_along(cmds)){
    cmd <- input[cmds[i]]
    if(cmd=="$ ls"){
      # if(cmds[i+1]!=cmds[i]+1)
      content <- input[(cmds[i]+1):(cmds[i+1]-1)]
      type <- strsplit(content," ")
      names <- sapply(type,function(x) x[2])
      sizes <- sapply(type,function(x) suppressWarnings(as.integer(x[1])))
      # names <- ifelse(is.na(sizes),names,paste0("f-",names))
      g <- add_vertices(g,length(content),
                        attr = list(name = names),
                                    fsize = sizes)
      # el <- matrix(c(rep(V(g)$name[current_node],length(content)),names),ncol=2)
      el <- matrix(c(rep(current_node,length(content)),(n+1):(n+length(content))),ncol = 2)
      g <- add_edges(g,c(t(el)))
      n <- n+length(content)
    } else if(cmd=="$ cd .."){
      current_node <- neighborhood(g,1,current_node,"in",mindist = 1)[[1]][1]
    } else{
      dir <- gsub("\\$ cd ","",cmd)
      cands <- as.integer(neighborhood(g,1,current_node,"out",mindist = 1)[[1]])
      tmp <- which(V(g)$name==dir)
      if(length(tmp)>1){
        current_node <- tmp[tmp%in%cands]
      } else{
        current_node <- tmp 
      }
      # print(current_node)
    }
    if(!is.connected(g)){
      break
    }
  }
  g
}
g <- traverse_graph(input)

dirs <- which(is.na(V(g)$fsize))
sizes <- sapply(dirs,function(x)sum(V(g)$fsize[bfs(g,x,mode = "out",unreachable = FALSE)$order],na.rm=TRUE))
# part 1
sum(sizes[sizes<100000])

#part 2
free <- 70000000-sizes[1]
to_free <- 30000000-free
made_free <- sort(sizes)-to_free
sort(sizes)[which(made_free>0)[1]]

library(ggraph)
ggraph(g,"stress") + 
  geom_edge_link0()+
  geom_node_point(shape=21,aes(fill=is.na(fsize)),size=3,show.legend = FALSE)+
  theme_graph()

ggsave("graph07.png", height=8,width=8)
