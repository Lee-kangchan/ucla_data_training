library(igraph)

#3번 문제 
df.fb <- read.table(file.choose(), header = F)

#4번 문제
G.fb <- graph.data.frame(df.fb , directed = FALSE) 
plot(G.fb)
G.fb
df.fb
centralization.degree(G.fb, normalized = FALSE) # 연결 정도
centralization.betweenness(G.fb) #중개
centralization.closeness(G.fb) # 근접도


#5번 문제 / #6번 문제 
plot(degree(G.fb), main="투표 분포",ylab="투표 빈도" , xlab="투표자" ,type="h")


#7번 문제
sortlist = sort(degree(G.fb), decreasing = TRUE )
for(i in (1:10)){
  cat(i,"번째 높은 값 \n")
  print(sortlist[i])  
  cat("\n")
}

#8번 문제
V.max <- V(G.fb)$name[degree(G.fb) == sortlist[2]]
degree(G.fb, V.max)

V.max.idx <- which(V(G.fb)$name == V.max)
V.set <- neighbors(G.fb, v=V.max.idx)
V3 <- c(V.max.idx, V.set)
G.fb.max <- induced_subgraph(G.fb, V3)
V(G.fb.max)$color <- ifelse(V(G.fb.max)$name == V.max, 'red', 'green')
V(G.fb.max)$label <- ifelse(V(G.fb.max)$name == V.max, NA, NA)
plot(G.fb.max)


#9번 문제
V(G.fb.max)$size <- ifelse(V(G.fb.max)$name == V.max, 100, 20)
plot(G.fb.max)

