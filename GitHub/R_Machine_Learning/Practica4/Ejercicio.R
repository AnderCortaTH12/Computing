library(ISLR2)

nci.labs <- NCI60$labs
nci.data <- NCI60$data

table(nci.labs)

sd.data = scale(nci.data)

hc.complete =hclust(dist(sd.data), method="complete")
hc.average =hclust(dist(sd.data), method ="average")

par(mfrow=c(1,2))
plot(hc.complete ,main="Complete Linkage", xlab="", sub="", cex=.9, labels = nci.labs)
plot(hc.average , main="Average Linkage", xlab="", sub="", cex=.9, labels = nci.labs)

hc.complete

hc.complete = cutree(hc.complete, 4)

tab = table(hc.complete, nci.labs)
print(tab)

"Podemos ver como en prácticamente todos los canceres, estan todos dentro del mismo subgrupo por ejemplo la leucemia en el grupo 3, el de ovarios en el 1 y demás. 
Existen otros cánceres que , sin embargo, no están en el mismo cluster, este es el caso del cancer de pecho en el que los casos están en diferentes cluster
y lo mismo pasa con otros como el de colon y hasta cierto punto el renal y NSCLC "