LoR1 <- scan("LoR1.txt", what = "character", encoding = "UTF-8")
head(LoR1)
LoR1 <- gsub("<i>", "", LoR1)
LoR1 <- gsub("</i>", "", LoR1)
LoR1 <- gsub("[^[:alpha:]]", "", LoR1)
head(LoR1)
LoR1 <- tolower(LoR1)
LoR1 <- LoR1[nchar(LoR1) > 0]
LoR1_freq <- table(LoR1)
sort(LoR1_freq, decreasing = TRUE)[1:20]

LoR2 <- scan("LoR2.txt", what = "character", encoding = "UTF-8")
head(LoR2)
LoR2 <- gsub("<i>", "", LoR2)
LoR2 <- gsub("</i>", "", LoR2)
LoR2 <- gsub("[^[:alpha:]]", "", LoR2)
head(LoR2)
LoR2 <- tolower(LoR2)
LoR2 <- LoR2[nchar(LoR2) > 0]
LoR2_freq <- table(LoR2)

LoR3 <- scan("LoR3.txt", what = "character", encoding = "UTF-8")
head(LoR3)
LoR3 <- gsub("<i>", "", LoR3)
LoR3 <- gsub("</i>", "", LoR3)
LoR3 <- gsub("[^[:alpha:]]", "", LoR3)
head(LoR3)
LoR3 <- tolower(LoR3)
LoR3 <- LoR3[nchar(LoR3) > 0]
LoR3_freq <- table(LoR3)

Hobbit1 <- scan("Hobbit1.txt", what = "character", encoding = "UTF-8")
head(Hobbit1)
Hobbit1 <- gsub("<i>", "", Hobbit1)
Hobbit1 <- gsub("</i>", "", Hobbit1)
Hobbit1 <- gsub("[^[:alpha:]]", "", Hobbit1)
head(Hobbit1)
Hobbit1 <- tolower(Hobbit1)
Hobbit1 <- Hobbit1[nchar(Hobbit1) > 0]
Hobbit1_freq <- table(Hobbit1)

Hobbit1 <- scan("Hobbit1.txt", what = "character", encoding = "UTF-8")
head(Hobbit1)
Hobbit1 <- gsub("<i>", "", Hobbit1)
Hobbit1 <- gsub("</i>", "", Hobbit1)
Hobbit1 <- gsub("[^[:alpha:]]", "", Hobbit1)
head(Hobbit1)
Hobbit1 <- tolower(Hobbit1)
Hobbit1 <- Hobbit1[nchar(Hobbit1) > 0]
Hobbit1_freq <- table(Hobbit1)

Hobbit2 <- scan("Hobbit2.txt", what = "character", encoding = "UTF-8")
head(Hobbit2)
Hobbit2 <- gsub("<i>", "", Hobbit2)
Hobbit2 <- gsub("</i>", "", Hobbit2)
Hobbit2 <- gsub("[^[:alpha:]]", "", Hobbit2)
head(Hobbit2)
Hobbit2 <- tolower(Hobbit2)
Hobbit2 <- Hobbit2[nchar(Hobbit2) > 0]
Hobbit2_freq <- table(Hobbit2)

Hobbit3 <- scan("Hobbit3.txt", what = "character", encoding = "UTF-8")
head(Hobbit3)
Hobbit3 <- gsub("<i>", "", Hobbit3)
Hobbit3 <- gsub("</i>", "", Hobbit3)
Hobbit3 <- gsub("[^[:alpha:]]", "", Hobbit3)
head(Hobbit3)
Hobbit3 <- tolower(Hobbit3)
Hobbit3 <- Hobbit3[nchar(Hobbit3) > 0]
Hobbit3_freq <- table(Hobbit3)

Hobbit1_df <- data.frame(Word = names(Hobbit1_freq), Hobbit1 = as.numeric(Hobbit1_freq))
head(Hobbit1_df)

Hobbit2_df <- data.frame(Word = names(Hobbit2_freq), Hobbit2 = as.numeric(Hobbit2_freq))
Hobbit3_df <- data.frame(Word = names(Hobbit3_freq), Hobbit3 = as.numeric(Hobbit3_freq))
LoR1_df <- data.frame(Word = names(LoR1_freq), LoR1 = as.numeric(LoR1_freq))
LoR2_df <- data.frame(Word = names(LoR2_freq), LoR2 = as.numeric(LoR2_freq))
LoR3_df <- data.frame(Word = names(LoR3_freq), LoR3 = as.numeric(LoR3_freq))


Tolkien <- merge(LoR1_df, LoR2_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, LoR3_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, Hobbit1_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, Hobbit2_df, by = "Word", all = TRUE) 
Tolkien <- merge(Tolkien, Hobbit3_df, by = "Word", all = TRUE) 

head(Tolkien)
Tolkien[is.na(Tolkien)] <- 0

cor(Tolkien[, 2:7], method = "spearman")
Tolkien_dist <- as.dist(1 - cor(Tolkien[, 2:7]))
Tolkien_clust <- hclust(Tolkien_dist, method = "average")
plot(Tolkien_clust)

Tolkien_clust <- hclust(Tolkien_dist, method = "ward.D2")
plot(Tolkien_clust)

Tolkien_clust <- hclust(Tolkien_dist, method = "complete")
plot(Tolkien_clust)


library(smacof) #install the package first!
Tolkien_mds <- mds(Tolkien_dist)
plot(Tolkien_mds)