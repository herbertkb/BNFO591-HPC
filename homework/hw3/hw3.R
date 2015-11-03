# Keith Herbert
# Homework 3
# Finds diameter for a network described by node-node connections


# Functions ##############################################################

vector.index <- function(v, x) {
    # linear search vector for x and return its index
    for(i in 1:length(v)){
        if(v[i] == x){
            return(i)
        }
    }
}

# Main ####################################################################

# get the data
input.file <- "experimental_data.dat"
data <- read.table(input.file)
data <- as.matrix(data)

# setup the matrix
n <- dim(data)[1]
A <- matrix(rep(0, n*n), nrow=n, ncol=n )
Z <- matrix(rep(F, n*n), nrow=n, ncol=n )

# prepare sorted list of labels for indexing in matrix
labels <- sort(unique(as.vector(data)))

#print(A)

# fill the matrix
for (i in 1:n) {
    a <- vector.index(labels, data[i,1])
    b <- vector.index(labels, data[i,2])
    
    A[a,b] <- 1
    Z[a,b] <- T
    
    A[b,a] <- 1
    Z[b,a] <- T
}

#print(A)

diameter <- n-1
for(d in 1:n-1){

    A <- A %*% A
    #print(A)
    
    for (i in 1:n) {
        for(j in 1:n) {
            if( A[i,j] != 0 ){
                Z[i,j] <- T
            }
        }
    }
    
    if( all(Z) ){
        diameter <- d
        break
    }
}

print(diameter)

    
