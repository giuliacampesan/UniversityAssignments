k2_learn_net <- function(BNdata, max.parents){
    bn <- BN(BNdata)
    num.nodes(bn)  <- num.variables(BNdata)
    node.sizes(bn) <- node.sizes(BNdata)
    variables(bn)  <- variables(BNdata)
    validObject(bn)
    data = as.data.frame(raw.data(BNdata))
    scoring.func(bn) <- 'k2'

    nodes <- colnames(data)
    n <- ncol(data)
    dag <- matrix(0, nrow = n, ncol = n)
    cpts <- list()

    for (i in 1:n) {
        parent <- character()
        p.old <- myscore(parent, i, data)
        proceed <- TRUE
        pred_xi <- nodes[1:i-1]
        while (proceed & (length(parent)<max.parents)) {
            set <- setdiff(pred_xi, parent)
            unions <- lapply(set, union, parent)
            new.parent <- unlist(unions[which.max(sapply(unions, function(x) myscore(x, i, data)))])
            p.new <- myscore(new.parent, i, data)
            if ( p.new > p.old ) {
                p.old <- p.new
                parent <- new.parent
            }
            else {
                proceed <- FALSE
            }
        }
        if (length(parent) == 0){
                cpts[nodes[i]] <- list(table(data[nodes[i]])/sum(table(data[nodes[i]])))
        }
        else {
            count <- 1
            for (p in parent){
                j <- grep(p, colnames(data))
                dag[j,i] <- 1
                cpts[nodes[i]][count] <- list(table(data[c(nodes[i], p)])/colSums(table(data[c(nodes[i], p)])))
                count <- count+1
            }
        }
        cat('node', nodes[i], 'has parents', parent, '\n')
    }
    dag(bn) <- dag
    cpts(bn) <- cpts
    return(bn)
}

myscore <- function(pi, i, D){
    # number of possible ourcomes for node i
    r = length(unique(D[,i]))  
    # generalized formula for no-parent case (from paper)
    if (length(pi) == 0){
        alpha = table(D[i])
        N = sum(alpha)
        prob = sum(sapply(1:(r-1),log))-sum(sapply(1:(N+r-1),log))+sum(sapply(alpha,function(a) ifelse(a==0,0,sum(sapply(1:a,log))))) 
    }
    # standard formula (from paper)
    else{ 
        D2 = cbind(D[i],D[pi])
        alpha = table(D2)
        if (length(dim(alpha))>2){dim(alpha)=c(r,length(alpha)/r)}
        alpha = t(alpha)
        N.j = rowSums(alpha)
        M.j = apply(apply(alpha,c(1,2),function(a) ifelse(a==0,0,sum(sapply(1:a,log)))), 1 , sum)
        prob = sum(apply(cbind(N.j,M.j),1, function(x) sum(sapply(1:(r-1),log))-sum(sapply(1:(x[1]+r-1),log))+x[2]))
    }
}             