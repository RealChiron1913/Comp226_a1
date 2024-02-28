book.total_volumes <- function(book) {
    return(list(
        ask = sum(book$ask$size),
        bid = sum(book$bid$size)
    ))

}

book.best_prices <- function(book) {
    return(list(
        ask = min(book$ask$price),
        bid = max(book$bid$price))
    )
}

book.midprice <- function(book) {
    return((min(book$ask$price) + max(book$bid$price)) / 2)
}

book.spread <- function(book) {
    return(min(book$ask$price) - max(book$bid$price))
}

book.add <- function(book, message) {
    if (message$side == "B"){
        book$bid <- rbind(book$bid, message[c("oid", "price", "size")])
    }
    else if (message$side == "S") {
        book$ask <- rbind(book$ask, message[c("oid", "price", "size")])
        }
    book <- book.sort(book,T,T)
    while(book$ask$price [1]<= book$bid$price[1] & nrow(book$ask) > 0 & nrow(book$bid) > 0){
        size <- min(book$ask$size[1], book$bid$size[1])
        book$ask$size[1] <- book$ask$size[1] - size
        book$bid$size[1] <- book$bid$size[1] - size
        if (book$ask$size[1] == 0){
            book$ask <- book$ask[-1,]
        }
        if (book$bid$size[1] == 0){
            book$bid <- book$bid[-1,]
        }
    }
    return(book)
}

book.reduce <- function(book, message) {
    book$ask[book$ask$oid == message$oid, "size"] <- book$ask[book$ask$oid == message$oid, "size"] - message$amount
    book$bid[book$bid$oid == message$oid, "size"] <- book$bid[book$bid$oid == message$oid, "size"] - message$amount
    book$ask <- book$ask[book$ask$size > 0, ]
    book$bid <- book$bid[book$bid$size > 0, ]
    return(book)
}

###############################################################################
###############################################################################

# The following functions are the "extra" functions; marks for these functions
# are only available if you have fully correct implementations for the 6
# functions above

book.extra1 <- function(book, size) {
    # See handout for instructions
    # return the expected value of the midprice after execution of a buy limit order with size size and price drawn uniformly at random from the set of prices in book$ask.
    if (nrow(book$ask) == 0 | size == sum(book$ask$size)){
        return(NA)
    }
    else{
        old_book <- book
        total_mid <- 0
        for (i in 1:nrow(old_book$ask)){
            book <- old_book
            new_size <- size
            while (book$ask$size[1] <= new_size & book$ask$price[1] <= old_book$ask$price[i]){
                new_size <- new_size - book$ask$size[1]
                book$ask <- book$ask[-1,]
                book <- book.sort(book, T, T)
            }

            total_mid <- total_mid + (book$ask$price[1] + max(book$bid$price)) / 2
        }
        return(total_mid / nrow(old_book$ask))
    }


}

book.extra2 <- function(book, size) {
    # See handout for instructions
    if (nrow(book$ask) == 0 | size == sum(book$ask$size)){
        return(NA)
    }
    else{
        old_book <- book
        total_mid <- 0
        count <- 0
        for (i in 1:nrow(old_book$ask)){
            if (i > 1){
                if (old_book$ask$price[i] == old_book$ask$price[i - 1]) {
                    next
                }
            }
            book <- old_book
            new_size <- size
            while (book$ask$size[1] <= new_size & book$ask$price[1] <= old_book$ask$price[i]){
                new_size <- new_size - book$ask$size[1]
                book$ask <- book$ask[-1,]
                book <- book.sort(book, T, T)
            }
            total_mid <- total_mid + (book$ask$price[1] + max(book$bid$price)) / 2
            count <- count + 1
        }

        return(total_mid / count)
    }
}

book.extra3 <- function(book) {
    # See handout for instructions
    if (nrow(book$ask) == 0){
        return(NA)
    }
    else{
        total_mid <- 0
        old_book <- book
        total_volumes <- sum(book$ask$size)
        for (size in 1:total_volumes - 1){
            new_size <- size
            book <- old_book
            while (book$ask$size[1] <= new_size){
                new_size <- new_size - book$ask$size[1]
                book$ask <- book$ask[-1,]
                book <- book.sort(book, T, T)

            }
            total_mid <- total_mid + (book$ask$price[1] + max(book$bid$price)) / 2
        }

        return(total_mid / (total_volumes - 1))
    }
}

book.extra4 <- function(book, k) {
    k <- k / 100
    if (nrow(book$ask) == 0){
        return(0)
    }
    else{
        for (i in 1:nrow(book$ask) - 1){
            if ((book$ask$price[i + 1] + max(book$bid$price)) / 2 > (1 + k) * (max(book$bid$price) + min(book$ask$price)) / 2){
                # print(i)
                sum_size <- sum(book$ask$size[1:i])-1
                return(sum_size)
            }
            if (i == nrow(book$ask) - 1){
                return(sum(book$ask$size)-1)
            }
        }
    }
    # See handout for instructions
}