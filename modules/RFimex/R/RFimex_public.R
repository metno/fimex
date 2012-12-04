# public function of the RFimex module

mifi.vec.2R <- function(vec) {
    if ( extends(class(vec), '_p_std__vectorT_double_std__allocatorT_double_t_t' ) ) {
       ; ans <- .Call("mifi_Rdoublevector", altitude, PACKAGE='RFimex');
    } else {
       stop("cannot find overloaded function for mifi.vec.2R and type", toString(class(vec)));
    };
    ans
}


