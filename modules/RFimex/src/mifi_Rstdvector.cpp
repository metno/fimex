#include <R.h>
#include <Rinternals.h>
#include <vector>

extern "C" SEXP mifi_Rdoublevector(SEXP args)
{
    SEXP retlist, retlistnames;
    std::vector< double > *arg1;

    /*-- Create output object and initialize return values --------------------*/
    PROTECT(retlist = allocVector(VECSXP, 3));
    SET_VECTOR_ELT(retlist, 0, allocVector(REALSXP, 1));
    SET_VECTOR_ELT(retlist, 1, allocVector(STRSXP,  1));
    SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, 0)); // dummy here

    PROTECT(retlistnames = allocVector(STRSXP, 3)); 
    SET_STRING_ELT(retlistnames, 0, mkChar("status"));
    SET_STRING_ELT(retlistnames, 1, mkChar("errmsg")); 
    SET_STRING_ELT(retlistnames, 2, mkChar("data")); 
    setAttrib(retlist, R_NamesSymbol, retlistnames); 

    arg1 = reinterpret_cast<std::vector<double>* >(R_ExternalPtrAddr(args));
    if (arg1 == 0) {
        SET_VECTOR_ELT (retlist, 1, mkChar("wrong input, no vector<double>*"));
        REAL(VECTOR_ELT(retlist, 0))[0] = 1;	 
        UNPROTECT(2);
        return(retlist);
    }        

    // allocate a new pointer
    SET_VECTOR_ELT(retlist, 2, allocVector(REALSXP, arg1->size()));

    /*-- Copy from C++ to R object ----------------------------------------------*/
    for(size_t i=0; i < arg1->size(); i++)
        REAL(VECTOR_ELT(retlist, 2))[i] = arg1->at(i);


    UNPROTECT(2);
    return(retlist);
}

