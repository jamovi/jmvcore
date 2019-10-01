
#include <Rcpp.h>
#include <map>

using namespace Rcpp ;

typedef std::map<SEXP, SEXP> Clones;


SEXP deepCloneRecurse(SEXP obj) {

    if (is<Environment>(obj)) {
        Environment old = obj;

        SEXP enclos = old.get(".__enclos_env__");
        SEXP cl4ss = old.attr("class");
        bool isR6 = false;
        if (cl4ss != R_NilValue) {
            CharacterVector scl4ss = as<CharacterVector>(cl4ss);
            isR6 = (enclos != R_NilValue && std::find(scl4ss.begin(), scl4ss.end(), "R6") != scl4ss.end());
        }

        Environment nu = Environment::empty_env().new_child(true);

        SEXP nu_enclos;

        CharacterVector names = as<CharacterVector>(old.ls(true));

        for (SEXP name : names) {
            String nameStr = name;
            if (isR6 && (nameStr) == ".__enclos_env__") {
                Environment old_enclos(enclos);
                Environment enc = old_enclos.parent().new_child(false);
                enc.assign("self", nu);
                nu_enclos = enc;
                SEXP child = enc;
                nu.assign(nameStr, child);

                SEXP priv = old_enclos.get("private");
                if (priv != R_NilValue) {
                    Environment penv = as<Environment>(priv);
                    Environment pnu = penv.parent().new_child(false);

                    CharacterVector pnames = penv.ls(true);
                    for (SEXP pname : pnames) {
                        SEXP pchild = deepCloneRecurse(penv.get(pname));
                        if (isR6 && is<Function>(pchild)) {
                            SET_CLOENV(pchild, nu_enclos);
                        }
                        pnu.assign(CHAR(pname), pchild);
                    }
                    enc.assign("private", pnu);
                }
            }
            else {
                SEXP sym = Rf_install(CHAR(name));
                if (R_BindingIsActive(sym, obj)) {
                    SEXP frame = FRAME(obj);
                    while (frame != R_NilValue && TAG(frame) != sym)
                        frame = CDR(frame);
                    SEXP fun = CAR(frame);
                    fun = Rcpp::clone(fun);
                    SET_CLOENV(fun, nu_enclos);
                    R_MakeActiveBinding(sym, fun, nu);
                }
                else {
                    SEXP child = deepCloneRecurse(old.get(name));
                    if (isR6 && is<Function>(child)) {
                        if (CLOENV(child) == enclos)
                            SET_CLOENV(child, nu_enclos);
                    }
                    nu.assign(nameStr, child);
                }
            }

        }

        nu.attr("class") = cl4ss;

        // if (R_EnvironmentIsLocked(old))
        //     R_LockEnvironment(nu, Rboolean(true));

        return nu;
    }
    else if (is<List>(obj)) {
        List nu = Rcpp::clone(obj);
        for (int i = 0; i < nu.size(); i++) {
            SEXP child = nu[i];
            if (is<Environment>(child))
                nu[i] = deepCloneRecurse(child);
        }
        return nu;
    }
    else {
        return Rcpp::clone(obj);
    }
}

// [[Rcpp::export]]
SEXP deepClone(SEXP obj) {
    return deepCloneRecurse(obj);
}

// [[Rcpp::export]]
SEXP repR6(SEXP obj, int n) {
    List lst(n);

    for (int i = 0; i < n; i++) {
        lst[i] = deepClone(obj);
    }

    return lst;
}

// [[Rcpp::export]]
void swap(SEXP one, SEXP two) {
    SEXP *pone = &one;
    SEXP *ptwo = &two;
    SEXP *temp = &one;
    pone = ptwo;
    ptwo = temp;
}

// [[Rcpp::export]]
void set_priv(Rcpp::Environment r6, SEXP priv) {

    Environment enc = r6.get(".__enclos_env__");
    // SEXP priv2 = enc.get("private");
    // SEXP temp = priv;
    // priv = priv2;
    // priv2 = priv;
    enc.assign("private", priv);
    // enc.assign("self", r6);
}

RCPP_MODULE(R6Utils) {
    function("repR6", &repR6);
    function("set_priv", &set_priv);
    function("swap", &swap);
}
