#define TMB_LIB_INIT R_init_mypkg
#include <TMB.hpp>
using namespace density;

template<class Type>
bool isNA(Type x){
  return R_IsNA(asDouble(x));
}

template<class Type>
Type softplus(Type x,Type epsilon)
{
  return 0.5*(x+sqrt(x*x+epsilon*epsilon));
}

template<class Type>
bool isFinite(Type x){
  return R_finite(asDouble(x));
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_STRING(model);
  if (model == "yaps_sync") {
    #include "yaps_sync.h"
  } else if (model == "yaps_track") {
    #include "yaps_track.h"
  } else {
    error ("Unknown model!");
  }
  return 0;
}
