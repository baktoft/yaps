#include <TMB.hpp>
using namespace density;

template<class Type>
bool isNA(Type x){
  return R_IsNA(asDouble(x));
}

template<class Type>
Type objective_function<Type>::operator() ()
{
	DATA_ARRAY(H);
	DATA_ARRAY(toa);   		// Time of arrival at hydro. One row per buoy, one column per ping
	// DATA_VECTOR(beac);
	DATA_VECTOR(sync_tag_idx_vec);
	DATA_INTEGER(np);
	DATA_INTEGER(nh);
	DATA_INTEGER(tk);		// hydro number that is time keeper
	DATA_VECTOR(fixed_hydros_vec);
	DATA_VECTOR(offset_idx);
	DATA_INTEGER(n_offset_idx);
	DATA_VECTOR(ss_idx);
	DATA_INTEGER(n_ss_idx);
	
	PARAMETER_VECTOR(TOP);		// Estimated time of pings
	PARAMETER_ARRAY(OFFSET);
	PARAMETER_ARRAY(SLOPE1);
	PARAMETER_ARRAY(SLOPE2);
	PARAMETER_VECTOR(SS);
	// PARAMETER_VECTOR(TRUE_X);
	// PARAMETER_VECTOR(TRUE_Y);
	PARAMETER_ARRAY(TRUE_H)

	PARAMETER(LOG_SIGMA_TOA);    		
	Type SIGMA_TOA = exp(LOG_SIGMA_TOA);
	
	PARAMETER_VECTOR(LOG_SIGMA_HYDROS_XY);
	vector<Type> SIGMA_HYDROS_XY = exp(LOG_SIGMA_HYDROS_XY);

	array<Type> mu_toa(np,nh);  // mu-matrix
	array<Type> eps_toa(np,nh);  // mu-matrix
	array<Type> dist_mat(nh,nh);

	// Type nll = 0.0;
	parallel_accumulator<Type> nll(this);  
	
	for(int h1=0; h1 < nh; ++h1){
		for(int h2=0; h2 < nh; ++h2){
			if(h1 == h2){
				dist_mat(h1, h2) = Type(0.0);
			} else {
				// dist_mat(h1, h2) = sqrt(pow(TRUE_X(h1) - TRUE_X(h2), 2) + pow(TRUE_Y(h1) - TRUE_Y(h2), 2));
				dist_mat(h1, h2) = sqrt(pow(TRUE_H(h1,0) - TRUE_H(h2,0), 2) + pow(TRUE_H(h1,1) - TRUE_H(h2,1), 2));
			}
		}
	}
	
	// OFFSET as 1-D random walks...
	for(int i = 0; i < n_offset_idx; ++i){	
		for(int h=0; h <nh;++h){
			if(h==tk){
					nll -= dnorm(OFFSET(h, i) , Type(0.0), Type(0.0000000001), true);
					nll -= dnorm(SLOPE1(h, i) , Type(0.0), Type(0.0000000001), true);
					nll -= dnorm(SLOPE2(h,i) , Type(0.0), Type(0.0000000001), true);
			}
			else {
					nll -= dnorm(OFFSET(h,i), Type(0),Type(30),true);
					nll -= dnorm(SLOPE1(h,i), Type(0),Type(10),true);
					nll -= dnorm(SLOPE2(h,i), Type(0),Type(10),true);
			}
		}
	}
	for(int p = 0; p < np; ++p){   		// iterate pings
		for(int h=0; h < nh; ++h){		// iterate hydros in ping p
			if(!isNA(toa(p,h))){
				int beac_p = CppAD::Integer(sync_tag_idx_vec(p));
				int ss_idx_int = CppAD::Integer(ss_idx(p));
				int off_idx_int = CppAD::Integer(offset_idx(p));
				
				mu_toa(p,h) = TOP(p) + dist_mat(beac_p, h)/SS(ss_idx_int) + OFFSET(h, off_idx_int) + SLOPE1(h, off_idx_int)*(toa(p,h)/1E6) + SLOPE2(h, off_idx_int)*pow((toa(p,h)/1E6),2);
				eps_toa(p,h) = toa(p,h) - mu_toa(p,h);
				// nll -= dnorm(eps_toa(p,h), Type(0.0), SIGMA_TOA, true);
				// nll -= log(dt(eps_toa(p,h)/SCALE, Type(3.0), false)/SCALE);
				nll -= log(dt(eps_toa(p,h)/SIGMA_TOA, Type(3.0), false)/SIGMA_TOA);
			}
		}
	}

	for(int h=0; h<nh; ++h){
		if(fixed_hydros_vec(h) == 1){
			// nll -= dnorm(TRUE_X(h), H(h,0), Type(1e-5), true);
			// nll -= dnorm(TRUE_Y(h), H(h,1), Type(1e-5), true);
			nll -= dnorm(TRUE_H(h,0), H(h,0), Type(1e-6), true);
			nll -= dnorm(TRUE_H(h,1), H(h,1), Type(1e-6), true);
			nll -= dnorm(SIGMA_HYDROS_XY(h), Type(0), Type(1), true);
		} else {
			// nll -= dnorm(TRUE_X(h), H(h,0), SIGMA_HYDROS_XY(h), true);
			// nll -= dnorm(TRUE_Y(h), H(h,1), SIGMA_HYDROS_XY(h), true);
			nll -= dnorm(TRUE_H(h,0), H(h,0), SIGMA_HYDROS_XY(h), true);
			nll -= dnorm(TRUE_H(h,1), H(h,1), SIGMA_HYDROS_XY(h), true);
		}
	}

	//speed of sound component
	for(int i = 0; i < n_ss_idx; ++i){
		nll -= dnorm(SS(i), Type(1450.0), Type(20), true);
	}
	
	
	REPORT(eps_toa);
	REPORT(SS);
	REPORT(TRUE_H);
	REPORT(dist_mat);
	
	return nll;
}

