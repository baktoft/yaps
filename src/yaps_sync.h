	DATA_ARRAY(H);
	DATA_ARRAY(toa_offset);
	DATA_IVECTOR(sync_tag_idx_vec);
	DATA_INTEGER(np);
	DATA_INTEGER(nh);
	DATA_INTEGER(tk);
	DATA_VECTOR(fixed_hydros_vec);
	DATA_IVECTOR(offset_idx);
	DATA_INTEGER(n_offset_idx);

	DATA_VECTOR(ss_vec);		// Vector of SS-data if used - length(ss_data) = np
	
	DATA_INTEGER(E_model); //1 => Gaus; 2 => t
	
	PARAMETER_ARRAY(OFFSET);

	PARAMETER_ARRAY(TRUE_H);

	PARAMETER(LOG_SIGMA_TOA);    		
	Type SIGMA_TOA = exp(LOG_SIGMA_TOA);
	
	PARAMETER(LOG_SIGMA_OFFSET);    		
	Type SIGMA_OFFSET = exp(LOG_SIGMA_OFFSET);
	
	PARAMETER_VECTOR(TOP);

	array<Type> mu(np,nh); 
	array<Type> eps(np,nh);

	array<Type> dist_mat(nh,nh);
	vector<Type> ss_i(np);
	
	// // Adapting to zero-based index in cpp
	sync_tag_idx_vec = sync_tag_idx_vec - 1;
	offset_idx = offset_idx - 1;
	tk = tk - 1;
	
	// // Type nll = 0.0;
	parallel_accumulator<Type> nll(this);  
	
	for(int h1=0; h1 < nh; ++h1){
		for(int h2=0; h2 < nh; ++h2){
			if(h1 == h2){
				dist_mat(h1, h2) = Type(0.0);
			} else {
				dist_mat(h1, h2) = sqrt(pow(TRUE_H(h1,0) - TRUE_H(h2,0), 2) + pow(TRUE_H(h1,1) - TRUE_H(h2,1), 2) + pow(TRUE_H(h1,2) - TRUE_H(h2,2), 2));
			}
		}
	}
	
	if(ss_vec(0) == 0){
		#include "nll_sync_ss_est.h"
	} else {
		#include "nll_sync_ss_data.h"
	}

	
	// // OFFSET as 1-D random walks...
	// for(int i = 0; i < n_offset_idx; ++i){	
		// for(int h=0; h <nh;++h){
			// if(h==tk){
					// nll -= dnorm(OFFSET(h, i) , Type(0.0), Type(0.0000000001), true);
			// }
			// else {
				// if(i == 0){
					// nll -= dnorm(OFFSET(h,i), Type(0),Type(30),true);
				// } else {
					// nll -= dnorm(OFFSET(h,i), OFFSET(h, i-1), SIGMA_OFFSET, true);
				// }
			// }
		// }
	// }

	// OFFSET as 1-D random walks...
	for(int h=0; h <nh;++h){
		if(h==tk){
			for(int i = 0; i < n_offset_idx; ++i){	
				nll -= dnorm(OFFSET(h, i) , Type(0.0), Type(0.0000000001), true);
			}
		}
		else {
			nll -= dnorm(OFFSET(h,0), Type(0),Type(30),true);
			for(int i = 1; i < n_offset_idx; ++i){	
				nll -= dnorm(OFFSET(h,i), OFFSET(h, i-1), SIGMA_OFFSET, true);
			}
		}
	}



	for(int p = 0; p < np; ++p){   		// iterate pings
		for(int h=0; h < nh; ++h){		// iterate hydros in ping p
			if(!isNA(toa_offset(p,h))){
				mu(p,h) = TOP(p) + dist_mat(sync_tag_idx_vec(p), h)/ss_i(p) + OFFSET(h, offset_idx(p));
				eps(p,h) = toa_offset(p,h) - mu(p,h);
				
				if(E_model == 1){			//gaus sync model
					nll -= dnorm(eps(p,h), Type(0.0), SIGMA_TOA, true);
				} else if(E_model == 2){	//t sync model
					nll -= log(dt(eps(p,h)/SIGMA_TOA, Type(3.0), false)/SIGMA_TOA);
				}
			}
		}
	}
	REPORT(eps);

	for(int h=0; h<nh; ++h){
		nll -= dnorm(TRUE_H(h,2), H(h,2), Type(1e-6), true); // depth of hydros can rarely be estimated due to low vertical coverage and variation of hydros
		if(fixed_hydros_vec(h) == 1){
			nll -= dnorm(TRUE_H(h,0), H(h,0), Type(1e-6), true);
			nll -= dnorm(TRUE_H(h,1), H(h,1), Type(1e-6), true);
		} 
		else {
			nll -= dnorm(TRUE_H(h,0), H(h,0), Type(10), true);
			nll -= dnorm(TRUE_H(h,1), H(h,1), Type(10), true);
		}
	}

	REPORT(dist_mat);
	
	return nll;

