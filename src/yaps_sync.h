	DATA_STRING(sync_type);
	DATA_ARRAY(H);
	DATA_ARRAY(toa_offset);
	DATA_ARRAY(toa_delta); 
	DATA_IVECTOR(sync_tag_idx_vec);
	DATA_INTEGER(np);
	DATA_INTEGER(nh);
	DATA_INTEGER(ndelta);
	DATA_INTEGER(tk);
	DATA_VECTOR(fixed_hydros_vec);
	DATA_IVECTOR(offset_idx);
	DATA_INTEGER(n_offset_idx);
	
	DATA_STRING(ss_data_what);	// 'est' = estimate SS-data; 'data' = Use SS-data
	DATA_VECTOR(ss_data_vec);		// Vector of SS-data if used - length(ss_data) = np
	DATA_IVECTOR(ss_idx);
	DATA_INTEGER(n_ss_idx);
	
	PARAMETER_ARRAY(OFFSET);
	PARAMETER_ARRAY(SLOPE1);
	PARAMETER_ARRAY(SLOPE2);
	// PARAMETER_VECTOR(SS);

	PARAMETER_ARRAY(TRUE_H);

	PARAMETER(LOG_SIGMA_TOA);    		
	Type SIGMA_TOA = exp(LOG_SIGMA_TOA);
	
	// PARAMETER_VECTOR(LOG_SIGMA_HYDROS_XY);
	// vector<Type> SIGMA_HYDROS_XY = exp(LOG_SIGMA_HYDROS_XY);

	// if(sync_type == 'top'){
	// 	array<Type> mu(np,nh); 
	// 	array<Type> eps(np,nh);
	// } else {
	// 	vector<Type> mu(ndelta);
	// 	vector<Type> eps(ndelta);
	// }

	array<Type> dist_mat(nh,nh);
	vector<Type> ss_i(np);
	
	// Adapting to zero-based index in cpp
	sync_tag_idx_vec = sync_tag_idx_vec - 1;
	offset_idx = offset_idx - 1;
	ss_idx = ss_idx - 1;
	tk = tk - 1;
	
	// Type nll = 0.0;
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
	
	if(ss_data_what != "est"){
		#include "nll_sync_ss_data.h"
	} else {
		#include "nll_sync_ss_est.h"
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

	if(sync_type == "top"){
		PARAMETER_VECTOR(TOP);

		array<Type> mu(np,nh); 
		array<Type> eps(np,nh);


		for(int p = 0; p < np; ++p){   		// iterate pings
			for(int h=0; h < nh; ++h){		// iterate hydros in ping p
				if(!isNA(toa_offset(p,h))){
					mu(p,h) = TOP(p) + dist_mat(sync_tag_idx_vec(p), h)/ss_i(p) + OFFSET(h, offset_idx(p)) + SLOPE1(h, offset_idx(p))*(toa_offset(p,h)/1E6) + SLOPE2(h, offset_idx(p))*pow((toa_offset(p,h)/1E6),2);
					eps(p,h) = toa_offset(p,h) - mu(p,h);
					// nll -= dnorm(eps(p,h), Type(0.0), SIGMA_TOA, true);
					// nll -= log(dt(eps(p,h)/SCALE, Type(3.0), false)/SCALE);
					nll -= log(dt(eps(p,h)/SIGMA_TOA, Type(3.0), false)/SIGMA_TOA);
				}
			}
		}
		REPORT(eps);
	} else {
		vector<Type> mu(ndelta);
		vector<Type> eps(ndelta);


		for(int d = 0; d < ndelta; ++d){   		// iterate pings
			int H1 = CppAD::Integer(toa_delta(d,0)) - 1;
			int H2 = CppAD::Integer(toa_delta(d,1)) - 1;
			int ping_idx = CppAD::Integer(toa_delta(d,3)) - 1;
			int sync_tag_idx = CppAD::Integer(toa_delta(d,4)) - 1;
			int offset_idx = CppAD::Integer(toa_delta(d,5)) -1;
			int ss_idx = CppAD::Integer(toa_delta(d,6)) - 1;

			// mu(d) = 	(dist_mat(sync_tag_idx, H1)/ss_i(ping_idx) + OFFSET(H1, 0) + SLOPE1(H1, 0)*(toa(ping_idx, H1)/1E6) + SLOPE2(H1, 0)*pow(toa(ping_idx, H1)/1E6, 2) + SLOPE3(H1, 0)*pow(toa(ping_idx, H1)/1E6, 3))  - 
							// (dist_mat(sync_tag_idx, H2)/ss_i(ping_idx) + OFFSET(H2, 0) + SLOPE1(H2, 0)*(toa(ping_idx, H2)/1E6) + SLOPE2(H2, 0)*pow(toa(ping_idx, H2)/1E6, 2) + SLOPE3(H2, 0)*pow(toa(ping_idx, H2)/1E6, 3));
			mu(d) = 	(dist_mat(sync_tag_idx, H1)/ss_i(ping_idx) + OFFSET(H1, offset_idx) + SLOPE1(H1, offset_idx)*(toa_offset(ping_idx, H1)/1E6) + SLOPE2(H1, offset_idx)*pow(toa_offset(ping_idx, H1)/1E6, 2))  - 
							(dist_mat(sync_tag_idx, H2)/ss_i(ping_idx) + OFFSET(H2, offset_idx) + SLOPE1(H2, offset_idx)*(toa_offset(ping_idx, H2)/1E6) + SLOPE2(H2, offset_idx)*pow(toa_offset(ping_idx, H2)/1E6, 2));

			eps(d) = toa_delta(d, 2) - mu(d);
			nll -= log(dt(eps(d)/SIGMA_TOA, Type(3.0), false)/SIGMA_TOA);

		}
		REPORT(eps);
	}

	for(int h=0; h<nh; ++h){
		nll -= dnorm(TRUE_H(h,2), H(h,2), Type(1e-6), true); // depth of hydros can rarely be estimated due to low vertical coverage and variation of hydros
		if(fixed_hydros_vec(h) == 1){
			nll -= dnorm(TRUE_H(h,0), H(h,0), Type(1e-6), true);
			nll -= dnorm(TRUE_H(h,1), H(h,1), Type(1e-6), true);
			// nll -= dnorm(SIGMA_HYDROS_XY(h), Type(0), Type(1), true);
		} 
		else {
			// nll -= dnorm(TRUE_H(h,0), H(h,0), SIGMA_HYDROS_XY(h), true);
			// nll -= dnorm(TRUE_H(h,1), H(h,1), SIGMA_HYDROS_XY(h), true);
			nll -= dnorm(TRUE_H(h,0), H(h,0), Type(10), true);
			nll -= dnorm(TRUE_H(h,1), H(h,1), Type(10), true);
		}
	}

	REPORT(dist_mat);
	
	return nll;

